let batch_size = 1024

type join_how = Inner | Left | Full

type t =
  | Scan of { data : Row.t list; columns : string list; schema : Schema.t }
  | Filter of { child : t; predicate : Expr.t }
  | Project of { child : t; exprs : (string * Expr.t) list }
  | WithColumn of { child : t; name : string; expr : Expr.t }
  | Join of {
      left : t;
      right : t;
      left_on : string list;
      right_on : string list;
      how : join_how;
    }
  | Aggregate of { child : t; group_by : string list; aggs : Agg.t list }

let scan data ~columns : t = Scan { data; columns; schema = Schema.infer_from_rows columns data }
let filter child predicate : t = Filter { child; predicate }
let project child exprs = Project { child; exprs }
let with_column child name expr = WithColumn { child; name; expr}
let join left right ~left_on ~right_on ~how = Join { left; right; left_on; right_on; how }
let aggregate child group_by aggs = Aggregate { child; group_by; aggs }
  
let how_to_string = function Inner -> "inner" | Left -> "left" | Full -> "full"

let rec schema (p : t) : Schema.t =
  match p with
  | Scan { schema; _ } -> schema
  | Filter { child; _ } -> schema child
  | Project { child; exprs } ->
      let parent = schema child in
      Schema.make
        (List.map
           (fun (name, e) -> { Schema.name; dtype = Expr.output_dtype parent e; nullable = true })
           exprs)
  | WithColumn { child; name; expr } ->
      let parent = schema child in
      Schema.with_column parent name (Expr.output_dtype parent expr) ~nullable:true
  | Join { left; right; how; _ } ->
      let ls = schema left and rs = schema right in
      let ls = if how = Full then Schema.force_nullable ls else ls in
      let rs = if how = Left || how = Full then Schema.force_nullable rs else rs in
      Schema.merge ls rs
  | Aggregate { child; group_by; aggs } ->
      let parent = schema child in
      let group_cols =
        List.map (fun name -> { Schema.name; dtype = Schema.dtype_of parent name; nullable = Schema.nullable_of parent name }) group_by
      in
      let agg_cols =
        List.map (fun a -> { Schema.name = Agg.output_name a; dtype = Agg.output_dtype parent a; nullable = true }) aggs
      in
      Schema.make (group_cols @ agg_cols)

let key_indices (schema : Schema.t) (on : string list) : int list =
  let col_map = Schema.col_map schema in
  List.map (fun name -> List.assoc name col_map) on

let key_of (row : Row.t) (indices : int list) : Value.t option list =
  List.map (Array.get row) indices

let batched (rows : Row.t list) : Row.t list Seq.t =
  let rec go rows () =
    match rows with
    | [] -> Seq.Nil
    | _ ->
        let rec take n acc = function
          | rest when n = 0 -> (List.rev acc, rest)
          | [] -> (List.rev acc, [])
          | x :: xs -> take (n - 1) (x :: acc) xs
        in
        let chunk, rest = take batch_size [] rows in
        Seq.Cons (chunk, go rest)
  in
  go rows

let rec execute_batched (p : t) : Row.t list Seq.t =
  match p with
  | Scan { data; _ } -> batched data
  | Filter { child; predicate } ->
      let col_map = Schema.col_map (schema child) in
      let pred_fn = Expr.compile_bool col_map predicate in
      execute_batched child
      |> Seq.map (List.filter pred_fn)
      |> Seq.filter (fun chunk -> chunk <> [])
  | Project { child; exprs } ->
      let col_map = Schema.col_map (schema child) in
      let compiled = List.map (fun (_, e) -> Expr.compile col_map e) exprs in
      execute_batched child
      |> Seq.map (List.map (fun row -> Array.of_list (List.map (fun f -> f row) compiled)))
  | WithColumn { child; name; expr } ->
      let parent_cols = Schema.column_names (schema child) in
      let col_map = Schema.col_map (schema child) in
      let fn = Expr.compile col_map expr in
      let existing_idx = List.mapi (fun i n -> (n, i)) parent_cols |> List.assoc_opt name in
      execute_batched child
      |> Seq.map
           (List.map (fun row ->
                match existing_idx with
                | Some idx ->
                    let row' = Array.copy row in
                    row'.(idx) <- fn row;
                    row'
                | None -> Array.append row [| fn row |]))
  | Join { left; right; left_on; right_on; how } ->
        let rs = schema right and ls = schema left in
        let r_width = List.length (Schema.column_names rs) in
        let l_width = List.length (Schema.column_names ls) in
        let r_idx = key_indices rs right_on in
        let l_idx = key_indices ls left_on in

        (* --- build phase: hash the right side on its join key --- *)
        let right_ht : (Value.t option list, Row.t list) Hashtbl.t = Hashtbl.create 256 in
        execute_batched right
        |> Seq.iter
             (List.iter (fun row ->
                  let k = key_of row r_idx in
                  let existing = Option.value (Hashtbl.find_opt right_ht k) ~default:[] in
                  Hashtbl.replace right_ht k (row :: existing)));
        (* rows were prepended per bucket; reverse once to preserve original order *)
        Hashtbl.filter_map_inplace (fun _ rows -> Some (List.rev rows)) right_ht;

        let null_row (n : int) : Row.t = Array.make n None in
        let matched_keys : (Value.t option list, unit) Hashtbl.t = Hashtbl.create 256 in

        let out_batches = ref [] and buf = ref [] and n = ref 0 in
        let push row =
          buf := row :: !buf;
          incr n;
          if !n >= batch_size then begin
            out_batches := List.rev !buf :: !out_batches;
            buf := [];
            n := 0
          end
        in

        (* --- probe phase: stream the left side --- *)
        execute_batched left
        |> Seq.iter
             (List.iter (fun left_row ->
                  let key = key_of left_row l_idx in
                  match Hashtbl.find_opt right_ht key with
                  | Some matches ->
                      Hashtbl.replace matched_keys key ();
                      List.iter (fun right_row -> push (Array.append left_row right_row)) matches
                  | None ->
                      if how = Left || how = Full then
                        push (Array.append left_row (null_row r_width))));

        (* --- full outer: emit unmatched right rows, padded with null left --- *)
        if how = Full then
          Hashtbl.iter
            (fun key rows ->
              if not (Hashtbl.mem matched_keys key) then
                List.iter (fun right_row -> push (Array.append (null_row l_width) right_row)) rows)
            right_ht;

        if !buf <> [] then out_batches := List.rev !buf :: !out_batches;
        List.to_seq (List.rev !out_batches)
  | Aggregate { child; group_by; aggs } ->
      let parent_schema = schema child in
      let col_map = Schema.col_map parent_schema in
      let g_idx = List.map (fun c -> List.assoc c col_map) group_by in
      let key_of (row : Row.t) : Value.t option list = List.map (Array.get row) g_idx in

      let compiled_evals = List.map (fun (a : Agg.t) -> Expr.compile col_map a.expr) aggs in

      let accumulators : (Value.t option list, Agg.acc list) Hashtbl.t = Hashtbl.create 256 in
      execute_batched child
      |> Seq.iter
           (List.iter (fun row ->
                let key = key_of row in
                let accs =
                  match Hashtbl.find_opt accumulators key with
                  | Some accs -> accs
                  | None ->
                      let accs = List.map Agg.init_acc aggs in
                      Hashtbl.add accumulators key accs;
                      accs
                in
                List.iter2 (fun agg (acc, eval) -> Agg.update_acc agg acc (eval row))
                  aggs (List.combine accs compiled_evals)));
                (* List.iteri *)
                  (* (fun i agg -> Agg.update_acc agg (List.nth accs i) ((List.nth compiled_evals i) row)) *)
                  (* aggs; *)
      let out_batches = ref [] and buf = ref [] and n = ref 0 in
      Hashtbl.iter
        (fun key accs ->
          let agg_vals = List.map2 (fun a acc -> Agg.finalize_acc a acc) aggs accs in
          buf := Array.of_list (key @ agg_vals) :: !buf;
          incr n;
          if !n >= batch_size then begin
            out_batches := List.rev !buf :: !out_batches;
            buf := []; n := 0
          end)
        accumulators;
      if !buf <> [] then out_batches := List.rev !buf :: !out_batches;
      List.to_seq (List.rev !out_batches) 

let indent (s : string) : string =
  s |> String.split_on_char '\n' |> List.map (fun line -> "  " ^ line) |> String.concat "\n"

let rec explain (p : t) : string =
  match p with
  | Scan { data; columns; _ } ->
      Printf.sprintf "Scan [%s] (%d rows)" (String.concat ", " columns) (List.length data)
  | Filter { child; predicate } ->
      Printf.sprintf "Filter [%s]\n%s" (Expr.to_string predicate) (indent (explain child))
  | Project { child; exprs } ->
      let parts = List.map (fun (n, e) -> Printf.sprintf "%s = %s" n (Expr.to_string e)) exprs in
      Printf.sprintf "Project [%s]\n%s" (String.concat ", " parts) (indent (explain child))
  | WithColumn { child; name; expr } ->
      Printf.sprintf "WithColumn [%s = %s]\n%s" name (Expr.to_string expr) (indent (explain child))
  | Join { left; right; left_on; right_on; how } ->
      Printf.sprintf "Join [%s] %s = %s\n%s\n%s"
        (how_to_string how) (String.concat "," left_on) (String.concat "," right_on)
        (indent (explain left)) (indent (explain right))
  | Aggregate { child; group_by; aggs } ->
      Printf.sprintf "Aggregate by=[%s] aggs=[%s]\n%s"
        (String.concat ", " group_by) (String.concat ", " (List.map Agg.to_string aggs))
        (indent (explain child))

module StrSet = Expr.StrSet

(* Pass 1: Filter pushdown
   Recursively push each [Filter] node as close to the data as possible,
   so downstream operator see fewer rows. *)
let rec push_filters (node : t) : t =
  match node with
  | Filter { child; predicate } ->
      let child = push_filters child in
      try_push_filter predicate child
  | Project { child; exprs } -> Project { child = push_filters child; exprs }
  | WithColumn { child; name; expr } -> WithColumn { child = push_filters child; name; expr }
  | Join { left; right; left_on; right_on; how } ->
      Join { left = push_filters left; right = push_filters right; left_on; right_on; how }
  | Aggregate { child; group_by; aggs } -> Aggregate { child = push_filters child; group_by; aggs }
  | Scan _ -> node

(** Try to move [predicate] below [child]. If the predicate only needs columns
    that exist below some intermediate node, push it further down.
    Otherwise, leave it as a [Filter] directly above [child]. *)
and try_push_filter (predicate : Expr.t) (child : t) : t =
  let needed = Expr.required_columns predicate in
  match child with
  | Project { child = grandchild; exprs } ->
      let grandchild_cols = StrSet.of_list (Schema.column_names (schema grandchild)) in
      if StrSet.subset needed grandchild_cols then
        Project { child = Filter { child = grandchild; predicate }; exprs }
      else Filter { child; predicate }
  | Aggregate { child = grandchild; group_by; aggs } ->
      let group_keys = StrSet.of_list group_by in
      if StrSet.subset needed group_keys then
        Aggregate { child = Filter { child = grandchild; predicate }; group_by; aggs }
      else Filter { child; predicate }
  | Join { left; right; left_on; right_on; how } ->
      let left_cols = StrSet.of_list (Schema.column_names (schema left)) in
      let right_cols = StrSet.of_list (Schema.column_names (schema right)) in
      if StrSet.subset needed left_cols then
        Join { left = Filter { child = left; predicate }; right; left_on; right_on; how }
      else if StrSet.subset needed right_cols then
        Join { left; right = Filter { child = right; predicate }; left_on; right_on; how }
      else Filter { child; predicate }
  | _ -> Filter { child; predicate }

(** Pass 2: Column pruning
   Top-down: track wich columns are necessary downstream, and
   narrow [Scan]/[Project] nodes to only produce those. *)
let rec prune_columns (node : t) (needed : StrSet.t) : t =
  match node with
  | Scan { data; columns; schema = s } ->
      let available = StrSet.of_list columns in
      let keep = List.filter (fun c -> StrSet.mem c needed) columns in
      if List.length keep < StrSet.cardinal available && keep <> [] then
        Project { child = Scan { data; columns; schema = s }; exprs = List.map (fun c -> (c, Expr.col c)) keep }
      else Scan { data; columns; schema = s }
  | Project { child; exprs } ->
      let kept = List.filter (fun (name, _) -> StrSet.mem name needed) exprs in
      let kept = if kept = [] then exprs else kept in
      let child_needed =
        List.fold_left (fun acc (_, e) -> StrSet.union acc (Expr.required_columns e)) needed kept
      in
      Project { child = prune_columns child child_needed; exprs = kept }
  | Filter { child; predicate } ->
      let child_needed = StrSet.union needed (Expr.required_columns predicate) in
      Filter { child = prune_columns child child_needed; predicate }
  | WithColumn { child; name; expr } ->
      let child_needed = StrSet.union (StrSet.remove name needed) (Expr.required_columns expr) in
      WithColumn { child = prune_columns child child_needed; name; expr }
  | Aggregate { child; group_by; aggs } ->
      let agg_needed =
        List.fold_left (fun acc (a : Agg.t) -> StrSet.union acc (Expr.required_columns a.expr))
          StrSet.empty aggs
      in
      let child_needed = StrSet.union agg_needed (StrSet.of_list group_by) in
      Aggregate { child = prune_columns child child_needed; group_by; aggs }
  | Join { left; right; left_on; right_on; how } ->
      let left_cols = StrSet.of_list (Schema.column_names (schema left)) in
      let right_cols = StrSet.of_list (Schema.column_names (schema right)) in
      let left_needed = StrSet.union (StrSet.inter needed left_cols) (StrSet.of_list left_on) in
      let right_needed = StrSet.union (StrSet.inter needed right_cols) (StrSet.of_list right_on) in
      Join { left = prune_columns left left_needed; right = prune_columns right right_needed; left_on; right_on; how }

let optimize (p : t) : t =
  let p = push_filters p in
  let needed = StrSet.of_list (Schema.column_names (schema p)) in
  prune_columns p needed
