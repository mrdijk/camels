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
let scan data ~columns : t = Scan { data; columns; schema = Schema.infer_from_rows columns data }
let filter child predicate : t = Filter { child; predicate }
let project child exprs = Project { child; exprs }
let with_column child name expr = WithColumn { child; name; expr}
let join left right ~left_on ~right_on ~how = Join { left; right; left_on; right_on; how }

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
 
let rec explain (p : t) : string =
  match p with
  | Scan { data; columns; _ } ->
      Printf.sprintf "Scan [%s] (%d rows)" (String.concat ", " columns) (List.length data)
  | Filter { child; predicate } ->
      Printf.sprintf "Filter [%s]\n  %s" (Expr.to_string predicate) (explain child)
  | Project { child; exprs } ->
      let parts = List.map (fun (n, e) -> Printf.sprintf "%s = %s" n (Expr.to_string e)) exprs in
      Printf.sprintf "Project [%s]\n  %s" (String.concat ", " parts) (explain child)      
  | WithColumn { child; name; expr } ->
        Printf.sprintf "WithColumn [%s = %s]\n  %s" name (Expr.to_string expr) (explain child)
   | Join { left; right; left_on; right_on; how } ->
      Printf.sprintf "Join [%s] %s = %s\n  %s\n  %s"
        (how_to_string how) (String.concat "," left_on) (String.concat "," right_on)
        (explain left) (explain right)
