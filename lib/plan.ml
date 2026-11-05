(** Logical plan: the tree a [Lazy_frame] builds up before [collect]. *)

type join_type = Inner | Left | Full

type t =
  | Scan of {
      df : Dataframe.t;
      projection : string list option;  (** columns kept after the scan *)
      predicate : Expr.t option;  (** filter applied at the scan, before projection *)
    }
  | Filter of t * Expr.t
  | Select of t * Expr.t list
  | With_columns of t * Expr.t list
  | Aggregate of { input : t; keys : Expr.t list; aggs : Expr.t list }
  | Join of {
      left : t;
      right : t;
      left_on : string list;
      right_on : string list;
      how : join_type;
    }

let join_type_to_string = function Inner -> "INNER" | Left -> "LEFT" | Full -> "FULL"

(** Non-key right-hand columns as [(input_name, output_name)]. A column whose
    name collides with a left column gets the ["_right"] suffix. The right key
    columns are dropped (the left keys act as the coalesced keys). *)
let join_right_columns ~left_cols ~right_cols ~right_on =
  right_cols
  |> List.filter (fun c -> not (List.mem c right_on))
  |> List.map (fun c -> (c, if List.mem c left_cols then c ^ "_right" else c))

let rec schema = function
  | Scan { df; projection; _ } -> (
      match projection with None -> Dataframe.column_names df | Some p -> p)
  | Filter (i, _) -> schema i
  | Select (_, es) -> List.map Expr.output_name es
  | With_columns (i, es) ->
      let base = schema i in
      List.fold_left
        (fun acc e ->
          let n = Expr.output_name e in
          if List.mem n acc then acc else acc @ [ n ])
        base es
  | Aggregate { keys; aggs; _ } -> List.map Expr.output_name (keys @ aggs)
  | Join { left; right; right_on; _ } ->
      let l = schema left in
      l @ List.map snd (join_right_columns ~left_cols:l ~right_cols:(schema right) ~right_on)

(* ---- validation (done at collect time, like Polars' schema resolution) ---- *)

let check_unique what names =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun n ->
      if Hashtbl.mem seen n then Err.fail "duplicate output column %S in %s" n what;
      Hashtbl.add seen n ())
    names

let check_expr ~available e =
  List.iter
    (fun c ->
      if not (List.mem c available) then
        Err.fail "ColumnNotFound: %S in %s (available: [%s])" c (Expr.to_string e)
          (String.concat ", " available))
    (Expr.columns e)

let rec validate = function
  | Scan { df; predicate; projection } ->
      let avail = Dataframe.column_names df in
      Option.iter (check_expr ~available:avail) predicate;
      Option.iter (List.iter (fun c -> check_expr ~available:avail (Expr.Col c))) projection
  | Filter (i, p) ->
      validate i;
      check_expr ~available:(schema i) p
  | Select (i, es) ->
      validate i;
      List.iter (check_expr ~available:(schema i)) es;
      check_unique "select" (List.map Expr.output_name es)
  | With_columns (i, es) ->
      validate i;
      List.iter (check_expr ~available:(schema i)) es;
      check_unique "with_columns" (List.map Expr.output_name es)
  | Aggregate { input; keys; aggs } ->
      validate input;
      List.iter (check_expr ~available:(schema input)) (keys @ aggs);
      check_unique "aggregate" (List.map Expr.output_name (keys @ aggs))
  | Join { left; right; left_on; right_on; _ } ->
      validate left;
      validate right;
      if left_on = [] || List.length left_on <> List.length right_on then
        Err.fail "join requires the same, non-zero number of keys on both sides";
      List.iter (fun c -> check_expr ~available:(schema left) (Expr.Col c)) left_on;
      List.iter (fun c -> check_expr ~available:(schema right) (Expr.Col c)) right_on

(* ---- pretty printer (explain) ---- *)

let exprs_to_string es = "[" ^ String.concat ", " (List.map Expr.to_string es) ^ "]"

let to_string plan =
  let b = Buffer.create 256 in
  let rec go ind p =
    let line extra fmt =
      Printf.ksprintf
        (fun s ->
          Buffer.add_string b (String.make (ind + extra) ' ');
          Buffer.add_string b s;
          Buffer.add_char b '\n')
        fmt
    in
    match p with
    | Scan { df; projection; predicate } ->
        let names = Dataframe.column_names df in
        line 0 "DF [%s]" (String.concat ", " names);
        line 2 "PROJECT %s COLUMNS"
          (match projection with
          | None -> "*"
          | Some p -> Printf.sprintf "%d/%d" (List.length p) (List.length names));
        line 2 "SELECTION: %s"
          (match predicate with None -> "None" | Some p -> Expr.to_string p)
    | Filter (i, p) ->
        line 0 "FILTER %s" (Expr.to_string p);
        line 2 "FROM";
        go (ind + 4) i
    | Select (i, es) ->
        line 0 "SELECT %s" (exprs_to_string es);
        line 2 "FROM";
        go (ind + 4) i
    | With_columns (i, es) ->
        line 0 "WITH_COLUMNS %s" (exprs_to_string es);
        line 2 "FROM";
        go (ind + 4) i
    | Aggregate { input; keys; aggs } ->
        line 0 "AGGREGATE";
        line 2 "BY %s" (exprs_to_string keys);
        line 2 "AGGS %s" (exprs_to_string aggs);
        line 2 "FROM";
        go (ind + 4) input
    | Join { left; right; left_on; right_on; how } ->
        line 0 "%s JOIN ON [%s] = [%s]" (join_type_to_string how)
          (String.concat ", " left_on) (String.concat ", " right_on);
        line 2 "LEFT PLAN:";
        go (ind + 4) left;
        line 2 "RIGHT PLAN:";
        go (ind + 4) right
  in
  go 0 plan;
  Buffer.contents b
