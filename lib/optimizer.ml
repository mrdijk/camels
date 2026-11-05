(** Two rewrite passes, both semantics-preserving:

    - {b predicate pushdown}: move filters as close to the scans as possible
      (splitting conjunctions), including into either side of a join when legal.
    - {b projection pushdown}: tell each scan which columns are actually used
      so the rest are never carried through the plan. *)

open Plan

let cols = Expr.columns
let subset xs ys = List.for_all (fun x -> List.mem x ys) xs
let union a b = a @ List.filter (fun x -> not (List.mem x a)) b
let cols_of_list es = List.fold_left (fun acc e -> union acc (cols e)) [] es
let wrap node preds = List.fold_left (fun n p -> Filter (n, p)) node preds

(* ------------------------------------------------------------------ *)
(* Predicate pushdown                                                  *)
(* ------------------------------------------------------------------ *)

(** [pushdown plan preds]: rebuild [plan] such that the conjuncts [preds]
    (which filter its output) are applied as deep as is safe. *)
let rec pushdown plan (preds : Expr.t list) : Plan.t =
  (* Predicates without column references are never pushed past a node. *)
  let pushable ok p = cols p <> [] && ok p in
  match plan with
  | Filter (i, p) -> pushdown i (Expr.split_and p @ preds)
  | Scan s ->
      let existing = match s.predicate with Some p -> Expr.split_and p | None -> [] in
      Scan { s with predicate = Expr.and_all (existing @ preds) }
  | Select (i, es) ->
      (* Only pass-through columns, and only if no expr depends on row set. *)
      let ok p =
        (not (List.exists Expr.has_agg es))
        && List.for_all (fun c -> List.mem (Expr.Col c) es) (cols p)
      in
      let push, stay = List.partition (pushable ok) preds in
      wrap (Select (pushdown i push, es)) stay
  | With_columns (i, es) ->
      let new_names = List.map Expr.output_name es in
      let ok p =
        (not (List.exists Expr.has_agg es))
        && List.for_all (fun c -> not (List.mem c new_names)) (cols p)
      in
      let push, stay = List.partition (pushable ok) preds in
      wrap (With_columns (pushdown i push, es)) stay
  | Aggregate a ->
      (* Predicates on plain group-key columns commute with aggregation. *)
      let key_cols =
        List.filter_map (function Expr.Col c -> Some c | _ -> None) a.keys
      in
      let ok p = subset (cols p) key_cols in
      let push, stay = List.partition (pushable ok) preds in
      wrap (Aggregate { a with input = pushdown a.input push }) stay
  | Join j ->
      let lcols = Plan.schema j.left in
      let rmap =
        Plan.join_right_columns ~left_cols:lcols ~right_cols:(Plan.schema j.right)
          ~right_on:j.right_on
      in
      (* right columns whose output name is unchanged (no suffix) *)
      let right_plain =
        List.filter_map (fun (i, o) -> if i = o then Some i else None) rmap
      in
      let to_left p = j.how <> Full && subset (cols p) lcols in
      let to_right p = j.how = Inner && subset (cols p) right_plain in
      let lp, rest = List.partition (pushable to_left) preds in
      let rp, stay = List.partition (pushable to_right) rest in
      wrap
        (Join { j with left = pushdown j.left lp; right = pushdown j.right rp })
        stay

(* ------------------------------------------------------------------ *)
(* Projection pushdown                                                 *)
(* ------------------------------------------------------------------ *)

(** [prune plan needed]: [needed = Some names] means the consumer only reads
    those output columns; [None] means all of them. *)
let rec prune plan (needed : string list option) : Plan.t =
  match plan with
  | Scan s -> (
      match needed with
      | None -> plan
      | Some n ->
          let all = Dataframe.column_names s.df in
          let keep = List.filter (fun c -> List.mem c n) all in
          (* keep one column so the row count survives e.g. select(len()) *)
          let keep = match (keep, all) with [], c :: _ -> [ c ] | k, _ -> k in
          Scan { s with projection = Some keep })
  | Filter (i, p) ->
      Filter (prune i (Option.map (fun n -> union n (cols p)) needed), p)
  | Select (i, es) -> Select (prune i (Some (cols_of_list es)), es)
  | With_columns (i, es) -> (
      match needed with
      | None -> With_columns (prune i None, es)
      | Some n ->
          (* drop expressions nobody reads *)
          let kept = List.filter (fun e -> List.mem (Expr.output_name e) n) es in
          let new_names = List.map Expr.output_name es in
          let below =
            union (List.filter (fun c -> not (List.mem c new_names)) n) (cols_of_list kept)
          in
          let child = prune i (Some below) in
          if kept = [] then child else With_columns (child, kept))
  | Aggregate a ->
      Aggregate { a with input = prune a.input (Some (cols_of_list (a.keys @ a.aggs))) }
  | Join j -> (
      match needed with
      | None -> Join { j with left = prune j.left None; right = prune j.right None }
      | Some n ->
          let lcols = Plan.schema j.left and rcols = Plan.schema j.right in
          let rmap =
            Plan.join_right_columns ~left_cols:lcols ~right_cols:rcols ~right_on:j.right_on
          in
          let right_keep =
            List.filter_map (fun (i, o) -> if List.mem o n then Some i else None) rmap
          in
          (* A kept right column that collides with a left column must keep
             its left twin, or the "_right" suffix (and thus the name) changes. *)
          let twins = List.filter (fun c -> List.mem c lcols) right_keep in
          let left_keep =
            union (List.filter (fun c -> List.mem c n) lcols) (union j.left_on twins)
          in
          Join
            {
              j with
              left = prune j.left (Some left_keep);
              right = prune j.right (Some (union right_keep j.right_on));
            })

let optimize plan = prune (pushdown plan []) None
