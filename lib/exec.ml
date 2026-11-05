(** Physical execution of a (possibly optimised) logical plan. *)

open Plan
module S = Series

let mask_of_ev n = function
  | Eval.Scalar (Value.Bool b) -> Array.make n b
  | Eval.Scalar Value.Null -> Array.make n false
  | Eval.Scalar v -> Err.fail "filter predicate must be boolean, got %s" (Value.to_string v)
  | Eval.Column s -> (
      match s.data with
      | S.Bools m -> Array.init n (fun i -> S.is_valid s i && Bitmap.get m i) (* null = false *)
      | _ ->
          Err.fail "filter predicate must be boolean, got a %s column"
            (Dtype.to_string (S.dtype s)))

let filter_by df p =
  let ev = Eval.eval (Eval.frame_ctx df) p in
  Dataframe.filter df (mask_of_ev (Dataframe.height df) ev)

(* ---- select / with_columns ---- *)

let exec_select df es =
  let ctx = Eval.frame_ctx df in
  let evs = List.map (fun e -> (Expr.output_name e, Eval.eval ctx e)) es in
  let any_col = List.exists (function _, Eval.Column _ -> true | _ -> false) evs in
  (* An all-scalar select (e.g. only aggregations) yields a single row. *)
  let height = if any_col then Dataframe.height df else 1 in
  Dataframe.create (List.map (fun (n, ev) -> S.rename (Eval.materialize ~height ev) n) evs)

let exec_with_columns df es =
  let ctx = Eval.frame_ctx df in
  let height = Dataframe.height df in
  (* evaluate everything against the input frame first, then apply *)
  let series =
    List.map
      (fun e -> S.rename (Eval.materialize ~height (Eval.eval ctx e)) (Expr.output_name e))
      es
  in
  List.fold_left Dataframe.with_column df series

(* ---- group by / aggregate ---- *)

let exec_aggregate df keys aggs =
  let n = Dataframe.height df in
  let ctx = Eval.frame_ctx df in
  let key_series =
    List.map
      (fun k -> S.rename (Eval.materialize ~height:n (Eval.eval ctx k)) (Expr.output_name k))
      keys
  in
  let groups : int array array =
    match key_series with
    | [] -> [| Array.init n Fun.id |]
    | _ ->
        let tbl : (Value.t list, int list ref) Hashtbl.t = Hashtbl.create 64 in
        let order = ref [] in
        for i = 0 to n - 1 do
          let k = List.map (fun s -> S.get s i) key_series in
          match Hashtbl.find_opt tbl k with
          | Some r -> r := i :: !r
          | None ->
              let r = ref [ i ] in
              Hashtbl.add tbl k r;
              order := r :: !order
        done;
        (* groups in order of first appearance, rows in original order *)
        Array.of_list (List.rev_map (fun r -> Array.of_list (List.rev !r)) !order)
  in
  let first_rows = Array.map (fun idx -> idx.(0)) groups in
  let key_out = List.map (fun s -> S.take s first_rows) key_series in
  let gctx = Eval.group_ctx df groups (List.map (fun (s : S.t) -> (s.name, s)) key_out) in
  let ng = Array.length groups in
  let agg_out =
    List.map
      (fun e -> S.rename (Eval.materialize ~height:ng (Eval.eval gctx e)) (Expr.output_name e))
      aggs
  in
  Dataframe.create (key_out @ agg_out)

(* ---- hash join ---- *)

let index_of x l =
  let rec go i = function [] -> None | y :: ys -> if y = x then Some i else go (i + 1) ys in
  go 0 l

let exec_join ~how (l : Dataframe.t) (r : Dataframe.t) left_on right_on =
  let cols df names = List.map (Dataframe.get_column df) names in
  let lkeys = cols l left_on and rkeys = cols r right_on in
  let key_of series i = List.map (fun s -> Value.normalize (S.get s i)) series in
  let has_null = List.exists Value.is_null in
  (* build side: right *)
  let tbl : (Value.t list, int list ref) Hashtbl.t = Hashtbl.create 64 in
  for j = 0 to Dataframe.height r - 1 do
    let k = key_of rkeys j in
    if not (has_null k) then
      match Hashtbl.find_opt tbl k with
      | Some cell -> cell := j :: !cell
      | None -> Hashtbl.add tbl k (ref [ j ])
  done;
  Hashtbl.filter_map_inplace (fun _ cell -> cell := List.rev !cell; Some cell) tbl;
  (* probe side: left *)
  let li = ref [] and ri = ref [] in
  let emit a b = li := a :: !li; ri := b :: !ri in
  let matched = Array.make (Dataframe.height r) false in
  for i = 0 to Dataframe.height l - 1 do
    let k = key_of lkeys i in
    let ms =
      if has_null k then []
      else match Hashtbl.find_opt tbl k with Some c -> !c | None -> []
    in
    match ms with
    | [] -> if how <> Inner then emit i (-1)
    | _ -> List.iter (fun j -> matched.(j) <- true; emit i j) ms
  done;
  if how = Full then Array.iteri (fun j m -> if not m then emit (-1) j) matched;
  let li = Array.of_list (List.rev !li) and ri = Array.of_list (List.rev !ri) in
  let m = Array.length li in
  (* left columns; for a full join, keys are coalesced with the right keys *)
  let left_cols =
    Array.to_list l.columns
    |> List.map (fun (s : S.t) ->
           match (how, index_of s.name left_on) with
           | Full, Some k ->
               let rk = List.nth rkeys k in
               let dtype =
                 match Dtype.supertype (S.dtype s) (S.dtype rk) with
                 | Some t -> t
                 | None -> S.dtype s
               in
               S.of_values ~dtype s.name
                 (Array.init m (fun p ->
                      if li.(p) >= 0 then S.get s li.(p) else S.get rk ri.(p)))
           | _ -> S.take_opt s li)
  in
  let rmap =
    Plan.join_right_columns ~left_cols:(Dataframe.column_names l)
      ~right_cols:(Dataframe.column_names r) ~right_on
  in
  let right_cols =
    List.map
      (fun (input, output) -> S.rename (S.take_opt (Dataframe.get_column r input) ri) output)
      rmap
  in
  Dataframe.create (left_cols @ right_cols)

(* ---- driver ---- *)

let rec run = function
  | Scan { df; projection; predicate } -> (
      let df = match predicate with None -> df | Some p -> filter_by df p in
      match projection with None -> df | Some cols -> Dataframe.select_columns df cols)
  | Filter (i, p) -> filter_by (run i) p
  | Select (i, es) -> exec_select (run i) es
  | With_columns (i, es) -> exec_with_columns (run i) es
  | Aggregate { input; keys; aggs } -> exec_aggregate (run input) keys aggs
  | Join { left; right; left_on; right_on; how } ->
      exec_join ~how (run left) (run right) left_on right_on
