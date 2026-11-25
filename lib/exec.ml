(** Physical execution of a (possibly optimised) logical plan. *)

open Plan
module S = Series

(* Row indices selected by a boolean predicate result ([None] = every row).
   A null predicate drops the row, as in Polars. *)
let selection = function
  | Eval.Scalar (Value.Bool true) -> None
  | Eval.Scalar (Value.Bool false) | Eval.Scalar Value.Null -> Some [||]
  | Eval.Scalar v -> Err.fail "filter predicate must be boolean, got %s" (Value.to_string v)
  | Eval.Column s -> (
      match s.data with
      | S.Bools m ->
          let n = S.length s in
          let count = ref 0 in
          (match s.validity with
          | None -> for i = 0 to n - 1 do if Bitmap.get m i then incr count done
          | Some v ->
              for i = 0 to n - 1 do if Bitmap.get m i && Bitmap.get v i then incr count done);
          if !count = n then None
          else begin
            let idx = Array.make !count 0 and k = ref 0 in
            (match s.validity with
            | None ->
                for i = 0 to n - 1 do
                  if Bitmap.get m i then begin idx.(!k) <- i; incr k end
                done
            | Some v ->
                for i = 0 to n - 1 do
                  if Bitmap.get m i && Bitmap.get v i then begin idx.(!k) <- i; incr k end
                done);
            Some idx
          end
      | _ ->
          Err.fail "filter predicate must be boolean, got a %s column"
            (Dtype.to_string (S.dtype s)))

let filter_by df p =
  match selection (Eval.eval (Eval.frame_ctx df) p) with
  | None -> df
  | Some idx -> Dataframe.take df idx

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
  let g = Groups.of_keys key_series n in
  let key_out = List.map (fun s -> S.take_opt s g.first_rows) key_series in
  let gctx = Eval.group_ctx df g (List.map (fun (s : S.t) -> (s.name, s)) key_out) in
  let agg_out =
    List.map
      (fun e ->
        S.rename (Eval.materialize ~height:g.ngroups (Eval.eval gctx e)) (Expr.output_name e))
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
  let nl = Dataframe.height l and nr = Dataframe.height r in
  let lc, rc, ncodes = Groups.join_codes lkeys rkeys in
  (* bucket the right rows by key code (counting sort, keeps right row order) *)
  let counts = Array.make ncodes 0 in
  Array.iter (fun c -> if c >= 0 then counts.(c) <- counts.(c) + 1) rc;
  let start = Array.make (ncodes + 1) 0 in
  for c = 0 to ncodes - 1 do start.(c + 1) <- start.(c) + counts.(c) done;
  let fill = Array.sub start 0 ncodes in
  let sorted = Array.make start.(ncodes) 0 in
  for j = 0 to nr - 1 do
    let c = rc.(j) in
    if c >= 0 then begin sorted.(fill.(c)) <- j; fill.(c) <- fill.(c) + 1 end
  done;
  (* pass 1: output size and which right codes are hit by some left row *)
  let hit = Bytes.make ncodes '\000' in
  let total = ref 0 in
  for i = 0 to nl - 1 do
    let c = lc.(i) in
    let m = if c >= 0 then counts.(c) else 0 in
    if m > 0 then begin total := !total + m; Bytes.set hit c '\001' end
    else if how <> Inner then incr total
  done;
  let unmatched_right j =
    let c = rc.(j) in
    c < 0 || Bytes.get hit c = '\000'
  in
  if how = Full then
    for j = 0 to nr - 1 do if unmatched_right j then incr total done;
  (* pass 2: fill the index arrays; -1 marks "no row on this side" *)
  let li = Array.make !total (-1) and ri = Array.make !total (-1) in
  let k = ref 0 in
  for i = 0 to nl - 1 do
    let c = lc.(i) in
    let m = if c >= 0 then counts.(c) else 0 in
    if m > 0 then
      for t = start.(c) to start.(c) + m - 1 do
        li.(!k) <- i;
        ri.(!k) <- sorted.(t);
        incr k
      done
    else if how <> Inner then begin li.(!k) <- i; incr k end
  done;
  if how = Full then
    for j = 0 to nr - 1 do
      if unmatched_right j then begin ri.(!k) <- j; incr k end
    done;
  (* left columns; for a full join the keys are coalesced with the right keys *)
  let left_cols =
    Array.to_list l.columns
    |> List.map (fun (s : S.t) ->
           match (how, index_of s.name left_on) with
           | Full, Some kidx ->
               let rk = List.nth rkeys kidx in
               let lt = S.take_opt s li and rt = S.take_opt rk ri in
               let lt, rt =
                 match (S.dtype s, S.dtype rk) with
                 | a, b when a = b -> (lt, rt)
                 | _ -> (S.to_float lt, S.to_float rt)
               in
               S.coalesce lt rt
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
  | Scan { source; projection; predicate } ->
      (* Read only the columns that the projection and the predicate need,
         filter each chunk right away, then project and stack the survivors.
         Nothing here depends on the kind of source. *)
      let needed =
        match projection with
        | None -> None
        | Some cols ->
            let pcols = match predicate with None -> [] | Some p -> Expr.columns p in
            Some (cols @ List.filter (fun c -> not (List.mem c cols)) pcols)
      in
      let chunks = ref [] in
      Scan.iter_chunks source ~columns:needed (fun df ->
          let df = match predicate with None -> df | Some p -> filter_by df p in
          let df =
            match projection with None -> df | Some cols -> Dataframe.select_columns df cols
          in
          chunks := df :: !chunks);
      Dataframe.concat (List.rev !chunks)
  | Filter (i, p) -> filter_by (run i) p
  | Select (i, es) -> exec_select (run i) es
  | With_columns (i, es) -> exec_with_columns (run i) es
  | Aggregate { input; keys; aggs } -> exec_aggregate (run input) keys aggs
  | Join { left; right; left_on; right_on; how } ->
      exec_join ~how (run left) (run right) left_on right_on
