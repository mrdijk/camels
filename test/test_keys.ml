(* Differential tests of group-by and join against naive reference
   implementations that work on plain Value rows. *)
open Camels
module V = Value
module L = Lazyframe

let failures = ref 0
let checks = ref 0
let fail fmt = Printf.ksprintf (fun s -> incr failures; print_endline s) fmt

let row_str r = "[" ^ String.concat "; " (List.map V.to_string r) ^ "]"
let rows_str rows = String.concat " " (List.map row_str rows)
let rows_eq a b =
  List.length a = List.length b
  && List.for_all2 (fun r1 r2 -> List.length r1 = List.length r2 && List.for_all2 V.equal r1 r2) a b

let pick l = List.nth l (Random.int (List.length l))
let nullable gen = Array.map (fun _ -> if Random.int 6 = 0 then None else Some (gen ()))

(* ---- random data ------------------------------------------------------- *)

let float_pool = [| 0.0; -0.0; 1.5; 2.5; Float.nan; 3.0; 10.0; -1.0 |]

let gen_left n =
  let idx = Array.make n () in
  Dataframe.create
    [ Series.of_opt_ints "i" (nullable (fun () -> Random.int 6) idx);
      Series.of_opt_ints "big" (nullable (fun () -> pick [ -4_000_000_000_000; 0; 7; 1 lsl 40; max_int; min_int ]) idx);
      Series.of_opt_floats "f" (nullable (fun () -> float_pool.(Random.int (Array.length float_pool))) idx);
      Series.of_opt_strings "s" (nullable (fun () -> pick [ "a"; "b"; "c"; "" ]) idx);
      Series.of_opt_bools "b" (nullable (fun () -> Random.bool ()) idx);
      Series.of_ints "lp" (Array.init n (fun k -> k)) ]

let gen_right n =
  let idx = Array.make n () in
  Dataframe.create
    [ Series.of_opt_ints "ri" (nullable (fun () -> Random.int 6) idx);
      Series.of_opt_ints "rbig" (nullable (fun () -> pick [ -4_000_000_000_000; 0; 7; 1 lsl 40; max_int; min_int ]) idx);
      Series.of_opt_floats "rf" (nullable (fun () -> float_pool.(Random.int (Array.length float_pool))) idx);
      Series.of_opt_strings "rs" (nullable (fun () -> pick [ "a"; "b"; "c"; "" ]) idx);
      Series.of_opt_bools "rb" (nullable (fun () -> Random.bool ()) idx);
      Series.of_opt_floats "rx" (nullable (fun () -> pick [ 0.0; 1.0; 2.0; 3.0; 5.0 ]) idx);
      Series.of_ints "s" (Array.init n (fun k -> 100 + k)) (* collides with left "s" *) ]

let rows_of df = Array.of_list (List.map Array.of_list (Dataframe.to_rows df))

(* ---- group by ------------------------------------------------------------- *)

let key_eq a b = List.for_all2 V.equal a b

let naive_groups (keys : int list) (rows : V.t array array) : (V.t list * V.t array list) list =
  (* groups in order of first appearance; rows in order *)
  let groups : (V.t list * V.t array list ref) list ref = ref [] in
  Array.iter
    (fun row ->
      let k = List.map (fun c -> row.(c)) keys in
      match List.find_opt (fun (k', _) -> key_eq k k') !groups with
      | Some (_, cell) -> cell := row :: !cell
      | None -> groups := !groups @ [ (k, ref [ row ]) ])
    rows;
  List.map (fun (k, cell) -> (k, List.rev !cell)) !groups

let nn l = List.filter (fun v -> not (V.is_null v)) l
let fold_best better l =
  List.fold_left (fun acc v -> if V.is_null acc || better (V.compare v acc) then v else acc) V.Null (nn l)

let lt c = c < 0
let gt c = c > 0

let to_f = function V.Int i -> float_of_int i | V.Float f -> f | _ -> assert false

let aggs : (string * Expr.t * (V.t list -> V.t)) list =
  let open Expr in
  [ ("n", len (), fun vs -> V.Int (List.length vs));
    ("si", sum (col "i"), fun vs -> V.Int (List.fold_left (fun a v -> match v with V.Int x -> Stdlib.( + ) a x | _ -> a) 0 vs));
    ("sf", sum (col "f"), fun vs -> V.Float (List.fold_left (fun a v -> match v with V.Float x -> a +. x | _ -> a) 0.0 vs));
    ("cs", count (col "s"), fun vs -> V.Int (List.length (nn vs)));
    ("mini", min (col "i"), fold_best lt);
    ("maxi", max (col "i"), fold_best gt);
    ("minf", min (col "f"), fold_best lt);
    ("maxf", max (col "f"), fold_best gt);
    ("mf", mean (col "f"), fun vs -> match nn vs with [] -> V.Null | l -> V.Float (List.fold_left (fun a v -> a +. to_f v) 0.0 l /. float_of_int (List.length l)));
    ("mi", mean (col "i"), fun vs -> match nn vs with [] -> V.Null | l -> V.Float (List.fold_left (fun a v -> a +. to_f v) 0.0 l /. float_of_int (List.length l)));
    ("fs", first (col "s"), fun vs -> match vs with [] -> V.Null | v :: _ -> v);
    ("ls", last (col "s"), fun vs -> match List.rev vs with [] -> V.Null | v :: _ -> v);
    ("ns", n_unique (col "s"), fun vs -> V.Int (List.length (List.fold_left (fun acc v -> if List.exists (V.equal v) acc then acc else v :: acc) [] vs)));
    ("mins", min (col "s"), fold_best lt);
    ("maxs", max (col "s"), fold_best gt);
    ("minb", min (col "b"), fold_best lt) ]

(* which input column each aggregate reads *)
let agg_input = function
  | "n" -> None | "si" | "mini" | "maxi" | "mi" -> Some 0 | "sf" | "minf" | "maxf" | "mf" -> Some 2
  | "cs" | "fs" | "ls" | "ns" | "mins" | "maxs" -> Some 3 | "minb" -> Some 4 | _ -> None

let test_group_by () =
  let names = [ "i"; "big"; "f"; "s"; "b" ] in
  for it = 1 to 600 do
    let n = if it mod 20 = 0 then 0 else Random.int 70 in
    let df = gen_left n in
    let rows = rows_of df in
    let nk = Random.int 4 in  (* 0..3 keys; 0 = no keys *)
    let keys = List.sort_uniq compare (List.init nk (fun _ -> pick names)) in
    let key_idx = List.map (fun k -> let rec find i = function [] -> assert false | x :: _ when x = k -> i | _ :: r -> find (i + 1) r in find 0 names) keys in
    let expected_groups =
      if keys = [] then [ ([], Array.to_list rows) ] else naive_groups key_idx rows in
    let expected =
      List.map
        (fun (k, grows) ->
          k @ List.map (fun (nm, _, f) ->
                let col = match agg_input nm with None -> 0 | Some c -> c in
                f (List.map (fun r -> r.(col)) grows)) aggs)
        expected_groups
    in
    incr checks;
    let got =
      match
        L.of_df df |> L.group_by (List.map Expr.col keys)
        |> L.agg (List.map (fun (nm, e, _) -> Expr.alias nm e) aggs)
        |> L.collect
      with
      | exception Err.Camels_error m -> fail "group_by #%d keys=[%s]: error %s" it (String.concat "," keys) m; []
      | d -> Dataframe.to_rows d
    in
    if not (rows_eq expected got) then
      fail "group_by #%d keys=[%s] n=%d MISMATCH\n  expected %s\n  got      %s" it (String.concat "," keys) n
        (rows_str expected) (rows_str got)
  done

(* ---- join ------------------------------------------------------------------ *)

let key_pairs = [ (0, 0); (1, 1); (2, 2); (3, 3); (4, 4); (0, 5) ] (* left col, right col; last is int vs float *)
let lnames = [| "i"; "big"; "f"; "s"; "b"; "lp" |]
let rnames = [| "ri"; "rbig"; "rf"; "rs"; "rb"; "rx"; "s" |]

let test_join () =
  for it = 1 to 800 do
    let nl = if it mod 25 = 0 then 0 else Random.int 30 and nr = if it mod 31 = 0 then 0 else Random.int 30 in
    let l = gen_left nl and r = gen_right nr in
    let lrows = rows_of l and rrows = rows_of r in
    let nkeys = 1 + Random.int 2 in
    let pairs = List.sort_uniq compare (List.init nkeys (fun _ -> pick key_pairs)) in
    (* a column may only be a key once on each side *)
    let pairs = List.fold_left (fun acc (a, b) -> if List.exists (fun (a', b') -> a = a' || b = b') acc then acc else acc @ [ (a, b) ]) [] pairs in
    let how = pick [ L.Inner; L.Left; L.Full ] in
    let matches li =
      let acc = ref [] in
      Array.iteri
        (fun ri rrow ->
          if List.for_all (fun (a, b) ->
                 let x = lrows.(li).(a) and y = rrow.(b) in
                 (not (V.is_null x)) && (not (V.is_null y)) && V.compare x y = 0) pairs
          then acc := ri :: !acc)
        rrows;
      List.rev !acc
    in
    let right_hit = Array.make nr false in
    let pairs_out = ref [] in
    for li = 0 to nl - 1 do
      match matches li with
      | [] -> if how <> L.Inner then pairs_out := (li, -1) :: !pairs_out
      | ms -> List.iter (fun ri -> right_hit.(ri) <- true; pairs_out := (li, ri) :: !pairs_out) ms
    done;
    if how = L.Full then Array.iteri (fun ri h -> if not h then pairs_out := (-1, ri) :: !pairs_out) right_hit;
    let pairs_out = List.rev !pairs_out in
    let right_keys = List.map snd pairs in
    let right_out_cols = List.filter (fun c -> not (List.mem c right_keys)) [ 0; 1; 2; 3; 4; 5; 6 ] in
    let expected =
      List.map
        (fun (li, ri) ->
          let left_part =
            List.init 6 (fun c ->
                match List.find_opt (fun (a, _) -> a = c) pairs with
                | Some (_, b) when how = L.Full && li < 0 -> rrows.(ri).(b)
                | _ -> if li < 0 then V.Null else lrows.(li).(c))
          in
          let right_part = List.map (fun c -> if ri < 0 then V.Null else rrows.(ri).(c)) right_out_cols in
          left_part @ right_part)
        pairs_out
    in
    let left_on = List.map (fun (a, _) -> lnames.(a)) pairs and right_on = List.map (fun (_, b) -> rnames.(b)) pairs in
    incr checks;
    let got =
      match L.of_df l |> L.join ~how ~left_on ~right_on (L.of_df r) |> L.collect with
      | exception Err.Camels_error m -> fail "join #%d: error %s" it m; []
      | d -> Dataframe.to_rows d
    in
    if not (rows_eq expected got) then
      fail "join #%d how=%s on [%s]=[%s] nl=%d nr=%d MISMATCH\n  expected %s\n  got      %s" it
        (match how with L.Inner -> "inner" | L.Left -> "left" | L.Full -> "full")
        (String.concat "," left_on) (String.concat "," right_on) nl nr (rows_str expected) (rows_str got)
  done;
  (* dtype mismatch is an error, not a silent empty join *)
  incr checks;
  (match
     L.collect
       (L.of_df (Dataframe.create [ Series.of_strings "k" [| "a" |] ])
        |> L.join ~on:[ "k" ] (L.of_df (Dataframe.create [ Series.of_ints "k" [| 1 |] ])))
   with
   | exception Err.Camels_error _ -> ()
   | _ -> fail "join on str vs int should raise")

let () =
  Random.init 777;
  test_group_by ();
  test_join ();
  Printf.printf "keys: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
