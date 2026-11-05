open Camels
module L = Lazyframe
module V = Value

let failures = ref 0
let checks = ref 0

let row_to_string r = "[" ^ String.concat "; " (List.map V.to_string r) ^ "]"
let rows_equal a b =
  List.length a = List.length b
  && List.for_all2 (fun r1 r2 -> List.length r1 = List.length r2 && List.for_all2 V.equal r1 r2) a b

let check_rows name expected lf =
  incr checks;
  let got = Dataframe.to_rows (L.collect lf) in
  let got_noopt = Dataframe.to_rows (L.collect ~optimize:false lf) in
  if not (rows_equal expected got) then begin
    incr failures;
    Printf.printf "FAIL %s\n  expected: %s\n  got:      %s\n" name
      (String.concat " " (List.map row_to_string expected))
      (String.concat " " (List.map row_to_string got))
  end
  else if not (rows_equal got got_noopt) then begin
    incr failures;
    Printf.printf "FAIL %s: optimised and unoptimised results differ\n" name
  end

let check_cols name expected lf =
  incr checks;
  let got = Dataframe.column_names (L.collect lf) in
  if got <> expected then begin
    incr failures;
    Printf.printf "FAIL %s: columns [%s]\n" name (String.concat "," got)
  end

let check_true name b =
  incr checks;
  if not b then (incr failures; Printf.printf "FAIL %s\n" name)

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

let raises_error f = try ignore (f ()); false with Err.Polars_error _ -> true

let i x = V.Int x and f x = V.Float x and s x = V.Str x

let emp =
  Dataframe.create
    [
      Series.of_ints "id" [| 1; 2; 3; 4; 5; 6 |];
      Series.of_strings "name" [| "Alice"; "Bob"; "Cara"; "Dan"; "Eve"; "Fay" |];
      Series.of_opt_ints "dept_id" [| Some 10; Some 10; Some 20; Some 20; Some 30; None |];
      Series.of_floats "salary" [| 100.; 200.; 150.; 50.; 300.; 80. |];
    ]

let dept =
  Dataframe.create
    [
      Series.of_ints "dept_id" [| 10; 20; 40 |];
      Series.of_strings "dept_name" [| "Eng"; "Ops"; "HR" |];
      Series.of_strings "region" [| "EU"; "US"; "EU" |];
    ]

let () =
  let open Expr in
  let e = L.of_df emp and d = L.of_df dept in

  (* ---- filter + project ---- *)
  check_rows "filter+select"
    [ [ s "Bob"; f 200. ]; [ s "Cara"; f 150. ]; [ s "Eve"; f 300. ] ]
    (e |> L.filter (col "salary" > float 100.) |> L.select [ col "name"; col "salary" ]);

  check_rows "filter on V.Null key drops null"
    [ [ i 1 ]; [ i 2 ] ]
    (e |> L.filter (col "dept_id" = int 10) |> L.select [ col "id" ]);

  check_rows "filter with and/or"
    [ [ i 2 ]; [ i 4 ] ]
    (e |> L.filter ((col "salary" > float 150. && col "dept_id" = int 10) || col "salary" < float 60.)
       |> L.select [ col "id" ]);

  check_rows "alias in select"
    [ [ f 50. ]; [ f 25. ] ]
    (e |> L.filter (col "id" <= int 4) |> L.filter (col "dept_id" = int 10 || col "id" = int 4)
       |> L.filter (col "id" <> int 2 && col "id" <> int 3)
       |> L.select [ (col "salary" / float 2. |> alias "half") ]);

  (* ---- with_column ---- *)
  check_rows "with_column new"
    [ [ i 1; f 50. ]; [ i 2; f 100. ] ]
    (e |> L.with_column (col "salary" / float 2. |> alias "half")
       |> L.filter (col "id" < int 3) |> L.select [ col "id"; col "half" ]);

  check_rows "with_column replaces in place"
    [ [ i 1; f 101. ]; [ i 2; f 201. ] ]
    (e |> L.with_column (col "salary" + int 1 |> alias "salary")
       |> L.filter (col "id" < int 3) |> L.select [ col "id"; col "salary" ]);

  check_cols "with_column keeps position"
    [ "id"; "name"; "dept_id"; "salary" ]
    (e |> L.with_column (col "salary" * int 2 |> alias "salary"));

  check_rows "with_column literal broadcast"
    [ [ i 1; s "x" ]; [ i 2; s "x" ] ]
    (e |> L.filter (col "id" < int 3) |> L.with_column (str "x" |> alias "tag")
       |> L.select [ col "id"; col "tag" ]);

  check_rows "with_column agg broadcast (filter must NOT be pushed below)"
    [ [ i 5; f (300. -. 880. /. 6.) ] ]
    (e |> L.with_column (col "salary" - mean (col "salary") |> alias "diff")
       |> L.filter (col "diff" > float 100.) |> L.select [ col "id"; col "diff" ]);

  check_rows "filter on pass-through col is still NOT pushed below an agg with_column"
    [ [ i 1; f (100. -. 880. /. 6.) ] ]
    (e |> L.with_column (col "salary" - mean (col "salary") |> alias "diff")
       |> L.filter (col "id" = int 1) |> L.select [ col "id"; col "diff" ]);

  (* ---- aggregate ---- *)
  check_rows "group_by agg (V.Null key is its own group, first-appearance order)"
    [ [ i 10; f 300.; i 2 ]; [ i 20; f 200.; i 2 ]; [ i 30; f 300.; i 1 ]; [ V.Null; f 80.; i 1 ] ]
    (e |> L.group_by [ col "dept_id" ]
       |> L.agg [ sum (col "salary") |> alias "total"; len () |> alias "n" ]);

  check_rows "multiple aggregations"
    [ [ i 10; f 150.; f 100.; f 200.; i 2 ]; [ i 20; f 100.; f 50.; f 150.; i 2 ] ]
    (e |> L.filter (col "dept_id" <= int 20)
       |> L.group_by [ col "dept_id" ]
       |> L.agg [ mean (col "salary") |> alias "avg"; min (col "salary") |> alias "lo";
                  max (col "salary") |> alias "hi"; count (col "id") |> alias "n" ]);

  check_rows "agg expression over aggregates"
    [ [ i 10; f 150. ] ]
    (e |> L.filter (col "dept_id" = int 10) |> L.group_by [ col "dept_id" ]
       |> L.agg [ (sum (col "salary") / count (col "salary") |> alias "avg2") ]);

  check_rows "agg of computed expression, key expression"
    [ [ V.Bool false; f 230. ]; [ V.Bool true; f 650. ] ]
    (e |> L.group_by [ (col "salary" >= float 150. |> alias "rich") ]
       |> L.agg [ sum (col "salary" * float 1.) |> alias "t" ]);

  check_rows "select with only aggregations -> single row"
    [ [ f 880.; i 6 ] ]
    (e |> L.select [ sum (col "salary") |> alias "t"; len () |> alias "n" ]);

  check_rows "with_column float literal"
    [ [ i 1; f 880. ]; [ i 2; f 880. ] ]
    (e |> L.filter (col "id" < int 3) |> L.select [ col "id" ] |> L.with_column (float 880. |> alias "t"));

  check_rows "n_unique / first / last"
    [ [ i 4; i 1; i 6 ] ] (* null counts as a distinct value *)
    (e |> L.select [ n_unique (col "dept_id") |> alias "u"; first (col "id") |> alias "f";
                     last (col "id") |> alias "l" ]);

  check_rows "filter on key pushed below aggregate"
    [ [ i 20; i 2 ] ]
    (e |> L.group_by [ col "dept_id" ] |> L.agg [ len () |> alias "n" ]
       |> L.filter (col "dept_id" = int 20));

  check_rows "filter on agg result stays above"
    [ [ i 10; i 2 ]; [ i 20; i 2 ] ]
    (e |> L.group_by [ col "dept_id" ] |> L.agg [ len () |> alias "n" ]
       |> L.filter (col "n" > int 1));

  (* ---- join ---- *)
  check_rows "inner join"
    [ [ s "Alice"; s "Eng" ]; [ s "Bob"; s "Eng" ]; [ s "Cara"; s "Ops" ]; [ s "Dan"; s "Ops" ] ]
    (e |> L.join ~on:[ "dept_id" ] d |> L.select [ col "name"; col "dept_name" ]);

  check_rows "left join keeps unmatched left rows"
    [ [ s "Alice"; s "Eng" ]; [ s "Bob"; s "Eng" ]; [ s "Cara"; s "Ops" ]; [ s "Dan"; s "Ops" ];
      [ s "Eve"; V.Null ]; [ s "Fay"; V.Null ] ]
    (e |> L.join ~how:L.Left ~on:[ "dept_id" ] d |> L.select [ col "name"; col "dept_name" ]);

  check_rows "full join coalesces keys"
    [ [ i 10; s "Alice" ]; [ i 10; s "Bob" ]; [ i 20; s "Cara" ]; [ i 20; s "Dan" ];
      [ i 30; s "Eve" ]; [ V.Null; s "Fay" ]; [ i 40; V.Null ] ]
    (e |> L.join ~how:L.Full ~on:[ "dept_id" ] d |> L.select [ col "dept_id"; col "name" ]);

  check_rows "join with differently named keys"
    [ [ s "Alice"; s "EU" ] ]
    (let dept2 = Dataframe.create [ Series.of_ints "did" [| 10 |]; Series.of_strings "region" [| "EU" |] ] in
     e |> L.filter (col "id" = int 1)
     |> L.join ~left_on:[ "dept_id" ] ~right_on:[ "did" ] (L.of_df dept2)
     |> L.select [ col "name"; col "region" ]);

  check_cols "colliding column is suffixed"
    [ "id"; "name"; "dept_id"; "salary"; "name_right" ]
    (let o = Dataframe.create [ Series.of_ints "id" [| 1 |]; Series.of_strings "name" [| "z" |] ] in
     e |> L.join ~on:[ "id" ] (L.of_df o));

  check_rows "suffix survives projection pushdown"
    [ [ s "Alice"; s "z" ] ]
    (let o = Dataframe.create [ Series.of_ints "id" [| 1 |]; Series.of_strings "name" [| "z" |] ] in
     e |> L.join ~on:[ "id" ] (L.of_df o) |> L.select [ col "name"; col "name_right" ]);

  check_rows "multi-key join"
    [ [ s "Alice" ] ]
    (let o = Dataframe.create [ Series.of_ints "id" [| 1; 2 |]; Series.of_ints "dept_id" [| 10; 99 |] ] in
     e |> L.join ~on:[ "id"; "dept_id" ] (L.of_df o) |> L.select [ col "name" ]);

  (* ---- full pipeline ---- *)
  let pipeline =
    e
    |> L.join ~on:[ "dept_id" ] d
    |> L.filter (col "region" = str "EU" && col "salary" > float 100.)
    |> L.with_column (col "salary" * float 2. |> alias "double")
    |> L.group_by [ col "dept_name" ]
    |> L.agg [ mean (col "double") |> alias "avg_double"; len () |> alias "n" ]
  in
  check_rows "full pipeline" [ [ s "Eng"; f 400.; i 1 ] ] pipeline;

  let plan = L.explain pipeline in
  check_true "predicate pushed into right scan"
    (contains plan {|SELECTION: (col("region") == "EU")|});
  check_true "predicate pushed into left scan"
    (contains plan {|SELECTION: (col("salary") > 100.0)|});
  check_true "projection pushdown prunes left scan" (contains plan "PROJECT 2/4 COLUMNS");
  check_true "projection pushdown prunes right scan" (contains plan "PROJECT 2/3 COLUMNS");
  check_true "unoptimised plan has FILTER" (contains (L.explain ~optimize:false pipeline) "FILTER");

  (* ---- errors ---- *)
  check_true "unknown column is an error"
    (raises_error (fun () -> L.collect (e |> L.select [ col "nope" ])));
  check_true "type error"
    (raises_error (fun () -> L.collect (e |> L.filter (col "name" > int 3))));
  check_true "non-bool filter"
    (raises_error (fun () -> L.collect (e |> L.filter (col "salary"))));
  check_true "non-key column outside agg"
    (raises_error (fun () -> L.collect (e |> L.group_by [ col "dept_id" ] |> L.agg [ col "name" ])));
  check_true "duplicate output name"
    (raises_error (fun () -> L.collect (e |> L.select [ col "id"; col "id" ])));
  check_true "bad join spec"
    (raises_error (fun () -> L.collect (e |> L.join d)));
  check_true "laziness: errors only at collect"
    (let _ = e |> L.select [ col "nope" ] in true)


(* ------------------------------------------------------------------ *)
(* Differential fuzzing: optimised plan must equal unoptimised plan.  *)
(* ------------------------------------------------------------------ *)

let base =
  let n = 40 in
  let opt_int m = Array.init n (fun _ -> if Random.int 6 = 0 then None else Some (Random.int m)) in
  Dataframe.create
    [
      Series.of_opt_ints "a" (opt_int 10);
      Series.of_opt_ints "b" (opt_int 5);
      Series.of_opt_floats "f" (Array.init n (fun _ -> if Random.int 6 = 0 then None else Some (float_of_int (Random.int 20) /. 2.)));
      Series.of_opt_ints "k" (opt_int 6);
      Series.of_opt_strings "s" (Array.init n (fun _ -> if Random.int 6 = 0 then None else Some (String.make 1 (Char.chr (97 + Random.int 3)))));
    ]

let dim =
  Dataframe.create
    [
      Series.of_ints "k" [| 0; 1; 2; 3; 7 |];
      Series.of_ints "a" [| 100; 101; 102; 103; 107 |]; (* collides with base "a" *)
      Series.of_ints "d" [| 5; 6; 7; 8; 9 |];
    ]

(* expression-building operators, so we can still use Stdlib ints freely *)
let ( +: ) = Expr.( + )
let ( -: ) = Expr.( - )
let ( *: ) = Expr.( * )
let ( >: ) = Expr.( > )
let ( <=: ) = Expr.( <= )
let ( =: ) = Expr.( = )
let ( <>: ) = Expr.( <> )
let ( &&: ) = Expr.( && )
let ( ||: ) = Expr.( || )

type st = { lf : L.t; cols : (string * bool) list; joined : bool; ctr : int }

let pick l = List.nth l (Random.int (List.length l))
let nums st = List.filter_map (fun (n, b) -> if b then Some n else None) st.cols
let strs st = List.filter_map (fun (n, b) -> if b then None else Some n) st.cols

let rec gen_num st depth =
  match Random.int (if depth = 0 then 2 else 5) with
  | 0 -> if nums st = [] then Expr.int 1 else Expr.col (pick (nums st))
  | 1 -> Expr.int (Random.int 8)
  | 2 -> gen_num st (depth - 1) +: gen_num st (depth - 1)
  | 3 -> gen_num st (depth - 1) -: gen_num st (depth - 1)
  | _ -> gen_num st (depth - 1) *: gen_num st (depth - 1)

let rec gen_pred st depth =
  match Random.int (if depth = 0 then 5 else 7) with
  | 0 -> gen_num st 1 >: gen_num st 1
  | 1 -> gen_num st 1 <=: gen_num st 1
  | 2 -> gen_num st 1 =: gen_num st 0
  | 3 -> Expr.is_not_null (Expr.col (pick (List.map fst st.cols)))
  | 4 -> (
      match strs st with
      | [] -> gen_num st 0 <>: Expr.int 3
      | ss -> Expr.col (pick ss) =: Expr.str (String.make 1 (Char.chr (97 + Random.int 3))))
  | 5 -> gen_pred st (depth - 1) &&: gen_pred st (depth - 1)
  | _ -> gen_pred st (depth - 1) ||: gen_pred st (depth - 1)

let step st =
  let fresh p = (Printf.sprintf "%s%d" p st.ctr, st.ctr + 1) in
  match Random.int 5 with
  | 0 | 1 -> { st with lf = L.filter (gen_pred st 2) st.lf }
  | 2 ->
      let name, ctr, replace =
        if Random.bool () || nums st = [] then (let n, c = fresh "w" in (n, c, false))
        else (pick (nums st), st.ctr, true)
      in
      let e =
        if Random.int 4 = 0 then
          gen_num st 0 -: Expr.mean (gen_num st 0)
        else gen_num st 2
      in
      let cols =
        if replace then List.map (fun (n, b) -> if n = name then (n, true) else (n, b)) st.cols
        else st.cols @ [ (name, true) ]
      in
      { st with lf = L.with_column (Expr.alias name e) st.lf; cols; ctr }
  | 3 ->
      let chosen = List.filter (fun _ -> Random.int 3 > 0) st.cols in
      let chosen = if chosen = [] then [ List.hd st.cols ] else chosen in
      let name, ctr = fresh "c" in
      let extra = Random.bool () in
      let exprs =
        List.map (fun (n, _) -> Expr.col n) chosen
        @ (if extra then
             [ Expr.alias name (if Random.bool () then gen_num st 1 else Expr.sum (gen_num st 1)) ]
           else [])
      in
      { st with lf = L.select exprs st.lf;
        cols = chosen @ (if extra then [ (name, true) ] else []); ctr }
  | _ ->
      if st.joined || not (List.mem_assoc "k" st.cols) then st
      else
        let how = pick [ L.Inner; L.Left; L.Full ] in
        let lnames = List.map fst st.cols in
        let rn =
          List.map (fun n -> if List.mem n lnames then (n ^ "_right", true) else (n, true)) [ "a"; "d" ]
        in
        { st with lf = L.join ~how ~on:[ "k" ] (L.of_df dim) st.lf;
          cols = st.cols @ rn; joined = true }

(* group-by: keys are plain columns, aggregates may use any column inside sum() *)
let group_step st =
  let nk = 1 + Random.int 2 in
  let keys = List.sort_uniq compare (List.init nk (fun _ -> fst (pick st.cols))) in
  let s1 = Printf.sprintf "s%d" st.ctr and n1 = Printf.sprintf "n%d" st.ctr in
  let lf =
    st.lf
    |> L.group_by (List.map Expr.col keys)
    |> L.agg [ Expr.alias s1 (Expr.sum (gen_num st 1)); Expr.alias n1 (Expr.len ()) ]
  in
  { st with lf;
    cols = List.filter (fun (n, _) -> List.mem n keys) st.cols @ [ (s1, true); (n1, true) ];
    ctr = st.ctr + 1 }

let step st = if Random.int 6 = 0 then group_step st else step st

let fuzz iterations =
  Random.init 12345;
  for it = 1 to iterations do
    let st0 = { lf = L.of_df base; cols = [ ("a", true); ("b", true); ("f", true); ("k", true); ("s", false) ]; joined = false; ctr = 0 } in
    let st = ref st0 in
    for _ = 1 to 1 + Random.int 6 do st := step !st done;
    incr checks;
    match
      (try Ok (Dataframe.to_rows (L.collect !st.lf), Dataframe.to_rows (L.collect ~optimize:false !st.lf))
       with Err.Polars_error m -> Error m)
    with
    | Ok (a, b) ->
        if not (rows_equal a b) then begin
          incr failures;
          Printf.printf "FUZZ MISMATCH #%d\n--- unoptimised plan:\n%s--- optimised plan:\n%s\n" it
            (L.explain ~optimize:false !st.lf) (L.explain !st.lf)
        end
    | Error m ->
        incr failures;
        Printf.printf "FUZZ ERROR #%d: %s\n%s\n" it m (L.explain ~optimize:false !st.lf)
  done

let () =
  fuzz 3000;
  Printf.printf "%d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
