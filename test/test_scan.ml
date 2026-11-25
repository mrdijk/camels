(* The lazy CSV scan: equivalence with eager reading under tiny chunk sizes,
   and proof that projection / predicate pushdown reaches the file. *)
open Camels
module V = Value
module L = Lazyframe

let failures = ref 0
let checks = ref 0
let fail fmt = Printf.ksprintf (fun s -> incr failures; print_endline s) fmt
let ok name c = incr checks; if not c then fail "FAIL %s" name
let pick l = List.nth l (Random.int (List.length l))
let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

let value_same a b =
  match (a, b) with
  | V.Float x, V.Float y -> Int64.bits_of_float x = Int64.bits_of_float y
  | _ -> V.compare a b = 0 && V.is_null a = V.is_null b

let rows_same a b =
  Dataframe.column_names a = Dataframe.column_names b
  && Dataframe.height a = Dataframe.height b
  && List.for_all2 (fun x y -> List.for_all2 value_same x y) (Dataframe.to_rows a) (Dataframe.to_rows b)

let with_temp text f =
  let path = Filename.temp_file "polars" ".csv" in
  let oc = open_out_bin path in
  output_string oc text;
  close_out oc;
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)

let result f = match f () with v -> Ok v | exception Err.Camels_error m -> Error m

(* ---- random CSV with typed columns (quoted newlines, CRLF, nulls ...) ---- *)

let gen_csv () =
  let n = Random.int 60 in
  let eol = pick [ "\n"; "\r\n" ] in
  let b = Buffer.create 512 in
  Buffer.add_string b ("id,cat,x,flag,note" ^ eol);
  for i = 0 to n - 1 do
    let cat = pick [ "a"; "b"; "c"; "" ] in
    let x = if Random.int 6 = 0 then "" else Printf.sprintf "%.2f" (Random.float 100.) in
    let flag = if Random.int 8 = 0 then "" else pick [ "true"; "false" ] in
    let note = pick [ "plain"; "\"with,comma\""; "\"multi\nline\""; "\"say \"\"hi\"\"\""; "" ] in
    Buffer.add_string b (Printf.sprintf "%d,%s,%s,%s,%s%s" i cat x flag note eol)
  done;
  Buffer.contents b

let queries : (string * (L.t -> L.t)) list =
  let open Expr in
  [ ("filter+select", fun q -> q |> L.filter (col "x" > float 50.) |> L.select [ col "id"; col "cat" ]);
    ("filter on string, select other", fun q -> q |> L.filter (col "cat" = str "a") |> L.select [ col "x" ]);
    ("filter on col not selected", fun q -> q |> L.filter (is_not_null (col "x") && col "flag" = bool true) |> L.select [ col "id" ]);
    ("select only", fun q -> q |> L.select [ col "note"; col "id" ]);
    ("with_column+filter", fun q -> q |> L.with_column (col "x" * float 2. |> alias "x2") |> L.filter (col "x2" > float 80.) |> L.select [ col "id"; col "x2" ]);
    ("group_by", fun q -> q |> L.filter (col "id" >= int 3) |> L.group_by [ col "cat" ] |> L.agg [ len () |> alias "n"; sum (col "x") |> alias "sx"; max (col "note") |> alias "mn" ]);
    ("all columns", fun q -> q);
    ("len only", fun q -> q |> L.select [ len () |> alias "n" ]) ]

let test_equivalence () =
  for it = 1 to 500 do
    let text = gen_csv () in
    with_temp text (fun path ->
        let eager = Io.read_csv ~infer_rows:max_int path in
        let schema = List.combine (Dataframe.column_names eager)
            (Array.to_list (Array.map Series.dtype eager.Dataframe.columns)) in
        List.iter
          (fun (qname, q) ->
            let chunk_bytes = pick [ 16; 23; 40; 64; 150; 1_000_000 ] in
            let expected = result (fun () -> L.collect (q (L.of_df eager))) in
            let lazy_opt = result (fun () -> L.collect (q (Io.scan_csv ~schema ~chunk_bytes path))) in
            let lazy_noopt = result (fun () -> L.collect ~optimize:false (q (Io.scan_csv ~schema ~chunk_bytes path))) in
            incr checks;
            (match (expected, lazy_opt, lazy_noopt) with
             | Ok e, Ok a, Ok b ->
                 if not (rows_same e a) then fail "scan #%d %S (optimised, chunk_bytes=%d) differs from eager\ntext=%S" it qname chunk_bytes text
                 else if not (rows_same e b) then fail "scan #%d %S (unoptimised, chunk_bytes=%d) differs from eager\ntext=%S" it qname chunk_bytes text
             | Error _, Error _, Error _ -> ()
             | _ -> fail "scan #%d %S: error status differs (chunk_bytes=%d)\ntext=%S" it qname chunk_bytes text))
          queries)
  done

(* ---- pushdown reaches the file ---- *)

let test_pushdown () =
  let text = "a,junk,k\n1,5,x\n2,6,y\n3,oops,x\n" in
  with_temp text (fun path ->
      (* junk is inferred as an int from the first 2 rows; row 3 does not fit *)
      let q = Io.scan_csv ~infer_rows:2 path |> L.select [ Expr.col "a" ] in
      ok "projection pushdown: the unused column is never parsed"
        (match result (fun () -> L.collect q) with
         | Ok df -> Dataframe.to_rows df = [ [ V.Int 1 ]; [ V.Int 2 ]; [ V.Int 3 ] ]
         | Error _ -> false);
      ok "...whereas without the optimizer it is parsed (and fails)"
        (match result (fun () -> L.collect ~optimize:false q) with Error _ -> true | Ok _ -> false);
      let q2 = Io.scan_csv ~infer_rows:2 path |> L.filter Expr.(col "a" > int 1) |> L.select [ Expr.col "k" ] in
      let plan = L.explain q2 in
      ok "explain names the CSV scan" (contains plan "CSV SCAN");
      ok "predicate is pushed into the scan" (contains plan "SELECTION: (col(\"a\") > 1)");
      ok "only the needed columns are projected" (contains plan "PROJECT 1/3 COLUMNS" || contains plan "PROJECT 2/3 COLUMNS");
      ok "filter on a column that is not selected still works"
        (match result (fun () -> L.collect q2) with
         | Ok df -> Dataframe.to_rows df = [ [ V.Str "y" ]; [ V.Str "x" ] ]
         | Error _ -> false);
      ok "scan_csv reads the header at scan time (missing file fails early)"
        (match result (fun () -> Io.scan_csv "/definitely/not/here.csv") with Error _ -> true | Ok _ -> false);
      ok "unknown column is reported at collect, as for any plan"
        (match result (fun () -> L.collect (Io.scan_csv path |> L.select [ Expr.col "nope" ])) with Error _ -> true | Ok _ -> false);
      ok "schema is available without reading the data"
        (L.schema (Io.scan_csv path) = [ "a"; "junk"; "k" ]));
  (* empty / header-only files *)
  with_temp "a,b\n" (fun path ->
      let df = L.collect (Io.scan_csv path) in
      ok "header-only scan has columns and no rows" (Dataframe.width df = 2 && Dataframe.height df = 0));
  with_temp "" (fun path ->
      ok "empty file scan has no columns" (Dataframe.width (L.collect (Io.scan_csv path)) = 0));
  (* a long record forces the buffer to grow *)
  let long = String.make 5000 'z' in
  with_temp ("a,b\n1," ^ long ^ "\n2,y\n") (fun path ->
      let df = L.collect (Io.scan_csv ~chunk_bytes:64 path) in
      ok "record longer than the chunk"
        (match Dataframe.to_rows df with [ [ V.Int 1; V.Str s ]; [ V.Int 2; V.Str "y" ] ] -> s = long | _ -> false))

let test_source () =
  let df = Dataframe.create [ Series.of_ints "a" [| 1 |]; Series.of_opt_strings "b" [| None |] ] in
  ok "Source.schema of an in-memory frame"
    (Source.schema (Source.Memory df) = [ ("a", Dtype.Int64); ("b", Dtype.Utf8) ]);
  ok "Source.columns" (Source.columns (Source.Memory df) = [ "a"; "b" ]);
  ok "Source.describe (memory)" (Source.describe (Source.Memory df) = "DF [a, b]");
  with_temp "x,y\n1,2.5\n" (fun path ->
      match Io.scan_csv path |> fun lf -> lf with
      | _ ->
          let lf = Io.scan_csv path in
          ok "schema of a CSV scan, without reading rows" (L.schema lf = [ "x"; "y" ]);
          ok "explain describes the CSV source"
            (contains (L.explain lf) ("CSV SCAN " ^ path ^ " [x, y]")));
  (* the schema is a plain value: the same scan can be built by hand *)
  with_temp "x,y\n1,2.5\n3,4.5\n" (fun path ->
      let options = { Csv_options.sep = ','; quote = '"'; has_header = true; null_values = [ "" ] } in
      let src = Source.Csv { path; options; schema = [ ("x", Dtype.Int64); ("y", Dtype.Float64) ]; chunk_bytes = 1024 } in
      let df = L.collect (L.of_source src) in
      ok "hand-built CSV source" (Dataframe.to_rows df = [ [ V.Int 1; V.Float 2.5 ]; [ V.Int 3; V.Float 4.5 ] ]))

let () =
  Random.init 99;
  test_source ();
  test_equivalence ();
  test_pushdown ();
  Printf.printf "scan: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
