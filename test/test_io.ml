open Polars
module V = Value
module L = Lazy_frame

let failures = ref 0
let checks = ref 0

let row_str r = "[" ^ String.concat "; " (List.map V.to_string r) ^ "]"
let rows_str rows = String.concat " " (List.map row_str rows)

let ok name cond =
  incr checks;
  if not cond then (incr failures; Printf.printf "FAIL %s\n" name)

let rows_eq a b =
  List.length a = List.length b
  && List.for_all2 (fun r1 r2 -> List.length r1 = List.length r2 && List.for_all2 V.equal r1 r2) a b

let check name ?sep ?quote ?has_header ?null_values ?infer_rows ?schema ?names ?dtypes text rows =
  incr checks;
  match Io.read_csv_string ?sep ?quote ?has_header ?null_values ?infer_rows ?schema text with
  | exception Err.Polars_error m -> incr failures; Printf.printf "FAIL %s: error %s\n" name m
  | df ->
      let got = Dataframe.to_rows df in
      if not (rows_eq got rows) then begin
        incr failures;
        Printf.printf "FAIL %s\n  expected %s\n  got      %s\n" name (rows_str rows) (rows_str got)
      end;
      (match names with
       | Some n when Dataframe.column_names df <> n ->
           incr failures; Printf.printf "FAIL %s: names [%s]\n" name (String.concat "," (Dataframe.column_names df))
       | _ -> ());
      (match dtypes with
       | Some d ->
           let got = Array.to_list (Array.map Series.dtype df.columns) in
           if got <> d then (incr failures; Printf.printf "FAIL %s: dtypes [%s]\n" name
                              (String.concat "," (List.map Dtype.to_string got)))
       | None -> ())

let raises name f =
  incr checks;
  match f () with
  | exception Err.Polars_error _ -> ()
  | _ -> incr failures; Printf.printf "FAIL %s: expected an error\n" name

let i x = V.Int x and f x = V.Float x and s x = V.Str x and b x = V.Bool x and nul = V.Null

let () =
  check "basic types" "id,name,score,ok\n1,Ann,1.5,true\n2,Bob,,false\n"
    ~names:[ "id"; "name"; "score"; "ok" ]
    ~dtypes:[ Dtype.Int64; Dtype.Utf8; Dtype.Float64; Dtype.Boolean ]
    [ [ i 1; s "Ann"; f 1.5; b true ]; [ i 2; s "Bob"; nul; b false ] ];

  check "quoting: separator, escaped quote, newline"
    "a,b\n\"x,y\",\"he said \"\"hi\"\"\"\n\"line1\nline2\",z\n"
    [ [ s "x,y"; s "he said \"hi\"" ]; [ s "line1\nline2"; s "z" ] ];

  check "CRLF + BOM + no trailing newline" "\xEF\xBB\xBFa,b\r\n1,2\r\n3,4"
    ~names:[ "a"; "b" ] [ [ i 1; i 2 ]; [ i 3; i 4 ] ];

  check "blank lines skipped" "a\n1\n\n2\n" [ [ i 1 ]; [ i 2 ] ];

  check "quoted empty is a string, unquoted empty is null" "a,b\n\"\",\n"
    ~dtypes:[ Dtype.Utf8; Dtype.Utf8 ] [ [ s ""; nul ] ];

  check "trailing separator gives a null field" "a,b\n1,\n" [ [ i 1; nul ] ];

  check "custom null values" ~null_values:[ "NA"; "" ] "a\nNA\n3\n"
    ~dtypes:[ Dtype.Int64 ] [ [ nul ]; [ i 3 ] ];

  check "NA header is not turned into null" ~null_values:[ "NA" ] "NA,b\n1,2\n" ~names:[ "NA"; "b" ] [ [ i 1; i 2 ] ];

  check "quoted NA stays a string" ~null_values:[ "NA" ] "a\n\"NA\"\nx\n" [ [ s "NA" ]; [ s "x" ] ];

  check "semicolon separator" ~sep:';' "a;b\n1;x,y\n" [ [ i 1; s "x,y" ] ];
  check "tab separator" ~sep:'\t' "a\tb\n1\t2\n" [ [ i 1; i 2 ] ];
  check "single quote char" ~quote:'\'' "a,b\n'x,y',2\n" [ [ s "x,y"; i 2 ] ];

  check "no header" ~has_header:false "1,2\n3,4\n" ~names:[ "column_1"; "column_2" ]
    [ [ i 1; i 2 ]; [ i 3; i 4 ] ];

  check "schema override keeps leading zeros" ~schema:[ ("id", Dtype.Utf8) ] "id\n007\n" [ [ s "007" ] ];
  check "default infers 007 as int" "id\n007\n" [ [ i 7 ] ];
  check "schema float for int-looking" ~schema:[ ("x", Dtype.Float64) ] "x\n5\n" [ [ f 5. ] ];

  check "int/float mix promotes" "x\n1\n2.5\n" ~dtypes:[ Dtype.Float64 ] [ [ f 1. ]; [ f 2.5 ] ];
  check "huge int becomes float" "x\n99999999999999999999\n" ~dtypes:[ Dtype.Float64 ] [ [ f 1e20 ] ];
  check "signs" "x\n+5\n-3\n" ~dtypes:[ Dtype.Int64 ] [ [ i 5 ]; [ i (-3) ] ];
  check "float syntaxes" "x\n1e3\n.5\n-2.\n" ~dtypes:[ Dtype.Float64 ] [ [ f 1000. ]; [ f 0.5 ]; [ f (-2.) ] ];
  check "hex stays a string" "x\n0x10\n" ~dtypes:[ Dtype.Utf8 ] [ [ s "0x10" ] ];
  check "underscores stay a string" "x\n1_000\n" ~dtypes:[ Dtype.Utf8 ] [ [ s "1_000" ] ];
  check "bool any case" "x\nTRUE\nFalse\n" ~dtypes:[ Dtype.Boolean ] [ [ b true ]; [ b false ] ];
  check "mixed bool/int is string" "x\ntrue\n1\n" ~dtypes:[ Dtype.Utf8 ] [ [ s "true" ]; [ s "1" ] ];
  check "whitespace is not trimmed" "x\n 5\n" ~dtypes:[ Dtype.Utf8 ] [ [ s " 5" ] ];

  check "infer_rows too small -> error is caught below; large -> string" "x\n1\n2\nabc\n"
    ~dtypes:[ Dtype.Utf8 ] [ [ s "1" ]; [ s "2" ]; [ s "abc" ] ];
  raises "later row does not fit inferred type" (fun () -> Io.read_csv_string ~infer_rows:2 "x\n1\n2\nabc\n");

  raises "ragged row" (fun () -> Io.read_csv_string "a,b\n1\n");
  raises "unterminated quote" (fun () -> Io.read_csv_string "a\n\"abc\n");
  raises "duplicate header" (fun () -> Io.read_csv_string "a,a\n1,2\n");
  raises "schema names unknown column" (fun () -> Io.read_csv_string ~schema:[ ("zz", Dtype.Int64) ] "a\n1\n");
  raises "schema value does not parse" (fun () -> Io.read_csv_string ~schema:[ ("a", Dtype.Int64) ] "a\nx\n");
  raises "missing file" (fun () -> Io.read_csv "/definitely/not/here.csv");

  ok "empty text -> empty frame" (Dataframe.width (Io.read_csv_string "") = 0);
  (let df = Io.read_csv_string "a,b\n" in
   ok "header only" (Dataframe.width df = 2 && Dataframe.height df = 0));

  (* file round trip + lazy pipeline *)
  let path = Filename.temp_file "polars" ".csv" in
  let oc = open_out_bin path in
  output_string oc "city,n\nAMS,1\nAMS,2\nRTM,5\nUTR,\n";
  close_out oc;
  let df = Io.read_csv path in
  ok "read_csv file" (Dataframe.height df = 4 && Dataframe.column_names df = [ "city"; "n" ]);
  let out =
    let open Expr in
    Io.scan_csv path
    |> L.filter (is_not_null (col "n"))
    |> L.group_by [ col "city" ]
    |> L.agg [ sum (col "n") |> alias "total" ]
    |> L.collect
  in
  ok "scan_csv + lazy query"
    (rows_eq (Dataframe.to_rows out) [ [ s "AMS"; i 3 ]; [ s "RTM"; i 5 ] ]);
  Sys.remove path;

  Printf.printf "io: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
