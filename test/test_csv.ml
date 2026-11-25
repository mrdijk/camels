(* Differential tests of the CSV engine against the original reader, plus
   chunk-boundary tests with tiny chunk sizes. *)
open Camels
module V = Value

let failures = ref 0
let checks = ref 0
let fail fmt = Printf.ksprintf (fun s -> incr failures; print_endline s) fmt
let pick l = List.nth l (Random.int (List.length l))

(* ---- random CSV text ----------------------------------------------------- *)

let float_forms =
  [ "1.5"; "-0.25"; "0.1"; "0.001"; "123456789.123456789"; "1e3"; "1E-2"; "-.5"; "7."; "+3.25";
    "12345678901234567890"; "0.30000000000000004"; "1e22"; "1e23"; "4.9e-324"; "1.7976931348623157e308";
    "9007199254740993"; "100"; "nan"; "-inf"; "2.2250738585072014e-308" ]

let word_pool = [ "a"; "bb"; "x y"; "he said \"hi\""; "c,d"; "line1\nline2"; "é"; "NA"; " "; "tab\there"; "" ]

let gen_field kind =
  match kind with
  | `Int ->
      pick [ string_of_int (Random.int 1000 - 500); "007"; "-0"; "+12"; "4611686018427387903";
             "-4611686018427387904"; "99999999999999999999"; "123456789012345678" ]
  | `Float -> pick float_forms
  | `Bool -> pick [ "true"; "false"; "TRUE"; "False" ]
  | `Str -> pick word_pool

let render_field ~sep ~quote_all s =
  let needs = String.contains s sep || String.contains s '"' || String.contains s '\n' || String.contains s '\r' in
  if needs || quote_all then "\"" ^ String.concat "\"\"" (String.split_on_char '"' s) ^ "\"" else s

let gen_csv () =
  let ncols = 1 + Random.int 4 and nrows = Random.int 25 in
  let sep = pick [ ','; ';'; '\t' ] in
  let kinds = Array.init ncols (fun _ -> pick [ `Int; `Float; `Bool; `Str; `Int; `Float ]) in
  let eol = pick [ "\n"; "\r\n"; "\r" ] in
  let quote_all = Random.int 6 = 0 in
  let has_header = Random.int 5 > 0 in
  let null_values = pick [ [ "" ]; [ ""; "NA" ]; [ "NA" ] ] in
  let b = Buffer.create 256 in
  if Random.int 8 = 0 then Buffer.add_string b "\xEF\xBB\xBF";
  let line cells = Buffer.add_string b (String.concat (String.make 1 sep) (List.map (render_field ~sep ~quote_all) cells)) in
  if has_header then begin
    line (List.init ncols (fun i -> Printf.sprintf "c%d" i));
    Buffer.add_string b eol
  end;
  for r = 0 to nrows - 1 do
    if Random.int 15 = 0 then Buffer.add_string b eol (* blank line *);
    line (List.init ncols (fun c ->
        if Random.int 7 = 0 then (if Random.bool () then "" else "NA") else gen_field kinds.(c)));
    if r < nrows - 1 || Random.bool () then Buffer.add_string b eol
  done;
  (Buffer.contents b, sep, has_header, null_values)

(* ---- comparison -------------------------------------------------------------- *)

let value_same a b =
  match (a, b) with
  | V.Float x, V.Float y -> Int64.bits_of_float x = Int64.bits_of_float y
  | _ -> V.compare a b = 0 && V.is_null a = V.is_null b

let frame_same a b =
  Dataframe.column_names a = Dataframe.column_names b
  && Array.map Series.dtype a.Dataframe.columns = Array.map Series.dtype b.Dataframe.columns
  && Dataframe.height a = Dataframe.height b
  &&
  let ra = Dataframe.to_rows a and rb = Dataframe.to_rows b in
  List.for_all2 (fun x y -> List.for_all2 value_same x y) ra rb

let describe df =
  Printf.sprintf "%s | %s" (String.concat "," (Dataframe.column_names df))
    (String.concat " " (List.map (fun r -> "[" ^ String.concat ";" (List.map V.to_string r) ^ "]") (Dataframe.to_rows df)))

let result f = match f () with df -> Ok df | exception Err.Camels_error m -> Error m

let test_vs_reference () =
  for it = 1 to 4000 do
    let text, sep, has_header, null_values = gen_csv () in
    let infer_rows = pick [ 1; 3; 1000 ] in
    incr checks;
    let expected = result (fun () -> Ref_csv.read_csv_string ~sep ~has_header ~null_values ~infer_rows text) in
    let got = result (fun () -> Io.read_csv_string ~sep ~has_header ~null_values ~infer_rows text) in
    match (expected, got) with
    | Ok a, Ok b -> if not (frame_same a b) then fail "csv #%d MISMATCH sep=%C text=%S\n  ref: %s\n  new: %s" it sep text (describe a) (describe b)
    | Error _, Error _ -> ()
    | Ok a, Error m -> fail "csv #%d new errors but reference succeeds sep=%C text=%S\n  error: %s\n  ref: %s" it sep text m (describe a)
    | Error m, Ok b -> fail "csv #%d reference errors (%s) but new succeeds sep=%C text=%S\n  new: %s" it m sep text (describe b)
  done

(* ---- floats: bit-exact against float_of_string -------------------------------- *)

let test_floats () =
  let digits n = String.init n (fun _ -> Char.chr (48 + Random.int 10)) in
  for _ = 1 to 20000 do
    let s =
      (if Random.bool () then "-" else "")
      ^ digits (Random.int 20)
      ^ (if Random.int 4 > 0 then "." ^ digits (Random.int 20) else "")
      ^ (if Random.int 3 = 0 then "e" ^ (if Random.bool () then "-" else "") ^ string_of_int (Random.int 330) else "")
    in
    if s <> "" && s <> "-" && s <> "." && s <> "-." then begin
      incr checks;
      let expected = float_of_string_opt s in
      let got =
        match result (fun () -> Io.read_csv_string ~schema:[ ("x", Dtype.Float64) ] ("x\n" ^ s ^ "\n")) with
        | Ok df -> (match Dataframe.to_rows df with [ [ V.Float f ] ] -> Some f | _ -> None)
        | Error _ -> None
      in
      match (expected, got) with
      | Some e, Some g -> if Int64.bits_of_float e <> Int64.bits_of_float g then fail "float %S: expected %h got %h" s e g
      | None, None -> ()
      | Some e, None -> fail "float %S: reader rejected a valid float (%h)" s e
      | None, Some g -> fail "float %S: reader accepted an invalid float (%h)" s g
    end
  done

(* ---- chunk boundaries -------------------------------------------------------------- *)

let with_temp text f =
  let path = Filename.temp_file "polars" ".csv" in
  let oc = open_out_bin path in
  output_string oc text;
  close_out oc;
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)

let test_chunks () =
  for it = 1 to 1500 do
    let text, sep, has_header, null_values = gen_csv () in
    match result (fun () -> Io.read_csv_string ~sep ~has_header ~null_values ~infer_rows:max_int text) with
    | Error _ -> ()
    | Ok whole ->
        (* pin the types so that chunked inference cannot differ *)
        let schema = List.map2 (fun n d -> (n, d)) (Dataframe.column_names whole)
            (Array.to_list (Array.map Series.dtype whole.Dataframe.columns)) in
        let chunk_bytes = pick [ 16; 17; 19; 23; 31; 40; 64; 100 ] in
        incr checks;
        with_temp text (fun path ->
            match result (fun () -> Io.read_csv ~sep ~has_header ~null_values ~schema ~chunk_bytes path) with
            | Ok chunked ->
                if not (frame_same whole chunked) then
                  fail "chunk #%d chunk_bytes=%d MISMATCH text=%S\n  whole:   %s\n  chunked: %s" it chunk_bytes text
                    (describe whole) (describe chunked)
            | Error m -> fail "chunk #%d chunk_bytes=%d errored (%s) text=%S" it chunk_bytes m text)
  done

let () =
  Random.init 4242;
  test_vs_reference ();
  test_floats ();
  test_chunks ();
  Printf.printf "csv: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
