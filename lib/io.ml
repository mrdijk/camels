(* ------------------------------------------------------------------ *)
(* Tokenizer                                                           *)
(* ------------------------------------------------------------------ *)

(* Splits [s] into records of fields; [None] marks a null field. *)
let parse_records ~sep ~quote ~has_header ~null_values (s : string) :
    string option array array =
  let n = String.length s in
  let start =
    if n >= 3 && s.[0] = '\xEF' && s.[1] = '\xBB' && s.[2] = '\xBF' then 3 else 0
  in
  let rows = ref [] and nrows = ref 0 in
  let row = ref [] in
  let field = Buffer.create 64 in
  let quoted = ref false and touched = ref false in
  let line = ref 1 in
  let end_field () =
    let v = Buffer.contents field in
    let is_header = has_header && !nrows = 0 in
    let null = (not !quoted) && (not is_header) && List.mem v null_values in
    row := (if null then None else Some v) :: !row;
    Buffer.clear field;
    quoted := false
  in
  let end_row () =
    if !touched then begin
      end_field ();
      rows := Array.of_list (List.rev !row) :: !rows;
      incr nrows
    end;
    row := [];
    touched := false;
    quoted := false;
    Buffer.clear field
  in
  let i = ref start in
  while !i < n do
    let c = s.[!i] in
    if c = quote && (not !quoted) && Buffer.length field = 0 then begin
      (* quoted field *)
      touched := true;
      quoted := true;
      let open_line = !line in
      incr i;
      let closed = ref false in
      while not !closed do
        if !i >= n then Err.fail "CSV: unterminated quoted field opened on line %d" open_line;
        let d = s.[!i] in
        if d = quote then begin
          if !i + 1 < n && s.[!i + 1] = quote then begin
            Buffer.add_char field quote;
            i := !i + 2
          end
          else begin
            closed := true;
            incr i
          end
        end
        else begin
          if d = '\n' then incr line;
          Buffer.add_char field d;
          incr i
        end
      done
    end
    else if c = sep then begin
      touched := true;
      end_field ();
      incr i
    end
    else if c = '\n' then begin
      end_row ();
      incr line;
      incr i
    end
    else if c = '\r' then begin
      end_row ();
      incr line;
      if !i + 1 < n && s.[!i + 1] = '\n' then i := !i + 2 else incr i
    end
    else begin
      touched := true;
      Buffer.add_char field c;
      incr i
    end
  done;
  end_row ();
  Array.of_list (List.rev !rows)

(* ------------------------------------------------------------------ *)
(* Field parsing and type inference                                    *)
(* ------------------------------------------------------------------ *)

let is_digit c = c >= '0' && c <= '9'

let is_int_syntax s =
  let n = String.length s in
  let i = if n > 0 && (s.[0] = '-' || s.[0] = '+') then 1 else 0 in
  n > i
  &&
  let ok = ref true in
  for k = i to n - 1 do
    if not (is_digit s.[k]) then ok := false
  done;
  !ok

let int_of_field s = if is_int_syntax s then int_of_string_opt s else None

let is_float_syntax s =
  let n = String.length s in
  let i = ref 0 in
  if !i < n && (s.[!i] = '-' || s.[!i] = '+') then incr i;
  let rest = String.lowercase_ascii (String.sub s !i (n - !i)) in
  if rest = "nan" || rest = "inf" || rest = "infinity" then true
  else begin
    let digits () =
      let st = !i in
      while !i < n && is_digit s.[!i] do incr i done;
      !i - st
    in
    let a = digits () in
    let b =
      if !i < n && s.[!i] = '.' then begin incr i; digits () end else 0
    in
    if a + b = 0 then false
    else begin
      if !i < n && (s.[!i] = 'e' || s.[!i] = 'E') then begin
        incr i;
        if !i < n && (s.[!i] = '-' || s.[!i] = '+') then incr i;
        if digits () = 0 then i := -1
      end;
      !i = n
    end
  end

let float_of_field s = if is_float_syntax s then float_of_string_opt s else None

let bool_of_field s =
  match String.lowercase_ascii s with
  | "true" -> Some true
  | "false" -> Some false
  | _ -> None

let infer_dtype ~limit (vals : string option array) : Dtype.t =
  let all_bool = ref true and all_int = ref true and all_num = ref true in
  let seen = ref false in
  let m = min limit (Array.length vals) in
  for k = 0 to m - 1 do
    match vals.(k) with
    | None -> ()
    | Some v ->
        seen := true;
        if bool_of_field v = None then all_bool := false;
        if int_of_field v = None then all_int := false;
        if float_of_field v = None then all_num := false
  done;
  if not !seen then Utf8
  else if !all_bool then Boolean
  else if !all_int then Int64
  else if !all_num then Float64
  else Utf8

let build_series name (dtype : Dtype.t) (vals : string option array) : Series.t =
  let conv what parse =
    Array.mapi
      (fun r -> function
        | None -> None
        | Some v -> (
            match parse v with
            | Some x -> Some x
            | None ->
                Err.fail "CSV column %S, data row %d: cannot parse %S as %s%s" name
                  (r + 1) v what
                  (if what = "str" then ""
                   else " (raise ~infer_rows or pass ~schema to override)")))
      vals
  in
  match dtype with
  | Int64 -> Series.of_opt_ints name (conv "i64" int_of_field)
  | Float64 -> Series.of_opt_floats name (conv "f64" float_of_field)
  | Boolean -> Series.of_opt_bools name (conv "bool" bool_of_field)
  | Utf8 -> Series.of_opt_strings name vals

(* ------------------------------------------------------------------ *)
(* Public API                                                          *)
(* ------------------------------------------------------------------ *)

let read_csv_string ?(sep = ',') ?(quote = '"') ?(has_header = true)
    ?(null_values = [ "" ]) ?(infer_rows = 1000) ?(schema = []) text =
  let records = parse_records ~sep ~quote ~has_header ~null_values text in
  if Array.length records = 0 then Dataframe.empty
  else begin
    let width = Array.length records.(0) in
    let names, body =
      if has_header then
        ( Array.map (function Some s -> s | None -> "") records.(0),
          Array.sub records 1 (Array.length records - 1) )
      else (Array.init width (fun i -> Printf.sprintf "column_%d" (i + 1)), records)
    in
    Array.iteri
      (fun r row ->
        if Array.length row <> width then
          Err.fail "CSV: data row %d has %d fields, expected %d" (r + 1)
            (Array.length row) width)
      body;
    List.iter
      (fun (n, _) ->
        if not (Array.mem n names) then
          Err.fail "CSV: schema names column %S, which is not in the file" n)
      schema;
    let cols =
      List.init width (fun c ->
          let vals = Array.map (fun row -> row.(c)) body in
          let name = names.(c) in
          let dtype =
            match List.assoc_opt name schema with
            | Some d -> d
            | None -> infer_dtype ~limit:infer_rows vals
          in
          build_series name dtype vals)
    in
    Dataframe.create cols
  end

let read_file path =
  try In_channel.with_open_bin path In_channel.input_all
  with Sys_error msg -> Err.fail "CSV: cannot read file (%s)" msg

let read_csv ?sep ?quote ?has_header ?null_values ?infer_rows ?schema path =
  read_csv_string ?sep ?quote ?has_header ?null_values ?infer_rows ?schema (read_file path)

let scan_csv ?sep ?quote ?has_header ?null_values ?infer_rows ?schema path =
  Lazyframe.of_df (read_csv ?sep ?quote ?has_header ?null_values ?infer_rows ?schema path)
