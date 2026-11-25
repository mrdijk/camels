module S = Series

type opts = Csv_options.t = { sep : char; quote : char; has_header : bool; null_values : string list }
type resolved = { names : string array; dtypes : Dtype.t array }

(* ------------------------------------------------------------------ *)
(* Field scanner                                                       *)
(* ------------------------------------------------------------------ *)

type field = {
  mutable start : int;  (* content range, quotes excluded *)
  mutable stop : int;
  mutable quoted : bool;
  mutable escaped : bool;  (* contains doubled quotes *)
  mutable next : int;  (* position after the delimiter *)
  mutable eor : bool;  (* this field ends the record *)
  mutable incomplete : bool;  (* ran off the end of a buffer that is not the end of the input *)
}

let new_field () =
  { start = 0; stop = 0; quoted = false; escaped = false; next = 0; eor = false; incomplete = false }

exception Unterminated
exception Bad_quote

(* End of a field at [i]: record the delimiter's effect. *)
let finish_at f ~sep buf i len =
  if i >= len then begin f.next <- len; f.eor <- true end
  else begin
    let c = Bytes.unsafe_get buf i in
    if c = sep then begin f.next <- i + 1; f.eor <- false end
    else if c = '\n' then begin f.next <- i + 1; f.eor <- true end
    else begin
      (* '\r', optionally followed by '\n' *)
      f.next <- (if i + 1 < len && Bytes.unsafe_get buf (i + 1) = '\n' then i + 2 else i + 1);
      f.eor <- true
    end
  end

let scan_field f ~sep ~quote buf pos len ~eof =
  f.incomplete <- false;
  f.escaped <- false;
  f.quoted <- false;
  if pos < len && Bytes.unsafe_get buf pos = quote then begin
    f.quoted <- true;
    let i = ref (pos + 1) and fin = ref false in
    while not !fin do
      if !i >= len then begin
        if eof then raise Unterminated;
        f.incomplete <- true;
        fin := true
      end
      else if Bytes.unsafe_get buf !i = quote then begin
        if !i + 1 < len && Bytes.unsafe_get buf (!i + 1) = quote then begin
          f.escaped <- true;
          i := !i + 2
        end
        else if !i + 1 >= len && not eof then begin
          (* a quote at the very end of the buffer might start a doubled quote *)
          f.incomplete <- true;
          fin := true
        end
        else begin
          f.start <- pos + 1;
          f.stop <- !i;
          incr i;
          fin := true
        end
      end
      else incr i
    done;
    if not f.incomplete then begin
      let i = !i in
      if i >= len then begin
        if eof then begin f.next <- len; f.eor <- true end else f.incomplete <- true
      end
      else begin
        let c = Bytes.unsafe_get buf i in
        if c = sep || c = '\n' || c = '\r' then finish_at f ~sep buf i len else raise Bad_quote
      end
    end
  end
  else begin
    let i = ref pos in
    while
      !i < len
      &&
      let c = Bytes.unsafe_get buf !i in
      c <> sep && c <> '\n' && c <> '\r'
    do incr i done;
    if !i >= len && not eof then f.incomplete <- true
    else begin
      f.start <- pos;
      f.stop <- !i;
      finish_at f ~sep buf !i len
    end
  end

let field_string ~quote buf f =
  if not f.escaped then Bytes.sub_string buf f.start (f.stop - f.start)
  else begin
    let b = Buffer.create (f.stop - f.start) in
    let i = ref f.start in
    while !i < f.stop do
      let c = Bytes.unsafe_get buf !i in
      Buffer.add_char b c;
      if c = quote && !i + 1 < f.stop && Bytes.unsafe_get buf (!i + 1) = quote then incr i;
      incr i
    done;
    Buffer.contents b
  end

(* ------------------------------------------------------------------ *)
(* Number parsing, in place                                            *)
(* ------------------------------------------------------------------ *)

type num = { mutable ok : bool }

let is_digit c = c >= '0' && c <= '9'

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
    let b = if !i < n && s.[!i] = '.' then begin incr i; digits () end else 0 in
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

let parse_int (n : num) buf start stop : int =
  n.ok <- true;
  let len = stop - start in
  if len = 0 then begin n.ok <- false; 0 end
  else begin
    let c0 = Bytes.unsafe_get buf start in
    let neg = c0 = '-' in
    let i0 = if neg || c0 = '+' then start + 1 else start in
    let nd = stop - i0 in
    if nd = 0 then begin n.ok <- false; 0 end
    else if nd > 18 then begin
      (* rare: let int_of_string decide about overflow, after checking the syntax *)
      let ok = ref true in
      for i = i0 to stop - 1 do if not (is_digit (Bytes.unsafe_get buf i)) then ok := false done;
      if not !ok then begin n.ok <- false; 0 end
      else
        match int_of_string_opt (Bytes.sub_string buf start len) with
        | Some v -> v
        | None -> n.ok <- false; 0
    end
    else begin
      let acc = ref 0 in
      for i = i0 to stop - 1 do
        let d = Char.code (Bytes.unsafe_get buf i) - 48 in
        if d < 0 || d > 9 then n.ok <- false else acc := (!acc * 10) + d
      done;
      if neg then - !acc else !acc
    end
  end

let pow10 =
  [| 1e0; 1e1; 1e2; 1e3; 1e4; 1e5; 1e6; 1e7; 1e8; 1e9; 1e10; 1e11; 1e12; 1e13; 1e14; 1e15;
     1e16; 1e17; 1e18; 1e19; 1e20; 1e21; 1e22 |]

let float_slow (n : num) buf start stop =
  let s = Bytes.sub_string buf start (stop - start) in
  if is_float_syntax s then
    match float_of_string_opt s with Some v -> v | None -> n.ok <- false; 0.0
  else begin n.ok <- false; 0.0 end

(* Exact fast path (Clinger): a mantissa below 2^53 and a decimal exponent in
   [-22, 22] need one correctly rounded multiplication or division. Anything
   else (long mantissas, nan/inf, odd syntax) goes through float_of_string. *)
let parse_float (n : num) buf start stop : float =
  n.ok <- true;
  let i = ref start in
  let neg = ref false in
  if !i < stop then begin
    let c = Bytes.unsafe_get buf !i in
    if c = '-' then begin neg := true; incr i end else if c = '+' then incr i
  end;
  let m = ref 0 and nd = ref 0 and fd = ref 0 and any = ref false and long = ref false in
  while !i < stop && is_digit (Bytes.unsafe_get buf !i) do
    if !nd < 18 then begin
      m := (!m * 10) + (Char.code (Bytes.unsafe_get buf !i) - 48);
      incr nd
    end
    else long := true;
    any := true;
    incr i
  done;
  if !i < stop && Bytes.unsafe_get buf !i = '.' then begin
    incr i;
    while !i < stop && is_digit (Bytes.unsafe_get buf !i) do
      if !nd < 18 then begin
        m := (!m * 10) + (Char.code (Bytes.unsafe_get buf !i) - 48);
        incr nd;
        incr fd
      end
      else long := true;
      any := true;
      incr i
    done
  end;
  let e = ref 0 and exp_ok = ref true in
  if !any && !i < stop && (Bytes.unsafe_get buf !i = 'e' || Bytes.unsafe_get buf !i = 'E') then begin
    incr i;
    let eneg = ref false in
    if !i < stop then begin
      let c = Bytes.unsafe_get buf !i in
      if c = '-' then begin eneg := true; incr i end else if c = '+' then incr i
    end;
    let ed = ref 0 in
    while !i < stop && is_digit (Bytes.unsafe_get buf !i) do
      if !ed < 4 then e := (!e * 10) + (Char.code (Bytes.unsafe_get buf !i) - 48) else exp_ok := false;
      incr ed;
      incr i
    done;
    if !ed = 0 then exp_ok := false;
    if !eneg then e := - !e
  end;
  if !any && !i = stop && (not !long) && !exp_ok then begin
    let e10 = !e - !fd in
    if !m <= 9007199254740992 && e10 >= -22 && e10 <= 22 then begin
      let v =
        if e10 >= 0 then float_of_int !m *. Array.unsafe_get pow10 e10
        else float_of_int !m /. Array.unsafe_get pow10 (-e10)
      in
      if !neg then -.v else v
    end
    else float_slow n buf start stop
  end
  else float_slow n buf start stop

let lower c = if c >= 'A' && c <= 'Z' then Char.unsafe_chr (Char.code c + 32) else c

let parse_bool (n : num) buf start stop : bool =
  n.ok <- true;
  let len = stop - start in
  let eq lit =
    let r = ref true in
    for k = 0 to len - 1 do
      if lower (Bytes.unsafe_get buf (start + k)) <> String.unsafe_get lit k then r := false
    done;
    !r
  in
  if len = 4 && eq "true" then true
  else if len = 5 && eq "false" then false
  else begin n.ok <- false; false end

(* ------------------------------------------------------------------ *)
(* Null values                                                         *)
(* ------------------------------------------------------------------ *)

type nullspec = { empty : bool; others : string array }

let nullspec_of (vs : string list) =
  { empty = List.mem "" vs; others = Array.of_list (List.filter (fun s -> s <> "") vs) }

let bytes_eq buf start (s : string) =
  let r = ref true in
  for k = 0 to String.length s - 1 do
    if Bytes.unsafe_get buf (start + k) <> String.unsafe_get s k then r := false
  done;
  !r

let is_null ns buf f =
  (not f.quoted)
  &&
  let len = f.stop - f.start in
  if len = 0 then ns.empty
  else if Array.length ns.others = 0 then false
  else Array.exists (fun s -> String.length s = len && bytes_eq buf f.start s) ns.others

(* ------------------------------------------------------------------ *)
(* Typed column builders                                               *)
(* ------------------------------------------------------------------ *)

type builder =
  | Skip
  | BInts of { a : int array; v : Bytes.t; mutable nulls : int }
  | BFloats of { a : float array; v : Bytes.t; mutable nulls : int }
  | BStrs of { a : string array; v : Bytes.t; mutable nulls : int }
  | BBools of { a : Bytes.t; v : Bytes.t; mutable nulls : int }

let make_builder (d : Dtype.t) cap =
  let v = Bytes.make cap '\001' in
  match d with
  | Int64 -> BInts { a = Array.make cap 0; v; nulls = 0 }
  | Float64 -> BFloats { a = Array.make cap 0.0; v; nulls = 0 }
  | Utf8 -> BStrs { a = Array.make cap ""; v; nulls = 0 }
  | Boolean -> BBools { a = Bytes.make cap '\000'; v; nulls = 0 }

let trim_arr a n = if Array.length a = n then a else Array.sub a 0 n
let trim_bytes b n = if Bytes.length b = n then b else Bytes.sub b 0 n

let finish_builder name n = function
  | Skip -> None
  | BInts { a; v; nulls } ->
      Some (S.make name (S.Ints (trim_arr a n))
              (if nulls = 0 then None else Some (Bitmap.of_bytes (trim_bytes v n))))
  | BFloats { a; v; nulls } ->
      Some (S.make name (S.Floats (trim_arr a n))
              (if nulls = 0 then None else Some (Bitmap.of_bytes (trim_bytes v n))))
  | BStrs { a; v; nulls } ->
      Some (S.make name (S.Strings (trim_arr a n))
              (if nulls = 0 then None else Some (Bitmap.of_bytes (trim_bytes v n))))
  | BBools { a; v; nulls } ->
      Some (S.make name (S.Bools (Bitmap.of_bytes (trim_bytes a n)))
              (if nulls = 0 then None else Some (Bitmap.of_bytes (trim_bytes v n))))

(* ------------------------------------------------------------------ *)
(* Record loop                                                         *)
(* ------------------------------------------------------------------ *)

type state = {
  o : opts;
  f : field;
  num : num;
  ns : nullspec;
  width : int;
  mutable rows_done : int;  (* data rows in earlier chunks, for error messages *)
}

exception Parse_error of int * int * string  (* column, row in chunk, text *)
exception Ragged of int * int  (* row in chunk, field count *)

(* Upper bound on the number of records in buf[pos..len). *)
let count_records buf pos len =
  let c = ref 1 in
  for i = pos to len - 1 do
    let ch = Bytes.unsafe_get buf i in
    if ch = '\n' then incr c
    else if ch = '\r' && not (i + 1 < len && Bytes.unsafe_get buf (i + 1) = '\n') then incr c
  done;
  !c

let store st buf (b : builder) col r =
  let f = st.f in
  match b with
  | Skip -> ()
  | BInts b ->
      if is_null st.ns buf f then begin Bytes.set b.v r '\000'; b.nulls <- b.nulls + 1 end
      else begin
        let x = parse_int st.num buf f.start f.stop in
        if st.num.ok then b.a.(r) <- x
        else raise (Parse_error (col, r, field_string ~quote:st.o.quote buf f))
      end
  | BFloats b ->
      if is_null st.ns buf f then begin Bytes.set b.v r '\000'; b.nulls <- b.nulls + 1 end
      else begin
        let x = parse_float st.num buf f.start f.stop in
        if st.num.ok then b.a.(r) <- x
        else raise (Parse_error (col, r, field_string ~quote:st.o.quote buf f))
      end
  | BBools b ->
      if is_null st.ns buf f then begin Bytes.set b.v r '\000'; b.nulls <- b.nulls + 1 end
      else begin
        let x = parse_bool st.num buf f.start f.stop in
        if st.num.ok then (if x then Bytes.set b.a r '\001')
        else raise (Parse_error (col, r, field_string ~quote:st.o.quote buf f))
      end
  | BStrs b ->
      if is_null st.ns buf f then begin Bytes.set b.v r '\000'; b.nulls <- b.nulls + 1 end
      else b.a.(r) <- field_string ~quote:st.o.quote buf f

(* Skip blank lines starting at [pos]; returns the position of the next record. *)
let rec skip_blank buf pos len =
  if pos < len then begin
    let c = Bytes.unsafe_get buf pos in
    if c = '\n' then skip_blank buf (pos + 1) len
    else if c = '\r' then
      skip_blank buf (if pos + 1 < len && Bytes.unsafe_get buf (pos + 1) = '\n' then pos + 2 else pos + 1) len
    else pos
  end
  else pos

(* Parse complete records from buf[pos..len) into the builders. Returns the
   position after the last complete record and the number of rows stored. *)
let parse_records st (builders : builder array) buf pos len ~eof =
  let sep = st.o.sep and quote = st.o.quote and f = st.f in
  let pos = ref pos and r = ref 0 and go = ref true in
  while !go do
    pos := skip_blank buf !pos len;
    if !pos >= len then go := false
    else begin
      let rec_start = !pos in
      let col = ref 0 and p = ref !pos and fin = ref false and incomplete = ref false in
      while not !fin do
        scan_field f ~sep ~quote buf !p len ~eof;
        if f.incomplete then begin incomplete := true; fin := true end
        else begin
          if !col < st.width then store st buf (Array.unsafe_get builders !col) !col !r;
          incr col;
          p := f.next;
          if f.eor then fin := true
        end
      done;
      if !incomplete then begin go := false; pos := rec_start end
      else begin
        if !col <> st.width then raise (Ragged (!r, !col));
        incr r;
        pos := !p
      end
    end
  done;
  (!pos, !r)

(* ------------------------------------------------------------------ *)
(* Header and inference                                                *)
(* ------------------------------------------------------------------ *)

(* Scan one record, collecting its fields as strings. [None] if incomplete. *)
let read_record_strings ~sep ~quote buf pos len ~eof =
  let f = new_field () in
  let acc = ref [] and p = ref pos and fin = ref false and incomplete = ref false in
  while not !fin do
    scan_field f ~sep ~quote buf !p len ~eof;
    if f.incomplete then begin incomplete := true; fin := true end
    else begin
      acc := field_string ~quote buf f :: !acc;
      p := f.next;
      if f.eor then fin := true
    end
  done;
  if !incomplete then None else Some (Array.of_list (List.rev !acc), !p)

let infer_types st buf pos len ~eof ~limit : Dtype.t array =
  let w = st.width in
  let all_bool = Array.make w true and all_int = Array.make w true in
  let all_num = Array.make w true and seen = Array.make w false in
  let f = st.f and pos = ref pos and rows = ref 0 and go = ref true in
  let sep = st.o.sep and quote = st.o.quote in
  while !go && !rows < limit do
    pos := skip_blank buf !pos len;
    if !pos >= len then go := false
    else begin
      let col = ref 0 and p = ref !pos and fin = ref false and incomplete = ref false in
      while not !fin do
        scan_field f ~sep ~quote buf !p len ~eof;
        if f.incomplete then begin incomplete := true; fin := true end
        else begin
          let c = !col in
          if c < w && not (is_null st.ns buf f) then begin
            seen.(c) <- true;
            if all_bool.(c) then begin ignore (parse_bool st.num buf f.start f.stop); if not st.num.ok then all_bool.(c) <- false end;
            if all_int.(c) then begin ignore (parse_int st.num buf f.start f.stop); if not st.num.ok then all_int.(c) <- false end;
            if all_num.(c) then begin ignore (parse_float st.num buf f.start f.stop); if not st.num.ok then all_num.(c) <- false end
          end;
          incr col;
          p := f.next;
          if f.eor then fin := true
        end
      done;
      if !incomplete then go := false
      else begin incr rows; pos := !p end
    end
  done;
  Array.init w (fun c ->
      if not seen.(c) then Dtype.Utf8
      else if all_bool.(c) then Dtype.Boolean
      else if all_int.(c) then Dtype.Int64
      else if all_num.(c) then Dtype.Float64
      else Dtype.Utf8)

exception Need_more  (* the header record is not complete in this buffer *)

(* Names, types and the position of the first data record. [None] for empty input. *)
let resolve_buffer_exn (o : opts) ~infer_rows ~schema buf pos len ~eof : (resolved * int) option =
  let pos = skip_blank buf pos len in
  if pos >= len then (if eof then None else raise Need_more)
  else begin
    let ns = nullspec_of o.null_values in
    let first = read_record_strings ~sep:o.sep ~quote:o.quote buf pos len ~eof in
    match first with
    | None -> raise Need_more
    | Some (fields, after_first) ->
        let width = Array.length fields in
        let names, data_pos =
          if o.has_header then (fields, after_first)
          else (Array.init width (fun i -> Printf.sprintf "column_%d" (i + 1)), pos)
        in
        List.iter
          (fun (n, _) ->
            if not (Array.mem n names) then
              Err.fail "CSV: schema names column %S, which is not in the file" n)
          schema;
        let st =
          { o; f = new_field (); num = { ok = true }; ns; width; rows_done = 0 }
        in
        let inferred = infer_types st buf data_pos len ~eof ~limit:infer_rows in
        let dtypes =
          Array.mapi
            (fun c d -> match List.assoc_opt names.(c) schema with Some d' -> d' | None -> d)
            inferred
        in
        Some ({ names; dtypes }, data_pos)
  end

let resolve_buffer (o : opts) ~infer_rows ~schema buf pos len ~eof : (resolved * int) option =
  try resolve_buffer_exn o ~infer_rows ~schema buf pos len ~eof with
  | Unterminated -> Err.fail "CSV: unterminated quoted field near the start of the input"
  | Bad_quote -> Err.fail "CSV: unexpected character after a closing quote near the start of the input"

(* ------------------------------------------------------------------ *)
(* Turning a buffer into a frame                                       *)
(* ------------------------------------------------------------------ *)

let wrap_errors (r : resolved) st f =
  try f () with
  | Unterminated -> Err.fail "CSV: unterminated quoted field (data row %d or later)" (st.rows_done + 1)
  | Bad_quote -> Err.fail "CSV: unexpected character after a closing quote near data row %d" (st.rows_done + 1)
  | Ragged (row, n) ->
      Err.fail "CSV: data row %d has %d fields, expected %d" (st.rows_done + row + 1) n st.width
  | Parse_error (col, row, text) ->
      Err.fail "CSV column %S, data row %d: cannot parse %S as %s (raise ~infer_rows or pass ~schema to override)"
        r.names.(col) (st.rows_done + row + 1) text
        (Dtype.to_string r.dtypes.(col))

(* Parse the complete records of buf[pos..len) into a frame holding [wanted]. *)
let parse_buffer st (r : resolved) ~wanted buf pos len ~eof : int * Dataframe.t * int =
  let cap = count_records buf pos len in
  let builders =
    Array.mapi (fun c d -> if wanted.(c) then make_builder d cap else Skip) r.dtypes
  in
  let consumed, rows =
    wrap_errors r st (fun () -> parse_records st builders buf pos len ~eof)
  in
  let cols =
    List.filter_map Fun.id
      (Array.to_list (Array.mapi (fun c b -> finish_builder r.names.(c) rows b) builders))
  in
  let df = if cols = [] then Dataframe.empty else Dataframe.create cols in
  (consumed, df, rows)

let make_state o (r : resolved) =
  { o; f = new_field (); num = { ok = true }; ns = nullspec_of o.null_values;
    width = Array.length r.names; rows_done = 0 }

let of_string (o : opts) ~infer_rows ~schema (text : string) : Dataframe.t =
  let buf = Bytes.unsafe_of_string text (* read-only use *) in
  let len = Bytes.length buf in
  let pos0 =
    if len >= 3 && text.[0] = '\xEF' && text.[1] = '\xBB' && text.[2] = '\xBF' then 3 else 0
  in
  match resolve_buffer o ~infer_rows ~schema buf pos0 len ~eof:true with
  | None -> Dataframe.empty
  | exception Need_more -> assert false
  | Some (r, data_pos) ->
      let st = make_state o r in
      let wanted = Array.make st.width true in
      let _, df, _ = parse_buffer st r ~wanted buf data_pos len ~eof:true in
      if Dataframe.width df = 0 then
        (* no data rows: typed empty columns *)
        let _, df, _ = parse_buffer st r ~wanted buf len len ~eof:true in df
      else df

(* ------------------------------------------------------------------ *)
(* Files, in chunks                                                    *)
(* ------------------------------------------------------------------ *)

let with_channel path f =
  match open_in_bin path with
  | exception Sys_error m -> Err.fail "CSV: cannot read file (%s)" m
  | ic -> Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> f ic)

exception Resolved of (resolved * int) option

(* Feed the file to [step buf pos len eof] -> new pos, buffer by buffer. A step
   that consumes nothing without reaching end of input makes the buffer grow
   (a record longer than the buffer). *)
let drive ic ~chunk_bytes (step : Bytes.t -> int -> int -> bool -> int) =
  (* never allocate more than the file needs (+1 so that end of file is seen) *)
  let file_cap = try in_channel_length ic + 1 with Sys_error _ -> max_int in
  let cap = ref (max 16 (min chunk_bytes file_cap)) in
  let buf = ref (Bytes.create !cap) in
  let len = ref 0 and pos = ref 0 and eof = ref false and first = ref true in
  while not (!eof && !pos >= !len) do
    if !pos > 0 then begin
      Bytes.blit !buf !pos !buf 0 (!len - !pos);
      len := !len - !pos;
      pos := 0
    end;
    while (not !eof) && !len < !cap do
      let n = input ic !buf !len (!cap - !len) in
      if n = 0 then eof := true else len := !len + n
    done;
    if !first then begin
      first := false;
      if !len >= 3 && Bytes.get !buf 0 = '\xEF' && Bytes.get !buf 1 = '\xBB' && Bytes.get !buf 2 = '\xBF'
      then pos := 3
    end;
    let next = step !buf !pos !len !eof in
    if next = !pos && !pos < !len then begin
      if !eof then Err.fail "CSV: internal error (no progress at end of input)";
      let ncap = !cap * 2 in
      let nb = Bytes.create ncap in
      Bytes.blit !buf 0 nb 0 !len;
      buf := nb;
      cap := ncap
    end;
    pos := next
  done

let resolve_file (o : opts) ~infer_rows ~schema ~chunk_bytes path : resolved =
  let result =
    try
      with_channel path (fun ic ->
          drive ic ~chunk_bytes (fun buf pos len eof ->
              match resolve_buffer o ~infer_rows ~schema buf pos len ~eof with
              | exception Need_more -> pos
              | res -> raise (Resolved res));
          None)
    with Resolved res -> res
  in
  match result with Some (r, _) -> r | None -> { names = [||]; dtypes = [||] }

let read_chunks (o : opts) (r : resolved) ~chunk_bytes ~path ~columns (emit : Dataframe.t -> unit) =
  let width = Array.length r.names in
  if width = 0 then emit Dataframe.empty
  else begin
    let wanted =
      match columns with
      | None -> Array.make width true
      | Some cs ->
          List.iter
            (fun c -> if not (Array.mem c r.names) then Err.fail "CSV: no column %S in %s" c path)
            cs;
          Array.map (fun n -> List.mem n cs) r.names
    in
    let st = make_state o r in
    let header_done = ref (not o.has_header) in
    let emitted = ref false in
    with_channel path (fun ic ->
        drive ic ~chunk_bytes (fun buf pos len eof ->
            let pos0 = pos in
            let pos =
              if !header_done then Some pos
              else
                match read_record_strings ~sep:o.sep ~quote:o.quote buf (skip_blank buf pos len) len ~eof with
                | None -> None
                | Some (_, next) -> header_done := true; Some next
            in
            match pos with
            | None -> (* header record longer than the buffer: ask for more data *) pos0
            | Some pos ->
                let consumed, df, rows = parse_buffer st r ~wanted buf pos len ~eof in
                if rows > 0 then begin emitted := true; emit df end;
                st.rows_done <- st.rows_done + rows;
                consumed));
    if not !emitted then begin
      (* no data rows: a typed empty frame *)
      let buf = Bytes.empty in
      let _, df, _ = parse_buffer st r ~wanted buf 0 0 ~eof:true in
      emit df
    end
  end

let schema_of_resolved (r : resolved) =
  Array.to_list (Array.mapi (fun i n -> (n, r.dtypes.(i))) r.names)

let resolved_of_schema (schema : (string * Dtype.t) list) : resolved =
  { names = Array.of_list (List.map fst schema); dtypes = Array.of_list (List.map snd schema) }
