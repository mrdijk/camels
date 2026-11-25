(** Reading CSV data.

    The parser follows RFC 4180: fields may be wrapped in the quote
    character (a double quote by default), a doubled quote inside a quoted
    field is a literal quote, and quoted fields may contain separators and
    newlines. Both [\n] and [\r\n] line endings are
    accepted, a leading UTF-8 byte-order mark is ignored, and completely blank
    lines are skipped. Whitespace is never trimmed.

    {b Nulls.} An {i unquoted} field equal to one of [null_values] is null
    (by default only the empty field). A quoted field is never null, so
    a quoted empty field is an empty string while an unquoted empty field is
    null. The header row is
    never interpreted as nulls.

    {b Types.} Each column is typed from its first [infer_rows] rows:
    all [true]/[false] (any case) gives [Boolean]; all integers gives
    [Int64]; integers and decimals ([1.5], [1e3], [.5], [nan], [inf]) give
    [Float64]; anything else, and any column with no non-null values, gives
    [Utf8]. Integers too large for [int] become floats. If a later row does
    not fit the inferred type, an error names the column and row; use
    [~infer_rows] or [~schema] to fix that.

    All functions raise {!Err.Camels_error} on malformed input (ragged rows,
    unterminated quotes, duplicate column names, unreadable files, values
    that do not fit [schema]). *)

(** Parse CSV text.
    - [sep] field separator (default [',']).
    - [quote] quote character (default: double quote).
    - [has_header] first record holds column names (default [true]); without
      it, columns are named [column_1], [column_2], ...
    - [null_values] unquoted fields treated as null (default: only the empty string).
    - [infer_rows] rows examined to infer types (default 100; pass
      [max_int] for all rows).
    - [schema] explicit dtypes by column name, overriding inference. Naming a
      column that is not in the file is an error. *)
val read_csv_string :
  ?sep:char ->
  ?quote:char ->
  ?has_header:bool ->
  ?null_values:string list ->
  ?infer_rows:int ->
  ?schema:(string * Dtype.t) list ->
  string ->
  Dataframe.t

(** [read_csv path] reads and parses a file; see {!read_csv_string}. The file
    is read in chunks of about [chunk_bytes] (default 16 MB) and the chunks
    are stacked, so the file text is never held in memory all at once (the
    resulting frame, of course, is). Types are inferred from the first chunk. *)
val read_csv :
  ?sep:char ->
  ?quote:char ->
  ?has_header:bool ->
  ?null_values:string list ->
  ?infer_rows:int ->
  ?schema:(string * Dtype.t) list ->
  ?chunk_bytes:int ->
  string ->
  Dataframe.t

(** [scan_csv path] is a lazy scan of a CSV file: only the header and a sample
    for type inference are read now; the rows are read when the query is
    collected. The optimizer pushes the query's column selection and filters
    into the scan, so the file is streamed in chunks of [chunk_bytes], only the
    needed columns are parsed, and rows that fail a pushed-down filter are
    dropped chunk by chunk, before they are ever held in memory. (Operators
    above the scan, such as joins and aggregations, still need their input in
    memory.) The file must not change between the scan and the collect. *)
val scan_csv :
  ?sep:char ->
  ?quote:char ->
  ?has_header:bool ->
  ?null_values:string list ->
  ?infer_rows:int ->
  ?schema:(string * Dtype.t) list ->
  ?chunk_bytes:int ->
  string ->
  Lazyframe.t
