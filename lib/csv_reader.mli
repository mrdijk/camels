(** CSV parsing and typed loading into columns of [Value.t]. *)

(** Low-level: parse raw CSV text into rows of string fields.
    Handles quoted fields, embedded commas/newlines, and "" escapes. *)
val parse_string : string -> string list list

(** A single column: its inferred/declared type plus its values.
    [None] represents an empty field. *)
type column = {
  dtype : Dtype.t;
  values : Value.t option array;
}

type t = {
  column_names : string list;
  columns : (string, column) Hashtbl.t;
  n_rows : int;
}

(** [read_csv ?has_header ?dtypes ?na_values path] reads a CSV file
    into a typed, column-oriented table.

    - [has_header]: whether the first row is a header (default true).
      If false, columns are named ["column_0"; "column_1"; ...].
    - [dtypes]: optional explicit types for named columns, skipping
      inference for those columns.
    - [na_values]: strings treated as null in addition to [""].
      Default is [[""]]. *)
val read_csv :
  ?has_header:bool ->
  ?dtypes:(string * Dtype.t) list ->
  ?na_values:string list ->
  string ->
  t
