(** CSV engine: single-pass, in-place parsing straight into typed buffers.

    Fields are scanned in a byte buffer and numbers are parsed without
    allocating; each column is written into a typed array sized from the
    buffer's line count. The same engine reads a whole string at once or a
    file in chunks, parsing only the columns that were asked for. *)

(** Column names and types of a CSV input. *)
type resolved = { names : string array; dtypes : Dtype.t array }

(** Parse CSV text (header handling, type inference and parsing included). *)
val of_string : Csv_options.t -> infer_rows:int -> schema:(string * Dtype.t) list -> string -> Dataframe.t

(** [resolve_file options ~infer_rows ~schema ~chunk_bytes path] reads only the
    first chunk of a file to get the column names and types. *)
val resolve_file :
  Csv_options.t -> infer_rows:int -> schema:(string * Dtype.t) list -> chunk_bytes:int -> string -> resolved

(** [read_chunks options resolved ~chunk_bytes ~path ~columns f] streams the file
    in chunks of about [chunk_bytes], parsing only [columns] (all if [None]),
    and calls [f] on a frame per chunk. Each frame holds the requested columns
    in file order. [f] is called at least once, with an empty frame if the file
    has no data rows. *)
val read_chunks :
  Csv_options.t ->
  resolved ->
  chunk_bytes:int ->
  path:string ->
  columns:string list option ->
  (Dataframe.t -> unit) ->
  unit

(** Resolved names and types as a schema list, and back. *)
val schema_of_resolved : resolved -> (string * Dtype.t) list
val resolved_of_schema : (string * Dtype.t) list -> resolved
