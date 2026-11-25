(** Where a scan gets its rows from. A plan holds a source, never a reader:
    everything needed to read it later is plain data, so plans can be printed,
    compared and inspected. How a source is actually read lives in the private
    [Scan] module. *)

(** A CSV file to be read lazily. The schema was resolved when the scan was
    created and stays fixed for every chunk. *)
type csv = {
  path : string;
  options : Csv_options.t;
  schema : (string * Dtype.t) list;  (** column names and types, in file order *)
  chunk_bytes : int;  (** the file is read in chunks of about this size *)
}

type t =
  | Memory of Dataframe.t  (** an in-memory frame *)
  | Csv of csv

(** Column names and dtypes, in order. Known without reading any rows. *)
val schema : t -> (string * Dtype.t) list

(** Column names, in order. *)
val columns : t -> string list

(** One-line description used by [explain], e.g. [CSV SCAN data.csv [a, b]]. *)
val describe : t -> string
