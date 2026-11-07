(** Eager, column-oriented dataframe. Column names are unique and all
    columns have length [height]. *)

type t = { columns : Series.t array; height : int }

(** The frame with no columns and no rows. *)
val empty : t

(** Raises {!Err.Polars_error} on duplicate names or unequal lengths. *)
val create : Series.t list -> t

(** From [(name, scalars)] pairs; each column's type is inferred as in
    {!Series.of_values}. *)
val of_columns : (string * Value.t array) list -> t

(** Number of rows. *)
val height : t -> int

(** Number of columns. *)
val width : t -> int

(** Column names, in order. *)
val column_names : t -> string list

(** Raises {!Err.Polars_error} ("ColumnNotFound") if absent. *)
val get_column : t -> string -> Series.t

(** Keeps the requested columns in the requested order (and the height). *)
val select_columns : t -> string list -> t

(** [take df idx] gathers rows [idx]; every index must be in range. *)
val take : t -> int array -> t

(** Row gather; in [take_opt] negative indices yield [Null] rows. *)
val take_opt : t -> int array -> t

(** Keeps rows where the mask is [true]. *)
val filter : t -> bool array -> t

(** Replaces the same-named column in place, otherwise appends. *)
val with_column : t -> Series.t -> t

(** Row-major scalars, mainly for tests and printing. *)
val to_rows : t -> Value.t list list

(** ASCII table with a row of dtypes under the header. *)
val to_string : t -> string

(** Prints {!to_string} to standard output. *)
val print : t -> unit
