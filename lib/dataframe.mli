(** Eager, column-oriented dataframe. Column names are unique and all
    columns have length [height]. *)

type t = { columns : Series.t array; height : int }

val empty : t

val create : Series.t list -> t
(** Raises {!Err.Polars_error} on duplicate names or unequal lengths. *)

val of_columns : (string * Value.t array) list -> t

val height : t -> int
val width : t -> int
val column_names : t -> string list

val get_column : t -> string -> Series.t
(** Raises {!Err.Polars_error} ("ColumnNotFound") if absent. *)

val select_columns : t -> string list -> t
(** Keeps the requested columns in the requested order (and the height). *)

val take : t -> int array -> t
val take_opt : t -> int array -> t
(** Row gather; in [take_opt] negative indices yield [Null] rows. *)

val filter : t -> bool array -> t
(** Keeps rows where the mask is [true]. *)

val with_column : t -> Series.t -> t
(** Replaces the same-named column in place, otherwise appends. *)

val to_rows : t -> Value.t list list
val to_string : t -> string
val print : t -> unit
