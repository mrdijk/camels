(** A chunk of rows stored columnar: same length across all columns. *)

type t

val length : t -> int
val columns : t -> (string * Series.t) list
val get_column : t -> string -> Series.t

val of_columns : (string * Series.t) list -> t
val of_rows : Row.t list -> t   (** convert a list of rows into one columnar batch *)

val apply_mask : t -> bool array -> t
val to_rows : t -> Row.t list   (** convert back, e.g. for [collect] *)
