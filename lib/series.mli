(** A single column: a typed, homogeneous, columnar array of values,
    with a validity bitmap for null values (similar to apache Arrow,
    but without the bitpacking — [bool array] instead of a packed bitmap, since
    that's cheaper to work with in OCaml at batch-sized lengths). *)

type buffer =
  | Ints of int array
  | Floats of float array
  | Strings of string array
  | Bools of bool array

type t = {
  length : int;
  validity : bool array;  (** [validity.(i) = true] means row [i] is non-null *)
  data : buffer;
}

val length : t -> int

(** The dtype this series holds, derived from its buffer constructor. *)
val dtype : t -> Dtype.t

(** [get t i] is [None] if row [i] is null, else [Some Value.t]
    Raises [Invalid argument] if [i] is out of bounds. *)
val get : t -> int -> Value.t option

 (** Build a series from a flat array of optional values. Assumes the
    series is homogeneous: the dtype is taken from the first [Some]
    value found; an all-null array defaults to [Dtype.String]. Raises
    if a later value doesn't match the inferred dtype. *)
val of_values : Value.t option array -> t

(** Inverse of [of_values]: expand back into boxed, tagged, optional
    values — e.g. for converting a [Batch.t] back to [Row.t list] at a
    plan boundary that still needs rows (aggregation, joins). *)
val to_values : t -> Value.t option array
