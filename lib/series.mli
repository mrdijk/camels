(** A named, typed column: a buffer of one type plus an optional validity
    mask ([None] = no nulls). The data under a null slot is a placeholder and
    must never be read without checking validity. *)

type buffer =
  | Ints of int array
  | Floats of float array
  | Strings of string array
  | Bools of Bitmap.t

type t = { name : string; data : buffer; validity : Bitmap.t option }

val make : string -> buffer -> Bitmap.t option -> t
val length : t -> int
val rename : t -> string -> t
val dtype : t -> Dtype.t

(** {1 Constructors} *)

val of_ints : string -> int array -> t
val of_floats : string -> float array -> t
val of_strings : string -> string array -> t
val of_bools : string -> bool array -> t
val of_opt_ints : string -> int option array -> t
val of_opt_floats : string -> float option array -> t
val of_opt_strings : string -> string option array -> t
val of_opt_bools : string -> bool option array -> t

val of_values : ?dtype:Dtype.t -> string -> Value.t array -> t
(** Infers the type unless [dtype] is given (Int64 for empty / all-null input). *)

val of_scalar : ?dtype:Dtype.t -> string -> int -> Value.t -> t
(** [of_scalar name n v]: [n] copies of [v]; [dtype] types a [Null]. *)

(** {1 Access} *)

val is_valid : t -> int -> bool
val null_count : t -> int
val get : t -> int -> Value.t
(** Row as a scalar; [Null] if the slot is invalid. *)

val take : t -> int array -> t
val take_opt : t -> int array -> t
(** [take_opt]: a negative index yields a null row. *)
