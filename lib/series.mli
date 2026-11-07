(** A named, typed column: a buffer of one type plus an optional validity
    mask ([None] = no nulls). The data under a null slot is a placeholder and
    must never be read without checking validity. *)
 
type buffer =
  | Ints of int array
  | Floats of float array
  | Strings of string array
  | Bools of Bitmap.t
 
type t = { name : string; data : buffer; validity : Bitmap.t option }
 
(** [make name data validity] checks that the mask, if any, is as long as
    the data; raises {!Err.Polars_error} otherwise. *)
val make : string -> buffer -> Bitmap.t option -> t
val length : t -> int
val rename : t -> string -> t

(** The column's type, read from its buffer in O(1). *)
val dtype : t -> Dtype.t

(** {1 Constructors} *)

val of_ints : string -> int array -> t
val of_floats : string -> float array -> t
val of_strings : string -> string array -> t
val of_bools : string -> bool array -> t

(** Like {!of_ints}, but [None] becomes a null. The mask is dropped
    if there are no nulls. *)
val of_opt_ints : string -> int option array -> t
 
(** Like {!of_floats}, but [None] becomes a null. *)
val of_opt_floats : string -> float option array -> t
 
(** Like {!of_strings}, but [None] becomes a null. *)
val of_opt_strings : string -> string option array -> t
 
(** Like {!of_bools}, but [None] becomes a null. *)
val of_opt_bools : string -> bool option array -> t

(** Infers the type unless [dtype] is given (Int64 for empty / all-null input). *)
val of_values : ?dtype:Dtype.t -> string -> Value.t array -> t
 
(** [of_scalar name n v]: [n] copies of [v]; [dtype] types a [Null]. *)
val of_scalar : ?dtype:Dtype.t -> string -> int -> Value.t -> t
 
(** {1 Access} *)
 
(** [is_valid s i] is [false] if row [i] is null. *)
val is_valid : t -> int -> bool
 
(** Number of null rows. *)
val null_count : t -> int
 
(** Row as a scalar; [Null] if the slot is invalid. *)
val get : t -> int -> Value.t
 
(** [take s idx] gathers rows [idx]; every index must be in range. *)
val take : t -> int array -> t
 
(** [take_opt]: a negative index yields a null row. *)
val take_opt : t -> int array -> t
 
