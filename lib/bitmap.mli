(** Fixed-length bit/byte mask. [true] = set. *)
type t

val create : int -> bool -> t
val length : t -> int
val get : t -> int -> bool
val set : t -> int -> bool -> unit
val and_ : t -> t -> t
val count_set : t -> int
