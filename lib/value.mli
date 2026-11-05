(** Dynamically typed scalar values. [Null] is a member of every type. *)

type t = Null | Int of int | Float of float | Str of string | Bool of bool

val dtype_of : t -> Dtype.t option
val is_null : t -> bool

val to_string : t -> string
(** Display form; [Null] prints as ["null"], whole floats as ["3.0"]. *)

val compare : t -> t -> int
(** Total order: nulls first, then bools, numbers (compared numerically
    across [Int]/[Float]), then strings. *)

val equal : t -> t -> bool
(** [equal a b] is [compare a b = 0]. *)

val normalize : t -> t
(** Canonical form for hashing: integral floats become [Int], so that
    [Int 1] and [Float 1.0] land in the same hash bucket. *)
