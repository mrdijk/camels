(** A single scalar vlaue that can live in a row/column. *)

type t = VInt of int | VFloat of float | VStr of string | VBool of bool

val to_string : t -> string

(** smart constructors *)
val str : string -> t
val int : int -> t
val float : float -> t
val bool : bool -> t
