(* A row is a set of named values, analogous to a Python dict. *)

type t = Value.t option array

(* val make : (string * Value.t) list -> t *)
val get : t -> int -> Value.t option
val some_of : Value.t array -> t
