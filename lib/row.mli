(* A row is a set of named values, analogous to a Python dict. *)

type t = Value.t array

(* val make : (string * Value.t) list -> t *)
val get : t -> int -> Value.t
