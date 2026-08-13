(** A row is a set of named values, analogous to a Python dict. *)

type t = (string * Value.t) list

val make : (string * Value.t) list -> t
val get : t -> string -> Value.t
