(** A single column: a typed, homogeneous array of values. *)

type t =
  | IntCol of int array
  | FloatCol of float array
  | StrCol of string array
  | BoolCol of bool array

val length : t -> int
val get : t -> int -> Value.t

(** Keep only the elements where mask.(i) = true. *)
val apply_mask : t -> bool array -> t

val of_values : Value.t array -> t
