type t = Int64 | Float64 | Utf8 | Boolean

val to_string : t -> string
val is_numeric : t -> bool

val supertype : t -> t -> t option
(** Common type for arithmetic/comparison: [Int64]+[Float64] is [Float64];
    equal types give themselves; anything else is [None]. *)
