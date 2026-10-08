type t = TInt | TFloat | TStr | TBool

val of_value : Value.t -> t
val to_string : t -> string
val widen : t -> t -> t
