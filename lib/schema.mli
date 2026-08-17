type column = { name : string; dtype : Dtype.t }

type t

val make : column list -> t
val column_names : t -> string list
val dtype_of : t -> string -> Dtype.t
val index_of : t -> string -> int
val col_map : t -> (string * int) list
val infer_from_rows : string list -> Row.t list -> t
val to_string : t -> string
val select : t -> string list -> t
