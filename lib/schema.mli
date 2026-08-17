type column = { name : string; dtype : Dtype.t; nullable : bool }
type t

val make : column list -> t
val column_names : t -> string list
val dtype_of : t -> string -> Dtype.t
val nullable_of : t -> string -> bool
val col_map : t -> (string * int) list
val index_of : t -> string -> int
val select : t -> string list -> t
val with_column : t -> string -> Dtype.t -> nullable:bool -> t
val merge : t -> ?suffix:string -> t -> t
val infer_from_rows : string list -> Row.t list -> t
val to_string : t -> string
val force_nullable : t -> t
