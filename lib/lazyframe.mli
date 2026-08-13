(** The user-facing API: builds a Plan lazily, executes only on [collect]. *)

type t

val of_rows : Row.t list -> t
val of_rows_chunked : Row.t list -> chunk_size:int -> t
val filter : t -> Expr.t -> t
val collect : t -> Row.t list
