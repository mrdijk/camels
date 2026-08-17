(** The user-facing API: builds a Plan lazily, executes only on [collect]. *)
type t

val of_rows : Row.t list -> columns:string list -> t
val filter : t -> Expr.t -> t
val select : t -> string list -> t
val schema : t -> Schema.t
val collect : t -> Row.t list
val explain : t -> string
