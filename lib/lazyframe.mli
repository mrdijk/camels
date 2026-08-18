(* The user-facing API: builds a Plan lazily, executes only on [collect]. *)
type t

val of_rows : Row.t list -> columns:string list -> t
val filter : t -> Expr.t -> t
val select : t -> string list -> t
val with_column : t -> string -> Expr.t -> t

val join : t -> t -> left_on:string list -> right_on:string list -> how:Plan.join_how -> t

type grouped

val schema : t -> Schema.t
val explain : ?optimize:bool -> t -> string

val group_by : t -> string list -> grouped
val agg : grouped -> Agg.t list -> t

val collect : ?optimize:bool -> t -> Row.t list
