(** The logical query plan: a tree of operators, built lazily. *)
type t

val scan : Row.t list -> columns:string list -> t
val filter : t -> Expr.t -> t
val select : t -> string list -> t
val schema : t -> Schema.t          (* structural, no data touched *)
val execute_batched : t -> Row.t list Seq.t
val explain : t -> string
