(** The logical query plan: a tree of operators, built lazily. *)

type t

val scan : Row.t list -> chunk_size:int -> t
val filter : t -> Expr.t -> t
val execute : t -> Batch.t Seq.t

