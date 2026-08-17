(* The logical query plan: a tree of operators, built lazily. *)
type t

val scan : Row.t list -> columns:string list -> t
val filter : t -> Expr.t -> t
val project : t -> (string * Expr.t) list -> t
val with_column :  t -> string -> Expr.t -> t
val schema : t -> Schema.t          
val execute_batched : t -> Row.t list Seq.t
val explain : t -> string
