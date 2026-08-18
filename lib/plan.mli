(* The logical query plan: a tree of operators, built lazily. *)
type join_how = Inner | Left | Full

type t

val scan : Row.t list -> columns:string list -> t
val filter : t -> Expr.t -> t
val project : t -> (string * Expr.t) list -> t
val with_column :  t -> string -> Expr.t -> t
val join : t -> t -> left_on:string list -> right_on:string list -> how:join_how -> t
val aggregate : t -> string list -> Agg.t list -> t

val schema : t -> Schema.t          
val execute_batched : t -> Row.t list Seq.t
val explain : t -> string
 
val optimize : t -> t
