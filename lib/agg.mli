type kind = Sum | Count | Mean | Min | Max

type t = { expr : Expr.t; kind : kind; alias : string option }

val sum   : Expr.t -> t
val count : Expr.t -> t
val mean  : Expr.t -> t
val min   : Expr.t -> t
val max   : Expr.t -> t
val alias : string -> t -> t

val output_name  : t -> string
val output_dtype : Schema.t -> t -> Dtype.t
val to_string    : t -> string

type acc

val init_acc     : t -> acc
val update_acc   : t -> acc -> Value.t option -> unit
val finalize_acc : t -> acc -> Value.t option
