(* Expressions: an inspectable AST for predicates and computed columns.
    Kept opaque so callers build expressions only through the exposed *)

type t

module StrSet : Set.S with type elt = string

(* --- constructors --- *)
val col : string -> t
val lit : Value.t -> t
val null : t

(* --- operand coercion, so `>.` etc. accept either an expr or a raw value --- *)
type operand = E of t | I of int | F of float | S of string | B of bool

val ( >. ) : t -> operand -> t
val ( <. ) : t -> operand -> t
val ( =. ) : t -> operand -> t
val ( *. ) : t -> operand -> t
val neg : t -> t

val required_columns : t -> StrSet.t
val output_dtype : Schema.t -> t -> Dtype.t
val output_name : t -> string option
val compile : (string * int) list -> t -> (Row.t -> Value.t option)
val compile_bool : (string * int) list -> t -> (Row.t -> bool)

val to_string : t -> string
