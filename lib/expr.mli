(* Expressions: an inspectable AST for predicates and computed columns.
    Kept opaque so callers build expressions only through the exposed *)

type t

module StrSet : Set.S with type elt = string

(* --- constructors --- *)
val col : string -> t
val lit : Value.t -> t

(* --- operand coercion, so `>.` etc. accept either an expr or a raw value --- *)
type operand = E of t | I of int | F of float | S of string | B of bool

val ( >. ) : t -> operand -> t
val ( <. ) : t -> operand -> t
val ( =. ) : t -> operand -> t
val neg : t -> t

(* Returns which columns this expression reads, without evaluating it. *)
val required_columns : t -> StrSet.t

(* Infers dtype given a parent schema *)
val output_dtype : Schema.t -> t -> Dtype.t

(* Human-readable name for this expresion's output column *)
val output_name : t -> string option

(* Compile expression into a closure over row indices, gibven a name->index map*)
val compile : (string * int) list -> t -> (Row.t -> Value.t)

(* Convenience built on top of compile, for predicates specifically. *)
val compile_bool : (string * int) list -> t -> (Row.t -> bool)

val to_string : t -> string
