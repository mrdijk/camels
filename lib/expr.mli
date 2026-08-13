(** Expressions: an inspectable AST for predicates and computed columns.
    Kept opaque so callers build expressions only through the exposed
    constructors/operators, never by pattern-matching on internals. *)

type t

module StrSet : Set.S with type elt = string

(** Which columns this expression reads, without evaluating it. *)
val required_columns : t -> StrSet.t

val eval : Row.t -> t -> Value.t
val eval_bool : Row.t -> t -> bool
val to_string : t -> string

(** Vectorized evaluation: apply this expression to a whole batch at once. *)
val eval_batch : Batch.t -> t -> Series.t

(** Vectorized predicate: produce a boolean mask array for a whole batch. *)
val eval_mask : Batch.t -> t -> bool array

(* --- constructors --- *)
val col : string -> t
val lit : Value.t -> t

(* --- operand coercion, so `>.` etc. accept either an expr or a raw value --- *)
type operand = E of t | I of int | F of float | S of string | B of bool

val ( >. ) : t -> operand -> t
val ( <. ) : t -> operand -> t
val ( =. ) : t -> operand -> t
val neg : t -> t
