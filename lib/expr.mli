(** Expressions and a small DSL for building them.

    {[ Expr.(col "salary" * float 1.1 |> alias "raised") ]}

    Inside [Expr.( ... )] the infix operators build expressions instead of
    computing values, shadowing the Stdlib ones. *)

type binop = Add | Sub | Mul | Div | Eq | Neq | Lt | Lt_eq | Gt | Gt_eq | And | Or

type agg = Sum | Mean | Min | Max | Count | Len | N_unique | First | Last

type t =
  | Col of string
  | Lit of Value.t
  | Binary of t * binop * t
  | Not of t
  | Is_null of t
  | Alias of t * string
  | Agg of agg * t

(** {1 Analysis (used by the planner and optimizer)} *)

val output_name : t -> string
(** Name of the resulting column: leftmost column, or the alias; ["literal"]
    for literals and ["len"] for {!len}. *)

val columns : t -> string list
(** Referenced column names, deduplicated, in first-use order. *)

val has_agg : t -> bool
val split_and : t -> t list
(** Flattens nested [And]s into conjuncts. *)

val and_all : t list -> t option
(** Inverse of {!split_and}; [None] for the empty list. *)

val binop_to_string : binop -> string
val agg_to_string : agg -> string
val to_string : t -> string

(** {1 Constructors} *)

val col : string -> t
val int : int -> t
val float : float -> t
val str : string -> t
val bool : bool -> t
val null : t

val alias : string -> t -> t
(** [e |> alias "name"] *)

val not_ : t -> t
val is_null : t -> t
val is_not_null : t -> t

(** {1 Aggregations} *)

val sum : t -> t
val mean : t -> t
val min : t -> t
val max : t -> t
val count : t -> t
(** Number of non-null values. *)

val n_unique : t -> t
(** Distinct values; null counts as one value. *)

val first : t -> t
val last : t -> t

val len : unit -> t
(** Number of rows (or rows in the group). *)

(** {1 Operators} *)

val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
(** Division always yields a float. *)

val ( = ) : t -> t -> t
val ( <> ) : t -> t -> t
val ( < ) : t -> t -> t
val ( <= ) : t -> t -> t
val ( > ) : t -> t -> t
val ( >= ) : t -> t -> t
val ( && ) : t -> t -> t
val ( || ) : t -> t -> t
(** Kleene three-valued logic. *)
