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
 
(** Name of the resulting column: leftmost column, or the alias; ["literal"]
    for literals and ["len"] for {!len}. *)
val output_name : t -> string
 
(** Referenced column names, deduplicated, in first-use order. *)
val columns : t -> string list
 
(** Whether the expression contains an aggregation. *)
val has_agg : t -> bool
 
(** Flattens nested [And]s into conjuncts. *)
val split_and : t -> t list
 
(** Inverse of {!split_and}; [None] for the empty list. *)
val and_all : t list -> t option
 
(** Operator symbol, as printed by [explain]. *)
val binop_to_string : binop -> string
 
(** Aggregation name, as printed by [explain]. *)
val agg_to_string : agg -> string
 
(** Polars-style rendering, as shown by [explain]. *)
val to_string : t -> string
 
(** {1 Constructors} *)
 
(** Reference a column by name. *)
val col : string -> t
 
(** Integer literal. *)
val int : int -> t
 
(** Float literal. *)
val float : float -> t
 
(** String literal. *)
val str : string -> t
 
(** Boolean literal. *)
val bool : bool -> t
 
(** The null literal; it takes the type of whatever it is combined with. *)
val null : t
 
(** [e |> alias "name"] *)
val alias : string -> t -> t
 
(** Logical negation; null stays null. *)
val not_ : t -> t
 
(** [true] where the value is null (never null itself). *)
val is_null : t -> t
 
(** Negation of {!is_null}. *)
val is_not_null : t -> t
 
(** {1 Aggregations} *)
 
val sum : t -> t
val mean : t -> t
val min : t -> t
val max : t -> t
val count : t -> t
val n_unique : t -> t
val first : t -> t
val last : t -> t
val len : unit -> t

(** {1 Operators} *)

 
(** Addition; also concatenates two strings. *)
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

(** Logical and with Kleene logic: [false && null] is [false]. *)
val ( && ) : t -> t -> t
 
(** Logical or with Kleene logic: [true || null] is [true]. *)
val ( || ) : t -> t -> t
