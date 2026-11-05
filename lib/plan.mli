(** Logical plan: the tree a lazy frame builds before [collect]. *)

type join_type = Inner | Left | Full

type t =
  | Scan of {
      df : Dataframe.t;
      projection : string list option;
          (** columns kept after the scan; [None] keeps all *)
      predicate : Expr.t option;
          (** filter applied at the scan, before the projection *)
    }
  | Filter of t * Expr.t
  | Select of t * Expr.t list
  | With_columns of t * Expr.t list
  | Aggregate of { input : t; keys : Expr.t list; aggs : Expr.t list }
  | Join of {
      left : t;
      right : t;
      left_on : string list;
      right_on : string list;
      how : join_type;
    }

val join_type_to_string : join_type -> string

val join_right_columns :
  left_cols:string list ->
  right_cols:string list ->
  right_on:string list ->
  (string * string) list
(** Non-key right columns as [(input_name, output_name)]: right keys are
    dropped, and a name colliding with a left column gets a ["_right"] suffix. *)

val schema : t -> string list
(** Output column names, in order (no dtypes yet). *)

val validate : t -> unit
(** Checks that every referenced column exists, output names are unique and
    join keys are well-formed. Raises {!Err.Polars_error}. *)

val to_string : t -> string
(** Indented tree, as printed by [explain]. *)
