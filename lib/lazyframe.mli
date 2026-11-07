(** The lazy API. Every function only extends the logical plan; nothing runs
    until {!collect}. The frame is always the last argument so calls chain
    with [|>]. *)

(** A lazy query. *)
type t

(** Result of {!group_by}, awaiting {!agg}. *)
type grouped

type join_type = Plan.join_type = Inner | Left | Full

(** Wrap an eager dataframe as a lazy query. *)
val of_df : Dataframe.t -> t

(** Keep rows where the predicate is [true]; null counts as false. *)
val filter : Expr.t -> t -> t

(** Projection: the output is exactly these expressions. If all of them are
    scalars (e.g. only aggregations) the result has a single row. *)
val select : Expr.t list -> t -> t

(** Add one column, or replace the same-named one in place. The
    expression sees the input frame. *)
val with_column : Expr.t -> t -> t

(** Add columns, or replace same-named ones in place. All expressions see the
    input frame, not each other's results. *)
val with_columns : Expr.t list -> t -> t

(** Start a grouped query; finish it with {!agg}. *)
val group_by : Expr.t list -> t -> grouped

(** Output: the key columns followed by the aggregate columns. *)
val agg : Expr.t list -> grouped -> t

(** [lf |> join ~on:["id"] other]: [lf] is the left side, [how] defaults to
    [Inner]. Give either [~on] or both [~left_on] and [~right_on], otherwise
    {!Err.Polars_error} is raised. Right keys are dropped from the output;
    colliding right columns get a ["_right"] suffix. Null keys never match. *)
val join :
  ?how:join_type ->
  ?on:string list ->
  ?left_on:string list ->
  ?right_on:string list ->
  t ->
  t ->
  t

(** Validate, optionally optimise (default [true]), then execute. *)
val collect : ?optimize:bool -> t -> Dataframe.t

(** Pretty-printed plan (optimised by default). *)
val explain : ?optimize:bool -> t -> string

(** Output column names. *)
val schema : t -> string list
