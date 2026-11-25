(** Expression evaluation over typed columns.

    An expression evaluates to a [Scalar] (literals, full-frame aggregates) or
    a [Column]. Binary operations dispatch on the operand dtypes once per
    column, then run a monomorphic loop over the buffers; validity masks are
    combined separately. Scalars meeting a column are broadcast to a constant
    column (a dedicated column-by-scalar kernel is a possible optimisation). *)

type ev = Scalar of Value.t | Column of Series.t

type ctx = {
  col : string -> ev;
  agg : Expr.agg -> Expr.t -> ev;
}

(** Evaluate an expression in the given context. *)
val eval : ctx -> Expr.t -> ev

(** Broadcast a scalar to [height] rows; columns are returned as is (their
    name is unspecified: rename before use). *)
val materialize : height:int -> ev -> Series.t

(** Aggregations reduce the whole frame to a scalar. *)
val frame_ctx : Dataframe.t -> ctx

(** [group_ctx df groups keys]: [groups] assigns every row of [df] to a group;
    [keys] maps key-column names to one value per group. Everything
    evaluates to one value per group; a non-key column used outside an
    aggregation raises {!Err.Polars_error}. *)
val group_ctx : Dataframe.t -> Groups.t -> (string * Series.t) list -> ctx

(** Scalar-scalar semantics (null-propagating; Kleene [And]/[Or]). *)
val apply_binop : Expr.binop -> Value.t -> Value.t -> Value.t

(** Aggregate a whole column. [Sum]/[Mean]/[Min]/[Max]/[Count] ignore nulls. *)
val reduce : Expr.agg -> Series.t -> Value.t

(** One aggregate per group, typed by {!Series.dtype} and the aggregation. *)
val reduce_groups : Expr.agg -> Series.t -> Groups.t -> Series.t
