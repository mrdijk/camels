(** Physical execution of a logical plan: filter by mask, expression
    select / with_columns, hash group-by and hash join (inner, left, full). *)

(** Execute the plan. Group order is first appearance; join output follows
    left row order, with full-join leftovers from the right appended last.
    Raises {!Err.Polars_error} on type errors. *)
val run : Plan.t -> Dataframe.t
