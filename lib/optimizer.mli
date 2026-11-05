(** Semantics-preserving plan rewrites. *)

val pushdown : Plan.t -> Expr.t list -> Plan.t
(** Predicate pushdown. [pushdown plan preds] rebuilds [plan] with the
    conjuncts [preds] (which filter its output) applied as deep as is safe.
    Filters are split on [&&] and move through [with_columns]/[select]
    (only if no aggregation is involved and only on pass-through columns),
    through [group_by] (key columns only) and into join sides (left side for
    inner/left joins, right side for inner joins), landing in the scan. *)

val prune : Plan.t -> string list option -> Plan.t
(** Projection pushdown. [prune plan (Some cols)] means the consumer reads
    only [cols]; [None] means all. Sets scan projections and drops unused
    [with_columns] expressions, keeping join ["_right"] naming stable. *)

val optimize : Plan.t -> Plan.t
(** [prune (pushdown plan []) None]. *)
