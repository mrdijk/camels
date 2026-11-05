(** The lazy API. Every function only extends the logical plan; nothing runs
    until [collect]. [t] is always the last argument so calls chain with [|>]. *)

type t = Plan.t
type join_type = Plan.join_type = Inner | Left | Full
type grouped = { input : t; keys : Expr.t list }

let of_df (df : Dataframe.t) : t = Scan { df; projection = None; predicate = None }

(** [df |> Lazy_frame.filter Expr.(col "a" > int 3)] *)
let filter pred (t : t) : t = Filter (t, pred)

(** Projection: output exactly these expressions. *)
let select exprs (t : t) : t = Select (t, exprs)

let with_columns exprs (t : t) : t = With_columns (t, exprs)
let with_column expr (t : t) : t = With_columns (t, [ expr ])

let group_by keys (t : t) : grouped = { input = t; keys }
let agg aggs (g : grouped) : t = Aggregate { input = g.input; keys = g.keys; aggs }

(** [lf |> join ~on:["id"] other] — [lf] is the left side. Specify either
    [~on] or both [~left_on] and [~right_on]. Defaults to an inner join. *)
let join ?(how = Inner) ?on ?left_on ?right_on (right : t) (left : t) : t =
  let left_on, right_on =
    match (on, left_on, right_on) with
    | Some k, None, None -> (k, k)
    | None, Some l, Some r -> (l, r)
    | _ -> Err.fail "join: give either ~on, or both ~left_on and ~right_on"
  in
  Join { left; right; left_on; right_on; how }

(** Run the plan. [optimize] (default true) enables predicate and projection
    pushdown. Raises [Err.Polars_error] on schema or type errors. *)
let collect ?(optimize = true) (t : t) : Dataframe.t =
  Plan.validate t;
  Exec.run (if optimize then Optimizer.optimize t else t)

let explain ?(optimize = true) (t : t) : string =
  Plan.validate t;
  Plan.to_string (if optimize then Optimizer.optimize t else t)

let schema (t : t) = Plan.schema t
