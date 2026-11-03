# Camels

A small Polars-style **lazy** dataframe library in OCaml (≥ 4.14, dune ≥ 3.0, no dependencies).

```ocaml
open Polars
module L = Lazy_frame

let result =
  let open Expr in
  L.of_df employees
  |> L.join ~on:["dept_id"] (L.of_df departments)
  |> L.filter (col "region" = str "EU" && col "salary" > float 60.)
  |> L.with_column (col "salary" * float 1.1 |> alias "raised")
  |> L.group_by [col "dept_name"]
  |> L.agg [len () |> alias "headcount"; sum (col "raised") |> alias "payroll"]
  |> L.select [col "dept_name"; col "payroll"]
  |> L.collect
```

## Operations

| Polars                         | here                                              |
|--------------------------------|---------------------------------------------------|
| `df.lazy()`                    | `Lazy_frame.of_df df`                             |
| `.filter(expr)`                | `L.filter expr`                                   |
| `.select([...])` (project)     | `L.select [...]`                                  |
| `.with_columns / with_column`  | `L.with_columns [...]` / `L.with_column e`        |
| `.group_by(keys).agg(aggs)`    | `L.group_by keys \|> L.agg aggs`                   |
| `.join(other, on=…, how=…)`    | `L.join ~how:(Inner\|Left\|Full) ~on:[..] other`   |
| `.collect()` / `.explain()`    | `L.collect ?optimize` / `L.explain ?optimize`     |

Expressions: `col lit(int/float/str/bool/null)`, `+ - * /`, `= <> < <= > >=`, `&& ||`,
`not_ is_null is_not_null alias`, and aggregations `sum mean min max count len n_unique first last`.
Inside `Expr.( ... )` the infix operators build expressions rather than computing values.

## Layout

| file            | role |
|-----------------|------|
| `value.ml`, `series.ml`, `dataframe.ml` | eager data model (boxed values, `Null` in every type) |
| `expr.ml`       | expression AST + DSL |
| `eval.ml`       | expression evaluator (scalar/column broadcasting; frame & per-group contexts) |
| `plan.ml`       | logical plan, schema resolution, validation, `explain` printer |
| `optimizer.ml`  | predicate pushdown + projection pushdown |
| `exec.ml`       | executor: hash group-by, hash join (inner/left/full), select, with_columns |
| `lazy_frame.ml` | public lazy API |

## Semantics worth knowing

* Nothing runs until `collect`; unknown columns / duplicate outputs are reported then (before execution).
* Nulls: arithmetic and comparisons with null give null; `&&`/`||` use Kleene logic; a null filter result drops the row;
  null group keys form their own group; null join keys never match; `sum` ignores nulls; `n_unique` counts null.
* `/` always yields float. `select` of only aggregations yields one row; aggregations next to columns broadcast.
* Group output order = first appearance. Join output order = left order, then matches in right order.
* Join: right key columns are dropped (left keys are the output keys; for a full join they are coalesced);
  colliding non-key right columns get a `_right` suffix.

## Optimizer (all rewrites are guarded)

* Filters are split on `&&` and pushed through `with_columns`/`select` (only if no aggregation is involved and
  the predicate uses pass-through columns), through `group_by` (key columns only), and into join sides
  (left side for inner/left joins; right side for inner joins), ending up in the scan's `SELECTION`.
* Scans are told which columns the plan really needs (`PROJECT n/m COLUMNS`); unused `with_column`s are dropped.
  The `_right` renaming is kept stable while pruning.

## Tests

`dune test` runs hand-written cases **and** a 3000-plan differential fuzzer asserting that the optimised and
unoptimised plans produce identical results (deliberately breaking either pushdown rule makes it fail).
`dune exec bin/main.exe` prints a demo with naive vs optimised plans.

## Not implemented (by design, for now)

sort/limit, other join types (cross/semi/anti), typed columnar storage (columns are `Value.t array`),
casts, string/date functions, window expressions, IO (CSV/Parquet), streaming/parallel execution.
