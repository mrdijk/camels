# Camels

A small [Polars](https://pola.rs)-style dataframe library for OCaml with a **lazy query API**:
build a query with `filter`, `select`, `with_column`, `group_by |> agg` and `join`, look at the plan,
and run it with `collect`. Columns are typed, nullable and stored in flat arrays; an optimizer pushes
filters and column pruning down to the scans before anything executes.

* OCaml ≥ 4.14, dune ≥ 3.0, **no dependencies**
* Includes a CSV reader with type inference
* Status: early (0.1). The API is small and tested; performance has not been benchmarked yet.

## Quick start

```ocaml
open Camels
module L = Lazyframe

let orders =
  Io.read_csv_string
    "order_id,customer,amount,status\n\
     1,ann,10.5,paid\n\
     2,bob,20,paid\n\
     3,ann,5,refunded\n\
     4,cy,,paid\n\
     5,bob,7.25,paid\n"

let customers =
  Dataframe.create
    [ Series.of_strings "customer" [| "ann"; "bob"; "cy" |];
      Series.of_strings "country" [| "NL"; "DE"; "NL" |] ]

let () =
  let open Expr in
  L.of_df orders
  |> L.filter (col "status" = str "paid" && is_not_null (col "amount"))
  |> L.join ~on:[ "customer" ] (L.of_df customers)
  |> L.with_column (col "amount" * float 1.21 |> alias "gross")
  |> L.group_by [ col "country" ]
  |> L.agg [ len () |> alias "orders"; sum (col "gross") |> alias "revenue" ]
  |> L.collect
  |> Dataframe.print
```

```
shape: (2, 3)
+---------+--------+---------+
| country | orders | revenue |
| str     | i64    | f64     |
+---------+--------+---------+
| NL      | 1      | 12.705  |
| DE      | 2      | 32.9725 |
+---------+--------+---------+
```

This file is `examples/quickstart.ml`; run it with `dune exec examples/quickstart.exe`.
`examples/main.ml` is a bigger demo that also prints the naive and optimized plans.

## Install

```sh
opam install .                 # from a checkout
opam pin add polars .          # or pin it from another project
```

Then add `(libraries polars)` to your `dune` file and `open Polars` in your code.

## Building dataframes

From arrays, one `Series` per column (use the `of_opt_*` variants when there are nulls):

```ocaml
let df =
  Dataframe.create
    [ Series.of_ints "id" [| 1; 2; 3 |];
      Series.of_opt_strings "name" [| Some "a"; None; Some "c" |];
      Series.of_floats "score" [| 1.5; 2.0; 3.5 |] ]
```

From CSV:

```ocaml
let df  = Io.read_csv "data.csv"                                  (* eager *)
let lf  = Io.scan_csv ~sep:';' ~null_values:[ "NA"; "" ] "data.csv" (* lazy query on top *)
let df2 = Io.read_csv ~schema:[ ("zip", Dtype.Utf8) ] "data.csv"   (* pin a column's type *)
```

The CSV reader follows RFC 4180 (quoted separators, doubled quotes, embedded newlines), accepts LF and
CRLF line endings, ignores a UTF-8 BOM and skips blank lines. An unquoted field matching `null_values`
(default: the empty field) is null; a quoted empty field is an empty string. Each column's type is
inferred from its first `infer_rows` rows (default 1000): bool, then int, then float, otherwise string;
integers too large for `int` become floats. Whitespace is never trimmed. `scan_csv` reads the file
**eagerly**; only the query on top of it is lazy. See `src/io.mli` for all options.

## The lazy API

Every function only extends a logical plan and takes the frame as its **last** argument, so queries
chain with `|>`. Nothing runs until `collect`.

| Polars                          | here                                              |
|---------------------------------|---------------------------------------------------|
| `df.lazy()`                     | `L.of_df df`                                      |
| `.filter(expr)`                 | `L.filter expr`                                   |
| `.select([...])` (project)      | `L.select [...]`                                  |
| `.with_columns(...)`            | `L.with_columns [...]` / `L.with_column e`        |
| `.group_by(keys).agg(aggs)`     | `L.group_by keys \|> L.agg aggs`                  |
| `.join(other, on=…, how=…)`     | `L.join ~how:(Inner \| Left \| Full) ~on:[..] other` |
| `.collect()` / `.explain()`     | `L.collect ?optimize` / `L.explain ?optimize`     |

`join` also takes `~left_on`/`~right_on` for differently named keys. `lf |> L.join other` joins `lf`
(left) with `other` (right).

**Expressions** are built with the `Expr` module. Inside `Expr.( ... )` the usual operators build
expressions instead of computing values:

```ocaml
Expr.(col "price" * float 1.2 > int 100 && is_not_null (col "name"))
```

| kind          | functions |
|---------------|-----------|
| leaves        | `col`, `int`, `float`, `str`, `bool`, `null` |
| arithmetic    | `+ - * /` (`/` always gives a float; `+` also concatenates strings) |
| comparison    | `= <> < <= > >=` |
| logic         | `&& \|\|` (Kleene), `not_`, `is_null`, `is_not_null` |
| naming        | `e \|> alias "name"` |
| aggregations  | `sum mean min max count len n_unique first last` |

An expression's output name is its alias, otherwise its leftmost column (`len ()` is called `len`).

**Inspecting a plan.** `L.explain ~optimize:false q` prints the plan as built; `L.explain q` prints the
optimized one. For the demo query in `examples/main.ml`:

```
INNER JOIN ON [dept_id] = [dept_id]
  LEFT PLAN:
    DF [id, name, dept_id, salary]
      PROJECT 2/4 COLUMNS
      SELECTION: (col("salary") > 60.0)
  RIGHT PLAN:
    DF [dept_id, dept_name, region]
      PROJECT 2/3 COLUMNS
      SELECTION: (col("region") == "EU")
```

The excerpt shows the join part of the plan: both halves of the filter were pushed into their scans,
and each scan reads only the columns the rest of the plan needs.

## Semantics

* **Types.** Columns are `Int64`, `Float64`, `Utf8` or `Boolean`, each with an optional null mask.
  `Int64` is backed by OCaml's native 63-bit `int`; integer arithmetic wraps on overflow.
  `Int64` and `Float64` mix in arithmetic and comparison (the result is a float); anything else is a
  type error.
* **Nulls.** Arithmetic and comparison with null give null. `&&`/`||` use Kleene logic
  (`false && null` is `false`). A null filter result drops the row. `sum`, `mean`, `min`, `max` and
  `count` skip nulls; `n_unique` counts null as a value; the sum of nothing is `0`.
* **Grouping.** Null keys form their own group. Groups come out in order of first appearance.
* **Joins.** `Inner`, `Left` and `Full`, via a hash join. Null keys never match. Output is in left-row
  order, with unmatched right rows of a full join last. Right key columns are dropped (left keys are
  the output keys; for a full join they are filled in from the right when the left row is missing).
  A non-key right column whose name collides with a left column gets a `_right` suffix.
* **`select`.** The output is exactly the given expressions. If every expression is a scalar (for
  example only aggregations) the result has one row; scalars next to columns are broadcast.
* **Errors.** Everything raises `Err.Camels_error of string`: unknown columns and duplicate output
  names (reported at `collect`, before any work is done), type errors, malformed CSV, bad join specs.

```ocaml
match L.collect query with
| df -> Dataframe.print df
| exception Err.Camels_error msg -> prerr_endline msg
```

## Optimizer

`collect` optimizes by default (`~optimize:false` turns it off). Every rewrite is guarded, and the test
suite checks that optimized and unoptimized plans always agree.

* **Predicate pushdown.** Filters are split on `&&` and moved as deep as is safe: below `select` and
  `with_column` (only on pass-through columns and only if no aggregation is involved), below `group_by`
  (key columns only), and into join inputs (left side of inner/left joins, right side of inner joins),
  ending as the scan's `SELECTION`.
* **Projection pushdown.** Each scan is told which columns the plan actually uses
  (`PROJECT n/m COLUMNS`), and `with_column` expressions nobody reads are dropped. The `_right` naming
  of join output is kept stable while pruning.

Not done yet: pruning unused aggregations, constant folding, join reordering.

## Code layout

Public modules (`open Polars`): `Lazy_frame`, `Expr`, `Dataframe`, `Series`, `Dtype`, `Value`, `Io`,
`Plan`, `Bitmap`, `Err`. `Eval`, `Exec` and `Optimizer` are private to the library.

| file | role |
|------|------|
| `dtype`, `bitmap`, `value`, `series`, `dataframe` | eager data model: typed buffers + validity bitmap |
| `expr` | expression AST and DSL |
| `eval` | typed expression kernels; scalar/column broadcasting; frame and per-group contexts |
| `plan` | logical plan, schema resolution, validation, the `explain` printer |
| `optimizer` | predicate and projection pushdown |
| `exec` | executor: select, with_columns, hash group-by, hash join |
| `lazy_frame` | the public lazy API |
| `io` | CSV reading |

Every module has an `.mli`; the doc comments sit directly **above** the declaration they describe.

## Performance notes

Not benchmarked yet, so no numbers. The design choices that matter:

* Columns are flat `int array` / `float array` / `string array` plus a validity bitmap (one byte per
  row for now), so numeric kernels run over unboxed data and dispatch on type once per column.
* Known costs: kernels call a closure per element; a scalar meeting a column is broadcast to a
  constant column; group-by and join keys are still hashed as boxed `Value.t list`s; CSV parsing holds
  every field as a string before converting; whole files are read into memory.

## Development

```sh
dune build                          # build everything
dune test                           # run the tests
dune exec examples/main.exe         # demo with plans
dune build @doc                     # API docs (needs: opam install odoc)
```

`dune test` runs two suites. `test/test.ml` has hand-checked cases for each operation plus a
differential fuzzer: 3000 random plans run with and without the optimizer must give identical results
(breaking a pushdown rule on purpose makes it fail). `test/test_io.ml` covers the CSV reader.

If you use `ocamlformat`, set `doc-comments = before` in `.ocamlformat` to keep the comment style above.

## Not implemented

`sort` and `limit`; cross/semi/anti joins; casts; string and date functions; window expressions;
CSV writing; Parquet; streaming or parallel execution; a dedicated column-by-scalar kernel.

## License

GLP-3.0, see `LICENSE`.
