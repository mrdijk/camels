# Camels

A small [Polars](https://pola.rs)-style dataframe library for OCaml with a **lazy query API**:
build a query with `filter`, `select`, `with_column`, `group_by |> agg` and `join`, look at the plan,
and run it with `collect`. Columns are typed, nullable and stored in flat arrays; an optimizer pushes
filters and column pruning down to the scans before anything executes.

* OCaml ≥ 4.14, dune ≥ 3.0, **no dependencies**
* Includes a CSV reader with type inference
* Status: early. The API is currenlty small but it is tested on a 1M-row CSV and roughly on par with pandas
  and 2-6x slower than Polars on one core; see [`bench/README.md`](bench/README.md) for measured numbers.

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
integers too large for `int` become floats. Whitespace is never trimmed. See `src/io.mli` for all options.

**`scan_csv` is genuinely lazy.** Creating the scan reads only the header and a sample for type
inference. When the query is collected, the optimizer has pushed the column selection and the filters
*into the scan*: the file is streamed in chunks (`~chunk_bytes`, default 16 MB), only the columns the
query needs are parsed, and rows that fail a pushed-down filter are dropped chunk by chunk, before they
are ever held in memory.

```ocaml
Io.scan_csv "orders.csv"
|> L.filter Expr.(col "category" = str "c3" && col "amount" > float 900.)
|> L.group_by [ Expr.col "user_id" ]
|> L.agg [ Expr.(sum (col "amount") |> alias "total") ]
|> L.collect
```

```
AGGREGATE
  BY [col("user_id")]
  AGGS [col("amount").sum().alias("total")]
  FROM
    CSV SCAN orders.csv [id, user_id, category, amount, qty]
      PROJECT 2/5 COLUMNS
      SELECTION: ((col("category") == "c3") & (col("amount") > 900.0))
```

(`PROJECT 2/5` is what survives the scan: `user_id` and `amount`. The filter's own column,
`category`, is parsed as well, so 3 of the 5 columns are read; `id` and `qty` are skipped.) This is
`examples/scan_demo.ml`.

On a 197 MB file this query peaks at about 130 MB of memory (eager read-then-query: 926 MB), and the
peak does not grow with the file size. Operators above the scan (joins, aggregations, sorts) still need
their input in memory, so this bounds the *scan*, not arbitrary queries. The file must not change
between the scan and the collect.

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

Public modules (`open Camels`): `Lazyframe`, `Expr`, `Dataframe`, `Series`, `Dtype`, `Value`, `Io`,
`Plan`, `Source`, `Csv_options`, `Bitmap`, `Err`. `Eval`, `Exec`, `Optimizer`, `Groups`, `Csv` and `Scan` are
private to the library.

| file | role |
|------|------|
| `dtype`, `bitmap`, `value`, `series`, `dataframe` | eager data model: typed buffers + validity bitmap |
| `expr` | expression AST and DSL |
| `eval` | typed expression kernels; scalar/column broadcasting; frame and per-group contexts |
| `source`, `csv_options` | where a scan reads from (an in-memory frame or a CSV file) as plain data: schema, options, description |
| `plan` | logical plan, schema resolution, validation, the `explain` printer |
| `scan` | how each kind of source is read (chunked); the executor goes through it and never mentions a format |
| `optimizer` | predicate and projection pushdown |
| `groups` | typed group and join keys: dense integer codes, direct-lookup or open-addressing tables |
| `exec` | executor: select, with_columns, group-by, counting-sort hash join, and one generic scan branch |
| `lazy_frame` | the public lazy API |
| `csv` | single-pass CSV engine: in-place number parsing straight into typed buffers, chunked reading (used by `scan` and `io`) |
| `io` | public CSV API (`read_csv`, `read_csv_string`, `scan_csv`) |

Every module has an `.mli`; the doc comments sit directly **above** the declaration they describe.

## Performance notes

Measured on one core (1M-row, 25 MB CSV; full tables, methodology and caveats in
[`bench/README.md`](bench/README.md)):

| operation                       | this library | Polars | pandas |
|---------------------------------|-------------:|-------:|-------:|
| read CSV                        |       241 ms | 117 ms | 246 ms |
| filter                          |        35 ms |   7 ms |  41 ms |
| group by (20 groups)            |        38 ms |  17 ms |  60 ms |
| group by (10k groups)           |        14 ms |  17 ms |  20 ms |
| join + group by                 |        61 ms |  25 ms | 111 ms |

How it gets there:

* Columns are flat `int array` / `float array` / `string array` plus a validity bitmap, so kernels run
  over unboxed data and dispatch on type once per column. Comparison and arithmetic kernels are
  written as explicit loops (no closure per element), with column-by-scalar variants that never
  broadcast the scalar.
* Group-by and join keys become dense integer codes: a direct-lookup table when the integer range is
  small, an open-addressing table otherwise, strings and floats through typed hashtables. Joins bucket
  the right side with a counting sort. No boxed per-row keys are built.
* The CSV reader scans bytes in place, parses integers and floats without allocating (floats use the
  exact Clinger fast path and fall back to `float_of_string`), and writes each column directly into its
  typed buffer.

Still slow, on purpose or not yet addressed: strings are one OCaml string per value (no dictionary
encoding), a scalar meeting a string column is broadcast, there is no parallelism, and the `&&` kernel
and `Dataframe.take` are plain loops. Nothing here is tuned for more than one core.

## Development

```sh
dune build                          # build everything
dune test                           # run the tests
dune exec examples/main.exe         # demo with plans
dune build @doc                     # API docs (needs: opam install odoc)
```

`dune test` runs five suites (about 34,000 checks):

* `test.ml`: hand-checked cases for each operation, plus a fuzzer: 3000 random plans run with and
  without the optimizer must give identical results.
* `test_keys.ml`: group-by and join (all key types, multi-key, nulls, empty frames, all join types)
  against naive reference implementations.
* `test_csv.ml`: the CSV engine against the original reader on 4000 random CSV texts, bit-exact float
  parsing on 20,000 random numbers, and chunk-boundary reads with chunk sizes down to 16 bytes.
* `test_scan.ml`: lazy scans equal eager reads under tiny chunk sizes, and projection/predicate
  pushdown demonstrably reaches the file.
* `test_io.ml`: hand-picked CSV cases.

The tests were checked by mutation: breaking the optimizer, the key codes, the join bookkeeping, the
float fast path or the chunk handling on purpose makes them fail.

If you use `ocamlformat`, set `doc-comments = before` in `.ocamlformat` to keep the comment style above.

## Not implemented

`sort` and `limit`; cross/semi/anti joins; casts; string and date functions; window expressions;
CSV writing; Parquet; streaming execution of operators above the scan (aggregations and joins still
need their input in memory); parallelism; dictionary-encoded strings.

## License

GLP-3.0, see `LICENSE`.
