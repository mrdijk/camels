# Benchmarks

Workload: `orders.csv` (1,000,000 rows x 5 columns, 5% null amounts, 25 MB) and `users.csv` (10,000 rows).
Everything below was measured on **one core with 3 GB of RAM** (a shared sandbox, so treat results as
order-of-magnitude: run-to-run noise is around 10-20%). OCaml 4.14, release profile; Polars 2.1.0;
pandas 3.0.6.

```sh
python3 bench/gen_data.py /tmp/data         # writes /tmp/data/orders.csv and users.csv (stdlib only)
dune build --profile release                # release matters: dev builds are slower
./_build/default/bench/bench.exe /tmp/data/orders.csv /tmp/data/users.csv   # operations
python3 bench/bench.py /tmp/data            # Polars + pandas, same operations (pip install polars pandas)
./_build/default/bench/scan_bench.exe (eager|lazy) /tmp/data/orders.csv     # one scan query, own process
python3 bench/scan_bench.py (polars-lazy|polars-eager|pandas|pandas-usecols) /tmp/data
```

## 1. Operations

Wall-clock ms. Queries are best of 3; the CSV read is shown both as the best of 3 and as the first
(cold) run. "Before" is the library as it was at the start of this work (boxed per-field CSV parsing,
boxed group/join keys, closure-per-element kernels), measured in the same session.

| operation                      | before | after | speedup | Polars | pandas |
|--------------------------------|-------:|------:|--------:|-------:|-------:|
| read_csv (1M x 5), best of 3   |   1300 |   241 |    5.4x |    117 |    246 |
| read_csv (1M x 5), cold        |   1145 |   319 |    3.6x |      - |      - |
| filter (amount>500 & cat=c3)   |   73.8 |  35.0 |    2.1x |    6.7 |   41.0 |
| with_column + sum              |   27.0 |   9.1 |    3.0x |    1.6 |    4.1 |
| group_by category (20 groups)  |  144.2 |  38.1 |    3.8x |   16.8 |   60.4 |
| group_by user_id (10k groups)  |  328.2 |  14.4 |   22.8x |   16.9 |   19.8 |
| join users + group by country  |  517.2 |  61.1 |    8.5x |   24.9 |  110.7 |

Caveats: the Polars and pandas group-bys keep first-appearance order (`maintain_order=True`,
`sort=False`) to match this library's semantics, which is not their fastest mode; Polars is
multi-threaded in general but this machine has a single core, so its parallelism is not visible here.

## 2. Lazy scan: time and peak memory

One query, run in a fresh process per run: `filter category = "c3" and amount > 900`, then
`sum(amount)` and a count per `user_id` (3 of the 5 columns are needed). Best of 2-3 runs; Python
timings start after the imports.

| engine                          | 25 MB file (1M rows) | 197 MB file (8M rows) |
|---------------------------------|---------------------:|----------------------:|
| this library, eager read        |   257 ms / 138 MB    |    2277 ms / 926 MB   |
| **this library, `scan_csv`**    | **189 ms / 90 MB**   | **1443 ms / 130 MB**  |
| Polars `scan_csv`               |    76 ms / 93 MB     |     601 ms / 258 MB   |
| Polars eager                    |   110 ms / 133 MB    |     924 ms / 622 MB   |
| pandas (all columns)            |   278 ms / 138 MB    |    2195 ms / 672 MB   |
| pandas (`usecols`)              |   221 ms / 108 MB    |    1813 ms / 429 MB   |

The point of the lazy scan is the memory column: the eager path grows linearly with the file, the lazy
scan does not (90 MB at 25 MB of input, 130 MB at 197 MB; the remainder is the chunk buffer, the
runtime and the result). The same pushdown also makes it 1.4-1.6x faster here.

## 3. What the profile said, and what was done about it

Before the changes, probes showed that (a) type inference was *not* the CSV bottleneck (1313 ms
inferred vs 1285 ms with a schema given): the time went into tokenising and into a string plus an option
per field; (b) group-by was dominated by key hashing (`group_by category |> agg [len]` took ~190 ms with
no aggregate work, because every row built a boxed `Value.t list` key); (c) the minor GC was not the
limit (a 32 MB minor heap changed nothing). The fixes, in order of effect:

1. Typed group and join keys: dense integer codes, direct-lookup or open-addressing tables, counting-sort
   join. Group-by on 10k groups: 328 -> 14 ms. Join: 517 -> 61 ms.
2. A single-pass CSV engine parsing numbers in place into typed buffers: read 1300 -> 241 ms, and peak
   memory roughly halved.
3. Closure-free comparison/arithmetic kernels and column-by-scalar kernels: filter 74 -> 35 ms.
4. A lazy, chunked CSV scan with projection and predicate pushdown: constant-memory scans (section 2).

## 4. Gaps that remain

2-6x behind Polars on CSV reads, filters and group-bys. The profile points at: one OCaml string per
string value (no dictionary encoding, so reads and string comparisons allocate and chase pointers),
no SIMD or multi-threading, and `Dataframe.take` / the `&&` kernel being plain loops.
