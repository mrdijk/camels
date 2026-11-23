# Benchmarks

Workload: `orders.csv` (1,000,000 rows x 5 columns, 5% null amounts) and `users.csv` (10,000 rows).

```sh
python3 bench/gen_data.py /tmp/bench                                  # generate data
dune build --profile release                                          # release matters: dev builds are slower
./_build/default/bench/bench.exe /tmp/bench/orders.csv /tmp/bench/users.csv
./_build/default/bench/probe.exe /tmp/bench/orders.csv                # where does the time go?
python3 bench/bench.py                                                # Polars + pandas, run from the data dir
```

## Results (2026-11, 1 core / 3 GB sandbox, OCaml 4.14, Polars 2.1.0, pandas 3.0.2)

Wall-clock ms; queries are best of 3, the CSV read is a single run. Treat these as order-of-magnitude:
a shared sandbox is noisy.

| operation                        | this library | Polars | pandas | vs Polars | vs pandas |
|----------------------------------|-------------:|-------:|-------:|----------:|----------:|
| read_csv (1M x 5)                |       1265.5 |  121.3 |  223.1 |     10.4x |      5.7x |
| filter (amount>500 & cat=c3)     |         67.8 |    4.6 |   38.0 |     14.7x |      1.8x |
| with_column + sum                |         26.2 |    1.4 |    4.4 |     18.7x |      6.0x |
| group_by category (20 groups)    |        131.3 |   14.2 |   57.6 |      9.2x |      2.3x |
| group_by user_id (10k groups)    |        297.0 |   14.4 |   20.7 |     20.6x |     14.3x |
| join users + group by country    |        475.0 |   21.7 |  125.0 |     21.9x |      3.8x |
| peak memory (MB)                 |          512 |    207 |    375 |      2.5x |      1.4x |

## What the probes say

* Type inference is not the CSV bottleneck (1313 ms inferred vs 1285 ms with a schema given): the time
  is in tokenizing and in allocating a string and an option per field.
* Group-by is dominated by key hashing. `group_by category |> agg [len]` still takes ~190 ms with no
  aggregate work, because each row builds a boxed `Value.t list` key.
* A 32 MB minor heap changes nothing, so the minor GC is not the limit.
* Pass-through `select` costs 0 ms: columns are shared, not copied.
