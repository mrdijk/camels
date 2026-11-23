# /// script
# requires-python = ">=3.14"
# dependencies = [
#     "pandas>=3.1.1",
#     "polars>=2.1.0",
# ]
# ///
import time, resource
import polars as pl, pandas as pd


ORDERS = "bench/tmp/orders.csv"
USERS = "bench/tmp/users.csv"

def best(n, f):
    b = 1e9; r = None
    for _ in range(n):
        t = time.perf_counter(); r = f(); b = min(b, time.perf_counter() - t)
    return r, b
def rep(name, t, rows): print(f"{name:<34} {t*1000:8.1f} ms   ({rows} rows)")

print("== polars", pl.__version__, "==")
t = time.perf_counter(); o = pl.read_csv(ORDERS); rep("read_csv orders (1M x 5)", time.perf_counter()-t, o.height)
t = time.perf_counter(); u = pl.read_csv(USERS);  rep("read_csv users (10k x 3)", time.perf_counter()-t, u.height)
ol, ul = o.lazy(), u.lazy()
def run(name, q):
    r, t = best(3, lambda: q.collect()); rep(name, t, r.height)
run("filter (amount>500 & cat=c3)", ol.filter((pl.col("amount") > 500) & (pl.col("category") == "c3")))
run("with_column + sum", ol.with_columns((pl.col("amount")*1.2+1).alias("gross")).select(pl.col("gross").sum().alias("total")))
run("group_by category (20 groups)", ol.group_by("category", maintain_order=True).agg(pl.col("amount").sum().alias("total"), pl.col("amount").mean().alias("avg"), pl.len().alias("n")))
run("group_by user_id (10k groups)", ol.group_by("user_id", maintain_order=True).agg(pl.col("amount").sum().alias("total")))
run("join users + group by country", ol.join(ul, on="user_id", how="inner").group_by("country", maintain_order=True).agg(pl.col("amount").sum().alias("revenue")))
print("peak RSS", resource.getrusage(resource.RUSAGE_SELF).ru_maxrss, "kB")

print("\n== pandas", pd.__version__, "==")
t = time.perf_counter(); po = pd.read_csv(ORDERS); rep("read_csv orders (1M x 5)", time.perf_counter()-t, len(po))
t = time.perf_counter(); pu = pd.read_csv(USERS);  rep("read_csv users (10k x 3)", time.perf_counter()-t, len(pu))
def prun(name, f):
    r, t = best(3, f); rep(name, t, len(r) if hasattr(r, "__len__") else 1)
prun("filter (amount>500 & cat=c3)", lambda: po[(po.amount > 500) & (po.category == "c3")])
prun("with_column + sum", lambda: (po.amount*1.2+1).sum())
prun("group_by category (20 groups)", lambda: po.groupby("category", sort=False).agg(total=("amount","sum"), avg=("amount","mean"), n=("amount","size")))
prun("group_by user_id (10k groups)", lambda: po.groupby("user_id", sort=False).agg(total=("amount","sum")))
prun("join users + group by country", lambda: po.merge(pu, on="user_id").groupby("country", sort=False).agg(revenue=("amount","sum")))
print("peak RSS", resource.getrusage(resource.RUSAGE_SELF).ru_maxrss, "kB")
