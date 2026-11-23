# /// script
# requires-python = ">=3.14"
# dependencies = [
#     "numpy>=2.5.4",
#     "pandas>=3.1.1",
# ]
# ///

"""
Generate the benchmark data: orders.csv (1M rows) and users.csv (10k rows).
Usage: python3 gen_data.py <outdir>
"""

import os, sys, numpy as np, pandas as pd

out = sys.argv[1] if len(sys.argv) > 1 else "."
rng = np.random.default_rng(42)
n = 1_000_000

os.makedirs(out, exist_ok=True)

df = pd.DataFrame({
    "id": np.arange(n),
    "user_id": rng.integers(0, 10_000, n),
    "category": rng.choice([f"c{i}" for i in range(20)], n),
    "amount": np.round(rng.random(n) * 1000, 2),
    "qty": rng.integers(1, 50, n),
})

df.loc[rng.random(n) < 0.05, "amount"] = np.nan   # 5% nulls
df.to_csv(f"{out}/orders.csv", index=False)

pd.DataFrame({"user_id": np.arange(10_000),
              "country": rng.choice(["NL","DE","FR","US","UK","ES","IT","PL"], 10_000),
              "tier": rng.integers(1, 4, 10_000)}).to_csv(f"{out}/users.csv", index=False)
