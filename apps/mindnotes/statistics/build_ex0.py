"""
Recompute all numbers and regenerate all plots for Statistics Exercise Set 0.

Outputs:
- Plots saved as PNG in apps/mindnotes/statistics/images/
- All numerical results printed to stdout for verification against the official solutions.
"""
import os
import pyreadr
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = os.path.dirname(os.path.abspath(__file__))
IMG = os.path.join(HERE, "images")
os.makedirs(IMG, exist_ok=True)

RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/ex0/Exe0_Data.Rdata"
data = pyreadr.read_r(RDATA)["Data_USA"].copy()

# Frost categorical -> ordered
frost_order = ["[0,60)", "[60,90)", "[90,120)", "[120,150)", "[150,180)", "[180,200]"]
data["Frost"] = pd.Categorical(data["Frost"], categories=frost_order, ordered=True)
hap_order = ["Unhappiest", "Quite unhappy", "So and so", "Quite happy", "Happiest"]
data["Happyness.Level"] = pd.Categorical(
    data["Happyness.Level"], categories=hap_order, ordered=True
)

print("=" * 70)
print("DATA: shape =", data.shape)
print("Frost NaN:", data["Frost"].isna().sum())
print("Happyness.Level NaN:", data["Happyness.Level"].isna().sum())
print("=" * 70)


def save(name):
    p = os.path.join(IMG, name)
    plt.tight_layout()
    plt.savefig(p, dpi=120, bbox_inches="tight")
    plt.close()
    print(f"  saved -> images/{name}")


# ---------------- Ex 1a: Frost — barplot vs histogram ----------------
print("\n[1a] Frost: barplot (wrong) vs density histogram (right)")
counts = data["Frost"].value_counts().reindex(frost_order)
props = counts / counts.sum()
edges = [0, 60, 90, 120, 150, 180, 200]
widths = np.diff(edges)
dens = props.values / widths
print("Frost counts:\n", counts)
print("Frost proportions:\n", props.round(2))
print("Frost widths:", widths)
print("Frost densities:", dens.round(5))

fig, axes = plt.subplots(1, 2, figsize=(10, 4))
axes[0].bar(range(len(frost_order)), counts.values, color="#5fa8d3", edgecolor="black")
axes[0].set_xticks(range(len(frost_order)))
axes[0].set_xticklabels(frost_order, rotation=30, ha="right")
axes[0].set_title("Bar plot: Frost (misleading)")
axes[0].set_ylabel("Counts")
for i in range(len(edges) - 1):
    axes[1].bar(
        edges[i], dens[i], width=widths[i], align="edge",
        color="#5fa8d3", edgecolor="black",
    )
axes[1].set_title("Histogram: Frost (density)")
axes[1].set_xlabel("Frost")
axes[1].set_ylabel("Density")
save("ex1a-frost-bar-vs-hist.png")

# ---------------- Ex 1b: build Density ----------------
print("\n[1b] Density = Population / Area")
data["Density"] = data["Population"] / data["Area"]
print(data[["State", "Population", "Area", "Density"]].head())

# ---------------- Ex 1c: histograms of Density ----------------
print("\n[1c] Density histograms (3 binnings)")
fig, axes = plt.subplots(1, 3, figsize=(13, 4))
axes[0].hist(data["Density"], bins=6, color="#5fa8d3", edgecolor="black")
axes[0].set_title("Histogram: Density, breaks=6")
axes[0].set_xlabel("Density"); axes[0].set_ylabel("Counts")
custom = [0, 30, 60, 100, 200, 300, 1200]
axes[1].hist(data["Density"], bins=custom, density=True,
             color="#5fa8d3", edgecolor="black")
axes[1].set_title("Histogram: Density, custom breaks (density)")
axes[1].set_xlabel("Density"); axes[1].set_ylabel("Density")
axes[2].hist(data["Density"], bins=20, color="#5fa8d3", edgecolor="black")
axes[2].set_title("Histogram: Density, breaks=20")
axes[2].set_xlabel("Density"); axes[2].set_ylabel("Counts")
save("ex1c-density-hist3.png")

# ---------------- Ex 1d: Rate.Violent ----------------
print("\n[1d] Rate.Violent = 100000*(Violent.Assault+Murder+Rape+Robbery)/Population")
data["Rate.Violent"] = (
    100000.0
    * (
        data["Violent.Assault"] + data["Violent.Murder"]
        + data["Violent.Rape"] + data["Violent.Robbery"]
    )
    / data["Population"]
)
print(data[["State", "Rate.Violent"]].head())

# ---------------- Ex 1e: Rate.Violent histograms ----------------
print("\n[1e] Rate.Violent histograms")
fig, axes = plt.subplots(2, 2, figsize=(11, 8))
axes[0, 0].hist(data["Rate.Violent"], bins=15, color="#5fa8d3", edgecolor="black")
axes[0, 0].set_title("Histogram: Rate.Violent, breaks=15")
axes[0, 0].set_xlabel("Rate.Violent"); axes[0, 0].set_ylabel("Counts")
axes[0, 1].hist(data["Rate.Violent"], bins=20, color="#5fa8d3", edgecolor="black")
axes[0, 1].set_title("Histogram: Rate.Violent, breaks=20")
axes[0, 1].set_xlabel("Rate.Violent"); axes[0, 1].set_ylabel("Counts")
vbreaks = [110, 200, 250, 300, 350, 400, 450, 500, 550, 600, 1050]
axes[1, 0].hist(data["Rate.Violent"], bins=vbreaks, color="#5fa8d3", edgecolor="black")
axes[1, 0].set_title("Histogram: Rate.Violent, custom (counts)")
axes[1, 0].set_xlabel("Rate.Violent"); axes[1, 0].set_ylabel("Counts")
axes[1, 1].hist(data["Rate.Violent"], bins=vbreaks, density=True,
                color="#5fa8d3", edgecolor="black")
axes[1, 1].set_title("Histogram: Rate.Violent, custom (density)")
axes[1, 1].set_xlabel("Rate.Violent"); axes[1, 1].set_ylabel("Density")
save("ex1e-rateviolent-hist.png")

# ---------------- Ex 1f: Property.Rates, % > 3000 ----------------
print("\n[1f] Property.Rates and proportion > 3000")
data["Property.Rates"] = (
    100000.0
    * (data["Property.Burglary"] + data["Property.Larceny"] + data["Property.Motor"])
    / data["Population"]
)
is_higher = data["Property.Rates"] > 3000
n_total = len(data)
n_higher = int(is_higher.sum())
print(f"Total states: {n_total}")
print(f"States with Property.Rates > 3000: {n_higher}")
print(f"distr.table.x result -> FALSE: {(~is_higher).sum()}  TRUE: {n_higher}")
print(f"Percent (TRUE row, col Percent): {100 * n_higher / n_total:.2f}")
print(f"sum(is.higher30): {n_higher}")
print(f"100*sum(...)/length: {100 * n_higher / n_total:.6f}")
print(f"mean(is.higher30): {n_higher / n_total:.8f}")

# Plot: histogram of Property.Rates with threshold at 3000
rates = data["Property.Rates"].values
fig, ax = plt.subplots(figsize=(8, 4.5))
counts_bin, edges_bin, patches = ax.hist(
    rates, bins=15, color="#5fa8d3", edgecolor="black"
)
# Color bins fully above 3000 in warm yellow
for patch, left in zip(patches, edges_bin[:-1]):
    if left >= 3000:
        patch.set_facecolor("#e0a800")
ax.axvline(3000, color="#b00020", linestyle="--", linewidth=1.5, label="threshold = 3000")
ax.set_xlabel("Property.Rates (per 100,000 pop.)")
ax.set_ylabel("Counts")
ax.set_title(
    f"Property.Rates across 51 states  —  {n_higher} states > 3000  "
    f"({100 * n_higher / n_total:.2f}%)"
)
ax.legend(loc="upper right", frameon=False)
save("ex1f-propertyrates-hist.png")

# ---------------- Ex 1g: P(Frost < 80) by uniform-on-interval ----------------
print("\n[1g] P(Frost < 80) via uniform-on-interval")
prop_0_60 = 0.20
prop_60_90 = 0.16
share = 20.0 / 30.0  # share of [60,90) that lies below 80
ans_1g = prop_0_60 + prop_60_90 * share
print(f"0.20 + 0.16 * 2/3 = {ans_1g:.7f}")
ans_1g_alt = (10 + 8 * 2 / 3) / 50.0
print(f"(10 + 8*2/3)/50  = {ans_1g_alt:.7f}")

# ---------------- Ex 1h: Happyness.Level ----------------
print("\n[1h] Happyness.Level cumulative + happy/happiest percentage")
hap = data["Happyness.Level"].dropna()
hcounts = hap.value_counts().reindex(hap_order)
hprops = hcounts / hcounts.sum()
print("counts:\n", hcounts)
print("props:\n", hprops.round(2))
hcum = hcounts.cumsum()
hcumprop = hprops.cumsum()
print("cum.count:\n", hcum)
print("cum.prop:\n", hcumprop.round(4))
print("Happiest + Quite happy prop:", round(hprops["Happiest"] + hprops["Quite happy"], 4))
print("Exact (8+7)/49 =", (8 + 7) / 49)
print("Cum prop at 'So and so':", round(hcumprop["So and so"], 7))

# ---------------- Ex 1i: Rate.murders by Density split ----------------
print("\n[1i] Rate.murders, split by Density < 100 vs >= 100")
data["Rate.murders"] = 100000.0 * data["Violent.Murder"] / data["Population"]
lt = data.loc[data["Density"] < 100, "Rate.murders"]
ge = data.loc[data["Density"] >= 100, "Rate.murders"]
print(f"states Density<100: {len(lt)}  states Density>=100: {len(ge)}")
bins_i = [0, 2, 4, 8, 12]
fig, axes = plt.subplots(1, 2, figsize=(10, 4))
axes[0].hist(lt, bins=bins_i, color="#5fa8d3", edgecolor="black", density=True)
axes[0].set_title("Rate.murders: Density < 100 (density)")
axes[0].set_xlabel("Rate.murders"); axes[0].set_ylabel("Density")
axes[1].hist(ge, bins=bins_i, color="#5fa8d3", edgecolor="black", density=True)
axes[1].set_title("Rate.murders: Density >= 100 (density)")
axes[1].set_xlabel("Rate.murders"); axes[1].set_ylabel("Density")
save("ex1i-rate-murders-split.png")
# Frequency tables
def tab(vals, breaks):
    cats = pd.cut(vals, bins=breaks, right=False)
    counts = cats.value_counts().sort_index()
    return counts, counts / counts.sum()
c_lt, p_lt = tab(lt, bins_i)
c_ge, p_ge = tab(ge, bins_i)
print("Rate.murders_lt_100 table:\n", pd.DataFrame({"Count": c_lt, "Prop": p_lt.round(2)}))
print("Rate.murders_ge_100 table:\n", pd.DataFrame({"Count": c_ge, "Prop": p_ge.round(2)}))

# ================== EXERCISE 2 — Titanic from prompt tables ==================
print("\n" + "=" * 70)
print("Exercise 2 — uses frequency tables given in the prompt")
print("=" * 70)

# Fare density table
fare_intervals = [(0, 10), (10, 20), (20, 30), (30, 60), (60, 100), (100, 180), (180, 270)]
fare_density = [0.03765, 0.02002, 0.0158, 0.00419, 0.00196, 0.00044, 0.00029]
# Proportion = density * width
fare_prop = [d * (b - a) for (a, b), d in zip(fare_intervals, fare_density)]
print("Fare density table -> proportions per interval:")
for (a, b), d, p in zip(fare_intervals, fare_density, fare_prop):
    print(f"  [{a},{b}): density={d}  prop={p:.5f}")
print("Sum of proportions:", round(sum(fare_prop), 5))
fare_cum = np.cumsum(fare_prop)

# ---------------- Ex 2a1: P(10 <= fare < 60) exact ----------------
print("\n[2a1] P(10 <= fare < 60) = prop[10,20) + prop[20,30) + prop[30,60)")
ans_2a1 = fare_prop[1] + fare_prop[2] + fare_prop[3]
print(f"  density form: (20-10)*0.02002 + (30-20)*0.0158 + (60-30)*0.00419 = "
      f"{(20-10)*0.02002 + (30-20)*0.0158 + (60-30)*0.00419:.4f}")
print(f"  Result: {ans_2a1:.4f}")

# ---------------- Ex 2a2: P(50 <= fare < 100) approx ----------------
print("\n[2a2] P(50 <= fare < 100) — partial interval [30,60), full [60,100)")
ans_2a2 = (60 - 50) * 0.00419 + (100 - 60) * 0.00196
ans_2a2_alt = (60 - 30) * 0.00419 * (1.0 / 3.0) + (100 - 60) * 0.00196
print(f"  (60-50)*0.00419 + (100-60)*0.00196 = {ans_2a2:.4f}")
print(f"  (60-30)*0.00419*(1/3) + (100-60)*0.00196 = {ans_2a2_alt:.4f}")

# ---------------- Ex 2a3: ogive ----------------
print("\n[2a3] Ogive of fare distribution")
edges_a = [0, 10, 20, 30, 60, 100, 180, 270]
fig, ax = plt.subplots(figsize=(6, 5))
ax.plot(edges_a, [0] + list(fare_cum), "o-", color="black")
ax.plot([edges_a[0], edges_a[-1]], [0, 1], "--", color="red", alpha=0.7)
ax.set_xlabel("fare"); ax.set_ylabel("Cumulative Proportion")
ax.set_title("Ogive: fare")
ax.set_ylim(0, 1.05)
save("ex2a3-fare-ogive.png")
print("Cumulative proportions at endpoints:")
for e, c in zip(edges_a[1:], fare_cum):
    print(f"  up to {e}: {c:.2f}")

# ---------------- Ex 2b1: P(size.family >= 4) ----------------
print("\n[2b1] P(size.family >= 4)")
size_vals = [0, 1, 2, 3, 4, 5, 8]
size_counts = [891, 319, 42, 20, 22, 6, 9]
n_size = sum(size_counts)
n_ge4 = sum(c for v, c in zip(size_vals, size_counts) if v >= 4)
print(f"  count(size.family >= 4) = 22+6+9 = {n_ge4}")
print(f"  total = {n_size}")
ans_2b1 = n_ge4 / n_size
print(f"  proportion = {n_ge4}/{n_size} = {ans_2b1:.8f}")

# ---------------- Ex 2b2: spike plot ----------------
print("\n[2b2] Spike plot: size.family")
size_prop = [c / n_size for c in size_counts]
fig, ax = plt.subplots(figsize=(6, 4))
ax.vlines(size_vals, 0, size_prop, color="black")
ax.plot(size_vals, size_prop, "o", color="black")
ax.set_xlabel("size.family"); ax.set_ylabel("Proportion")
ax.set_title("Spike plot: size.family")
ax.set_xticks(range(0, 9))
ax.set_ylim(0, max(size_prop) * 1.1)
save("ex2b2-size-family-spike.png")
for v, c, p in zip(size_vals, size_counts, size_prop):
    print(f"  {v}: count={c}  prop={p:.3f}")

# ---------------- Ex 2b3: cumulative step diagram ----------------
print("\n[2b3] Cumulative frequencies and step diagram for size.family")
size_cum = np.cumsum(size_counts)
size_cumprop = np.cumsum(size_prop)
for v, c, p, cc, cp in zip(size_vals, size_counts, size_prop, size_cum, size_cumprop):
    print(f"  {v}: Count={c}  Prop={p:.3f}  Cum.Count={cc}  Cum.Prop={cp:.3f}")

fig, ax = plt.subplots(figsize=(6, 4.5))
# Step diagram: piecewise-constant on integer steps
x_step = []; y_step = []
prev = 0.0
for v, p in zip(size_vals, size_cumprop):
    x_step += [v, v]
    y_step += [prev, p]
    prev = p
# Extend the final flat segment a little to the right
x_step += [size_vals[-1] + 1]
y_step += [prev]
ax.plot(x_step, y_step, color="black")
ax.plot([0, size_vals[-1] + 1], [0, 1], "--", color="red", alpha=0.7)
ax.set_xlabel("size.family"); ax.set_ylabel("Cumulative Proportion")
ax.set_title("Cumulative freq: size.family")
ax.set_ylim(0, 1.05)
ax.set_xticks(range(0, 10))
save("ex2b3-size-family-step.png")

print("\nDONE.")
