"""Build AI walkthrough plot for Ex 1.1e — % pizzerias with Sales in [15000, 30000).

The exercise: what percentage of pizzerias has Sales in the "medium-turnover"
interval [15000, 30000)?

The walkthrough makes the computation traceable in two complementary views:

  LEFT  — the actual Sales histogram (true data from pizzerie.Rdata), with the
          bins overlapping [15000, 30000) shaded in accent yellow. The shaded
          region IS the count we want; the count is summed on top of the panel.
  RIGHT — a step-by-step computation panel showing the logical-vector method
          (mean of TRUE/FALSE) plus the cumulative-frequency interpolation
          fallback when only the grouped table is available. Verbatim R code
          (`mean(Sales>=15000 & Sales<30000)`, `quantile`) is shown.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex1/ex1_1_1e_ai.png"

# -----------------------------------------------------------------------------
# Load real Sales data from pizzerie.Rdata
# -----------------------------------------------------------------------------
import pyreadr
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata"
pizz = pyreadr.read_r(RDATA)["pizzerie"]
sales = pizz["Sales"].to_numpy()
n = len(sales)

LO, HI = 15000, 30000
in_band = (sales >= LO) & (sales < HI)
k_in = int(in_band.sum())
prop = k_in / n
pct = prop * 100

# Cumulative-table values (matches the answer-PNG breaks 0,15000,30000,90000)
edges_custom = np.array([0, 15000, 30000, 90000], dtype=float)
counts_c, _ = np.histogram(sales, bins=edges_custom)
cum_c = np.cumsum(counts_c)
cum_pct = cum_c / n * 100  # 21, 84, 100

# -----------------------------------------------------------------------------
# Figure
# -----------------------------------------------------------------------------
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 6),
                               gridspec_kw={"width_ratios": [1.15, 1.0]})

# =============================================================================
# LEFT — Sales histogram with [15000,30000) shaded
# =============================================================================
# Use a fine-grained binning (width=2500) so the shaded region maps cleanly
bin_w = 2500
bins = np.arange(0, sales.max() + bin_w, bin_w)
counts, edges, patches = ax1.hist(
    sales, bins=bins, color=PALETTE["secondary"],
    edgecolor=PALETTE["primary"], linewidth=0.9, alpha=0.85,
)
# Shade bars whose left edge is in [LO, HI)
for patch, left_edge in zip(patches, edges[:-1]):
    if LO <= left_edge < HI:
        patch.set_facecolor(PALETTE["accent"])
        patch.set_edgecolor(PALETTE["primary"])
        patch.set_alpha(0.95)

ymax = counts.max()

# Boundary lines
ax1.axvline(LO, color=PALETTE["primary"], lw=1.4, ls="--", alpha=0.8)
ax1.axvline(HI, color=PALETTE["primary"], lw=1.4, ls="--", alpha=0.8)
ax1.text(LO, ymax * 1.04, "15000", ha="center", va="bottom",
         fontsize=10.5, fontweight="bold", color=PALETTE["primary"])
ax1.text(HI, ymax * 1.04, "30000", ha="center", va="bottom",
         fontsize=10.5, fontweight="bold", color=PALETTE["primary"])

# Bracket-style annotation above the bars
mid = (LO + HI) / 2
ax1.annotate("", xy=(HI, ymax * 1.18), xytext=(LO, ymax * 1.18),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["accent"],
                             lw=2.0))
ax1.text(mid, ymax * 1.25,
         f"k = {k_in} pizzerias  ->  {k_in}/{n} = {prop:.2f}  =  {pct:.0f}%",
         ha="center", va="bottom", fontsize=11.5, fontweight="bold",
         color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.35", linewidth=1.1))

# Side labels
ax1.text(LO * 0.5, ymax * 0.55, "low\nturnover",
         ha="center", va="center", fontsize=9.5,
         color=PALETTE["muted"], style="italic")
ax1.text((HI + edges[-1]) / 2, ymax * 0.55, "high\nturnover",
         ha="center", va="center", fontsize=9.5,
         color=PALETTE["muted"], style="italic")

ax1.set_xlabel("Sales (EUR turnover)")
ax1.set_ylabel("Count")
ax1.set_title(f"Histogram of Sales — shaded region = medium turnover  ($n={n}$)")
ax1.set_ylim(0, ymax * 1.45)
ax1.grid(axis="y", alpha=0.5)

# =============================================================================
# RIGHT — computation panel
# =============================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)

ax2.text(0.5, 0.975,
         "Computing the percentage in [15000, 30000)",
         ha="center", va="top", fontsize=12.5, fontweight="bold",
         color=PALETTE["primary"])

# Step 1 — logical-vector method
ax2.text(0.02, 0.905, "1. Logical-vector method (raw data)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.855,
         "ind <- pizzerie$Sales >= 15000 &\n"
         "       pizzerie$Sales <  30000\n"
         "mean(ind)\n"
         "## [1] 0.59\n"
         "sum(ind)        ## [1] 59 of n=100",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["ok"],
         bbox=dict(facecolor="#eaf5ee", edgecolor=PALETTE["ok"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 2 — why mean of TRUE/FALSE = proportion
ax2.text(0.02, 0.625, "2. Why mean(TRUE/FALSE) = proportion",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.575,
         "TRUE -> 1, FALSE -> 0\n"
         "sum  = number of pizzerias in band (= k)\n"
         "mean = k / n  = sample proportion = p\n"
         "percentage = 100 * p",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 3 — cumulative-table fallback (grouped data)
ax2.text(0.02, 0.345,
         "3. Cumulative-table fallback (grouped data)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.295,
         "class           cum%      \n"
         "[0,    15000)   21        <- F(15000)\n"
         "[15000,30000)   84        <- F(30000)\n"
         "[30000,90000)  100\n"
         "p ~ F(30000) - F(15000) = 84 - 21 = 63%\n"
         "(approx: only the table is available)",
         fontsize=9.8, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 4 — final answer
ax2.text(0.02, 0.075,
         f"-> Answer (raw):  p = {prop:.2f}   ->   {pct:.0f}%   "
         "of pizzerias = medium turnover",
         fontsize=11, fontweight="bold", color=PALETTE["primary"],
         bbox=dict(facecolor=PALETTE["accent"], edgecolor=PALETTE["primary"],
                   boxstyle="round,pad=0.5", linewidth=1.2),
         alpha=0.95)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  n={n}, k={k_in}, p={prop:.4f}, pct={pct:.2f}%")
print(f"  grouped-table cum% = {cum_pct.tolist()}")
