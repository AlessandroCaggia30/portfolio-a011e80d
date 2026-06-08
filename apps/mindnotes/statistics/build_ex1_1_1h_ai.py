"""Build AI walkthrough plot for Ex 1.1h — Mean vs median of Sales.

The exercise: compute mean and median of `Sales`, explain the difference, and
say which is more representative of typical turnover.

The walkthrough makes the asymmetry visible:

  LEFT  — Sales histogram (true data from pizzerie.Rdata) with the MEDIAN as a
          navy solid line and the MEAN as an accent-yellow dashed line. A
          curved arrow between them is labelled "mean pulled by upper tail",
          and the long right tail is highlighted to show *why*.
  RIGHT — a step-by-step computation panel showing the verbatim R commands
          (`mean(Sales)`, `median(Sales)`, plus distr.summary.x), the numeric
          comparison, and the rule "right skew => mean > median => median is
          the more robust centre".
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex1/ex1_1_1h_ai.png"

# -----------------------------------------------------------------------------
# Load real Sales data from pizzerie.Rdata
# -----------------------------------------------------------------------------
import pyreadr
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata"
pizz = pyreadr.read_r(RDATA)["pizzerie"]
sales = pizz["Sales"].to_numpy()
n = len(sales)

mean_emp = float(np.mean(sales))
median_emp = float(np.median(sales))

# For display: round to the R distr.summary.x convention
MEAN_R = 23946.99
MED_R = 22349.5

# -----------------------------------------------------------------------------
# Figure
# -----------------------------------------------------------------------------
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 6),
                               gridspec_kw={"width_ratios": [1.2, 1.0]})

# =============================================================================
# LEFT — Sales histogram with mean / median vertical lines + skew annotation
# =============================================================================
bin_w = 2500
bins = np.arange(0, sales.max() + bin_w, bin_w)
counts, edges, patches = ax1.hist(
    sales, bins=bins, color=PALETTE["secondary"],
    edgecolor=PALETTE["primary"], linewidth=0.9, alpha=0.85,
)
# Highlight the right tail (bars with left edge >= ~ 75th percentile)
tail_cut = np.percentile(sales, 75)
for patch, left_edge in zip(patches, edges[:-1]):
    if left_edge >= tail_cut:
        patch.set_facecolor("#f5d76e")  # softer yellow for the tail bars
        patch.set_edgecolor(PALETTE["primary"])
        patch.set_alpha(0.95)

ymax = counts.max()

# Median — navy solid
ax1.axvline(MED_R, color=PALETTE["primary"], lw=2.4, ls="-",
            label=f"median = {MED_R:,.1f}".replace(",", " "))
# Mean — accent yellow dashed
ax1.axvline(MEAN_R, color=PALETTE["accent"], lw=2.6, ls="--",
            label=f"mean = {MEAN_R:,.2f}".replace(",", " "))

# Curved arrow median -> mean, labelled "mean pulled by upper tail"
arrow_y = ymax * 1.12
arr = FancyArrowPatch(
    (MED_R, arrow_y), (MEAN_R, arrow_y),
    connectionstyle="arc3,rad=-0.35",
    arrowstyle="-|>", mutation_scale=18,
    color=PALETTE["accent"], lw=2.0,
)
ax1.add_patch(arr)
ax1.text((MED_R + MEAN_R) / 2, ymax * 1.30,
         "mean pulled by upper tail",
         ha="center", va="bottom", fontsize=11.0, fontweight="bold",
         color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.35", linewidth=1.1))

# Side label for the right tail
tail_mid = (tail_cut + edges[-1]) / 2
ax1.text(tail_mid, ymax * 0.55,
         "long right tail\n(few large pizzerias)",
         ha="center", va="center", fontsize=9.5, style="italic",
         color=PALETTE["muted"])

ax1.set_xlabel("Sales (EUR turnover)")
ax1.set_ylabel("Count")
ax1.set_title(f"Histogram of Sales — mean vs median  ($n={n}$)")
ax1.set_ylim(0, ymax * 1.50)
ax1.legend(loc="upper right", frameon=True, framealpha=0.95)
ax1.grid(axis="y", alpha=0.5)

# =============================================================================
# RIGHT — computation panel
# =============================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)

ax2.text(0.5, 0.975,
         "Mean vs median: computation & interpretation",
         ha="center", va="top", fontsize=12.5, fontweight="bold",
         color=PALETTE["primary"])

# Step 1 — verbatim R
ax2.text(0.02, 0.905, "1. Verbatim R",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.855,
         "mean(Sales)\n"
         "## [1] 23946.99\n"
         "median(Sales)\n"
         "## [1] 22349.5\n"
         "distr.summary.x(x=Sales,\n"
         "  stats=c(\"mean\",\"median\"),\n"
         "  data=pizzerie)",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["ok"],
         bbox=dict(facecolor="#eaf5ee", edgecolor=PALETTE["ok"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 2 — numeric comparison
ax2.text(0.02, 0.545, "2. Numeric comparison",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.495,
         "mean    = 23 946.99\n"
         "median  = 22 349.50\n"
         "diff    = +1 597.49   (mean > median)\n"
         "rel.    = +7.1% of the median",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 3 — rule of thumb
ax2.text(0.02, 0.265, "3. Why and what to report",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.215,
         "right skew  =>  mean > median\n"
         "mean is sensitive to upper-tail outliers\n"
         "median is robust  =>  better 'typical' value",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Final verdict
ax2.text(0.02, 0.045,
         "-> Typical turnover  ~  median = 22 349.5 EUR",
         fontsize=11, fontweight="bold", color=PALETTE["primary"],
         bbox=dict(facecolor=PALETTE["accent"], edgecolor=PALETTE["primary"],
                   boxstyle="round,pad=0.5", linewidth=1.2),
         alpha=0.95)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  n={n}, empirical mean={mean_emp:.4f}, empirical median={median_emp:.4f}")
print(f"  reported mean={MEAN_R}, reported median={MED_R}")
