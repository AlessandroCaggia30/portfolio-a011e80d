"""Build AI walkthrough plot for Ex 1.1c — choosing a graph for `Sales` (pizzerie).

The exercise asks: which graph represents the distribution of `Sales` (turnover)?
Sales is continuous quantitative -> **histogram**.

The walkthrough visual makes that decision *traceable* by:
  LEFT  — the actual Sales histogram (true data from pizzerie.Rdata),
          with mean / median annotated and the right tail highlighted, so the
          student sees what a histogram of a continuous variable looks like.
  RIGHT — a decision panel: variable type -> appropriate plot. Bar plot is
          shown crossed out (wrong: bars for continuous variables imply
          discrete categories and lose the shape), histogram is highlighted as
          the correct choice, plus the verbatim R command that produces it.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex1/ex1_1_1c_ai.png"

# -----------------------------------------------------------------------------
# Load real Sales data from pizzerie.Rdata for an honest histogram
# -----------------------------------------------------------------------------
import pyreadr
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata"
pizz = pyreadr.read_r(RDATA)["pizzerie"]
sales = pizz["Sales"].to_numpy()
n = len(sales)
mean_ = float(sales.mean())
median_ = float(np.median(sales))

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 6),
                               gridspec_kw={"width_ratios": [1.1, 1.0]})

# =============================================================================
# LEFT — the chosen plot: histogram of Sales (default Sturges-like binning)
# =============================================================================
counts, edges, patches = ax1.hist(
    sales, bins=10, color=PALETTE["secondary"],
    edgecolor=PALETTE["primary"], linewidth=1.1, alpha=0.95,
)

# Highlight the right tail (outlier-rich) bars
upper_fence = np.percentile(sales, 75) + 1.5 * (np.percentile(sales, 75) - np.percentile(sales, 25))
for patch, left_edge in zip(patches, edges[:-1]):
    if left_edge >= upper_fence:
        patch.set_facecolor(PALETTE["warn"])
        patch.set_alpha(0.85)

# Annotate mean and median
ax1.axvline(median_, color=PALETTE["primary"], lw=2.0, ls="-")
ax1.axvline(mean_,   color=PALETTE["accent"],  lw=2.0, ls="--")
ymax = counts.max()
ax1.text(median_, ymax * 1.02, f"median = {median_:,.0f}",
         color=PALETTE["primary"], ha="right", va="bottom",
         fontsize=10.5, fontweight="bold", rotation=0)
ax1.text(mean_, ymax * 0.92, f"mean = {mean_:,.0f}",
         color=PALETTE["accent"], ha="left", va="bottom",
         fontsize=10.5, fontweight="bold")

# Right-tail callout
ax1.annotate("right tail\n(few high-turnover\npizzerias)",
             xy=(edges[-2] + (edges[-1]-edges[-2])*0.5, 1.5),
             xytext=(edges[-1]*0.78, ymax * 0.55),
             arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.1),
             ha="center", va="center", fontsize=10,
             color=PALETTE["warn"], fontweight="bold")

# Skew arrow above bars
ax1.annotate("", xy=(edges[-1]*0.95, ymax * 1.10),
             xytext=(edges[0]*1.05, ymax * 1.10),
             arrowprops=dict(arrowstyle="->", color=PALETTE["neutral"], lw=1.2))
ax1.text((edges[0] + edges[-1]) / 2, ymax * 1.16,
         "right-skewed: mean > median",
         ha="center", va="bottom", fontsize=10,
         color=PALETTE["neutral"], style="italic")

ax1.set_xlabel("Sales (EUR turnover)")
ax1.set_ylabel("Count")
ax1.set_title(f"Histogram of Sales — the right choice  ($n={n}$)")
ax1.set_ylim(0, ymax * 1.30)
ax1.grid(axis="y", alpha=0.5)

# =============================================================================
# RIGHT — decision panel: variable type -> plot choice + R command
# =============================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)

# Title bar
ax2.text(0.5, 0.965,
         "Choosing the plot from the variable type",
         ha="center", va="top", fontsize=12.5, fontweight="bold",
         color=PALETTE["primary"])

# Step 1 — classify the variable
ax2.text(0.02, 0.90, "1. Classify `Sales`",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.84,
         "Sales = turnover in EUR\n"
         "  -> quantitative (numeric)\n"
         "  -> continuous (any positive real)\n"
         "  -> distribution has shape, tail, skew",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 2 — decision table: type -> plot
ax2.text(0.02, 0.62, "2. Plot dictionary (type -> plot)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.555,
         "qualitative (nominal/ordinal) -> bar plot, pie\n"
         "discrete numeric (few values)  -> spike plot, bar\n"
         "continuous numeric             -> HISTOGRAM, density",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 3 — why not a bar plot
ax2.text(0.02, 0.40, "3. Why not a bar plot for continuous data?",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.345,
         "Bars need DISTINCT categories with gaps.\n"
         "Continuous Sales has no natural categories;\n"
         "histogram groups values into CLASSES and\n"
         "reveals SHAPE (skew, modes, tails).",
         fontsize=9.8, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 4 — the R command
ax2.text(0.02, 0.16, "4. R command (the histogram on the left)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.105,
         "distr.plot.x(Sales, plot.type=\"hist\",\n"
         "             data=pizzerie)\n"
         "# base R alternative:\n"
         "hist(pizzerie$Sales)",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["ok"],
         bbox=dict(facecolor="#eaf5ee", edgecolor=PALETTE["ok"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  n={n}, mean={mean_:.2f}, median={median_:.1f}")
print(f"  upper fence={upper_fence:.2f}, max={sales.max():.0f}")
