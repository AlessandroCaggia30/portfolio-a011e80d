"""Build AI walkthrough plot for Ex 1.1g — % pizzerias with fewer than 5 Employees.

The exercise: what percentage of pizzerias has Employees < 5?

Two complementary views in one figure:

  LEFT  — a spike plot of the discrete variable Employees (true data from
          pizzerie.Rdata). Bars at values 1..4 are highlighted in accent
          yellow (= small/medium-sized), bars at >=5 are drawn in the
          secondary navy. Count labels sit on top of each bar; the
          highlighted region is summed and shown as k=45 of n=100 in a
          bracket above the bars.
  RIGHT — a computation panel with three blocks:
            1. logical-vector method:  mean(Employees < 5)  -> 0.45
            2. cumulative-frequency table (matches the answer PNG):
                 Cum.Prop at Employees=4 is 0.45 -> 45 %
                 Highlighted row shaded in soft yellow.
            3. final boxed answer.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex1/ex1_1_1g_ai.png"

# -----------------------------------------------------------------------------
# Load real Employees data from pizzerie.Rdata
# -----------------------------------------------------------------------------
import pyreadr
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata"
pizz = pyreadr.read_r(RDATA)["pizzerie"]
emp = pizz["Employees"].to_numpy().astype(int)
n = len(emp)

THRESH = 5
in_band = emp < THRESH
k_in = int(in_band.sum())
prop = k_in / n
pct = prop * 100

vals, counts = np.unique(emp, return_counts=True)
props = counts / n
cum_counts = np.cumsum(counts)
cum_props = cum_counts / n

# -----------------------------------------------------------------------------
# Figure
# -----------------------------------------------------------------------------
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 6),
                               gridspec_kw={"width_ratios": [1.15, 1.0]})

# =============================================================================
# LEFT — spike plot of Employees, bars Employees<5 highlighted
# =============================================================================
bar_colors = [PALETTE["accent"] if v < THRESH else PALETTE["secondary"]
              for v in vals]
bars = ax1.bar(vals, counts, color=bar_colors,
               edgecolor=PALETTE["primary"], linewidth=1.0, alpha=0.92,
               width=0.78)

ymax = counts.max()

# Count labels on top of bars
for v, c in zip(vals, counts):
    ax1.text(v, c + ymax * 0.025, str(int(c)),
             ha="center", va="bottom", fontsize=10,
             color=PALETTE["primary"], fontweight="bold")

# Threshold line at 4.5 (between Employees=4 and =5)
ax1.axvline(THRESH - 0.5, color=PALETTE["primary"], lw=1.4, ls="--", alpha=0.85)
ax1.text(THRESH - 0.5, ymax * 1.06, "Employees < 5", ha="center", va="bottom",
         fontsize=10.5, fontweight="bold", color=PALETTE["primary"])

# Bracket annotation above highlighted region
left_b = vals[vals < THRESH].min() - 0.4
right_b = vals[vals < THRESH].max() + 0.4
ax1.annotate("", xy=(right_b, ymax * 1.22), xytext=(left_b, ymax * 1.22),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["accent"],
                             lw=2.0))
mid = (left_b + right_b) / 2
ax1.text(mid, ymax * 1.30,
         f"k = {k_in} pizzerias  ->  {k_in}/{n} = {prop:.2f}  =  {pct:.0f}%",
         ha="center", va="bottom", fontsize=11.5, fontweight="bold",
         color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.35", linewidth=1.1))

# Side labels
ax1.text(2.5, ymax * 0.62, "small / medium\n(< 5)",
         ha="center", va="center", fontsize=9.5,
         color=PALETTE["muted"], style="italic")
ax1.text(9.5, ymax * 0.62, "larger\n(>= 5)",
         ha="center", va="center", fontsize=9.5,
         color=PALETTE["muted"], style="italic")

ax1.set_xlabel("Employees (count per pizzeria)")
ax1.set_ylabel("Count")
ax1.set_title(f"Distribution of Employees — bars <5 highlighted  ($n={n}$)")
ax1.set_xticks(vals)
ax1.set_ylim(0, ymax * 1.50)
ax1.grid(axis="y", alpha=0.5)

# =============================================================================
# RIGHT — computation panel with cumulative table + boxed answer
# =============================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)

ax2.text(0.5, 0.975,
         "Computing the percentage with Employees < 5",
         ha="center", va="top", fontsize=12.5, fontweight="bold",
         color=PALETTE["primary"])

# Step 1 — logical-vector method (green box, matches 1.1e style)
ax2.text(0.02, 0.905, "1. Logical-vector method (raw data)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.860,
         "ind <- pizzerie$Employees < 5\n"
         "mean(ind)        ## [1] 0.45\n"
         "sum(ind)         ## [1] 45 of n=100",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["ok"],
         bbox=dict(facecolor="#eaf5ee", edgecolor=PALETTE["ok"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 2 — cumulative table (matches the answer PNG)
ax2.text(0.02, 0.660,
         "2. Cumulative-frequency table",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])

# Render the table-ish block as monospace text; we shade the row Employees=4
# by drawing a rectangle behind it via ax2.add_patch
table_rows = [
    ("Employees", "Count", "Prop", "Cum.Count", "Cum.Prop"),
    ("    1    ", "  2  ", "0.02", "    2    ", "  0.02   "),
    ("    2    ", "  3  ", "0.03", "    5    ", "  0.05   "),
    ("    3    ", " 12  ", "0.12", "   17    ", "  0.17   "),
    ("    4    ", " 28  ", "0.28", "   45    ", "  0.45   "),  # shaded
    ("    5    ", " 23  ", "0.23", "   68    ", "  0.68   "),
    ("   ...   ", " ... ", "... ", "   ...   ", "  ...    "),
]

# Build the text block
table_text = "\n".join("  ".join(r) for r in table_rows)

# Shade the row for Employees=4 (5th row, index 4)
# We approximate the row strip by an axes rectangle
row_h = 0.030
top_y = 0.620
y_emp4 = top_y - (4 + 0.5) * row_h - 0.005
ax2.add_patch(Rectangle((0.025, y_emp4 - row_h * 0.55), 0.95, row_h,
                        transform=ax2.transAxes,
                        facecolor=PALETTE["accent"], alpha=0.30,
                        edgecolor=PALETTE["accent"], linewidth=0.6,
                        zorder=0))

ax2.text(0.04, top_y, table_text,
         fontsize=9.6, family="monospace", va="top",
         color=PALETTE["neutral"], zorder=2)

ax2.text(0.04, 0.388,
         "Cum.Prop at Employees = 4   ->   0.45   =   45 %",
         fontsize=10.5, fontweight="bold", color=PALETTE["primary"],
         va="top")

# Step 3 — why mean(TRUE/FALSE) = proportion (compact note)
ax2.text(0.02, 0.318, "3. Why mean(TRUE/FALSE) = proportion",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.272,
         "TRUE -> 1, FALSE -> 0\n"
         "sum  = number of pizzerias with Employees < 5  (= k)\n"
         "mean = k / n = sample proportion = p  ;  percentage = 100 * p",
         fontsize=9.8, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 4 — final answer
ax2.text(0.02, 0.075,
         f"-> Answer:  p = {prop:.2f}   ->   {pct:.0f}%   "
         "of pizzerias are small/medium",
         fontsize=11, fontweight="bold", color=PALETTE["primary"],
         bbox=dict(facecolor=PALETTE["accent"], edgecolor=PALETTE["primary"],
                   boxstyle="round,pad=0.5", linewidth=1.2),
         alpha=0.95)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  n={n}, k={k_in}, p={prop:.4f}, pct={pct:.2f}%")
print(f"  unique vals = {vals.tolist()}")
print(f"  counts      = {counts.tolist()}")
print(f"  cum.props   = {[round(x,2) for x in cum_props.tolist()]}")
