"""Build AI walkthrough plot for Ex 2.1b — 5-number summary + boxplot of Sales (pizzerie).

Visual: annotated boxplot of `Sales` showing the five-number summary
(min, Q1, median, Q3, max-regular) with the box, whiskers and the three
outliers labelled. A side panel reconstructs the upper-whisker / outlier-
threshold logic (Q3 + 1.5*IQR) and the IQR bracket, so the geometry of the
boxplot is tied directly to the numerical values.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_1b_ai.png"

# Five-number summary (from the snippet's R output)
n         = 100
xmin      = 8428.0
q1        = 17683.25
median    = 22349.5
q3        = 28975.0
xmax      = 63683.0
iqr       = q3 - q1                       # 11291.75
upper_fence = q3 + 1.5 * iqr              # 45912.625
max_regular = 42987.0                     # largest Sales below the upper fence
outliers  = [54418.0, 58762.0, 63683.0]

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 6),
                               gridspec_kw={"width_ratios": [1.05, 1.0]})

# =========================================================================
# LEFT — annotated boxplot of Sales
# =========================================================================
bp = ax1.boxplot(
    [[xmin, q1, q1, median, q3, q3, max_regular] + outliers],
    vert=True, widths=0.45, patch_artist=True,
    boxprops=dict(facecolor=PALETTE["secondary"], edgecolor=PALETTE["primary"], linewidth=1.4),
    medianprops=dict(color=PALETTE["primary"], linewidth=2.2),
    whiskerprops=dict(color=PALETTE["primary"], linewidth=1.2),
    capprops=dict(color=PALETTE["primary"], linewidth=1.2),
    flierprops=dict(marker="o", markerfacecolor=PALETTE["warn"],
                    markeredgecolor=PALETTE["warn"], markersize=7, alpha=0.9),
)

# Annotate the five-number summary on the right side of the box
x_box_right = 1.30
ann_kwargs = dict(ha="left", va="center", fontsize=10.5,
                  color=PALETTE["neutral"])
ax1.annotate(f"min = {xmin:,.0f}",
             xy=(1.0, xmin), xytext=(x_box_right, xmin),
             arrowprops=dict(arrowstyle="-", color=PALETTE["muted"], lw=0.8),
             **ann_kwargs)
ax1.annotate(f"$Q_1$ = {q1:,.2f}",
             xy=(1.225, q1), xytext=(x_box_right, q1),
             arrowprops=dict(arrowstyle="-", color=PALETTE["muted"], lw=0.8),
             **ann_kwargs)
ax1.annotate(f"median = {median:,.1f}",
             xy=(1.225, median), xytext=(x_box_right, median),
             arrowprops=dict(arrowstyle="-", color=PALETTE["primary"], lw=0.9),
             ha="left", va="center", fontsize=10.5,
             color=PALETTE["primary"], fontweight="bold")
ax1.annotate(f"$Q_3$ = {q3:,.0f}",
             xy=(1.225, q3), xytext=(x_box_right, q3),
             arrowprops=dict(arrowstyle="-", color=PALETTE["muted"], lw=0.8),
             **ann_kwargs)
ax1.annotate(f"max regular = {max_regular:,.0f}\n(top of whisker)",
             xy=(1.0, max_regular), xytext=(x_box_right, max_regular),
             arrowprops=dict(arrowstyle="-", color=PALETTE["muted"], lw=0.8),
             **ann_kwargs)

# Outlier callout
ax1.annotate(f"3 outliers:\n{', '.join(f'{int(v):,}' for v in outliers)}",
             xy=(1.0, outliers[2]),
             xytext=(x_box_right, outliers[2]),
             arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.0),
             ha="left", va="center", fontsize=10.5,
             color=PALETTE["warn"], fontweight="bold")

# IQR bracket on the left of the box
ax1.annotate("",
             xy=(0.55, q1), xytext=(0.55, q3),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["accent"], lw=1.6))
ax1.text(0.50, (q1 + q3) / 2,
         f"IQR\n= {iqr:,.2f}",
         color=PALETTE["accent"], ha="right", va="center",
         fontsize=11, fontweight="bold")

# Upper fence horizontal reference
ax1.axhline(upper_fence, color=PALETTE["warn"], lw=1.0, ls="--", alpha=0.7)
ax1.text(0.55, upper_fence,
         f"upper fence = $Q_3+1.5\\cdot IQR$ = {upper_fence:,.2f}",
         color=PALETTE["warn"], ha="left", va="bottom", fontsize=9.5,
         fontweight="bold")

ax1.set_xticks([1])
ax1.set_xticklabels(["Sales (pizzerie)"])
ax1.set_ylabel("Sales (EUR)")
ax1.set_title("Five-number summary as a boxplot ($n=100$)")
ax1.set_xlim(0.3, 2.05)
ax1.set_ylim(0, xmax * 1.08)
ax1.grid(axis="y", alpha=0.5)

# =========================================================================
# RIGHT — the construction logic: numbers + R commands
# =========================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)

# Title bar
ax2.text(0.5, 0.965,
         "How the boxplot is built from the 5-number summary",
         ha="center", va="top", fontsize=12.5, fontweight="bold",
         color=PALETTE["primary"])

# Step 1 — five-number summary
ax2.text(0.02, 0.90, "1. Five-number summary",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.84,
         "distr.summary.x(x=Sales, stats=\"fivenumbers\", data=pizzerie)\n"
         "   min      Q1     median     Q3      max\n"
         f"  {xmin:,.0f}   {q1:,.2f}   {median:,.1f}   {q3:,.0f}   {xmax:,.0f}",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 2 — IQR + outlier threshold
ax2.text(0.02, 0.62, "2. IQR and outlier threshold (upper)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.555,
         f"IQR = Q3 - Q1 = {q3:,.0f} - {q1:,.2f} = {iqr:,.2f}\n"
         f"upper fence = Q3 + 1.5 * IQR\n"
         f"            = {q3:,.0f} + 1.5 * {iqr:,.2f}\n"
         f"            = {upper_fence:,.2f}",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 3 — max regular value (upper whisker top)
ax2.text(0.02, 0.34, "3. Upper-whisker top = max regular value",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.275,
         "max(pizzerie$Sales[pizzerie$Sales < 45912.62])\n"
         f"## [1] {max_regular:,.0f}\n"
         f"  -> upper whisker stops at {max_regular:,.0f}\n"
         f"  -> outliers = Sales > {upper_fence:,.2f}",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 4 — list outliers
ax2.text(0.02, 0.085, "4. Outliers (extreme right-tail Sales)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.035,
         "pizzerie$Sales[pizzerie$Sales > 45912.62]\n"
         f"## [1] {int(outliers[0]):,}  {int(outliers[2]):,}  {int(outliers[1]):,}",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["warn"],
         bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  n={n}, min={xmin}, Q1={q1}, median={median}, Q3={q3}, max={xmax}")
print(f"  IQR={iqr}, upper fence={upper_fence}, max regular={max_regular}")
print(f"  outliers={outliers}")
