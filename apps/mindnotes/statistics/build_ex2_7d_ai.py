"""Build AI walkthrough plot for Ex 2.7d — Range and IQR of Nr_visits.

Visual: annotated boxplot of `Nr_visits` showing the five-number summary
(min, Q1, median, Q3, max) with two brackets overlaid — the IQR span (Q3-Q1)
and the full Range span (max-min). The IQR/R ratio is highlighted to show
that the central 50% spans about half of the full variation, i.e. data are
heterogeneous even in the centre. A side panel reconstructs the numerical
derivation step by step from the 5-number summary.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_7d_ai.png"

# Five-number summary (from 2.7c)
xmin      = 1.0
q1        = 3.0
median    = 10.0
q3        = 14.0
xmax      = 24.0
rng       = xmax - xmin            # 23
iqr       = q3   - q1              # 11
ratio     = iqr / rng              # 0.478... -> 0.48

# Synthetic sample only for matplotlib's boxplot machinery (we set whiskers
# manually to span [xmin, xmax] -> no outliers in this exercise).
sample = [xmin, q1, q1, median, q3, q3, xmax]

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 6),
                               gridspec_kw={"width_ratios": [1.05, 1.0]})

# =========================================================================
# LEFT — annotated boxplot of Nr_visits with R and IQR brackets
# =========================================================================
ax1.boxplot(
    [sample],
    vert=True, widths=0.45, patch_artist=True,
    boxprops=dict(facecolor=PALETTE["secondary"], edgecolor=PALETTE["primary"], linewidth=1.4),
    medianprops=dict(color=PALETTE["primary"], linewidth=2.2),
    whiskerprops=dict(color=PALETTE["primary"], linewidth=1.2),
    capprops=dict(color=PALETTE["primary"], linewidth=1.2),
    showfliers=False,
)

# Annotate the five-number summary on the right side of the box
x_box_right = 1.32
ann_kwargs = dict(ha="left", va="center", fontsize=10.5,
                  color=PALETTE["neutral"])
ax1.annotate(f"min = {xmin:.0f}",
             xy=(1.0, xmin), xytext=(x_box_right, xmin),
             arrowprops=dict(arrowstyle="-", color=PALETTE["muted"], lw=0.8),
             **ann_kwargs)
ax1.annotate(f"$Q_1$ = {q1:.0f}",
             xy=(1.225, q1), xytext=(x_box_right, q1),
             arrowprops=dict(arrowstyle="-", color=PALETTE["muted"], lw=0.8),
             **ann_kwargs)
ax1.annotate(f"Me = {median:.0f}",
             xy=(1.225, median), xytext=(x_box_right, median),
             arrowprops=dict(arrowstyle="-", color=PALETTE["primary"], lw=0.9),
             ha="left", va="center", fontsize=10.5,
             color=PALETTE["primary"], fontweight="bold")
ax1.annotate(f"$Q_3$ = {q3:.0f}",
             xy=(1.225, q3), xytext=(x_box_right, q3),
             arrowprops=dict(arrowstyle="-", color=PALETTE["muted"], lw=0.8),
             **ann_kwargs)
ax1.annotate(f"max = {xmax:.0f}",
             xy=(1.0, xmax), xytext=(x_box_right, xmax),
             arrowprops=dict(arrowstyle="-", color=PALETTE["muted"], lw=0.8),
             **ann_kwargs)

# IQR bracket on the LEFT of the box
x_iqr = 0.55
ax1.annotate("",
             xy=(x_iqr, q1), xytext=(x_iqr, q3),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["accent"], lw=1.8))
ax1.text(x_iqr - 0.05, (q1 + q3) / 2,
         f"IQR\n= $Q_3 - Q_1$\n= {iqr:.0f}",
         color=PALETTE["accent"], ha="right", va="center",
         fontsize=11, fontweight="bold")

# RANGE bracket further to the LEFT
x_rng = 0.22
ax1.annotate("",
             xy=(x_rng, xmin), xytext=(x_rng, xmax),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["warn"], lw=1.8))
ax1.text(x_rng - 0.05, (xmin + xmax) / 2,
         f"Range\n= max $-$ min\n= {rng:.0f}",
         color=PALETTE["warn"], ha="right", va="center",
         fontsize=11, fontweight="bold")

# Ratio badge in the upper-right corner
ax1.text(0.98, 0.97,
         f"IQR / R = {iqr:.0f} / {rng:.0f} = {ratio:.2f}\n"
         "central 50% spans\n~ half the full range\n-> heterogeneous core",
         transform=ax1.transAxes, ha="right", va="top",
         fontsize=10.5, fontweight="bold", color=PALETTE["warn"],
         bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.5", linewidth=1.1))

ax1.set_xticks([1])
ax1.set_xticklabels(["Nr_visits (customer_habits)"])
ax1.set_ylabel("Nr_visits per year")
ax1.set_title("Range vs IQR on the boxplot ($n = 2200$)")
ax1.set_xlim(-0.15, 2.05)
ax1.set_ylim(-1, xmax * 1.18)
ax1.grid(axis="y", alpha=0.5)

# =========================================================================
# RIGHT — step-by-step derivation from the 5-number summary
# =========================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)

ax2.text(0.5, 0.965,
         "How Range and IQR are read off the 5-number summary",
         ha="center", va="top", fontsize=12.5, fontweight="bold",
         color=PALETTE["primary"])

# Step 1 — 5-number summary
ax2.text(0.02, 0.90, "1. Five-number summary (from 2.7c)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.84,
         " min   Q1   Me   Q3   max\n"
         f"  {xmin:.0f}    {q1:.0f}   {median:.0f}   {q3:.0f}    {xmax:.0f}",
         fontsize=10.5, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 2 — Range
ax2.text(0.02, 0.66, "2. Range R (full spread of the data)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.605,
         f"R = max - min = {xmax:.0f} - {xmin:.0f} = {rng:.0f}\n"
         "  -> some clients visit once a year,\n"
         "     others almost twice a month",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 3 — IQR
ax2.text(0.02, 0.43, "3. Interquartile range IQR (central 50%)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.375,
         f"IQR = Q3 - Q1 = {q3:.0f} - {q1:.0f} = {iqr:.0f}\n"
         "  -> within the central half: 3 visits/year\n"
         "     vs 14 visits/year (still very wide)",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 4 — IQR / R verdict
ax2.text(0.02, 0.19, "4. Relative spread of the centre",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.135,
         f"IQR / R = {iqr:.0f} / {rng:.0f} = {ratio:.2f}\n"
         "  -> the central 50% covers ~ half\n"
         "     of the entire range\n"
         "  -> not only the tails: even the centre\n"
         "     of the distribution is heterogeneous",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["warn"],
         bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  min={xmin}, Q1={q1}, Me={median}, Q3={q3}, max={xmax}")
print(f"  R={rng}, IQR={iqr}, IQR/R={ratio:.4f}")
