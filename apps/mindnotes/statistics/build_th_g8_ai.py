"""Build AI theory diagram for G8 — Conditional summary measures.

Two-panel figure:
LEFT  — Side-by-side boxplots of a numerical variable across three
        qualitative groups, with the overall median drawn as a dashed
        reference line and conditional medians highlighted.
RIGHT — Conditional summary card: per-group n, mean, median, SD, CV;
        overall row at the bottom for comparison; arrow callouts to
        highlight location-shift and dispersion-shift readings.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, Rectangle

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/theory/th_g8_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# ----- Synthetic data: salary by department -----
rng = np.random.default_rng(7)
groups = ["A", "B", "C"]
# A: tight, low; B: medium spread, medium mean; C: high mean & spread (right-skew + outliers)
data = {
    "A": rng.normal(28, 3, 40),
    "B": rng.normal(35, 5, 45),
    "C": np.concatenate([rng.normal(44, 6, 38), [62, 65, 70]]),
}
all_vals = np.concatenate(list(data.values()))
overall_median = float(np.median(all_vals))
overall_mean   = float(np.mean(all_vals))

def stats_row(x):
    return dict(n=len(x),
                mean=np.mean(x),
                med=np.median(x),
                sd=np.std(x, ddof=1),
                cv=np.std(x, ddof=1) / abs(np.mean(x)))

stats = {g: stats_row(data[g]) for g in groups}
overall = stats_row(all_vals)

# ----- Figure -----
fig = plt.figure(figsize=(13.2, 6.4))
gs = fig.add_gridspec(1, 2, width_ratios=[1.15, 1.0], wspace=0.22,
                      left=0.07, right=0.985, top=0.90, bottom=0.10)

# ============================================================
# LEFT — Side-by-side boxplots with overall reference lines
# ============================================================
axL = fig.add_subplot(gs[0, 0])

box_colors = [PALETTE["secondary"], PALETTE["warn"], PALETTE["ok"]]
positions  = [1, 2, 3]
parts = axL.boxplot([data[g] for g in groups], positions=positions,
                    widths=0.55, patch_artist=True,
                    medianprops=dict(color=PALETTE["primary"], lw=2.2),
                    whiskerprops=dict(color=PALETTE["neutral"], lw=1.3),
                    capprops=dict(color=PALETTE["neutral"], lw=1.3),
                    flierprops=dict(marker="o", mfc=PALETTE["warn"],
                                    mec=PALETTE["neutral"], ms=5, alpha=0.85))
for patch, col in zip(parts["boxes"], box_colors):
    patch.set_facecolor(col)
    patch.set_alpha(0.35)
    patch.set_edgecolor(col)
    patch.set_linewidth(1.6)

# Conditional means as diamonds
for pos, g, col in zip(positions, groups, box_colors):
    axL.plot(pos, stats[g]["mean"], marker="D", color=col, ms=9,
             markeredgecolor="white", markeredgewidth=1.4, zorder=5)

# Overall median reference (dashed) + overall mean (dotted)
axL.axhline(overall_median, ls="--", color=PALETTE["primary"], lw=1.6,
            alpha=0.85,
            label=f"Overall median = {overall_median:.1f}")
axL.axhline(overall_mean, ls=":", color=PALETTE["neutral"], lw=1.6,
            alpha=0.85,
            label=f"Overall mean  = {overall_mean:.1f}")

# Annotate group medians near the boxes
for pos, g, col in zip(positions, groups, box_colors):
    m = stats[g]["med"]
    axL.annotate(f"med={m:.1f}", xy=(pos + 0.30, m),
                 xytext=(8, 0), textcoords="offset points",
                 fontsize=9.5, color=col, fontweight="bold",
                 va="center")

axL.set_xticks(positions)
axL.set_xticklabels([f"Group {g}\n(n={stats[g]['n']})" for g in groups],
                    fontsize=10.5)
axL.set_ylabel("Numerical variable  Y")
axL.set_title("Side-by-side boxplots — conditional distributions",
              color=PALETTE["primary"])
axL.legend(loc="upper left", frameon=True, fancybox=True, framealpha=0.95,
           fontsize=9.5)

# Diamond legend handle (manual)
axL.plot([], [], "D", color=PALETTE["neutral"], ms=8,
         markeredgecolor="white", markeredgewidth=1.2,
         label="Conditional mean")
axL.legend(loc="upper left", frameon=True, fancybox=True, framealpha=0.95,
           fontsize=9.5)

# ============================================================
# RIGHT — Conditional summary table card
# ============================================================
axR = fig.add_subplot(gs[0, 1])
axR.set_xlim(0, 10)
axR.set_ylim(0, 10)
axR.axis("off")

axR.set_title("Conditional summary table",
              color=PALETTE["primary"], pad=14)

# Header
header_y = 9.0
axR.add_patch(FancyBboxPatch((0.2, header_y - 0.55), 9.6, 0.85,
                             boxstyle="round,pad=0.02",
                             fc=PALETTE["primary"], ec="none", zorder=2))
col_x = [1.0, 2.6, 4.2, 5.6, 7.0, 8.4]
headers = ["group", "$n$", "mean", "median", "SD", "CV"]
for x, txt in zip(col_x, headers):
    axR.text(x, header_y - 0.13, txt, ha="center", va="center",
             color="white", fontsize=11, fontweight="bold", zorder=3)

row_h = 0.78
top = header_y - 0.95
rows = [(g, stats[g], box_colors[i]) for i, g in enumerate(groups)]
for i, (g, s, col) in enumerate(rows):
    y = top - i * row_h
    if i % 2 == 0:
        axR.add_patch(Rectangle((0.2, y - row_h/2 + 0.04),
                                9.6, row_h - 0.04,
                                fc="#f6f7fb", ec="none", zorder=1))
    # group badge
    axR.add_patch(FancyBboxPatch((0.55, y - 0.22), 0.9, 0.44,
                                 boxstyle="round,pad=0.02",
                                 fc=col, ec="none", alpha=0.85, zorder=2))
    axR.text(col_x[0], y, g, ha="center", va="center", color="white",
             fontsize=10.5, fontweight="bold", zorder=3)
    vals = [s["n"], s["mean"], s["med"], s["sd"], s["cv"]]
    fmts = ["{:d}", "{:.2f}", "{:.2f}", "{:.2f}", "{:.3f}"]
    for x, v, f in zip(col_x[1:], vals, fmts):
        axR.text(x, y, f.format(v), ha="center", va="center",
                 fontsize=10.5, color=PALETTE["neutral"], zorder=2)

# Overall row
y_ov = top - len(rows) * row_h - 0.05
axR.add_patch(FancyBboxPatch((0.2, y_ov - row_h/2 + 0.05),
                             9.6, row_h - 0.04,
                             boxstyle="round,pad=0.02",
                             fc="#fff8d8", ec=PALETTE["accent"], lw=1.0,
                             zorder=2))
axR.text(col_x[0], y_ov, "all", ha="center", va="center",
         fontsize=10.5, color=PALETTE["primary"], fontweight="bold", zorder=3)
vals = [overall["n"], overall["mean"], overall["med"],
        overall["sd"], overall["cv"]]
fmts = ["{:d}", "{:.2f}", "{:.2f}", "{:.2f}", "{:.3f}"]
for x, v, f in zip(col_x[1:], vals, fmts):
    axR.text(x, y_ov, f.format(v), ha="center", va="center",
             fontsize=10.5, color=PALETTE["primary"], fontweight="bold",
             zorder=3)

# Reading-the-comparison footer
foot_y = y_ov - 1.05
axR.add_patch(FancyBboxPatch((0.2, foot_y - 1.45), 9.6, 1.45,
                             boxstyle="round,pad=0.02",
                             fc="#eef3ff", ec=PALETTE["primary"], lw=1.0,
                             zorder=2))
axR.text(0.5, foot_y - 0.28,
         r"$\Delta$ medians $\Rightarrow$ location shift   "
         r"$|$   $\Delta$ IQR/SD $\Rightarrow$ spread shift",
         ha="left", va="center", fontsize=10.5,
         color=PALETTE["neutral"], zorder=3)
axR.text(0.5, foot_y - 0.70,
         r"$\Delta$ CV $\Rightarrow$ relative-spread shift   "
         r"$|$   $\Delta$ shape $\Rightarrow$ skew/outlier shift",
         ha="left", va="center", fontsize=10.5,
         color=PALETTE["neutral"], zorder=3)
axR.text(0.5, foot_y - 1.18,
         "All boxes near-identical  $\\Rightarrow$  groups independent of Y.",
         ha="left", va="center", fontsize=10.5,
         color=PALETTE["primary"], fontweight="bold", zorder=3)

fig.suptitle("Conditional summary measures — boxplots + per-group statistics",
             fontsize=14, fontweight="bold", color=PALETTE["primary"],
             y=0.985)

fig.savefig(OUT, dpi=150)
print(f"Saved {OUT}")
