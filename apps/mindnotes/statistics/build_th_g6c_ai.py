"""Build AI theory diagram for G6.C — Outliers and extreme values.

Two-panel figure:
LEFT  — Horizontal boxplot of a sample with mild upper outliers; the
        IQR box, whiskers, fences (Q1 - 1.5 IQR  and  Q3 + 1.5 IQR),
        and flagged outliers are explicitly annotated.
RIGHT — Strip / scatter plot of the same observations along the
        number line, with the fences drawn as vertical dashed lines.
        Points beyond the fences are colored as outliers; the IQR
        rule is summarized in a yellow card.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, Rectangle, FancyArrowPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/theory/th_g6c_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# ----- Sample with deliberate outliers -----
rng = np.random.default_rng(7)
core = rng.normal(loc=50, scale=8, size=40)
out_hi = np.array([88.0, 95.0])              # upper outliers
out_lo = np.array([18.0])                    # lower outlier
data = np.concatenate([core, out_hi, out_lo])

q1, med, q3 = np.percentile(data, [25, 50, 75])
iqr = q3 - q1
low_fence = q1 - 1.5 * iqr
up_fence  = q3 + 1.5 * iqr

# Whisker endpoints = most extreme non-outlier observations
non_out = data[(data >= low_fence) & (data <= up_fence)]
w_lo, w_hi = non_out.min(), non_out.max()

outliers = data[(data < low_fence) | (data > up_fence)]
inliers  = data[(data >= low_fence) & (data <= up_fence)]

# ----- Figure -----
fig = plt.figure(figsize=(13.2, 6.2))
gs = fig.add_gridspec(2, 1, height_ratios=[1.0, 1.1], hspace=0.55,
                      left=0.07, right=0.985, top=0.88, bottom=0.10)

x_lo, x_hi = 10, 105

# ============================================================
# TOP — Annotated horizontal boxplot
# ============================================================
axT = fig.add_subplot(gs[0, 0])

# Draw the box manually to control colors / annotations
box_y = 0.5
box_h = 0.42
# IQR box
axT.add_patch(Rectangle((q1, box_y - box_h/2), q3 - q1, box_h,
                        fc=PALETTE["primary"], ec=PALETTE["primary"],
                        alpha=0.22, lw=1.6, zorder=2))
# Median line
axT.plot([med, med], [box_y - box_h/2, box_y + box_h/2],
         color=PALETTE["primary"], lw=2.4, zorder=3)
# Whiskers
axT.plot([w_lo, q1], [box_y, box_y], color=PALETTE["primary"], lw=1.6, zorder=3)
axT.plot([q3, w_hi], [box_y, box_y], color=PALETTE["primary"], lw=1.6, zorder=3)
# Whisker caps
for x in (w_lo, w_hi):
    axT.plot([x, x], [box_y - 0.12, box_y + 0.12],
             color=PALETTE["primary"], lw=1.6, zorder=3)
# Outliers
axT.plot(outliers, np.full_like(outliers, box_y), "o",
         color=PALETTE["warn"], ms=8.5, mec="white", mew=1.2,
         zorder=5, label="Outliers")

# Fence lines
axT.axvline(low_fence, ls="--", color=PALETTE["secondary"], lw=1.6, zorder=1)
axT.axvline(up_fence,  ls="--", color=PALETTE["secondary"], lw=1.6, zorder=1)

# Labels for Q1, Med, Q3
for x, lab in [(q1, "$Q_1$"), (med, "Med"), (q3, "$Q_3$")]:
    axT.annotate(lab, xy=(x, box_y + box_h/2),
                 xytext=(0, 10), textcoords="offset points",
                 ha="center", fontsize=11, color=PALETTE["primary"],
                 fontweight="bold")

# Fence labels
axT.annotate(f"Lower fence\n$Q_1-1.5\\,IQR={low_fence:.1f}$",
             xy=(low_fence, box_y - box_h/2),
             xytext=(low_fence, box_y - 0.55), ha="center",
             fontsize=10, color=PALETTE["secondary"], fontweight="bold")
axT.annotate(f"Upper fence\n$Q_3+1.5\\,IQR={up_fence:.1f}$",
             xy=(up_fence, box_y - box_h/2),
             xytext=(up_fence, box_y - 0.55), ha="center",
             fontsize=10, color=PALETTE["secondary"], fontweight="bold")

# Arrow showing IQR span (placed well above the Q1/Med/Q3 labels)
iqr_y = box_y + box_h/2 + 0.45
axT.annotate("", xy=(q3, iqr_y), xytext=(q1, iqr_y),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["neutral"], lw=1.2))
axT.text((q1 + q3) / 2, iqr_y + 0.08,
         f"IQR = {iqr:.1f}", ha="center", va="bottom",
         fontsize=10.5, color=PALETTE["neutral"], fontweight="bold")

# Outlier callouts
for x in outliers:
    axT.annotate("outlier", xy=(x, box_y),
                 xytext=(x, box_y + 0.55), ha="center",
                 fontsize=9.5, color=PALETTE["warn"], fontweight="bold",
                 arrowprops=dict(arrowstyle="->", color=PALETTE["warn"],
                                 lw=1.0, connectionstyle="arc3,rad=0.15"))

axT.set_xlim(x_lo, x_hi)
axT.set_ylim(-0.25, 1.85)
axT.set_yticks([])
axT.set_xlabel("x")
axT.set_title("Boxplot with IQR box, whiskers and 1.5·IQR fences",
              color=PALETTE["primary"])
axT.legend(loc="upper right", frameon=True, fancybox=True, framealpha=0.95)

# ============================================================
# BOTTOM — Strip plot of raw observations with fences
# ============================================================
axB = fig.add_subplot(gs[1, 0])

# Jitter inliers / outliers a bit on y
rng2 = np.random.default_rng(11)
yi = 0.55 + rng2.normal(0, 0.06, size=len(inliers))
yo = 0.55 + rng2.normal(0, 0.06, size=len(outliers))

axB.plot(inliers, yi, "o", color=PALETTE["primary"], ms=7,
         alpha=0.7, mec="white", mew=0.8, label="Inliers")
axB.plot(outliers, yo, "o", color=PALETTE["warn"], ms=10,
         mec="white", mew=1.2, label="Outliers ($>Q_3+1.5\\,IQR$ or $<Q_1-1.5\\,IQR$)")

# Fences
axB.axvline(low_fence, ls="--", color=PALETTE["secondary"], lw=1.6)
axB.axvline(up_fence,  ls="--", color=PALETTE["secondary"], lw=1.6)

# Q1, Med, Q3 markers
for x, lab, col in [(q1, "$Q_1$", PALETTE["primary"]),
                    (med, "Med", PALETTE["primary"]),
                    (q3, "$Q_3$", PALETTE["primary"])]:
    axB.axvline(x, ls=":", color=col, lw=1.0, alpha=0.6)
    axB.text(x, 1.05, lab, ha="center", va="bottom",
             fontsize=10, color=col, fontweight="bold")

# Shaded "outlier zones"
axB.axvspan(x_lo, low_fence, color=PALETTE["warn"], alpha=0.08, zorder=0)
axB.axvspan(up_fence, x_hi,  color=PALETTE["warn"], alpha=0.08, zorder=0)

# Rule card
card_x, card_y, card_w, card_h = 11.5, 0.05, 32, 0.32
axB.add_patch(FancyBboxPatch((card_x, card_y), card_w, card_h,
                             boxstyle="round,pad=0.02",
                             fc="#fff8d8", ec=PALETTE["accent"], lw=1.0,
                             zorder=4))
axB.text(card_x + 0.6, card_y + card_h - 0.07,
         r"IQR rule:  flag $x$ as outlier if",
         ha="left", va="top", fontsize=10.5,
         color=PALETTE["neutral"], fontweight="bold", zorder=5)
axB.text(card_x + 0.6, card_y + card_h - 0.20,
         r"$x < Q_1 - 1.5\,IQR$  or  $x > Q_3 + 1.5\,IQR$.",
         ha="left", va="top", fontsize=10.5,
         color=PALETTE["neutral"], zorder=5)

axB.set_xlim(x_lo, x_hi)
axB.set_ylim(0, 1.25)
axB.set_yticks([])
axB.set_xlabel("x")
axB.set_title("Raw observations vs fences — points outside the dashed lines are outliers",
              color=PALETTE["primary"])
axB.legend(loc="upper right", frameon=True, fancybox=True, framealpha=0.95,
           fontsize=9.5)

fig.suptitle("Outliers — 1.5·IQR rule visualized on boxplot and number line",
             fontsize=14, fontweight="bold", color=PALETTE["primary"],
             y=0.975)

fig.savefig(OUT)
print(f"Saved {OUT}")
