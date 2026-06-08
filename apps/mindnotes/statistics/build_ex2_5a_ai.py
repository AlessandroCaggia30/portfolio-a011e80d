"""Build AI walkthrough plot for Ex 2.5a — boxplot of Age in customer_habits
(n = 34 866) and interpretation of its shape (skewness, outliers, central box).

Also uses shutil to clone the recurring Exercise 2.5 textbook prompt image
(already saved as ex2_5c_question.png — the Ex 2.5 page is shared across
sub-parts 5a, 5b, 5c, 5d, 5f, 5g) into the ex2/questions/ folder under the
Ex 2.5a filename, so the snippet can reference it directly.

No specific textbook answer crop exists for Ex 2.5a in this repo, so the
snippet does NOT reference an answer PNG (we only reference what we copy).
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
OUT  = f"{BASE}/ex2_5a_ai.png"

# ---------- 1. Clone the recurring Exercise 2.5 textbook prompt PNG ----------
# The Ex 2.5 prompt page is shared across 5a/5c/5d/5f (identical MD5 hash).
src_q = f"{BASE}/questions/ex2_5c_question.png"
dst_q = f"{BASE}/questions/ex2_5a_question.png"

if os.path.exists(src_q):
    shutil.copyfile(src_q, dst_q)
    print(f"copied -> {dst_q}")
else:
    print(f"WARNING: source question PNG not found at {src_q}")

# ---------- 2. Build the AI walkthrough figure ----------
# Five-number summary from customer_habits$Age (n = 34 866), reused
# from build_ex2_plots.py / Ex 2.5c cross-check:
n      = 34866
Mn     = 16
Q1     = 40
Med    = 46
Q3     = 54
Mx     = 96
IQR    = Q3 - Q1                                # 14
upper_w = Q3 + 1.5 * IQR                        # 75   -> Tukey upper whisker cap
lower_w = Q1 - 1.5 * IQR                        # 19   -> Tukey lower whisker cap
# In the actual data, whiskers extend to the most extreme value still
# inside [lower_w, upper_w]; everything outside is shown as an outlier.
upper_whisker = 75
lower_whisker = 20
# Approximate outlier marks (just for illustration of the right-tail mass)
np.random.seed(0)
out_hi = np.linspace(76, 96, 9)
out_lo = np.array([16, 17, 18])

# Approximate proportions outside each whisker (from Ex 2.5b: p5=32, p95=66)
p_above_75 = 0.038   # ~3.8% of sample above the upper whisker (right tail heavy)
p_below_20 = 0.003   # ~0.3% of sample below the lower whisker

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.4))

# ---------- LEFT: annotated vertical boxplot showing the five-number summary ----------
box_x = 0.0
box_w = 0.6
# IQR box
ax1.add_patch(Rectangle((box_x - box_w/2, Q1), box_w, Q3 - Q1,
                        facecolor=PALETTE["primary"], alpha=0.28,
                        edgecolor=PALETTE["primary"], linewidth=1.6))
# Median line
ax1.plot([box_x - box_w/2, box_x + box_w/2], [Med, Med],
         color=PALETTE["warn"], lw=2.4)
# Whiskers
ax1.plot([box_x, box_x], [Q3, upper_whisker], color=PALETTE["primary"], lw=1.4)
ax1.plot([box_x, box_x], [Q1, lower_whisker], color=PALETTE["primary"], lw=1.4)
ax1.plot([box_x - box_w/4, box_x + box_w/4], [upper_whisker]*2,
         color=PALETTE["primary"], lw=1.4)
ax1.plot([box_x - box_w/4, box_x + box_w/4], [lower_whisker]*2,
         color=PALETTE["primary"], lw=1.4)
# Outliers
ax1.scatter([box_x]*len(out_hi), out_hi, s=18, color=PALETTE["accent"],
            edgecolor="#2a3142", linewidth=0.5, zorder=5)
ax1.scatter([box_x]*len(out_lo), out_lo, s=18, color=PALETTE["accent"],
            edgecolor="#2a3142", linewidth=0.5, zorder=5)

# Annotate the five-number summary on the right side of the box
labels = [
    (Mn,            f"min $\\approx {Mn}$",           PALETTE["neutral"]),
    (Q1,            f"$Q_1 = {Q1}$",                  PALETTE["primary"]),
    (Med,           f"Me $= {Med}$",                  PALETTE["warn"]),
    (Q3,            f"$Q_3 = {Q3}$",                  PALETTE["primary"]),
    (upper_whisker, f"upper whisker $\\approx {upper_whisker}$", PALETTE["muted"]),
    (Mx,            f"max $\\approx {Mx}$",           PALETTE["neutral"]),
]
for y, text, col in labels:
    ax1.annotate(text, xy=(box_x + box_w/2 + 0.05, y),
                 xytext=(box_x + box_w/2 + 0.35, y),
                 ha="left", va="center", fontsize=10.5, color=col,
                 fontweight="bold",
                 arrowprops=dict(arrowstyle="-", color=col, lw=0.9, alpha=0.7))

# IQR bracket on the left of the box
ax1.annotate("", xy=(box_x - box_w/2 - 0.18, Q3),
             xytext=(box_x - box_w/2 - 0.18, Q1),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["accent"], lw=1.6))
ax1.text(box_x - box_w/2 - 0.25, (Q1 + Q3)/2,
         f"IQR $= Q_3 - Q_1 = {IQR}$",
         color=PALETTE["accent"], fontsize=10.5,
         ha="right", va="center", rotation=90, fontweight="bold")

ax1.set_xlim(-1.3, 1.4)
ax1.set_ylim(10, 100)
ax1.set_xticks([])
ax1.set_ylabel("Age (years)")
ax1.set_title("Boxplot of customer_habits$Age  (n = 34 866)")

# ---------- RIGHT: shape diagnostics — symmetry of box vs. tails ----------
# Four bars: distance from median to Q1, Q3 (box halves) and to whiskers (tails).
labels_r = [
    r"Me $-Q_1$",       # lower half of box
    r"$Q_3 -$ Me",      # upper half of box
    r"$Q_1 - $low whisk", # lower whisker length
    r"upper whisk $-Q_3$", # upper whisker length
]
values_r = [Med - Q1, Q3 - Med, Q1 - lower_whisker, upper_whisker - Q3]
colors   = [PALETTE["primary"], PALETTE["primary"],
            PALETTE["muted"],   PALETTE["accent"]]

bars = ax2.bar(labels_r, values_r, color=colors,
               edgecolor="#2a3142", linewidth=0.9, alpha=0.85)
for b, v in zip(bars, values_r):
    ax2.text(b.get_x() + b.get_width()/2, b.get_height() + 0.4,
             f"{v}", ha="center", va="bottom",
             fontsize=11, fontweight="bold", color="#2a3142")

ax2.set_ylim(0, max(values_r) * 1.35)
ax2.set_ylabel("Years")
ax2.set_title("Shape diagnostics — right-skewed (upper tail dominates)")
ax2.tick_params(axis="x", labelsize=9.5)

# Verdict box
verdict = (
    r"$\bullet$ Box halves: $Me - Q_1 = 6$ vs $Q_3 - Me = 8$   "
    r"$\to$ box almost symmetric." + "\n"
    r"$\bullet$ Whiskers: lower $= 20$ vs upper $= 21$   "
    r"$\to$ similar reach, but" + "\n"
    r"   the upper tail carries far more mass (max $= 96$)." + "\n"
    r"$\bullet$ Outliers on both sides, heavier on the right" + "\n"
    r"   $\Rightarrow$ distribution is mildly skewed to the right."
)
ax2.text(0.98, 0.97, verdict, transform=ax2.transAxes,
         ha="right", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  five-number summary: min={Mn}, Q1={Q1}, Me={Med}, Q3={Q3}, max={Mx}")
print(f"  IQR = {IQR}; whiskers = [{lower_whisker}, {upper_whisker}]")
print(f"  box halves: Me-Q1={Med-Q1}, Q3-Me={Q3-Med}")
