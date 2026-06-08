"""Build AI walkthrough plot for Ex 2.7e — Boxplot of Nr_visits and its asymmetry.

Visual: annotated boxplot of `Nr_visits` showing the five-number summary
(1, 3, 10, 14, 24) with overlaid asymmetry brackets:
  - whisker asymmetry:  Q1 - min = 2   vs   max - Q3 = 10
  - box asymmetry:      Me - Q1  = 7   vs   Q3 - Me  = 4
A side panel reports the outlier fence check and the asymmetry verdict.

Also clones the shared Exercise 2.7 textbook prompt/answer images into
ex2/questions/ex2_7e_question.png and ex2/answers/ex2_7e_answer.png so the
snippet can reference them directly (same source as 2.7b/2.7d/2.7f).
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import matplotlib.pyplot as plt

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
OUT  = f"{BASE}/ex2_7e_ai.png"

# ---------- 1. Clone the shared Exercise 2.7 textbook PNGs via shutil ----------
src_q = f"{BASE}/questions/ex2_7d_question.png"
dst_q = f"{BASE}/questions/ex2_7e_question.png"
src_a = f"{BASE}/answers/ex2_7d_answer.png"
dst_a = f"{BASE}/answers/ex2_7e_answer.png"

if os.path.exists(src_q):
    shutil.copyfile(src_q, dst_q)
    print(f"copied -> {dst_q}")
if os.path.exists(src_a):
    shutil.copyfile(src_a, dst_a)
    print(f"copied -> {dst_a}")

# ---------- 2. Build the AI walkthrough figure ----------
# Five-number summary (from 2.7c)
xmin   = 1.0
q1     = 3.0
median = 10.0
q3     = 14.0
xmax   = 24.0
iqr    = q3 - q1                   # 11
fence_lo = q1 - 1.5 * iqr          # -13.5
fence_hi = q3 + 1.5 * iqr          # 30.5

# Whisker / box asymmetry components
lw_len = q1 - xmin                 # 2   lower whisker
uw_len = xmax - q3                 # 10  upper whisker
lb_len = median - q1               # 7   lower half of box
ub_len = q3 - median               # 4   upper half of box

# Synthetic sample only to drive matplotlib's boxplot (no outliers).
sample = [xmin, q1, q1, median, q3, q3, xmax]

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 6),
                               gridspec_kw={"width_ratios": [1.05, 1.0]})

# =========================================================================
# LEFT — annotated boxplot of Nr_visits with asymmetry brackets
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

# 5-number summary labels on the right of the box
x_lab = 1.32
ann_kwargs = dict(ha="left", va="center", fontsize=10.5, color=PALETTE["neutral"])
ax1.annotate(f"min = {xmin:.0f}",
             xy=(1.0, xmin), xytext=(x_lab, xmin),
             arrowprops=dict(arrowstyle="-", color=PALETTE["muted"], lw=0.8), **ann_kwargs)
ax1.annotate(f"$Q_1$ = {q1:.0f}",
             xy=(1.225, q1), xytext=(x_lab, q1),
             arrowprops=dict(arrowstyle="-", color=PALETTE["muted"], lw=0.8), **ann_kwargs)
ax1.annotate(f"Me = {median:.0f}",
             xy=(1.225, median), xytext=(x_lab, median),
             arrowprops=dict(arrowstyle="-", color=PALETTE["primary"], lw=0.9),
             ha="left", va="center", fontsize=10.5,
             color=PALETTE["primary"], fontweight="bold")
ax1.annotate(f"$Q_3$ = {q3:.0f}",
             xy=(1.225, q3), xytext=(x_lab, q3),
             arrowprops=dict(arrowstyle="-", color=PALETTE["muted"], lw=0.8), **ann_kwargs)
ax1.annotate(f"max = {xmax:.0f}",
             xy=(1.0, xmax), xytext=(x_lab, xmax),
             arrowprops=dict(arrowstyle="-", color=PALETTE["muted"], lw=0.8), **ann_kwargs)

# Whisker-asymmetry brackets (left of the box)
x_wh = 0.55
ax1.annotate("", xy=(x_wh, xmin), xytext=(x_wh, q1),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["warn"], lw=1.6))
ax1.text(x_wh - 0.05, (xmin + q1) / 2,
         f"lower whisker\n$Q_1-$min = {lw_len:.0f}",
         color=PALETTE["warn"], ha="right", va="center",
         fontsize=10, fontweight="bold")

ax1.annotate("", xy=(x_wh, q3), xytext=(x_wh, xmax),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["warn"], lw=1.6))
ax1.text(x_wh - 0.05, (q3 + xmax) / 2,
         f"upper whisker\nmax$-Q_3$ = {uw_len:.0f}",
         color=PALETTE["warn"], ha="right", va="center",
         fontsize=10, fontweight="bold")

# Box-half asymmetry brackets (right of the box, inside-out)
x_bb = 1.95
ax1.annotate("", xy=(x_bb, q1), xytext=(x_bb, median),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["accent"], lw=1.6))
ax1.text(x_bb + 0.04, (q1 + median) / 2,
         f"lower half of box\nMe$-Q_1$ = {lb_len:.0f}",
         color=PALETTE["accent"], ha="left", va="center",
         fontsize=10, fontweight="bold")

ax1.annotate("", xy=(x_bb, median), xytext=(x_bb, q3),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["accent"], lw=1.6))
ax1.text(x_bb + 0.04, (median + q3) / 2,
         f"upper half of box\n$Q_3-$Me = {ub_len:.0f}",
         color=PALETTE["accent"], ha="left", va="center",
         fontsize=10, fontweight="bold")

# No-outlier verdict in the corner
ax1.text(0.02, 0.97,
         f"Fences: $Q_1-1.5\\cdot$IQR = {fence_lo:.1f}\n"
         f"           $Q_3+1.5\\cdot$IQR = {fence_hi:.1f}\n"
         f"-> min={xmin:.0f} > {fence_lo:.1f}, max={xmax:.0f} < {fence_hi:.1f}\n"
         "   no outliers",
         transform=ax1.transAxes, ha="left", va="top",
         fontsize=10, fontweight="bold", color=PALETTE["ok"],
         bbox=dict(facecolor="#ebf7ef", edgecolor=PALETTE["ok"],
                   boxstyle="round,pad=0.45", linewidth=1.1))

ax1.set_xticks([1])
ax1.set_xticklabels(["Nr_visits (customer_habits)"])
ax1.set_ylabel("Nr_visits per year")
ax1.set_title("Boxplot of Nr_visits — whisker vs box asymmetry ($n = 2200$)")
ax1.set_xlim(-0.15, 2.75)
ax1.set_ylim(-1, xmax * 1.18)
ax1.grid(axis="y", alpha=0.5)

# =========================================================================
# RIGHT — outlier check + asymmetry verdict
# =========================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)

ax2.text(0.5, 0.965,
         "Boxplot construction and asymmetry, step by step",
         ha="center", va="top", fontsize=12.5, fontweight="bold",
         color=PALETTE["primary"])

# Step 1 — fence check
ax2.text(0.02, 0.90, "1. Outlier check (Tukey fences)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.84,
         f"Q1 - 1.5*IQR = {q1:.0f} - 1.5*{iqr:.0f} = {fence_lo:.1f}\n"
         f"Q3 + 1.5*IQR = {q3:.0f} + 1.5*{iqr:.0f} = {fence_hi:.1f}\n"
         f"min = {xmin:.0f} > {fence_lo:.1f}  and  max = {xmax:.0f} < {fence_hi:.1f}\n"
         "  -> no outliers; whiskers reach min and max",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["ok"],
         bbox=dict(facecolor="#ebf7ef", edgecolor=PALETTE["ok"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 2 — whisker asymmetry
ax2.text(0.02, 0.62, "2. Whisker asymmetry (tails of the distribution)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.56,
         f"lower whisker:  Q1 - min = {q1:.0f} - {xmin:.0f} = {lw_len:.0f}\n"
         f"upper whisker:  max - Q3 = {xmax:.0f} - {q3:.0f} = {uw_len:.0f}\n"
         f"  -> upper is {uw_len/lw_len:.0f}x longer than lower\n"
         "  -> top 25% (regular clients) is heterogeneous,\n"
         "     bottom 25% (rare visitors) is compact",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["warn"],
         bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 3 — box asymmetry
ax2.text(0.02, 0.30, "3. Box asymmetry (central 50% around Me)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.24,
         f"lower half:  Me - Q1 = {median:.0f} - {q1:.0f} = {lb_len:.0f}\n"
         f"upper half:  Q3 - Me = {q3:.0f} - {median:.0f} = {ub_len:.0f}\n"
         "  -> box leans to the LEFT (lower half wider)\n"
         "  -> opposite sign vs the whiskers:\n"
         "     no clean overall left/right skew",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["accent"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  fences=[{fence_lo:.1f}, {fence_hi:.1f}]  no-outlier check OK")
print(f"  whiskers: lower={lw_len:.0f}, upper={uw_len:.0f}  (ratio {uw_len/lw_len:.1f}x)")
print(f"  box halves: Me-Q1={lb_len:.0f}, Q3-Me={ub_len:.0f}")
