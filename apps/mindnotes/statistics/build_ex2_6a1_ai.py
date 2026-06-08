"""Build AI walkthrough plot for Ex 2.6a1 — deciles + p90/p95/p99 of
`Revenue` (customer_habits, n = 34 866).

The original snippet just prints a table of deciles and high-tail
percentiles. The textual description ("the gaps between deciles grow with
their order, hence the right-skewness") is exactly the kind of claim that
becomes obvious once you SEE the decile gaps on a number line and the
cumulative mass climbing.

LEFT
  A number line with the 9 deciles + p95 + p99 marked as vertical ticks,
  on a Revenue-axis truncated to p99 * 1.05 so the high-tail can be read.
  The horizontal "gap bars" between consecutive deciles are drawn ABOVE
  the number line: gap_k = D_{k+1} - D_k. The bars get visibly wider on
  the right -- this is the empirical signature of right-skewness that
  motivates reporting deciles instead of just the mean.

RIGHT
  The empirical CDF zoomed on Revenue <= p99 with horizontal dashed lines
  at 0.1, 0.2, ..., 0.9, 0.95, 0.99 and vertical dashed lines at the
  corresponding decile / percentile values. The viewer reads off each
  decile by projecting horizontally then dropping vertically; the kink
  on the right shows how much extra Revenue mass lives above p90.

We use shutil to copy the (newly captured) question + answer screenshots
into the ex2/questions and ex2/answers folders under the canonical names
ex2_6a1_question.png and ex2_6a1_answer.png. Only the copies that
actually succeed are referenced from the snippet.
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import pyreadr
import matplotlib.pyplot as plt

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
Q_DIR = f"{BASE}/questions"
A_DIR = f"{BASE}/answers"
OUT   = f"{BASE}/ex2_6a1_ai.png"
os.makedirs(BASE,  exist_ok=True)
os.makedirs(Q_DIR, exist_ok=True)
os.makedirs(A_DIR, exist_ok=True)

# ---------------------------------------------------------------
# 1.  Use shutil to copy the user-provided question / answer
#     screenshots into the canonical ex2/{questions,answers}/
#     folder structure used by every other 2.x snippet.
#     Only successful copies are referenced from the snippet.
# ---------------------------------------------------------------
COPIED = []

# Note: macOS Screenshot filenames use U+202F (narrow no-break space)
# between the time and "PM", NOT a regular ASCII space.
NBSP = "\u202f"
SRC_Q = (f"/Users/Alessandro/Repos/my note taking app/statistics/ex2/questions/"
         f"Screenshot 2026-06-05 at 4.11.45{NBSP}PM.png")
DST_Q = f"{Q_DIR}/ex2_6a1_question.png"
if os.path.exists(SRC_Q):
    shutil.copyfile(SRC_Q, DST_Q)
    COPIED.append(DST_Q)
    print(f"copied -> {DST_Q}")
else:
    print(f"skip (missing source) -> {SRC_Q}")

SRC_A = (f"/Users/Alessandro/Repos/my note taking app/statistics/ex2/answers/"
         f"Screenshot 2026-06-05 at 4.08.45{NBSP}PM.png")
DST_A = f"{A_DIR}/ex2_6a1_answer.png"
if os.path.exists(SRC_A):
    shutil.copyfile(SRC_A, DST_A)
    COPIED.append(DST_A)
    print(f"copied -> {DST_A}")
else:
    print(f"skip (missing source) -> {SRC_A}")

# ---------------------------------------------------------------
# 2.  Load Revenue and compute deciles + p95, p99.
# ---------------------------------------------------------------
result = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata")
rev = result["customer_habits"]["Revenue"].to_numpy()
n   = len(rev)

probs = np.array([0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90])
deciles = np.quantile(rev, probs)
p95 = float(np.quantile(rev, 0.95))
p99 = float(np.quantile(rev, 0.99))
mn  = float(np.min(rev))
mx  = float(np.max(rev))

print("  deciles:", dict(zip([f"p{int(100*p)}" for p in probs], deciles.round(2))))
print(f"  p95={p95:.2f}  p99={p99:.2f}  min={mn:.2f}  max={mx:.2f}")

# Gaps D_{k+1} - D_k between consecutive deciles
labels = [f"p{int(100*p)}" for p in probs]
gaps   = np.diff(deciles)
gap_labels = [f"{labels[i]}->{labels[i+1]}" for i in range(len(gaps))]

# ---------------------------------------------------------------
# 3.  Figure.
# ---------------------------------------------------------------
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.6, 5.6),
                               gridspec_kw={"width_ratios": [1.05, 1.0]})

x_max_view = p99 * 1.05

# =============== LEFT — decile gaps growing with order ===============
# Bars: width = gap between consecutive deciles, positioned BETWEEN
# the two deciles on the x-axis. Bars are drawn above the number line.
y_bar = 1.0
bar_h = 0.55
for i, (lo, hi) in enumerate(zip(deciles[:-1], deciles[1:])):
    ax1.add_patch(plt.Rectangle((lo, y_bar), hi - lo, bar_h,
                                facecolor=PALETTE["accent"], alpha=0.40 + 0.05 * i,
                                edgecolor=PALETTE["primary"], linewidth=0.9,
                                zorder=3))
    # gap label, centered on bar, only every other one to avoid crowding
    if i % 2 == 1 or i >= 5:
        ax1.text((lo + hi) / 2, y_bar + bar_h + 0.06,
                 f"{(hi-lo):.0f}", ha="center", va="bottom",
                 fontsize=9.0, color=PALETTE["primary"], fontweight="bold")

# Decile tick marks on the number line at y = 0
for d, lab in zip(deciles, labels):
    ax1.vlines(d, 0, 0.35, color=PALETTE["primary"], lw=1.4, zorder=4)
    ax1.text(d, -0.08, f"{lab}\n{d:.0f}", ha="center", va="top",
             fontsize=8.6, color=PALETTE["primary"])

# p95 and p99 ticks
for x, lab in [(p95, f"p95\n{p95:.0f}"), (p99, f"p99\n{p99:.0f}")]:
    if x <= x_max_view:
        ax1.vlines(x, 0, 0.35, color=PALETTE["warn"], lw=1.6, zorder=4)
        ax1.text(x, -0.08, lab, ha="center", va="top",
                 fontsize=8.6, color=PALETTE["warn"], fontweight="bold")

# Number line baseline
ax1.hlines(0, -x_max_view * 0.02, x_max_view * 1.02,
           color=PALETTE["primary"], lw=1.2, zorder=2)

# Annotation: arrow showing that gaps grow with order
ax1.annotate("",
             xy=(deciles[7] + (deciles[8] - deciles[7]) * 0.5, y_bar + bar_h + 0.45),
             xytext=(deciles[0] + (deciles[1] - deciles[0]) * 0.5, y_bar + bar_h + 0.45),
             arrowprops=dict(arrowstyle="->", color=PALETTE["warn"],
                             lw=1.6, shrinkA=0, shrinkB=0))
ax1.text((deciles[0] + deciles[8]) / 2, y_bar + bar_h + 0.52,
         "decile gaps grow with order  =>  right-skewness",
         ha="center", va="bottom", fontsize=10.5,
         color=PALETTE["warn"], fontweight="bold")

ax1.set_xlim(-x_max_view * 0.02, x_max_view * 1.02)
ax1.set_ylim(-0.55, 2.1)
ax1.set_yticks([])
ax1.set_xlabel(fr"Revenue  (view truncated at $p_{{99}} \cdot 1.05 = {x_max_view:.0f}$;  global max = {mx:.0f})")
ax1.set_title("Ex 2.6a1 - Deciles of Revenue (gap above each decile band)")
ax1.grid(False)

# =============== RIGHT — empirical CDF with decile cross-hairs ===============
xs = np.sort(rev)
ys = np.arange(1, n + 1) / n
mask = xs <= p99
ax2.plot(xs[mask], ys[mask], color=PALETTE["primary"], lw=1.8,
         label="empirical CDF F(x)")

# Horizontal dashed lines at decile + p95 + p99 probabilities
all_probs  = list(probs) + [0.95, 0.99]
all_vals   = list(deciles) + [p95, p99]
all_lab    = labels + ["p95", "p99"]
all_colors = [PALETTE["secondary"]] * len(probs) + [PALETTE["warn"], PALETTE["warn"]]

for p, v, lab, col in zip(all_probs, all_vals, all_lab, all_colors):
    ax2.hlines(p, 0, v, color=col, lw=0.8, ls="--", alpha=0.55, zorder=2)
    ax2.vlines(v, 0, p, color=col, lw=0.8, ls="--", alpha=0.55, zorder=2)
    ax2.scatter([v], [p], s=22, color=col, zorder=5,
                edgecolor=PALETTE["primary"], linewidth=0.6)

# Probability axis labels on the right side
for p, lab in zip(all_probs, all_lab):
    ax2.text(p99 * 1.02, p, lab, va="center", ha="left",
             fontsize=8.5,
             color=PALETTE["warn"] if p >= 0.95 else PALETTE["primary"])

ax2.set_xlim(0, p99 * 1.10)
ax2.set_ylim(0, 1.02)
ax2.set_xlabel(fr"Revenue  (truncated at $p_{{99}} = {p99:.0f}$)")
ax2.set_ylabel("cumulative probability  F(x)")
ax2.set_title("Empirical CDF: deciles + p95, p99 as cross-hairs")
ax2.legend(loc="lower right", framealpha=0.95, fontsize=9.5)

# Annotation explaining the right-tail kink
ax2.text(0.02, 0.98,
         (fr"$F(p_{{90}}) = 0.90$  at  Revenue $= {deciles[-1]:.0f}$" + "\n"
          fr"$F(p_{{95}}) = 0.95$  at  Revenue $= {p95:.0f}$" + "\n"
          fr"$F(p_{{99}}) = 0.99$  at  Revenue $= {p99:.0f}$" + "\n"
          fr"global max $= {mx:.0f}$  (>> $p_{{99}}$)"),
         transform=ax2.transAxes, ha="left", va="top", fontsize=9.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
COPIED.append(OUT)
print(f"saved -> {OUT}")
print(f"PNGs available for snippet: {COPIED}")
