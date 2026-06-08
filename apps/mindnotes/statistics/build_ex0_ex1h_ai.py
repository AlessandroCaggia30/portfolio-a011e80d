"""Build the PNG assets for Ex 0 / 1h — cumulative frequency for the ordinal
factor ``Happyness.Level``.

Two responsibilities:

1. Use ``shutil`` to clone the textbook prompt + textbook answer screenshots
   from ``my note taking app/statistics/ex0/{questions,answers}/`` into
   ``portfolio-a011e80d/apps/mindnotes/statistics/images/ex0/{questions,answers}/``
   under the canonical ``ex1h_question.png`` / ``ex1h_answer.png`` filenames
   that the ``ex0["ex1h"]`` snippet references.  Source filenames use the
   narrow-no-break-space (U+202F) that macOS now inserts between "at" and the
   time, so we use the exact unicode literal and ``shutil.copyfile``.

2. Render an AI walkthrough plot ``ex0/ex1h_ai.png`` that makes the ordinal
   cumulative reasoning visual:
     LEFT  -- proportion bars (relative frequencies) for the five
              Happyness.Level categories, with the two top levels ("Quite
              happy" + "Happiest") highlighted as the answer to the "what
              % are happiest or quite happy" sub-question.
     RIGHT -- the cumulative proportion step (ordered factor), with the
              level "So and so" marked at 0.6939 — the second sub-question.
"""
import os, sys, shutil

sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

apply_style()

# ----------------------------------------------------------------------
# 1. Clone the textbook prompt + answer screenshots via shutil
# ----------------------------------------------------------------------
NBSP = "\u202f"   # narrow no-break space used in macOS screenshot filenames

SRC_Q_DIR = "/Users/Alessandro/Repos/my note taking app/statistics/ex0/questions"
SRC_A_DIR = "/Users/Alessandro/Repos/my note taking app/statistics/ex0/answers"
DST_Q_DIR = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex0/questions"
DST_A_DIR = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex0/answers"
DST_DIR   = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex0"
os.makedirs(DST_Q_DIR, exist_ok=True)
os.makedirs(DST_A_DIR, exist_ok=True)

# The Ex 1 prompt screenshot covers every Ex 1 sub-question (a..l), so the
# same source file feeds ex1a, ex1d, ex1h, ...  The answer screenshot is
# the dedicated "h." panel showing the Happyness.Level cumulative table.
clones = [
    (f"{SRC_Q_DIR}/Screenshot 2026-06-05 at 3.47.05{NBSP}PM.png",
     f"{DST_Q_DIR}/ex1h_question.png"),
    (f"{SRC_A_DIR}/Screenshot 2026-06-05 at 3.35.24{NBSP}PM.png",
     f"{DST_A_DIR}/ex1h_answer.png"),
]
for src, dst in clones:
    if os.path.exists(src):
        shutil.copyfile(src, dst)
        print(f"copied -> {dst}")
    else:
        print(f"WARN: source missing, skipped -> {src}")

# ----------------------------------------------------------------------
# 2. AI walkthrough plot — proportions + cumulative for an ordered factor
# ----------------------------------------------------------------------
OUT = f"{DST_DIR}/ex1h_ai.png"

levels = ["Unhappiest", "Quite unhappy", "So and so", "Quite happy", "Happiest"]
counts = np.array([10, 12, 12, 8, 7])
N = counts.sum()                       # 49 (two NAs dropped from 51)
prop  = counts / N                     # 0.2041, 0.2449, 0.2449, 0.1633, 0.1429
cumprop = np.cumsum(prop)              # ..., 0.6939 at "So and so"

# Indices for the two highlighted answers
HAPPY_IDX = [3, 4]                     # Quite happy + Happiest
SOSO_IDX  = 2                          # "So and so" — cumulative answer
happy_sum = prop[HAPPY_IDX].sum()      # 0.3061
soso_cum  = cumprop[SOSO_IDX]          # 0.6939

print(f"N (non-NA) = {N}")
for L, c, p, cp in zip(levels, counts, prop, cumprop):
    print(f"  {L:<14} count={c:>2}  prop={p:.4f}  cum.prop={cp:.4f}")
print(f"Quite happy + Happiest = {happy_sum:.4f}  ((8+7)/49 = {(8+7)/49:.7f})")
print(f"Cum.Prop at 'So and so' = {soso_cum:.4f}")

x = np.arange(len(levels))

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.2, 5.4),
                               gridspec_kw={"width_ratios": [1.15, 1]})

# =====================================================================
# LEFT — proportion bars with the top-two levels highlighted
# =====================================================================
bar_colors = [PALETTE["primary"] if i not in HAPPY_IDX else PALETTE["accent"]
              for i in range(len(levels))]
alphas     = [0.30 if i not in HAPPY_IDX else 0.90 for i in range(len(levels))]
for xi, p, col, a in zip(x, prop, bar_colors, alphas):
    ax1.bar(xi, p, width=0.72, color=col, alpha=a,
            edgecolor=PALETTE["primary"], linewidth=1.0, zorder=2)
    ax1.text(xi, p + 0.008, f"{p:.4f}",
             ha="center", va="bottom",
             fontsize=10, color=PALETTE["primary"], fontweight="bold")

# Bracket above the two highlighted levels
y_top = max(prop) * 1.30
ax1.annotate("", xy=(HAPPY_IDX[1] + 0.32, y_top),
             xytext=(HAPPY_IDX[0] - 0.32, y_top),
             arrowprops=dict(arrowstyle="-", color="#7a6700", lw=1.4))
ax1.text((HAPPY_IDX[0] + HAPPY_IDX[1]) / 2, y_top + 0.012,
         fr"Quite happy + Happiest $= {prop[3]:.4f} + {prop[4]:.4f} = {happy_sum:.4f}$"
         "\n"
         fr"$\equiv (8 + 7)/49 = {(8+7)/49:.4f}$",
         ha="center", va="bottom", fontsize=10,
         color="#7a6700", fontweight="bold")

ax1.set_xticks(x)
ax1.set_xticklabels(levels, rotation=20, ha="right", fontsize=10)
ax1.set_ylim(0, y_top + 0.10)
ax1.set_ylabel(r"Relative frequency $p_k = n_k / N$")
ax1.set_title("Happyness.Level — proportions  (N = 49, two NAs dropped)",
              fontsize=11.5, color=PALETTE["primary"])

# =====================================================================
# RIGHT — cumulative step diagram (ordered factor)
# =====================================================================
# Build a step path: flat-then-jump at each level index
x_step = []
y_step = []
prev = 0.0
for xi, cp in zip(x, cumprop):
    x_step += [xi - 0.5, xi + 0.5]
    y_step += [prev, prev]      # flat at prev (entry-side)
    # then jump at xi+0.5 ... actually do a discrete step diagram:
    prev = cp

# Re-do cleanly: a simple staircase rising at each level mid-point.
x_stair = []
y_stair = []
prev = 0.0
for xi, cp in zip(x, cumprop):
    # flat segment on [xi-0.5, xi+0.5] at cp
    x_stair += [xi - 0.5, xi + 0.5]
    y_stair += [cp, cp]

ax2.step(np.concatenate([[-0.5], x + 0.5]),
         np.concatenate([[0.0], cumprop]),
         where="post", color=PALETTE["primary"], lw=2.0, zorder=3)

# Marker dots at each level showing Cum.Prop
for xi, cp in zip(x, cumprop):
    ax2.plot(xi, cp, "o", color=PALETTE["primary"], ms=7, zorder=4)
    ax2.text(xi, cp + 0.025, f"{cp:.4f}",
             ha="center", va="bottom", fontsize=9.5,
             color=PALETTE["primary"])

# Highlight "So and so" cumulative answer
ax2.axhline(soso_cum, color=PALETTE["warn"], lw=1.2, ls="--", alpha=0.9,
            zorder=2)
ax2.axvline(SOSO_IDX, color=PALETTE["warn"], lw=1.2, ls="--", alpha=0.9,
            zorder=2)
ax2.plot(SOSO_IDX, soso_cum, "o", color=PALETTE["warn"], ms=12,
         markerfacecolor=PALETTE["warn"], markeredgecolor="#7a6700",
         markeredgewidth=1.5, zorder=5)
ax2.annotate(fr"Cum.Prop at 'So and so' $= {soso_cum:.4f}$",
             xy=(SOSO_IDX, soso_cum),
             xytext=(SOSO_IDX + 0.35, soso_cum - 0.18),
             fontsize=10.5, color="#7a6700", fontweight="bold",
             arrowprops=dict(arrowstyle="->", color="#7a6700", lw=1.2))

ax2.set_xticks(x)
ax2.set_xticklabels(levels, rotation=20, ha="right", fontsize=10)
ax2.set_xlim(-0.6, len(levels) - 0.4)
ax2.set_ylim(0, 1.08)
ax2.set_ylabel("Cumulative proportion")
ax2.set_title("Cumulative — only valid because the factor is ORDERED",
              fontsize=11.5, color=PALETTE["primary"])

# Card emphasizing the ordinal prerequisite
card = (
    r"Cum.Prop requires an ORDERED factor."
    "\n"
    r"Here R's levels are: Unhappiest $<$ Quite unhappy $<$"
    "\n"
    r"So and so $<$ Quite happy $<$ Happiest."
    "\n\n"
    r"If the factor were unordered, both answers"
    "\n"
    r"(sum of top two, cumulative at a level) would be"
    "\n"
    r"\textbf{undefined}."
)
ax2.text(0.02, 0.97, card.replace(r"\textbf{undefined}", "undefined"),
         transform=ax2.transAxes,
         ha="left", va="top", fontsize=9.0,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle(r"Ex 0 / 1h — Happyness.Level: proportions, top-two sum, and cumulative at 'So and so'",
             fontsize=12.5, color=PALETTE["primary"], y=1.02)

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
