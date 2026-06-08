"""Build the PNG assets for Ex 0 / 2a2 — P(50 <= fare < 100) by the
uniform-on-interval assumption (Titanic fare, grouped density table).

Two responsibilities:

1. Use ``shutil`` to clone the textbook prompt + textbook answer screenshots
   from ``my note taking app/statistics/ex0/{questions,answers}/`` into
   ``portfolio-a011e80d/apps/mindnotes/statistics/images/ex0/{questions,answers}/``
   under the canonical ``ex0_2a2_question.png`` / ``ex0_2a2_answer.png`` filenames
   that the ``ex0["ex2a2"]`` snippet references.  Source filenames use the
   narrow-no-break-space (U+202F) that macOS now inserts between "at" and the
   time, so we use the exact unicode literal and ``shutil.copyfile``.

2. Render an AI walkthrough plot ``ex0/ex0_2a2_ai.png`` that makes the
   uniform-on-interval split of the class [30, 60) completely concrete:
     LEFT  -- the original density histogram with the target band [50, 100)
              shaded.  Inside class [30, 60) we mark the sub-bar [50, 60) as
              the "uniform 1/3" slice (since 10/30 of the class width).  The
              full bar over [60, 100) is shaded too.
     RIGHT -- a stacked bar decomposes the answer 0.1203 into its two
              contributions: the partial [50, 60) chunk (~0.0419) and the
              full [60, 100) class (~0.0784).  A small card spells out the
              arithmetic and flags that the split is an APPROXIMATION.
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

# The Exercise 2 prompt screenshot covers every Ex 2 sub-question (a1..a3,
# b1..b3) — so the same source feeds ex2a1, ex2a2, ex2a3, ...  The answer
# screenshot is the dedicated "a2." panel showing the uniform-on-interval
# split for fare [50, 100).
clones = [
    (f"{SRC_Q_DIR}/Screenshot 2026-06-05 at 3.47.12{NBSP}PM.png",
     f"{DST_Q_DIR}/ex0_2a2_question.png"),
    (f"{SRC_A_DIR}/Screenshot 2026-06-05 at 3.36.57{NBSP}PM.png",
     f"{DST_A_DIR}/ex0_2a2_answer.png"),
]
for src, dst in clones:
    if os.path.exists(src):
        shutil.copyfile(src, dst)
        print(f"copied -> {dst}")
    else:
        print(f"WARN: source missing, skipped -> {src}")

# ----------------------------------------------------------------------
# 2. AI walkthrough plot — uniform-on-interval split of [30, 60)
# ----------------------------------------------------------------------
OUT = f"{DST_DIR}/ex0_2a2_ai.png"

# Grouped fare density table from the prompt
lower = np.array([  0.0,  10.0,  20.0,  30.0,  60.0, 100.0, 180.0])
upper = np.array([ 10.0,  20.0,  30.0,  60.0, 100.0, 180.0, 270.0])
dens  = np.array([0.03765, 0.02002, 0.01580, 0.00419, 0.00196, 0.00044, 0.00029])
width = upper - lower
prop  = dens * width   # 0.3765, 0.2002, 0.1580, 0.1257, 0.0784, 0.0352, 0.0261

# Target band [50, 100) — split by class boundary at 60
PARTIAL_LO, BOUNDARY, FULL_HI = 50.0, 60.0, 100.0
CLASS_LO, CLASS_HI = 30.0, 60.0
D_BIG = dens[3]   # density of the class [30, 60)
D_RIGHT = dens[4] # density of the class [60, 100)

# Contributions
partial_share = (BOUNDARY - PARTIAL_LO) / (CLASS_HI - CLASS_LO)     # 10/30 = 1/3
partial_mass  = prop[3] * partial_share                              # 0.1257/3 = 0.0419
full_mass     = prop[4]                                              # 0.0784
total_mass    = partial_mass + full_mass                             # 0.1203

print(f"partial [50,60) mass = {partial_mass:.4f}")
print(f"full    [60,100) mass = {full_mass:.4f}")
print(f"total   [50,100) mass = {total_mass:.4f}")

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.2, 5.4),
                               gridspec_kw={"width_ratios": [1.4, 1]})

# =====================================================================
# LEFT — density histogram with the target band highlighted
# =====================================================================
for lo, hi, d in zip(lower, upper, dens):
    in_target = (hi > PARTIAL_LO) and (lo < FULL_HI)
    ax1.bar(lo, d, width=hi - lo, align="edge",
            color=PALETTE["primary"],
            alpha=0.55 if in_target else 0.18,
            edgecolor=PALETTE["primary"], linewidth=1.0, zorder=2)

# Sub-bar [50, 60) under the class [30, 60) — "uniform 1/3" slice
ax1.add_patch(Rectangle((PARTIAL_LO, 0), BOUNDARY - PARTIAL_LO, D_BIG,
                        facecolor=PALETTE["accent"], alpha=0.85,
                        edgecolor="#7a6700", linewidth=1.4, zorder=3,
                        label=fr"partial $[50,60)$: $(60-50)\cdot 0.00419 = 0.0419$"))

# Full class [60, 100) — already shaded above; outline it for emphasis
ax1.add_patch(Rectangle((BOUNDARY, 0), FULL_HI - BOUNDARY, D_RIGHT,
                        facecolor=PALETTE["secondary"], alpha=0.75,
                        edgecolor=PALETTE["primary"], linewidth=1.4, zorder=3,
                        label=fr"full $[60,100)$: $(100-60)\cdot 0.00196 = 0.0784$"))

# Threshold markers
ax1.axvline(PARTIAL_LO, color=PALETTE["warn"], lw=1.6, ls="--", zorder=4,
            label="threshold $x = 50$ (mid-class)")
ax1.axvline(FULL_HI, color=PALETTE["neutral"], lw=1.2, ls=":", zorder=4)
ax1.axvline(BOUNDARY, color=PALETTE["neutral"], lw=0.9, ls="-", alpha=0.6, zorder=2)

# Annotations on the bars
ax1.annotate(r"uniform on $[30,60)$",
             xy=((CLASS_LO + CLASS_HI) / 2, D_BIG * 1.12),
             ha="center", va="bottom",
             fontsize=10, color=PALETTE["primary"], style="italic")
ax1.annotate(r"$\frac{60-50}{60-30}=\frac{1}{3}$ of mass",
             xy=((PARTIAL_LO + BOUNDARY) / 2, D_BIG * 0.55),
             ha="center", va="center",
             fontsize=9.5, color="#7a6700", fontweight="bold")

ax1.set_xlim(-5, 195)
ax1.set_ylim(0, max(dens) * 1.18)
ax1.set_xlabel("fare")
ax1.set_ylabel(r"Density  $c_k = p_k / w_k$")
ax1.set_title(r"Splitting $[50,100)$ across a class boundary at 60",
              fontsize=11.5, color=PALETTE["primary"])
ax1.legend(loc="upper right", framealpha=0.95, fontsize=9.2)

# =====================================================================
# RIGHT — stacked decomposition of the answer 0.1203
# =====================================================================
labels = ["partial\n$[50,60)$", "full\n$[60,100)$", "total\n$[50,100)$"]
vals   = [partial_mass, full_mass, total_mass]
cols   = [PALETTE["accent"], PALETTE["secondary"], PALETTE["primary"]]

# First two as separate bars; third bar is stacked
xpos = np.arange(3)
ax2.bar(xpos[0], partial_mass, width=0.6, color=cols[0],
        edgecolor="#2a3142", linewidth=0.9, alpha=0.9)
ax2.bar(xpos[1], full_mass, width=0.6, color=cols[1],
        edgecolor="#2a3142", linewidth=0.9, alpha=0.9)
# Stacked total: partial on bottom, full on top
ax2.bar(xpos[2], partial_mass, width=0.6, color=cols[0],
        edgecolor="#2a3142", linewidth=0.9, alpha=0.9)
ax2.bar(xpos[2], full_mass, bottom=partial_mass, width=0.6, color=cols[1],
        edgecolor="#2a3142", linewidth=0.9, alpha=0.9)

# Value labels
for i, v in enumerate(vals):
    ax2.text(xpos[i], v + 0.004, f"{v:.4f}",
             ha="center", va="bottom",
             fontsize=11, fontweight="bold", color="#2a3142")
# Label the two stack segments on the third bar
ax2.text(xpos[2], partial_mass / 2, f"{partial_mass:.4f}",
         ha="center", va="center", fontsize=9, color="#5a4d00")
ax2.text(xpos[2], partial_mass + full_mass / 2, f"{full_mass:.4f}",
         ha="center", va="center", fontsize=9, color=PALETTE["primary"])

ax2.set_xticks(xpos)
ax2.set_xticklabels(labels, fontsize=10.5)
ax2.set_ylabel(r"Relative frequency contribution")
ax2.set_ylim(0, total_mass * 1.30)
ax2.set_title("Two contributions add to 0.1203",
              fontsize=11.5, color=PALETTE["primary"])

# Arithmetic card
card = (
    r"Uniform-on-interval split of $[30,60)$:"
    "\n"
    r"  $\mathrm{Freq}[50,100) = (60-50)\cdot c_4 + (100-60)\cdot c_5$"
    "\n"
    r"  $\quad = 10\cdot 0.00419 + 40\cdot 0.00196$"
    "\n"
    r"  $\quad = 0.0419 + 0.0784 = 0.1203$"
    "\n\n"
    r"$\Rightarrow$ 0.1203 is an APPROXIMATION:"
    "\n"
    r"the within-class shape on $[30,60)$ is unknown."
)
ax2.text(0.02, 0.97, card, transform=ax2.transAxes,
         ha="left", va="top", fontsize=9.2,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle(r"Ex 0 / 2a2 — $P(50 \leq \mathrm{fare} < 100)$ via the uniform-on-interval rule",
             fontsize=13, color=PALETTE["primary"], y=1.02)

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
