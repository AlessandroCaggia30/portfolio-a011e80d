"""Build the PNG assets for Ex 0 / 1d — building a rate variable for violent crimes.

Two responsibilities:

1. Use ``shutil`` to clone the textbook prompt + textbook answer screenshots
   from ``my note taking app/statistics/ex0/{questions,answers}/`` into
   ``portfolio-a011e80d/apps/mindnotes/statistics/images/`` under the flat
   ``ex0-ex1d-question.png`` / ``ex0-ex1d-answer.png`` filenames that the
   ``ex0["ex1d"]`` snippet references. The source filenames contain the
   narrow-no-break-space (U+202F) that macOS now inserts between "at" and
   the time, so we use the exact unicode literal and ``shutil.copyfile``
   rather than any shell glob.

2. Render an AI walkthrough plot ``ex0-ex1d-ai.png`` that makes the rate
   construction completely concrete. Two panels:
     LEFT  -- horizontal bars comparing raw violent-crime counts for three
              illustrative US states (CA huge, NY medium, WY tiny) next to
              the same states' populations, so the visual confound between
              raw counts and population size is obvious.
     RIGHT -- the same three states after dividing by population and
              multiplying by 100 000, with a small card spelling out the
              formula
                Rate.Violent = 100 000 * (Assault + Murder + Rape + Robbery) / Population.
"""
import os, sys, shutil

sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

# ----------------------------------------------------------------------
# 1. Clone the textbook prompt + answer screenshots via shutil
# ----------------------------------------------------------------------
NBSP = "\u202f"   # narrow no-break space used in macOS screenshot filenames

SRC_Q_DIR = "/Users/Alessandro/Repos/my note taking app/statistics/ex0/questions"
SRC_A_DIR = "/Users/Alessandro/Repos/my note taking app/statistics/ex0/answers"
DST_DIR   = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images"
os.makedirs(DST_DIR, exist_ok=True)

# The Ex 1 prompt screenshot covers every Ex 1 sub-question (a..l),
# so the same source file feeds ex1a, ex1d, ... The answer screenshot is
# the dedicated "d." panel showing the Rate.Violent construction.
clones = [
    (f"{SRC_Q_DIR}/Screenshot 2026-06-05 at 3.47.05{NBSP}PM.png",
     f"{DST_DIR}/ex0-ex1d-question.png"),
    (f"{SRC_A_DIR}/Screenshot 2026-06-05 at 3.34.39{NBSP}PM.png",
     f"{DST_DIR}/ex0-ex1d-answer.png"),
]
for src, dst in clones:
    if os.path.exists(src):
        shutil.copyfile(src, dst)
        print(f"copied -> {dst}")
    else:
        print(f"WARN: source missing, skipped -> {src}")

# ----------------------------------------------------------------------
# 2. AI walkthrough plot — why a rate, not a raw count
# ----------------------------------------------------------------------
OUT = f"{DST_DIR}/ex0-ex1d-ai.png"

# Three illustrative states (approx. 2010-ish figures, used for visual
# intuition only — the snippet's actual numbers come from Data_USA).
states = ["California", "New York", "Wyoming"]
pop    = np.array([37_253_956, 19_378_102,   563_626])   # inhabitants
# Sum of the four violent-crime counts (assault + murder + rape + robbery)
viol   = np.array([     154_944,     78_954,     1_193])   # raw counts

rate   = 100_000 * viol / pop                              # per 100 000

print("State            pop           viol     rate/100k")
for s, p, v, r in zip(states, pop, viol, rate):
    print(f"  {s:<14} {p:>10}   {v:>8}   {r:8.1f}")

# Color per state — visual continuity across the two panels
state_colors = [PALETTE["accent"], PALETTE["primary"], PALETTE["warn"]]

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.2, 5.4),
                               gridspec_kw={"width_ratios": [1.25, 1]})

# =====================================================================
# LEFT — raw counts side-by-side with population (the confound)
# =====================================================================
y = np.arange(len(states))
bar_h = 0.36

# Population bars (light, in the background) on a secondary x-axis
ax1b = ax1.twiny()
ax1b.barh(y + bar_h/2, pop, height=bar_h,
          color=PALETTE["neutral"], alpha=0.25,
          edgecolor=PALETTE["neutral"], linewidth=0.6,
          label="Population (inhabitants)")
ax1b.set_xlim(0, max(pop) * 1.18)
ax1b.set_xlabel("Population", color=PALETTE["neutral"], fontsize=9.5)
ax1b.tick_params(axis="x", colors=PALETTE["neutral"], labelsize=8.5)
ax1b.spines["top"].set_visible(True)
ax1b.spines["top"].set_color(PALETTE["neutral"])

# Raw violent-crime counts (bold) on the primary x-axis
for i, (v, col) in enumerate(zip(viol, state_colors)):
    ax1.barh(i - bar_h/2, v, height=bar_h,
             color=col, edgecolor=PALETTE["primary"], linewidth=0.8)
    ax1.text(v + max(viol) * 0.015, i - bar_h/2,
             f"{v:,}", va="center", ha="left",
             fontsize=9.5, color=PALETTE["primary"], fontweight="bold")

ax1.set_yticks(y)
ax1.set_yticklabels(states, fontsize=10.5)
ax1.invert_yaxis()
ax1.set_xlim(0, max(viol) * 1.28)
ax1.set_xlabel("Raw violent-crime count")
ax1.set_title("Raw counts are confounded by state size",
              fontsize=11, color=PALETTE["primary"])

# Annotate the trap
ax1.text(max(viol) * 0.55, 0 + bar_h * 1.3,
         "CA looks worst — but it also has\nby far the largest population.",
         fontsize=9, color=PALETTE["accent"],
         ha="left", va="top", style="italic")

# =====================================================================
# RIGHT — rate per 100 000 + formula card
# =====================================================================
ax2.barh(y, rate,
         color=state_colors,
         edgecolor=PALETTE["primary"], linewidth=0.8)
for i, r in enumerate(rate):
    ax2.text(r + max(rate) * 0.02, i,
             f"{r:,.0f}", va="center", ha="left",
             fontsize=10.5, color=PALETTE["primary"], fontweight="bold")

ax2.set_yticks(y)
ax2.set_yticklabels(states, fontsize=10.5)
ax2.invert_yaxis()
ax2.set_xlim(0, max(rate) * 1.30)
ax2.set_xlabel("Rate.Violent  (per 100 000 inhabitants)")
ax2.set_title("After dividing by population — fair comparison",
              fontsize=11, color=PALETTE["primary"])

# Formula card under the right panel
ax2.text(0.02, -0.30,
         r"$\mathtt{Rate.Violent} \,=\, 100{,}000 \cdot "
         r"\dfrac{\mathtt{Assault}+\mathtt{Murder}+\mathtt{Rape}+\mathtt{Robbery}}"
         r"{\mathtt{Population}}$",
         transform=ax2.transAxes,
         ha="left", va="top", fontsize=11,
         color=PALETTE["primary"],
         bbox=dict(boxstyle="round,pad=0.45",
                   facecolor="#fbfbfd",
                   edgecolor=PALETTE["primary"],
                   linewidth=1.0))

# Footer note that links back to the histogram question (1c -> 1e)
fig.text(0.5, 0.005,
         "Same logic as Density = Population / Area in 1b: build the right "
         "per-capita variable before plotting / comparing.",
         ha="center", va="bottom",
         fontsize=9, color=PALETTE["neutral"], style="italic")

plt.tight_layout(rect=[0, 0.04, 1, 1])
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
