"""Build AI walkthrough plot for Ex 0.1g — approximate P(Frost < 80)
under the uniform-on-interval assumption.

Visual: two-panel figure that makes the uniform-on-interval rule tangible.

LEFT panel
  Density histogram of Frost (counts → density = prop / width).
  - First class [0, 60)  shaded fully (contributes prop = 0.20).
  - Second class [60, 90) shown in full outline; the sub-band [60, 80)
    is shaded to represent the fraction 20/30 of that class.
  - Vertical line at Frost = 80, with annotation showing the
    sub-interval width and the uniform-on-interval logic.

RIGHT panel
  Arithmetic card: the formula, the substitution, and the R command,
  so the reader can connect the picture to the number 0.3066667.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, Rectangle

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex0/ex0_1g_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# Frost distribution (from official tabulation in 1g)
edges  = [0, 60, 90, 120, 150, 180, 200]
counts = [10, 8, 8, 13, 9, 2]
n_tot  = sum(counts)                       # 50
props  = [c / n_tot for c in counts]
widths = [edges[i+1] - edges[i] for i in range(len(counts))]
dens   = [props[i] / widths[i] for i in range(len(counts))]

CUT = 80
# Class containing the cut
k = 1                                       # [60, 90)
frac = (CUT - edges[k]) / widths[k]         # 20/30 = 2/3
partial_prop = props[k] * frac              # 0.16 * 2/3
total = props[0] + partial_prop             # 0.20 + 0.16 * 2/3

print(f"  P(Frost < {CUT}) approx = {props[0]:.4f} + {props[k]:.4f}*({CUT-edges[k]}/{widths[k]})"
      f" = {total:.7f}")

# =====================================================================
# Layout
# =====================================================================
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.4),
                               gridspec_kw={"width_ratios": [2.3, 1]})

# =====================================================================
# LEFT — density histogram with shaded "before 80" region
# =====================================================================
ymax = max(dens) * 1.0
for i in range(len(counts)):
    if i == 0:
        face = PALETTE["secondary"]
        alpha = 0.55
    elif i == k:
        face = "#ffffff"     # outline only; we'll shade the sub-band below
        alpha = 1.0
    else:
        face = PALETTE["secondary"]
        alpha = 0.18
    ax1.add_patch(Rectangle((edges[i], 0), widths[i], dens[i],
                            facecolor=face, alpha=alpha,
                            edgecolor=PALETTE["primary"], linewidth=1.1))

# Shade the sub-band [60, 80) inside class [60, 90)
ax1.add_patch(Rectangle((edges[k], 0), CUT - edges[k], dens[k],
                        facecolor=PALETTE["ok"], alpha=0.45,
                        edgecolor=PALETTE["primary"], linewidth=1.1,
                        hatch="//"))

# Vertical cut line at Frost = 80
ax1.axvline(CUT, color=PALETTE["accent"], lw=2.0, ls="--",
            label=f"cut: Frost = {CUT}")

# Labels for proportions on top of each bar
for i in range(len(counts)):
    ax1.text(edges[i] + widths[i] / 2, dens[i] + ymax * 0.04,
             f"{props[i]:.2f}", ha="center", va="bottom",
             fontsize=9.0, color=PALETTE["primary"])

# Annotation: full first class
ax1.annotate("full class [0,60)\nprop = 0.20",
             xy=(30, dens[0] * 0.55), xytext=(8, dens[0] * 1.55),
             fontsize=9.5, color=PALETTE["primary"],
             arrowprops=dict(arrowstyle="->", color=PALETTE["primary"],
                             lw=1.0, shrinkA=0, shrinkB=2))

# Annotation: partial second class (uniform-on-interval)
ax1.annotate(f"partial [60,80)\nuniform: 20/30 = 2/3\nshare = 0.16 · 2/3 = {partial_prop:.4f}",
             xy=(70, dens[k] * 0.45),
             xytext=(105, dens[k] * 1.85),
             fontsize=9.5, color=PALETTE["ok"],
             arrowprops=dict(arrowstyle="->", color=PALETTE["ok"],
                             lw=1.0, shrinkA=0, shrinkB=2))

ax1.set_xlim(0, 210)
ax1.set_ylim(0, ymax * 2.1)
ax1.set_xlabel("Frost  (days)")
ax1.set_ylabel(r"density  $d_i = f_i / w_i$")
ax1.set_title("Ex 0.1g — Uniform-on-interval approx for P(Frost < 80)")
ax1.legend(loc="upper right", framealpha=0.95, fontsize=9.5)

# Ticks at class boundaries
ax1.set_xticks(edges + [CUT])

# =====================================================================
# RIGHT — arithmetic / formula card
# =====================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)
ax2.add_patch(FancyBboxPatch((0.02, 0.04), 0.96, 0.92,
                             boxstyle="round,pad=0.02",
                             linewidth=1.1,
                             facecolor="#fbfbfd",
                             edgecolor=PALETTE["primary"]))

txt = (
    r"$\bf{Uniform\!-\!on\!-\!interval\ rule}$"
    "\n"
    r"For a class $[a,b)$ with proportion $p$ and"
    "\n"
    r"width $w = b - a$, the share attributed to"
    "\n"
    r"$[a, x)$  with  $a \leq x \leq b$  is"
    "\n"
    r"$\quad p \cdot \dfrac{x - a}{w}.$"
    "\n"
    r"$\bf{Apply\ to\ P(Frost < 80)}$"
    "\n"
    r"$P([0,60)) = 0.20$ (full class)"
    "\n"
    r"$P([60,80)) \approx 0.16 \cdot \dfrac{80-60}{90-60}$"
    "\n"
    r"$\quad = 0.16 \cdot \dfrac{2}{3} = 0.10667$"
    "\n"
    r"$P(\mathrm{Frost} < 80) \approx 0.20 + 0.10667$"
    "\n"
    fr"$\quad = {total:.7f}$"
)
ax2.text(0.06, 0.96, txt, ha="left", va="top", fontsize=10.4,
         color=PALETTE["primary"])

ax2.text(0.06, 0.04,
         'R:  0.20 + 0.16 * 2/3\n'
         '##  [1] 0.3066667\n'
         '    (10 + 8 * 2/3) / 50\n'
         '##  [1] 0.3066667',
         ha="left", va="bottom", fontsize=8.6, family="monospace",
         color=PALETTE["neutral"])

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")

# =====================================================================
# Copy real question & answer PNGs into the canonical ex0/ subfolders
# =====================================================================
import shutil

SRC_DIR = "/Users/Alessandro/Repos/my note taking app/statistics/ex0"
DST_DIR = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex0"

# Real screenshots identified for Ex 0 / 1g:
#   question = full Exercise 1 page (3.47.05 PM) — shared with 1a..1h
#   answer   = solution screenshot for 1g (3.35.15 PM)
# Source filenames use a NARROW NO-BREAK SPACE (\u202f) before "PM",
# the macOS Screenshot default in the user's locale — keep as-is.
NNBSP = "\u202f"
copies = [
    (os.path.join(SRC_DIR, "questions", f"Screenshot 2026-06-05 at 3.47.05{NNBSP}PM.png"),
     os.path.join(DST_DIR, "questions", "ex0_1g_question.png")),
    (os.path.join(SRC_DIR, "answers",   f"Screenshot 2026-06-05 at 3.35.15{NNBSP}PM.png"),
     os.path.join(DST_DIR, "answers",   "ex0_1g_answer.png")),
]
for src, dst in copies:
    if not os.path.isfile(src):
        raise FileNotFoundError(f"Source PNG missing: {src}")
    os.makedirs(os.path.dirname(dst), exist_ok=True)
    shutil.copyfile(src, dst)
    print(f"copied -> {dst}")
