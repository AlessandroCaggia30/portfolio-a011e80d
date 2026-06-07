"""Build AI walkthrough plot for Ex 2.2a — proportion P(X > 14) from grouped data
(weekly hours of access to devices, n=300 primary-school pupils).

Visual: density histogram of the grouped distribution with the tail area
{X > 14} shaded.  Annotations split the shaded area into three pieces that
add up to 0.77:
  - partial slice of class [10,25) above 14: width (25-14)=11, density 0.020 -> 0.220
  - whole class [25,30):  p = 0.250
  - whole class [30,40]:  p = 0.300
A side panel shows the arithmetic and the equivalent "complement" computation
P(X<14) = 0.23, illustrating the uniform-on-interval discretization assumption.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_2a_ai.png"

# --- Grouped table from the prompt ---
lower    = np.array([0.0, 10.0, 25.0, 30.0])
upper    = np.array([10.0, 25.0, 30.0, 40.0])
freq     = np.array([45,   90,   75,   90], dtype=float)
n        = int(freq.sum())            # 300
p        = freq / freq.sum()          # 0.15, 0.30, 0.25, 0.30
width    = upper - lower              # 10, 15, 5, 10
dens     = p / width                  # 0.015, 0.020, 0.050, 0.030

THRESH   = 14.0

# --- Tail contributions: P(X > 14) ---
slice_w    = upper[1] - THRESH        # 25 - 14 = 11
slice_d    = dens[1]                  # 0.020
slice_a    = slice_w * slice_d        # 0.220   partial slice of [10,25)
p_2530     = p[2]                     # 0.250
p_3040     = p[3]                     # 0.300
tail       = slice_a + p_2530 + p_3040  # 0.770

# --- Complement: P(X < 14) ---
left_w     = THRESH - lower[1]        # 14 - 10 = 4
left_a     = left_w * dens[1]         # 0.080
p_010      = p[0]                     # 0.150
head       = p_010 + left_a           # 0.230

assert abs(tail + head - 1.0) < 1e-9

print(f"  P(X>14) tail slice  = {slice_a:.3f}  (width {slice_w} * density {slice_d})")
print(f"  P(X>14) total       = {tail:.3f}")
print(f"  P(X<14) total       = {head:.3f}")

# =====================================================================
# Figure layout: histogram on left, arithmetic card on right
# =====================================================================
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.4),
                               gridspec_kw={"width_ratios": [2.4, 1]})

# =====================================================================
# LEFT — density histogram with tail shaded
# =====================================================================
# Light grey base bars (for the "below threshold" portion of class [10,25))
for lo, hi, d in zip(lower, upper, dens):
    ax1.bar(lo, d, width=hi - lo, align="edge",
            color=PALETTE["primary"], alpha=0.18,
            edgecolor=PALETTE["primary"], linewidth=1.2)

# Shaded tail above 14: partial slice of [10,25) + classes [25,30) + [30,40]
# Partial slice
ax1.bar(THRESH, dens[1], width=upper[1] - THRESH, align="edge",
        color=PALETTE["warn"], alpha=0.55,
        edgecolor=PALETTE["primary"], linewidth=1.4,
        hatch="//")
# Whole classes
ax1.bar(lower[2], dens[2], width=width[2], align="edge",
        color=PALETTE["warn"], alpha=0.55,
        edgecolor=PALETTE["primary"], linewidth=1.4,
        hatch="//")
ax1.bar(lower[3], dens[3], width=width[3], align="edge",
        color=PALETTE["warn"], alpha=0.55,
        edgecolor=PALETTE["primary"], linewidth=1.4,
        hatch="//")

# Threshold vertical line at x = 14
ymax_left = dens.max() * 1.45
ax1.axvline(THRESH, color=PALETTE["accent"], lw=2.2, ls="-",
            label="threshold $x = 14$")
ax1.text(THRESH, ymax_left * 0.97, "  $x = 14$",
         color=PALETTE["accent"], fontsize=11, ha="left", va="top",
         fontweight="bold")

# Area labels on each tail piece
ax1.annotate(f"area = {slice_a:.2f}\n$(25-14)\\cdot 0.020$",
             xy=((THRESH + upper[1]) / 2, dens[1] / 2),
             ha="center", va="center", fontsize=9.5,
             color="#5b4a00", fontweight="bold")
ax1.annotate(f"$p={p_2530:.2f}$",
             xy=((lower[2] + upper[2]) / 2, dens[2] / 2),
             ha="center", va="center", fontsize=10,
             color="#5b4a00", fontweight="bold")
ax1.annotate(f"$p={p_3040:.2f}$",
             xy=((lower[3] + upper[3]) / 2, dens[3] / 2),
             ha="center", va="center", fontsize=10,
             color="#5b4a00", fontweight="bold")

# Density tags on top of each bar
for lo, hi, d in zip(lower, upper, dens):
    ax1.text((lo + hi) / 2, d + ymax_left * 0.025,
             f"$c={d:.3f}$",
             ha="center", va="bottom", fontsize=9.5,
             color=PALETTE["neutral"])

# Total annotation
ax1.text(32, ymax_left * 0.72,
         f"$P(X>14) = {tail:.2f}$",
         ha="center", va="center", fontsize=12.5,
         color=PALETTE["primary"], fontweight="bold",
         bbox=dict(facecolor="#fffbe6",
                   edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.2))

ax1.set_xlim(-1.5, 41.5)
ax1.set_ylim(0, ymax_left)
ax1.set_xlabel("Hours of access to devices per week")
ax1.set_ylabel("Density  $c_k = p_k / w_k$")
ax1.set_title("Ex 2.2a — Tail area $P(X>14)$ under the grouped distribution")
ax1.set_xticks([0, 10, 14, 25, 30, 40])
ax1.legend(loc="upper left", framealpha=0.95, fontsize=10)

# =====================================================================
# RIGHT — arithmetic / R panel
# =====================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)
ax2.add_patch(FancyBboxPatch((0.02, 0.04), 0.96, 0.92,
                             boxstyle="round,pad=0.02",
                             linewidth=1.1,
                             facecolor="#fbfbfd",
                             edgecolor=PALETTE["primary"]))

txt = (
    r"$\bf{Two\ equivalent\ routes}$"
    "\n\n"
    r"$\bf{(i)\ Tail\ directly:}$"
    "\n"
    r"$P(X>14) = (25-14)\,c_{[10,25)} + p_{[25,30)} + p_{[30,40]}$"
    "\n"
    fr"$\quad = 11\cdot 0.020 + 0.25 + 0.30$"
    "\n"
    fr"$\quad = {slice_a:.2f} + 0.25 + 0.30 = {tail:.2f}$"
    "\n\n"
    r"$\bf{(ii)\ Via\ the\ complement:}$"
    "\n"
    r"$P(X<14) = p_{[0,10)} + (14-10)\,c_{[10,25)}$"
    "\n"
    fr"$\quad = 0.15 + 4\cdot 0.020 = {head:.2f}$"
    "\n"
    fr"$P(X>14) = 1 - {head:.2f} = {tail:.2f}$"
    "\n\n"
    r"$P(X = 14) = 0$ (continuous-style)"
)
ax2.text(0.06, 0.96, txt, ha="left", va="top", fontsize=10.0,
         color=PALETTE["primary"])

ax2.text(0.06, 0.04,
         'R:  p   <- c(0.15, 0.30, 0.25, 0.30)\n'
         '    dens<- c(0.015,0.020,0.050,0.030)\n'
         '    (25-14)*dens[2] + p[3] + p[4]\n'
         '##  [1] 0.77',
         ha="left", va="bottom", fontsize=8.6, family="monospace",
         color=PALETTE["neutral"])

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
