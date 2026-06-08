"""Build AI walkthrough plot for Ex 2.5g — histogram of Age with breakpoints
at the five-number summary (16, 40, 46, 54, 96) and identification of the
class with highest density.

The trick: each of the four classes
    [16, 40),  [40, 46),  [46, 54),  [54, 96]
contains by construction about 25% of the data (one quartile of the mass).
Density per class is therefore
    c_k = f_k / w_k  with f_k ~ 0.25,
so the SHORTEST class wins. Widths are
    40-16 = 24,  46-40 = 6,  54-46 = 8,  96-54 = 42
=> [40, 46) is shortest (w = 6) and hence the densest class.

Visual: two-panel figure.

LEFT panel
  Density histogram of Age with breakpoints at (16, 40, 46, 54, 96).
  The [40, 46) bar is highlighted with the accent colour and labelled
  "highest density".

RIGHT panel
  Formula card listing class widths, the ~0.25 relative frequencies, the
  resulting densities, the verdict, and the R command actually used in
  the snippet.

Also uses shutil to clone the recurring Exercise 2.5 textbook prompt PNG
(saved as ex2_5c_question.png — the Ex 2.5 page is shared across sub-parts
5a, 5b, 5c, 5d, 5f, 5g) into ex2/questions/ under the Ex 2.5g filename,
and the shared Ex 2.5 reference answer PNG (ex2_5f_answer.png) into
ex2/answers/ as ex2_5g_answer.png, so the snippet can reference them.
We only reference PNGs we actually copy.
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import pyreadr
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
OUT  = f"{BASE}/ex2_5g_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# ---------- 1. Clone the recurring textbook PNGs via shutil ----------
src_q = f"{BASE}/questions/ex2_5c_question.png"
dst_q = f"{BASE}/questions/ex2_5g_question.png"
src_a = f"{BASE}/answers/ex2_5f_answer.png"
dst_a = f"{BASE}/answers/ex2_5g_answer.png"

copied_q = copied_a = False
if os.path.exists(src_q):
    shutil.copyfile(src_q, dst_q)
    copied_q = True
    print(f"copied -> {dst_q}")
else:
    print(f"WARNING: source question PNG not found at {src_q}")

if os.path.exists(src_a):
    shutil.copyfile(src_a, dst_a)
    copied_a = True
    print(f"copied -> {dst_a}")
else:
    print(f"WARNING: source answer PNG not found at {src_a}")

# ---------- 2. Build the AI walkthrough figure ----------
result = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata")
age = result["customer_habits"]["Age"].to_numpy()

breaks = np.array([16, 40, 46, 54, 96], dtype=float)
n = len(age)

# Class membership w.r.t. the 5-number-summary breakpoints
counts, edges = np.histogram(age, bins=breaks)
widths = np.diff(breaks)                          # [24, 6, 8, 42]
freqs  = counts / counts.sum()                    # ~ [0.25, 0.25, 0.25, 0.25]
dens   = freqs / widths                           # density per class
k_max  = int(np.argmax(dens))                     # which class has max density

print(f"  n={n}")
for k in range(len(widths)):
    print(f"  class [{breaks[k]:.0f}, {breaks[k+1]:.0f})"
          f"  count={counts[k]:5d}  freq={freqs[k]:.4f}"
          f"  width={widths[k]:.0f}  density={dens[k]:.5f}")
print(f"  densest class index = {k_max}  -> "
      f"[{breaks[k_max]:.0f}, {breaks[k_max+1]:.0f})")

# =====================================================================
# Layout
# =====================================================================
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.4),
                               gridspec_kw={"width_ratios": [2.4, 1]})

# =====================================================================
# LEFT — density histogram with 5-number-summary breaks
# =====================================================================
left_edges  = breaks[:-1]
bar_colors  = [PALETTE["secondary"]] * len(widths)
bar_edges   = [PALETTE["primary"]]   * len(widths)
bar_alphas  = [0.55] * len(widths)
# Highlight the winning class
bar_colors[k_max] = PALETTE["accent"]
bar_edges[k_max]  = PALETTE["accent"]
bar_alphas[k_max] = 0.85

bars = ax1.bar(left_edges, dens, width=widths, align="edge",
               color=bar_colors, edgecolor=bar_edges,
               linewidth=1.2)
for b, a in zip(bars, bar_alphas):
    b.set_alpha(a)

ymax = float(dens.max())
ax1.set_ylim(0, ymax * 1.40)

# Annotate each bar with its density and width
for k in range(len(widths)):
    xc = 0.5 * (breaks[k] + breaks[k+1])
    ax1.text(xc, dens[k] + ymax * 0.04,
             fr"$c_{{{k+1}}}={dens[k]:.4f}$" + "\n"
             fr"$w={widths[k]:.0f}$",
             ha="center", va="bottom",
             fontsize=9.2,
             color=(PALETTE["accent"] if k == k_max else PALETTE["primary"]),
             fontweight=("bold" if k == k_max else "normal"))

# Mark the breakpoints
for x, tag in zip(breaks,
                  ["min\n16", r"$Q_1$" + "\n40", "Me\n46",
                   r"$Q_3$" + "\n54", "max\n96"]):
    ax1.axvline(x, color=PALETTE["neutral"], lw=0.8, ls=":")
    ax1.text(x, -ymax * 0.085, tag,
             ha="center", va="top", fontsize=9.5,
             color=PALETTE["primary"], fontweight="bold")

# Arrow pointing at the winning class
xc_max = 0.5 * (breaks[k_max] + breaks[k_max+1])
ax1.annotate("highest\ndensity",
             xy=(xc_max, dens[k_max]),
             xytext=(xc_max + 18, dens[k_max] * 1.15),
             fontsize=10.5, color=PALETTE["accent"], fontweight="bold",
             ha="left", va="center",
             arrowprops=dict(arrowstyle="->", color=PALETTE["accent"], lw=1.4))

ax1.set_xlabel("Age  (years)")
ax1.set_ylabel("Density   $c_k = f_k / w_k$")
ax1.set_title("Ex 2.5g — Age histogram with 5-number-summary breakpoints")
ax1.set_xlim(breaks[0] - 2, breaks[-1] + 2)

# =====================================================================
# RIGHT — verdict / R panel
# =====================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)
ax2.add_patch(FancyBboxPatch((0.02, 0.04), 0.96, 0.92,
                             boxstyle="round,pad=0.02",
                             linewidth=1.1,
                             facecolor="#fbfbfd",
                             edgecolor=PALETTE["primary"]))

txt = (
    r"$\bf{Why\ density\ wins}$"
    "\n"
    r"By construction each class holds"
    "\n"
    r"$f_k \approx 0.25$ (one quartile),"
    "\n"
    r"so $c_k = f_k / w_k \approx 0.25 / w_k$."
    "\n"
    r"$\Rightarrow$ smallest width = densest."
    "\n"
    r"$\bf{Widths}$ (from 5-num. summary)"
    "\n"
    fr"$[16,40)$: $w = {widths[0]:.0f}$,"
    fr" $c \approx {dens[0]:.4f}$"
    "\n"
    fr"$[40,46)$: $w = {widths[1]:.0f}$,"
    fr" $c \approx {dens[1]:.4f}$"
    "\n"
    fr"$[46,54)$: $w = {widths[2]:.0f}$,"
    fr" $c \approx {dens[2]:.4f}$"
    "\n"
    fr"$[54,96]$: $w = {widths[3]:.0f}$,"
    fr" $c \approx {dens[3]:.4f}$"
    "\n"
    r"$\bf{Verdict}$: densest class is"
    "\n"
    fr"$[{breaks[k_max]:.0f},\,{breaks[k_max+1]:.0f})$"
    r"  ($Q_1\to$ Me)."
)
ax2.text(0.06, 0.96, txt, ha="left", va="top", fontsize=10.0,
         color=PALETTE["primary"])

ax2.text(0.06, 0.04,
         'R:  distr.plot.x(Age,\n'
         '       plot.type="histogram",\n'
         '       breaks=c(16,40,46,54,96),\n'
         '       data=customer_habits)',
         ha="left", va="bottom", fontsize=8.6, family="monospace",
         color=PALETTE["neutral"])

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
print(f"copied_q={copied_q}  copied_a={copied_a}")
