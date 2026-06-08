"""Build AI walkthrough plot for Ex 2.6a3 — equal vs custom (decile) breaks
for Revenue from customer_habits.

The question asks: build histograms with 10 equal intervals and with custom
intervals chosen from the deciles (computed in 2.6a1), then comment on what
each representation reveals.

Insight to convey:
  * A density histogram uses height = relative_freq / bin_width, so its
    AREAS (not heights) sum to 1.  When you change the breaks the SAME data
    yields very different-looking bars.
  * 10 equal-width bins on [0.67, 15548] -> each bin is ~1555 USD wide.
    Because Revenue is extremely right-skewed (max = 15548, p90 ~ 2336),
    almost the entire mass (> 95%) falls in the FIRST bin, producing one
    huge bar and 9 near-invisible ones.  The right tail is "visible" only
    because the bins are wide enough to capture it -> good for the tail,
    bad for the bulk.
  * Decile breaks make each bin contain ~10% of the data by construction.
    So bar AREAS are equal (~0.10) but widths differ wildly:
        [0,51]   width 51    -> density ~0.00196
        [51,107] width 56    -> density ~0.00179
        ...
        [2336,15548] width 13212 -> density ~7.6e-6
    The narrow low-revenue bins now show up as very tall thin bars,
    revealing the dense bulk; the wide tail bin becomes a flat sliver,
    summarising the long right tail in a single low bar.
  * The textbook compromise picks intermediate widths, keeping resolution
    where mass concentrates without crushing the tail to invisibility.

Uses shutil to clone Ex 2.6 textbook prompt/answer pages into
ex2/{questions,answers}/ subfolders IF those source pages already exist
on disk; otherwise it skips the copy silently and the snippet does not
reference the missing files (per the "only reference PNGs you actually
copy" rule).
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
OUT  = f"{BASE}/ex2_6a3_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# ---------- 1. Optional shutil clone of recurring Ex 2.6 textbook pages -------
# These source files are only present if the Ex 2.6 textbook page has already
# been digitised into another sub-part (e.g. 2_6a1).  Copy when available;
# never reference a file we did not actually copy.
copied = []
candidates = [
    ("questions/ex2_6a1_question.png", "questions/ex2_6a3_question.png"),
    ("questions/ex2_6a2_question.png", "questions/ex2_6a3_question.png"),
    ("answers/ex2_6a1_answer.png",     "answers/ex2_6a3_answer.png"),
    ("answers/ex2_6a2_answer.png",     "answers/ex2_6a3_answer.png"),
]
for rel_src, rel_dst in candidates:
    src = f"{BASE}/{rel_src}"
    dst = f"{BASE}/{rel_dst}"
    if os.path.exists(src) and not os.path.exists(dst):
        shutil.copyfile(src, dst)
        copied.append(rel_dst)
        print(f"copied -> {dst}")
if not copied:
    print("note: no Ex 2.6 textbook source PNG found; skipping shutil clone")

# ---------- 2. Load Revenue ---------------------------------------------------
result = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata")
ch     = result["customer_habits"].copy()
rev    = ch["Revenue"].to_numpy()
n      = len(rev)
print(f"  n = {n}, min = {rev.min():.2f}, max = {rev.max():.0f}")

# ---------- 3. Decile breaks from the snippet (== distr.summary.x deciles) ----
deciles = np.array([0, 51, 107, 179, 276, 428, 654, 957, 1412, 2336, 15548], dtype=float)

# ---------- 4. Compute densities for both representations ---------------------
# Equal-width: 10 bins from min to max
eq_edges  = np.linspace(rev.min(), rev.max(), 11)
eq_counts, _ = np.histogram(rev, bins=eq_edges)
eq_props  = eq_counts / n
eq_widths = np.diff(eq_edges)
eq_dens   = eq_props / eq_widths

# Decile-based
dc_counts, _ = np.histogram(rev, bins=deciles)
dc_props  = dc_counts / n
dc_widths = np.diff(deciles)
dc_dens   = dc_props / dc_widths

# Sanity prints
print("  EQUAL-WIDTH bins:")
for lo, hi, p in zip(eq_edges[:-1], eq_edges[1:], eq_props):
    print(f"    [{lo:8.1f}, {hi:8.1f}]  p={p:.4f}")
print("  DECILE bins:")
for lo, hi, p, d in zip(deciles[:-1], deciles[1:], dc_props, dc_dens):
    print(f"    [{lo:7.0f}, {hi:7.0f}]  w={hi-lo:6.0f}  p={p:.4f}  c={d:.6f}")

# =============================================================================
# Figure: 1 row, 3 panels
#   (1) equal-width density histogram (linear scale) — bulk collapses to 1 bar
#   (2) decile-based density histogram (focus on lower revenues) — bulk revealed
#   (3) commentary card with the trade-off, areas, and R code
# =============================================================================
fig = plt.figure(figsize=(15.0, 5.6))
gs  = fig.add_gridspec(1, 3, width_ratios=[1.15, 1.15, 1.0])
axA = fig.add_subplot(gs[0, 0])
axB = fig.add_subplot(gs[0, 1])
axC = fig.add_subplot(gs[0, 2])

# ---------- PANEL A: 10 equal-width bins ----
for lo, w, d in zip(eq_edges[:-1], eq_widths, eq_dens):
    axA.bar(lo, d, width=w, align="edge",
            color=PALETTE["primary"], alpha=0.45,
            edgecolor=PALETTE["primary"], linewidth=1.0)
# Annotate the dominating first bar
axA.annotate(f"p $\\approx$ {eq_props[0]:.2f}\n(bulk)",
             xy=(eq_edges[0] + eq_widths[0]/2, eq_dens[0]),
             xytext=(eq_edges[0] + eq_widths[0]/2, eq_dens[0]*1.05),
             ha="center", va="bottom", fontsize=10,
             color=PALETTE["primary"], fontweight="bold")
axA.annotate(f"tail bin\np $\\approx$ {eq_props[-1]:.4f}",
             xy=(eq_edges[-2] + eq_widths[-1]/2, eq_dens[-1]),
             xytext=(eq_edges[-2], eq_dens[0]*0.55),
             ha="center", va="center", fontsize=9,
             color=PALETTE["accent"],
             arrowprops=dict(arrowstyle="->", color=PALETTE["accent"], lw=1.0))
axA.set_xlabel("Revenue (USD)")
axA.set_ylabel("Density  $c_k = p_k / w_k$")
axA.set_title("breaks = 10 equal bins  $\\to$  bulk hides bins")

# ---------- PANEL B: decile-based bins, zoom on [0, 3000] for readability ----
for lo, w, d in zip(deciles[:-1], dc_widths, dc_dens):
    axB.bar(lo, d, width=w, align="edge",
            color=PALETTE["warn"], alpha=0.55,
            edgecolor=PALETTE["primary"], linewidth=1.0)
# Annotate area of EACH bar = 0.10 by construction
axB.text(0.02, 0.96,
         "By construction, each\nbar has area $p_k \\approx 0.10$\n"
         "$\\Rightarrow$ widths differ $\\Rightarrow$ heights vary.",
         transform=axB.transAxes, ha="left", va="top", fontsize=10,
         color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.40", linewidth=1.0))
# Mark the wide tail bin
axB.annotate(f"tail bin [{int(deciles[-2])}, {int(deciles[-1])}]\n"
             f"w = {int(dc_widths[-1])}, c $\\approx$ {dc_dens[-1]:.1e}",
             xy=(deciles[-2], dc_dens[-1]),
             xytext=(1700, dc_dens[2]*0.55),
             ha="left", va="center", fontsize=9,
             color=PALETTE["accent"],
             arrowprops=dict(arrowstyle="->", color=PALETTE["accent"], lw=1.0))
axB.set_xlim(0, 3000)
axB.set_xlabel("Revenue (USD)  —  zoomed to [0, 3000]")
axB.set_ylabel("Density")
axB.set_title("breaks = deciles  $\\to$  bulk revealed, tail compressed")

# ---------- PANEL C: commentary + R ----
axC.axis("off")
axC.set_xlim(0, 1); axC.set_ylim(0, 1)
axC.add_patch(FancyBboxPatch((0.02, 0.04), 0.96, 0.92,
                             boxstyle="round,pad=0.02",
                             linewidth=1.1,
                             facecolor="#fbfbfd",
                             edgecolor=PALETTE["primary"]))

txt = (
    r"$\bf{Density\ histogram\ identity}$"
    "\n"
    r"$c_k = \dfrac{p_k}{w_k},\quad \sum_k p_k = \sum_k c_k w_k = 1$"
    "\n\n"
    r"$\bf{Equal\ widths}$ (panel A)"
    "\n"
    fr"$\quad w \approx {eq_widths[0]:.0f}$, first bin holds $p \approx {eq_props[0]:.2f}$"
    "\n"
    r"   $\Rightarrow$ bulk dominates a single bar;"
    "\n"
    r"   tail bins capture the long right tail."
    "\n\n"
    r"$\bf{Decile\ widths}$ (panel B)"
    "\n"
    r"   $p_k = 0.10$ by construction,"
    "\n"
    r"   so $c_k$ scales as $1/w_k$:"
    "\n"
    fr"$\quad w_1={int(dc_widths[0])}\to c\approx{dc_dens[0]:.4f}$"
    "\n"
    fr"$\quad w_{{10}}={int(dc_widths[-1])}\to c\approx{dc_dens[-1]:.1e}$"
    "\n\n"
    r"$\bf{Trade-off}$"
    "\n"
    r"   equal $\to$ tail detail, bulk lost;"
    "\n"
    r"   decile $\to$ bulk detail, tail flat."
)
axC.text(0.06, 0.97, txt, ha="left", va="top", fontsize=10.0,
         color=PALETTE["primary"])

axC.text(0.06, 0.04,
         'R:  distr.plot.x(Revenue, plot.type="hist",\n'
         '       breaks=10, data=customer_habits)\n'
         '    distr.plot.x(Revenue, plot.type="hist",\n'
         '       breaks=c(0,51,107,179,276,428,\n'
         '               654,957,1412,2336,15548),\n'
         '       data=customer_habits)',
         ha="left", va="bottom", fontsize=8.4, family="monospace",
         color=PALETTE["neutral"])

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
