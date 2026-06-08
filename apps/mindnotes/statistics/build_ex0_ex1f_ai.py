"""Build the PNG assets for Ex 0 / 1f — % of states with Property.Rates > 3000.

Two responsibilities:

1. Use ``shutil`` to clone the textbook prompt + textbook answer screenshots
   from ``my note taking app/statistics/ex0/{questions,answers}/`` into
   ``portfolio-a011e80d/apps/mindnotes/statistics/images/ex0/{questions,answers}/``
   under the canonical ``ex0_1f_question.png`` / ``ex0_1f_answer.png``
   filenames that the ``ex0["ex1f"]`` snippet references. The source
   filenames contain the narrow-no-break-space (U+202F) that macOS now
   inserts between "at" and the time, so we use the exact unicode literal
   and ``shutil.copyfile`` rather than any shell glob. Only the copies
   that actually succeed are referenced from the snippet.

2. Render an AI walkthrough plot ``ex0_1f_ai.png`` that makes the
   threshold-rule completely concrete. Two panels driven by the actual
   Data_USA dataframe:
     LEFT  -- Histogram of Property.Rates with the 3000 threshold drawn
              as a red vertical line; bars to the right shaded warn-red to
              highlight the 3 states above 3000.
     RIGHT -- Horizontal bar chart of every state's Property.Rates ranked
              descending, with the 3 above-threshold states labelled and
              the 3000 line drawn vertically. The card in the corner shows
              count, percentage, and the underlying R one-liner.
"""
import os, sys, shutil

sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import pyreadr
import matplotlib.pyplot as plt

apply_style()

# ----------------------------------------------------------------------
# 1. Clone the textbook prompt + answer screenshots via shutil
# ----------------------------------------------------------------------
NBSP = "\u202f"   # narrow no-break space used in macOS screenshot filenames

SRC_Q_DIR = "/Users/Alessandro/Repos/my note taking app/statistics/ex0/questions"
SRC_A_DIR = "/Users/Alessandro/Repos/my note taking app/statistics/ex0/answers"
BASE      = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex0"
Q_DIR     = f"{BASE}/questions"
A_DIR     = f"{BASE}/answers"
os.makedirs(Q_DIR, exist_ok=True)
os.makedirs(A_DIR, exist_ok=True)

# The Ex 1 prompt screenshot covers every Ex 1 sub-question (a..l). The
# answer screenshot showing 1f is the one beginning with "f. The variable
# can be built as seen at point d." (Screenshot 3.35.06 PM).
CLONES = [
    (f"{SRC_Q_DIR}/Screenshot 2026-06-05 at 3.47.05{NBSP}PM.png",
     f"{Q_DIR}/ex0_1f_question.png"),
    (f"{SRC_A_DIR}/Screenshot 2026-06-05 at 3.35.06{NBSP}PM.png",
     f"{A_DIR}/ex0_1f_answer.png"),
]

COPIED = []
for src, dst in CLONES:
    if os.path.exists(src):
        shutil.copyfile(src, dst)
        print(f"copied -> {dst}")
        COPIED.append(dst)
    else:
        print(f"WARN: source missing, skipped -> {src}")

# ----------------------------------------------------------------------
# 2. AI walkthrough plot — Property.Rates threshold rule
# ----------------------------------------------------------------------
OUT = f"{BASE}/ex0_1f_ai.png"

# Load Data_USA and compute Property.Rates exactly as the snippet does.
r = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex0/Exe0_Data.Rdata")
df = r["Data_USA"]
prop = 100_000.0 * (df["Property.Burglary"]
                    + df["Property.Larceny"]
                    + df["Property.Motor"]) / df["Population"]
states = df["State"].to_numpy()
rates  = prop.to_numpy()

THRESH = 3000.0
above_mask = rates > THRESH
n_above = int(above_mask.sum())
n_total = int(rates.size)
pct     = 100.0 * n_above / n_total

above_states = states[above_mask]
above_vals   = rates[above_mask]
print(f"Above {THRESH:.0f}: {n_above}/{n_total} = {pct:.2f}%")
for s, v in zip(above_states, above_vals):
    print(f"  {s:<20} {v:8.1f}")

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14.0, 6.0),
                               gridspec_kw={"width_ratios": [1.0, 1.05]})

# =====================================================================
# LEFT — histogram with threshold line, above-threshold bars highlighted
# =====================================================================
bins = np.linspace(rates.min() * 0.95, rates.max() * 1.05, 16)
counts, edges = np.histogram(rates, bins=bins)
widths = np.diff(edges)
centers = (edges[:-1] + edges[1:]) / 2

# Per-bin color: warn-red if the bin's lower edge is at or above the
# threshold, navy otherwise. Bins straddling the threshold get the navy
# color (they are mostly below).
bar_colors = [
    PALETTE["warn"] if edges[i] >= THRESH else PALETTE["primary"]
    for i in range(len(counts))
]
ax1.bar(centers, counts, width=widths * 0.95, color=bar_colors,
        edgecolor=PALETTE["primary"], linewidth=0.8, alpha=0.85)

# Threshold line
ax1.axvline(THRESH, color=PALETTE["warn"], lw=2.0, ls="--", zorder=4)
ax1.text(THRESH, ax1.get_ylim()[1] * 0.94 if ax1.get_ylim()[1] > 0 else 1,
         "  threshold = 3000",
         color=PALETTE["warn"], fontsize=10.5, fontweight="bold",
         ha="left", va="top")

ax1.set_xlabel("Property.Rates (per 100 000 inhabitants)")
ax1.set_ylabel("Number of states")
ax1.set_title("Distribution of Property.Rates across the 51 states",
              fontsize=11.5, color=PALETTE["primary"])

# Caption on the right shoulder summarising the count
ax1.text(0.97, 0.97,
         f"{n_above} states above 3000\n"
         f"=> {pct:.2f}% of {n_total}",
         transform=ax1.transAxes, ha="right", va="top",
         fontsize=10.5, color=PALETTE["warn"], fontweight="bold",
         bbox=dict(boxstyle="round,pad=0.4",
                   facecolor="#fff5f3",
                   edgecolor=PALETTE["warn"], linewidth=1.0))

# =====================================================================
# RIGHT — sorted bar chart with the 3 above-threshold states labelled
# =====================================================================
order = np.argsort(rates)[::-1]
sorted_states = states[order]
sorted_rates  = rates[order]
sorted_above  = above_mask[order]

y = np.arange(n_total)
ax2.barh(y, sorted_rates,
         color=[PALETTE["warn"] if a else PALETTE["primary"] for a in sorted_above],
         edgecolor=PALETTE["primary"], linewidth=0.4, alpha=0.9)

# Vertical threshold
ax2.axvline(THRESH, color=PALETTE["warn"], lw=2.0, ls="--", zorder=4)
ax2.text(THRESH, n_total - 0.5, " 3000 ",
         color=PALETTE["warn"], fontsize=10, fontweight="bold",
         ha="left", va="top",
         bbox=dict(boxstyle="round,pad=0.2", facecolor="white",
                   edgecolor=PALETTE["warn"], linewidth=0.8))

# Annotate the 3 above-threshold states (they are at the top after sort)
for rank in range(n_above):
    nm = sorted_states[rank]
    vl = sorted_rates[rank]
    ax2.text(vl + max(sorted_rates) * 0.012, rank,
             f"{nm}  ({vl:,.0f})",
             color=PALETTE["warn"], fontsize=10, fontweight="bold",
             va="center", ha="left")

ax2.invert_yaxis()
ax2.set_yticks([])
ax2.set_xlim(0, max(sorted_rates) * 1.32)
ax2.set_xlabel("Property.Rates per state (sorted descending)")
ax2.set_title("Only 3 states sit above the 3000 line",
              fontsize=11.5, color=PALETTE["primary"])

# R one-liner card under the plot
ax2.text(0.02, 0.02,
         "R:  mean(Data_USA$Property.Rates > 3000)\n"
         f"    [1] {n_above/n_total:.8f}    # = {pct:.2f}%",
         transform=ax2.transAxes, ha="left", va="bottom",
         fontsize=9.5, family="monospace", color=PALETTE["primary"],
         bbox=dict(boxstyle="round,pad=0.45",
                   facecolor="#fffbe6",
                   edgecolor=PALETTE["accent"], linewidth=1.0))

# Footer
fig.text(0.5, 0.005,
         "Same per-100,000 normalisation trick as Rate.Violent in 1d: "
         "raw counts depend on state size, the rate makes the threshold rule fair.",
         ha="center", va="bottom",
         fontsize=9, color=PALETTE["neutral"], style="italic")

plt.tight_layout(rect=[0, 0.04, 1, 1])
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
COPIED.append(OUT)
print(f"saved -> {OUT}")
print(f"PNGs available for snippet: {COPIED}")
