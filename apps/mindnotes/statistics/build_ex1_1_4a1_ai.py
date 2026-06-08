"""Build AI walkthrough plot for Ex 1.4 a1 — Spike plot for Quantity_New.

The exercise: propose a correct graphical representation for `Quantity_New`,
a quantitative DISCRETE variable. The correct plot is the SPIKE diagram, NOT
a bar plot, because the value 5 is unobserved and a bar plot would visually
close the gap between 4 and 6.

The walkthrough has two panels:
  LEFT  — the correct spike plot of Quantity_New (percentages), with the
          unobserved value 5 highlighted as an empty "phantom" tick. Each
          spike is annotated with its frequency and percentage so the user
          can read the distribution directly.
  RIGHT — a side-by-side comparison: bar plot (WRONG — closes the gap at 5)
          vs spike plot (RIGHT — keeps the numerical axis honest). The
          decision rule (discrete numerical -> spike) and verbatim R code
          are shown.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex1/ex1_1_4a1_ai.png"

# -----------------------------------------------------------------------------
# Frequency table for Quantity_New (2022-2023 survey)
# -----------------------------------------------------------------------------
vals    = np.array([1, 2, 3, 4, 6])
counts  = np.array([5401, 7340, 8238, 2561, 700])
N       = int(counts.sum())  # 24240
pct     = counts / N * 100   # 22.3, 30.3, 34.0, 10.6, 2.9

# Full integer axis 1..6 — value 5 has zero count (the "phantom")
all_x   = np.arange(1, 7)
all_pct = np.array([pct[list(vals).index(v)] if v in vals else 0.0 for v in all_x])

# -----------------------------------------------------------------------------
# Figure
# -----------------------------------------------------------------------------
fig = plt.figure(figsize=(14, 6.4))
gs  = fig.add_gridspec(1, 2, width_ratios=[1.25, 1.0], wspace=0.28)
ax1 = fig.add_subplot(gs[0, 0])
ax2 = fig.add_subplot(gs[0, 1])

# =============================================================================
# LEFT — correct spike plot with phantom tick for unobserved value 5
# =============================================================================
for x, p in zip(all_x, all_pct):
    if p > 0:
        ax1.vlines(x, 0, p, color=PALETTE["primary"], lw=3.0, alpha=0.95)
        ax1.plot(x, p, "o", color=PALETTE["primary"], ms=9)
        ax1.text(x, p + 1.1, f"{p:.1f}%",
                 ha="center", va="bottom", fontsize=10.5,
                 fontweight="bold", color=PALETTE["primary"])
    else:
        # phantom marker for x=5 (unobserved)
        ax1.vlines(x, 0, 0.6, color=PALETTE["warn"], lw=2.0,
                   ls=(0, (2, 2)), alpha=0.85)
        ax1.plot(x, 0.0, "x", color=PALETTE["warn"], ms=11, mew=2.2)
        ax1.text(x, 2.4, "no data\n(value 5\nnever seen)",
                 ha="center", va="bottom", fontsize=9.5,
                 color=PALETTE["warn"], style="italic")

# Counts under x-axis
for x, c in zip(vals, counts):
    ax1.text(x, -3.2, f"n={c:,}", ha="center", va="top",
             fontsize=9.3, color=PALETTE["neutral"])

ax1.set_xticks(all_x)
ax1.set_xticklabels([str(v) for v in all_x])
ax1.set_xlim(0.4, 6.6)
ax1.set_ylim(-1.5, max(all_pct) * 1.35)
ax1.set_xlabel("Quantity_New (integer count of items per purchase)")
ax1.set_ylabel("Percentage (%)")
ax1.set_title(f"Spike plot of Quantity_New — discrete numerical  ($N={N:,}$)")
ax1.grid(axis="y", alpha=0.55)

# Subtitle banner
ax1.text(0.5, 0.96,
         "Spikes spaced on a NUMERICAL axis -> distance between 4 and 6 is visible",
         transform=ax1.transAxes, ha="center", va="top",
         fontsize=10.5, color=PALETTE["ok"], fontweight="bold",
         bbox=dict(facecolor="#eaf5ee", edgecolor=PALETTE["ok"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

# =============================================================================
# RIGHT — decision panel + WRONG vs RIGHT comparison + R code
# =============================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)

ax2.text(0.5, 0.98, "Why a SPIKE plot (and not a bar plot)?",
         ha="center", va="top", fontsize=12.5, fontweight="bold",
         color=PALETTE["primary"])

# Decision rule box
ax2.text(0.02, 0.91, "Decision rule by variable type:",
         fontsize=11, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.86,
         "categorical / nominal -> bar plot or pie\n"
         "quantitative DISCRETE -> SPIKE plot  <- here\n"
         "quantitative CONTINUOUS -> histogram",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Inset axes for tiny WRONG bar plot
ax_bar = fig.add_axes([0.66, 0.34, 0.13, 0.22])
ax_bar.bar(range(len(vals)), pct, color=PALETTE["warn"],
           edgecolor="black", alpha=0.85)
ax_bar.set_xticks(range(len(vals)))
ax_bar.set_xticklabels([str(v) for v in vals], fontsize=8)
ax_bar.set_title("Bar plot — WRONG\n(gap '5' is hidden)",
                 fontsize=9.5, color=PALETTE["warn"], fontweight="bold")
ax_bar.tick_params(axis="y", labelsize=8)
ax_bar.set_ylabel("%", fontsize=8)
ax_bar.grid(False)
for s in ax_bar.spines.values():
    s.set_edgecolor(PALETTE["warn"]); s.set_linewidth(1.2)

# Inset axes for tiny RIGHT spike plot
ax_sp = fig.add_axes([0.83, 0.34, 0.13, 0.22])
ax_sp.vlines(vals, 0, pct, color=PALETTE["primary"], lw=2.2)
ax_sp.plot(vals, pct, "o", color=PALETTE["primary"], ms=4)
ax_sp.set_xticks([1, 2, 3, 4, 5, 6])
ax_sp.tick_params(axis="x", labelsize=8)
ax_sp.tick_params(axis="y", labelsize=8)
ax_sp.set_title("Spike — RIGHT\n(gap '5' visible)",
                fontsize=9.5, color=PALETTE["ok"], fontweight="bold")
ax_sp.set_ylabel("%", fontsize=8)
ax_sp.grid(False)
for s in ax_sp.spines.values():
    s.set_edgecolor(PALETTE["ok"]); s.set_linewidth(1.2)

# R code box
ax2.text(0.02, 0.28, "R commands:",
         fontsize=11, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.235,
         'distr.plot.x(x=Quantity_New,\n'
         '             freq="Counts",\n'
         '             plot.type="spike")\n'
         'distr.plot.x(x=Quantity_New,\n'
         '             freq="perc",\n'
         '             plot.type="spike")',
         fontsize=9.5, family="monospace", va="top",
         color=PALETTE["ok"],
         bbox=dict(facecolor="#eaf5ee", edgecolor=PALETTE["ok"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

# Bottom take-away
ax2.text(0.5, 0.03,
         "Distance(4,6)=2 must look TWICE distance(3,4)=1  ->  spike, not bar",
         ha="center", va="bottom",
         fontsize=10.5, fontweight="bold", color=PALETTE["primary"],
         bbox=dict(facecolor=PALETTE["accent"], edgecolor=PALETTE["primary"],
                   boxstyle="round,pad=0.45", linewidth=1.2),
         alpha=0.95)

os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  N={N}, vals={vals.tolist()}, pct={[round(p,2) for p in pct]}")
