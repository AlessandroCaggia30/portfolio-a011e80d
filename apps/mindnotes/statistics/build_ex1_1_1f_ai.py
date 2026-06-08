"""Build AI walkthrough plot for Ex 1.1f — central tendency for District (mode).

District is a qualitative NOMINAL variable. Only the mode is meaningful:
arithmetic mean is undefined (no numerical scale) and the median is undefined
(no canonical ordering). The walkthrough shows:

  LEFT  — bar chart of District counts (Lodi 35, Milano 33, Pavia 32) with the
          modal class (Lodi) highlighted in accent yellow. The count and
          percentage are annotated above each bar; the modal bar is labelled
          "mode".
  RIGHT — a logic panel listing which central-tendency measures are
          admissible for each variable type, why mean/median are undefined for
          nominal data, and verbatim R code computing the mode via
          `table(District)` and `names(which.max(table(District)))`.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex1/ex1_1_1f_ai.png"

# -----------------------------------------------------------------------------
# Real counts from pizzerie.Rdata District column (verified: 35 / 33 / 32)
# -----------------------------------------------------------------------------
districts = ["Lodi", "Milano", "Pavia"]
counts    = [35, 33, 32]
n         = sum(counts)
pcts      = [c / n * 100 for c in counts]
mode_idx  = int(np.argmax(counts))

# -----------------------------------------------------------------------------
# Figure
# -----------------------------------------------------------------------------
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 6),
                               gridspec_kw={"width_ratios": [1.0, 1.1]})

# =============================================================================
# LEFT — bar chart of District with mode highlighted
# =============================================================================
bar_colors = [PALETTE["secondary"]] * len(districts)
bar_colors[mode_idx] = PALETTE["accent"]
edge_colors = [PALETTE["primary"]] * len(districts)

bars = ax1.bar(districts, counts, color=bar_colors,
               edgecolor=edge_colors, linewidth=1.2, alpha=0.92, width=0.62)

# Annotate count + percentage on top of each bar
for i, (b, c, p) in enumerate(zip(bars, counts, pcts)):
    ax1.text(b.get_x() + b.get_width() / 2, c + 0.7,
             f"{c}\n({p:.0f}%)",
             ha="center", va="bottom", fontsize=11,
             fontweight="bold" if i == mode_idx else "normal",
             color=PALETTE["primary"])

# Tag the modal class
mb = bars[mode_idx]
ax1.annotate("mode",
             xy=(mb.get_x() + mb.get_width() / 2, counts[mode_idx]),
             xytext=(mb.get_x() + mb.get_width() / 2, counts[mode_idx] + 7.5),
             ha="center", fontsize=11.5, fontweight="bold",
             color=PALETTE["primary"],
             arrowprops=dict(arrowstyle="->", color=PALETTE["primary"], lw=1.4),
             bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                       boxstyle="round,pad=0.3", linewidth=1.1))

ax1.set_xlabel("District (qualitative — nominal)")
ax1.set_ylabel("Count")
ax1.set_title(f"District frequencies  ($n={n}$) — modal class shaded")
ax1.set_ylim(0, max(counts) * 1.45)
ax1.grid(axis="y", alpha=0.5)

# Footer caption inside the panel: mode poorly representative
ax1.text(0.5, -0.18,
         "Mode = Lodi (35%) — but only 35% of obs, so it is poorly representative",
         transform=ax1.transAxes, ha="center", va="top",
         fontsize=10, style="italic", color=PALETTE["muted"])

# =============================================================================
# RIGHT — central-tendency-by-type panel
# =============================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)

ax2.text(0.5, 0.975,
         "Which central-tendency measure for District?",
         ha="center", va="top", fontsize=12.5, fontweight="bold",
         color=PALETTE["primary"])

# Step 1 — admissibility table
ax2.text(0.02, 0.905, "1. Admissibility by variable type",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.855,
         "type                 mode  median  mean\n"
         "qualitative nominal   YES    no     no\n"
         "qualitative ordinal   YES   YES     no\n"
         "quantitative          YES   YES    YES\n"
         "District -> nominal  ->  only the mode is defined",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 2 — why mean/median are undefined
ax2.text(0.02, 0.605, "2. Why mean & median are undefined here",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.555,
         "mean   needs +, / on the values  -> Lodi+Milano = ?\n"
         "median needs a canonical order   -> Lodi < Milano?\n"
         "mode   needs only equality (=)   -> always defined",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["warn"],
         bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 3 — verbatim R
ax2.text(0.02, 0.355, "3. R code — compute the mode",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.305,
         "table(District)\n"
         "##   Lodi Milano  Pavia\n"
         "##     35     33     32\n"
         "names(which.max(table(District)))\n"
         "## [1] \"Lodi\"",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["ok"],
         bbox=dict(facecolor="#eaf5ee", edgecolor=PALETTE["ok"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 4 — final answer
ax2.text(0.02, 0.075,
         "-> Answer:  mode = Lodi  (35/100 = 35%)  — poorly representative",
         fontsize=11, fontweight="bold", color=PALETTE["primary"],
         bbox=dict(facecolor=PALETTE["accent"], edgecolor=PALETTE["primary"],
                   boxstyle="round,pad=0.5", linewidth=1.2),
         alpha=0.95)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  n={n}, mode=Lodi ({counts[0]}), Milano={counts[1]}, Pavia={counts[2]}")
