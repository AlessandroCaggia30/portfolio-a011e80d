"""Build AI walkthrough plot for Ex 0/1b — Density = Population / Area.

Visual concept: a per-state quantity that depends on size (Population) is
made comparable across states by dividing by Area, yielding people per
square mile (Density). The figure shows three coordinated panels using a
small, synthetic but realistic illustrative subset of US states:

  Left  : Population bars (raw counts depend on state size)
  Mid   : Area bars       (state size differs hugely)
  Right : Density bars    (= Population / Area), reveals the actual
          per-square-mile crowding ranking

The right panel highlights how the *ranking* changes once we normalize:
a populous state can have low density (CA, TX) while a small state can
have very high density (NJ, RI).
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex0/ex0_1b_ai.png"

# Illustrative subset (approximate, just to make the point visually).
states     = ["CA",    "TX",    "NY",    "NJ",    "RI"]
population = [39_500, 29_100,  19_500,   9_300,    1_100]   # thousands
area_sqmi  = [163_700, 268_600, 54_500,   8_700,    1_500]  # sq mi
# Density (people per sq mi) = (pop * 1000) / area
density    = [p * 1000.0 / a for p, a in zip(population, area_sqmi)]

x   = np.arange(len(states))
bar = dict(width=0.65, edgecolor=PALETTE["primary"], linewidth=1.1)

fig, axes = plt.subplots(1, 3, figsize=(15, 5))

# --- LEFT: Population (raw)
axes[0].bar(x, population, color=PALETTE["primary"], **bar)
axes[0].set_xticks(x); axes[0].set_xticklabels(states)
axes[0].set_ylabel("Population (thousands)")
axes[0].set_title("Population — raw counts\n(depends on state size)")
for xi, v in zip(x, population):
    axes[0].text(xi, v, f"{v/1000:.1f}M", ha="center", va="bottom",
                 fontsize=10, color=PALETTE["primary"])
axes[0].margins(y=0.18)

# --- MIDDLE: Area
axes[1].bar(x, area_sqmi, color=PALETTE["neutral"], **bar)
axes[1].set_xticks(x); axes[1].set_xticklabels(states)
axes[1].set_ylabel("Area (sq mi)")
axes[1].set_title("Area — state size differs hugely\n(denominator of the ratio)")
for xi, v in zip(x, area_sqmi):
    axes[1].text(xi, v, f"{v/1000:.0f}k", ha="center", va="bottom",
                 fontsize=10, color=PALETTE["neutral"])
axes[1].margins(y=0.18)

# --- RIGHT: Density (the new built variable)
axes[2].bar(x, density, color=PALETTE["warn"], **bar)
axes[2].set_xticks(x); axes[2].set_xticklabels(states)
axes[2].set_ylabel("Density (people / sq mi)")
axes[2].set_title("Density = Population / Area\n(per-area comparable)")
for xi, v in zip(x, density):
    axes[2].text(xi, v, f"{v:,.0f}", ha="center", va="bottom",
                 fontsize=10, color=PALETTE["warn"], fontweight="bold")
axes[2].margins(y=0.22)

# R command box anchored on the right panel
axes[2].text(0.97, 0.97,
             "R command:\n"
             "Data_USA$Density <-\n"
             "  Data_USA$Population /\n"
             "  Data_USA$Area",
             transform=axes[2].transAxes, ha="right", va="top",
             fontsize=10, family="monospace",
             bbox=dict(facecolor="#fffbe6",
                       edgecolor=PALETTE["accent"],
                       boxstyle="round,pad=0.45", linewidth=1.0))

# Caption underlining the key takeaway
fig.suptitle("Building Density makes states comparable: big populous CA/TX "
             "have LOW density; small NJ/RI flip to the TOP after normalizing.",
             fontsize=11, y=1.02, color=PALETTE["primary"])

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
for s, p, a, d in zip(states, population, area_sqmi, density):
    print(f"  {s}: pop={p:>6} (k)  area={a:>7} sqmi  density={d:8.2f} per sqmi")
