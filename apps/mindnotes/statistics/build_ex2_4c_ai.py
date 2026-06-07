"""Build AI walkthrough plot for Ex 2.4c — minimum number of contracts for the
"top" consultants, i.e. the 90th percentile read directly off the ogive
(insurance company, n=2000)."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_4c_ai.png"

# --- Grouped table (reconstructed from the ogive in the prompt) ---
endpoints = np.array([0, 10, 20, 30, 60, 90, 150], dtype=float)
cumfreq   = np.array([0, 200, 400, 800, 1300, 1800, 2000], dtype=float)
n         = 2000
cumprop   = cumfreq / n          # 0, 0.10, 0.20, 0.40, 0.65, 0.90, 1.00

# --- Target percentile: P90 ---
target = 0.90
# direct read: cumprop[5] == 0.90 exactly at endpoint 90, no interpolation needed
P90 = float(np.interp(target, cumprop, endpoints))   # 90

# --- Figure: two panels ---
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.2, 5.2))

# =================================================================
# LEFT: cumulative ogive with the P90 read-off marked
# =================================================================
ax1.plot(endpoints, cumprop, marker="o", color=PALETTE["primary"],
         lw=2.0, markersize=7, markerfacecolor=PALETTE["primary"],
         markeredgecolor="#2a3142", label="Ogive  $F(x)/n$")

# Highlight the segment that contains the 0.90 level (it is exactly at the right endpoint of [60,90])
ax1.plot([60, 90], [0.65, 0.90], color=PALETTE["accent"], lw=3.2,
         alpha=0.85, zorder=4, label="segment hitting $F=0.90$")

# Annotate the cumulative proportions at each endpoint
for x, y in zip(endpoints, cumprop):
    ax1.annotate(f"{y:.2f}", xy=(x, y), xytext=(0, 9),
                 textcoords="offset points", ha="center",
                 fontsize=9.0, color=PALETTE["neutral"])

# Horizontal P90 guide
ax1.axhline(target, color=PALETTE["warn"], lw=1.3, ls="--",
            label="$F = 0.90$ level")
# Vertical at the read-off
ax1.axvline(P90, color=PALETTE["warn"], lw=1.3, ls="--")

# Star marker on the (P90, 0.90) point
ax1.scatter([P90], [target], s=180, marker="*", color=PALETTE["warn"],
            edgecolor="#2a3142", linewidth=0.9, zorder=6,
            label=f"$P_{{90}} = {P90:.0f}$")

# Arrow + label
ax1.annotate(f"$P_{{90}} = {P90:.0f}$\n(direct read — no interpolation)",
             xy=(P90, target), xytext=(P90 - 55, target - 0.18),
             fontsize=10.5, color=PALETTE["warn"], fontweight="bold",
             arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.3))

ax1.set_xlim(-5, 155)
ax1.set_ylim(0, 1.06)
ax1.set_xlabel("Number of contracts")
ax1.set_ylabel("Cumulative relative frequency")
ax1.set_title("Ogive — reading off the 90th percentile")
ax1.legend(loc="lower right", framealpha=0.95, fontsize=9.5)

# =================================================================
# RIGHT: same picture restated as the inverse function look-up
# =================================================================
# Plot horizontal jumps from y=0.90 left to the curve, then drop to the x-axis
ax2.plot(endpoints, cumprop, marker="o", color=PALETTE["primary"],
         lw=2.0, markersize=7, markerfacecolor=PALETTE["primary"],
         markeredgecolor="#2a3142", label="Ogive  $F(x)/n$")

# Step trajectory: (0, 0.90) -> (P90, 0.90) -> (P90, 0)
ax2.plot([0, P90], [target, target], color=PALETTE["warn"], lw=2.0, ls="-")
ax2.plot([P90, P90], [target, 0], color=PALETTE["warn"], lw=2.0, ls="-")
ax2.scatter([P90], [target], s=180, marker="*", color=PALETTE["warn"],
            edgecolor="#2a3142", linewidth=0.9, zorder=6)
ax2.scatter([P90], [0], s=90, marker="v", color=PALETTE["warn"],
            edgecolor="#2a3142", linewidth=0.9, zorder=6,
            label=f"answer: $P_{{90}}={P90:.0f}$")

# Shade the top 10% region on the x-axis
ax2.axvspan(P90, 150, color=PALETTE["accent"], alpha=0.18,
            label="top 10% of consultants")

# Brief textbook recipe inset
recipe = (
    r"Recipe:  $P_{90}$ = value $x$ s.t. $F(x)/n = 0.90$."
    "\n"
    r"From the table: $F(90)/n = 1800/2000 = 0.90$ exactly,"
    "\n"
    r"so no linear interpolation is needed: $P_{90} = 90$."
)
ax2.text(0.02, 0.97, recipe, transform=ax2.transAxes,
         ha="left", va="top", fontsize=10, fontweight="normal",
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

ax2.set_xlim(-5, 155)
ax2.set_ylim(0, 1.06)
ax2.set_xlabel("Number of contracts")
ax2.set_ylabel("Cumulative relative frequency")
ax2.set_title('"Top" consultants = right 10% of the distribution')
ax2.legend(loc="lower right", framealpha=0.95, fontsize=9.5)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  P90 = {P90}")
