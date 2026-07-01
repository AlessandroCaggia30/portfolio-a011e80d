"""AI walkthrough for past-exam Sep-2024 Ex2a — histogram of Score with unequal-width classes.

Compares the specific branch (given percentages: 30/20/30/20 over classes
[0,200)/[200,300)/[300,600)/[600,1000)) with a plausible "main branches" reference
distribution centred more in [300,600).

Two panels:
  Left  : relative frequency bars (misleading — modal class visually [0,200))
  Right : density bars (correct — modal class [200,300); tallest bar)
Also overlays the main-branches density for comparison.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_sep_2024_2a_ai.png"

# Class edges and specific-branch rel freqs from the exam question
edges = np.array([0, 200, 300, 600, 1000], dtype=float)
widths = np.diff(edges)
rel = np.array([0.30, 0.20, 0.30, 0.20])
densities = rel / widths

# Main branches: typical pattern where [300,600) is modal
rel_main = np.array([0.10, 0.15, 0.45, 0.30])
densities_main = rel_main / widths

fig, axes = plt.subplots(1, 2, figsize=(13, 5))

# --- LEFT: raw relative frequency bars (wrong axis) ---
ax = axes[0]
centers = (edges[:-1] + edges[1:]) / 2
ax.bar(edges[:-1], rel, width=widths, align="edge",
       color=PALETTE["muted"], edgecolor=PALETTE["primary"], linewidth=1.4,
       alpha=0.55, label="specific branch: rel. freq. (WRONG axis)")
# Annotate percentages
for e, w, r in zip(edges[:-1], widths, rel):
    ax.text(e + w/2, r + 0.008, f"{int(r*100)}%", ha="center", fontsize=11,
            color=PALETTE["primary"])
ax.set_xticks(edges)
ax.set_xlabel("Score")
ax.set_ylabel("relative frequency")
ax.set_ylim(0, 0.45)
ax.set_title("Wrong: raw % ignores class widths\n"
             "modal *bar* would appear to be [0,200) or [300,600)")
ax.legend(loc="upper right", framealpha=0.95)

# --- RIGHT: correct density bars + main-branches overlay ---
ax2 = axes[1]
# specific branch density
ax2.bar(edges[:-1], densities, width=widths, align="edge",
        color=PALETTE["warn"], edgecolor=PALETTE["primary"], linewidth=1.4,
        alpha=0.6, label="specific branch: density = rel/width")
# main branches density as step outline
edges_step = np.concatenate([edges[:-1], [edges[-1]]])
dens_step = np.concatenate([densities_main, [densities_main[-1]]])
ax2.step(edges_step, dens_step, where="post",
         color=PALETTE["primary"], linewidth=2.2, linestyle="--",
         label="main branches: density (comparison)")
# Highlight modal class of specific branch
modal_idx = int(np.argmax(densities))
ax2.bar(edges[modal_idx], densities[modal_idx], width=widths[modal_idx],
        align="edge", facecolor="none",
        edgecolor=PALETTE["accent"], linewidth=3.0,
        label=f"modal (specific): [{int(edges[modal_idx])},{int(edges[modal_idx+1])})")
# density annotations
for e, w, d in zip(edges[:-1], widths, densities):
    ax2.text(e + w/2, d + 0.00008, f"{d:.4f}", ha="center", fontsize=10,
             color=PALETTE["primary"])
ax2.set_xticks(edges)
ax2.set_xlabel("Score")
ax2.set_ylabel("density")
ax2.set_ylim(0, 0.0028)
ax2.set_title("Correct: y = density → bar areas equal proportions\n"
              "specific-branch mode = [200,300); main-branches mode = [300,600)")
ax2.legend(loc="upper right", framealpha=0.95)

# R-command box
ax2.text(0.02, 0.72,
         "R:\n"
         "distr.plot.x(Score,\n"
         "  plot.type='hist',\n"
         "  breaks=c(0,200,300,600,1000),\n"
         "  data=Credit)\n"
         "# y-axis = Density",
         transform=ax2.transAxes, ha="left", va="bottom",
         fontsize=9, family="monospace",
         bbox=dict(facecolor="#fffbe6",
                   edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

fig.suptitle("Unequal-width histogram: density = rel.freq. / width  "
             "(specific branch mode = [200,300), shifted left vs main branches)",
             fontsize=12, y=1.02, color=PALETTE["primary"])

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"densities: {densities}")
print(f"modal class specific: [{edges[modal_idx]}, {edges[modal_idx+1]})")
