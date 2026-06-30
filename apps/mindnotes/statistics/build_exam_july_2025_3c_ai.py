"""AI walkthrough for Jul-2025 Ex3c — Margin of error and required sample size
to reduce ME below 35. Two panels:
  Left  : Required n* vs target ME, with ME=35 marked giving n* = 1414.
  Right : ME shrinks like 1/sqrt(n) — annotated.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2025_3c_ai.png"

sigma = 800.0
z = norm.ppf(0.95)        # 1.6449
n_obs = 998
ME_obs = z * sigma / np.sqrt(n_obs)
ME_target = 35.0
n_star = (z * sigma / ME_target) ** 2
n_star_ceil = int(np.ceil(n_star))

fig, axes = plt.subplots(1, 2, figsize=(13, 5.3))

# --- LEFT: required n vs target ME ---
ax = axes[0]
me_grid = np.linspace(20, 60, 400)
n_req = (z * sigma / me_grid) ** 2
ax.plot(me_grid, n_req, color=PALETTE["primary"], linewidth=2.2,
        label=r"$n^*(ME) = \left(z_{0.95}\sigma/ME\right)^2$")
ax.axvline(ME_target, color=PALETTE["warn"], linestyle="--", linewidth=1.6,
           label=f"target $ME = {ME_target:.0f}$")
ax.axhline(n_star_ceil, color=PALETTE["accent"], linestyle="--", linewidth=1.6,
           label=f"$n^* \\approx {n_star_ceil}$")
ax.scatter([ME_target], [n_star], color=PALETTE["warn"], s=80, zorder=6)
ax.scatter([ME_obs], [n_obs], color=PALETTE["ok"], s=80, zorder=6,
           label=f"observed: $n={n_obs}$, $ME\\approx{ME_obs:.2f}$")
ax.annotate(f"$n^* = ({z:.3f}\\cdot 800 / {ME_target:.0f})^2 \\approx {n_star:.1f}\n\\Rightarrow$ round up to {n_star_ceil}",
            xy=(ME_target, n_star), xytext=(ME_target + 6, n_star + 200),
            fontsize=10.5, color=PALETTE["warn"], fontweight="bold",
            arrowprops=dict(arrowstyle="->", color=PALETTE["warn"]))
ax.set_xlabel("desired ME (\u20ac)")
ax.set_ylabel("required sample size $n^*$")
ax.set_title(f"Step 1 — invert $ME = z\\,\\sigma/\\sqrt{{n}}$ to get $n^* = (z\\,\\sigma/ME)^2$\n"
             f"observed $ME = z\\cdot\\sigma/\\sqrt{{998}} \\approx {ME_obs:.2f}$")
ax.set_xlim(20, 60)
ax.set_ylim(0, 4500)
ax.legend(loc="upper right", framealpha=0.95)

# --- RIGHT: ME(n) -- shrinking curve, log-x ---
ax2 = axes[1]
n_grid = np.linspace(200, 4000, 600)
me_curve = z * sigma / np.sqrt(n_grid)
ax2.plot(n_grid, me_curve, color=PALETTE["primary"], linewidth=2.2,
         label=r"$ME(n) = z_{0.95}\,\sigma/\sqrt{n}$")
ax2.axhline(ME_target, color=PALETTE["warn"], linestyle="--", linewidth=1.6,
            label=f"target $ME = {ME_target:.0f}$")
ax2.axvline(n_star_ceil, color=PALETTE["accent"], linestyle="--", linewidth=1.6,
            label=f"$n^* = {n_star_ceil}$")
ax2.scatter([n_obs], [ME_obs], color=PALETTE["ok"], s=80, zorder=6,
            label=f"current sample $n={n_obs}$")
ax2.scatter([n_star_ceil], [z * sigma / np.sqrt(n_star_ceil)],
            color=PALETTE["warn"], s=80, zorder=6)
ax2.set_xlabel("sample size $n$")
ax2.set_ylabel("Margin of Error $ME$ (\u20ac)")
ax2.set_title(f"Step 2 — current $n=998$ gives $ME \\approx {ME_obs:.2f}$\n"
              f"need $n \\geq {n_star_ceil}$ to drop $ME$ below {ME_target:.0f}")
ax2.set_xlim(200, 4000)
ax2.set_ylim(15, 60)
ax2.legend(loc="upper right", framealpha=0.95)

# R box
ax2.text(0.97, 0.08,
         "R reconstruction:\n"
         f"z <- qnorm(0.95)      # {z:.4f}\n"
         f"sigma <- 800\n"
         f"ME_target <- 35\n"
         f"n_star <- (z*sigma/ME_target)^2  # {n_star:.2f}\n"
         f"ceiling(n_star)                  # {n_star_ceil}",
         transform=ax2.transAxes, ha="right", va="bottom",
         fontsize=9, family="monospace",
         bbox=dict(facecolor="#f4f7fb", edgecolor=PALETTE["primary"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

fig.suptitle(f"Jul-2025 Ex3c  —  current ME \u2248 {ME_obs:.2f}\u20ac; "
             f"need n* = {n_star_ceil} clients to get ME \u2264 {ME_target:.0f}\u20ac",
             fontsize=12, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  z={z:.4f}, ME_obs={ME_obs:.4f}, n*={n_star:.2f}, ceil={n_star_ceil}")
