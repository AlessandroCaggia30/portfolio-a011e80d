"""AI walkthrough for past-exam G1-2026 Ex1c — sample size for 99% CI width <= 0.09.

Concept: width = 2 * z * sqrt(p*(1-p)/n) <= 0.09  =>  ME <= 0.045.
Worst-case at p = 0.5; required n = ceiling((z_{0.995} * 0.5 / ME)^2).

Two panels:
  Left  : p*(1-p) curve vs p, peak at 0.5 highlighted (worst-case).
  Right : required n vs ME (log-y), vertical line at ME=0.045 with n=820 callout.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2026_1c_ai.png"

z = norm.ppf(0.995)  # 2.5758
ME_target = 0.045
p_worst = 0.5
n_req = int(np.ceil((z * p_worst / ME_target) ** 2))  # 820

fig, axes = plt.subplots(1, 2, figsize=(13, 5))

# --- LEFT: p*(1-p) parabola, peak at p=0.5 ---
p = np.linspace(0, 1, 400)
var = p * (1 - p)
axes[0].plot(p, var, color=PALETTE["primary"], linewidth=2.2)
axes[0].fill_between(p, 0, var, color=PALETTE["primary"], alpha=0.10)
axes[0].axvline(0.5, color=PALETTE["warn"], linestyle="--", linewidth=1.4)
axes[0].scatter([0.5], [0.25], s=110, color=PALETTE["warn"], zorder=5,
                edgecolor=PALETTE["primary"], linewidth=1.2)
axes[0].annotate("worst-case\n$p(1-p)=0.25$",
                 xy=(0.5, 0.25), xytext=(0.72, 0.20),
                 fontsize=11, color=PALETTE["primary"],
                 arrowprops=dict(arrowstyle="->", color=PALETTE["primary"]))
axes[0].set_xlabel("$p$")
axes[0].set_ylabel("$p(1-p)$")
axes[0].set_title("Step 1 — pick worst-case variance\n(unknown $p$ $\\Rightarrow$ use $p=0.5$)")
axes[0].set_xlim(0, 1)
axes[0].set_ylim(0, 0.30)

# --- RIGHT: required n vs ME, log-y, mark ME=0.045 ---
me_grid = np.linspace(0.01, 0.10, 400)
n_grid = np.ceil((z * 0.5 / me_grid) ** 2)
axes[1].plot(me_grid, n_grid, color=PALETTE["primary"], linewidth=2.2,
             label=r"$n = \lceil (z\cdot 0.5 / ME)^2 \rceil$")
axes[1].set_yscale("log")
axes[1].axvline(ME_target, color=PALETTE["warn"], linestyle="--", linewidth=1.4,
                label=f"ME = 0.045  (width 0.09)")
axes[1].scatter([ME_target], [n_req], s=130, color=PALETTE["warn"], zorder=5,
                edgecolor=PALETTE["primary"], linewidth=1.2)
axes[1].annotate(f"n = {n_req}",
                 xy=(ME_target, n_req), xytext=(0.058, 1900),
                 fontsize=12, color=PALETTE["primary"], fontweight="bold",
                 arrowprops=dict(arrowstyle="->", color=PALETTE["primary"]))
axes[1].set_xlabel("Margin of error  $ME = \\mathrm{width}/2$")
axes[1].set_ylabel("Required sample size  $n$  (log)")
axes[1].set_title("Step 2 — invert the CI-width formula\n$n \\geq (z_{0.995}\\cdot 0.5 / ME)^2$")
axes[1].legend(loc="upper right", framealpha=0.95)
axes[1].grid(True, which="both", alpha=0.25)

# R command box anchored on the right panel
axes[1].text(0.03, 0.05,
             "R command:\n"
             "ceiling(\n"
             "  (qnorm(0.995)*0.5/0.045)^2\n"
             ")\n# = 820",
             transform=axes[1].transAxes, ha="left", va="bottom",
             fontsize=10, family="monospace",
             bbox=dict(facecolor="#fffbe6",
                       edgecolor=PALETTE["accent"],
                       boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle(f"99% CI width $\\leq 0.09$  $\\Rightarrow$  ME $\\leq 0.045$  with $p=0.5$  "
             f"$\\Rightarrow$  minimum  n = {n_req}",
             fontsize=12, y=1.02, color=PALETTE["primary"])

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  z_0.995 = {z:.4f}   ME = {ME_target}   p_worst = {p_worst}   n_req = {n_req}")
