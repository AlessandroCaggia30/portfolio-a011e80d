"""AI walkthrough for G1-2024 Ex1.a3 — Use the CI to test H0: diff = 0."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2024_1a3_ai.png"

diff = 32.31; lo, hi = 24.47, 40.16

fig, ax = plt.subplots(figsize=(11, 4.2))
ax.errorbar([diff], [0], xerr=[[diff-lo],[hi-diff]],
            fmt="o", color=PALETTE["primary"], ecolor=PALETTE["primary"],
            elinewidth=3.0, capsize=14, markersize=12,
            label=f"99% CI [{lo}, {hi}]")
ax.axvline(0, ls="--", color=PALETTE["warn"], lw=2.0, label="zero (null value)")
ax.axvspan(lo, hi, color=PALETTE["accent"], alpha=0.25)
ax.axvspan(-5, 0, color=PALETTE["warn"], alpha=0.10)

ax.annotate("CI is entirely > 0\n=> reject $H_0:\\mu_1-\\mu_2=0$\n   at $\\alpha = 0.01$",
            xy=(lo, 0), xytext=(5, 0.6),
            arrowprops=dict(arrowstyle="->", color=PALETTE["ok"], lw=1.7),
            fontsize=11.5, color=PALETTE["ok"], fontweight="bold",
            bbox=dict(facecolor="#eafaf1", edgecolor=PALETTE["ok"],
                      boxstyle="round,pad=0.45", linewidth=1.0))
ax.text(diff, -0.45, f"point estimate = {diff}", ha="center",
        fontsize=10.5, color=PALETTE["primary"])

ax.set_xlim(-5, 50)
ax.set_ylim(-1.0, 1.2)
ax.set_yticks([])
ax.set_xlabel(r"$\mu_{\rm non\text{-}free} - \mu_{\rm free}$  (Read2)")
ax.set_title("G1-2024 Ex1.a3 — Is the difference significantly different from 0?\n"
             "CI / test equivalence: reject $H_0$ at level $\\alpha$  ⇔  $0 \\notin$ (1−α) CI")
ax.legend(loc="upper right", framealpha=0.95)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
