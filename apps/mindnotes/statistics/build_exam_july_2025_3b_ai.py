"""AI walkthrough for Jul-2025 Ex3b — 90% CI for mean Savings, sigma known = 800.
Two panels:
 Left  : Standard normal with central 90% band shaded, z = 1.645 marked.
 Right : CI bar on a number line: [233.68, 316.99] centred at xbar=275.33.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2025_3b_ai.png"

# given quantities
n = 998
xbar = 275.3343
sigma = 800.0
SE = sigma / np.sqrt(n)
z = norm.ppf(0.95)        # 1.6449
ME = z * SE               # 41.6535
lo = xbar - ME
hi = xbar + ME

fig, axes = plt.subplots(1, 2, figsize=(13, 5))

# --- LEFT: N(0,1) with central 90% area ---
ax = axes[0]
x = np.linspace(-4, 4, 800)
pdf = norm.pdf(x)
ax.plot(x, pdf, color=PALETTE["primary"], linewidth=2.2,
        label="$N(0,1)$ — pivot distribution (sigma known)")
mask = (x >= -z) & (x <= z)
ax.fill_between(x[mask], 0, pdf[mask], color=PALETTE["accent"], alpha=0.45,
                label="central 90% area")
ax.axvline(z, color=PALETTE["warn"], linestyle="--", linewidth=1.4)
ax.axvline(-z, color=PALETTE["warn"], linestyle="--", linewidth=1.4)
ax.annotate(f"$z_{{0.95}} = {z:.3f}$",
            xy=(z, norm.pdf(z)), xytext=(z + 0.3, 0.22),
            fontsize=11, color=PALETTE["warn"],
            arrowprops=dict(arrowstyle="->", color=PALETTE["warn"]))
ax.annotate(f"$-z_{{0.95}} = {-z:.3f}$",
            xy=(-z, norm.pdf(-z)), xytext=(-3.95, 0.22),
            fontsize=11, color=PALETTE["warn"],
            arrowprops=dict(arrowstyle="->", color=PALETTE["warn"]))
ax.set_xlabel("standardized statistic")
ax.set_ylabel("density")
ax.set_title("Step 1 — sigma known $\\Rightarrow$ pivot $\\sim N(0,1)$\n"
             f"$z_{{0.95}} = {z:.3f}$ for the 90% confidence level")
ax.set_xlim(-4, 4); ax.set_ylim(0, 0.45)
ax.legend(loc="upper right", framealpha=0.95)

# --- RIGHT: CI on number line ---
ax2 = axes[1]
ax2.axhline(0, color=PALETTE["primary"], linewidth=1.2)
ax2.hlines(0, lo, hi, color=PALETTE["accent"], linewidth=12, alpha=0.55,
           label=f"90% CI = [{lo:.2f}, {hi:.2f}]")
ax2.plot([lo, hi], [0, 0], "|", color=PALETTE["primary"],
         markersize=20, markeredgewidth=2.2)
ax2.plot([xbar], [0], "o", color=PALETTE["warn"], markersize=12,
         label=f"$\\bar x = {xbar:.2f}$")
ax2.text(lo, 0.12, f"{lo:.2f}", ha="center", fontsize=10, color=PALETTE["primary"])
ax2.text(hi, 0.12, f"{hi:.2f}", ha="center", fontsize=10, color=PALETTE["primary"])
ax2.text(xbar, -0.15, f"{xbar:.2f}", ha="center", fontsize=10,
         color=PALETTE["warn"], fontweight="bold")

# Margin-of-error annotation
ax2.annotate("", xy=(hi, 0.55), xytext=(xbar, 0.55),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["primary"], lw=1.4))
ax2.text((xbar + hi) / 2, 0.62, f"$ME = z\\cdot SE = {ME:.2f}$",
         ha="center", va="bottom", fontsize=10.5, color=PALETTE["primary"])

# R formula box
ax2.text(0.5, 0.88,
         "$\\bar x \\pm z_{0.95} \\cdot \\sigma/\\sqrt{n}$\n"
         f"$= {xbar:.2f} \\pm {z:.3f} \\cdot 800/\\sqrt{{{n}}}$\n"
         f"$= {xbar:.2f} \\pm {ME:.2f}$\n"
         f"$= [{lo:.2f},\\;{hi:.2f}]$ \u20ac",
         transform=ax2.transAxes, ha="center", va="top",
         fontsize=11, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Interpretation box
ax2.text(0.02, 0.05,
         "Interpretation:\n"
         "with 90% confidence, the population\n"
         "mean Savings lies between\n"
         f"{lo:.2f}\u20ac and {hi:.2f}\u20ac.\n"
         "(n=998 large \u2192 CLT applies even\nif Savings is not normal)",
         transform=ax2.transAxes, ha="left", va="bottom",
         fontsize=9.5, color=PALETTE["primary"],
         bbox=dict(facecolor="#f4f7fb", edgecolor=PALETTE["primary"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

ax2.set_xlim(180, 380)
ax2.set_ylim(-0.5, 1.1)
ax2.set_yticks([])
ax2.set_xlabel("mean Savings (\u20ac)")
ax2.set_title("Step 2 — 90% CI = $\\bar x \\pm z_{0.95}\\cdot\\sigma/\\sqrt{n}$\n"
              f"$SE = 800/\\sqrt{{{n}}} = {SE:.2f}$")
ax2.legend(loc="lower right", framealpha=0.95)

fig.suptitle("Jul-2025 Ex3b  —  90% CI for mean Savings  "
             f"$= [{lo:.2f},\\;{hi:.2f}]$ \u20ac",
             fontsize=12, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  n={n}, xbar={xbar}, sigma={sigma}, SE={SE:.4f}, z={z:.4f}, ME={ME:.4f}")
print(f"  CI = [{lo:.4f}, {hi:.4f}]")
