"""Build AI walkthrough plot for Ex 2.7f — Mean, SD and CV for Nr_visits.

Visual: empirical bar chart of the Nr_visits distribution (proportions f_k)
with the sample mean and mean ± 1 SD overlaid as vertical reference lines.
A side panel reconstructs the computation step by step:
  x_bar = sum f_k * x_k
  var  = sum f_k * x_k^2 - x_bar^2  (with n/(n-1) correction collapsing,
                                     since this is the population-style
                                     formula used in the snippet)
  CV   = SD / x_bar * 100
so the geometry of the bar chart is tied directly to the numerical answer.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_7f_ai.png"

# ---- Distribution of Nr_visits (n = 2200) -------------------------------
x      = np.array([1, 2, 3, 4, 6, 8, 9, 10, 12, 14, 15, 16, 20, 24])
counts = np.array([193, 272, 138, 115, 92, 60, 118, 228, 309, 130, 113, 104, 122, 206])
n      = counts.sum()
prop   = np.round(counts / n, 3)

x_bar  = float(np.sum(x * prop))             # ~ 10.106
var    = float(np.sum(x**2 * prop) - x_bar**2)  # ~ 49.263
sd     = float(np.sqrt(var))                 # ~ 7.019
cv     = sd / x_bar * 100                    # ~ 69.45 %

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 6),
                               gridspec_kw={"width_ratios": [1.1, 1.0]})

# =========================================================================
# LEFT — proportion bars + mean and SD overlay
# =========================================================================
bars = ax1.bar(x, prop, width=0.8,
               color=PALETTE["secondary"], edgecolor=PALETTE["primary"],
               linewidth=1.1, alpha=0.92, label="$f_k$ (proportion)")

# Mean line
ax1.axvline(x_bar, color=PALETTE["primary"], lw=2.2,
            label=f"mean $\\bar x = {x_bar:.3f}$")
# Mean +/- SD band
ax1.axvspan(x_bar - sd, x_bar + sd,
            color=PALETTE["accent"], alpha=0.18,
            label=f"$\\bar x \\pm \\sigma$  ($\\sigma = {sd:.3f}$)")
ax1.axvline(x_bar - sd, color=PALETTE["accent"], lw=1.4, ls="--")
ax1.axvline(x_bar + sd, color=PALETTE["accent"], lw=1.4, ls="--")

# Annotate mean and SD endpoints
y_top = prop.max() * 1.18
ax1.annotate(f"$\\bar x = {x_bar:.3f}$",
             xy=(x_bar, y_top * 0.78),
             xytext=(x_bar + 0.6, y_top * 0.94),
             color=PALETTE["primary"], fontsize=11, fontweight="bold",
             arrowprops=dict(arrowstyle="-", color=PALETTE["primary"], lw=1.0))
ax1.annotate(f"$\\bar x - \\sigma$\n= {x_bar - sd:.2f}",
             xy=(x_bar - sd, y_top * 0.55),
             xytext=(x_bar - sd - 4.6, y_top * 0.78),
             color=PALETTE["accent"], fontsize=10, fontweight="bold",
             ha="left",
             arrowprops=dict(arrowstyle="->", color=PALETTE["accent"], lw=1.0))
ax1.annotate(f"$\\bar x + \\sigma$\n= {x_bar + sd:.2f}",
             xy=(x_bar + sd, y_top * 0.55),
             xytext=(x_bar + sd + 1.2, y_top * 0.78),
             color=PALETTE["accent"], fontsize=10, fontweight="bold",
             ha="left",
             arrowprops=dict(arrowstyle="->", color=PALETTE["accent"], lw=1.0))

# CV badge in the upper-right corner
ax1.text(0.98, 0.97,
         f"CV = $\\sigma / \\bar x \\cdot 100$\n   = {cv:.2f}%  (> 30%)\n   -> high dispersion",
         transform=ax1.transAxes, ha="right", va="top",
         fontsize=11, fontweight="bold", color=PALETTE["warn"],
         bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.5", linewidth=1.1))

ax1.set_xlabel("Nr_visits ($x_k$)")
ax1.set_ylabel("Proportion $f_k$")
ax1.set_title(f"Distribution of Nr_visits ($n = {n}$) — mean and $\\pm 1\\sigma$ band")
ax1.set_xticks(x)
ax1.set_ylim(0, y_top)
ax1.grid(axis="y", alpha=0.5)
ax1.legend(loc="upper left", framealpha=0.95)

# =========================================================================
# RIGHT — step-by-step construction of mean, SD and CV
# =========================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)

ax2.text(0.5, 0.965,
         "How $\\bar x$, $\\sigma$ and CV are built from $(x_k, f_k)$",
         ha="center", va="top", fontsize=12.5, fontweight="bold",
         color=PALETTE["primary"])

# Step 1 — distribution table
ax2.text(0.02, 0.90, "1. Frequency distribution (proportions)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.84,
         "x_k     : 1   2   3   4   6   8   9  10  12  14  15  16  20  24\n"
         "f_k(%)  : 8.8 12.4 6.3 5.2 4.2 2.7 5.4 10.4 14.0 5.9 5.1 4.7 5.5 9.4\n"
         f"n = sum(counts) = {n}",
         fontsize=9.5, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 2 — mean
ax2.text(0.02, 0.66, "2. Sample mean",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.605,
         "x_bar = sum_k f_k * x_k\n"
         f"      = 0.088*1 + 0.124*2 + ... + 0.094*24\n"
         f"      = {x_bar:.3f}",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 3 — variance and SD
ax2.text(0.02, 0.44, "3. Variance and standard deviation",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.385,
         "Var = sum_k f_k * x_k^2  -  x_bar^2\n"
         f"    = {float(np.sum(x**2 * prop)):.3f}  -  {x_bar**2:.3f}\n"
         f"    = {var:.3f}\n"
         f"sigma = sqrt(Var) = {sd:.3f}",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# Step 4 — CV verdict
ax2.text(0.02, 0.20, "4. Coefficient of variation",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.145,
         f"CV = sigma / x_bar * 100  =  {sd:.3f} / {x_bar:.3f} * 100\n"
         f"   = {cv:.2f}%\n"
         "Rule of thumb: CV > 30%  =>  very dispersed data\n"
         "  -> sigma is ~70% of the mean: heavy tails confirmed\n"
         "     by the boxplot in 2.7e.",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["warn"],
         bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  n={n}, x_bar={x_bar:.4f}, var={var:.4f}, sd={sd:.4f}, cv={cv:.4f}%")
