"""Build AI walkthrough plot for Ex 1.5d — Ogive of Time and median estimation.

Two panels:
  LEFT  — Full ogive (piecewise-linear cumulative distribution under
          uniform-within-class), with the 50% horizontal line, median class
          [30,60) shaded, and median p50 = 41.01 min located by linear
          interpolation. Each break point (b_i, F_i) is labelled.
  RIGHT — Zoom into the median class [30,60) showing the interpolation
          formula and the geometric construction (delta_F / d_4) that yields
          p50, plus a small recap of the graphical guess (~40 min) vs the
          precise value (~41.01 min), and verbatim R code.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex1/ex1_1_5d_ai.png"

# -----------------------------------------------------------------------------
# Time class data (N = 1800 customers, see TIME_TABLE in build_snippets.py)
# -----------------------------------------------------------------------------
edges = np.array([0, 10, 20, 30, 60, 90, 150], dtype=float)
counts = np.array([122, 420, 294, 176, 571, 217], dtype=float)
N = counts.sum()
rel = counts / N
cum = np.concatenate(([0.0], np.cumsum(rel)))  # cumulative at each edge

# Median class: cum[3]=0.464 < 0.5, cum[4]=0.562 >= 0.5  ->  [30, 60)
L = edges[3]            # 30
F_below = 0.464         # rounded per snippet (= 836/1800 = 0.4644...)
W = edges[4] - edges[3] # 30
f_med = rel[3]          # 0.098
d_med = 0.00327         # rounded per snippet (= 0.098/30)
delta = 0.5 - F_below   # 0.036
p50 = L + delta / d_med # 41.01 with rounded d_med

# -----------------------------------------------------------------------------
# Figure
# -----------------------------------------------------------------------------
fig = plt.figure(figsize=(14, 6.4))
gs = fig.add_gridspec(1, 2, width_ratios=[1.15, 1.0], wspace=0.28)
ax1 = fig.add_subplot(gs[0, 0])
ax2 = fig.add_subplot(gs[0, 1])

# =============================================================================
# LEFT — full ogive with median construction
# =============================================================================
# Shade median class
ax1.axvspan(L, edges[4], color=PALETTE["accent"], alpha=0.25,
            label="median class [30,60)")

# Ogive line
ax1.plot(edges, cum, "-", color=PALETTE["primary"], lw=2.4, zorder=3)
ax1.plot(edges, cum, "o", color=PALETTE["primary"], ms=7, zorder=4)

# Label each break point (b_i, F_i)
for x, y in zip(edges, cum):
    ax1.text(x, y + 0.035, f"({int(x)}, {y:.3f})",
             ha="center", va="bottom", fontsize=9,
             color=PALETTE["primary"])

# 50% horizontal line
ax1.axhline(0.5, color=PALETTE["warn"], lw=1.6, ls="--", alpha=0.9)
ax1.text(edges[-1], 0.5 + 0.018, "F = 0.50  (median)",
         ha="right", va="bottom", fontsize=10,
         color=PALETTE["warn"], fontweight="bold")

# Vertical drop at p50
ax1.plot([p50, p50], [0, 0.5], color=PALETTE["ok"], lw=1.8, ls=":")
ax1.plot(p50, 0.5, "D", color=PALETTE["ok"], ms=9, zorder=5)
ax1.annotate(f"p50 = {p50:.2f} min",
             xy=(p50, 0.5), xytext=(p50 + 22, 0.36),
             fontsize=11, fontweight="bold", color=PALETTE["ok"],
             arrowprops=dict(arrowstyle="->", color=PALETTE["ok"], lw=1.4))
ax1.text(p50, -0.045, f"{p50:.2f}", ha="center", va="top",
         fontsize=10, color=PALETTE["ok"], fontweight="bold")

ax1.set_xlim(-3, 155)
ax1.set_ylim(-0.06, 1.12)
ax1.set_xticks(edges)
ax1.set_xlabel("Time (minutes)")
ax1.set_ylabel("Cumulative relative frequency  F(t)")
ax1.set_title(f"Ogive of Time  (N = {int(N)}, piecewise-linear under uniform-within-class)")
ax1.grid(alpha=0.55)
ax1.legend(loc="lower right", fontsize=9.5, frameon=True)

# =============================================================================
# RIGHT — zoom + linear interpolation
# =============================================================================
ax2.set_xlim(28, 62)
ax2.set_ylim(0.44, 0.58)

# segment within median class
ax2.plot([L, edges[4]], [F_below, cum[4]],
         "-", color=PALETTE["primary"], lw=2.6, zorder=3)
ax2.plot([L, edges[4]], [F_below, cum[4]],
         "o", color=PALETTE["primary"], ms=8, zorder=4)
ax2.text(L, F_below - 0.008, f"(30, {F_below:.3f})",
         ha="left", va="top", fontsize=10, color=PALETTE["primary"])
ax2.text(edges[4], cum[4] + 0.005, f"(60, {cum[4]:.3f})",
         ha="right", va="bottom", fontsize=10, color=PALETTE["primary"])

# 0.5 line + p50 marker
ax2.axhline(0.5, color=PALETTE["warn"], lw=1.6, ls="--", alpha=0.9)
ax2.plot([p50, p50], [F_below, 0.5], color=PALETTE["ok"], lw=2.0, ls=":")
ax2.plot(p50, 0.5, "D", color=PALETTE["ok"], ms=10, zorder=5)
ax2.plot([L, p50], [0.5, 0.5], color=PALETTE["ok"], lw=1.0, ls=":", alpha=0.6)

# Geometric labels: rise = delta, run = p50 - L
ax2.annotate("",
             xy=(L - 0.4, 0.5), xytext=(L - 0.4, F_below),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["ok"], lw=1.4))
ax2.text(L - 1.2, (F_below + 0.5) / 2,
         f"$\\Delta F$ = 0.500 - 0.464\n      = 0.036",
         ha="right", va="center", fontsize=9.5,
         color=PALETTE["ok"], fontweight="bold")

ax2.annotate("",
             xy=(p50, F_below - 0.006), xytext=(L, F_below - 0.006),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["ok"], lw=1.4))
ax2.text((L + p50) / 2, F_below - 0.012,
         f"run = {p50 - L:.2f}",
         ha="center", va="top", fontsize=9.5,
         color=PALETTE["ok"], fontweight="bold")

ax2.text(p50, 0.5 + 0.005, f"p50 = {p50:.2f}",
         ha="left", va="bottom", fontsize=11,
         color=PALETTE["ok"], fontweight="bold")

ax2.set_xticks([30, 35, 40, 45, 50, 55, 60])
ax2.set_xlabel("Time (minutes)  —  zoom of class [30, 60)")
ax2.set_ylabel("F(t)")
ax2.set_title("Linear interpolation inside the median class")
ax2.grid(alpha=0.55)

# Formula box (top-left of right panel)
formula = (
    r"$p_{50} = L + \dfrac{0.5 - F_{<}}{d_{\text{med}}}$" + "\n"
    r"$\quad = 30 + \dfrac{0.036}{0.00327}$" + "\n"
    r"$\quad \approx 41.01\ \mathrm{min}$"
)
ax2.text(0.97, 0.97, formula,
         transform=ax2.transAxes, ha="right", va="top",
         fontsize=10.5, color=PALETTE["primary"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["primary"],
                   boxstyle="round,pad=0.45", linewidth=1.1))

# Recap: graphical vs precise
ax2.text(0.03, 0.05,
         "graphical guess  ~ 40 min\n"
         "interpolated     ~ 41.01 min   (no material difference)",
         transform=ax2.transAxes, ha="left", va="bottom",
         fontsize=9.5, family="monospace",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#fff7e6", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.35", linewidth=1.0))

# Bottom-strip R code beneath both panels
fig.text(0.5, -0.02,
         'R:  distr.plot.x(x=Time, freq="cum", plot.type="cumfreq")   '
         '|   p50 = 30 + (0.5 - 0.464) / (0.098/30)  ->  41.01',
         ha="center", va="top", fontsize=10, family="monospace",
         color=PALETTE["ok"],
         bbox=dict(facecolor="#eaf5ee", edgecolor=PALETTE["ok"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  cum at edges = {[round(c,3) for c in cum.tolist()]}")
print(f"  median class = [{int(L)},{int(edges[4])}), d_med={d_med:.5f}, p50={p50:.4f}")
