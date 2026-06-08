"""Build AI theory diagram for G6.A — Quantiles and percentiles.

Two-panel figure:
LEFT  — ECDF/ogive of grouped data with horizontal read-offs at
        q = 0.25, 0.50, 0.75 dropping vertical lines to the x-axis
        to expose p_25, p_50, p_75. Median class is highlighted and
        the linear-interpolation formula is annotated.
RIGHT — Sorted-data definition card: the n=12 sample in rank order,
        the empirical CDF F_k = k/n column, and the smallest-k rule
        used for the discrete definition.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, Rectangle

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/theory/th_g6a_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# ----- Grouped data for the ogive (left panel) -----
edges = np.array([0, 10, 20, 30, 40, 50, 60], dtype=float)
freq  = np.array([0.08, 0.17, 0.30, 0.25, 0.14, 0.06])
F     = np.concatenate([[0.0], np.cumsum(freq)])  # cumulative rel freq at edges

# Linear interpolation for a quantile q on the ogive
def quantile_from_ogive(q):
    k = np.searchsorted(F, q, side="left")
    # k is the first cumulative reaching q
    a, b = edges[k-1], edges[k]
    Fk_1, Fk = F[k-1], F[k]
    p = a + (q - Fk_1) / (Fk - Fk_1) * (b - a)
    return p, k

# ----- Sorted-data example (right panel) -----
sorted_x = np.array([3, 5, 7, 8, 11, 12, 14, 17, 19, 22, 25, 30])
n = len(sorted_x)
Fk = np.arange(1, n + 1) / n

# ----- Figure -----
fig = plt.figure(figsize=(13.2, 6.4))
gs = fig.add_gridspec(1, 2, width_ratios=[1.25, 1.0], wspace=0.22,
                      left=0.07, right=0.985, top=0.90, bottom=0.10)

# ============================================================
# LEFT — Ogive with quartile read-offs
# ============================================================
axL = fig.add_subplot(gs[0, 0])

# Shade the median class
p50, k50 = quantile_from_ogive(0.50)
axL.axvspan(edges[k50-1], edges[k50], color=PALETTE["accent"], alpha=0.12,
            zorder=0, label="Median class")

# Ogive: piecewise-linear cumulative from (a_k, F_{k-1}) to (b_k, F_k)
axL.plot(edges, F, "-", color=PALETTE["primary"], lw=2.4, zorder=3,
         label="Ogive (cum. rel. freq.)")
axL.plot(edges, F, "o", color=PALETTE["primary"], ms=5.5, zorder=4)

# Read-off lines for q = 0.25, 0.50, 0.75
qs = [0.25, 0.50, 0.75]
labels = [r"$Q_1=p_{0.25}$", r"$\mathrm{Med}=p_{0.50}$", r"$Q_3=p_{0.75}$"]
colors = [PALETTE["secondary"], PALETTE["warn"], PALETTE["ok"]]
for q, lab, col in zip(qs, labels, colors):
    pq, _ = quantile_from_ogive(q)
    # horizontal arrow from y-axis to the ogive
    axL.plot([edges[0], pq], [q, q], "--", color=col, lw=1.5, zorder=2)
    # vertical drop from ogive to x-axis
    axL.plot([pq, pq], [0, q], "--", color=col, lw=1.5, zorder=2)
    axL.plot(pq, q, "o", color=col, ms=7, zorder=5,
             markeredgecolor="white", markeredgewidth=1.2)
    # tick label on x-axis
    axL.annotate(f"{pq:.1f}", xy=(pq, 0), xytext=(0, -22),
                 textcoords="offset points", ha="center",
                 fontsize=10, color=col, fontweight="bold")
    # label on y-axis side
    axL.annotate(lab, xy=(edges[0], q), xytext=(4, 4),
                 textcoords="offset points", ha="left",
                 fontsize=10.5, color=col, fontweight="bold")

# Formula annotation pointing at the median read-off
axL.annotate(
    r"$p_q \;=\; a_k + \dfrac{q - F_{k-1}}{f_k}\,w_k$",
    xy=(p50, 0.50),
    xytext=(p50 + 8, 0.18),
    fontsize=11.5, color=PALETTE["neutral"],
    bbox=dict(boxstyle="round,pad=0.4", fc="#fff8d8",
              ec=PALETTE["accent"], lw=1.0),
    arrowprops=dict(arrowstyle="->", color=PALETTE["neutral"],
                    lw=1.0, connectionstyle="arc3,rad=-0.18"),
)

axL.set_xlim(edges[0] - 1.5, edges[-1] + 1.5)
axL.set_ylim(-0.02, 1.04)
axL.set_xticks(edges)
axL.set_yticks([0, 0.25, 0.50, 0.75, 1.0])
axL.set_yticklabels(["0", "0.25", "0.50", "0.75", "1"])
axL.set_xlabel("x  (class boundaries)")
axL.set_ylabel("Cumulative relative frequency  $F(x)$")
axL.set_title("Grouped data — read $p_q$ off the ogive",
              color=PALETTE["primary"])
axL.legend(loc="lower right", frameon=True, fancybox=True, framealpha=0.95)

# ============================================================
# RIGHT — Sorted-data definition table (ECDF flavour)
# ============================================================
axR = fig.add_subplot(gs[0, 1])
axR.set_xlim(0, 10)
axR.set_ylim(0, 10)
axR.axis("off")

axR.set_title("Discrete data — smallest $k$ with $F_k \\geq q$",
              color=PALETTE["primary"], pad=14)

# Header box
header_y = 9.1
axR.add_patch(FancyBboxPatch((0.2, header_y - 0.55), 9.6, 0.85,
                             boxstyle="round,pad=0.02",
                             fc=PALETTE["primary"], ec="none", zorder=2))
for x, txt in zip([1.2, 3.2, 5.4, 7.8],
                  ["rank $k$", "$x_{(k)}$", "$F_k = k/n$", "quantile flag"]):
    axR.text(x, header_y - 0.13, txt, ha="center", va="center",
             color="white", fontsize=11, fontweight="bold", zorder=3)

# Determine quantile flags
def smallest_k(q):
    # Return 1-indexed smallest k with Fk = k/n >= q
    return int(np.searchsorted(Fk, q - 1e-12, side="left")) + 1

k25 = smallest_k(0.25)
k50 = smallest_k(0.50)
k75 = smallest_k(0.75)
flag_map = {k25: ("$Q_1$", PALETTE["secondary"]),
            k50: ("Med",  PALETTE["warn"]),
            k75: ("$Q_3$", PALETTE["ok"])}

row_h = 0.62
top = header_y - 0.85
for i in range(n):
    y = top - i * row_h
    # alternating row shading
    if i % 2 == 0:
        axR.add_patch(Rectangle((0.2, y - row_h/2 + 0.05), 9.6, row_h - 0.04,
                                fc="#f6f7fb", ec="none", zorder=1))
    axR.text(1.2, y, f"{i+1}", ha="center", va="center", fontsize=10.5,
             color=PALETTE["neutral"], zorder=2)
    axR.text(3.2, y, f"{sorted_x[i]}", ha="center", va="center",
             fontsize=10.5, color=PALETTE["primary"], fontweight="bold",
             zorder=2)
    axR.text(5.4, y, f"{Fk[i]:.3f}", ha="center", va="center",
             fontsize=10.5, color=PALETTE["neutral"], zorder=2)
    if (i + 1) in flag_map:
        lbl, col = flag_map[i + 1]
        axR.add_patch(FancyBboxPatch((6.9, y - 0.22), 1.8, 0.44,
                                     boxstyle="round,pad=0.02",
                                     fc=col, ec="none", alpha=0.85, zorder=2))
        axR.text(7.8, y, lbl, ha="center", va="center", color="white",
                 fontsize=10.5, fontweight="bold", zorder=3)

# Footer rule card
foot_y = top - n * row_h - 0.25
axR.add_patch(FancyBboxPatch((0.2, foot_y - 1.05), 9.6, 1.05,
                             boxstyle="round,pad=0.02",
                             fc="#fff8d8", ec=PALETTE["accent"], lw=1.0,
                             zorder=2))
axR.text(0.5, foot_y - 0.30,
         r"Rule:  $p_q \;=\; x_{(k^*)}$  where  $k^* = \min\{k : F_k \geq q\}.$",
         ha="left", va="center", fontsize=11,
         color=PALETTE["neutral"], zorder=3)
axR.text(0.5, foot_y - 0.78,
         f"Here  n = {n};  $Q_1 = x_{{({k25})}} = {sorted_x[k25-1]}$,  "
         f"Med $= x_{{({k50})}} = {sorted_x[k50-1]}$,  "
         f"$Q_3 = x_{{({k75})}} = {sorted_x[k75-1]}$.",
         ha="left", va="center", fontsize=10.5,
         color=PALETTE["neutral"], zorder=3)

fig.suptitle("Quantiles — ogive read-off (grouped) and rank rule (discrete)",
             fontsize=14, fontweight="bold", color=PALETTE["primary"],
             y=0.985)

fig.savefig(OUT)
print(f"Saved {OUT}")
