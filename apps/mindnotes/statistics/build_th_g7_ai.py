"""Build AI theory diagram for G7 — Two-way tables and independence.

Four-panel figure:
TL — Joint frequency table with row/column marginals highlighted.
TR — Expected counts under independence + chi-squared independence check.
BL — Row-conditional bars (stacked %) for X = each row level.
BR — Side-by-side (beside) row-conditional bars for direct comparison.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, Rectangle

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/theory/th_g7_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# ----- Two-way table data (X = education level rows, Y = product preference cols) -----
row_labels = ["Low", "Mid", "High"]
col_labels = ["A", "B", "C"]
# Joint counts n_ij  -- clearly NOT independent (Low prefers A, High prefers C)
N = np.array([
    [40, 25, 10],   # Low
    [20, 30, 20],   # Mid
    [10, 25, 45],   # High
], dtype=float)

n = N.sum()
row_tot = N.sum(axis=1)        # n_i.
col_tot = N.sum(axis=0)        # n_.j

# Expected counts under independence  E_ij = n_i. * n_.j / n
E = np.outer(row_tot, col_tot) / n
chi2 = float(np.sum((N - E) ** 2 / E))
df   = (N.shape[0] - 1) * (N.shape[1] - 1)
# Critical value chi^2_{0.95, df=4} = 9.488
chi2_crit = 9.488

# Row-conditional relative frequencies f_{j|i} = n_ij / n_i.
P_row = (N.T / row_tot).T       # shape (rows, cols), each row sums to 1

# Marginal of Y as the "independence reference" bar
f_y = col_tot / n

# ----- Figure -----
fig = plt.figure(figsize=(13.4, 9.2))
gs = fig.add_gridspec(2, 2, width_ratios=[1.0, 1.0],
                      height_ratios=[1.0, 1.0],
                      wspace=0.22, hspace=0.34,
                      left=0.06, right=0.985, top=0.92, bottom=0.07)

col_pal = [PALETTE["primary"], PALETTE["accent"], PALETTE["ok"]]

# ============================================================
# TL — Joint frequency table with marginals
# ============================================================
axTL = fig.add_subplot(gs[0, 0])
axTL.set_xlim(0, 10); axTL.set_ylim(0, 10); axTL.axis("off")
axTL.set_title("Joint counts $n_{ij}$ with marginals",
               color=PALETTE["primary"], pad=10)

# Header row: Y categories + "Row total"
header_y = 8.6
x_cells = [2.0, 3.7, 5.4, 7.1, 8.8]   # X label col + 3 Y cols + row total col
# Top-left blank header
axTL.add_patch(FancyBboxPatch((0.4, header_y - 0.55), 9.2, 0.9,
                              boxstyle="round,pad=0.02",
                              fc=PALETTE["primary"], ec="none", zorder=2))
axTL.text(x_cells[0], header_y - 0.13, r"$X \backslash Y$", ha="center", va="center",
          color="white", fontsize=11.5, fontweight="bold", zorder=3)
for x, lab in zip(x_cells[1:4], col_labels):
    axTL.text(x, header_y - 0.13, lab, ha="center", va="center",
              color="white", fontsize=11.5, fontweight="bold", zorder=3)
axTL.text(x_cells[4], header_y - 0.13, r"$n_{i\cdot}$", ha="center", va="center",
          color=PALETTE["accent"], fontsize=12, fontweight="bold", zorder=3)

row_h = 0.95
top = header_y - 1.0
for i, rlab in enumerate(row_labels):
    y = top - i * row_h
    if i % 2 == 0:
        axTL.add_patch(Rectangle((0.4, y - row_h/2 + 0.05), 9.2, row_h - 0.04,
                                 fc="#f6f7fb", ec="none", zorder=1))
    axTL.text(x_cells[0], y, rlab, ha="center", va="center", fontsize=11.5,
              color=PALETTE["primary"], fontweight="bold", zorder=2)
    for j in range(3):
        axTL.text(x_cells[1 + j], y, f"{int(N[i,j])}", ha="center", va="center",
                  fontsize=11.5, color=PALETTE["neutral"], zorder=2)
    # Row total (highlighted)
    axTL.add_patch(FancyBboxPatch((x_cells[4] - 0.5, y - 0.32), 1.0, 0.62,
                                  boxstyle="round,pad=0.02",
                                  fc=PALETTE["accent"], ec="none", alpha=0.85,
                                  zorder=2))
    axTL.text(x_cells[4], y, f"{int(row_tot[i])}", ha="center", va="center",
              fontsize=11.5, color="white", fontweight="bold", zorder=3)

# Column-total row
y = top - 3 * row_h
axTL.add_patch(FancyBboxPatch((0.4, y - row_h/2 + 0.05), 9.2, row_h - 0.04,
                              boxstyle="round,pad=0.02",
                              fc="#fff8d8", ec=PALETTE["accent"], lw=1.0, zorder=1))
axTL.text(x_cells[0], y, r"$n_{\cdot j}$", ha="center", va="center",
          fontsize=12, color=PALETTE["accent"], fontweight="bold", zorder=2)
for j in range(3):
    axTL.text(x_cells[1 + j], y, f"{int(col_tot[j])}", ha="center", va="center",
              fontsize=11.5, color=PALETTE["primary"], fontweight="bold", zorder=2)
axTL.text(x_cells[4], y, f"n = {int(n)}", ha="center", va="center",
          fontsize=11.5, color=PALETTE["warn"], fontweight="bold", zorder=2)

# Caption
axTL.text(0.4, 0.9,
          r"$f_{ij}=n_{ij}/n,\ \ f_{i\cdot}=n_{i\cdot}/n,\ \ f_{\cdot j}=n_{\cdot j}/n$",
          ha="left", va="center", fontsize=10.5, color=PALETTE["neutral"])

# ============================================================
# TR — Expected counts under independence + chi-squared
# ============================================================
axTR = fig.add_subplot(gs[0, 1])
axTR.set_xlim(0, 10); axTR.set_ylim(0, 10); axTR.axis("off")
axTR.set_title(r"Expected $E_{ij}=n_{i\cdot}n_{\cdot j}/n$  vs.  $\chi^2$ check",
               color=PALETTE["primary"], pad=10)

header_y = 8.6
axTR.add_patch(FancyBboxPatch((0.4, header_y - 0.55), 9.2, 0.9,
                              boxstyle="round,pad=0.02",
                              fc=PALETTE["primary"], ec="none", zorder=2))
axTR.text(x_cells[0], header_y - 0.13, r"$X \backslash Y$", ha="center", va="center",
          color="white", fontsize=11.5, fontweight="bold", zorder=3)
for x, lab in zip(x_cells[1:4], col_labels):
    axTR.text(x, header_y - 0.13, lab, ha="center", va="center",
              color="white", fontsize=11.5, fontweight="bold", zorder=3)
axTR.text(x_cells[4], header_y - 0.13, r"$(O-E)^2/E$", ha="center", va="center",
          color=PALETTE["warn"], fontsize=10.5, fontweight="bold", zorder=3)

top = header_y - 1.0
for i, rlab in enumerate(row_labels):
    y = top - i * row_h
    if i % 2 == 0:
        axTR.add_patch(Rectangle((0.4, y - row_h/2 + 0.05), 9.2, row_h - 0.04,
                                 fc="#f6f7fb", ec="none", zorder=1))
    axTR.text(x_cells[0], y, rlab, ha="center", va="center", fontsize=11.5,
              color=PALETTE["primary"], fontweight="bold", zorder=2)
    row_chi = 0.0
    for j in range(3):
        # E on top, O small below
        axTR.text(x_cells[1 + j], y + 0.12, f"{E[i,j]:.1f}", ha="center",
                  va="center", fontsize=11, color=PALETTE["neutral"], zorder=2)
        axTR.text(x_cells[1 + j], y - 0.25,
                  f"(O={int(N[i,j])})", ha="center", va="center",
                  fontsize=8.5, color=PALETTE["muted"], zorder=2)
        row_chi += (N[i,j] - E[i,j])**2 / E[i,j]
    axTR.text(x_cells[4], y, f"{row_chi:.2f}", ha="center", va="center",
              fontsize=11.5, color=PALETTE["warn"], fontweight="bold", zorder=2)

# Conclusion card
foot_y = 3.4
axTR.add_patch(FancyBboxPatch((0.4, foot_y - 1.5), 9.2, 1.6,
                              boxstyle="round,pad=0.02",
                              fc="#fff8d8", ec=PALETTE["accent"], lw=1.0, zorder=2))
axTR.text(0.7, foot_y - 0.2,
          r"$\chi^2 \;=\; \sum_{i,j}\dfrac{(n_{ij}-E_{ij})^2}{E_{ij}}"
          rf"\;=\;{chi2:.2f}$",
          ha="left", va="center", fontsize=12, color=PALETTE["primary"],
          fontweight="bold", zorder=3)
axTR.text(0.7, foot_y - 0.78,
          rf"df $=(r-1)(c-1)={df}$,  critical $\chi^2_{{0.95,{df}}}={chi2_crit:.2f}$",
          ha="left", va="center", fontsize=10.5, color=PALETTE["neutral"], zorder=3)
verdict = "Reject independence" if chi2 > chi2_crit else "Cannot reject independence"
axTR.text(0.7, foot_y - 1.30,
          rf"$\chi^2={chi2:.2f} > {chi2_crit:.2f}$  $\Rightarrow$  {verdict}.",
          ha="left", va="center", fontsize=11, color=PALETTE["warn"],
          fontweight="bold", zorder=3)

# ============================================================
# BL — Row-conditional STACKED bars
# ============================================================
axBL = fig.add_subplot(gs[1, 0])
y_pos = np.arange(len(row_labels))
left = np.zeros(len(row_labels))
for j in range(3):
    axBL.barh(y_pos, P_row[:, j], left=left,
              color=col_pal[j], edgecolor="white", linewidth=1.2,
              label=f"Y = {col_labels[j]}")
    # text labels inside segments
    for i in range(len(row_labels)):
        if P_row[i, j] > 0.06:
            axBL.text(left[i] + P_row[i, j] / 2, i,
                      f"{P_row[i, j]*100:.0f}%", ha="center", va="center",
                      color="white", fontsize=10.5, fontweight="bold")
    left += P_row[:, j]

# Add marginal reference line at cumulative marginal of Y
cum = np.cumsum(f_y)
for c in cum[:-1]:
    axBL.axvline(c, color=PALETTE["warn"], lw=1.3, ls="--", alpha=0.85, zorder=5)
axBL.text(cum[0], len(row_labels) - 0.4,
          "  marginal of Y (independence ref.)",
          color=PALETTE["warn"], fontsize=9.5, va="center")

axBL.set_yticks(y_pos)
axBL.set_yticklabels([f"X = {r}" for r in row_labels])
axBL.set_xlim(0, 1.0)
axBL.set_xticks([0, 0.25, 0.5, 0.75, 1.0])
axBL.set_xticklabels(["0%", "25%", "50%", "75%", "100%"])
axBL.set_xlabel("Row-conditional relative frequency  $f_{j|i}$")
axBL.set_title("Stacked bars — $Y \\mid X = x_i$",
               color=PALETTE["primary"])
axBL.invert_yaxis()
axBL.legend(loc="lower right", frameon=True, fancybox=True, framealpha=0.95,
            ncol=3, fontsize=10)
axBL.grid(axis="y", visible=False)

# ============================================================
# BR — Side-by-side (beside) row-conditional bars
# ============================================================
axBR = fig.add_subplot(gs[1, 1])
x = np.arange(len(col_labels))
w = 0.25
for i in range(len(row_labels)):
    axBR.bar(x + (i - 1) * w, P_row[i, :], width=w,
             color=col_pal[i], edgecolor="white", linewidth=1.0,
             label=f"X = {row_labels[i]}")
    for j in range(len(col_labels)):
        axBR.text(x[j] + (i - 1) * w, P_row[i, j] + 0.012,
                  f"{P_row[i, j]*100:.0f}%", ha="center", va="bottom",
                  fontsize=9.5, color=PALETTE["neutral"])

# Overlay marginal of Y as the independence baseline (dashed)
axBR.plot(x, f_y, "o--", color=PALETTE["warn"], lw=1.6, ms=8,
          markeredgecolor="white", markeredgewidth=1.2,
          label=r"marginal $f_{\cdot j}$  (indep. ref.)", zorder=5)

axBR.set_xticks(x)
axBR.set_xticklabels([f"Y = {c}" for c in col_labels])
axBR.set_ylim(0, max(P_row.max(), f_y.max()) * 1.22)
axBR.set_ylabel("Conditional relative frequency  $f_{j|i}$")
axBR.set_title("Side-by-side bars — compare $f_{j|i}$ across rows",
               color=PALETTE["primary"])
axBR.legend(loc="upper right", frameon=True, fancybox=True, framealpha=0.95,
            fontsize=9.5)
axBR.grid(axis="x", visible=False)
# percentage y-ticks
yticks = axBR.get_yticks()
axBR.set_yticklabels([f"{t*100:.0f}%" for t in yticks])

fig.suptitle("Two-way tables — joint, marginal, conditional and independence",
             fontsize=14.5, fontweight="bold", color=PALETTE["primary"],
             y=0.985)

fig.savefig(OUT)
print(f"Saved {OUT}")
print(f"chi2 = {chi2:.3f}, df = {df}, crit_0.95 = {chi2_crit}")
