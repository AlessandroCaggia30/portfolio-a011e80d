"""Build AI walkthrough plot for Ex 2.3b — identify extreme-spending customers
via *upper* outliers of `AmountSpent` in the `DS` dataframe.

The exercise asks two things:
  (1) *How* would you assess whether there are customers with particularly
      high expenditures?  -> compute the Tukey upper fence
                              F_U = Q3 + 1.5 * (Q3 - Q1)
                              and count observations above it.
  (2) *How many* are there?  -> count above the fence.

The AI plot makes the construction *visible*:

LEFT  panel  — horizontal boxplot of `AmountSpent` with the **upper fence**
               drawn as a vertical dashed line.  The whiskers, the box
               (Q1, Me, Q3) and the cloud of outlier dots are annotated
               explicitly; the IQR is bracketed at the bottom of the
               plot, and the 1.5*IQR extension is bracketed *to the right
               of Q3* so the reader sees where the fence comes from.

RIGHT panel  — histogram of `AmountSpent` with the upper-fence vertical
               line drawn at the same value.  Bars to the right of the
               fence are coloured in red to make the outlier mass visible.
               A stats card lists Q1, Me, Q3, IQR, the fence, and the
               outlier count.

Saves to: statistics/images/ex2/ex2_3b_ai.png
"""
import os
import sys

sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import pyreadr
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_3b_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# --- Load AmountSpent from DS (same dataset as the exercise prompt) -----
result = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata")
x = result["DS"]["AmountSpent"].to_numpy().astype(float)

n     = len(x)
mn    = float(np.min(x))
mx    = float(np.max(x))
q1    = float(np.quantile(x, 0.25))
med   = float(np.median(x))
q3    = float(np.quantile(x, 0.75))
iqr   = q3 - q1
upper = q3 + 1.5 * iqr          # Tukey upper fence
lower = q1 - 1.5 * iqr
n_out = int((x > upper).sum())
mean  = float(np.mean(x))
sd    = float(np.std(x, ddof=1))

print(f"  n={n}  min={mn}  Q1={q1}  Me={med}  Q3={q3}  max={mx}")
print(f"  IQR={iqr}  upper fence (Q3+1.5*IQR) = {upper:.3f}")
print(f"  # upper outliers (x > fence) = {n_out}")

# =====================================================================
# Layout
# =====================================================================
fig = plt.figure(figsize=(14.0, 5.6))
gs  = fig.add_gridspec(1, 2, width_ratios=[1.05, 1.35], wspace=0.20)
axL = fig.add_subplot(gs[0, 0])
axR = fig.add_subplot(gs[0, 1])

# =====================================================================
# LEFT - boxplot with upper-fence construction annotated
# =====================================================================
bp = axL.boxplot(
    x, vert=False, widths=0.55, patch_artist=True,
    boxprops=dict(facecolor=PALETTE["secondary"], alpha=0.55,
                  edgecolor=PALETTE["primary"], linewidth=1.4),
    medianprops=dict(color=PALETTE["warn"], linewidth=2.4),
    whiskerprops=dict(color=PALETTE["primary"], linewidth=1.4),
    capprops=dict(color=PALETTE["primary"], linewidth=1.4),
    flierprops=dict(marker="o", markersize=4.5,
                    markerfacecolor=PALETTE["warn"],
                    markeredgecolor=PALETTE["primary"], alpha=0.75),
)

axL.set_yticks([])
axL.set_xlim(-200, mx + 350)

# Annotate the five summary numbers
for v, tag, col in [(mn,    "min",    PALETTE["primary"]),
                    (q1,    r"$Q_1$", PALETTE["secondary"]),
                    (med,   "Me",     PALETTE["warn"]),
                    (q3,    r"$Q_3$", PALETTE["secondary"]),
                    (mx,    "max",    PALETTE["primary"])]:
    axL.axvline(v, ymin=0.05, ymax=0.18, color=col, lw=1.0, alpha=0.7)
    axL.text(v, 0.55, f"{tag}\n{v:,.0f}",
             ha="center", va="top", fontsize=9.0, color=col)

# Highlight the UPPER FENCE with a tall dashed red line
axL.axvline(upper, color=PALETTE["warn"], lw=2.0, ls="--",
            label=fr"$F_U = Q_3 + 1.5\,\mathrm{{IQR}} = {upper:,.2f}$")
axL.text(upper, 1.62, fr"$F_U={upper:,.0f}$",
         color=PALETTE["warn"], ha="center", va="bottom",
         fontsize=10, fontweight="bold")

# Bracket 1: the IQR (between Q1 and Q3) drawn ABOVE the box
y_iqr = 1.42
axL.annotate("", xy=(q1, y_iqr), xytext=(q3, y_iqr),
             arrowprops=dict(arrowstyle="|-|", color=PALETTE["secondary"],
                             lw=1.5, shrinkA=0, shrinkB=0))
axL.text((q1 + q3) / 2, y_iqr + 0.05,
         fr"$\mathrm{{IQR}} = Q_3 - Q_1 = {iqr:.2f}$",
         ha="center", va="bottom", color=PALETTE["secondary"],
         fontsize=10, fontweight="bold")

# Bracket 2: the 1.5*IQR extension from Q3 to the fence
y_ext = 1.18
axL.annotate("", xy=(q3, y_ext), xytext=(upper, y_ext),
             arrowprops=dict(arrowstyle="|-|", color=PALETTE["warn"],
                             lw=1.5, shrinkA=0, shrinkB=0))
axL.text((q3 + upper) / 2, y_ext + 0.03,
         fr"$1.5\,\mathrm{{IQR}} = {1.5*iqr:.2f}$",
         ha="center", va="bottom", color=PALETTE["warn"],
         fontsize=9.5, fontweight="bold")

# Annotate the cloud of upper outliers
axL.annotate(f"{n_out} customers above $F_U$\n(upper outliers)",
             xy=(upper + 400, 1.0),
             xytext=(upper + 350, 0.55),
             color=PALETTE["warn"], fontsize=10, fontweight="bold",
             ha="left",
             arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.2))

axL.set_ylim(0.30, 1.78)
axL.set_xlabel("AmountSpent  (USD)")
axL.set_title("Boxplot: build the upper fence  $F_U = Q_3 + 1.5\\,\\mathrm{IQR}$")
axL.legend(loc="upper right", framealpha=0.95, fontsize=9.5)

# =====================================================================
# RIGHT - histogram with the upper fence overlaid + outlier bars in red
# =====================================================================
bins = np.linspace(0, mx + 50, 36)
counts, edges, patches = axR.hist(
    x, bins=bins,
    color=PALETTE["secondary"], edgecolor=PALETTE["primary"],
    alpha=0.55, linewidth=0.8, label=f"AmountSpent  ($n = {n}$)"
)

# Recolour the bars whose LEFT edge sits >= upper fence
for left, patch in zip(edges[:-1], patches):
    if left >= upper:
        patch.set_facecolor(PALETTE["warn"])
        patch.set_alpha(0.60)
        patch.set_edgecolor(PALETTE["warn"])

ymax = counts.max()
axR.set_ylim(0, ymax * 1.30)

axR.axvline(upper, color=PALETTE["warn"], lw=2.0, ls="--",
            label=fr"$F_U = {upper:,.2f}$")
axR.axvline(q3, color=PALETTE["secondary"], lw=1.6, ls=":",
            label=fr"$Q_3 = {q3:,.2f}$")

# Annotation arrow over the outlier mass
y_arrow = ymax * 1.10
axR.annotate(f"{n_out} customers\nabove $F_U$",
             xy=(upper + 100, ymax * 0.20),
             xytext=(upper + 50, y_arrow),
             color=PALETTE["warn"], fontsize=10, fontweight="bold",
             ha="left",
             arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.3))

axR.set_xlabel("AmountSpent  (USD)")
axR.set_ylabel("Counts")
axR.set_title("Histogram: bars above $F_U$ are the upper outliers")
axR.legend(loc="upper right", framealpha=0.95, fontsize=9.5)

# Stats card with the construction in numbers
card = (
    r"$\bf{Outlier\ construction}$" + "\n"
    fr"$Q_1 = {q1:,.2f},\;\;\mathrm{{Me}} = {med:,.0f}$" + "\n"
    fr"$Q_3 = {q3:,.2f}$" + "\n"
    fr"$\mathrm{{IQR}} = Q_3 - Q_1 = {iqr:.2f}$" + "\n"
    fr"$F_U = Q_3 + 1.5\,\mathrm{{IQR}} = {upper:,.2f}$" + "\n"
    fr"$\#\{{x_i > F_U\}} = {n_out}$"
)
axR.text(0.985, 0.62, card,
         transform=axR.transAxes, ha="right", va="top", fontsize=9.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
