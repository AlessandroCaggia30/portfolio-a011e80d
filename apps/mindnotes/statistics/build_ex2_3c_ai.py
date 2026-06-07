"""Build AI walkthrough plot for Ex 2.3c — Shape of the AmountSpent distribution.

The exercise asks to *describe the shape* of the distribution. The two
canonical tools are the **boxplot** and the **histogram**; the verdict is
**right-skewness** (positive skew). The AI plot makes the three visual
diagnostics for right-skew immediately legible, and adds the numerical
indices that summarise them.

LEFT  panel  — Boxplot (horizontal) with the three skewness diagnostics
               annotated directly on the box:
                 (a) median sits closer to Q1 than to Q3
                     (i.e. the left half of the box is *narrower*),
                 (b) lower whisker is much shorter than the upper one,
                 (c) a long right tail of upper outliers
                     (count: 17 above Q3 + 1.5*IQR = 3607.88).
RIGHT panel  — Histogram of AmountSpent with the mean and median
               overlaid. For right-skewed data:  mean > median
               (mean is pulled up by the long right tail).
               A small "stats card" lists the skewness coefficient
               (Pearson g1) and the mean-median gap to give a number.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import pyreadr
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_3c_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# --- Load AmountSpent from the DS dataset (same source as the prompt) ---
result = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata")
x = result["DS"]["AmountSpent"].to_numpy().astype(float)

n     = len(x)
mn    = float(np.min(x))
mx    = float(np.max(x))
mean  = float(np.mean(x))
med   = float(np.median(x))
q1    = float(np.quantile(x, 0.25))
q3    = float(np.quantile(x, 0.75))
sd    = float(np.std(x, ddof=1))
iqr   = q3 - q1
upper = q3 + 1.5 * iqr
lower = q1 - 1.5 * iqr
n_out = int((x > upper).sum())
# Pearson g1 (Fisher–Pearson) skewness coefficient
g1    = float(((x - mean) ** 3).mean() / (sd ** 3))
# Quartile (Bowley) skewness
bowley = ((q3 - med) - (med - q1)) / iqr

print(f"  n={n}  mean={mean:.2f}  median={med:.2f}  sd={sd:.2f}")
print(f"  min={mn} Q1={q1} Me={med} Q3={q3} max={mx}  IQR={iqr}  upper_fence={upper:.2f}")
print(f"  upper outliers = {n_out}   g1={g1:.3f}   Bowley={bowley:.3f}")

# =====================================================================
# Layout
# =====================================================================
fig = plt.figure(figsize=(13.8, 5.6))
gs  = fig.add_gridspec(1, 2, width_ratios=[1.05, 1.35], wspace=0.18)
axL = fig.add_subplot(gs[0, 0])
axR = fig.add_subplot(gs[0, 1])

# =====================================================================
# LEFT — horizontal boxplot with skewness diagnostics
# =====================================================================
bp = axL.boxplot(
    x, vert=False, widths=0.55, patch_artist=True,
    boxprops=dict(facecolor=PALETTE["secondary"], alpha=0.55,
                  edgecolor=PALETTE["primary"], linewidth=1.4),
    medianprops=dict(color=PALETTE["warn"], linewidth=2.4),
    whiskerprops=dict(color=PALETTE["primary"], linewidth=1.4),
    capprops=dict(color=PALETTE["primary"], linewidth=1.4),
    flierprops=dict(marker="o", markersize=4.5,
                    markerfacecolor=PALETTE["accent"],
                    markeredgecolor=PALETTE["primary"], alpha=0.75),
)

# Annotate min/Q1/Me/Q3/upper-fence on the x-axis
axL.set_yticks([])
axL.set_xlim(-200, mx + 250)
for v, tag, col in [(mn,    "min",       PALETTE["primary"]),
                    (q1,    r"$Q_1$",    PALETTE["secondary"]),
                    (med,   "Me",        PALETTE["warn"]),
                    (q3,    r"$Q_3$",    PALETTE["secondary"]),
                    (upper, "upper\nfence", PALETTE["primary"]),
                    (mx,    "max",       PALETTE["primary"])]:
    axL.axvline(v, ymin=0.05, ymax=0.18, color=col, lw=1.0, alpha=0.7)
    axL.text(v, 0.55, f"{tag}\n{v:,.0f}",
             ha="center", va="top", fontsize=9.0, color=col)

# Bracket (a): left vs right half-box widths
y_top = 1.40
axL.annotate("", xy=(q1, y_top), xytext=(med, y_top),
             arrowprops=dict(arrowstyle="|-|", color=PALETTE["ok"],
                             lw=1.5, shrinkA=0, shrinkB=0))
axL.text((q1 + med) / 2, y_top + 0.05,
         fr"Me $-$ $Q_1 = {med - q1:.0f}$",
         ha="center", va="bottom", color=PALETTE["ok"], fontsize=9.5,
         fontweight="bold")
axL.annotate("", xy=(med, y_top), xytext=(q3, y_top),
             arrowprops=dict(arrowstyle="|-|", color=PALETTE["accent"],
                             lw=1.5, shrinkA=0, shrinkB=0))
axL.text((med + q3) / 2, y_top + 0.05,
         fr"$Q_3 -$ Me $= {q3 - med:.0f}$",
         ha="center", va="bottom", color=PALETTE["accent"], fontsize=9.5,
         fontweight="bold")

# Bracket (b): whisker lengths
y_low = 0.78
axL.annotate("", xy=(mn, y_low), xytext=(q1, y_low),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["ok"],
                             lw=1.2, shrinkA=0, shrinkB=0))
axL.text((mn + q1) / 2, y_low - 0.04,
         f"lower whisker\n= {q1 - mn:.0f}",
         ha="center", va="top", color=PALETTE["ok"], fontsize=9.0)
axL.annotate("", xy=(q3, y_low), xytext=(upper, y_low),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["accent"],
                             lw=1.2, shrinkA=0, shrinkB=0))
axL.text((q3 + upper) / 2, y_low - 0.04,
         f"upper whisker\n= {upper - q3:.0f}",
         ha="center", va="top", color=PALETTE["accent"], fontsize=9.0)

# Annotate the outlier cloud
axL.annotate(f"{n_out} upper\noutliers",
             xy=(upper + 600, 1.0),
             xytext=(upper + 400, 1.55),
             color=PALETTE["accent"], fontsize=10, fontweight="bold",
             ha="center",
             arrowprops=dict(arrowstyle="->", color=PALETTE["accent"], lw=1.1))

axL.set_ylim(0.35, 1.75)
axL.set_xlabel("AmountSpent  (USD)")
axL.set_title("Boxplot diagnostics: median off-centre, asymmetric whiskers,\nlong right tail of outliers  $\\Rightarrow$ right-skewed")

# =====================================================================
# RIGHT — histogram with mean & median overlay + stats card
# =====================================================================
bins = np.linspace(0, mx + 50, 36)
counts, edges, _ = axR.hist(
    x, bins=bins,
    color=PALETTE["secondary"], edgecolor=PALETTE["primary"],
    alpha=0.55, linewidth=0.8, label=f"AmountSpent  ($n = {n}$)"
)
ymax = counts.max()
axR.set_ylim(0, ymax * 1.28)

# Mean & median verticals
axR.axvline(med,  color=PALETTE["warn"], lw=2.0, ls="--",
            label=fr"median $= {med:,.0f}$")
axR.axvline(mean, color=PALETTE["primary"], lw=2.0, ls="-",
            label=fr"mean $= {mean:,.2f}$")

# Arrow showing mean - median gap
y_arrow = ymax * 1.10
axR.annotate("", xy=(mean, y_arrow), xytext=(med, y_arrow),
             arrowprops=dict(arrowstyle="->", color=PALETTE["warn"],
                             lw=1.6))
axR.text((mean + med) / 2, y_arrow + ymax * 0.03,
         fr"mean $-$ median $= {mean - med:,.2f}$",
         ha="center", va="bottom", color=PALETTE["warn"],
         fontsize=10, fontweight="bold")

axR.set_xlabel("AmountSpent  (USD)")
axR.set_ylabel("Counts")
axR.set_title("Histogram: mean > median  $\\Rightarrow$  right-skewed")
axR.legend(loc="upper right", framealpha=0.95, fontsize=9.5)

# Stats card with the two skewness coefficients
card = (
    r"$\bf{Shape\ diagnostics}$" + "\n"
    fr"mean $-$ median $= {mean - med:,.1f}\;>\;0$" + "\n"
    fr"$Q_3 -$ Me $= {q3 - med:.0f}\;>\;$ Me $- Q_1 = {med - q1:.0f}$" + "\n"
    fr"Bowley skew $= \dfrac{{(Q_3-\mathrm{{Me}})-(\mathrm{{Me}}-Q_1)}}{{\mathrm{{IQR}}}} = {bowley:.3f}$" + "\n"
    fr"Pearson $g_1 = \dfrac{{m_3}}{{s^{{3}}}} = {g1:.3f}\;>\;0$" + "\n"
    fr"# upper outliers $= {n_out}$"
)
axR.text(0.985, 0.55, card,
         transform=axR.transAxes, ha="right", va="top", fontsize=9.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
