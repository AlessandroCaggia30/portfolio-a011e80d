"""Build AI walkthrough plot for Ex 2.5d — outliers of Age (customer_habits).

The exercise asks to identify the outliers of Age using the Tukey fences and
to compute their share on the sample.

Five-number summary (n = 34 866):
    min = 16, Q1 = 40, Me = 46, Q3 = 54, max = 96.
    IQR = 14.
Fences:
    lower = Q1 - 1.5*IQR = 40 - 21 = 19
    upper = Q3 + 1.5*IQR = 54 + 21 = 75
Outliers: Age < 19  OR  Age > 75.
Share: 100 * mean(Age < 19 | Age > 75) = 0.9952%.

The plot has two panels:

LEFT
  Histogram of Age with the Tukey machinery overlaid:
    * shaded box [Q1, Q3] (the IQR);
    * vertical median line;
    * lower fence 19 and upper fence 75 as red dashed lines;
    * lower/upper outlier tails coloured warn;
    * a bracket showing 1.5*IQR on each side, anchored at Q1 and Q3.

RIGHT
  Formula card with:
    * the five-number summary;
    * the IQR;
    * both fences derived step by step;
    * the count and the percentage of outliers;
    * the R command actually used in the snippet.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import pyreadr
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_5d_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# --- Load Age from customer_habits ---
result = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata")
age = result["customer_habits"]["Age"].to_numpy()

mn  = float(np.min(age))
mx  = float(np.max(age))
med = float(np.median(age))
q1  = float(np.quantile(age, 0.25))
q3  = float(np.quantile(age, 0.75))
iqr = q3 - q1
upper = q3 + 1.5 * iqr
lower = q1 - 1.5 * iqr
n = len(age)
n_out = int(((age < lower) | (age > upper)).sum())
pct_out = 100 * n_out / n

print(f"  n={n}  q1={q1}  med={med}  q3={q3}  iqr={iqr}")
print(f"  lower fence={lower}  upper fence={upper}")
print(f"  n_outliers={n_out}  pct={pct_out:.4f}%")

# =====================================================================
# Layout
# =====================================================================
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.4),
                               gridspec_kw={"width_ratios": [2.4, 1]})

# =====================================================================
# LEFT — histogram with Tukey fences overlay
# =====================================================================
bins = 30
counts, edges, patches = ax1.hist(age, bins=bins, density=True,
                                  color=PALETTE["secondary"],
                                  edgecolor=PALETTE["primary"],
                                  alpha=0.55, linewidth=1.0,
                                  label=f"Age (n = {n:,})")

# Recolour outlier bins (those whose centre lies below `lower` or above `upper`)
for patch, left, right in zip(patches, edges[:-1], edges[1:]):
    centre = 0.5 * (left + right)
    if centre < lower or centre > upper:
        patch.set_facecolor(PALETTE["warn"])
        patch.set_alpha(0.55)
        patch.set_edgecolor(PALETTE["warn"])

ymax = counts.max()
ax1.set_ylim(0, ymax * 1.50)

# Shaded IQR box [Q1, Q3]
ax1.axvspan(q1, q3, ymin=0.0, ymax=ymax / (ymax * 1.50),
            color=PALETTE["ok"], alpha=0.13,
            label=fr"IQR $= Q_3 - Q_1 = {iqr:.0f}$")

# Vertical lines: median, Q1, Q3, fences
ax1.axvline(med, color=PALETTE["accent"], lw=1.8, ls="--",
            label=fr"median Me $= {med:.0f}$")
ax1.axvline(q1, color=PALETTE["primary"], lw=1.2, ls=":")
ax1.axvline(q3, color=PALETTE["primary"], lw=1.2, ls=":")
ax1.axvline(lower, color=PALETTE["warn"], lw=2.0, ls="--",
            label=fr"lower fence $Q_1 - 1.5\,\mathrm{{IQR}} = {lower:.0f}$")
ax1.axvline(upper, color=PALETTE["warn"], lw=2.0, ls="--",
            label=fr"upper fence $Q_3 + 1.5\,\mathrm{{IQR}} = {upper:.0f}$")

# Brackets showing 1.5*IQR on each side
y_br = ymax * 1.22
ax1.annotate("", xy=(q1, y_br), xytext=(lower, y_br),
             arrowprops=dict(arrowstyle="|-|", color=PALETTE["warn"],
                             lw=1.4, shrinkA=0, shrinkB=0))
ax1.text(0.5 * (q1 + lower), y_br + ymax * 0.045,
         r"$1.5\,\mathrm{IQR}$",
         ha="center", va="bottom", color=PALETTE["warn"],
         fontsize=10, fontweight="bold")

ax1.annotate("", xy=(q3, y_br), xytext=(upper, y_br),
             arrowprops=dict(arrowstyle="|-|", color=PALETTE["warn"],
                             lw=1.4, shrinkA=0, shrinkB=0))
ax1.text(0.5 * (q3 + upper), y_br + ymax * 0.045,
         r"$1.5\,\mathrm{IQR}$",
         ha="center", va="bottom", color=PALETTE["warn"],
         fontsize=10, fontweight="bold")

# Position tick labels
for x, tag, col in [(q1, fr"$Q_1$" + f"\n{q1:.0f}", PALETTE["primary"]),
                    (med, "Me\n" + f"{med:.0f}", PALETTE["accent"]),
                    (q3, fr"$Q_3$" + f"\n{q3:.0f}", PALETTE["primary"]),
                    (lower, f"{lower:.0f}", PALETTE["warn"]),
                    (upper, f"{upper:.0f}", PALETTE["warn"])]:
    ax1.text(x, -ymax * 0.085, tag,
             ha="center", va="top", fontsize=9.5, color=col, fontweight="bold")

# Annotate the outlier tails with their share
ax1.annotate(f"outliers\n{n_out:,} obs.\n({pct_out:.3f}% of sample)",
             xy=(upper + 5, ymax * 0.18),
             xytext=(upper + 7, ymax * 0.85),
             fontsize=10, color=PALETTE["warn"], fontweight="bold",
             ha="left",
             arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.2))

ax1.set_xlabel("Age  (years)")
ax1.set_ylabel("Density")
ax1.set_title("Ex 2.5d — Tukey fences and outliers of Age")
ax1.legend(loc="upper right", framealpha=0.95, fontsize=9.2)

# =====================================================================
# RIGHT — derivation / verdict card
# =====================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)
ax2.add_patch(FancyBboxPatch((0.02, 0.04), 0.96, 0.92,
                             boxstyle="round,pad=0.02",
                             linewidth=1.1,
                             facecolor="#fbfbfd",
                             edgecolor=PALETTE["primary"]))

txt = (
    r"$\bf{Five\!-\!number\ summary}$"
    "\n"
    fr"min $= {mn:.0f}$,  $Q_1 = {q1:.0f}$,  Me $= {med:.0f}$,"
    "\n"
    fr"$Q_3 = {q3:.0f}$,  max $= {mx:.0f}$"
    "\n"
    fr"IQR $= Q_3 - Q_1 = {iqr:.0f}$"
    "\n"
    r"$\bf{Tukey\ fences}$"
    "\n"
    fr"upper $= Q_3 + 1.5 \cdot {iqr:.0f} = {upper:.0f}$"
    "\n"
    fr"lower $= Q_1 - 1.5 \cdot {iqr:.0f} = {lower:.0f}$"
    "\n"
    r"$\bf{Outliers}$: Age $< 19$ or $> 75$"
    "\n"
    fr"count $= {n_out:,}$ on $n = {n:,}$"
    "\n"
    fr"share $= {pct_out:.4f}\%$  ($<1\%$,"
    "\n"
    "  residual weight)"
)
ax2.text(0.06, 0.96, txt, ha="left", va="top", fontsize=10.3,
         color=PALETTE["primary"])

ax2.text(0.06, 0.04,
         'R:  100*mean(\n'
         '       customer_habits$Age < 19 |\n'
         '       customer_habits$Age > 75)\n'
         f'## [1] {pct_out:.7f}',
         ha="left", va="bottom", fontsize=8.6, family="monospace",
         color=PALETTE["neutral"])

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
