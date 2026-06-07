"""Build AI walkthrough plot for Ex 2.5f — mean vs median of Age (customer_habits).

The point of 2.5f is: from the right-skewed boxplot of part (a), one expects
  mean > median.
The sales-managers' belief that the mean Age sits in [40, 45] is checked
against the actual mean = 47.1 (above the median = 46, and above the upper
endpoint 45).

Visual: two-panel figure on the Age histogram.

LEFT panel
  Histogram of Age with overlays:
    - vertical median line (Me = 46),
    - vertical mean line  (mean = 47.1),
    - shaded belief interval [40, 45],
    - bracket showing skewness gap (mean - median).
  Makes the right-skew "mean above median" relationship immediately visible.

RIGHT panel
  Formula card listing the belief, the actual mean, the verdict, and the R
  command (distr.summary.x(... stats="mean")).
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import pyreadr
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_5f_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# --- Load Age from customer_habits ---
result = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata")
age = result["customer_habits"]["Age"].to_numpy()

mn   = float(np.min(age))
mx   = float(np.max(age))
mean = float(np.mean(age))
med  = float(np.median(age))
q1   = float(np.quantile(age, 0.25))
q3   = float(np.quantile(age, 0.75))
n    = len(age)

belief_lo, belief_hi = 40.0, 45.0

print(f"  n={n}  mean={mean:.4f}  median={med}  q1={q1}  q3={q3}")
print(f"  belief interval=[{belief_lo}, {belief_hi}]  -> mean above upper endpoint? {mean > belief_hi}")

# =====================================================================
# Layout
# =====================================================================
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.4),
                               gridspec_kw={"width_ratios": [2.4, 1]})

# =====================================================================
# LEFT — histogram + mean/median overlays + belief band
# =====================================================================
bins = 20
counts, edges, patches = ax1.hist(age, bins=bins, density=True,
                                  color=PALETTE["secondary"],
                                  edgecolor=PALETTE["primary"],
                                  alpha=0.55, linewidth=1.0,
                                  label=f"Age (n = {n:,})")

ymax = counts.max()
ax1.set_ylim(0, ymax * 1.45)

# Belief interval [40, 45] — shaded
ax1.axvspan(belief_lo, belief_hi,
            ymin=0.0, ymax=ymax / (ymax * 1.45),
            color=PALETTE["warn"], alpha=0.18,
            label=f"managers' belief: mean $\\in$ [{belief_lo:.0f}, {belief_hi:.0f}]")

# Median vertical line
ax1.axvline(med,  color=PALETTE["accent"], lw=1.8, ls="--",
            label=fr"median Me $= {med:.0f}$")
# Mean vertical line
ax1.axvline(mean, color=PALETTE["primary"], lw=2.0, ls="-",
            label=fr"mean $\bar{{x}} = {mean:.1f}$  (actual)")
# Upper endpoint of belief interval
ax1.axvline(belief_hi, color=PALETTE["warn"], lw=1.4, ls=":",
            label=fr"upper belief endpoint $= {belief_hi:.0f}$")

# Bracket showing the skewness gap mean - median
y_gap = ymax * 1.18
ax1.annotate("", xy=(med, y_gap), xytext=(mean, y_gap),
             arrowprops=dict(arrowstyle="|-|", color=PALETTE["primary"],
                             lw=1.8, shrinkA=0, shrinkB=0))
ax1.text((med + mean) / 2, y_gap + ymax * 0.04,
         fr"$\bar{{x}} - $ Me $= {mean - med:.1f}$" + "\n(right-skew gap)",
         ha="center", va="bottom", color=PALETTE["primary"],
         fontsize=10, fontweight="bold")

# Tick labels for key positions
for x, tag, col in [(belief_lo, f"{belief_lo:.0f}", PALETTE["warn"]),
                    (belief_hi, f"{belief_hi:.0f}", PALETTE["warn"]),
                    (med, f"Me\n{med:.0f}", PALETTE["accent"]),
                    (mean, fr"$\bar{{x}}$" + f"\n{mean:.1f}", PALETTE["primary"])]:
    ax1.text(x, -ymax * 0.08, tag,
             ha="center", va="top", fontsize=9.5, color=col, fontweight="bold")

ax1.set_xlabel("Age  (years)")
ax1.set_ylabel("Density")
ax1.set_title("Ex 2.5f — Mean vs. median of Age: belief [40, 45] vs. actual 47.1")
ax1.legend(loc="upper right", framealpha=0.95, fontsize=9.2)

# =====================================================================
# RIGHT — verdict / R panel
# =====================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)
ax2.add_patch(FancyBboxPatch((0.02, 0.04), 0.96, 0.92,
                             boxstyle="round,pad=0.02",
                             linewidth=1.1,
                             facecolor="#fbfbfd",
                             edgecolor=PALETTE["primary"]))

txt = (
    r"$\bf{Belief\ check\ for\ mean\ Age}$"
    "\n"
    r"From 2.5a: boxplot $\Rightarrow$ right-skew"
    "\n"
    r"$\Rightarrow\ \bar{x} \,>\, \mathrm{Me} \,=\, 46$"
    "\n"
    fr"Belief: $\bar{{x}} \in [{belief_lo:.0f},\,{belief_hi:.0f}]$"
    "\n"
    fr"Actual: $\bar{{x}} \,=\, {mean:.1f}$ years"
    "\n"
    fr"Gap: $\bar{{x}} - \mathrm{{Me}} \,=\, {mean - med:.1f}$"
    "\n"
    r"$\bf{Verdict}$: belief is "
    "\n"
    r"$\bf{not\ reasonable}$ — actual"
    "\n"
    fr"mean is $\,{mean - belief_hi:.1f}\,$ years above"
    "\n"
    fr"the upper endpoint {belief_hi:.0f}."
)
ax2.text(0.06, 0.96, txt, ha="left", va="top", fontsize=10.4,
         color=PALETTE["primary"])

ax2.text(0.06, 0.04,
         'R:  distr.summary.x(Age,\n'
         '         stats="mean",\n'
         '         digits=1,\n'
         '         data=customer_habits)\n'
         '##   n  n.a   mean\n'
         '## 34866  0   47.1',
         ha="left", va="bottom", fontsize=8.6, family="monospace",
         color=PALETTE["neutral"])

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
