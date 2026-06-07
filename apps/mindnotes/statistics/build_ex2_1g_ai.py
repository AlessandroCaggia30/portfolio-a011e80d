"""Build AI walkthrough plot for Ex 2.1g — dispersion measures for Price.

Visual: two-panel figure that makes the four dispersion indices
(range, IQR, sd, CV) tangible on the actual Price distribution.

LEFT panel
  Histogram of Price with overlays:
    - vertical mean and median lines,
    - one-sigma band [mean - sd, mean + sd] (forest-green shaded),
    - IQR span [Q1, Q3] as a bracket above the histogram (sky-blue),
    - full range span [min, max] as a wider bracket above (navy).
  Each bracket carries the *numerical width* so the reader can directly
  compare range = 6.5 > IQR = 2.5 > sd = 1.47 and see why range > IQR.

RIGHT panel
  Arithmetic / formula card listing each index with its value and the
  R command (distr.summary.x(... stats="dispersion")). The CV is given
  both in decimal (0.23) and percentage (23.4%) form so the reader sees
  the "scale-free" character of CV.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import pyreadr
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_1g_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# --- Load Price from the same dataset used by ex1/ex2 builders ---
result = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata")
price = result["pizzerie"]["Price"].to_numpy()

mn   = float(np.min(price))                     # 3.5
mx   = float(np.max(price))                     # 10.0
mean = float(np.mean(price))                    # 6.29
med  = float(np.median(price))                  # 6.0
q1   = float(np.quantile(price, 0.25))          # 5.0
q3   = float(np.quantile(price, 0.75))          # 7.5
sd   = float(np.std(price, ddof=1))             # 1.4739831...
var_ = float(np.var(price, ddof=1))             # 2.1726262...
rng  = mx - mn                                  # 6.5
iqr  = q3 - q1                                  # 2.5
cv   = sd / mean                                # 0.2343375...

print(f"  n={len(price)}  mean={mean:.4f}  median={med}  sd={sd:.4f}")
print(f"  range={rng}  IQR={iqr}  var={var_:.4f}  CV={cv:.4f}")

# =====================================================================
# Layout
# =====================================================================
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.4),
                               gridspec_kw={"width_ratios": [2.4, 1]})

# =====================================================================
# LEFT — histogram + dispersion overlays
# =====================================================================
bins = np.arange(3.0, 10.6, 0.5)
counts, edges, patches = ax1.hist(price, bins=bins,
                                  color=PALETTE["secondary"],
                                  edgecolor=PALETTE["primary"],
                                  alpha=0.55, linewidth=1.0,
                                  label="Price (n = 100)")

ymax = counts.max()
ax1.set_ylim(0, ymax * 1.65)        # headroom for the brackets above

# One-sigma band around the mean (forest-green)
ax1.axvspan(mean - sd, mean + sd,
            ymin=0.0, ymax=counts.max() / (ymax * 1.65),
            color=PALETTE["ok"], alpha=0.12,
            label=fr"$\bar{{x}} \pm s$  (width $2s = {2*sd:.2f}$)")

# Mean & median vertical lines
ax1.axvline(mean, color=PALETTE["primary"], lw=1.8, ls="-",
            label=fr"mean $\bar{{x}} = {mean:.2f}$")
ax1.axvline(med,  color=PALETTE["accent"], lw=1.6, ls="--",
            label=fr"median $= {med:.1f}$")

# Brackets above the histogram for IQR and range
def _bracket(xa, xb, y, color, label):
    ax1.annotate("", xy=(xa, y), xytext=(xb, y),
                 arrowprops=dict(arrowstyle="|-|", color=color,
                                 lw=1.8, shrinkA=0, shrinkB=0))
    ax1.text((xa + xb) / 2, y + 0.7, label,
             ha="center", va="bottom", color=color,
             fontsize=10.5, fontweight="bold")

y_iqr  = ymax * 1.10
y_rng  = ymax * 1.40
_bracket(q1, q3, y_iqr, PALETTE["secondary"],
         fr"IQR = $Q_3 - Q_1 = {iqr:.1f}$")
_bracket(mn, mx, y_rng, PALETTE["primary"],
         fr"range = max $-$ min = {rng:.1f}")

# Tick marks for min/Q1/median/Q3/max on the x-axis
for x, tag, col in [(mn,  "min",  PALETTE["primary"]),
                    (q1,  r"$Q_1$", PALETTE["secondary"]),
                    (med, "Me",   PALETTE["accent"]),
                    (q3,  r"$Q_3$", PALETTE["secondary"]),
                    (mx,  "max",  PALETTE["primary"])]:
    ax1.text(x, -ymax * 0.13, f"{tag}\n{x:.2f}".rstrip("0").rstrip("."),
             ha="center", va="top", fontsize=9.5, color=col)

ax1.set_xlabel("Price  (EUR)")
ax1.set_ylabel("Counts")
ax1.set_title("Ex 2.1g — Dispersion of Price: range $>$ IQR $>$ sd")
ax1.legend(loc="upper right", framealpha=0.95, fontsize=9.5)

# =====================================================================
# RIGHT — arithmetic / R panel
# =====================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)
ax2.add_patch(FancyBboxPatch((0.02, 0.04), 0.96, 0.92,
                             boxstyle="round,pad=0.02",
                             linewidth=1.1,
                             facecolor="#fbfbfd",
                             edgecolor=PALETTE["primary"]))

txt = (
    r"$\bf{Dispersion\ indices\ for\ Price}$"
    "\n"
    r"$\mathrm{range} = \max - \min$"
    "\n"
    fr"$\quad = {mx:.1f} - {mn:.1f} = {rng:.1f}$"
    "\n"
    r"$\mathrm{IQR} = Q_3 - Q_1$"
    "\n"
    fr"$\quad = {q3:.1f} - {q1:.1f} = {iqr:.1f}$"
    "\n"
    r"$s^{2} = \frac{1}{n-1}\sum_{i}(x_i - \bar{x})^{2}$"
    "\n"
    fr"$\quad = {var_:.2f}\;\mathrm{{EUR}}^{{2}}$"
    "\n"
    fr"$s = \sqrt{{s^{{2}}}} = {sd:.2f}\;\mathrm{{EUR}}$"
    "\n"
    r"$\mathrm{CV} = s\,/\,\bar{x}$"
    "\n"
    fr"$\quad = {sd:.2f}/{mean:.2f} = {cv:.2f}\;({cv*100:.1f}\%)$"
)
ax2.text(0.06, 0.96, txt, ha="left", va="top", fontsize=10.2,
         color=PALETTE["primary"])

ax2.text(0.06, 0.04,
         'R:  distr.summary.x(x=Price,\n'
         '         data=pizzerie,\n'
         '         stats="dispersion")\n'
         '##  range IQrange  sd  var   cv\n'
         '##   6.5    2.5   1.47 2.17 0.23',
         ha="left", va="bottom", fontsize=8.6, family="monospace",
         color=PALETTE["neutral"])

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
