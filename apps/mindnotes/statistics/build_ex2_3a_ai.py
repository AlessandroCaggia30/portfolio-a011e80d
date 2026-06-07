"""Build AI walkthrough plot for Ex 2.3a — comparing dispersion of AmountSpent
across two companies (DS vs. competitor).

The exercise has two layers:
  (i)  ABSOLUTE dispersion — variance / SD, in USD and USD²;
  (ii) RELATIVE dispersion — coefficient of variation CV = SD / mean,
       which is a pure number and is the correct comparison when the
       means differ.

The plot shows both: left panel = SD bars in absolute USD with the
mean lines overlaid, right panel = CV bars (pure number). The verdict
flips between panels — DS has slightly higher absolute dispersion,
the competitor has higher relative dispersion (higher CV).
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_3a_ai.png"

# --- Given data ---
# DS (our company)
n_DS    = 750
mean_DS = 1228.44
var_DS  = 940900.9
sd_DS   = np.sqrt(var_DS)              # 970.51
cv_DS   = sd_DS / mean_DS              # 0.79

# Competitor
n_C    = 620
total_C = 682000.0
mean_C  = total_C / n_C                 # 1100.00
var_C   = 921486.0
sd_C    = np.sqrt(var_C)                # 960.0
cv_C    = sd_C / mean_C                 # 0.8727204

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.3))

labels = ["DS\n(n=750)", "Competitor\n(n=620)"]
x      = np.arange(len(labels))

# ===================================================================
# LEFT: ABSOLUTE dispersion — SD bars + mean reference lines
# ===================================================================
sd_vals   = [sd_DS, sd_C]
mean_vals = [mean_DS, mean_C]
w = 0.42

b_sd = ax1.bar(x, sd_vals, w,
               color=[PALETTE["primary"], PALETTE["accent"]],
               edgecolor="#2a3142", linewidth=0.9,
               label=r"$s = \sqrt{s^{2}}$  (SD, USD)")
for b, v in zip(b_sd, sd_vals):
    ax1.text(b.get_x() + b.get_width()/2, v + 12, f"{v:.2f}",
             ha="center", va="bottom", fontsize=11, fontweight="bold")

# overlay mean lines (as horizontal segments above each bar)
for xi, m in zip(x, mean_vals):
    ax1.hlines(m, xi - w/2 - 0.05, xi + w/2 + 0.05,
               colors=PALETTE["warn"], linewidth=2.0, linestyles="--",
               zorder=4)
    ax1.text(xi + w/2 + 0.07, m, fr"$\bar x = {m:,.2f}$",
             ha="left", va="center", color=PALETTE["warn"], fontsize=10.5,
             fontweight="bold")

ax1.set_xticks(x); ax1.set_xticklabels(labels)
ax1.set_ylabel("USD")
ax1.set_title("Absolute dispersion: SD (bars) vs. mean (dashed)")
ax1.set_ylim(0, max(max(sd_vals), max(mean_vals)) * 1.30)

ax1.text(0.02, 0.97,
         f"$s^2_{{DS}}={var_DS:,.1f}$  USD$^2$\n"
         f"$s^2_{{C}}\\,={var_C:,.1f}$  USD$^2$\n"
         f"In absolute terms DS is\n"
         f"slightly more dispersed.",
         transform=ax1.transAxes, ha="left", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))
ax1.legend(loc="upper right", framealpha=0.95)

# ===================================================================
# RIGHT: RELATIVE dispersion — CV (pure number)
# ===================================================================
cv_vals = [cv_DS, cv_C]
b_cv = ax2.bar(x, cv_vals, w,
               color=[PALETTE["primary"], PALETTE["accent"]],
               edgecolor="#2a3142", linewidth=0.9,
               label=r"$\mathrm{CV} = s / \bar x$  (pure number)")
for b, v in zip(b_cv, cv_vals):
    ax2.text(b.get_x() + b.get_width()/2, v + 0.012, f"{v:.4f}",
             ha="center", va="bottom", fontsize=11, fontweight="bold")

# annotate the winner (higher CV) with a marker
i_max = int(np.argmax(cv_vals))
ax2.annotate("higher CV\n(more variable in\nrelative terms)",
             xy=(x[i_max], cv_vals[i_max]),
             xytext=(x[i_max] - 0.45, cv_vals[i_max] + 0.13),
             fontsize=10.5, color=PALETTE["warn"], fontweight="bold",
             ha="center",
             arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.2))

ax2.set_xticks(x); ax2.set_xticklabels(labels)
ax2.set_ylabel("CV")
ax2.set_title("Relative dispersion: coefficient of variation")
ax2.set_ylim(0, max(cv_vals) * 1.45)

ax2.text(0.02, 0.97,
         f"$\\mathrm{{CV}}_{{DS}} = {sd_DS:.2f}/{mean_DS:,.2f} = {cv_DS:.4f}$\n"
         f"$\\mathrm{{CV}}_{{C}}\\, = {sd_C:.2f}/{mean_C:,.2f} = {cv_C:.4f}$\n"
         "Verdict flips: in RELATIVE\n"
         "terms the competitor is more\n"
         "dispersed (CV is higher).",
         transform=ax2.transAxes, ha="left", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))
ax2.legend(loc="upper right", framealpha=0.95)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  DS:         mean={mean_DS:.2f}  sd={sd_DS:.4f}  cv={cv_DS:.6f}")
print(f"  Competitor: mean={mean_C:.2f}  sd={sd_C:.4f}  cv={cv_C:.6f}")
