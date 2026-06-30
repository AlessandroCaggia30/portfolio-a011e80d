"""AI walkthrough for G1-2024 Ex1.b3 — Chi-sq independence SchoolLoc x Lunch."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2024_1b3_ai.png"

x2_obs = 189.79; df = 3
crit_01 = stats.chi2.ppf(0.99, df)
crit_05 = stats.chi2.ppf(0.95, df)

xx = np.linspace(0, max(20, crit_01*1.3), 1200)
yy = stats.chi2.pdf(xx, df)

fig, ax = plt.subplots(figsize=(12, 6))
ax.plot(xx, yy, color=PALETTE["primary"], lw=2.4, label=f"$\\chi^2_{df}$ density (under $H_0$)")
ax.fill_between(xx[xx >= crit_05], 0, yy[xx >= crit_05], color=PALETTE["accent"], alpha=0.30,
                label=f"reject at 5%  ($X^2 > {crit_05:.2f}$)")
ax.fill_between(xx[xx >= crit_01], 0, yy[xx >= crit_01], color=PALETTE["warn"], alpha=0.40,
                label=f"reject at 1%  ($X^2 > {crit_01:.2f}$)")
ax.axvline(crit_05, ls="--", color=PALETTE["accent"], lw=1.2)
ax.axvline(crit_01, ls="--", color=PALETTE["warn"], lw=1.4)

# Mark observed (off the chart)
ax.annotate(f"$X^2_{{\\rm obs}}$ = {x2_obs:.2f}\n   (off chart →)\np-value ≈ 0",
            xy=(xx[-1]*0.92, max(yy)*0.05),
            xytext=(xx[-1]*0.55, max(yy)*0.55),
            arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=2.0),
            fontsize=12, color=PALETTE["warn"], fontweight="bold",
            bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                      boxstyle="round,pad=0.45", linewidth=1.0))

ax.set_xlabel("$X^2$  statistic")
ax.set_ylabel("density")
ax.set_title("G1-2024 Ex1.b3 — Chi-sq test of independence  (SchoolLoc × Lunch,  df = 3)\n"
             "$X^2_{\\rm obs} = 189.79$  >>  critical values  =>  reject $H_0$ at any usual $\\alpha$")
ax.legend(loc="upper right", framealpha=0.95)
ax.set_ylim(0, max(yy)*1.15)

obs_tab = np.array([[21, 178],[233, 146],[155, 56],[24, 19]])
rowtot = obs_tab.sum(1, keepdims=True); coltot = obs_tab.sum(0, keepdims=True); n = obs_tab.sum()
exp = rowtot @ coltot / n
chi2 = ((obs_tab - exp)**2 / exp).sum()

txt = ("Pearson:  $X^2 = \\sum (O_{jk} - E_{jk})^2 / E_{jk}$\n"
       f"          $E_{{jk}} = R_j C_k / n$\n\n"
       f"Hand-computed:  $X^2$ = {chi2:.2f}\n"
       f"df = (4 − 1)(2 − 1) = 3\n"
       f"p-value = P($\\chi^2_3$ ≥ {x2_obs:.2f}) ≈ 0\n\n"
       "=> SchoolLoc and Lunch are NOT independent.")
ax.text(0.02, 0.97, txt, transform=ax.transAxes, ha="left", va="top",
        fontsize=10, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (X2 reproduced = {chi2:.4f})")
