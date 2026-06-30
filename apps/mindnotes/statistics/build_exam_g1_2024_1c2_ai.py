"""AI walkthrough for G1-2024 Ex1.c2 — Experience effect on Read2."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2024_1c2_ai.png"

beta, se, tval, pval = 0.4227, 0.1227, 3.446, 0.000598
df_res = 824
tc = stats.t.ppf(0.975, df_res)
lo, hi = beta - tc*se, beta + tc*se

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.8),
                               gridspec_kw={"width_ratios":[1.0, 1.2]})

# Left: coefficient CI vs 0
ax1.errorbar([beta], [0], xerr=[[beta-lo],[hi-beta]],
             fmt="o", color=PALETTE["primary"], ecolor=PALETTE["primary"],
             elinewidth=3, capsize=12, markersize=11)
ax1.axvline(0, ls="--", color=PALETTE["warn"], lw=1.6, label="zero (no effect)")
ax1.axvspan(lo, hi, color=PALETTE["accent"], alpha=0.25,
            label=f"95% CI = [{lo:.3f},  {hi:.3f}]")
ax1.text(beta, 0.35, f"$\\hat\\beta$ = {beta:.4f}",
         ha="center", fontsize=12, color=PALETTE["primary"], fontweight="bold")
ax1.set_xlim(-0.05, hi+0.1)
ax1.set_ylim(-1, 1)
ax1.set_yticks([])
ax1.set_xlabel(r"$\hat\beta_{\rm Experience}$  (Read2 points per extra year)")
ax1.set_title("Slope estimate + 95% CI")
ax1.legend(loc="upper right", framealpha=0.95)

# Right: marginal effect over realistic Experience range
xs = np.arange(0, 41)
eff = beta * xs
ax2.plot(xs, eff, color=PALETTE["primary"], lw=2.5, label="$\\hat\\beta_{\\rm Exp}\\cdot x$")
ax2.fill_between(xs, lo*xs, hi*xs, color=PALETTE["accent"], alpha=0.30,
                 label="95% CI band")
# Highlight typical year ranges
for x0 in [5, 10, 20, 40]:
    ax2.scatter([x0], [beta*x0], color=PALETTE["warn"], zorder=5)
    ax2.text(x0, beta*x0 + 0.3, f"+{beta*x0:.1f}", ha="center",
             fontsize=10, color=PALETTE["warn"], fontweight="bold")
ax2.set_xlabel("teacher Experience  (years)")
ax2.set_ylabel("ceteris-paribus change in Read2")
ax2.set_title("Effect on Read2 for a teacher with $x$ years of experience")
ax2.legend(loc="upper left", framealpha=0.95)

txt = (f"$\\hat\\beta$ = {beta},   SE = {se}\n"
       f"$t$ = {tval},   $p$ = {pval:.4f}\n"
       f"95% CI:  [{lo:.3f},  {hi:.3f}]\n\n"
       "0 ∉ CI  AND  $p \\ll 0.05$\n"
       "=>  Experience effect on Read2\n"
       "    is positive & significant\n"
       "    (ceteris paribus,\n"
       "     +1 year ≈ +0.42 reading points).")
ax2.text(0.98, 0.03, txt, transform=ax2.transAxes, ha="right", va="bottom",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle("G1-2024 Ex1.c2 — Effect of teacher Experience on Read2 (ceteris paribus)",
             fontsize=12.5, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
