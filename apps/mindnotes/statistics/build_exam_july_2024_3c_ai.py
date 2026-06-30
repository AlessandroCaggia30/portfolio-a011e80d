"""AI walkthrough for Jul-2024 Ex3.c — R^2 / adj R^2 / global F-test visualisation."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2024_3c_ai.png"

# Numbers from R
R2     = 0.7117
adjR2  = 0.7074
F_obs  = 165.02
df1, df2 = 6, 401
n      = 408
# critical F at 1%
from math import lgamma, exp
def f_pdf(x, d1, d2):
    if x <= 0: return 0
    num = (d1/d2)**(d1/2) * x**(d1/2 - 1)
    den = (1 + d1*x/d2)**((d1+d2)/2)
    Bln = lgamma(d1/2) + lgamma(d2/2) - lgamma((d1+d2)/2)
    return num / den / exp(Bln)
F_crit_99 = 2.852

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.8),
                               gridspec_kw={"width_ratios":[1.0, 1.3]})

# Left: variance decomposition donut
sizes = [R2, 1 - R2]
labels = [f"explained\n$R^2$ = {R2:.4f}", f"residual\n$1-R^2$ = {1-R2:.4f}"]
colors = [PALETTE["primary"], PALETTE["muted"]]
wedges, _ = ax1.pie(sizes, colors=colors, startangle=90, counterclock=False,
                    wedgeprops=dict(width=0.42, edgecolor="white", linewidth=2))
ax1.text(0, 0.05, f"$R^2$ = {R2:.4f}", ha="center", va="center",
         fontsize=15, color=PALETTE["primary"], fontweight="bold")
ax1.text(0, -0.18, f"adj $R^2$ = {adjR2:.4f}", ha="center", va="center",
         fontsize=11, color=PALETTE["neutral"])
ax1.set_title("Variance decomposition of Outstate")
ax1.legend(wedges, labels, loc="lower center",
           bbox_to_anchor=(0.5, -0.18), ncol=2, framealpha=0.95, fontsize=10)

# Right: F-distribution with critical region
xx = np.linspace(0.01, 6.5, 600)
yy = np.array([f_pdf(v, df1, df2) for v in xx])
ax2.fill_between(xx, 0, yy, where=(xx >= F_crit_99), color=PALETTE["warn"], alpha=0.45,
                 label=f"rejection region @ 1% (F > {F_crit_99:.2f})")
ax2.plot(xx, yy, color=PALETTE["primary"], lw=2.0)
ax2.axvline(F_crit_99, color=PALETTE["warn"], lw=1.5, ls="--")
ax2.annotate(f"observed F = {F_obs:.1f}\n(off the chart, p < 2.2e-16)",
             xy=(6.4, 0.02), xytext=(3.2, 0.55),
             arrowprops=dict(arrowstyle="->", color=PALETTE["primary"], lw=1.5),
             fontsize=11, color=PALETTE["primary"],
             bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                       boxstyle="round,pad=0.4", linewidth=1.0))
ax2.set_xlabel(f"$F$  with df1 = {df1}, df2 = {df2}")
ax2.set_ylabel("density")
ax2.set_title(f"Global $F$-test:  $F$ = {F_obs:.1f}  vs  $F_{{0.99}}$ = {F_crit_99:.2f}")
ax2.legend(loc="upper right", framealpha=0.95)
ax2.set_xlim(0, 6.5)

fig.suptitle("Jul-2024 Ex3.c — Overall fit and global significance",
             fontsize=12.5, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
