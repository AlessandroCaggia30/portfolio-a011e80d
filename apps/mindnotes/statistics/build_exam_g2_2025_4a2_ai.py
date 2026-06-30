"""AI walkthrough for G2-2025 Ex4.a — Global F-test of modA."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import f as fdist

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g2_2025_4a2_ai.png"

# Hard-coded from R (verified):
F_obs = 7.097
df1 = 6
df2 = 493
alpha = 0.05
Fcrit = fdist.ppf(1 - alpha, df1, df2)
pval = fdist.sf(F_obs, df1, df2)

xs = np.linspace(0, max(F_obs * 1.2, Fcrit * 2.0), 600)
ys = fdist.pdf(xs, df1, df2)

fig, ax = plt.subplots(figsize=(11, 5.8))
ax.plot(xs, ys, color=PALETTE["primary"], lw=2.0,
        label=fr"$F_{{{df1},{df2}}}$ null density")
mask = xs >= Fcrit
ax.fill_between(xs[mask], 0, ys[mask], color=PALETTE["warn"], alpha=0.35,
                label=fr"Rejection region ($F > F_{{0.95}} = {Fcrit:.3f}$)")
ax.axvline(F_obs, color=PALETTE["accent"], lw=2.2,
           label=fr"$F_{{obs}} = {F_obs}$")
ax.axvline(Fcrit, color=PALETTE["warn"], lw=1.4, ls="--")

ax.set_xlabel("F")
ax.set_ylabel("density")
ax.set_title(rf"G2-2025 Ex4.a — Global F-test of modA"
             rf" ($k={df1}$, $n-k-1={df2}$)")

ax.text(0.55, 0.85,
        r"$H_0: \beta_1=\dots=\beta_6=0$" "\n"
        r"$H_1: \exists j:\, \beta_j\neq 0$" "\n"
        f"$F_{{obs}} = {F_obs}$\n"
        f"p-value = $\\Pr(F_{{{df1},{df2}}} > {F_obs}) = {pval:.2e}$\n"
        r"$\Rightarrow$ reject $H_0$ at 5%: modA is overall significant.",
        transform=ax.transAxes, ha="left", va="top",
        fontsize=10.5, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.4", linewidth=1.0))

ax.legend(loc="upper right", framealpha=0.95, fontsize=10)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"F_obs={F_obs} Fcrit={Fcrit:.4f} p={pval:.4e}")
