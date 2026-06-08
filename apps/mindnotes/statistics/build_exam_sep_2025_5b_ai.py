"""AI walkthrough for past-exam Sep-2025 Ex5b — Levene's test for equal variances.

H0: sigma_A^2 = sigma_B^2  vs  H1: sigma_A^2 != sigma_B^2
F_obs = 0.41,  df1 = 1,  df2 = 436,  p-value = 0.524  (s_A^2 = 44.42, s_B^2 = 42.66)

Two panels:
  Left  : F(1, 436) null density with right-tail rejection regions at alpha = {0.01, 0.05, 0.10}
          shaded and F_obs marker; p-value (right-tail area) annotated.
  Right : group sample variances bar chart with relative difference annotated.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import f as f_dist

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_sep_2025_5b_ai.png"

df1, df2 = 1, 436
F_obs = 0.41
pval = 1 - f_dist.cdf(F_obs, df1, df2)
s2A, s2B = 44.42, 42.66
nA, nB = 58, 380

fig, axes = plt.subplots(1, 2, figsize=(13, 5))

# --- LEFT: F(1, 436) null density with rejection regions + F_obs ---
ax = axes[0]
x = np.linspace(0.001, 8, 800)
pdf = f_dist.pdf(x, df1, df2)
ax.plot(x, pdf, color=PALETTE["primary"], linewidth=2.2,
        label=f"$F_{{{df1},{df2}}}$ null density")

# Right-tail rejection regions for alpha = 0.10, 0.05, 0.01
alphas = [0.10, 0.05, 0.01]
shades = [0.18, 0.30, 0.55]
for a, sh in zip(alphas, shades):
    Fc = f_dist.ppf(1 - a, df1, df2)
    mask = x >= Fc
    ax.fill_between(x[mask], 0, pdf[mask], color=PALETTE["warn"], alpha=sh,
                    label=f"reject region $\\alpha = {a}$  (F > {Fc:.2f})")

# F_obs marker
ax.axvline(F_obs, color=PALETTE["primary"], linestyle="--", linewidth=1.6)
ax.annotate(f"$F_{{\\mathrm{{obs}}}} = {F_obs}$\np-value $\\approx {pval:.3f}$",
            xy=(F_obs, f_dist.pdf(F_obs, df1, df2)),
            xytext=(2.2, 1.2), fontsize=11, color=PALETTE["primary"],
            arrowprops=dict(arrowstyle="->", color=PALETTE["primary"]))

ax.set_xlabel("$F$ statistic")
ax.set_ylabel(f"$F_{{{df1},{df2}}}$ density")
ax.set_title("Step 1 — $F_{\\mathrm{obs}}$ falls well inside the 'fail-to-reject' zone\n"
             f"$p = P(F_{{{df1},{df2}}} > {F_obs}) \\approx {pval:.3f}$ $>$ 0.10")
ax.set_xlim(0, 8)
ax.set_ylim(0, 1.8)
ax.legend(loc="upper right", framealpha=0.95, fontsize=9)

# --- RIGHT: group sample variances bar chart ---
ax2 = axes[1]
groups = ["A", "B"]
variances = [s2A, s2B]
ns = [nA, nB]
colors = [PALETTE["accent"], PALETTE["primary"]]
bars = ax2.bar(groups, variances, color=colors, edgecolor=PALETTE["primary"],
               linewidth=1.4, alpha=0.75, width=0.55)

for b, v, n in zip(bars, variances, ns):
    ax2.text(b.get_x() + b.get_width()/2, v + 1.2,
             f"$s^2 = {v}$\n$n = {n}$",
             ha="center", va="bottom", fontsize=11, color=PALETTE["primary"])

rel = abs(s2A - s2B) / max(s2A, s2B) * 100
ax2.set_ylim(0, max(variances) * 1.35)
ax2.set_ylabel("sample variance of `Performance`")
ax2.set_xlabel("`Activity.type` group")
ax2.set_title(f"Step 2 — Group variances are almost identical\n"
              f"relative gap $|s_A^2 - s_B^2|/\\max \\approx {rel:.1f}\\%$  $\\Rightarrow$  pooled-variance OK")

# R-command callout box
ax2.text(0.02, 0.05,
         "R command:\n"
         "CI.diffmean(..., var.test=TRUE)\n"
         "# Levene: F=0.41, df=(1, 436)\n"
         f"# p-value = {pval:.4f}  ->  keep var.equal=TRUE",
         transform=ax2.transAxes, ha="left", va="bottom",
         fontsize=10, family="monospace",
         bbox=dict(facecolor="#fffbe6",
                   edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle("Levene's test  $H_0: \\sigma_A^2 = \\sigma_B^2$  vs  $H_1: \\sigma_A^2 \\neq \\sigma_B^2$  "
             f"$\\Rightarrow$  $F = {F_obs}$, $p \\approx {pval:.3f}$ — fail to reject $H_0$",
             fontsize=12, y=1.02, color=PALETTE["primary"])

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  F_obs={F_obs}  df=({df1},{df2})  p={pval:.5f}")
