"""AI walkthrough for G1-2024 Ex2.a — One-sided z test mean ReadGrowth (mu0=62, 1%)."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2024_ex2_a_ai.png"

mu0 = 62; sigma = 34; n = 832
se = sigma/np.sqrt(n)
z_crit = stats.norm.ppf(0.01)
x_crit = mu0 + z_crit*se
xbar = 60.01923
z_obs = (xbar - mu0)/se
p_val = stats.norm.cdf(z_obs)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 5.8))

# Left: distribution of xbar under H0 boundary mu=62
xx = np.linspace(mu0 - 5*se, mu0 + 5*se, 600)
yy = stats.norm.pdf(xx, mu0, se)
ax1.plot(xx, yy, color=PALETTE["primary"], lw=2.3,
         label=f"$\\bar X \\mid \\mu=62$  ~ $N(62, {se:.3f}^2)$")
ax1.fill_between(xx[xx <= x_crit], 0, yy[xx <= x_crit],
                 color=PALETTE["warn"], alpha=0.40,
                 label=f"reject  ($\\bar X < {x_crit:.3f}$)\n(area = 1% = α)")
ax1.axvline(mu0, color=PALETTE["neutral"], ls="--", lw=1.0)
ax1.axvline(x_crit, color=PALETTE["warn"], lw=1.7)
ax1.axvline(xbar, color=PALETTE["ok"], lw=2.0,
            label=f"observed  $\\bar x = {xbar:.3f}$")
ax1.annotate(f"$\\bar x_{{\\rm obs}}$ = {xbar:.3f}\n  NOT in RR",
             xy=(xbar, stats.norm.pdf(xbar, mu0, se)),
             xytext=(xbar+0.6, max(yy)*0.55),
             arrowprops=dict(arrowstyle="->", color=PALETTE["ok"], lw=1.6),
             fontsize=10.5, color=PALETTE["ok"], fontweight="bold")
ax1.set_xlabel(r"$\bar X$  (sample mean ReadGrowth)")
ax1.set_ylabel("density")
ax1.set_title(f"One-sided z test  H0: μ ≥ 62  vs  H1: μ < 62  at α = 1%\n"
              f"Rejection region:  $\\bar X < {x_crit:.3f}$  (equivalently  Z < {z_crit:.3f})")
ax1.legend(loc="upper right", framealpha=0.95, fontsize=9.5)

# Right: number box
ax2.axis("off")
ax2.text(0.05, 0.95,
         "Setup\n"
         f"  μ_0 = {mu0},  σ = {sigma} (known),  n = {n}\n"
         f"  SE = σ / √n = {se:.4f}\n\n"
         "Statistic (large sample, σ known)\n"
         "  Z = (X̄ − μ_0) / (σ / √n)  ~  N(0,1) under H0\n\n"
         "Rejection rule (one-sided, lower tail)\n"
         f"  reject if  Z < z_α = z_0.01 = {z_crit:.4f}\n"
         f"  equivalently  X̄ < μ_0 − |z_α|·σ/√n = {x_crit:.4f}\n\n"
         "Sample\n"
         f"  x̄ = {xbar:.4f}\n"
         f"  Z_obs = ({xbar:.4f} − {mu0}) / {se:.4f} = {z_obs:.4f}\n"
         f"  p-value = P(Z < {z_obs:.4f}) = {p_val:.4f}\n\n"
         "Conclusion\n"
         f"  {xbar:.4f}  >  {x_crit:.4f}   AND   {z_obs:.4f}  >  {z_crit:.4f}\n"
         "  => DO NOT REJECT H0 at 1%.\n"
         "  No evidence that μ < 62 -> no support program.\n"
         "  (At 5% we WOULD reject:  p = 0.046  <  0.05.)",
         transform=ax2.transAxes, ha="left", va="top",
         fontsize=10.5, color=PALETTE["primary"], family="monospace",
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.55", linewidth=1.0))

fig.suptitle("G1-2024 Ex2.a — Lower-tail z test on mean ReadGrowth (σ = 34 known)",
             fontsize=12.5, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (x_crit={x_crit:.4f}, z_obs={z_obs:.4f}, p={p_val:.5f})")
