"""AI walkthrough for G1-2024 Ex2.b — Two-proportion z test, rural vs inner-city."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2024_ex2_b_ai.png"

# Numbers (verified in R)
x_r, n_r = 171, 379
x_i, n_i = 103, 199
p_r = x_r/n_r; p_i = x_i/n_i
p0  = (x_r + x_i)/(n_r + n_i)
se0 = np.sqrt(p0*(1-p0)*(1/n_r + 1/n_i))
z_obs = (p_r - p_i)/se0
p_val = stats.norm.cdf(z_obs)
crit5 = stats.norm.ppf(0.05)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 5.8))

# Left: bar comparison of proportions
ax1.bar(["rural", "inner-city"], [p_r, p_i],
        color=[PALETTE["primary"], PALETTE["accent"]],
        alpha=0.85, edgecolor="black", width=0.55,
        yerr=[np.sqrt(p_r*(1-p_r)/n_r), np.sqrt(p_i*(1-p_i)/n_i)],
        capsize=10, error_kw=dict(elinewidth=1.5, ecolor=PALETTE["neutral"]))
for i, (p, n) in enumerate([(p_r, n_r), (p_i, n_i)]):
    ax1.text(i, p + 0.025, f"{p:.4f}\n({int(p*n)}/{n})",
             ha="center", fontsize=10.5, color=PALETTE["primary"], fontweight="bold")
ax1.set_ylim(0, 0.70)
ax1.set_ylabel(r"$\hat p$  (P(ReadGrowth < 62))")
ax1.set_title(r"Sample proportions  $\hat p_{\rm rural}$  vs  $\hat p_{\rm inner-city}$")

# Right: null distribution of Z and observed
zz = np.linspace(-4, 4, 600)
yy = stats.norm.pdf(zz)
ax2.plot(zz, yy, color=PALETTE["primary"], lw=2.3, label="N(0,1) under H0")
ax2.fill_between(zz[zz <= crit5], 0, yy[zz <= crit5],
                 color=PALETTE["warn"], alpha=0.35,
                 label=f"reject at 5%  (Z < {crit5:.3f})")
ax2.fill_between(zz[zz <= z_obs], 0, yy[zz <= z_obs],
                 color=PALETTE["ok"], alpha=0.50,
                 label=f"p-value = {p_val:.4f}")
ax2.axvline(z_obs, color=PALETTE["ok"], lw=2.0)
ax2.text(z_obs, max(yy)*0.5, f"$z_{{\\rm obs}}$ = {z_obs:.3f}",
         ha="right", fontsize=11, color=PALETTE["ok"], fontweight="bold",
         rotation=90)
ax2.axvline(crit5, color=PALETTE["warn"], lw=1.6, ls="--")
ax2.set_xlabel("Z")
ax2.set_ylabel("density")
ax2.set_title("Null sampling distribution of Z  (one-sided, lower tail)")
ax2.legend(loc="upper right", framealpha=0.95, fontsize=9.5)

txt = (f"H0:  p_R  ≥  p_I        H1:  p_R  <  p_I\n\n"
       f"p̂_R = {x_r}/{n_r} = {p_r:.4f}\n"
       f"p̂_I = {x_i}/{n_i} = {p_i:.4f}\n"
       f"p̂_0 = ({x_r}+{x_i})/({n_r}+{n_i}) = {p0:.4f}\n"
       f"SE_0 = √(p̂_0(1−p̂_0)(1/n_R + 1/n_I)) = {se0:.4f}\n\n"
       f"Z_obs = ({p_r:.4f} − {p_i:.4f}) / {se0:.4f}\n"
       f"      = {z_obs:.4f}\n\n"
       f"p-value = P(Z < {z_obs:.4f}) = {p_val:.4f}\n\n"
       f"At α = 5%:  p = {p_val:.4f}  >  0.05  =>  DO NOT REJECT.\n"
       f"(But it WOULD reject at α = 10%.)")
ax2.text(0.02, 0.97, txt, transform=ax2.transAxes, ha="left", va="top",
         fontsize=9.5, color=PALETTE["primary"], family="monospace",
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle("G1-2024 Ex2.b — Two-proportion z test  (ReadGrowth < 62: rural vs inner-city)",
             fontsize=12.5, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (z={z_obs:.4f}, p={p_val:.5f})")
