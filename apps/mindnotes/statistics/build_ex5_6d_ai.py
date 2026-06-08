"""AI walkthrough plot for Ex 5.6d -- SE of mean Post - Pre difference under
equal variances and correlation rho = 0.58 (paired data, dependent means).
Three panels:
  (1) Paired (Pre, Post) scatter showing positive within-employee correlation
      with the 45-degree reference line.
  (2) Variance reduction factor (1 - rho) as a function of rho, with the
      independent-samples baseline and the rho = 0.58 point highlighted.
  (3) Sampling distribution of the mean difference under independent vs
      paired sampling, illustrating the SE shrinkage.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex5/ex5_5_6d_ai.png"

# --- Worked-example numbers used in the snippet ---
n        = 120
s2_pre   = 38.0
s2_post  = 42.0
s2_pool  = (s2_pre + s2_post) / 2.0   # = 40
rho      = 0.58
mu_diff  = 0.5                         # illustrative mean change

se_paired = np.sqrt(2.0 * s2_pool * (1.0 - rho) / n)
se_indep  = np.sqrt(2.0 * s2_pool / n)

NAVY   = PALETTE["primary"]
YELLOW = PALETTE["accent"]
NEUT   = PALETTE["neutral"]

# --- Synthetic paired (Pre, Post) data with correlation ~ rho for panel 1 ---
rng     = np.random.default_rng(20260606)
mu_pre  = 70.0
sd      = np.sqrt(s2_pool)             # common sd
z1      = rng.normal(size=n)
z2      = rng.normal(size=n)
pre     = mu_pre + sd * z1
post    = mu_pre + mu_diff + sd * (rho * z1 + np.sqrt(1 - rho**2) * z2)
r_emp   = np.corrcoef(pre, post)[0, 1]

fig, (ax1, ax2, ax3) = plt.subplots(1, 3, figsize=(16, 5))

# ====================================================================
# LEFT -- paired scatter (Pre, Post), 45-degree line, positive correlation
# ====================================================================
ax1.scatter(pre, post, color=YELLOW, s=36, alpha=0.85,
            edgecolor="#2a3142", linewidth=0.6,
            label=fr"$n={n}$ paired employees")
lo = min(pre.min(), post.min()) - 2
hi = max(pre.max(), post.max()) + 2
ax1.plot([lo, hi], [lo, hi], ls="--", lw=1.2, color=NAVY, alpha=0.75,
         label="Post = Pre")
# overlay regression line to make the correlation visually obvious
b = np.cov(pre, post, ddof=1)[0, 1] / np.var(pre, ddof=1)
a = post.mean() - b * pre.mean()
xs_line = np.linspace(lo, hi, 50)
ax1.plot(xs_line, a + b * xs_line, color="#b85c00", lw=1.6,
         label=fr"OLS  (slope $\approx {b:.2f}$)")
ax1.set_xlim(lo, hi); ax1.set_ylim(lo, hi)
ax1.set_xlabel("Pre productivity")
ax1.set_ylabel("Post productivity")
ax1.set_title(r"Step 1 -- Same employees $\Rightarrow$ Pre, Post are dependent")
ax1.legend(loc="lower right", framealpha=0.95, fontsize=9)
ax1.text(0.03, 0.97,
         fr"$\widehat{{\rho}} \approx {r_emp:.2f}$"
         + "\n" + fr"target $\rho = {rho}$"
         + "\n" + r"$s^2_{\mathrm{pool}}=" + f"{s2_pool:.0f}" + r"$",
         transform=ax1.transAxes, ha="left", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=YELLOW,
                   boxstyle="round,pad=0.40", linewidth=1.0))

# ====================================================================
# MIDDLE -- variance-reduction factor (1 - rho) vs rho
# ====================================================================
rho_grid = np.linspace(-1.0, 1.0, 401)
factor   = 1.0 - rho_grid
ax2.plot(rho_grid, factor, color=NAVY, lw=2.2,
         label=r"$1 - \rho$  (paired factor)")
ax2.axhline(1.0, color=NEUT, ls="--", lw=1.2,
            label=r"$\rho = 0$ (independent baseline)")
ax2.axvline(rho, color=YELLOW, ls="--", lw=1.2)
ax2.scatter([rho], [1.0 - rho], color="#b85c00", s=90, zorder=5,
            edgecolor="#2a3142", linewidth=0.8,
            label=fr"$\rho = {rho}\Rightarrow 1-\rho = {1-rho:.2f}$")
ax2.fill_between(rho_grid, 0, factor, where=(rho_grid > 0),
                 color=YELLOW, alpha=0.15, label=r"$\rho>0$: SE shrinks")
ax2.set_xlabel(r"$\rho = \mathrm{cor}(\mathrm{Pre},\mathrm{Post})$")
ax2.set_ylabel(r"variance multiplier  $1 - \rho$")
ax2.set_title(r"Step 2 -- Positive $\rho$ shrinks $\mathrm{Var}(\bar X_{\rm Post}-\bar X_{\rm Pre})$")
ax2.set_xlim(-1.02, 1.02); ax2.set_ylim(-0.05, 2.1)
ax2.legend(loc="upper right", framealpha=0.95, fontsize=8.5)
ax2.text(0.03, 0.03,
         r"$\mathrm{Var}(\bar X_{\rm Post}-\bar X_{\rm Pre})$"
         + "\n" + r"$=\dfrac{2\sigma^2(1-\rho)}{n}$",
         transform=ax2.transAxes, ha="left", va="bottom", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=YELLOW,
                   boxstyle="round,pad=0.40", linewidth=1.0))

# ====================================================================
# RIGHT -- sampling distribution: independent vs paired (rho=0.58)
# ====================================================================
xs = np.linspace(mu_diff - 4 * se_indep, mu_diff + 4 * se_indep, 600)
ax3.plot(xs, norm.pdf(xs, loc=mu_diff, scale=se_indep),
         color=NEUT, lw=2.0,
         label=fr"independent: $SE = {se_indep:.3f}$")
ax3.fill_between(xs, norm.pdf(xs, loc=mu_diff, scale=se_indep), 0,
                 color=NEUT, alpha=0.18)
ax3.plot(xs, norm.pdf(xs, loc=mu_diff, scale=se_paired),
         color=NAVY, lw=2.2,
         label=fr"paired ($\rho=0.58$): $SE = {se_paired:.3f}$")
ax3.fill_between(xs, norm.pdf(xs, loc=mu_diff, scale=se_paired), 0,
                 color=YELLOW, alpha=0.30)
ax3.axvline(mu_diff, color="#b85c00", ls="--", lw=1.2,
            label=fr"$E[\bar X_{{\rm Post}}-\bar X_{{\rm Pre}}]={mu_diff}$")
ax3.set_xlabel(r"$\bar X_{\rm Post} - \bar X_{\rm Pre}$")
ax3.set_ylabel("density")
ax3.set_title(r"Step 3 -- Pairing tightens the sampling distribution")
ax3.legend(loc="upper right", framealpha=0.95, fontsize=8.5)
ratio = se_indep / se_paired
ax3.text(0.03, 0.97,
         fr"$\dfrac{{SE_{{\rm indep}}}}{{SE_{{\rm paired}}}}$"
         + "\n" + fr"$= \dfrac{{1}}{{\sqrt{{1-\rho}}}}\approx {ratio:.2f}\times$",
         transform=ax3.transAxes, ha="left", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=YELLOW,
                   boxstyle="round,pad=0.40", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  n={n} s2_pool={s2_pool} rho={rho}")
print(f"  SE_paired = {se_paired:.4f}   SE_indep = {se_indep:.4f}")
print(f"  ratio     = {ratio:.4f}")
