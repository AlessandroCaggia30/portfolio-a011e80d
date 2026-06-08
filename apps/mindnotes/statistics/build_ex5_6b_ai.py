"""Build AI walkthrough plot for Ex 5.6b — unbiased estimator of the fraction
of employees whose productivity improved after a transition (Post - Pre > 0)."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import binom, norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex5/ex5_5_6b_ai.png"

# --- Illustrative numerical setup (worked example used in the walkthrough) ---
n        = 80                          # employees in the sample
x_succ   = 52                          # number of improvers
phat     = x_succ / n                  # 0.65 sample proportion
p_true   = 0.60                        # assumed (unknown) population proportion
se_hat   = np.sqrt(phat * (1 - phat) / n)
z        = norm.ppf(0.975)
ci_lo    = phat - z * se_hat
ci_hi    = phat + z * se_hat

# --- Build per-employee paired (Pre, Post) data for the LEFT panel ---
rng = np.random.default_rng(20260606)
mu_pre, sd_pre = 70.0, 9.0
# generate Pre then build Post = Pre + delta where delta is signed change
pre  = rng.normal(mu_pre, sd_pre, size=n)
# Force exactly x_succ = 52 improvers so the panel and the worked example agree
imp_idx = rng.choice(n, size=x_succ, replace=False)
labels  = np.zeros(n, dtype=bool); labels[imp_idx] = True
delta_imp = rng.normal(+4.0, 2.5, size=n)               # improvers: +mean
delta_dec = rng.normal(-3.0, 2.5, size=n)               # decliners: -mean
# clip so the sign of delta is consistent with the label (no boundary noise)
delta_imp = np.where(delta_imp > 0.1, delta_imp, 0.1 + np.abs(delta_imp))
delta_dec = np.where(delta_dec < -0.1, delta_dec, -0.1 - np.abs(delta_dec))
delta     = np.where(labels, delta_imp, delta_dec)
post      = pre + delta
diff      = post - pre                                  # D_i
Y         = (diff > 0).astype(int)                      # indicator
phat_actual = Y.mean()                                  # == phat by construction

fig, (ax1, ax2, ax3) = plt.subplots(1, 3, figsize=(16, 5))

# ====================================================================
# LEFT — Pre vs Post scatter, points above diagonal = improvers (Y=1)
# ====================================================================
imp_mask = Y == 1
ax1.scatter(pre[~imp_mask], post[~imp_mask],
            color=PALETTE["neutral"], s=36, alpha=0.85,
            edgecolor="#2a3142", linewidth=0.6, label=r"$Y_i=0$  (no improvement)")
ax1.scatter(pre[imp_mask], post[imp_mask],
            color=PALETTE["accent"], s=36, alpha=0.95,
            edgecolor="#2a3142", linewidth=0.6, label=r"$Y_i=1$  (improved)")
lo = min(pre.min(), post.min()) - 2
hi = max(pre.max(), post.max()) + 2
ax1.plot([lo, hi], [lo, hi], ls="--", lw=1.2, color=PALETTE["primary"], alpha=0.7,
         label=r"Post = Pre")
ax1.set_xlim(lo, hi); ax1.set_ylim(lo, hi)
ax1.set_xlabel("Pre productivity")
ax1.set_ylabel("Post productivity")
ax1.set_title(r"Step 1 — Sign indicator  $Y_i = \mathbb{1}\{\mathrm{Post}_i - \mathrm{Pre}_i > 0\}$")
ax1.legend(loc="lower right", framealpha=0.95, fontsize=9)
ax1.text(0.03, 0.97,
         f"n = {n}\nimprovers = {Y.sum()}\n"
         rf"$\hat p = {phat_actual:.3f}$",
         transform=ax1.transAxes, ha="left", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.40", linewidth=1.0))

# ====================================================================
# MIDDLE — Sampling distribution of phat (Binomial / Normal approx)
# ====================================================================
k_grid     = np.arange(0, n + 1)
pmf        = binom.pmf(k_grid, n, p_true)
phat_grid  = k_grid / n
ax2.bar(phat_grid, pmf, width=1.0/n, color=PALETTE["primary"], alpha=0.55,
        edgecolor="#2a3142", linewidth=0.3,
        label=fr"Bin$(n={n},\,p={p_true})\,/\,n$")
# Normal approximation overlay
xs        = np.linspace(0.30, 0.90, 400)
se_true   = np.sqrt(p_true * (1 - p_true) / n)
ax2.plot(xs, norm.pdf(xs, loc=p_true, scale=se_true) * (1.0/n),
         color=PALETTE["accent"], lw=2.2,
         label=fr"Normal approx, SE $= \sqrt{{p(1-p)/n}} = {se_true:.3f}$")
ax2.axvline(p_true, color=PALETTE["neutral"], ls="--", lw=1.2,
            label=fr"true $p = {p_true}$")
ax2.axvline(phat_actual, color="#b85c00", ls="-", lw=1.6,
            label=fr"observed $\hat p = {phat_actual:.3f}$")
ax2.set_xlabel(r"$\hat p$")
ax2.set_ylabel(r"$\Pr(\hat p = k/n)$")
ax2.set_title(r"Step 2 — Sampling distribution of $\hat p$ (unbiased: $E[\hat p]=p$)")
ax2.set_xlim(0.35, 0.85)
ax2.legend(loc="upper left", framealpha=0.95, fontsize=9)

# ====================================================================
# RIGHT — SE shrinkage 1/sqrt(n) and 95% CI illustration
# ====================================================================
n_grid    = np.arange(10, 401, 5)
se_grid   = np.sqrt(p_true * (1 - p_true) / n_grid)
ax3.plot(n_grid, se_grid, color=PALETTE["primary"], lw=2.2,
         label=r"$SE(\hat p) = \sqrt{p(1-p)/n}$")
ax3.scatter([n], [np.sqrt(p_true*(1-p_true)/n)], color=PALETTE["accent"], s=80,
            edgecolor="#2a3142", linewidth=0.8, zorder=5,
            label=fr"sample ($n={n}$):  SE $\approx {se_hat:.3f}$")
ax3.set_xlabel(r"sample size $n$")
ax3.set_ylabel(r"$SE(\hat p)$")
ax3.set_title(r"Step 3 — SE shrinks as $1/\sqrt{n}$  &  95% CI")
ax3.legend(loc="upper right", framealpha=0.95, fontsize=9)
# 95% CI box for the worked example
ci_text = (rf"$\hat p \pm z_{{0.975}}\,\widehat{{SE}}$" + "\n"
           rf"$= {phat:.2f} \pm 1.96\cdot{se_hat:.3f}$" + "\n"
           rf"$= [{ci_lo:.3f},\,{ci_hi:.3f}]$")
ax3.text(0.97, 0.55, ci_text,
         transform=ax3.transAxes, ha="right", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  phat_actual = {phat_actual:.4f}  (target {phat:.2f})")
print(f"  SE_hat      = {se_hat:.4f}")
print(f"  95% CI      = [{ci_lo:.4f}, {ci_hi:.4f}]")
