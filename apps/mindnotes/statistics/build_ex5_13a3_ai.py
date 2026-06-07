"""Build AI walkthrough plot for Ex 5.13 a3 —
sample proportion of no-smoking Milan pizzerias + reliability (SE, CI)."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex5/ex5_13a3_ai.png"

# --- Illustrative values for the Milan sub-sample ---
# (Generic numbers consistent with a pizzerie-like sample;
#  exact values from the dataset depend on R output.)
n_Mi   = 80           # Milan sub-sample size (illustrative)
phat   = 0.55         # sample proportion: SmokingArea == "No" in Milan
se_hat = np.sqrt(phat * (1 - phat) / n_Mi)
z      = 1.96
ci_lo, ci_hi = phat - z*se_hat, phat + z*se_hat

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5))

# =================================================================
# LEFT — Sampling distribution of phat under CLT  (Normal(p, p(1-p)/n))
# Shaded 95% CI band centred on phat.
# =================================================================
xs  = np.linspace(0.25, 0.85, 600)
pdf = np.exp(-0.5 * ((xs - phat) / se_hat) ** 2) / (se_hat * np.sqrt(2 * np.pi))

ax1.plot(xs, pdf, color=PALETTE["primary"], lw=2.4,
         label=fr"$\hat p \sim N(p,\,p(1-p)/n)$  (CLT)")

mask = (xs >= ci_lo) & (xs <= ci_hi)
ax1.fill_between(xs[mask], pdf[mask], color=PALETTE["accent"], alpha=0.35,
                 label=fr"95% CI:  [{ci_lo:.3f},\,{ci_hi:.3f}]")

ax1.axvline(phat, color=PALETTE["neutral"], lw=1.4, ls="--", alpha=0.85)
ax1.text(phat, ax1.get_ylim()[1] if ax1.get_ylim()[1] else 0,
         fr"  $\hat p={phat:.2f}$", color=PALETTE["neutral"],
         fontsize=10, ha="left", va="top")
ax1.axvline(ci_lo, color=PALETTE["accent"], lw=1.1, ls=":")
ax1.axvline(ci_hi, color=PALETTE["accent"], lw=1.1, ls=":")

ax1.set_xlabel(r"$\hat p$"); ax1.set_ylabel("density")
ax1.set_title(r"(a3) Sampling distribution of $\hat p$  +  95% CI")
ax1.legend(loc="upper right", framealpha=0.95)
ax1.text(0.02, 0.97,
         f"$n={n_Mi}$,  $\\hat p={phat:.2f}$\n"
         f"$\\widehat{{se}}(\\hat p)=\\sqrt{{\\hat p(1-\\hat p)/n}}={se_hat:.4f}$\n"
         f"CLT valid: $n\\hat p={n_Mi*phat:.0f}\\geq 5$,  "
         f"$n(1-\\hat p)={n_Mi*(1-phat):.0f}\\geq 5$",
         transform=ax1.transAxes, ha="left", va="top", fontsize=9.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# =================================================================
# RIGHT — How SE shrinks with n  (for the same phat = 0.55)
# Bonus: dashed curve for the worst-case p = 0.5 (max variance).
# =================================================================
ns = np.arange(20, 401, 1)
se_p   = np.sqrt(phat * (1 - phat) / ns)
se_max = np.sqrt(0.5  * 0.5         / ns)

ax2.plot(ns, se_p,   color=PALETTE["primary"], lw=2.3,
         label=fr"$\sqrt{{\hat p(1-\hat p)/n}}$,  $\hat p=0.55$")
ax2.plot(ns, se_max, color=PALETTE["accent"], lw=2.0, ls="--",
         label=r"worst case $p=0.5$:  $1/(2\sqrt{n})$")

ax2.plot([n_Mi], [se_hat], "o", color=PALETTE["primary"],
         markersize=10, markeredgecolor="black", markeredgewidth=0.7, zorder=5)
ax2.annotate(fr"current sample$:\,n={n_Mi},\,\widehat{{se}}={se_hat:.3f}$",
             xy=(n_Mi, se_hat),
             xytext=(n_Mi + 35, se_hat + 0.012),
             fontsize=10, color=PALETTE["primary"],
             arrowprops=dict(arrowstyle="->", color=PALETTE["primary"], lw=1.0))

ax2.set_xlabel(r"sample size $n$")
ax2.set_ylabel(r"$\widehat{se}(\hat p)$")
ax2.set_title(r"SE shrinks as $1/\sqrt{n}$  —  bigger $n$ $\Rightarrow$ tighter CI")
ax2.legend(loc="upper right", framealpha=0.95)
ax2.text(0.02, 0.05,
         "Reliability summary: report\n"
         r"   (i) $\widehat{se}(\hat p)$,"
         "\n"
         r"   (ii) 95% CI $\;\hat p\pm z_{0.975}\,\widehat{se}(\hat p)$."
         "\n"
         "Both convey precision of the point estimate.",
         transform=ax2.transAxes, ha="left", va="bottom", fontsize=9.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  phat = {phat},  n = {n_Mi},  se = {se_hat:.4f}")
print(f"  95% CI = [{ci_lo:.4f}, {ci_hi:.4f}]")
