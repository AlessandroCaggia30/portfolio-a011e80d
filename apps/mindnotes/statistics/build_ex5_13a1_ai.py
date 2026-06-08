"""Build AI walkthrough plot for Ex 5.13 a1 —
Unbiased estimator of mean monthly turnover (Milano).

Two panels:
  LEFT  — illustrate the unbiasedness property: across B=2000 hypothetical
          samples of size n from N(mu, sigma^2), the distribution of Xbar
          is centred exactly on mu (vertical line) with spread sigma/sqrt(n).
  RIGHT — show how the sampling distribution tightens as n grows
          (n = 30, 60, 120) for fixed sigma = 11_500.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex5/ex5_5_13a1_ai.png"

# --- Assumed problem setup (illustrative) ---
mu_true   = 32_000          # illustrative population mean turnover (EUR)
sigma     = 11_500          # known population SD (given in problem)
n_demo    = 60              # illustrative Milan sub-sample size
B         = 2000            # number of hypothetical resamples
rng       = np.random.default_rng(20260606)

# --- LEFT panel: empirical sampling distribution of Xbar across B resamples
xbars = rng.normal(loc=mu_true, scale=sigma/np.sqrt(n_demo), size=B)
mean_of_xbars = xbars.mean()
SE_theory     = sigma / np.sqrt(n_demo)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5))

# Histogram of the B sample means
ax1.hist(xbars, bins=40, color=PALETTE["primary"], alpha=0.55,
         edgecolor="#2a3142", linewidth=0.5, density=True,
         label=fr"$\bar X$ across $B={B}$ resamples")

# Overlay theoretical N(mu, sigma^2/n)
xs = np.linspace(xbars.min(), xbars.max(), 400)
ax1.plot(xs, norm.pdf(xs, mu_true, SE_theory), color=PALETTE["accent"],
         lw=2.4, label=fr"$N(\mu,\sigma^2/n)$, $SE={SE_theory:,.0f}$")

# Vertical lines: true mu and empirical mean of Xbar
ax1.axvline(mu_true, color=PALETTE["neutral"], lw=1.6, ls="--",
            label=fr"$\mu={mu_true:,}$")
ax1.axvline(mean_of_xbars, color=PALETTE["accent"], lw=1.2, ls=":",
            label=fr"avg of $\bar X$ = {mean_of_xbars:,.0f}")

ax1.set_xlabel(r"$\bar X$ (EUR)")
ax1.set_ylabel("density")
ax1.set_title(r"Unbiasedness: $E[\bar X]=\mu$  —  centred on the truth")
ax1.legend(loc="upper right", framealpha=0.95, fontsize=9.5)

# Info box: definition of unbiasedness
ax1.text(0.02, 0.97,
         r"$\hat\theta$ unbiased for $\theta$ iff"
         "\n"
         r"   $E[\hat\theta]=\theta$ for all $\theta$."
         "\n\n"
         r"Here $E[\bar X]=\frac{1}{n}\sum E[X_i]=\mu$",
         transform=ax1.transAxes, ha="left", va="top", fontsize=9.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# =================================================================
# RIGHT — sampling distribution of Xbar tightens as n grows
# =================================================================
ns = [30, 60, 120]
colors_n = [PALETTE["primary"], PALETTE["accent"], PALETTE["neutral"]]
xs2 = np.linspace(mu_true - 4*sigma/np.sqrt(min(ns)),
                  mu_true + 4*sigma/np.sqrt(min(ns)), 600)

for n_i, col in zip(ns, colors_n):
    SE_i = sigma / np.sqrt(n_i)
    ax2.plot(xs2, norm.pdf(xs2, mu_true, SE_i), color=col, lw=2.4,
             label=fr"$n={n_i}$,  $SE=\sigma/\sqrt{{n}}={SE_i:,.0f}$")
    # shade +/- 1 SE
    xs_band = np.linspace(mu_true - SE_i, mu_true + SE_i, 80)
    ax2.fill_between(xs_band, 0, norm.pdf(xs_band, mu_true, SE_i),
                     color=col, alpha=0.10)

ax2.axvline(mu_true, color=PALETTE["neutral"], lw=1.2, ls="--",
            label=fr"$\mu={mu_true:,}$")

ax2.set_xlabel(r"$\bar X$ (EUR)")
ax2.set_ylabel("density")
ax2.set_title(r"Spread shrinks as $n$ grows: $SE=\sigma/\sqrt{n}$ with known $\sigma=11{,}500$")
ax2.legend(loc="upper right", framealpha=0.95, fontsize=9.5)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  true mu        = {mu_true}")
print(f"  sigma (known)  = {sigma}")
print(f"  n_demo         = {n_demo}")
print(f"  SE theory      = {SE_theory:.4f}")
print(f"  mean of B xbars = {mean_of_xbars:.4f}")
for n_i in ns:
    print(f"  SE(n={n_i})   = {sigma/np.sqrt(n_i):.4f}")
