"""Build AI walkthrough plot for Ex 5.13 a2 —
P(|X-bar - mu| > SE) under Normality / CLT (standardise to |Z|>1)."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex5/ex5_13a2_ai.png"

# --- Target probability ---
p_tail = 2 * (1 - norm.cdf(1))   # ~ 0.31731

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5))

# =================================================================
# LEFT — Standard Normal density: shade |Z|>1 tails
# =================================================================
zs = np.linspace(-4, 4, 800)
pdf = norm.pdf(zs)

ax1.plot(zs, pdf, color=PALETTE["primary"], lw=2.4,
         label=r"$Z=\dfrac{\bar X-\mu}{SE(\bar X)}\sim N(0,1)$")

# Shade tails |Z|>1
left_mask  = zs <= -1
right_mask = zs >=  1
ax1.fill_between(zs[left_mask],  pdf[left_mask],
                 color=PALETTE["accent"], alpha=0.45)
ax1.fill_between(zs[right_mask], pdf[right_mask],
                 color=PALETTE["accent"], alpha=0.45,
                 label=fr"$P(|Z|>1)\approx {p_tail:.4f}$")

# Vertical lines at +/- 1
for z0 in (-1, 1):
    ax1.axvline(z0, color=PALETTE["neutral"], lw=1.0, ls="--", alpha=0.85)

# Annotate tail areas
ax1.text(-2.2, 0.045, fr"$\Phi(-1)$"+"\n"+fr"$\approx {norm.cdf(-1):.4f}$",
         ha="center", va="center", fontsize=10, color=PALETTE["accent"],
         fontweight="bold")
ax1.text( 2.2, 0.045, fr"$1-\Phi(1)$"+"\n"+fr"$\approx {1-norm.cdf(1):.4f}$",
         ha="center", va="center", fontsize=10, color=PALETTE["accent"],
         fontweight="bold")
ax1.text(0, 0.18, r"$P(|Z|\leq 1)\approx 0.6827$",
         ha="center", va="center", fontsize=11, color=PALETTE["primary"],
         fontweight="bold")

ax1.set_xlabel(r"$z$"); ax1.set_ylabel("density")
ax1.set_title(r"(a2) $P(|\bar X-\mu|>SE)=P(|Z|>1)\approx 0.3173$")
ax1.legend(loc="upper right", framealpha=0.95)
ax1.set_ylim(0, 0.45)

# =================================================================
# RIGHT — Same probability across distributions: Normal vs Chebyshev bound
# Show that without Normality, only the loose bound P<=1 (i.e. trivial) applies.
# Also visualise sampling distribution of Xbar centred at mu with +/-1 SE.
# =================================================================
mu = 0.0
SE = 1.0                          # work in SE units on the X-bar axis
xs = np.linspace(mu - 4*SE, mu + 4*SE, 600)
pdf_xbar = norm.pdf(xs, mu, SE)

ax2.plot(xs, pdf_xbar, color=PALETTE["primary"], lw=2.4,
         label=r"$\bar X\sim N(\mu,SE^2)$ under Normality/CLT")

# Shade |Xbar - mu| > SE
left  = xs <= mu - SE
right = xs >= mu + SE
ax2.fill_between(xs[left],  pdf_xbar[left],
                 color=PALETTE["accent"], alpha=0.45)
ax2.fill_between(xs[right], pdf_xbar[right],
                 color=PALETTE["accent"], alpha=0.45,
                 label=r"$|\bar X-\mu|>SE$ region")

ax2.axvline(mu,        color=PALETTE["neutral"], lw=1.2, ls="--", alpha=0.85)
ax2.axvline(mu - SE,   color=PALETTE["accent"],  lw=1.0, ls=":")
ax2.axvline(mu + SE,   color=PALETTE["accent"],  lw=1.0, ls=":")

ax2.text(mu, 0.43, r"$\mu$", ha="center", va="bottom",
         fontsize=11, color=PALETTE["neutral"], fontweight="bold")
ax2.text(mu - SE, 0.43, r"$\mu-SE$", ha="center", va="bottom",
         fontsize=10, color=PALETTE["accent"])
ax2.text(mu + SE, 0.43, r"$\mu+SE$", ha="center", va="bottom",
         fontsize=10, color=PALETTE["accent"])

ax2.set_xlabel(r"$\bar X$"); ax2.set_ylabel("density")
ax2.set_title(r"Tail area $\approx 0.3173$ on the $\bar X$ scale")
ax2.legend(loc="upper right", framealpha=0.95)
ax2.set_ylim(0, 0.50)

# Info box: with / without assumptions
ax2.text(0.02, 0.97,
         "With Normality/CLT:\n"
         r"   $P(|Z|>1)=2(1-\Phi(1))\approx 0.3173$"
         "\n\nWithout it (Chebyshev only):\n"
         r"   $P(|Z|>1)\leq \dfrac{1}{1^2}=1$  (trivial)",
         transform=ax2.transAxes, ha="left", va="top", fontsize=9.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  P(|Z|>1) = {p_tail:.6f}")
print(f"  Phi(-1)  = {norm.cdf(-1):.6f}")
print(f"  1-Phi(1) = {1-norm.cdf(1):.6f}")
