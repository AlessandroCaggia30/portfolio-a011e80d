"""Build AI walkthrough plot for Ex 4.3b — 90th percentile of X ~ N(13.2, 1.2^2).

Top 10% of customers (by private-label spending) are those above the 90th
percentile, q_{0.90} = qnorm(0.9, 13.2, 1.2) = 14.7379.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex4/ex4_3b_ai.png"

mu, sigma = 13.2, 1.2
p90 = norm.ppf(0.90, loc=mu, scale=sigma)        # 14.7379
z90 = norm.ppf(0.90)                              # 1.2816

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5))

# =================================================================
# LEFT: N(13.2, 1.2^2) density with right-tail (top 10%) shaded
# =================================================================
xs = np.linspace(mu - 4*sigma, mu + 4*sigma, 600)
pdf = norm.pdf(xs, loc=mu, scale=sigma)
ax1.plot(xs, pdf, color=PALETTE["primary"], lw=2.4, label=r"$X \sim N(13.2,\,1.2^2)$")

# Right-tail shading: P(X > q_{0.90}) = 0.10
mask = xs >= p90
ax1.fill_between(xs[mask], 0, pdf[mask], color=PALETTE["accent"], alpha=0.55,
                 label=r"top 10% ($X > q_{0.90}$)")

# Vertical at the 90th percentile
ax1.axvline(p90, color=PALETTE["warn"], lw=1.7, ls="--")
ax1.text(p90 + 0.05, norm.pdf(mu, mu, sigma) * 0.55,
         fr"  $q_{{0.90}} = {p90:.4f}$",
         color=PALETTE["warn"], fontsize=11, fontweight="bold", ha="left")

# Mean reference
ax1.axvline(mu, color=PALETTE["neutral"], lw=1.1, ls=":", alpha=0.7)
ax1.text(mu, norm.pdf(mu, mu, sigma) * 1.03, r"$\mu=13.2$",
         color=PALETTE["neutral"], fontsize=10.5, ha="center", va="bottom")

ax1.set_xlabel(r"$X$  (euros)")
ax1.set_ylabel("density")
ax1.set_title(r"Top 10% of customers $\Leftrightarrow$ $X > q_{0.90}$")
ax1.legend(loc="upper left", framealpha=0.95)
ax1.set_ylim(bottom=0)
ax1.text(0.97, 0.97,
         f"Top 10% spend at least\n€{p90:.4f}\n"
         "(area to the right = 0.10)",
         transform=ax1.transAxes, ha="right", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# =================================================================
# RIGHT: standardisation — z = (x - mu)/sigma, z_{0.90} = 1.2816
#        x_{0.90} = mu + z_{0.90} * sigma = 13.2 + 1.2816*1.2 = 14.7379
# =================================================================
zs = np.linspace(-4, 4, 600)
pdfz = norm.pdf(zs)
ax2.plot(zs, pdfz, color=PALETTE["primary"], lw=2.4, label=r"$Z \sim N(0,1)$")

mask_z = zs >= z90
ax2.fill_between(zs[mask_z], 0, pdfz[mask_z], color=PALETTE["accent"], alpha=0.55,
                 label=fr"area = 0.10  ($z > {z90:.4f}$)")
ax2.axvline(z90, color=PALETTE["warn"], lw=1.7, ls="--")
ax2.text(z90 + 0.05, norm.pdf(0) * 0.55,
         fr"  $z_{{0.90}} = {z90:.4f}$",
         color=PALETTE["warn"], fontsize=11, fontweight="bold", ha="left")

ax2.set_xlabel(r"$Z = (X - \mu)/\sigma$")
ax2.set_ylabel("density")
ax2.set_title(r"Standardisation: $x_{0.90} = \mu + z_{0.90}\,\sigma$")
ax2.legend(loc="upper left", framealpha=0.95)
ax2.set_ylim(bottom=0)
ax2.text(0.97, 0.97,
         f"$z_{{0.90}} = $qnorm(0.9)$ = {z90:.4f}$\n"
         f"$x_{{0.90}} = 13.2 + {z90:.4f}\\cdot 1.2$\n"
         f"$\\;\\;\\;\\;\\;\\;\\,= {p90:.4f}$",
         transform=ax2.transAxes, ha="right", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  q_0.90  = {p90:.6f}")
print(f"  z_0.90  = {z90:.6f}")
