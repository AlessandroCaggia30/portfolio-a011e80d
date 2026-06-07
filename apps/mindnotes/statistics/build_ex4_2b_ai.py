"""Build AI walkthrough plot for Ex 4.2b — 80th percentile of X ~ N(27, 3.2^2).

Top 20% of cell phones (by battery life after two years) are those above the
80th percentile, q_{0.80} = qnorm(0.8, 27, 3.2) = 29.6932 hours.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex4/ex4_2b_ai.png"

mu, sigma = 27.0, 3.2
p80 = norm.ppf(0.80, loc=mu, scale=sigma)        # 29.6932
z80 = norm.ppf(0.80)                              # 0.8416

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5))

# =================================================================
# LEFT: N(27, 3.2^2) density with right-tail (top 20%) shaded
# =================================================================
xs = np.linspace(mu - 4*sigma, mu + 4*sigma, 600)
pdf = norm.pdf(xs, loc=mu, scale=sigma)
ax1.plot(xs, pdf, color=PALETTE["primary"], lw=2.4, label=r"$X \sim N(27,\,3.2^2)$")

# Right-tail shading: P(X > q_{0.80}) = 0.20
mask = xs >= p80
ax1.fill_between(xs[mask], 0, pdf[mask], color=PALETTE["accent"], alpha=0.55,
                 label=r"top 20% ($X > q_{0.80}$)")

# Vertical at the 80th percentile
ax1.axvline(p80, color=PALETTE["warn"], lw=1.7, ls="--")
ax1.text(p80 + 0.15, norm.pdf(mu, mu, sigma) * 0.55,
         fr"  $q_{{0.80}} = {p80:.4f}$",
         color=PALETTE["warn"], fontsize=11, fontweight="bold", ha="left")

# Mean reference
ax1.axvline(mu, color=PALETTE["neutral"], lw=1.1, ls=":", alpha=0.7)
ax1.text(mu, norm.pdf(mu, mu, sigma) * 1.03, r"$\mu=27$",
         color=PALETTE["neutral"], fontsize=10.5, ha="center", va="bottom")

ax1.set_xlabel(r"$X$  (hours of battery life)")
ax1.set_ylabel("density")
ax1.set_title(r"Longest-lasting 20% $\Leftrightarrow$ $X > q_{0.80}$")
ax1.legend(loc="upper left", framealpha=0.95)
ax1.set_ylim(bottom=0)
ax1.text(0.97, 0.97,
         f"Longest-lasting 20%\nhave battery life $\\geq$\n{p80:.4f} h\n"
         "(area to the right = 0.20)",
         transform=ax1.transAxes, ha="right", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# =================================================================
# RIGHT: standardisation — z = (x - mu)/sigma, z_{0.80} = 0.8416
#        x_{0.80} = mu + z_{0.80} * sigma = 27 + 0.8416*3.2 = 29.6932
# =================================================================
zs = np.linspace(-4, 4, 600)
pdfz = norm.pdf(zs)
ax2.plot(zs, pdfz, color=PALETTE["primary"], lw=2.4, label=r"$Z \sim N(0,1)$")

mask_z = zs >= z80
ax2.fill_between(zs[mask_z], 0, pdfz[mask_z], color=PALETTE["accent"], alpha=0.55,
                 label=fr"area = 0.20  ($z > {z80:.4f}$)")
ax2.axvline(z80, color=PALETTE["warn"], lw=1.7, ls="--")
ax2.text(z80 + 0.05, norm.pdf(0) * 0.55,
         fr"  $z_{{0.80}} = {z80:.4f}$",
         color=PALETTE["warn"], fontsize=11, fontweight="bold", ha="left")

ax2.set_xlabel(r"$Z = (X - \mu)/\sigma$")
ax2.set_ylabel("density")
ax2.set_title(r"Standardisation: $x_{0.80} = \mu + z_{0.80}\,\sigma$")
ax2.legend(loc="upper left", framealpha=0.95)
ax2.set_ylim(bottom=0)
ax2.text(0.97, 0.97,
         f"$z_{{0.80}} = $qnorm(0.8)$ = {z80:.4f}$\n"
         f"$x_{{0.80}} = 27 + {z80:.4f}\\cdot 3.2$\n"
         f"$\\;\\;\\;\\;\\;\\;\\,= {p80:.4f}$",
         transform=ax2.transAxes, ha="right", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  q_0.80  = {p80:.6f}")
print(f"  z_0.80  = {z80:.6f}")
