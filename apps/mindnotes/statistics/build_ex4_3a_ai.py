"""Build AI walkthrough plot for Ex 4.3a — P(X > 12) for X ~ N(13.2, 1.2^2)."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
from scipy.stats import norm
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex4/ex4_3a_ai.png"

# --- Given parameters ---
mu, sigma = 13.2, 1.2
x0 = 12.0
z0 = (x0 - mu) / sigma                          # -1.0
p_right = 1 - norm.cdf(x0, mu, sigma)           # 0.8413447

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5))

# --- LEFT: original-scale X ~ N(13.2, 1.2^2), shade X > 12 ---
xs = np.linspace(mu - 4*sigma, mu + 4*sigma, 600)
pdf_x = norm.pdf(xs, mu, sigma)
ax1.plot(xs, pdf_x, color=PALETTE["primary"], lw=2.2)
xs_shade = xs[xs >= x0]
ax1.fill_between(xs_shade, 0, norm.pdf(xs_shade, mu, sigma),
                 color=PALETTE["accent"], alpha=0.55,
                 label=fr"$\Pr(X>12) = {p_right:.4f}$")
ax1.axvline(x0,  color=PALETTE["warn"],    lw=1.4, ls="--")
ax1.axvline(mu,  color=PALETTE["neutral"], lw=1.0, ls=":", alpha=0.7)
ax1.text(x0,  ax1.get_ylim()[1]*0.05, r"  $x=12$",
         color=PALETTE["warn"], fontsize=11, ha="left", va="bottom")
ax1.text(mu, ax1.get_ylim()[1]*0.97, r"  $\mu=13.2$",
         color=PALETTE["neutral"], fontsize=10, ha="left", va="top")
ax1.set_xlabel(r"spending $X$ (euro)")
ax1.set_ylabel("density")
ax1.set_title(r"$X \sim N(13.2,\,1.2^2)$  —  $\Pr(X>12)$")
ax1.legend(loc="upper left", framealpha=0.95)

# --- RIGHT: standardized Z = (X - mu) / sigma, shade Z > -1 ---
zs = np.linspace(-4, 4, 600)
pdf_z = norm.pdf(zs)
ax2.plot(zs, pdf_z, color=PALETTE["primary"], lw=2.2)
zs_shade = zs[zs >= z0]
ax2.fill_between(zs_shade, 0, norm.pdf(zs_shade),
                 color=PALETTE["accent"], alpha=0.55,
                 label=fr"$\Pr(Z>-1) = {p_right:.4f}$")
ax2.axvline(z0, color=PALETTE["warn"],    lw=1.4, ls="--")
ax2.axvline(0,  color=PALETTE["neutral"], lw=1.0, ls=":", alpha=0.7)
ax2.text(z0, ax2.get_ylim()[1]*0.05, r"  $z=-1$",
         color=PALETTE["warn"], fontsize=11, ha="left", va="bottom")
ax2.text(0,  ax2.get_ylim()[1]*0.97, r"  $\mu_Z=0$",
         color=PALETTE["neutral"], fontsize=10, ha="left", va="top")
ax2.set_xlabel(r"standardized $Z=(X-\mu)/\sigma$")
ax2.set_ylabel("density")
ax2.set_title(r"$Z \sim N(0,1)$  —  same area, $z=(12-13.2)/1.2=-1$")
ax2.legend(loc="upper left", framealpha=0.95)

# Box with the standardisation summary (left panel)
ax1.text(0.03, 0.96,
         "Standardise:\n"
         r"$z = \dfrac{12-13.2}{1.2} = -1$" + "\n"
         r"$\Rightarrow \Pr(X>12)=\Pr(Z>-1)$" + "\n"
         r"$= 1-\Phi(-1) = \Phi(1) \approx 0.8413$",
         transform=ax1.transAxes, ha="left", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  z0          = {z0:.4f}")
print(f"  Pr(X > 12)  = {p_right:.7f}")
