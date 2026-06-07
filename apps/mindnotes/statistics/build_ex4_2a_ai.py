"""Build AI walkthrough plot for Ex 4.2a — P(X < 24) for battery life X ~ N(27, 3.2^2).

Visual: Normal density centered at mu=27 with sigma=3.2, with the LEFT-tail
P(X < 24) shaded. Side panel shows the same probability in standard-normal
coordinates (z = (24-27)/3.2 = -0.9375), connecting the original scale to the
z-score. Reinforces both the geometric meaning of a left-tail probability and
the location of the threshold relative to mu and sigma.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex4/ex4_2a_ai.png"

mu, sigma = 27.0, 3.2
thr       = 24.0
z_thr     = (thr - mu) / sigma                  # -0.9375
p_left    = norm.cdf(thr, mu, sigma)            # 0.17425

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5))

# --- LEFT: original scale N(27, 3.2^2) ---
xs = np.linspace(mu - 4*sigma, mu + 4*sigma, 500)
pdf = norm.pdf(xs, mu, sigma)
ax1.plot(xs, pdf, color=PALETTE["primary"], lw=2.2, label=r"$N(27,\, 3.2^2)$")
# shade left tail x < 24
xs_tail = np.linspace(mu - 4*sigma, thr, 200)
ax1.fill_between(xs_tail, 0, norm.pdf(xs_tail, mu, sigma),
                 color=PALETTE["warn"], alpha=0.35,
                 label=fr"$\Pr(X<24) = {p_left:.4f}$")
# vertical lines: mu and threshold
ax1.axvline(mu, color=PALETTE["neutral"], lw=1.0, ls="--", alpha=0.7)
ax1.axvline(thr, color=PALETTE["warn"], lw=1.3, ls="-", alpha=0.9)
y_top = pdf.max()
ax1.text(mu, y_top*1.02, r"$\mu=27$", color=PALETTE["neutral"],
         ha="center", va="bottom", fontsize=11)
ax1.text(thr, y_top*0.62, "battery threshold  \n$x=24$ h  ",
         color=PALETTE["warn"], ha="right", va="center", fontsize=11,
         fontweight="bold")
# sigma bracket below x-axis
ax1.annotate("", xy=(mu, -0.007), xytext=(mu+sigma, -0.007),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["accent"], lw=1.4))
ax1.text(mu + sigma/2, -0.011, r"$\sigma=3.2$",
         color=PALETTE["accent"], ha="center", va="top", fontsize=11,
         fontweight="bold")
ax1.set_xlabel(r"$X$ — battery life (hours)")
ax1.set_ylabel("density")
ax1.set_title(r"$X \sim N(27,\, 3.2^2)$ — left tail below 24 h")
ax1.set_ylim(-0.02, y_top*1.18)
ax1.legend(loc="upper left", framealpha=0.95)

# --- RIGHT: standardized N(0,1), same probability mass ---
zs = np.linspace(-4, 4, 500)
pdfz = norm.pdf(zs)
ax2.plot(zs, pdfz, color=PALETTE["primary"], lw=2.2, label=r"$Z = (X-\mu)/\sigma \sim N(0,1)$")
zs_tail = np.linspace(-4, z_thr, 200)
ax2.fill_between(zs_tail, 0, norm.pdf(zs_tail),
                 color=PALETTE["warn"], alpha=0.35,
                 label=fr"$\Pr(Z<{z_thr:.4f}) = {p_left:.4f}$")
ax2.axvline(0, color=PALETTE["neutral"], lw=1.0, ls="--", alpha=0.7)
ax2.axvline(z_thr, color=PALETTE["warn"], lw=1.3, ls="-", alpha=0.9)
ax2.text(z_thr, norm.pdf(0)*0.62,
         f"$z = (24-27)/3.2$  \n$\\approx {z_thr:.4f}$  ",
         color=PALETTE["warn"], ha="right", va="center", fontsize=11,
         fontweight="bold")
ax2.set_xlabel(r"$Z$ (standardized)")
ax2.set_ylabel("density")
ax2.set_title(r"Same tail in $Z$-coordinates  —  threshold is $0.94\,\sigma$ below $\mu$")
ax2.legend(loc="upper left", framealpha=0.95)

# Box: the R-command and result, anchored in the right panel
ax2.text(0.97, 0.97,
         "R command:\n"
         "pnorm(24, mean=27, sd=3.2)\n"
         f"= {p_left:.7f}",
         transform=ax2.transAxes, ha="right", va="top", fontsize=10,
         family="monospace",
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  mu = {mu}, sigma = {sigma}, threshold = {thr}")
print(f"  z = {z_thr:.6f}")
print(f"  P(X < 24) = {p_left:.7f}")
