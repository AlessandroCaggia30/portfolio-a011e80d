"""Build AI walkthrough plot for Ex 4.1b — P(X < 7.5) for X ~ N(8, 1.2^2).

Visual: Normal density centered at mu=8 with sigma=1.2, with the left-tail
P(X < 7.5) shaded. Side panel shows the same probability in standard-normal
coordinates (z = (7.5-8)/1.2 = -0.4167), connecting the original scale to the
z-score. Reinforces both the geometric meaning of a tail probability and the
fact that the threshold sits ~0.42 standard deviations below mu — hence the
non-trivial ~34% mass to its left.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex4/ex4_1b_ai.png"

mu, sigma = 8.0, 1.2
thr       = 7.5
z_thr     = (thr - mu) / sigma                  # -0.4167
p_left    = norm.cdf(thr, mu, sigma)            # 0.33846

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5))

# --- LEFT: original scale N(8, 1.2^2) ---
xs = np.linspace(mu - 4*sigma, mu + 4*sigma, 500)
pdf = norm.pdf(xs, mu, sigma)
ax1.plot(xs, pdf, color=PALETTE["primary"], lw=2.2, label=r"$N(8,\, 1.2^2)$")
# shade left tail x < 7.5
xs_tail = np.linspace(mu - 4*sigma, thr, 200)
ax1.fill_between(xs_tail, 0, norm.pdf(xs_tail, mu, sigma),
                 color=PALETTE["warn"], alpha=0.35,
                 label=fr"$\Pr(X<7.5) = {p_left:.4f}$")
# vertical lines: mu and threshold
ax1.axvline(mu, color=PALETTE["neutral"], lw=1.0, ls="--", alpha=0.7)
ax1.axvline(thr, color=PALETTE["warn"], lw=1.3, ls="-", alpha=0.9)
y_top = pdf.max()
ax1.text(mu, y_top*1.02, r"$\mu=8$", color=PALETTE["neutral"],
         ha="center", va="bottom", fontsize=11)
ax1.text(thr, y_top*0.62, "  threshold\n  $x=7.5$ cl",
         color=PALETTE["warn"], ha="right", va="center", fontsize=11,
         fontweight="bold")
# sigma bracket below x-axis
ax1.annotate("", xy=(mu, -0.018), xytext=(mu+sigma, -0.018),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["accent"], lw=1.4))
ax1.text(mu + sigma/2, -0.028, r"$\sigma=1.2$",
         color=PALETTE["accent"], ha="center", va="top", fontsize=11,
         fontweight="bold")
ax1.set_xlabel(r"$X$ — tea poured (cl)")
ax1.set_ylabel("density")
ax1.set_title(r"$X \sim N(8,\, 1.2^2)$ — left tail below 7.5 cl")
ax1.set_ylim(-0.05, y_top*1.18)
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
         f"  $z = (7.5-8)/1.2$\n  $\\approx {z_thr:.4f}$",
         color=PALETTE["warn"], ha="right", va="center", fontsize=11,
         fontweight="bold")
ax2.set_xlabel(r"$Z$ (standardized)")
ax2.set_ylabel("density")
ax2.set_title(r"Same tail in $Z$-coordinates  —  threshold is $0.42\,\sigma$ below $\mu$")
ax2.legend(loc="upper left", framealpha=0.95)

# Box: the R-command and result, anchored in the right panel
ax2.text(0.97, 0.97,
         "R command:\n"
         "pnorm(7.5, mean=8, sd=1.2)\n"
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
print(f"  P(X < 7.5) = {p_left:.7f}")
