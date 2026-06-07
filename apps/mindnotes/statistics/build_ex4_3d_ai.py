"""Build AI walkthrough plot for Ex 4.3d — CLT for proportion.

n = 150 customers; W ~ Bernoulli(p), p = 0.8413 (probability that a single
customer spends > 12 EUR from Ex 4.3a).  The sample proportion of customers
who spend > 12 EUR, P-bar, has by the CLT an approximate normal distribution:

    P-bar  ~  N( p,  p(1-p)/n )  =  N( 0.8413,  0.0298^2 ).

We want Pr( P-bar > 0.80 ) = 1 - pnorm(0.80, 0.8413, 0.0298) = 0.9171.

Left panel : sampling distribution of P-bar with the right-tail P-bar > 0.80
             shaded (probability = 0.9171).
Right panel: standardised picture — z = (0.80 - 0.8413)/0.0298 = -1.385 and
             the equivalent area Pr(Z > -1.385) = Phi(1.385) = 0.9171.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex4/ex4_3d_ai.png"

# ---- CLT parameters ----------------------------------------------------------
p   = 0.8413
n   = 150
mu  = p
sd  = np.sqrt(p * (1 - p) / n)        # 0.02980...
thr = 0.80
prob = 1 - norm.cdf(thr, loc=mu, scale=sd)   # 0.9171
z_thr = (thr - mu) / sd                       # -1.3851...

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5))

# =============================================================================
# LEFT: N(0.8413, 0.0298^2) density with right-tail P-bar > 0.80 shaded
# =============================================================================
xs = np.linspace(mu - 4 * sd, mu + 4 * sd, 600)
pdf = norm.pdf(xs, loc=mu, scale=sd)
ax1.plot(xs, pdf, color=PALETTE["primary"], lw=2.4,
         label=r"$\bar P \sim N(0.8413,\,0.0298^2)$")

# Right tail: P-bar > 0.80
mask = xs >= thr
ax1.fill_between(xs[mask], 0, pdf[mask], color=PALETTE["accent"], alpha=0.55,
                 label=fr"$\Pr(\bar P > 0.80) = {prob:.4f}$")

# Threshold line at 0.80
ax1.axvline(thr, color=PALETTE["warn"], lw=1.7, ls="--")
ax1.text(thr - 0.005, norm.pdf(mu, mu, sd) * 0.55,
         r"$0.80$  ", color=PALETTE["warn"],
         fontsize=11, fontweight="bold", ha="right")

# Mean reference
ax1.axvline(mu, color=PALETTE["neutral"], lw=1.1, ls=":", alpha=0.7)
ax1.text(mu, norm.pdf(mu, mu, sd) * 1.03, r"$p = 0.8413$",
         color=PALETTE["neutral"], fontsize=10.5, ha="center", va="bottom")

ax1.set_xlabel(r"$\bar P$  (sample proportion, $n = 150$)")
ax1.set_ylabel("density")
ax1.set_title(r"CLT: $\bar P \approx N\!\left(p,\,\frac{p(1-p)}{n}\right)$")
ax1.legend(loc="upper left", framealpha=0.95)
ax1.set_ylim(bottom=0)
ax1.text(0.03, 0.55,
         f"$p = 0.8413$  (from 4.3a)\n"
         f"$\\mathrm{{SE}} = \\sqrt{{p(1-p)/n}} = {sd:.4f}$\n"
         f"$\\Pr(\\bar P > 0.80) = {prob:.4f}$",
         transform=ax1.transAxes, ha="left", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# =============================================================================
# RIGHT: standardised — z = (0.80 - 0.8413)/0.0298 = -1.385
# =============================================================================
zs = np.linspace(-4, 4, 600)
pdfz = norm.pdf(zs)
ax2.plot(zs, pdfz, color=PALETTE["primary"], lw=2.4, label=r"$Z \sim N(0,1)$")

mask_z = zs >= z_thr
ax2.fill_between(zs[mask_z], 0, pdfz[mask_z], color=PALETTE["accent"], alpha=0.55,
                 label=fr"$\Pr(Z > {z_thr:.3f}) = {prob:.4f}$")
ax2.axvline(z_thr, color=PALETTE["warn"], lw=1.7, ls="--")
ax2.text(z_thr + 0.08, norm.pdf(0) * 0.55,
         fr"  $z = {z_thr:.4f}$",
         color=PALETTE["warn"], fontsize=11, fontweight="bold", ha="left")

ax2.set_xlabel(r"$Z = (\bar P - p)/\mathrm{SE}$")
ax2.set_ylabel("density")
ax2.set_title(r"Standardisation: $z = (0.80 - 0.8413)/0.0298$")
ax2.legend(loc="upper left", framealpha=0.95)
ax2.set_ylim(bottom=0)
ax2.text(0.97, 0.97,
         f"$z = \\dfrac{{0.80 - 0.8413}}{{0.0298}} = {z_thr:.4f}$\n"
         f"$\\Pr(Z > {z_thr:.4f}) = \\Phi({-z_thr:.4f})$\n"
         f"$\\;\\;\\;\\;\\;\\;\\,= {prob:.4f}$",
         transform=ax2.transAxes, ha="right", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  p   = {p}")
print(f"  SE  = {sd:.6f}")
print(f"  z   = {z_thr:.6f}")
print(f"  Pr(P-bar > 0.80) = {prob:.6f}")
