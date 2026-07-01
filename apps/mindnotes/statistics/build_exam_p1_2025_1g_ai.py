"""AI walkthrough for P1-2025 Ex1.f (id 1g) — CLT: P(Xbar > 15) for n=80 with mu=12, sigma^2=380."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from math import erf, sqrt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_p1_2025_1g_ai.png"

mu, sig2, n, x0 = 12.0, 380.0, 80, 15.0
SE = np.sqrt(sig2 / n)
z  = (x0 - mu) / SE
def norm_cdf(t): return 0.5 * (1.0 + erf(t / sqrt(2)))
p_tail = 1.0 - norm_cdf(z)

xx = np.linspace(mu - 4*SE, mu + 4*SE, 600)
pdf = (1.0 / (SE * np.sqrt(2*np.pi))) * np.exp(-0.5 * ((xx - mu) / SE)**2)

fig, ax = plt.subplots(figsize=(10.5, 5.8))
ax.plot(xx, pdf, color=PALETTE["primary"], lw=2.2,
        label=fr"sampling dist. of $\bar X$: $\mathcal{{N}}(12, 380/80)$")
mask = xx >= x0
ax.fill_between(xx[mask], pdf[mask], color=PALETTE["warn"], alpha=0.55,
                label=fr"$P(\bar X > 15) \approx {p_tail:.4f}$")
ax.axvline(mu, color=PALETTE["neutral"], ls=":", lw=1.4,
           label=fr"$\mu = {mu:g}$")
ax.axvline(x0, color=PALETTE["warn"], ls="--", lw=1.8,
           label=fr"threshold $\bar x = {x0:g}$  (z = {z:.3f})")

ax.set_xlabel(r"$\bar X$  (sample mean of next 80 posts)")
ax.set_ylabel("Density")
ax.set_title("P1-2025 Ex1.f — CLT tail probability for next-80-post mean")
ax.legend(loc="upper right", framealpha=0.95)
ax.text(0.02, 0.97,
        f"CLT applies (n={n} large):\n"
        f"$\\bar X \\sim \\mathcal{{N}}(\\mu,\\sigma^2/n)$\n"
        f"$SE = \\sqrt{{380/80}} = {SE:.3f}$\n"
        f"$z = (15 - 12)/{SE:.3f} = {z:.3f}$\n"
        f"$P(\\bar X > 15) = 1 - \\Phi({z:.3f}) \\approx {p_tail:.4f}$",
        transform=ax.transAxes, ha="left", va="top",
        fontsize=10.5, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}   (SE={SE:.4f}, z={z:.4f}, P={p_tail:.6f})")
