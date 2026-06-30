"""AI walkthrough for G1-2024 Ex2.a3 — Beta (Type II) when true mu = 58."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2024_ex2_a3_ai.png"

mu0 = 62; mu1 = 58; sigma = 34; n = 832
se = sigma/np.sqrt(n)
x_crit = mu0 + stats.norm.ppf(0.01)*se   # 59.2578...
z_at_alt = (x_crit - mu1)/se
beta = 1 - stats.norm.cdf(z_at_alt)
power = 1 - beta

fig, ax = plt.subplots(figsize=(12.5, 6.2))
xx = np.linspace(54, 64, 800)

# Under H0 boundary
y0 = stats.norm.pdf(xx, mu0, se)
ax.plot(xx, y0, color=PALETTE["primary"], lw=2.2, label=f"H0: $\\bar X \\sim N({mu0}, {se:.3f}^2)$")
ax.fill_between(xx[xx <= x_crit], 0, y0[xx <= x_crit],
                color=PALETTE["warn"], alpha=0.30, label="α = 1%  (reject region)")

# Under H1 (mu=58)
y1 = stats.norm.pdf(xx, mu1, se)
ax.plot(xx, y1, color=PALETTE["accent"], lw=2.2,
        label=f"True: $\\bar X \\sim N({mu1}, {se:.3f}^2)$")
mask_b = xx >= x_crit
ax.fill_between(xx[mask_b], 0, y1[mask_b],
                color=PALETTE["accent"], alpha=0.55,
                label=f"β = {beta:.4f}  (P(not reject | μ=58))")
mask_p = xx < x_crit
ax.fill_between(xx[mask_p], 0, y1[mask_p],
                color=PALETTE["ok"], alpha=0.35,
                label=f"1−β = {power:.4f}  (power)")

ax.axvline(x_crit, color=PALETTE["warn"], lw=1.6)
ax.axvline(mu0, color=PALETTE["primary"], ls="--", lw=1.0)
ax.axvline(mu1, color=PALETTE["accent"], ls="--", lw=1.0)

ax.text(x_crit, max(y0)*1.07, f"x* = {x_crit:.3f}", ha="center",
        fontsize=10.5, color=PALETTE["warn"], fontweight="bold")
ax.text(mu0, max(y0)*1.02, f"μ_0 = {mu0}", ha="center", color=PALETTE["primary"],
        fontsize=10)
ax.text(mu1, max(y1)*1.02, f"μ_1 = {mu1}", ha="center", color=PALETTE["accent"],
        fontsize=10, fontweight="bold")

ax.set_xlabel(r"$\bar X$  (sample mean ReadGrowth)")
ax.set_ylabel("density")
ax.set_title("G1-2024 Ex2.a3 — Probability of NOT rejecting H0 when the true mean is 58")
ax.legend(loc="upper left", framealpha=0.95, fontsize=9.5)

txt = (f"x* = critical value of X̄ from Ex 2.a:\n"
       f"      x* = 62 − z_0.99 · σ/√n = {x_crit:.4f}\n\n"
       f"β(μ=58) = P(X̄ ≥ x* | μ=58)\n"
       f"        = 1 − Φ( (x* − 58) / (σ/√n) )\n"
       f"        = 1 − Φ({z_at_alt:.4f})\n"
       f"        = {beta:.4f}\n\n"
       f"Power = 1 − β = {power:.4f}")
ax.text(0.98, 0.97, txt, transform=ax.transAxes, ha="right", va="top",
        fontsize=10.5, color=PALETTE["primary"], family="monospace",
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.5", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (beta={beta:.5f}, power={power:.5f})")
