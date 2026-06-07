"""Build AI walkthrough plot for Ex 4.4 a–c — call duration X ~ N(180, 1600).

Three panels on a shared normal density:
  a) Pr(X > 120)         right-tail beyond 120 s     (P = 0.9332)
  b) 15-th percentile    left-tail mass = 0.15       (q = 138.54 s)
  c) Pr(X > 300)         far right tail beyond 5 min (P = 0.00135)
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex4/ex4_4a1_ai.png"

mu, sigma = 180.0, 40.0  # variance 1600 -> sd 40
xs = np.linspace(mu - 4 * sigma, mu + 4 * sigma, 600)
pdf = norm.pdf(xs, mu, sigma)

fig, axes = plt.subplots(1, 3, figsize=(15, 4.6), sharey=True)

# ---------- a) Pr(X > 120) ----------
ax = axes[0]
ax.plot(xs, pdf, color=PALETTE["primary"], lw=2.2)
mask = xs > 120
ax.fill_between(xs[mask], 0, pdf[mask], color=PALETTE["primary"], alpha=0.32)
p_a = 1 - norm.cdf(120, mu, sigma)
ax.axvline(120, color=PALETTE["accent"], lw=1.4, ls="--")
ax.text(120, ax.get_ylim()[1] * 0.92, "  120 s", color=PALETTE["accent"],
        fontsize=10, ha="left", va="top", fontweight="bold")
ax.text(0.55, 0.55,
        fr"$\Pr(X>120)$" + "\n" + fr"$= {p_a:.4f}$",
        transform=ax.transAxes, fontsize=12, ha="left", va="center",
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))
ax.set_title(r"a)  $\Pr(X>120)$  —  right tail")
ax.set_xlabel("call duration  (seconds)")
ax.set_ylabel("density")

# ---------- b) 15-th percentile ----------
ax = axes[1]
ax.plot(xs, pdf, color=PALETTE["primary"], lw=2.2)
q15 = norm.ppf(0.15, mu, sigma)
mask = xs < q15
ax.fill_between(xs[mask], 0, pdf[mask], color=PALETTE["accent"], alpha=0.45)
ax.axvline(q15, color=PALETTE["accent"], lw=1.4, ls="--")
ax.text(q15, ax.get_ylim()[1] * 0.92, f"  $q_{{0.15}}={q15:.2f}$",
        color=PALETTE["accent"], fontsize=10, ha="left", va="top",
        fontweight="bold")
ax.text(0.55, 0.55,
        r"$\Pr(X<q_{0.15})=0.15$" + "\n" + fr"$q_{{0.15}}={q15:.2f}$ s",
        transform=ax.transAxes, fontsize=12, ha="left", va="center",
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))
ax.set_title(r"b)  15-th percentile  —  shortest 15%")
ax.set_xlabel("call duration  (seconds)")

# ---------- c) Pr(X > 300) ----------
ax = axes[2]
ax.plot(xs, pdf, color=PALETTE["primary"], lw=2.2)
mask = xs > 300
ax.fill_between(xs[mask], 0, pdf[mask], color="#d4493a", alpha=0.55)
p_c = 1 - norm.cdf(300, mu, sigma)
ax.axvline(300, color="#d4493a", lw=1.4, ls="--")
ax.text(300, ax.get_ylim()[1] * 0.92, "  300 s  =  5 min",
        color="#d4493a", fontsize=10, ha="left", va="top", fontweight="bold")
ax.text(0.05, 0.55,
        fr"$\Pr(X>300)$" + "\n" + fr"$= {p_c:.5f}$" + "\n" + r"$\approx 0.135\%$",
        transform=ax.transAxes, fontsize=12, ha="left", va="center",
        bbox=dict(facecolor="#fffbe6", edgecolor="#d4493a",
                  boxstyle="round,pad=0.45", linewidth=1.0))
ax.set_title(r"c)  $\Pr(X>300)$  —  callback cut-off")
ax.set_xlabel("call duration  (seconds)")

# common annotation across all panels
for ax in axes:
    ax.axvline(mu, color=PALETTE["neutral"], lw=1.0, ls=":", alpha=0.6)
    ax.set_xlim(mu - 4 * sigma, mu + 4 * sigma)

fig.suptitle(r"Ex 4.4 a–c  —  $X\sim N(180,\,40^2)$, the three target probabilities",
             fontsize=13, y=1.02)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  a) Pr(X>120) = {p_a:.6f}")
print(f"  b) q_0.15    = {q15:.6f}")
print(f"  c) Pr(X>300) = {p_c:.6f}")
