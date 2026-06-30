"""AI walkthrough for Jul-2025 Ex5b — CLT-based probability that the sample
proportion of card-intensive clients exceeds 0.30, in a new sample of 1200
clients drawn from the other branch.

Two panels:
 Left  : sampling distribution of p_hat ~ N(p, p(1-p)/n) for p=0.3397 and
         alternative p=0.35; threshold = 0.30 shaded.
 Right : numerical decision panel with both probabilities.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2025_5b_ai.png"

n = 1200
thr = 0.30
p_main = 0.3397
p_alt = 0.35

SE_main = np.sqrt(p_main * (1 - p_main) / n)
SE_alt = np.sqrt(p_alt * (1 - p_alt) / n)
z_main = (thr - p_main) / SE_main          # negative -> upper tail huge
z_alt = (thr - p_alt) / SE_alt
P_main = 1 - norm.cdf(z_main)
P_alt = 1 - norm.cdf(z_alt)

fig, axes = plt.subplots(1, 2, figsize=(13, 5.3),
                         gridspec_kw={"width_ratios": [1.25, 0.85]})

# --- LEFT: two sampling distributions ---
ax = axes[0]
x = np.linspace(0.27, 0.40, 600)
pdf_m = norm.pdf(x, loc=p_main, scale=SE_main)
pdf_a = norm.pdf(x, loc=p_alt, scale=SE_alt)
ax.plot(x, pdf_m, color=PALETTE["primary"], linewidth=2.2,
        label=f"$\\hat p \\sim N(p, p(1-p)/n)$,  $p={p_main}$,  SE={SE_main:.4f}")
ax.plot(x, pdf_a, color=PALETTE["warn"], linewidth=2.2,
        label=f"alt: $p={p_alt}$,  SE={SE_alt:.4f}")

mask = x >= thr
ax.fill_between(x[mask], 0, pdf_m[mask], color=PALETTE["primary"], alpha=0.25)
ax.fill_between(x[mask], 0, pdf_a[mask], color=PALETTE["warn"], alpha=0.25)
ax.axvline(thr, color=PALETTE["accent"], linestyle="--", linewidth=2.0,
           label=f"threshold = {thr}")
ax.axvline(p_main, color=PALETTE["primary"], linestyle=":", linewidth=1.2)
ax.axvline(p_alt, color=PALETTE["warn"], linestyle=":", linewidth=1.2)

ax.annotate(f"$P(\\hat p > {thr}) \\approx {P_main:.4f}$",
            xy=(0.33, pdf_m.max() * 0.4),
            xytext=(0.31, pdf_m.max() * 0.95),
            fontsize=10, color=PALETTE["primary"], fontweight="bold",
            arrowprops=dict(arrowstyle="->", color=PALETTE["primary"]))
ax.annotate(f"$P(\\hat p > {thr}) \\approx {P_alt:.4f}$",
            xy=(0.34, pdf_a.max() * 0.55),
            xytext=(0.355, pdf_a.max() * 0.95),
            fontsize=10, color=PALETTE["warn"], fontweight="bold",
            arrowprops=dict(arrowstyle="->", color=PALETTE["warn"]))

ax.set_xlabel("$\\hat p$ (sample proportion in n=1200)")
ax.set_ylabel("density")
ax.set_title("Step 1 — CLT-based sampling distribution of $\\hat p$\n"
             "shaded = upper tail right of $\\hat p = 0.30$")
ax.set_xlim(0.27, 0.40)
ax.legend(loc="upper left", framealpha=0.95, fontsize=9.5)

# --- RIGHT: decision panel ---
ax2 = axes[1]
ax2.axis("off")

ax2.text(0.5, 0.93,
         f"Main (p = $\\hat p_{{obs}}$ = {p_main}):\n"
         f"$z = (0.30 - {p_main})/SE = {z_main:.4f}$\n"
         f"$P(\\hat p > 0.30) = 1-\\Phi({z_main:.3f}) = {P_main:.4f}$",
         ha="center", va="top", fontsize=10.5, color=PALETTE["primary"],
         bbox=dict(facecolor="#f4f7fb", edgecolor=PALETTE["primary"],
                   boxstyle="round,pad=0.5", linewidth=1.0))

ax2.text(0.5, 0.58,
         f"Alternative (p = {p_alt}):\n"
         f"$z = (0.30 - {p_alt})/SE = {z_alt:.4f}$\n"
         f"$P(\\hat p > 0.30) \\approx {P_alt:.4f}$",
         ha="center", va="top", fontsize=10.5, color=PALETTE["warn"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.5", linewidth=1.0))

ax2.text(0.5, 0.25,
         "Conclusion: under both p values,\n"
         "P(at least 30%) is essentially 1.\n"
         "The sample (n=1200) is so large that\n"
         "the sampling distribution is tight around\n"
         "its mean, which exceeds 0.30.",
         ha="center", va="top", fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fafafa", edgecolor=PALETTE["muted"],
                   boxstyle="round,pad=0.4", linewidth=0.8))

fig.suptitle("Jul-2025 Ex5b  —  CLT: probability $\\hat p > 0.30$ in n=1200 clients  "
             f"= {P_main:.4f}  (alt: {P_alt:.4f})",
             fontsize=12, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  P(p_hat>0.30 | p=0.3397) = {P_main:.6f}")
print(f"  P(p_hat>0.30 | p=0.35)   = {P_alt:.6f}")
