"""AI walkthrough for Jul-2024 Ex4.b — CLT for sample proportion, P(p_hat > 0.35)."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from math import erf, sqrt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2024_4b_ai.png"

p, n, thresh = 0.30, 750, 0.35
se = np.sqrt(p*(1-p)/n)
z  = (thresh - p)/se
Phi = lambda z: 0.5 * (1 + erf(z / sqrt(2)))
prob = 1 - Phi(z)

fig, ax = plt.subplots(figsize=(11, 6.0))
xx = np.linspace(p - 5*se, p + 5*se, 600)
yy = (1/(se*np.sqrt(2*np.pi))) * np.exp(-0.5*((xx-p)/se)**2)
ax.plot(xx, yy, color=PALETTE["primary"], lw=2.2,
        label=f"$\\hat p$ ~ N(p = {p}, SE = {se:.5f})")
mask = xx >= thresh
ax.fill_between(xx[mask], 0, yy[mask], color=PALETTE["warn"], alpha=0.5,
                label=f"P($\\hat p$ > {thresh}) ≈ {prob:.5f}")
ax.axvline(p,      color=PALETTE["neutral"], lw=1.1, ls="--", label=f"p = {p}")
ax.axvline(thresh, color=PALETTE["warn"],    lw=1.5,           label=f"threshold = {thresh}")

# annotation: z-score
ax.annotate(f"z = (0.35 − 0.30) / {se:.5f}\n  = {z:.3f}\nP(Z > {z:.3f}) ≈ {prob:.5f}",
            xy=(thresh, yy[np.argmin(abs(xx-thresh))]),
            xytext=(thresh+0.018, max(yy)*0.55),
            arrowprops=dict(arrowstyle="->", color=PALETTE["primary"], lw=1.5),
            fontsize=11, color=PALETTE["primary"],
            bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                      boxstyle="round,pad=0.4", linewidth=1.0))

ax.set_xlabel(r"sample proportion  $\hat p$")
ax.set_ylabel("density")
ax.set_title(f"Jul-2024 Ex4.b — CLT sampling distribution of $\\hat p$  (p={p}, n={n})")
ax.legend(loc="upper left", framealpha=0.95)

ax.text(0.99, 0.95,
        f"Conditions:  np = {n*p:.0f} >= 5  ✓\n"
        f"             n(1-p) = {n*(1-p):.0f} >= 5  ✓\n"
        "=> CLT approximation excellent",
        transform=ax.transAxes, ha="right", va="top",
        fontsize=10, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.4", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (P = {prob:.6f})")
