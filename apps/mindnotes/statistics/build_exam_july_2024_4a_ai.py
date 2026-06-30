"""AI walkthrough for Jul-2024 Ex4.a — P(Top10 > 50) via Z standardisation."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from math import erf, sqrt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2024_4a_ai.png"

mu, sd, x0 = 28.8, 16.3, 50
z0 = (x0 - mu) / sd
Phi = lambda z: 0.5 * (1 + erf(z / sqrt(2)))
prob = 1 - Phi(z0)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.5))

# Left: original scale
xx = np.linspace(mu - 4*sd, mu + 4*sd, 600)
yy = (1/(sd*np.sqrt(2*np.pi))) * np.exp(-0.5*((xx-mu)/sd)**2)
ax1.plot(xx, yy, color=PALETTE["primary"], lw=2.2)
mask = xx >= x0
ax1.fill_between(xx[mask], 0, yy[mask], color=PALETTE["warn"], alpha=0.45,
                 label=f"P(Top10 > {x0}) ≈ {prob:.4f}")
ax1.axvline(mu, color=PALETTE["neutral"], lw=1.1, ls="--", label=f"μ = {mu}")
ax1.axvline(x0, color=PALETTE["warn"], lw=1.5)
ax1.set_xlabel("Top10  (%)")
ax1.set_ylabel("density")
ax1.set_title(f"X ~ N(μ = {mu}, σ = {sd})  on original scale")
ax1.legend(loc="upper right", framealpha=0.95)
ax1.text(x0+0.4, max(yy)*0.5, f"x = {x0}", color=PALETTE["warn"], fontsize=10)

# Right: standardised scale
zz = np.linspace(-4, 4, 600)
yz = (1/np.sqrt(2*np.pi)) * np.exp(-0.5*zz**2)
ax2.plot(zz, yz, color=PALETTE["primary"], lw=2.2)
maskz = zz >= z0
ax2.fill_between(zz[maskz], 0, yz[maskz], color=PALETTE["warn"], alpha=0.45,
                 label=f"P(Z > {z0:.3f}) ≈ {prob:.4f}")
ax2.axvline(0, color=PALETTE["neutral"], lw=1.1, ls="--")
ax2.axvline(z0, color=PALETTE["warn"], lw=1.5)
ax2.set_xlabel("Z = (X − μ)/σ")
ax2.set_ylabel("density")
ax2.set_title(f"Standardised:  z₀ = (50 − 28.8)/16.3 = {z0:.3f}")
ax2.legend(loc="upper right", framealpha=0.95)

fig.text(0.5, -0.02,
         f"Answer: P(Top10 > 50) = 1 − Φ({z0:.3f}) ≈ {prob:.4f}  (~{prob*100:.1f}% of colleges)",
         ha="center", fontsize=12, color=PALETTE["primary"], fontweight="bold",
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.5", linewidth=1.0))
fig.suptitle("Jul-2024 Ex4.a — Normal-tail probability via Z standardisation",
             fontsize=12.5, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (P = {prob:.5f})")
