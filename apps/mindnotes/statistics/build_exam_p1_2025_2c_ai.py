"""AI walkthrough for P1-2025 Ex2.a (id 2c) — Sample proportion p-hat is unbiased for p."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_p1_2025_2c_ai.png"

rng = np.random.default_rng(20240101)
p, n, R = 0.30, 200, 5000
sims = rng.binomial(n=n, p=p, size=R) / n     # 5000 sample proportions
mean_sim = float(sims.mean())
se_sim   = float(sims.std(ddof=0))
se_th    = float(np.sqrt(p * (1 - p) / n))

# Also demonstrate n does not affect unbiasedness: mean of p-hat for several n stays at p
n_grid = np.array([10, 25, 50, 100, 250, 500, 1000])
means_by_n = np.array([np.mean(rng.binomial(n_i, p, size=2000) / n_i) for n_i in n_grid])

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.5))

# Left: histogram of 5000 simulated p-hat centered on p
ax1.hist(sims, bins=40, color=PALETTE["secondary"], alpha=0.6,
         edgecolor=PALETTE["primary"], linewidth=0.5,
         label=f"5000 sim. $\\hat p$  (n={n}, p={p})")
ax1.axvline(p, color=PALETTE["ok"], lw=2.4, ls="--",
            label=f"true p = {p}")
ax1.axvline(mean_sim, color=PALETTE["warn"], lw=2.2, ls=":",
            label=f"mean of $\\hat p$ = {mean_sim:.4f}")
ax1.set_xlabel(r"$\hat p = \bar X$")
ax1.set_ylabel("Frequency")
ax1.set_title("Sampling distribution of $\\hat p$ is centred on $p$")
ax1.legend(loc="upper right", framealpha=0.95)
ax1.text(0.02, 0.97,
         f"$E[\\hat p] = \\frac{{1}}{{n}} \\sum E[X_i] = \\frac{{1}}{{n}} (np) = p$\n"
         f"Simulated $E[\\hat p] \\approx {mean_sim:.4f}$\n"
         f"Simulated $SE \\approx {se_sim:.4f}$\n"
         f"Theoretical $SE = \\sqrt{{p(1-p)/n}} = {se_th:.4f}$",
         transform=ax1.transAxes, ha="left", va="top",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

# Right: mean of p-hat vs n (stays at p for every n)
ax2.plot(n_grid, means_by_n, "o-", color=PALETTE["primary"], lw=2.0, markersize=8,
         label=r"$\overline{\hat p}$ over 2000 sims")
ax2.axhline(p, color=PALETTE["ok"], lw=2.0, ls="--", label=f"true p = {p}")
ax2.set_xscale("log")
ax2.set_xlabel("sample size n (log)")
ax2.set_ylabel(r"average $\hat p$ over repeated samples")
ax2.set_title("Unbiasedness holds for every n")
ax2.legend(loc="upper right", framealpha=0.95)
ax2.set_ylim(p - 0.02, p + 0.02)

plt.suptitle(r"P1-2025 Ex2.a — $\hat p = \bar X$ is unbiased for $p$", y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}   (E[phat]={mean_sim:.4f}, SE_sim={se_sim:.4f}, SE_th={se_th:.4f})")
