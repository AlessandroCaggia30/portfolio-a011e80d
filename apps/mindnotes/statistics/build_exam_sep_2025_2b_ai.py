"""AI walkthrough for past-exam Sep-2025 Ex2b — two-sided z-test from estimate & SE.

Estimate beta_hat = 0.510, SE = 0.221 -> z = 2.308, p = 0.021.

Two panels:
  Left  : standard normal pdf with both tails (|z|>=2.308) shaded; z markers + p-value label.
  Right : decision diagram — p-value compared to alpha grid {0.01,0.025,0.05,0.10}.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_sep_2025_2b_ai.png"

beta = 0.510
se = 0.221
z = beta / se
pval = 2 * (1 - norm.cdf(abs(z)))

fig, axes = plt.subplots(1, 2, figsize=(13, 5))

# --- LEFT: N(0,1) with two-sided rejection region shaded ---
x = np.linspace(-4, 4, 600)
pdf = norm.pdf(x)
ax = axes[0]
ax.plot(x, pdf, color=PALETTE["primary"], linewidth=2.2)
# fill both tails beyond |z|
mask_r = x >= z
mask_l = x <= -z
ax.fill_between(x[mask_r], 0, pdf[mask_r], color=PALETTE["warn"], alpha=0.45,
                label=f"two-sided tail area $= p \\approx {pval:.3f}$")
ax.fill_between(x[mask_l], 0, pdf[mask_l], color=PALETTE["warn"], alpha=0.45)
ax.axvline(z, color=PALETTE["primary"], linestyle="--", linewidth=1.3)
ax.axvline(-z, color=PALETTE["primary"], linestyle="--", linewidth=1.3)
ax.annotate(f"$z = {z:.3f}$", xy=(z, norm.pdf(z)), xytext=(z + 0.3, 0.18),
            fontsize=11, color=PALETTE["primary"],
            arrowprops=dict(arrowstyle="->", color=PALETTE["primary"]))
ax.annotate(f"$-z = {-z:.3f}$", xy=(-z, norm.pdf(-z)), xytext=(-3.9, 0.18),
            fontsize=11, color=PALETTE["primary"],
            arrowprops=dict(arrowstyle="->", color=PALETTE["primary"]))
ax.set_xlabel("standardized statistic")
ax.set_ylabel("$\\varphi(x)$ — $N(0,1)$ density")
ax.set_title("Step 1 — Wald $z$ and two-sided p-value\n"
             f"$z=\\hat\\beta/\\mathrm{{SE}}={beta}/{se}={z:.3f}$")
ax.set_xlim(-4, 4)
ax.set_ylim(0, 0.45)
ax.legend(loc="upper right", framealpha=0.95)

# --- RIGHT: decision bars across alpha levels ---
alphas = np.array([0.01, 0.025, 0.05, 0.10])
labels = [f"$\\alpha = {a}$" for a in alphas]
reject = pval < alphas
colors = [PALETTE["warn"] if r else PALETTE["muted"] for r in reject]

ax2 = axes[1]
ypos = np.arange(len(alphas))
ax2.barh(ypos, alphas, color=colors, edgecolor=PALETTE["primary"], linewidth=1.1,
         alpha=0.55, label="$\\alpha$ threshold")
ax2.axvline(pval, color=PALETTE["primary"], linestyle="--", linewidth=1.8,
            label=f"p-value $\\approx {pval:.3f}$")
for i, (a, r) in enumerate(zip(alphas, reject)):
    txt = "reject $H_0$" if r else "fail to reject"
    ax2.text(max(a, pval) + 0.005, i, txt, va="center", ha="left",
             fontsize=10, color=PALETTE["primary"],
             fontweight="bold" if r else "normal")
ax2.set_yticks(ypos)
ax2.set_yticklabels(labels)
ax2.set_xlabel("probability")
ax2.set_xlim(0, 0.16)
ax2.set_title("Step 2 — Decision rule: reject $H_0$ iff $p < \\alpha$\n"
              "moderate evidence: significant at 2.5%/5%/10% but not 1%")
ax2.legend(loc="lower right", framealpha=0.95)

# R-command box
ax2.text(0.02, 0.05,
         "R command:\n"
         "z <- 0.510/0.221\n"
         "2*(1-pnorm(abs(z)))\n"
         f"# z = {z:.3f}\n"
         f"# p = {pval:.4f}",
         transform=ax2.transAxes, ha="left", va="bottom",
         fontsize=10, family="monospace",
         bbox=dict(facecolor="#fffbe6",
                   edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle(f"Two-sided Wald test  $H_0:\\beta=0$  vs  $H_1:\\beta\\neq 0$  "
             f"$\\Rightarrow$  $z={z:.3f}$, $p\\approx{pval:.3f}$",
             fontsize=12, y=1.02, color=PALETTE["primary"])

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  beta={beta}  se={se}  z={z:.4f}  p={pval:.5f}")
