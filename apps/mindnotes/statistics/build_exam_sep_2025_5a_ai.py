"""AI walkthrough for past-exam Sep-2025 Ex5a — Assumptions for the CI on mu_A - mu_B.

Two panels:
  Left  : visual CLT — distribution of (X_bar_A - X_bar_B) becomes approximately
          Normal for n_A=58, n_B=380 even when the within-group response is
          NOT normal (uses a right-skewed proxy via Gamma to make the point).
  Right : the three-assumptions checklist with verdicts for this exercise.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import gamma, norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_sep_2025_5a_ai.png"

# Reported sample stats from R output (CI.diffmean):
nA, nB = 58, 380
xbar_A, xbar_B = 78.17, 82.74
sA, sB = 6.66, 6.53

rng = np.random.default_rng(7)

# --- Build a right-skewed within-group population (Gamma) with matching mean+sd ---
# Gamma(k, theta): mean = k*theta, var = k*theta^2 ; for visibly skew, use moderate k.
def gamma_params_from_mean_sd(mean, sd):
    var = sd ** 2
    theta = var / mean
    k = mean / theta
    return k, theta

kA, tA = gamma_params_from_mean_sd(xbar_A, sA)
kB, tB = gamma_params_from_mean_sd(xbar_B, sB)

# Simulate sampling distribution of (X_bar_A - X_bar_B)
B = 4000
diffs = np.empty(B)
for i in range(B):
    a = rng.gamma(shape=kA, scale=tA, size=nA)
    b = rng.gamma(shape=kB, scale=tB, size=nB)
    diffs[i] = a.mean() - b.mean()

# Analytical CLT approximation
mu_diff = xbar_A - xbar_B
se_pool = np.sqrt(sA**2 / nA + sB**2 / nB)
x_grid = np.linspace(mu_diff - 4*se_pool, mu_diff + 4*se_pool, 400)
normal_pdf = norm.pdf(x_grid, loc=mu_diff, scale=se_pool)

fig, axes = plt.subplots(1, 2, figsize=(13, 5.2))

# --- LEFT: histogram of simulated X_bar_A - X_bar_B vs Normal overlay ---
ax = axes[0]
ax.hist(diffs, bins=40, density=True, color=PALETTE["accent"], alpha=0.55,
        edgecolor=PALETTE["primary"], linewidth=0.6,
        label=f"sampling distr. of $\\bar X_A - \\bar X_B$\n"
              f"(simulated; underlying within-group law: skewed)")
ax.plot(x_grid, normal_pdf, color=PALETTE["primary"], linewidth=2.4,
        label=f"$N(\\mu_A-\\mu_B,\\;s_A^2/n_A + s_B^2/n_B)$\n"
              f"$SE = {se_pool:.3f}$")
ax.axvline(mu_diff, color=PALETTE["warn"], linestyle="--", linewidth=1.5,
           label=f"$\\bar x_A - \\bar x_B = {mu_diff:.2f}$")
ax.set_xlabel("$\\bar X_A - \\bar X_B$")
ax.set_ylabel("density")
ax.set_title(f"CLT in action — $n_A = {nA}$, $n_B = {nB}$ are large enough\n"
             "even if `Performance` within each group is not Normal")
ax.legend(loc="upper left", framealpha=0.95, fontsize=9)

# --- RIGHT: three-assumption checklist ---
ax2 = axes[1]
ax2.axis("off")
ax2.set_xlim(0, 1)
ax2.set_ylim(0, 1)

ax2.text(0.5, 0.96, "Three assumptions for an independent-samples CI on $\\mu_A-\\mu_B$",
         ha="center", va="top", fontsize=12, fontweight="bold",
         color=PALETTE["primary"])

rows = [
    ("(i) Independence",
     "A & B participants randomly assigned\nto different training programs.",
     "OK — design ensures it",
     PALETTE["accent"]),
    ("(ii) Normality of the response\n    in each group",
     f"$n_A = {nA}$ and $n_B = {nB}$ are large\n$\\Rightarrow$ CLT $\\Rightarrow$ $\\bar X_A - \\bar X_B$ approx. Normal.",
     "RELAXED via CLT",
     PALETTE["accent"]),
    ("(iii) Equality of variances\n    ($\\sigma_A^2 = \\sigma_B^2$)",
     "Decides between pooled-variance vs Welch.\nVerified in 5.b with a Levene test.",
     "CHECK with Levene (next step)",
     PALETTE["warn"]),
]

y0 = 0.84
dy = 0.27
for i, (head, body, verdict, color) in enumerate(rows):
    y = y0 - i*dy
    # left box
    ax2.add_patch(plt.Rectangle((0.02, y - 0.20), 0.55, 0.22,
                  facecolor="#fffbe6" if color == PALETTE["warn"] else "#eef5ff",
                  edgecolor=PALETTE["primary"], linewidth=1.0))
    ax2.text(0.04, y - 0.01, head, ha="left", va="top",
             fontsize=10.5, fontweight="bold", color=PALETTE["primary"])
    ax2.text(0.04, y - 0.09, body, ha="left", va="top",
             fontsize=9.5, color=PALETTE["primary"])
    # right verdict
    ax2.add_patch(plt.Rectangle((0.60, y - 0.13), 0.38, 0.12,
                  facecolor=color, edgecolor=PALETTE["primary"],
                  linewidth=1.0, alpha=0.55))
    ax2.text(0.79, y - 0.07, verdict, ha="center", va="center",
             fontsize=10, fontweight="bold", color=PALETTE["primary"])

# R reminder
ax2.text(0.5, 0.03,
         "R: CI.diffmean(..., type='independent', var.test=TRUE, conf.level=0.90)\n"
         "    -> returns pooled + Welch CIs AND the Levene test in one shot",
         ha="center", va="bottom", fontsize=9.5, family="monospace",
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

fig.suptitle("CI for $\\mu_A - \\mu_B$ — which assumptions matter, which can we relax?",
             fontsize=12.5, y=1.02, color=PALETTE["primary"])

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  nA={nA} nB={nB}  SE(diff)={se_pool:.4f}  xbarA-xbarB={mu_diff:.3f}")
