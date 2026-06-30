"""AI walkthrough for Jul-2025 Ex3a — sample mean as unbiased estimator
of the population mean. Two panels:
  Left  : sampling distribution of X-bar: as n grows the spread shrinks
          (Var(X-bar) = sigma^2 / n), three densities for n=100/300/998.
  Right : the actual Savings sample with x-bar = 275.33 marked.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm
import subprocess, tempfile, csv

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2025_3a_ai.png"

RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/july 2025/Exam202507.RData"
tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(BankClients[, "Savings"], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
sav = []
with open(tmp.name) as f:
    rd = csv.reader(f); next(rd)
    for row in rd:
        sav.append(float(row[0]))
os.unlink(tmp.name)
sav = np.array(sav)
n = len(sav)
xbar = float(np.mean(sav))
s = float(np.std(sav, ddof=1))
mu_assumed = xbar          # use the sample mean as a working "population" centre
sigma_assumed = 800.0      # the problem gives sigma = 800 in part b

fig, axes = plt.subplots(1, 2, figsize=(13, 5.3),
                         gridspec_kw={"width_ratios": [1.1, 1.0]})

# --- LEFT: sampling distribution of X-bar for different n ---
ax = axes[0]
nn_list = [100, 300, n]
colors_n = [PALETTE["secondary"], PALETTE["accent"], PALETTE["primary"]]
x_grid = np.linspace(mu_assumed - 200, mu_assumed + 200, 800)
for nn, c in zip(nn_list, colors_n):
    se = sigma_assumed / np.sqrt(nn)
    pdf = norm.pdf(x_grid, loc=mu_assumed, scale=se)
    ax.plot(x_grid, pdf, color=c, linewidth=2.0,
            label=f"$n = {nn}$,  $SE = \\sigma/\\sqrt{{n}} = {se:.2f}$")
ax.axvline(mu_assumed, color=PALETTE["warn"], linestyle="--", linewidth=1.6,
           label=f"$\\mu$  (centre)")
ax.set_xlabel("$\\bar{X}$  (estimator value)")
ax.set_ylabel("density of sampling distribution")
ax.set_title("Step 1 — $\\bar{X}$ is unbiased: $E[\\bar X]=\\mu$\n"
             "Var$(\\bar X) = \\sigma^2/n$  $\\Rightarrow$  spread shrinks like $1/\\sqrt{n}$")
ax.legend(loc="upper right", framealpha=0.95)

# --- RIGHT: actual Savings sample histogram + xbar ---
ax2 = axes[1]
# cap the histogram x-axis at the 95th percentile to keep it readable
cap = float(np.quantile(sav, 0.95))
bins = np.linspace(0, cap, 36)
ax2.hist(sav[sav <= cap], bins=bins, color=PALETTE["secondary"],
         edgecolor=PALETTE["primary"], alpha=0.55, linewidth=0.8)
ax2.axvline(xbar, color=PALETTE["warn"], linestyle="-", linewidth=2.4,
            label=f"$\\bar x = {xbar:.2f}$  (estimate)")
ax2.set_xlabel("Savings (€)")
ax2.set_ylabel("count")
ax2.set_title(f"Step 2 — observed sample, $n = {n}$\n"
              f"$\\bar x = \\frac{{1}}{{n}}\\sum_i X_i = {xbar:.2f}$\u20ac")
ax2.legend(loc="upper right", framealpha=0.95)
ax2.text(0.97, 0.55,
         "Properties of $\\bar X$:\n"
         "(1) $E[\\bar X] = \\mu$  (unbiased)\n"
         "(2) $\\mathrm{Var}(\\bar X) = \\sigma^2/n$\n"
         "(3) consistent  ($n \\to \\infty$  $\\Rightarrow$  $\\bar X \\to \\mu$)\n"
         "(4) BLUE under i.i.d. + finite variance",
         transform=ax2.transAxes, ha="right", va="top",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle("Jul-2025 Ex3a  —  sample mean $\\bar X = \\frac{1}{n}\\sum_i X_i$ "
             f"estimates the population mean Savings, here $\\bar x = {xbar:.2f}$\u20ac",
             fontsize=12, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  n={n}  xbar={xbar:.4f}  s={s:.4f}")
