"""AI walkthrough for G1-2025 Ex3c — Chi-square independence test
SleepDisorder × BloodPressure."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import chi2, chi2_contingency

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2025_4c_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 1 2025/Data_G_20250108.RData"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Sleep[, c("SleepDisorder","BloodPressure")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)

row_levels = ["None", "Insomnia", "Other"]
col_levels = ["Normal", "High", "VeryHigh"]
table = np.zeros((3, 3), dtype=int)
ridx = {v: i for i, v in enumerate(row_levels)}
cidx = {v: i for i, v in enumerate(col_levels)}
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        table[ridx[row["SleepDisorder"]], cidx[row["BloodPressure"]]] += 1
os.unlink(tmp.name)

chi2_stat, pval, df, expected = chi2_contingency(table)
# Signed Pearson residuals: (O - E)/sqrt(E)
resid = (table - expected) / np.sqrt(expected)

fig = plt.figure(figsize=(13.0, 5.6))
gs = fig.add_gridspec(1, 2, width_ratios=[1.15, 1.0])

# Left: heat-map of observed counts coloured by signed Pearson residual
ax1 = fig.add_subplot(gs[0, 0])
vmax = max(abs(resid.min()), abs(resid.max()))
im = ax1.imshow(resid, cmap="RdBu_r", vmin=-vmax, vmax=vmax, aspect="auto")
ax1.set_xticks(range(3)); ax1.set_xticklabels(col_levels)
ax1.set_yticks(range(3)); ax1.set_yticklabels(row_levels)
ax1.set_xlabel("BloodPressure")
ax1.set_ylabel("SleepDisorder")
ax1.set_title("Observed counts, cells coloured by Pearson residual\n"
              "(blue = under-represented, red = over-represented)")
for i in range(3):
    for j in range(3):
        ax1.text(j, i, f"O={table[i,j]}\nE={expected[i,j]:.1f}\nr={resid[i,j]:+.1f}",
                 ha="center", va="center", fontsize=10,
                 color="white" if abs(resid[i,j]) > vmax * 0.55 else "#222",
                 fontweight="bold" if abs(resid[i,j]) > 2 else "normal")
fig.colorbar(im, ax=ax1, fraction=0.04, pad=0.04, label="Pearson residual")

# Right: chi-square null density, rejection region, observed χ²
ax2 = fig.add_subplot(gs[0, 1])
alpha = 0.01
crit = chi2.ppf(1 - alpha, df)
xs = np.linspace(0, max(40, chi2_stat * 1.05), 800)
ys = chi2.pdf(xs, df)
ax2.plot(xs, ys, color=PALETTE["primary"], lw=2.0,
         label=fr"$\chi^2_{{{df}}}$ null density")
right = xs >= crit
ax2.fill_between(xs[right], 0, ys[right], color=PALETTE["warn"], alpha=0.35,
                 label=fr"Rejection region ($\chi^2 > {crit:.2f}$, $\alpha=0.01$)")
ax2.axvline(chi2_stat, color=PALETTE["accent"], lw=2.0,
            label=fr"$\chi^2_{{\rm obs}} = {chi2_stat:.2f}$ (off-scale)")
ax2.axvline(crit, color=PALETTE["warn"], lw=1.0, ls="--")
# label the observed value at the right edge
ax2.annotate(fr"$\chi^2 = {chi2_stat:.2f}$, $p < 2.2\times10^{{-16}}$",
             xy=(crit, chi2.pdf(crit, df)),
             xytext=(0.40, 0.80), textcoords="axes fraction",
             ha="left", va="center",
             fontsize=10.5, color=PALETTE["primary"], fontweight="bold",
             bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                       boxstyle="round,pad=0.35", linewidth=1.0),
             arrowprops=dict(arrowstyle="->", color=PALETTE["accent"], lw=1.4))
ax2.set_xlabel(r"$\chi^2$")
ax2.set_ylabel("density")
ax2.set_title(r"$H_0$: SleepDisorder $\perp$ BloodPressure — REJECT")
ax2.legend(loc="upper right", framealpha=0.95, fontsize=9.5)
ax2.set_xlim(0, max(40, crit * 4))

fig.suptitle(r"G1-2025 Ex3c — Chi-square independence: $\chi^2_{\rm obs} = 116.32$ on df=4, p $< 2.2\times10^{-16}$ (reject $H_0$)",
             fontsize=13, color=PALETTE["primary"], y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"chi2={chi2_stat:.3f}  df={df}  p={pval:.3e}")
print("expected:\n", np.round(expected, 2))
print("residuals:\n", np.round(resid, 2))
