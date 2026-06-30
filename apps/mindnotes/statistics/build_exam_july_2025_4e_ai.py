"""AI walkthrough for Jul-2025 Ex4e — goodness-of-fit of mod2 via R^2.

Two panels:
 Left  : R^2 / 1-R^2 stacked bar (explained vs unexplained).
 Right : Investments observed vs fitted scatter to show residual scatter.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
import subprocess, tempfile, csv

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2025_4e_ai.png"

R2 = 0.1085
R2_adj = 0.104

# Pull fitted values vs actual via R
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/july 2025/Exam202507.RData"
tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); m <- lm(Investments ~ Branch + AgeC + Cards + Tenure, data=BankClients); '
                f'write.csv(data.frame(obs=BankClients$Investments, fit=fitted(m), res=resid(m)), "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
obs, fit, res = [], [], []
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        obs.append(float(row["obs"]))
        fit.append(float(row["fit"]))
        res.append(float(row["res"]))
os.unlink(tmp.name)
obs = np.array(obs); fit = np.array(fit); res = np.array(res)

fig, axes = plt.subplots(1, 2, figsize=(13, 5.3),
                         gridspec_kw={"width_ratios": [0.7, 1.3]})

# --- LEFT: R^2 vs 1-R^2 stacked vertical bar ---
ax = axes[0]
ax.bar([0], [R2], width=0.6, color=PALETTE["accent"], alpha=0.7,
       edgecolor=PALETTE["primary"], linewidth=1.2,
       label=f"explained by mod2: $R^2 = {R2:.4f}$")
ax.bar([0], [1 - R2], width=0.6, bottom=[R2], color=PALETTE["muted"],
       alpha=0.55, edgecolor=PALETTE["primary"], linewidth=1.2,
       label=f"unexplained: $1-R^2 = {1-R2:.4f}$")
ax.text(0, R2 / 2, f"{R2*100:.2f}%",
        ha="center", va="center", fontsize=12, color=PALETTE["primary"],
        fontweight="bold")
ax.text(0, R2 + (1 - R2) / 2, f"{(1-R2)*100:.2f}%",
        ha="center", va="center", fontsize=12, color=PALETTE["primary"],
        fontweight="bold")
ax.set_xticks([]); ax.set_ylim(0, 1.0)
ax.set_ylabel("share of TSS (variance of Investments)")
ax.set_title(f"Step 1 — $R^2 = 1 - RSS/TSS = {R2:.4f}$\n"
             f"adj $R^2 = {R2_adj:.4f}$  =>  only ~11% explained")
ax.legend(loc="upper right", framealpha=0.95)

# --- RIGHT: observed vs fitted scatter ---
ax2 = axes[1]
cap = float(np.quantile(obs, 0.99))
mask = obs <= cap
ax2.scatter(fit[mask], obs[mask], s=8, alpha=0.35,
            color=PALETTE["secondary"], edgecolor="none",
            label=f"clients ({mask.sum()} shown of {len(obs)})")
lo, hi = 0, cap
ax2.plot([lo, hi], [lo, hi], color=PALETTE["warn"], linestyle="--",
         linewidth=1.7, label="y = x (perfect fit)")
ax2.set_xlim(lo, hi); ax2.set_ylim(lo, cap * 1.05)
ax2.set_xlabel("fitted Investments (mod2)")
ax2.set_ylabel("observed Investments")
ax2.set_title("Step 2 — observed vs fitted Investments\n"
              "wide vertical spread = large residuals = low $R^2$")
ax2.legend(loc="upper left", framealpha=0.95)

# Conclusion box
ax2.text(0.98, 0.05,
         "Conclusion:\n"
         "$R^2 \\approx 11\\%$  =>  the model captures\n"
         "only a small fraction of the variation\n"
         "in Investments. Other key drivers are\n"
         "missing  =>  mod2 is NOT suitable for\n"
         "individual-client predictions.",
         transform=ax2.transAxes, ha="right", va="bottom",
         fontsize=10.5, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle(f"Jul-2025 Ex4e  —  Goodness-of-fit of mod2  $R^2 = {R2:.4f}$",
             fontsize=12, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
