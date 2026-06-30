"""AI walkthrough for past-exam Jul-2025 Ex2b — 99th percentile of Loans by AgeC.

Uses real data from Exam202507.RData. Two panels:
 Left  : Loans distributions by AgeC (density curves) with the 99th
         percentile vertical lines clearly marked for each group.
 Right : compact bar chart comparing the three p99 values + "the maximum
         loan debt that affects 99% of clients" annotation.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
import subprocess, tempfile, csv
from scipy.stats import gaussian_kde

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2025_2b_ai.png"

RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/july 2025/Exam202507.RData"
tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(BankClients[,c("AgeC","Loans")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
groups = {"adult": [], "senior": [], "young": []}
with open(tmp.name) as f:
    for row in csv.DictReader(f):
        groups[row["AgeC"]].append(float(row["Loans"]))
os.unlink(tmp.name)

order = ["adult", "senior", "young"]
colors = {"adult": PALETTE["secondary"], "senior": PALETTE["accent"], "young": PALETTE["ok"]}
p99 = {k: float(np.quantile(groups[k], 0.99)) for k in order}
ns = {k: len(groups[k]) for k in order}

fig, axes = plt.subplots(1, 2, figsize=(13, 5.2),
                         gridspec_kw={"width_ratios": [2.0, 1.0]})

# --- LEFT: density estimates + p99 marks ---
ax = axes[0]
x_grid = np.linspace(0, 220000, 1000)
for k in order:
    d = np.array(groups[k])
    kde = gaussian_kde(d, bw_method=0.35)
    y = kde(x_grid)
    ax.plot(x_grid, y, color=colors[k], linewidth=2.0, label=f"{k} (n={ns[k]})")
    ax.fill_between(x_grid, 0, y, color=colors[k], alpha=0.18)
    ax.axvline(p99[k], color=colors[k], linestyle="--", linewidth=1.7, alpha=0.85)
    ax.annotate(f"p99 = {p99[k]:,.0f}", xy=(p99[k], 0),
                xytext=(p99[k] + 1500, ax.get_ylim()[1] * 0.5 if hasattr(ax, 'get_ylim') else 1e-5),
                rotation=90, fontsize=9, color=colors[k], fontweight="bold")
ax.set_xlim(0, 220000)
ax.set_xlabel("Loans (€)")
ax.set_ylabel("density (KDE)")
ax.set_title("Step 1 — distribution of Loans by AgeC, with $q_{0.99}$ marked\n"
             "P(Loans \u2264 $q_{0.99}$ | AgeC) = 0.99")
ax.legend(loc="upper right", framealpha=0.95)

# --- RIGHT: p99 bar comparison ---
ax2 = axes[1]
vals = [p99[k] for k in order]
bars = ax2.bar(order, vals, color=[colors[k] for k in order],
               edgecolor=PALETTE["primary"], linewidth=1.3, alpha=0.7)
for i, v in enumerate(vals):
    ax2.text(i, v + 2500, f"{v:,.0f}", ha="center", va="bottom",
             fontsize=11, color=PALETTE["primary"], fontweight="bold")
ax2.set_ylabel("Loans (€)  —  99th percentile")
ax2.set_title("Step 2 — three p99 values\n"
              "max loan-debt at which 99% of clients lie\n"
              "(adult > senior > young, but ranges are close)")
ax2.set_ylim(0, max(vals) * 1.12)
ax2.grid(True, axis="y", alpha=0.4)

ax.text(0.02, 0.97,
        "Reading:\n"
        f"  adult:  p99 = {p99['adult']:,.0f}\n"
        f"  senior: p99 = {p99['senior']:,.0f}\n"
        f"  young:  p99 = {p99['young']:,.0f}\n"
        "Interpretation: 99% of adult clients have Loans <= 143k;\n"
        "the corresponding upper limits for senior/young are\n"
        "noticeably lower (~127k and ~120k).",
        transform=ax.transAxes, ha="left", va="top",
        fontsize=9.5, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.5", linewidth=1.0))

fig.suptitle("Jul-2025 Ex2b  —  99th percentile of Loans by AgeC",
             fontsize=13, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print("p99:", p99)
