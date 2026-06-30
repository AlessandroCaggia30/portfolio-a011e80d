"""AI walkthrough for Jul-2025 Ex4a — Intercept of mod1 = lm(Investments ~ Branch + AgeC).

Shows the all-categorical regression baseline cell. With default alpha-order
levels, the baseline is (Branch=A, AgeC=adult) and intercept b0 = 424.01.
After re-ordering AgeC with young as reference, intercept = 352.76 — the
average Investments for branch A young clients.

Two panels:
  Left  : Group-mean bar chart, branch x age, with intercept cell highlighted.
  Right : Coefficient bars from summary(mod1).
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
import subprocess, tempfile, csv

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2025_4a_ai.png"

RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/july 2025/Exam202507.RData"
tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(BankClients[, c("Investments","Branch","AgeC")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
import collections
cells = collections.defaultdict(list)
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        cells[(row["Branch"], row["AgeC"])].append(float(row["Investments"]))
os.unlink(tmp.name)

branches = ["A", "B"]
ages = ["adult", "senior", "young"]
mean_grid = np.array([[np.mean(cells[(b, a)]) for a in ages] for b in branches])
n_grid    = np.array([[len(cells[(b, a)]) for a in ages] for b in branches])

# Coefficients from summary(mod1) — default factor order alphabetical (adult is ref):
coefs = {
    "(Intercept)\n[A, adult]": 424.01,
    "BranchB": 78.48,
    "AgeCsenior": 68.04,
    "AgeCyoung": -71.25,
}

fig, axes = plt.subplots(1, 2, figsize=(13, 5.5),
                         gridspec_kw={"width_ratios": [1.15, 1.0]})

# --- LEFT: 2x3 group-mean bar chart ---
ax = axes[0]
x = np.arange(len(ages))
w = 0.36
colors_b = {"A": PALETTE["secondary"], "B": PALETTE["accent"]}
for i, b in enumerate(branches):
    offset = -w / 2 + i * w
    bars = ax.bar(x + offset, mean_grid[i], w,
                  color=colors_b[b], edgecolor=PALETTE["primary"], linewidth=1.1,
                  alpha=0.7, label=f"Branch {b}")
    for j, (m, n) in enumerate(zip(mean_grid[i], n_grid[i])):
        ax.text(x[j] + offset, m + 6, f"{m:.0f}", ha="center", va="bottom",
                fontsize=9, color=PALETTE["primary"])
# highlight baseline cell (A, adult)
ax.bar(x[0] - w / 2, mean_grid[0, 0], w,
       facecolor="none", edgecolor=PALETTE["warn"], linewidth=2.5,
       label="baseline cell  $\\hat\\beta_0$ (A, adult)")
ax.set_xticks(x); ax.set_xticklabels(ages)
ax.set_ylabel("mean Investments (\u20ac)")
ax.set_title("Step 1 — mean Investments per (Branch, AgeC) cell\n"
             "intercept = mean of the baseline = (A, adult)")
ax.set_ylim(0, max(mean_grid.flatten()) * 1.20)
ax.legend(loc="upper right", framealpha=0.95)

# --- RIGHT: coefficient bars ---
ax2 = axes[1]
names = list(coefs.keys())
vals = [coefs[k] for k in names]
colors_c = [PALETTE["primary"] if v >= 0 else PALETTE["warn"] for v in vals]
ypos = np.arange(len(names))
ax2.barh(ypos, vals, color=colors_c, alpha=0.7,
         edgecolor=PALETTE["primary"], linewidth=1.1)
for i, v in enumerate(vals):
    ha = "left" if v >= 0 else "right"
    dx = 6 if v >= 0 else -6
    ax2.text(v + dx, i, f"{v:+.2f}", ha=ha, va="center",
             fontsize=10.5, color=PALETTE["primary"], fontweight="bold")
ax2.set_yticks(ypos); ax2.set_yticklabels(names)
ax2.invert_yaxis()
ax2.axvline(0, color=PALETTE["primary"], linewidth=1.1)
ax2.set_xlabel("coefficient estimate")
ax2.set_title("Step 2 — summary(mod1)\n"
              "$\\hat\\beta_0 = 424.01$  (Branch=A, AgeC=adult)")
ax2.set_xlim(min(vals) - 80, max(vals) + 80)

ax2.text(0.98, 0.97,
         "Interpretation:\n"
         "Average Investments for a client\n"
         "in Branch A and AgeC = adult (baseline)\n"
         f"is $\\hat\\beta_0 \\approx 424.01$ \u20ac.\n"
         "Switching levels (young as ref)\n"
         "gives intercept $= 352.76$\n"
         "= mean Investments for Branch A, young.",
         transform=ax2.transAxes, ha="right", va="top",
         fontsize=9.5, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle("Jul-2025 Ex4a  —  intercept of mod1 = lm(Investments ~ Branch + AgeC)",
             fontsize=12, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  mean_grid (A,B) x (adult,senior,young):\n{mean_grid}")
