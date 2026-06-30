"""AI walkthrough for past-exam Jul-2025 Ex2a — boxplot of Loans by AgeC.

Uses real data from Exam202507.RData (BankClients). Builds a side-by-side
boxplot of Loans for the three age groups (adult / senior / young), with
medians, IQR boxes, whiskers and outlier markers all visible. A small
companion panel shows the three sample sizes.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
import subprocess, json, tempfile

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2025_2a_ai.png"

# --- load Loans/AgeC from the R file via a one-shot Rscript ---
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/july 2025/Exam202507.RData"
tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False)
tmp.close()
subprocess.run(
    ["Rscript", "-e",
     f'load("{RDATA}"); write.csv(BankClients[,c("AgeC","Loans")], "{tmp.name}", row.names=FALSE)'],
    check=True, capture_output=True)
import csv
groups = {"adult": [], "senior": [], "young": []}
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        groups[row["AgeC"]].append(float(row["Loans"]))
os.unlink(tmp.name)

order = ["adult", "senior", "young"]
data = [np.array(groups[k]) for k in order]
ns = [len(d) for d in data]
medians = [float(np.median(d)) for d in data]
means = [float(np.mean(d)) for d in data]

fig, axes = plt.subplots(1, 2, figsize=(13, 5.5),
                         gridspec_kw={"width_ratios": [2.2, 1.0]})

# --- LEFT: boxplot Loans by AgeC ---
ax = axes[0]
bp = ax.boxplot(data, labels=order, patch_artist=True, widths=0.55,
                medianprops=dict(color=PALETTE["warn"], linewidth=2.0),
                flierprops=dict(marker="o", markerfacecolor=PALETTE["muted"],
                                markeredgecolor=PALETTE["muted"], markersize=4,
                                alpha=0.55),
                whiskerprops=dict(color=PALETTE["primary"], linewidth=1.2),
                capprops=dict(color=PALETTE["primary"], linewidth=1.2),
                boxprops=dict(edgecolor=PALETTE["primary"], linewidth=1.2))
fill_colors = [PALETTE["secondary"], PALETTE["accent"], PALETTE["ok"]]
for patch, c in zip(bp["boxes"], fill_colors):
    patch.set_facecolor(c)
    patch.set_alpha(0.45)

# overlay means as triangles
ax.scatter(range(1, 4), means, marker="^", s=60, zorder=5,
           color=PALETTE["primary"], edgecolor="white", linewidth=1.0,
           label="sample mean")
# annotate medians
for i, (m, n) in enumerate(zip(medians, ns), start=1):
    ax.annotate(f"med = {m:,.0f}", xy=(i, m), xytext=(i + 0.25, m + 4000),
                fontsize=9.5, color=PALETTE["warn"], fontweight="bold")

ax.set_ylabel("Loans (€)")
ax.set_xlabel("AgeC")
ax.set_title("Conditional boxplot of Loans by age group\n"
             "distr.plot.xy(AgeC, Loans, plot.type='boxplot', data=BankClients)")
ax.set_ylim(-5000, 220000)
ax.legend(loc="upper right", framealpha=0.95)
ax.grid(True, axis="y", alpha=0.5)

# --- RIGHT: group sample sizes ---
ax2 = axes[1]
ax2.bar(order, ns, color=fill_colors, alpha=0.55,
        edgecolor=PALETTE["primary"], linewidth=1.2)
for i, n in enumerate(ns):
    ax2.text(i, n + 12, f"n = {n}", ha="center", va="bottom",
             fontsize=11, color=PALETTE["primary"], fontweight="bold")
ax2.set_title("Group sample sizes (n)\n"
              f"total n = {sum(ns)}")
ax2.set_ylabel("count")
ax2.set_ylim(0, max(ns) * 1.18)
ax2.grid(True, axis="y", alpha=0.4)

# Conclusion box
ax.text(0.02, 0.97,
        "Key features:\n"
        f"  - medians:  adult={medians[0]:,.0f},  senior={medians[1]:,.0f},  young={medians[2]:,.0f}\n"
        f"  - senior has the LOWEST loans (skewed downward)\n"
        f"  - young & adult have similar central tendency,\n"
        f"    but young has the widest spread (largest IQR)\n"
        f"  - all three distributions are RIGHT-skewed with upper outliers",
        transform=ax.transAxes, ha="left", va="top",
        fontsize=9.5, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.5", linewidth=1.0))

fig.suptitle("Jul-2025 Ex2a  —  Loans by AgeC (boxplot)",
             fontsize=13, y=1.02, color=PALETTE["primary"])

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
for k, d in zip(order, data):
    print(f"  {k}: n={len(d)}, median={np.median(d):.2f}, mean={np.mean(d):.2f}, max={d.max():.2f}")
