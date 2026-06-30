"""AI walkthrough for Jul-2024 Ex1.b — Conditional boxplot of Enrol by Region."""
import os, sys, subprocess, tempfile, csv, collections
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE, PALETTE_CYCLE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2024_1b_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/july 2024/Data_General_202406.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Colleges[, c("Enrol","Region")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
groups = collections.defaultdict(list)
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        groups[row["Region"]].append(float(row["Enrol"]))
os.unlink(tmp.name)

order = ["Northeast", "Midwest", "South", "West"]
data  = [groups[r] for r in order]
ns    = [len(d)    for d in data]
meds  = [np.median(d) for d in data]

fig, ax = plt.subplots(figsize=(10, 6))
bp = ax.boxplot(data, labels=[f"{r}\n(n={n})" for r,n in zip(order, ns)],
                patch_artist=True, widths=0.55,
                medianprops=dict(color=PALETTE["primary"], linewidth=2.0),
                flierprops=dict(marker="o", markerfacecolor=PALETTE["warn"],
                                markeredgecolor=PALETTE["warn"], markersize=5, alpha=0.7))
colors = [PALETTE["secondary"], PALETTE["accent"], PALETTE["ok"], PALETTE_CYCLE[5]]
for patch, c in zip(bp["boxes"], colors):
    patch.set_facecolor(c); patch.set_alpha(0.55); patch.set_edgecolor(PALETTE["primary"])
for i, m in enumerate(meds, start=1):
    ax.text(i, m + 1.5, f"med={m:.1f}", ha="center", va="bottom",
            fontsize=9.5, color=PALETTE["primary"], fontweight="bold")
ax.axhline(np.mean(np.concatenate(data)), ls="--", color=PALETTE["neutral"],
           linewidth=1.1, label=f"overall mean = {np.mean(np.concatenate(data)):.1f}")
ax.set_ylabel("Enrol  (% of accepted who enrol)")
ax.set_title("Jul-2024 Ex1.b — Distribution of Enrol conditional on Region")
ax.legend(loc="lower right", framealpha=0.95)
ax.text(0.02, 0.97,
        "Reading:\n"
        "- Northeast median  ~37  (lowest)\n"
        "- Midwest / South   ~48  (highest)\n"
        "- West              ~45  (widest spread, small n=42)\n"
        "=> Region matters for Enrol.",
        transform=ax.transAxes, ha="left", va="top",
        fontsize=10, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.4", linewidth=1.0))
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
