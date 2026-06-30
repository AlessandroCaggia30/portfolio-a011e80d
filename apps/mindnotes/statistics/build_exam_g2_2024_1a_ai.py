"""AI walkthrough for G2-2024 Ex1.a — Boxplots of CrimeProperty by Region."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE, PALETTE_CYCLE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g2_2024_1a_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 2 2024/Data_General_202402.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
R_CMD = (
    f'load("{RDATA}"); '
    f'write.csv(data.frame(Region=as.character(CrimeUS$Region), '
    f'CrimeProperty=CrimeUS$CrimeProperty), "{tmp.name}", row.names=FALSE)'
)
subprocess.run(["Rscript", "-e", R_CMD], check=True, capture_output=True)

groups = {"NorthEast": [], "NorthCentre": [], "West": [], "South": []}
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        groups[row["Region"]].append(float(row["CrimeProperty"]))
os.unlink(tmp.name)

order = ["NorthEast", "NorthCentre", "West", "South"]
data = [groups[k] for k in order]
stats = {k: dict(n=len(v), mean=np.mean(v), sd=np.std(v, ddof=1),
                 med=np.median(v), q1=np.percentile(v, 25),
                 q3=np.percentile(v, 75)) for k, v in groups.items()}

fig, ax = plt.subplots(figsize=(11, 6.5))
bp = ax.boxplot(data, labels=order, vert=True, patch_artist=True,
                medianprops=dict(color=PALETTE["accent"], lw=2.2),
                flierprops=dict(marker="o", markersize=5,
                                markerfacecolor=PALETTE["warn"],
                                markeredgecolor=PALETTE["warn"], alpha=0.6))
colors = [PALETTE["secondary"], PALETTE_CYCLE[1], PALETTE["ok"], PALETTE["warn"]]
for patch, c in zip(bp["boxes"], colors):
    patch.set_facecolor(c); patch.set_alpha(0.45)
    patch.set_edgecolor(PALETTE["primary"])

# Annotate means with red diamonds
for i, k in enumerate(order, start=1):
    ax.scatter([i], [stats[k]["mean"]], marker="D", s=55,
               color=PALETTE["warn"], edgecolor=PALETTE["primary"],
               zorder=5, label="mean" if i == 1 else None)
    ax.text(i, stats[k]["mean"] + 8,
            f"$\\bar x$={stats[k]['mean']:.1f}",
            ha="center", va="bottom", fontsize=9, color=PALETTE["warn"])

ax.set_xlabel("Region")
ax.set_ylabel("CrimeProperty")
ax.set_title("G2-2024 Ex1.a — CrimeProperty by Region (n = 485 cities)")
ax.legend(loc="upper left", framealpha=0.95)

summary_txt = (
    "Region        n    mean     sd     med\n"
    + "\n".join([f"{k:<13s} {stats[k]['n']:<4d} {stats[k]['mean']:6.1f}  "
                 f"{stats[k]['sd']:5.1f}  {stats[k]['med']:6.1f}" for k in order])
    + "\n\nSouth: highest centre AND largest spread\nNorthEast: lowest and tightest"
)
ax.text(0.98, 0.97, summary_txt, transform=ax.transAxes,
        ha="right", va="top", fontsize=9.5, family="monospace",
        color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
