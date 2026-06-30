"""AI walkthrough for Jul-2024 Ex1.c — Distribution of Outstate (boxplot + histogram)."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2024_1c_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/july 2024/Data_General_202406.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Colleges[, c("Outstate")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
vals = []
with open(tmp.name) as f:
    rd = csv.reader(f); next(rd)
    for row in rd: vals.append(float(row[0]))
os.unlink(tmp.name)
x = np.array(vals)
q1, med, q3 = np.percentile(x, [25, 50, 75])
mean, sd = x.mean(), x.std(ddof=1)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.5),
                               gridspec_kw={"width_ratios":[0.8, 1.4]})

# Left: boxplot
bp = ax1.boxplot(x, vert=True, patch_artist=True, widths=0.5,
                 medianprops=dict(color=PALETTE["primary"], linewidth=2.2),
                 flierprops=dict(marker="o", markerfacecolor=PALETTE["warn"],
                                 markeredgecolor=PALETTE["warn"], markersize=5, alpha=0.7))
for patch in bp["boxes"]:
    patch.set_facecolor(PALETTE["secondary"]); patch.set_alpha(0.55); patch.set_edgecolor(PALETTE["primary"])
ax1.scatter([1], [mean], marker="D", color=PALETTE["warn"], s=70, zorder=5,
            label=f"mean = {mean:.1f}")
ax1.axhline(med, ls="--", color=PALETTE["primary"], lw=1.0, alpha=0.5)
ax1.set_xticks([1]); ax1.set_xticklabels(["Outstate"])
ax1.set_ylabel("Outstate (100 USD)")
ax1.set_title("Boxplot — Outstate")
ax1.text(1.30, q1, f"$Q_1$ = {q1:.1f}", va="center", fontsize=10, color=PALETTE["primary"])
ax1.text(1.30, med, f"$Q_2$ = {med:.1f}", va="center", fontsize=10, color=PALETTE["primary"], fontweight="bold")
ax1.text(1.30, q3, f"$Q_3$ = {q3:.1f}", va="center", fontsize=10, color=PALETTE["primary"])
ax1.legend(loc="upper right", framealpha=0.95)

# Right: histogram
ax2.hist(x, bins=30, color=PALETTE["accent"], alpha=0.7,
         edgecolor=PALETTE["primary"], linewidth=0.6)
ax2.axvline(mean, color=PALETTE["warn"], lw=2.0, label=f"mean = {mean:.1f}")
ax2.axvline(med,  color=PALETTE["primary"], lw=2.0, ls="--", label=f"median = {med:.1f}")
ax2.axvline(q1,   color=PALETTE["neutral"], lw=1.2, ls=":", label=f"$Q_1$ = {q1:.1f}")
ax2.axvline(q3,   color=PALETTE["neutral"], lw=1.2, ls=":", label=f"$Q_3$ = {q3:.1f}")
ax2.set_xlabel("Outstate (100 USD)")
ax2.set_ylabel("frequency")
ax2.set_title(f"Histogram — Outstate  (n={len(x)},  SD={sd:.1f})")
ax2.legend(loc="upper right", framealpha=0.95, fontsize=9.5)
ax2.text(0.02, 0.97,
         f"mean ~ median => roughly symmetric\n"
         f"IQR = {q3-q1:.1f}  (~6280 USD)\n"
         f"SD  = {sd:.1f}  (~4160 USD)\n"
         f"no extreme outliers",
         transform=ax2.transAxes, ha="left", va="top",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))
fig.suptitle("Jul-2024 Ex1.c — Distribution of Outstate (centre, spread, quartiles)",
             fontsize=12.5, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
