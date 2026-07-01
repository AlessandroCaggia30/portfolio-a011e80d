"""AI walkthrough for P1-2025 Ex1.c (id 1d) — Reach vs Engagement scatter + Pearson r + OLS."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_p1_2025_1d_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/1st partial 2025/Data_PI1_20242210_2.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Metrics2[, c("Reach","Engagement")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
xs, ys = [], []
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        xs.append(float(row["Reach"])); ys.append(float(row["Engagement"]))
os.unlink(tmp.name)
x = np.array(xs); y = np.array(ys)
r = np.corrcoef(x, y)[0, 1]
slope, intercept = np.polyfit(x, y, 1)

fig, ax = plt.subplots(figsize=(10, 6.2))
ax.scatter(x, y, s=28, alpha=0.55, color=PALETTE["secondary"],
           edgecolor=PALETTE["primary"], linewidth=0.4,
           label=f"posts (n={len(x)})")
xx = np.linspace(x.min(), x.max(), 200)
ax.plot(xx, intercept + slope*xx, color=PALETTE["warn"], lw=2.2,
        label=f"OLS  y = {intercept:.2f} + {slope:.4f}*x")
ax.set_xlabel("Reach  (followers reached, in hundreds)")
ax.set_ylabel("Engagement  (number of followers)")
ax.set_title("P1-2025 Ex1.c — Reach vs Engagement: strong positive linear trend")
ax.legend(loc="upper left", framealpha=0.95)
ax.text(0.98, 0.05,
        f"Pearson r = {r:.4f}\n"
        f"=> strong positive linear\n"
        f"   association, but wide cloud\n"
        f"   (residual scatter is large)",
        transform=ax.transAxes, ha="right", va="bottom",
        fontsize=10.5, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}   (r={r:.4f})")
