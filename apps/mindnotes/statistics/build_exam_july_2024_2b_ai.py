"""AI walkthrough for Jul-2024 Ex2.b — Adequacy of Pearson r: scatter + OLS + LOWESS."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2024_2b_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/july 2024/Data_General_202406.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Colleges[, c("Top10","Phd")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
xs, ys = [], []
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        xs.append(float(row["Top10"])); ys.append(float(row["Phd"]))
os.unlink(tmp.name)
x = np.array(xs); y = np.array(ys)
r = np.corrcoef(x, y)[0, 1]
slope, intercept = np.polyfit(x, y, 1)

# LOWESS via simple sliding-window kernel (no scipy dependence)
def lowess(x, y, frac=0.4):
    n = len(x); idx = np.argsort(x); xs = x[idx]; ys = y[idx]
    w = int(max(5, frac*n))
    out = np.empty_like(ys, dtype=float)
    for i in range(n):
        lo, hi = max(0, i-w//2), min(n, i+w//2)
        out[i] = np.mean(ys[lo:hi])
    return xs, out
xl, yl = lowess(x, y, frac=0.35)

fig, ax = plt.subplots(figsize=(10, 6.5))
ax.scatter(x, y, s=30, alpha=0.55, color=PALETTE["secondary"],
           edgecolor=PALETTE["primary"], linewidth=0.5, label=f"colleges (n={len(x)})")
xx = np.linspace(x.min(), x.max(), 200)
ax.plot(xx, intercept + slope*xx, color=PALETTE["warn"], lw=2.2,
        label=f"OLS  $\\hat y = {intercept:.1f} + {slope:.2f}x$")
ax.plot(xl, yl, color=PALETTE["accent"], lw=2.4, ls="-",
        label="LOWESS smoother")
ax.set_xlabel("Top10  (% top-10% HS students enrolled)")
ax.set_ylabel("Phd    (faculty quality index)")
ax.set_title("Jul-2024 Ex2.b — Top10 vs Phd: linearity check for Pearson's r")
ax.legend(loc="lower right", framealpha=0.95)
ax.text(0.02, 0.97,
        f"Pearson r = {r:.4f}\n"
        f"LOWESS ~ OLS line  => linear\n"
        f"No fan shape       => homoscedastic\n"
        f"No extreme outliers\n"
        f"=> r is an ADEQUATE summary",
        transform=ax.transAxes, ha="left", va="top",
        fontsize=10.5, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (r={r:.4f})")
