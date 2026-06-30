"""AI walkthrough for G1-2025 Ex1c — Percentiles of SleepQuality (tail reading)."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2025_1c_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 1 2025/Data_G_20250108.RData"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Sleep[, "SleepQuality", drop=FALSE], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
sq = []
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        sq.append(float(row["SleepQuality"]))
os.unlink(tmp.name)
sq = np.array(sq)
n = len(sq)

probs = [0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99]
qs = np.quantile(sq, probs)
labels = ["1%", "5%", "10%", "25%", "50%", "75%", "90%", "95%", "99%"]

# Two-panel: histogram with percentile cuts + ECDF with the same cuts
fig = plt.figure(figsize=(12.5, 5.6))
gs = fig.add_gridspec(1, 2, width_ratios=[1.1, 1.0])

# Left: histogram + percentile vertical lines
ax1 = fig.add_subplot(gs[0, 0])
ax1.hist(sq, bins=20, color=PALETTE["secondary"], alpha=0.55,
         edgecolor=PALETTE["primary"], linewidth=1.0)
ymax = ax1.get_ylim()[1]
for q, lab in zip(qs, labels):
    is_tail = lab in ("1%", "5%", "95%", "99%")
    is_med = lab == "50%"
    c = PALETTE["warn"] if is_tail else (PALETTE["accent"] if is_med else PALETTE["neutral"])
    lw = 2.0 if (is_tail or is_med) else 1.2
    ls = "-" if (is_tail or is_med) else "--"
    ax1.axvline(q, color=c, lw=lw, ls=ls, alpha=0.9)
    ax1.text(q, ymax * (0.96 if is_med else (0.85 if is_tail else 0.72)),
             f" {lab}\n {q:.2f}", color=c, fontsize=8.5,
             ha="left", va="top", fontweight="bold" if (is_tail or is_med) else "normal")
ax1.set_xlabel("SleepQuality")
ax1.set_ylabel("count")
ax1.set_title("Histogram with percentile cuts (red = tails, yellow = median)")

# Right: ECDF + horizontal probability cuts
ax2 = fig.add_subplot(gs[0, 1])
xs = np.sort(sq)
ys = np.arange(1, n + 1) / n
ax2.step(xs, ys, where="post", color=PALETTE["primary"], lw=2.0,
         label="Empirical CDF")
for p, q, lab in zip(probs, qs, labels):
    is_tail = lab in ("1%", "5%", "95%", "99%")
    c = PALETTE["warn"] if is_tail else PALETTE["muted"]
    ax2.hlines(p, xs.min() - 0.4, q, colors=c, linestyles=":", linewidth=1.1, alpha=0.8)
    ax2.vlines(q, 0, p, colors=c, linestyles=":", linewidth=1.1, alpha=0.8)
    ax2.plot(q, p, "o", color=c, markersize=5)
    ax2.text(q, p, f"  {lab}: q={q:.2f}", color=c, fontsize=8.5,
             ha="left", va="bottom")
ax2.set_xlabel("SleepQuality")
ax2.set_ylabel("F(x) = P(SleepQuality ≤ x)")
ax2.set_title("ECDF — reading percentiles off the curve")
ax2.set_ylim(0, 1.02)
ax2.legend(loc="lower right", framealpha=0.95)

fig.suptitle("G1-2025 Ex1c — SleepQuality percentiles: light tails, mild left skew",
             fontsize=13, color=PALETTE["primary"], y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print("percentiles:", dict(zip(labels, [round(float(q), 3) for q in qs])))
