"""AI walkthrough for P1-2025 Ex1.b (id 1c) — Engagement outlier threshold: Tukey fence vs 95th percentile."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_p1_2025_1c_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/1st partial 2025/Data_PI1_20242210_2.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Metrics2[, c("Engagement")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
vals = []
with open(tmp.name) as f:
    rd = csv.reader(f); next(rd)
    for row in rd:
        vals.append(float(row[0]))
os.unlink(tmp.name)
x = np.array(vals)

q1, med, q3 = np.quantile(x, [0.25, 0.5, 0.75])
iqr = q3 - q1
fence = q3 + 1.5*iqr
p95 = np.quantile(x, 0.95)
p90 = np.quantile(x, 0.90)
n = len(x)
n_fence = int((x > fence).sum())
n_p95 = int((x > p95).sum())

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.5))

# Left: histogram / density with vertical rules for thresholds
bins = np.linspace(0, x.max(), 60)
ax1.hist(x, bins=bins, color=PALETTE["secondary"], alpha=0.55,
         edgecolor=PALETTE["primary"], linewidth=0.5, label=f"Engagement (n={n})")
ax1.axvline(fence, color=PALETTE["warn"], lw=2.2, ls="--",
            label=f"Tukey fence Q3+1.5*IQR = {fence:.2f}")
ax1.axvline(p95, color=PALETTE["accent"], lw=2.2, ls="-",
            label=f"95th percentile = {p95:.2f}")
ax1.axvspan(p95, x.max(), color=PALETTE["accent"], alpha=0.12)
ax1.set_xlabel("Engagement (followers)")
ax1.set_ylabel("Frequency")
ax1.set_title("Right-skewed Engagement — outlier thresholds")
ax1.legend(loc="upper right", framealpha=0.95)
ax1.text(0.98, 0.55,
         f"Q1 = {q1:.2f}\nQ3 = {q3:.2f}\nIQR = {iqr:.2f}\n"
         f"Tukey fence = {fence:.2f}  -> {n_fence} posts ({100*n_fence/n:.1f}%)\n"
         f"95th %ile   = {p95:.2f}  -> {n_p95} posts ({100*n_p95/n:.1f}%)",
         transform=ax1.transAxes, ha="right", va="top",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

# Right: boxplot with the same thresholds annotated
bp = ax2.boxplot(x, vert=True, widths=0.5, patch_artist=True,
                 medianprops=dict(color=PALETTE["primary"], linewidth=2.0),
                 flierprops=dict(marker="o", markerfacecolor=PALETTE["warn"],
                                 markeredgecolor=PALETTE["warn"], markersize=4, alpha=0.6))
bp["boxes"][0].set_facecolor(PALETTE["secondary"]); bp["boxes"][0].set_alpha(0.55)
ax2.axhline(fence, color=PALETTE["warn"], lw=2.0, ls="--",
            label=f"Tukey fence = {fence:.2f}")
ax2.axhline(p95, color=PALETTE["accent"], lw=2.0, ls="-",
            label=f"95th %ile = {p95:.2f}")
ax2.set_ylabel("Engagement")
ax2.set_title("Boxplot: many upper outliers -> use 95th %ile for 'anomalously good'")
ax2.legend(loc="upper right", framealpha=0.95)
ax2.set_xticks([]);

plt.suptitle("P1-2025 Ex1.b — Threshold for anomalously good Engagement", y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}   (Tukey={fence:.4f}, p95={p95:.4f})")
