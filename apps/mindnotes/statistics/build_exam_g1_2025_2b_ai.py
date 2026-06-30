"""AI walkthrough for G1-2025 Ex2 — Coefficient-of-variation comparison
SleepQuality vs SleepDuration."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2025_2b_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 1 2025/Data_G_20250108.RData"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Sleep[, c("SleepQuality","SleepDuration")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
sq, sd = [], []
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        sq.append(float(row["SleepQuality"]))
        sd.append(float(row["SleepDuration"]))
os.unlink(tmp.name)
sq, sd = np.array(sq), np.array(sd)

m_q, s_q = sq.mean(), sq.std(ddof=1)
m_d, s_d = sd.mean(), sd.std(ddof=1)
cv_q, cv_d = s_q / m_q, s_d / m_d

fig = plt.figure(figsize=(12.5, 5.8))
gs = fig.add_gridspec(1, 3, width_ratios=[1.0, 1.0, 0.7])

# Left: standardized (divide by mean) histograms — same horizontal axis
ax1 = fig.add_subplot(gs[0, 0])
ax1.hist(sq / m_q, bins=22, color=PALETTE["secondary"], alpha=0.65,
         edgecolor=PALETTE["primary"], label=f"SleepQuality / mean (CV={cv_q:.3f})")
ax1.axvline(1.0, color=PALETTE["accent"], lw=2.0, ls="--", label="x / mean = 1")
ax1.set_xlabel("value / mean (unitless)")
ax1.set_ylabel("count")
ax1.set_title("SleepQuality — relative spread")
ax1.legend(framealpha=0.95, fontsize=10)
ax1.set_xlim(0.3, 1.7)

ax2 = fig.add_subplot(gs[0, 1])
ax2.hist(sd / m_d, bins=22, color=PALETTE["ok"], alpha=0.55,
         edgecolor=PALETTE["primary"], label=f"SleepDuration / mean (CV={cv_d:.3f})")
ax2.axvline(1.0, color=PALETTE["accent"], lw=2.0, ls="--", label="x / mean = 1")
ax2.set_xlabel("value / mean (unitless)")
ax2.set_title("SleepDuration — relative spread")
ax2.legend(framealpha=0.95, fontsize=10)
ax2.set_xlim(0.3, 1.7)

# Right: bar of CV values
ax3 = fig.add_subplot(gs[0, 2])
bars = ax3.bar(["SleepQuality", "SleepDuration"], [cv_q, cv_d],
               color=[PALETTE["secondary"], PALETTE["ok"]],
               edgecolor=PALETTE["primary"], linewidth=1.1, width=0.55)
for b, v in zip(bars, [cv_q, cv_d]):
    ax3.text(b.get_x() + b.get_width()/2, v + 0.006, f"{v:.3f}",
             ha="center", va="bottom", fontsize=11.5,
             color=PALETTE["primary"], fontweight="bold")
ax3.set_ylabel("CV = s / mean")
ax3.set_title("Coefficient of variation")
ax3.set_ylim(0, max(cv_q, cv_d) * 1.25)

ax3.text(0.5, -0.30,
         f"$\\bar X_Q = {m_q:.2f}$, $s_Q = {s_q:.2f}$\n"
         f"$\\bar X_D = {m_d:.2f}$, $s_D = {s_d:.2f}$\n"
         f"ratio CV(Q)/CV(D) $\\approx {cv_q/cv_d:.2f}$",
         transform=ax3.transAxes, ha="center", va="top",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

fig.suptitle("G1-2025 Ex2 — SleepQuality is ~1.8× more dispersed than SleepDuration (relative scale)",
             fontsize=13, color=PALETTE["primary"], y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"CV(Q)={cv_q:.4f}  CV(D)={cv_d:.4f}  ratio={cv_q/cv_d:.3f}")
