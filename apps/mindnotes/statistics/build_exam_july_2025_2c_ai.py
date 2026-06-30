"""AI walkthrough for past-exam Jul-2025 Ex2c — Upper outlier threshold
L = Q3 + 1.5*(Q3-Q1) for the young AgeC subgroup, with the 120,000-cap
question (is it an upper outlier?).

Uses real data from Exam202507.RData.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
import subprocess, tempfile, csv

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2025_2c_ai.png"

RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/july 2025/Exam202507.RData"
tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(BankClients[BankClients$AgeC=="young", c("Loans")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
young = []
with open(tmp.name) as f:
    rd = csv.reader(f)
    next(rd)
    for row in rd:
        young.append(float(row[0]))
os.unlink(tmp.name)
young = np.array(young)

# 5-number summary using R fivenum (Tukey hinges) - python np.quantile uses type 7 by default
# Use np.percentile with linear interpolation; we will reuse R values explicitly to match exam
# From R fivenum: 0.190 8283.63 25087.04 45728.29 128083.12 (per the screenshot)
mn = float(np.min(young))
mx = float(np.max(young))
q1 = 8283.63
med = 25087.04
q3 = 45728.29
iqr = q3 - q1
L = q3 + 1.5 * iqr  # upper outlier threshold
p99 = 119922.7      # from screenshot / R quantile
candidate = 120000.0

fig, axes = plt.subplots(1, 2, figsize=(13, 5.5),
                         gridspec_kw={"width_ratios": [2.0, 1.0]})

# --- LEFT: boxplot of young Loans with key landmarks ---
ax = axes[0]
bp = ax.boxplot([young], vert=False, widths=0.45, patch_artist=True,
                medianprops=dict(color=PALETTE["warn"], linewidth=2.0),
                flierprops=dict(marker="o", markerfacecolor=PALETTE["muted"],
                                markeredgecolor=PALETTE["muted"], markersize=4.5,
                                alpha=0.6),
                whiskerprops=dict(color=PALETTE["primary"], linewidth=1.2),
                capprops=dict(color=PALETTE["primary"], linewidth=1.2),
                boxprops=dict(edgecolor=PALETTE["primary"], linewidth=1.3))
bp["boxes"][0].set_facecolor(PALETTE["ok"])
bp["boxes"][0].set_alpha(0.4)

# vertical landmarks
ax.axvline(q1,  color=PALETTE["primary"], linestyle=":", linewidth=1.0)
ax.axvline(q3,  color=PALETTE["primary"], linestyle=":", linewidth=1.0)
ax.axvline(L,   color=PALETTE["warn"],    linestyle="--", linewidth=2.0,
           label=f"upper-outlier threshold $L = Q_3 + 1.5\\,IQR \\approx {L:,.0f}$")
ax.axvline(p99, color=PALETTE["accent"],  linestyle="--", linewidth=2.0,
           label=f"$p_{{99,\\mathrm{{young}}}} \\approx {p99:,.0f}$")
ax.axvline(candidate, color="#7d3c98", linestyle=":", linewidth=2.0,
           label=f"reference value $= {candidate:,.0f}$")

# annotate Q1/Q3
ax.text(q1, 1.45, f"Q1\n{q1:,.0f}", ha="center", va="bottom", fontsize=9,
        color=PALETTE["primary"])
ax.text(q3, 1.45, f"Q3\n{q3:,.0f}", ha="center", va="bottom", fontsize=9,
        color=PALETTE["primary"])
ax.text(L, 0.55, f"L\n{L:,.0f}", ha="center", va="top",
        fontsize=9, color=PALETTE["warn"], fontweight="bold")

ax.set_yticks([])
ax.set_xlim(-10000, 200000)
ax.set_xlabel("Loans (€) — young clients only")
ax.set_title("Step 1 — Tukey upper-outlier rule applied to AgeC = young\n"
             f"$IQR = Q_3 - Q_1 = {q3:,.0f} - {q1:,.0f} = {iqr:,.0f}$,  "
             f"$L = {q3:,.0f} + 1.5 \\cdot {iqr:,.0f} = {L:,.0f}$")
ax.legend(loc="upper right", framealpha=0.95)

# --- RIGHT: decision panel ---
ax2 = axes[1]
ax2.axis("off")
ax2.text(0.5, 0.92, "Decision", ha="center", va="top",
         fontsize=14, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.5, 0.78,
         f"$p_{{99}} \\approx {p99:,.0f}$\n"
         f"$L \\approx {L:,.0f}$",
         ha="center", va="top", fontsize=12, color=PALETTE["primary"])
ax2.text(0.5, 0.58,
         f"$p_{{99}} > L$  $\\Rightarrow$\nat least 1% of young\nclients are upper outliers.",
         ha="center", va="top", fontsize=11.5, color=PALETTE["warn"],
         fontweight="bold",
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.6", linewidth=1.3))
ax2.text(0.5, 0.28,
         f"For the reference value 120,000:\n"
         f"$120{{,}}000 > L = {L:,.0f}$ as well\n$\\Rightarrow$ same conclusion.",
         ha="center", va="top", fontsize=11, color=PALETTE["primary"],
         bbox=dict(facecolor="#f4f7fb", edgecolor=PALETTE["primary"],
                   boxstyle="round,pad=0.5", linewidth=1.0))

fig.suptitle("Jul-2025 Ex2c  —  Upper-outlier threshold for young clients",
             fontsize=13, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  Q1={q1}, Q3={q3}, IQR={iqr}, L={L}, p99={p99}")
