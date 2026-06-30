"""AI walkthrough for G1-2024 Ex3.a — CV: which is more dispersed, Read2 or Math2?"""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2024_ex3_a_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 1 2024/Data_General_202401.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Primary[, c("Read2","Math2")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
read2, math2 = [], []
with open(tmp.name) as f:
    for r in csv.DictReader(f):
        read2.append(float(r["Read2"])); math2.append(float(r["Math2"]))
os.unlink(tmp.name)
r = np.array(read2); m = np.array(math2)
mr, mm = r.mean(), m.mean()
sr, sm = r.std(ddof=1), m.std(ddof=1)
cvr, cvm = sr/mr, sm/mm

fig = plt.figure(figsize=(14.5, 6.5))
gs = fig.add_gridspec(2, 3, width_ratios=[1.1, 1.1, 1.0], height_ratios=[1, 0.0001])
ax_hist = fig.add_subplot(gs[:, 0:2])
ax_bar  = fig.add_subplot(gs[:, 2])

# Histograms
bins = np.linspace(min(r.min(), m.min())-5, max(r.max(), m.max())+5, 40)
ax_hist.hist(r, bins=bins, color=PALETTE["primary"], alpha=0.55,
             edgecolor="black", label=f"Read2  (mean = {mr:.2f}, sd = {sr:.2f})")
ax_hist.hist(m, bins=bins, color=PALETTE["warn"], alpha=0.55,
             edgecolor="black", label=f"Math2  (mean = {mm:.2f}, sd = {sm:.2f})")
ax_hist.axvline(mr, color=PALETTE["primary"], ls="--", lw=1.5)
ax_hist.axvline(mm, color=PALETTE["warn"], ls="--", lw=1.5)
ax_hist.set_xlabel("score")
ax_hist.set_ylabel("frequency")
ax_hist.set_title("Read2 vs Math2 — overlaid distributions")
ax_hist.legend(loc="upper right", framealpha=0.95, fontsize=10)

# Bar of CV
ax_bar.bar(["Read2", "Math2"], [cvr*100, cvm*100],
           color=[PALETTE["primary"], PALETTE["warn"]],
           alpha=0.85, edgecolor="black", width=0.55)
for i, v in enumerate([cvr, cvm]):
    ax_bar.text(i, v*100 + 0.08, f"{v*100:.2f}%", ha="center",
                fontsize=13, color=PALETTE["primary"], fontweight="bold")
winner = "Math2" if cvm > cvr else "Read2"
ax_bar.set_ylabel("CV  =  sd / mean   (%)")
ax_bar.set_ylim(0, max(cvr, cvm)*100 + 1.5)
ax_bar.set_title(f"Coefficient of variation\n{winner} is (slightly) more dispersed")

txt = (f"CV (Read2) = {sr:.3f} / {mr:.3f} = {cvr:.4f}\n"
       f"CV (Math2) = {sm:.3f} / {mm:.3f} = {cvm:.4f}\n\n"
       f"CV(Math2) > CV(Read2)\n=> Math2 is relatively more dispersed.")
ax_bar.text(0.02, 0.97, txt, transform=ax_bar.transAxes, ha="left", va="top",
            fontsize=10, color=PALETTE["primary"], family="monospace",
            bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                      boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle("G1-2024 Ex3.a — Which is more dispersed? Use the CV (scale-free)",
             fontsize=12.5, y=1.00, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (CV_read={cvr:.4f}, CV_math={cvm:.4f})")
