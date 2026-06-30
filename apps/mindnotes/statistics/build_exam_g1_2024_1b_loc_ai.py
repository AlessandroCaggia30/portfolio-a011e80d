"""AI walkthrough for G1-2024 Ex1.b — Conditional distribution SchoolLoc | Lunch."""
import os, sys, subprocess, tempfile, csv, collections
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE, PALETTE_CYCLE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2024_1b_loc_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 1 2024/Data_General_202401.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Primary[, c("SchoolLoc","Lunch")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
counts = collections.Counter()
with open(tmp.name) as f:
    for row in csv.DictReader(f):
        counts[(row["SchoolLoc"], row["Lunch"])] += 1
os.unlink(tmp.name)

locs = ["inner-city", "rural", "suburban", "urban"]
lunches = ["non-free", "free"]
mat = np.array([[counts[(l, lu)] for lu in lunches] for l in locs], dtype=float)
col_tot = mat.sum(axis=0)
cond = mat / col_tot   # P(loc | lunch)

fig, ax = plt.subplots(figsize=(11, 6))
x = np.arange(len(lunches))
width = 0.18
bar_colors = [PALETTE["primary"], PALETTE["accent"], PALETTE["secondary"], PALETTE["ok"]]
for i, loc in enumerate(locs):
    bars = ax.bar(x + (i - 1.5)*width, cond[i]*100, width=width,
                  color=bar_colors[i], edgecolor="black", label=loc, alpha=0.85)
    for j, b in enumerate(bars):
        h = b.get_height()
        ax.text(b.get_x() + b.get_width()/2, h + 0.6, f"{h:.1f}%",
                ha="center", fontsize=9.5, color=PALETTE["primary"])

# Highlight modes
modes = [locs[np.argmax(cond[:, j])] for j in range(2)]
for j, lu in enumerate(lunches):
    mode_idx = np.argmax(cond[:, j])
    ax.text(x[j], cond[mode_idx, j]*100 + 5, f"MODE: {modes[j]}",
            ha="center", fontsize=11, color=PALETTE["warn"], fontweight="bold")

ax.set_xticks(x); ax.set_xticklabels([f"{lu}\n(n = {int(c)})" for lu, c in zip(lunches, col_tot)])
ax.set_ylabel("conditional relative frequency  (%)")
ax.set_title("G1-2024 Ex1.b — Distribution of SchoolLoc conditional on Lunch\n"
             "(SchoolLoc is nominal -> use the MODE)")
ax.legend(title="SchoolLoc", loc="upper right", framealpha=0.95)
ax.set_ylim(0, 65)

txt = "Mode(SchoolLoc | non-free) = rural      (53.8%)\n" \
      "Mode(SchoolLoc | free)     = inner-city (44.6%)\n\n" \
      "Different modes  =>  the two conditional\n" \
      "distributions are different  =>  Lunch and\n" \
      "SchoolLoc look associated  (formal test in Ex 1.b3)."
ax.text(0.02, 0.97, txt, transform=ax.transAxes, ha="left", va="top",
        fontsize=10, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
