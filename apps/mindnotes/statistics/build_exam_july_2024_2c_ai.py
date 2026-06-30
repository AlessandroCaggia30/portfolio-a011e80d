"""AI walkthrough for Jul-2024 Ex2.c — Spearman vs Pearson side-by-side."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2024_2c_ai.png"
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

def rankdata(a):
    order = np.argsort(a)
    ranks = np.empty_like(order, dtype=float)
    ranks[order] = np.arange(1, len(a)+1)
    # handle ties by averaging
    sa = np.sort(a)
    i = 0
    while i < len(sa):
        j = i
        while j < len(sa) and sa[j] == sa[i]:
            j += 1
        if j - i > 1:
            avg = (i + j + 1) / 2
            mask = (a == sa[i])
            ranks[mask] = avg
        i = j
    return ranks

rx = rankdata(x); ry = rankdata(y)
r_p = np.corrcoef(x, y)[0, 1]
r_s = np.corrcoef(rx, ry)[0, 1]
sl_p, int_p = np.polyfit(x, y, 1)
sl_s, int_s = np.polyfit(rx, ry, 1)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.8))
ax1.scatter(x, y, s=24, alpha=0.55, color=PALETTE["secondary"],
            edgecolor=PALETTE["primary"], linewidth=0.4)
xx = np.linspace(x.min(), x.max(), 200)
ax1.plot(xx, int_p + sl_p*xx, color=PALETTE["warn"], lw=2.2)
ax1.set_xlabel("Top10"); ax1.set_ylabel("Phd")
ax1.set_title(f"Pearson on raw values\n$r$ = {r_p:.4f}")

ax2.scatter(rx, ry, s=24, alpha=0.55, color=PALETTE["accent"],
            edgecolor=PALETTE["primary"], linewidth=0.4)
xr = np.linspace(rx.min(), rx.max(), 200)
ax2.plot(xr, int_s + sl_s*xr, color=PALETTE["warn"], lw=2.2)
ax2.set_xlabel("rank(Top10)"); ax2.set_ylabel("rank(Phd)")
ax2.set_title(f"Spearman = Pearson on ranks\n$\\rho_S$ = {r_s:.4f}")

fig.text(0.5, -0.02,
         f"Pearson r = {r_p:.4f}    Spearman rho_S = {r_s:.4f}    Delta = {abs(r_p-r_s):.4f}\n"
         "=> virtually identical => association is monotone AND linear => Pearson is reliable",
         ha="center", fontsize=11.5, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))
fig.suptitle("Jul-2024 Ex2.c — Spearman as a robustness check on Pearson",
             fontsize=12.5, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (Pearson={r_p:.4f}, Spearman={r_s:.4f})")
