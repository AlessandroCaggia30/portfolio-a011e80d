"""AI walkthrough for Jul-2025 Ex5a — sample proportion P(Cards > 5.5).

Histogram of Cards with the threshold 5.5 marked, plus the empirical
proportion stamped on top.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
import subprocess, tempfile, csv

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2025_5a_ai.png"

RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/july 2025/Exam202507.RData"
tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(BankClients[, "Cards"], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
vals = []
with open(tmp.name) as f:
    rd = csv.reader(f); next(rd)
    for row in rd: vals.append(float(row[0]))
os.unlink(tmp.name)
vals = np.array(vals)
thr = 5.5
p_hat = float(np.mean(vals > thr))
n = len(vals)
n_above = int(np.sum(vals > thr))

fig, axes = plt.subplots(1, 2, figsize=(13, 5.2),
                         gridspec_kw={"width_ratios": [1.4, 0.8]})

# --- LEFT: histogram with threshold shaded ---
ax = axes[0]
cap = float(np.quantile(vals, 0.995))
bins = np.linspace(0, cap, 40)
n_bins, edges, patches = ax.hist(vals[vals <= cap], bins=bins,
                                  edgecolor=PALETTE["primary"], linewidth=0.7,
                                  color=PALETTE["secondary"], alpha=0.55)
# colour bins above threshold differently
for p, e in zip(patches, edges[:-1]):
    if e >= thr:
        p.set_facecolor(PALETTE["warn"])
        p.set_alpha(0.55)
ax.axvline(thr, color=PALETTE["primary"], linestyle="--", linewidth=1.8,
           label=f"threshold = {thr}")
ax.set_xlabel("Cards (intensity of card usage)")
ax.set_ylabel("count")
ax.set_title("Step 1 — empirical distribution of Cards\n"
             f"shaded (red) = {{Cards > {thr}}}  \u2192  {n_above:,} clients / n = {n:,}")
ax.set_xlim(0, cap)
ax.legend(loc="upper right", framealpha=0.95)

# --- RIGHT: proportion bar ---
ax2 = axes[1]
ax2.bar([0, 1], [1 - p_hat, p_hat],
        color=[PALETTE["secondary"], PALETTE["warn"]],
        alpha=0.7, edgecolor=PALETTE["primary"], linewidth=1.2,
        tick_label=[f"Cards \u2264 {thr}", f"Cards > {thr}"])
for i, v in enumerate([1 - p_hat, p_hat]):
    ax2.text(i, v + 0.015, f"{v:.4f}\n({v*100:.2f}%)",
             ha="center", va="bottom", fontsize=11,
             color=PALETTE["primary"], fontweight="bold")
ax2.set_ylim(0, 1.0)
ax2.set_ylabel("sample proportion")
ax2.set_title(f"Step 2 — $\\hat p = \\frac{{n_{{>}}}}{{n}} = {n_above:,}/{n:,} = {p_hat:.4f}$")

ax2.text(0.5, 0.55, "R command:\nmean(BankClients$Cards > 5.5)\n"
                    f"## [1] {p_hat:.7f}",
         transform=ax2.transAxes, ha="center", va="center",
         fontsize=9.5, family="monospace",
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle("Jul-2025 Ex5a  —  sample proportion of clients with Cards > 5.5  "
             f"= {p_hat:.4f}",
             fontsize=12, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  n={n}, n_above={n_above}, p_hat={p_hat:.6f}")
