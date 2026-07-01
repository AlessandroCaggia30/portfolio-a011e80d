"""AI walkthrough for P1-2025 Ex1.d (id 1e) — Shares by Content: row-proportion bars, offers vs nobrand."""
import os, sys, subprocess, tempfile, csv, collections
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE, PALETTE_CYCLE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_p1_2025_1e_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/1st partial 2025/Data_PI1_20242210_2.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Metrics2[, c("Content","Shares")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
counts = collections.defaultdict(lambda: collections.Counter())
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        counts[row["Content"]][row["Shares"]] += 1
os.unlink(tmp.name)

content_order = ["offers", "brand", "nobrand"]
share_order   = ["verylow", "low", "high", "veryhigh"]
props = {c: [counts[c][s] / sum(counts[c].values()) for s in share_order] for c in content_order}
success = {c: props[c][share_order.index("high")] + props[c][share_order.index("veryhigh")]
           for c in content_order}

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.5), gridspec_kw=dict(width_ratios=[1.35, 1]))

# Left: stacked row-proportion bars for all 3 Content levels
colors = [PALETTE_CYCLE[0], PALETTE_CYCLE[1], PALETTE_CYCLE[3], PALETTE_CYCLE[5]]
bottoms = np.zeros(len(content_order))
for i, s in enumerate(share_order):
    heights = np.array([props[c][i] for c in content_order])
    ax1.bar(content_order, heights, bottom=bottoms, color=colors[i], alpha=0.85,
            edgecolor="white", linewidth=1.2, label=s)
    for j, h in enumerate(heights):
        if h >= 0.05:
            ax1.text(j, bottoms[j] + h/2, f"{h:.2f}", ha="center", va="center",
                     fontsize=9.5, color="white", fontweight="bold")
    bottoms += heights
ax1.set_ylabel("Proportion within Content group")
ax1.set_ylim(0, 1.02)
ax1.set_title("Row-proportion table: Shares | Content")
ax1.legend(loc="upper right", framealpha=0.95, title="Shares", fontsize=9)

# Right: success (high + veryhigh) bar, offers vs nobrand highlighted
ok_colors = [PALETTE["warn"] if c == "offers"
             else PALETTE["ok"] if c == "nobrand"
             else PALETTE["neutral"]
             for c in content_order]
ax2.bar(content_order, [success[c] for c in content_order], color=ok_colors, alpha=0.85,
        edgecolor=PALETTE["primary"], linewidth=1.0)
for i, c in enumerate(content_order):
    ax2.text(i, success[c] + 0.015, f"{success[c]:.2f}", ha="center", va="bottom",
             fontsize=11, color=PALETTE["primary"], fontweight="bold")
ax2.set_ylim(0, 0.85)
ax2.set_ylabel("P(Shares in {high, veryhigh} | Content)")
ax2.set_title("Success rate: nobrand (0.67) > offers (0.49)  ->  sentence FALSE")
ax2.text(0.98, 0.55,
         "offers  : 0.11 + 0.38 = 0.49\n"
         "brand   : 0.52 + 0.13 = 0.64\n"
         "nobrand : 0.51 + 0.16 = 0.67",
         transform=ax2.transAxes, ha="right", va="top",
         fontsize=10.5, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

plt.suptitle("P1-2025 Ex1.d — Success rate of posts by Content type", y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}   (offers={success['offers']:.3f}, nobrand={success['nobrand']:.3f})")
