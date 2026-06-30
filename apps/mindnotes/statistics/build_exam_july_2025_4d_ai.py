"""AI walkthrough for Jul-2025 Ex4d — Compare AgeC significance in mod1 vs mod2.

Shows why AgeCyoung loses significance once Tenure and Cards are included:
the supposed 'age' effect was actually a tenure/card-usage effect that
correlates with age. Side-by-side coefficient comparison and a confounding
diagram.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
import subprocess, tempfile, csv, collections

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2025_4d_ai.png"

# coefficient estimates (from R)
mod1 = {"AgeCsenior": (68.04, 0.03106), "AgeCyoung": (-71.25, 0.01326)}
mod2 = {"AgeCsenior": (73.79, 0.01556), "AgeCyoung": (-43.87, 0.11702)}

# Real data — show mean Tenure & Cards by AgeC to motivate the confounding
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/july 2025/Exam202507.RData"
tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(BankClients[, c("AgeC","Tenure","Cards")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
g = collections.defaultdict(lambda: {"Tenure": [], "Cards": []})
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        g[row["AgeC"]]["Tenure"].append(float(row["Tenure"]))
        g[row["AgeC"]]["Cards"].append(float(row["Cards"]))
os.unlink(tmp.name)
order = ["adult", "senior", "young"]
mean_tenure = [np.mean(g[k]["Tenure"]) for k in order]
mean_cards  = [np.mean(g[k]["Cards"])  for k in order]

fig, axes = plt.subplots(1, 2, figsize=(13, 5.5),
                         gridspec_kw={"width_ratios": [1.0, 1.0]})

# --- LEFT: coefficient comparison mod1 vs mod2 ---
ax = axes[0]
labels = list(mod1.keys())
x = np.arange(len(labels))
w = 0.36
m1 = [mod1[k][0] for k in labels]
m2 = [mod2[k][0] for k in labels]
ax.bar(x - w/2, m1, w, color=PALETTE["secondary"],
       edgecolor=PALETTE["primary"], linewidth=1.1, alpha=0.75,
       label="mod1 (no Cards/Tenure)")
ax.bar(x + w/2, m2, w, color=PALETTE["accent"],
       edgecolor=PALETTE["primary"], linewidth=1.1, alpha=0.75,
       label="mod2 (+ Cards + Tenure)")
ax.axhline(0, color=PALETTE["primary"], linewidth=1.0)
for xi, v in zip(x - w/2, m1):
    ax.text(xi, v + (3 if v >= 0 else -3), f"{v:+.2f}", ha="center",
            va="bottom" if v >= 0 else "top",
            fontsize=10.5, color=PALETTE["primary"], fontweight="bold")
for xi, v in zip(x + w/2, m2):
    ax.text(xi, v + (3 if v >= 0 else -3), f"{v:+.2f}", ha="center",
            va="bottom" if v >= 0 else "top",
            fontsize=10.5, color=PALETTE["primary"], fontweight="bold")
ax.set_xticks(x); ax.set_xticklabels(labels)
ax.set_ylabel("$\\hat\\beta$ on Investments (\u20ac)")
ax.set_title("Step 1 — how AgeC coefficients shift after adding Cards & Tenure\n"
             "young: magnitude shrinks 71.25 \u2192 43.87 (loses 5% significance)")
ax.legend(loc="upper right", framealpha=0.95)
ax.set_ylim(-100, 110)

# --- RIGHT: mean Tenure and Cards by AgeC (the confounders) ---
ax2 = axes[1]
xb = np.arange(len(order))
w2 = 0.36
ax2.bar(xb - w2/2, mean_tenure, w2, color=PALETTE["primary"], alpha=0.7,
        edgecolor=PALETTE["primary"], linewidth=1.0, label="mean Tenure (months)")
ax2.set_ylabel("mean Tenure", color=PALETTE["primary"])
ax2.set_xticks(xb); ax2.set_xticklabels(order)
ax2b = ax2.twinx()
ax2b.bar(xb + w2/2, mean_cards, w2, color=PALETTE["warn"], alpha=0.7,
         edgecolor=PALETTE["primary"], linewidth=1.0, label="mean Cards intensity")
ax2b.set_ylabel("mean Cards", color=PALETTE["warn"])
ax2b.grid(False)
for i, (t, c) in enumerate(zip(mean_tenure, mean_cards)):
    ax2.text(i - w2/2, t + 1, f"{t:.1f}", ha="center", fontsize=10,
             color=PALETTE["primary"], fontweight="bold")
    ax2b.text(i + w2/2, c + 0.12, f"{c:.2f}", ha="center", fontsize=10,
              color=PALETTE["warn"], fontweight="bold")
ax2.set_title("Step 2 — confounders by AgeC\n"
              "young clients have shorter Tenure and fewer Cards\n"
              "\u2192 explains away the raw 'young' effect on Investments")

# Explanation box
ax.text(0.02, 0.97,
        "Confounding explanation:\n"
        "young clients differ from adults in two ways:\n"
        "  - shorter banking history (Tenure)\n"
        "  - lower card usage (Cards)\n"
        "Both push Investments down,\n"
        "so the raw AgeCyoung slope captures\nthose effects when Tenure/Cards are absent.\n"
        "Once both are in (mod2), the 'pure' age\neffect on young clients is no longer\nsignificantly different from zero.",
        transform=ax.transAxes, ha="left", va="top",
        fontsize=9, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle("Jul-2025 Ex4d  —  Why does AgeCyoung lose significance from mod1 to mod2?",
             fontsize=12, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"mean Tenure: {mean_tenure}")
print(f"mean Cards : {mean_cards}")
