"""AI walkthrough for Jul-2025 Ex4c — Significance of AgeC dummies (5% level)
in mod1 vs mod2. Two panels:
  Left  : p-values of the AgeC dummies in each model (bar plot vs alpha=0.05).
  Right : decision matrix per dummy & model.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2025_4c_ai.png"

# p-values from R summary outputs
# mod1 (Investments ~ Branch + AgeC):
#   AgeCsenior 0.03106, AgeCyoung 0.01326
# mod2 (Investments ~ Branch + AgeC + Cards + Tenure):
#   AgeCsenior 0.01556, AgeCyoung 0.11702
p_mod1 = {"AgeCsenior": 0.03106, "AgeCyoung": 0.01326}
p_mod2 = {"AgeCsenior": 0.01556, "AgeCyoung": 0.11702}
alpha = 0.05

fig, axes = plt.subplots(1, 2, figsize=(13, 5.3),
                         gridspec_kw={"width_ratios": [1.1, 1.0]})

ax = axes[0]
labels = ["AgeCsenior", "AgeCyoung"]
x = np.arange(len(labels))
w = 0.36
m1 = [p_mod1[k] for k in labels]
m2 = [p_mod2[k] for k in labels]
bars1 = ax.bar(x - w/2, m1, w, color=PALETTE["secondary"],
               edgecolor=PALETTE["primary"], linewidth=1.1, alpha=0.75,
               label="mod1 (Branch + AgeC)")
bars2 = ax.bar(x + w/2, m2, w, color=PALETTE["accent"],
               edgecolor=PALETTE["primary"], linewidth=1.1, alpha=0.75,
               label="mod2 (Branch + AgeC + Cards + Tenure)")
ax.axhline(alpha, color=PALETTE["warn"], linestyle="--", linewidth=1.6,
           label=f"$\\alpha = {alpha}$")
# annotate
for xi, v in zip(x - w/2, m1):
    ax.text(xi, v + 0.003, f"{v:.4f}", ha="center", fontsize=9.5,
            color=PALETTE["primary"])
for xi, v in zip(x + w/2, m2):
    ax.text(xi, v + 0.003, f"{v:.4f}", ha="center", fontsize=9.5,
            color=PALETTE["primary"])
# mark "reject"/"fail" beneath bars
for xi, v in zip(x - w/2, m1):
    txt = "reject" if v < alpha else "fail"
    col = PALETTE["ok"] if v < alpha else PALETTE["warn"]
    ax.text(xi, -0.01, txt, ha="center", va="top",
            fontsize=9, color=col, fontweight="bold")
for xi, v in zip(x + w/2, m2):
    txt = "reject" if v < alpha else "fail"
    col = PALETTE["ok"] if v < alpha else PALETTE["warn"]
    ax.text(xi, -0.01, txt, ha="center", va="top",
            fontsize=9, color=col, fontweight="bold")
ax.set_xticks(x); ax.set_xticklabels(labels)
ax.set_ylabel("p-value")
ax.set_title(f"Step 1 — p-values vs $\\alpha = {alpha}$ for AgeC dummies\n"
             "decision rule: reject $H_0: \\beta_{AgeC*} = 0$ iff $p < \\alpha$")
ax.set_ylim(-0.03, 0.16)
ax.legend(loc="upper right", framealpha=0.95)

# Right: decision summary table
ax2 = axes[1]
ax2.axis("off")
table_data = [
    ["",            "mod1",      "mod2"],
    ["AgeCsenior",  "p=0.0311\nREJECT", "p=0.0156\nREJECT"],
    ["AgeCyoung",   "p=0.0133\nREJECT", "p=0.1170\nFAIL"],
]
cell_colors = [
    ["#f4f7fb", "#f4f7fb", "#f4f7fb"],
    ["#ffffff", "#d9e8d9", "#d9e8d9"],
    ["#ffffff", "#d9e8d9", "#fde2dd"],
]
tbl = ax2.table(cellText=table_data,
                cellColours=cell_colors,
                cellLoc="center",
                loc="upper center",
                colWidths=[0.30, 0.32, 0.32])
tbl.auto_set_font_size(False)
tbl.set_fontsize(11)
tbl.scale(1.0, 2.6)
for (r, c), cell in tbl.get_celld().items():
    cell.set_edgecolor(PALETTE["primary"])
    cell.set_linewidth(1.0)
    if r == 0 or c == 0:
        cell.set_text_props(weight="bold", color=PALETTE["primary"])

ax2.text(0.5, 0.27,
         "Interpretation:\n"
         "In mod1 both AgeC dummies are\nsignificant at 5%.\n"
         "In mod2, after adding Cards & Tenure,\nonly AgeCsenior remains significant —\n"
         "the gap between adult and young\nis explained away by tenure / cards.",
         transform=ax2.transAxes, ha="center", va="top",
         fontsize=10.5, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.5", linewidth=1.0))

fig.suptitle("Jul-2025 Ex4c — Effect of client age group (AgeC) at the 5% level",
             fontsize=12, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
