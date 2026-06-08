"""AI walkthrough plot for Ex 3.2e — loyal customers (History=High) by Age group.

Visual:
  LEFT  — grouped/stacked bar plot of the row-conditional distribution
          History | Age  (Young / Middle / Senior).  Each Age group is one
          bar split into None / Low / Medium / High.  The 'High' slice
          (loyal customers) is highlighted in warm yellow; the bar of the
          group with the highest loyal share (Senior, 40%) is annotated.
  RIGHT — the row-percentage table, a "reading" panel that turns the
          numbers into the answer, and the R command used.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex3/ex3_2e_ai.png"

# Row-conditional distribution of History | Age (from the answer text).
# Order: None, Low, Medium, High
ages = ["Young", "Middle", "Senior"]
rows = np.array([
    [45, 44,  9,  2],   # Young
    [25, 16, 23, 36],   # Middle
    [14, 17, 29, 40],   # Senior
], dtype=float)
cats = ["None", "Low", "Medium", "High"]
# Greys for None/Low/Medium, warm gold for High (loyal)
colors = [PALETTE["muted"], "#bcc4d0", "#7a86a3", "#f1b300"]

# ============================================================
fig, (ax1, ax2) = plt.subplots(
    1, 2, figsize=(13.8, 6.4),
    gridspec_kw={"width_ratios": [1.05, 1.0]},
)

# ----------------------------------------------------------------
# LEFT — stacked bar plot of History | Age, with High highlighted
# ----------------------------------------------------------------
x = np.arange(len(ages))
left = np.zeros(len(ages))
for j, (cat, col) in enumerate(zip(cats, colors)):
    vals = rows[:, j]
    bars = ax1.barh(
        x, vals, left=left, color=col, edgecolor="white", linewidth=1.0,
        label=cat,
    )
    # Number inside each segment, white text on the High slice
    for i, v in enumerate(vals):
        if v >= 6:
            tx = left[i] + v / 2
            txt_col = "white" if cat == "High" else PALETTE["primary"]
            weight = "bold" if cat == "High" else "normal"
            ax1.text(tx, x[i], f"{int(v)}%", ha="center", va="center",
                     fontsize=10.5, color=txt_col, fontweight=weight)
    left += vals

ax1.set_yticks(x)
ax1.set_yticklabels(ages, fontsize=11.5, fontweight="bold")
ax1.set_xlabel("% within Age group (row total = 100%)")
ax1.set_xlim(0, 100)
ax1.invert_yaxis()
ax1.set_title("Row-conditional distribution: History | Age", pad=12,
              fontweight="bold", color=PALETTE["primary"])
ax1.grid(axis="x", alpha=0.4)
ax1.legend(loc="lower center", bbox_to_anchor=(0.5, -0.28), ncol=4,
           frameon=False, fontsize=10.5)
ax1.set_ylim(2.7, -0.6)  # add headroom below Senior for the callout

# Highlight the "High" slice of the winning row (Senior).
senior_i = 2
high_start = rows[senior_i, :3].sum()
high_w = rows[senior_i, 3]
# Outline + arrow callout
ax1.add_patch(plt.Rectangle((high_start, senior_i - 0.42),
                            high_w, 0.84,
                            facecolor="none", edgecolor=PALETTE["warn"],
                            linewidth=2.4, zorder=5))
ax1.annotate(
    "loyal share = 40%\n(highest of the 3 age groups)",
    xy=(high_start + high_w / 2, senior_i + 0.42),
    xytext=(78, senior_i + 0.95),
    arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.4),
    ha="center", va="center", fontsize=10.5,
    color=PALETTE["warn"], fontweight="bold",
)

# Small comparison strip on the right showing only the High column
ax1.text(101, -0.55, "High %", fontsize=10, color=PALETTE["primary"],
         fontweight="bold")
for i, name in enumerate(ages):
    val = rows[i, 3]
    bar_w = val * 0.5
    ax1.add_patch(plt.Rectangle((101, i - 0.18), bar_w, 0.36,
                                facecolor=colors[3] if name == "Senior" else "#e7d9a3",
                                edgecolor=PALETTE["primary"], linewidth=0.6,
                                clip_on=False))
    ax1.text(101 + bar_w + 1.2, i, f"{int(val)}%", va="center",
             fontsize=10, color=PALETTE["primary"],
             fontweight="bold" if name == "Senior" else "normal",
             clip_on=False)

# ----------------------------------------------------------------
# RIGHT — table + reading + R command
# ----------------------------------------------------------------
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)

ax2.text(0.5, 0.985, "Reading the row-conditional table",
         ha="center", va="top", fontsize=12.5, fontweight="bold",
         color=PALETTE["primary"])

# Numeric table block (monospace) — the row percentages of History | Age
table_txt = (
    "                History_recode\n"
    "Age_recode   None  Low  Medium  High  TOTAL\n"
    "  Young        45   44     9     2    100\n"
    "  Middle       25   16    23    36    100\n"
    "  Senior       14   17    29    40    100"
)
ax2.text(0.04, 0.93, table_txt, fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.5", linewidth=1.0))

# Reading
ax2.text(0.02, 0.55, "Comparison of the 'High' column",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.50,
         "  Young   :  2%   (very few loyal)\n"
         "  Middle  : 36%\n"
         "  Senior  : 40%   <-- highest loyal share\n"
         "\n"
         "Loyalty grows monotonically with age:\n"
         "  2%  <  36%  <  40%.",
         fontsize=10.5, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.5", linewidth=1.0))

# Why ROW conditional (not column / not joint)?
ax2.text(0.02, 0.235, "Why row %, not column or joint?",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.19,
         "Question = 'which Age has the highest %\n"
         "of loyal customers?'  ->  condition on Age,\n"
         "i.e. fix Age (rows sum to 100%) and compare\n"
         "the 'High' column across rows.",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# R command
ax2.text(0.02, 0.02 + 0.0, "R command",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, -0.015,
         'distr.table.xy(x=Age_recode, y=History_recode,\n'
         '               freq.type="row", freq="perc", data=DS)',
         fontsize=10, family="monospace", va="top",
         color=PALETTE["warn"],
         bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
