"""AI walkthrough plot for Ex 3.2g — Young share among home owners vs renters.

Visual:
  LEFT  — stacked horizontal bars of Age_recode | OwnHome (Young / Middle /
          Senior).  Each OwnHome category sums to 100%.  The 'Young' slice
          (the one the question is about) is highlighted in warm gold and
          its width annotated.  A side strip on the right shows just the
          Young column for the two groups (8% vs 51%) so the gap is
          immediately visible.
  RIGHT — the row-percentage table, a short 'reading' block that turns the
          numbers into the answer, a short 'why row %?' block, and the R
          command that produced the table.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex3/ex3_2g_ai.png"

# Row-conditional distribution of Age_recode | OwnHome (from the answer text).
# Order: Young, Middle, Senior
groups = ["Own", "Rent"]
rows = np.array([
    [ 8, 66, 27],   # Own
    [51, 38, 11],   # Rent
], dtype=float)
cats = ["Young", "Middle", "Senior"]
# Warm gold for Young (highlighted), greys for Middle / Senior
colors = ["#f1b300", "#7a86a3", "#bcc4d0"]

fig, (ax1, ax2) = plt.subplots(
    1, 2, figsize=(15.0, 7.2),
    gridspec_kw={"width_ratios": [1.15, 1.0]},
)

# ---------------------------------------------------------------------
# LEFT — stacked bars Age | OwnHome, Young slice highlighted
# ---------------------------------------------------------------------
y = np.arange(len(groups))
left = np.zeros(len(groups))
for j, (cat, col) in enumerate(zip(cats, colors)):
    vals = rows[:, j]
    ax1.barh(y, vals, left=left, color=col, edgecolor="white", linewidth=1.0,
             label=cat)
    for i, v in enumerate(vals):
        if v >= 6:
            tx = left[i] + v / 2
            txt_col = "white" if cat == "Young" else PALETTE["primary"]
            weight = "bold" if cat == "Young" else "normal"
            ax1.text(tx, y[i], f"{int(v)}%", ha="center", va="center",
                     fontsize=11, color=txt_col, fontweight=weight)
    left += vals

ax1.set_yticks(y)
ax1.set_yticklabels(groups, fontsize=12, fontweight="bold")
ax1.set_xlabel("% within OwnHome group (row total = 100%)")
ax1.set_xlim(0, 100)
ax1.invert_yaxis()
ax1.set_title("Row-conditional distribution: Age | OwnHome", pad=12,
              fontweight="bold", color=PALETTE["primary"])
ax1.grid(axis="x", alpha=0.4)
ax1.legend(loc="lower center", bbox_to_anchor=(0.5, -0.22), ncol=3,
           frameon=False, fontsize=11)
ax1.set_ylim(1.9, -1.1)

# Outline + arrow callout on the Young slice of the Rent bar (51%)
rent_i = 1
young_w_rent = rows[rent_i, 0]
ax1.add_patch(plt.Rectangle((0, rent_i - 0.42), young_w_rent, 0.84,
                            facecolor="none", edgecolor=PALETTE["warn"],
                            linewidth=2.4, zorder=5))
ax1.annotate(
    "Young share = 51%\n(majority among renters)",
    xy=(young_w_rent / 2, rent_i + 0.42),
    xytext=(70, rent_i + 0.78),
    arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.4),
    ha="center", va="center", fontsize=10.5,
    color=PALETTE["warn"], fontweight="bold",
)

# Outline + arrow callout on the Young slice of the Own bar (only 8%)
own_i = 0
young_w_own = rows[own_i, 0]
ax1.add_patch(plt.Rectangle((0, own_i - 0.42), young_w_own, 0.84,
                            facecolor="none", edgecolor=PALETTE["warn"],
                            linewidth=2.4, zorder=5))
ax1.annotate(
    "only 8%",
    xy=(young_w_own, own_i - 0.42),
    xytext=(28, own_i - 0.85),
    arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.2),
    ha="center", va="center", fontsize=10.5,
    color=PALETTE["warn"], fontweight="bold",
)

# Side comparison strip — Young % only (moved further right to avoid overlap)
ax1.text(112, -1.0, "Young %", fontsize=10, color=PALETTE["primary"],
         fontweight="bold")
for i, name in enumerate(groups):
    val = rows[i, 0]
    bar_w = val * 0.45
    ax1.add_patch(plt.Rectangle((112, i - 0.18), bar_w, 0.36,
                                facecolor=colors[0] if name == "Rent" else "#e7d9a3",
                                edgecolor=PALETTE["primary"], linewidth=0.6,
                                clip_on=False))
    ax1.text(112 + bar_w + 1.2, i, f"{int(val)}%", va="center",
             fontsize=10, color=PALETTE["primary"],
             fontweight="bold" if name == "Rent" else "normal",
             clip_on=False)

# ---------------------------------------------------------------------
# RIGHT — table + reading + 'why row?' + R command
# ---------------------------------------------------------------------
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)

ax2.text(0.5, 1.00, "Reading the row-conditional table",
         ha="center", va="top", fontsize=12.5, fontweight="bold",
         color=PALETTE["primary"])

table_txt = (
    "                Age_recode\n"
    "OwnHome   Young  Middle  Senior  TOTAL\n"
    "  Own        8     66      27    100\n"
    "  Rent      51     38      11    100"
)
ax2.text(0.04, 0.95, table_txt, fontsize=10.5, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.5", linewidth=1.0))

ax2.text(0.02, 0.72, "Comparison of the 'Young' column",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.68,
         "  Own   :  8%   (few young owners)\n"
         "  Rent  : 51%   <-- majority are young\n"
         "\n"
         "Gap   = 51% - 8% = 43 pp\n"
         "Ratio = 51 / 8 ~ 6.4 x more young\n"
         "        among renters than owners.",
         fontsize=10.5, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.5", linewidth=1.0))

ax2.text(0.02, 0.36, "Why row %, not column or joint?",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.32,
         "Question = 'is the proportion of Young\n"
         "*within* owners < *within* renters?'\n"
         "-> condition on OwnHome (rows = 100%)\n"
         "and compare the 'Young' column across\n"
         "the two groups.",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

ax2.text(0.02, 0.10, "R command",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.06,
         'distr.table.xy(x=OwnHome, y=Age_recode,\n'
         '               freq.type="y|x", freq="perc", data=DS)',
         fontsize=10, family="monospace", va="top",
         color=PALETTE["warn"],
         bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
