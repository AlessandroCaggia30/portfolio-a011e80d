"""AI walkthrough plot for Ex 3.12b — Pr(Eff = Medium or Medium-Low | Channel).

Compare two conditional proportions of Effectiveness ∈ {Medium, Medium-Low}
between Channel = In-Store and Channel = Online. Verdict: the claim that
the proportion is higher In-Store is FALSE (0.395 < 0.622).

Visual:
  LEFT  — stacked bars of the conditional distribution of Effectiveness for
          each channel, with the {Medium-Low, Medium} segments highlighted
          and labelled with their numerical contributions.
  RIGHT — (i) side-by-side bars comparing the two pooled proportions
          0.395 vs 0.622 with a red verdict banner,
          (ii) reading panel with the arithmetic + R commands.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle, FancyBboxPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex3/ex3_12b_ai.png"

# ----------------------------------------------------------------------
# Data from Ex 3.12 contingency table (row = Channel)
# ----------------------------------------------------------------------
LABELS = ["Low", "Medium-Low", "Medium", "Medium-High", "High"]
counts = {
    "Online":   np.array([16, 76, 36, 30, 22], dtype=float),
    "In-Store": np.array([25, 38, 90, 108, 63], dtype=float),
}
tot = {k: v.sum() for k, v in counts.items()}     # 180, 324
rel = {k: v / tot[k] for k, v in counts.items()}  # row-relative freqs

# Pooled target proportion for parts (Medium-Low + Medium):
p_in   = rel["In-Store"][1] + rel["In-Store"][2]  # 0.117 + 0.278 = 0.395
p_on   = rel["Online"][1]   + rel["Online"][2]    # 0.422 + 0.200 = 0.622

# ============================================================
fig = plt.figure(figsize=(15.0, 7.4))
gs  = fig.add_gridspec(2, 2, width_ratios=[1.10, 1.00],
                       height_ratios=[1.0, 1.0],
                       hspace=0.55, wspace=0.32)
ax_stack = fig.add_subplot(gs[:, 0])
ax_bar   = fig.add_subplot(gs[0, 1])
ax_txt   = fig.add_subplot(gs[1, 1])

# ----------------------------------------------------------------
# LEFT — stacked horizontal bars of conditional distribution
# ----------------------------------------------------------------
SEG_COLORS = [
    "#9aa3b2",                  # Low      — neutral grey
    PALETTE["warn"],            # Medium-Low — highlighted
    PALETTE["accent"],          # Medium     — highlighted (gold)
    "#6c84a8",                  # Medium-High — muted blue
    PALETTE["primary"],         # High       — primary navy
]
ALPHA = [0.55, 0.95, 0.95, 0.55, 0.85]

channels = ["In-Store", "Online"]
y_pos    = [0.65, 0.20]
bar_h    = 0.30

for ch, y in zip(channels, y_pos):
    left = 0.0
    for i, (lab, col, a) in enumerate(zip(LABELS, SEG_COLORS, ALPHA)):
        w = rel[ch][i]
        rect = Rectangle((left, y), w, bar_h, facecolor=col, alpha=a,
                         edgecolor="white", linewidth=1.4)
        ax_stack.add_patch(rect)
        # Annotate the two highlighted segments with their pct contribution
        if i in (1, 2):
            ax_stack.text(left + w / 2, y + bar_h / 2,
                          f"{w*100:.1f}%",
                          ha="center", va="center", fontsize=11,
                          fontweight="bold", color="#1a1f2b")
        left += w
    # Channel label on the left
    ax_stack.text(-0.015, y + bar_h / 2, ch,
                  ha="right", va="center", fontsize=12,
                  fontweight="bold", color=PALETTE["primary"])
    # Pooled target proportion bracket annotation
    p_target = rel[ch][1] + rel[ch][2]
    x_start  = rel[ch][0]
    ax_stack.annotate(
        f"Med-Low + Med = {p_target:.3f}",
        xy=(x_start + p_target / 2, y + bar_h + 0.005),
        xytext=(x_start + p_target / 2, y + bar_h + 0.10),
        ha="center", va="bottom", fontsize=10.5,
        color=PALETTE["warn"], fontweight="bold",
        arrowprops=dict(arrowstyle="-[,widthB=4.0,lengthB=0.4",
                        color=PALETTE["warn"], lw=1.3),
    )

# Legend (one entry per category)
from matplotlib.patches import Patch
handles = [Patch(facecolor=c, alpha=a, label=l)
           for l, c, a in zip(LABELS, SEG_COLORS, ALPHA)]
ax_stack.legend(handles=handles, loc="lower center",
                bbox_to_anchor=(0.5, -0.18),
                ncol=5, frameon=False, fontsize=10)

ax_stack.set_xlim(0, 1.02)
ax_stack.set_ylim(0.0, 1.15)
ax_stack.set_xticks(np.linspace(0, 1, 6))
ax_stack.set_xticklabels([f"{int(x*100)}%" for x in np.linspace(0, 1, 6)])
ax_stack.set_yticks([])
ax_stack.set_xlabel("Conditional relative frequency of Effectiveness")
ax_stack.set_title("Effectiveness | Channel  —  highlight Medium-Low + Medium",
                   pad=10, fontweight="bold", color=PALETTE["primary"])
for spine in ("top", "right", "left"):
    ax_stack.spines[spine].set_visible(False)
ax_stack.grid(axis="x", alpha=0.30)

# ----------------------------------------------------------------
# RIGHT TOP — head-to-head bars of the two pooled proportions
# ----------------------------------------------------------------
xs   = np.array([0, 1])
vals = np.array([p_in, p_on])
cols = [PALETTE["primary"], PALETTE["warn"]]

bars = ax_bar.bar(xs, vals, width=0.55, color=cols, alpha=0.85,
                  edgecolor="#2a3142", linewidth=1.2)
for b, v in zip(bars, vals):
    ax_bar.text(b.get_x() + b.get_width() / 2, v + 0.015,
                f"{v:.3f}", ha="center", va="bottom",
                fontsize=12.5, fontweight="bold", color="#1a1f2b")

ax_bar.set_xticks(xs)
ax_bar.set_xticklabels(["In-Store\n(n = 324)", "Online\n(n = 180)"],
                       fontsize=11, fontweight="bold")
ax_bar.set_ylabel("Pr(Eff ∈ {Med-Low, Med} | Channel)")
ax_bar.set_ylim(0, 0.78)
ax_bar.set_title("Pooled proportion head-to-head",
                 pad=8, fontweight="bold", color=PALETTE["primary"])
ax_bar.grid(axis="y", alpha=0.30)

# Verdict banner across the top of the bar chart
ax_bar.text(0.5, 0.95,
            f"0.395 < 0.622  →  claim is FALSE",
            transform=ax_bar.transAxes, ha="center", va="top",
            fontsize=11.5, fontweight="bold", color="white",
            bbox=dict(facecolor=PALETTE["warn"],
                      edgecolor=PALETTE["warn"],
                      boxstyle="round,pad=0.40", linewidth=1.0))

# ----------------------------------------------------------------
# RIGHT BOTTOM — reading panel + R commands
# ----------------------------------------------------------------
ax_txt.axis("off")
ax_txt.set_xlim(0, 1); ax_txt.set_ylim(0, 1)

ax_txt.text(0.5, 1.00, "Arithmetic + verdict",
            ha="center", va="top", fontsize=12.5, fontweight="bold",
            color=PALETTE["primary"])

ax_txt.text(0.02, 0.90,
            "Pr(Med-Low or Med | In-Store) = (38 + 90)/324\n"
            "                              = 0.117 + 0.278 = 0.395\n"
            "Pr(Med-Low or Med | Online)   = (76 + 36)/180\n"
            "                              = 0.422 + 0.200 = 0.622\n"
            "Since 0.395 < 0.622, the proportion of Medium / Medium-Low\n"
            "effectiveness is LOWER in-store than online — the statement\n"
            "in the question is FALSE.",
            fontsize=10, family="monospace", va="top",
            color=PALETTE["neutral"],
            bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                      boxstyle="round,pad=0.55", linewidth=1.0))

ax_txt.text(0.02, 0.34, "R commands",
            fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax_txt.text(0.04, 0.27,
            'distr.table.xy(x=Channel, y=Effectiveness,\n'
            '               freq="percentage", data=Campaign)',
            fontsize=10, family="monospace", va="top",
            color=PALETTE["warn"],
            bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                      boxstyle="round,pad=0.5", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  p_in_store = {p_in:.4f}   p_online = {p_on:.4f}")
