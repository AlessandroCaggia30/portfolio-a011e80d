"""Build AI walkthrough plot for Ex 2.1e — "regular" pizzeria region for Sales.

Visual: A number-line view of the Sales distribution showing the boxplot
five-number summary (min, Q1, median, Q3, max) and the upper fence
F_U = Q3 + 1.5*IQR = 45 912.62. The shaded "regular" region [min, F_U]
is the answer to the question "what is the revenue of a regular pizzeria?".
Outlier observations (54 418, 58 762, 63 683) sit beyond the fence and are
explicitly marked. A side panel reports the arithmetic for the upper fence
and the maximum regular value, 42 987.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_1e_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# Five-number summary + fence (from Ex 2.1b/2.1d)
mn      = 8428.0
q1      = 17683.25
med     = 22349.5
q3      = 28975.0
mx      = 63683.0
iqr     = q3 - q1                       # 11 291.75
fence_U = q3 + 1.5 * iqr                # 45 912.62
max_reg = 42987.0                       # largest Sales below the fence
outliers = [54418.0, 58762.0, 63683.0]

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.2),
                               gridspec_kw={"width_ratios": [2.4, 1]})

# =====================================================================
# LEFT: number line + boxplot + regular region + fence + outliers
# =====================================================================
xlo, xhi = 5000, 68000
ax1.set_xlim(xlo, xhi)
ax1.set_ylim(-1.4, 1.6)

# --- Regular region shading [mn, fence_U] ---
ax1.axvspan(mn, fence_U, ymin=0.0, ymax=1.0,
            color=PALETTE["ok"], alpha=0.10,
            label=fr"Regular region: $[{mn:,.0f},\; {fence_U:,.2f}]$".replace(",", "\u202f"))
# --- Outlier region shading (fence_U, +inf) ---
ax1.axvspan(fence_U, xhi, ymin=0.0, ymax=1.0,
            color=PALETTE["warn"], alpha=0.08,
            label="Outlier region (Sales > upper fence)")

# --- Boxplot drawn manually at y=0 ---
box_h = 0.45
# Box (Q1 -> Q3)
ax1.add_patch(plt.Rectangle((q1, -box_h/2), q3 - q1, box_h,
                            facecolor=PALETTE["secondary"], edgecolor=PALETTE["primary"],
                            linewidth=1.4, alpha=0.85))
# Median line
ax1.plot([med, med], [-box_h/2, box_h/2], color=PALETTE["primary"], lw=2.4)
# Whiskers: min -> Q1  and  Q3 -> max_reg
ax1.plot([mn, q1], [0, 0], color=PALETTE["primary"], lw=1.5)
ax1.plot([q3, max_reg], [0, 0], color=PALETTE["primary"], lw=1.5)
# Whisker caps
for x in (mn, max_reg):
    ax1.plot([x, x], [-box_h/3, box_h/3], color=PALETTE["primary"], lw=1.5)

# Outlier markers
ax1.scatter(outliers, [0]*len(outliers), color=PALETTE["warn"],
            s=80, zorder=5, edgecolor="#7a1b14", linewidth=0.9,
            label="Outliers")

# Upper fence vertical
ax1.axvline(fence_U, color=PALETTE["warn"], lw=1.8, ls="--", alpha=0.95)
ax1.text(fence_U, 1.18, fr"$F_U = Q_3 + 1.5\,IQR$" + "\n" + fr"$= {fence_U:,.2f}$".replace(",", "\u202f"),
         color=PALETTE["warn"], ha="center", va="bottom",
         fontsize=10.5, fontweight="bold")

# Five-number labels under boxplot
def _ann(x, label, dy=-0.95, color=None):
    color = color or PALETTE["neutral"]
    ax1.annotate(f"{label}\n{x:,.2f}".replace(",", "\u202f"),
                 xy=(x, -box_h/2), xytext=(x, dy),
                 ha="center", va="top", fontsize=10, color=color,
                 arrowprops=dict(arrowstyle="-", color=color, lw=0.8, alpha=0.6))

_ann(mn,      "min",  dy=-0.95)
_ann(q1,      r"$Q_1$",dy=-1.20)
_ann(med,     "median",dy=-0.95)
_ann(q3,      r"$Q_3$",dy=-1.20)
_ann(max_reg, "max reg.", dy=-0.95, color=PALETTE["ok"])

# Outlier value labels above the points
for x in outliers:
    ax1.text(x, 0.30, f"{x:,.0f}".replace(",", "\u202f"),
             color=PALETTE["warn"], ha="center", va="bottom",
             fontsize=10, fontweight="bold")

# Bracket: "regular pizzerie" along top
ax1.annotate("", xy=(mn, 0.95), xytext=(fence_U, 0.95),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["ok"], lw=1.6))
ax1.text((mn + fence_U)/2, 1.02, r"regular pizzerie  (Sales $\leq F_U$)",
         color=PALETTE["ok"], ha="center", va="bottom",
         fontsize=11.5, fontweight="bold")

ax1.set_yticks([])
ax1.set_xlabel("Sales (revenue)")
ax1.set_title("Ex 2.1e — Regular vs. outlier region for Sales")
ax1.legend(loc="lower right", framealpha=0.95, fontsize=10)
# Format x-ticks with thin space
ax1.set_xticks([10000, 20000, 30000, 40000, 50000, 60000])
ax1.set_xticklabels([f"{int(t):,}".replace(",", "\u202f") for t in
                     [10000, 20000, 30000, 40000, 50000, 60000]])

# =====================================================================
# RIGHT: arithmetic panel
# =====================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)
ax2.add_patch(FancyBboxPatch((0.02, 0.04), 0.96, 0.92,
                             boxstyle="round,pad=0.02",
                             linewidth=1.1,
                             facecolor="#fbfbfd",
                             edgecolor=PALETTE["primary"]))

def thin(s):
    return s.replace(",", "\u202f")

_line1 = thin(f"Fu  =  {q3:,.0f}  +  1.5 \u00b7 ({q3:,.0f} \u2212 {q1:,.2f})")
_line2 = thin(f"Fu  =  {fence_U:,.2f}")
_line3 = thin(f"max regular Sales  =  {max_reg:,.0f}")
_line4 = thin(f"[ {mn:,.0f} ;  {fence_U:,.2f} ]")

txt = (
    "Upper fence of the boxplot\n\n"
    "Fu  =  Q3 + 1.5 \u00b7 (Q3 \u2212 Q1)\n"
    + _line1 + "\n"
    + _line2 + "\n\n"
    "Maximum regular value\n"
    "max { Sales_i : Sales_i < Fu }\n"
    + _line3 + "\n\n"
    "Regular region\n"
    + _line4 + "\n\n"
    "Outliers\n"
    "54\u202f418,  58\u202f762,  63\u202f683"
)
ax2.text(0.05, 0.93, txt, ha="left", va="top", fontsize=10.5,
         color=PALETTE["primary"])

ax2.text(0.05, 0.06,
         "R:  28975 + 1.5*(28975-17683.25)  \n"
         "    # [1] 45912.62",
         ha="left", va="bottom", fontsize=9.5, family="monospace",
         color=PALETTE["neutral"])

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
