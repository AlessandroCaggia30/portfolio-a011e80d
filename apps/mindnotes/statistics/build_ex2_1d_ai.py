"""Build AI walkthrough plot for Ex 2.1d — numerical anatomy of the Sales boxplot.

Question: "Specify clearly what numerical values are reported in the boxplot."
The plot annotates EVERY number the boxplot encodes: the 5-number summary
(min, Q1, median, Q3, max), the IQR span, the upper Tukey fence
F_U = Q3 + 1.5*IQR = 45 912.62, the largest regular value (42 987, tip
of the upper whisker) and the three outliers above F_U
(54 418, 58 762, 63 683). A side panel maps each visual element of the
boxplot to its numerical value with the R commands that produce it.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_1d_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# Five-number summary + fence (from Ex 2.1b)
mn      = 8428.0
q1      = 17683.25
med     = 22349.5
q3      = 28975.0
mx      = 63683.0
iqr     = q3 - q1                       # 11 291.75
fence_U = q3 + 1.5 * iqr                # 45 912.62
max_reg = 42987.0                       # largest Sales below the fence
outliers = [54418.0, 58762.0, 63683.0]

def thin(s):
    return s.replace(",", "\u202f")

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.8, 5.6),
                               gridspec_kw={"width_ratios": [2.5, 1]})

# =====================================================================
# LEFT: boxplot with every number labeled
# =====================================================================
xlo, xhi = 5000, 68000
ax1.set_xlim(xlo, xhi)
ax1.set_ylim(-1.7, 2.1)

# --- Box (Q1 -> Q3) ---
box_h = 0.55
ax1.add_patch(plt.Rectangle((q1, -box_h/2), q3 - q1, box_h,
                            facecolor=PALETTE["secondary"],
                            edgecolor=PALETTE["primary"],
                            linewidth=1.5, alpha=0.85))
# Median line
ax1.plot([med, med], [-box_h/2, box_h/2],
         color=PALETTE["primary"], lw=2.6)

# Whiskers: min -> Q1  and  Q3 -> max_reg
ax1.plot([mn, q1], [0, 0], color=PALETTE["primary"], lw=1.5)
ax1.plot([q3, max_reg], [0, 0], color=PALETTE["primary"], lw=1.5)
for x in (mn, max_reg):
    ax1.plot([x, x], [-box_h/3, box_h/3], color=PALETTE["primary"], lw=1.5)

# Outliers (dots beyond F_U)
ax1.scatter(outliers, [0]*len(outliers),
            color=PALETTE["warn"], s=90, zorder=5,
            edgecolor="#7a1b14", linewidth=0.9,
            label="outliers  (Sales > F_U)")

# Upper fence vertical line
ax1.axvline(fence_U, color=PALETTE["warn"], lw=1.6, ls="--", alpha=0.9)
ax1.text(fence_U, 1.45,
         thin(fr"$F_U = Q_3 + 1.5\,IQR = {fence_U:,.2f}$"),
         color=PALETTE["warn"], ha="center", va="bottom",
         fontsize=10.5, fontweight="bold")

# IQR bracket above the box
ax1.annotate("", xy=(q1, 0.55), xytext=(q3, 0.55),
             arrowprops=dict(arrowstyle="<->",
                             color=PALETTE["primary"], lw=1.2))
ax1.text((q1 + q3)/2, 0.62,
         thin(fr"$IQR = Q_3 - Q_1 = {iqr:,.2f}$"),
         color=PALETTE["primary"], ha="center", va="bottom",
         fontsize=10.5, fontweight="bold")

# Five-number labels under the boxplot
def _ann(x, label, dy=-1.05, color=None):
    color = color or PALETTE["neutral"]
    ax1.annotate(thin(f"{label}\n{x:,.2f}"),
                 xy=(x, -box_h/2), xytext=(x, dy),
                 ha="center", va="top", fontsize=10, color=color,
                 arrowprops=dict(arrowstyle="-", color=color,
                                 lw=0.8, alpha=0.55))

_ann(mn,      "min",       dy=-1.05)
_ann(q1,      r"$Q_1$",    dy=-1.35)
_ann(med,     "median",    dy=-1.05)
_ann(q3,      r"$Q_3$",    dy=-1.35)
_ann(max_reg, "max regular", dy=-1.05, color=PALETTE["ok"])

# Outlier value labels above the points
for x in outliers:
    ax1.text(x, 0.30, thin(f"{x:,.0f}"),
             color=PALETTE["warn"], ha="center", va="bottom",
             fontsize=10, fontweight="bold")

ax1.set_yticks([])
ax1.set_xlabel("Sales (revenue, USD)")
ax1.set_title("Ex 2.1d — Numerical values encoded by the boxplot of Sales")
ax1.set_xticks([10000, 20000, 30000, 40000, 50000, 60000])
ax1.set_xticklabels([thin(f"{int(t):,}") for t in
                     [10000, 20000, 30000, 40000, 50000, 60000]])
ax1.legend(loc="upper right", framealpha=0.95, fontsize=10)

# =====================================================================
# RIGHT: numbered legend mapping boxplot elements to values
# =====================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)
ax2.add_patch(FancyBboxPatch((0.02, 0.04), 0.96, 0.92,
                             boxstyle="round,pad=0.02",
                             linewidth=1.1,
                             facecolor="#fbfbfd",
                             edgecolor=PALETTE["primary"]))

lines = [
    "Numerical values reported",
    "",
    thin(f"  min          =  {mn:,.0f}"),
    thin(f"  Q1 (25%)     =  {q1:,.2f}"),
    thin(f"  median (50%) =  {med:,.2f}"),
    thin(f"  Q3 (75%)     =  {q3:,.2f}"),
    thin(f"  max          =  {mx:,.0f}"),
    "",
    thin(f"  IQR = Q3 - Q1 = {iqr:,.2f}"),
    "",
    "Upper Tukey fence",
    "  Fu = Q3 + 1.5 \u00b7 IQR",
    thin(f"     = {fence_U:,.2f}"),
    "",
    "Whisker tip (max regular)",
    thin(f"  max Sales_i < Fu = {max_reg:,.0f}"),
    "",
    "Outliers  (Sales > Fu)",
    "  54\u202f418,  58\u202f762,  63\u202f683",
]

ax2.text(0.05, 0.95, "\n".join(lines),
         ha="left", va="top", fontsize=10.5,
         color=PALETTE["primary"])

ax2.text(0.05, 0.08,
         "R:  28975 + 1.5*(28975-17683.25)\n"
         "    # [1] 45912.62\n"
         "    max(Sales[Sales < 45912.62])\n"
         "    # [1] 42987",
         ha="left", va="bottom", fontsize=9.5, family="monospace",
         color=PALETTE["neutral"])

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
