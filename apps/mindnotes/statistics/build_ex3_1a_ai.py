"""AI walkthrough plot for Ex 3.1a — conditional boxplots of Sales | SmokingArea.

Visual: side-by-side boxplots of Sales conditioned on SmokingArea (No vs Yes),
with the five-number summary annotated on each group, IQR brackets, and a
side panel reading off the geometric differences (location, dispersion, shape,
tail/outliers) that the answer text comments on.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import pyreadr
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex3/ex3_1a_ai.png"

# ------------------ Load real pizzerie data ------------------
r = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex3/Exe3_Data.Rdata")
pizz = r["pizzerie"]
no_sales  = pizz.loc[pizz["SmokingArea"] == "No",  "Sales"].to_numpy(dtype=float)
yes_sales = pizz.loc[pizz["SmokingArea"] == "Yes", "Sales"].to_numpy(dtype=float)

def summary(x):
    q1, med, q3 = np.quantile(x, [.25, .5, .75])
    iqr = q3 - q1
    upper_fence = q3 + 1.5 * iqr
    lower_fence = q1 - 1.5 * iqr
    regular = x[(x >= lower_fence) & (x <= upper_fence)]
    outliers = x[(x < lower_fence) | (x > upper_fence)]
    return dict(n=len(x), xmin=x.min(), xmax=x.max(),
                q1=q1, med=med, q3=q3, iqr=iqr,
                upper_fence=upper_fence, lower_fence=lower_fence,
                w_lo=regular.min(), w_hi=regular.max(),
                outliers=outliers, mean=x.mean(), sd=x.std(ddof=1))

S_no  = summary(no_sales)
S_yes = summary(yes_sales)

# ------------------ Figure ------------------
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 6.2),
                               gridspec_kw={"width_ratios": [1.15, 1.0]})

# ============================================================
# LEFT: side-by-side conditional boxplots with annotations
# ============================================================
positions = [1, 2]
bp = ax1.boxplot(
    [no_sales, yes_sales],
    positions=positions, widths=0.5, patch_artist=True,
    boxprops=dict(facecolor=PALETTE["secondary"], edgecolor=PALETTE["primary"], linewidth=1.4),
    medianprops=dict(color=PALETTE["primary"], linewidth=2.4),
    whiskerprops=dict(color=PALETTE["primary"], linewidth=1.2),
    capprops=dict(color=PALETTE["primary"], linewidth=1.2),
    flierprops=dict(marker="o", markerfacecolor=PALETTE["warn"],
                    markeredgecolor=PALETTE["warn"], markersize=7, alpha=0.9),
)
# Recolor the "Yes" box with a warmer shade to set it apart
bp["boxes"][1].set_facecolor("#f7d36b")
bp["boxes"][1].set_edgecolor(PALETTE["primary"])

ax1.set_xticks(positions)
ax1.set_xticklabels(["No  (n=51)", "Yes  (n=49)"])
ax1.set_xlabel("SmokingArea")
ax1.set_ylabel("Sales (EUR)")
ax1.set_title("Side-by-side boxplots: Sales | SmokingArea")
ax1.set_xlim(0.35, 2.95)
ymax = max(S_no["xmax"], S_yes["xmax"]) * 1.10
ax1.set_ylim(0, ymax)
ax1.grid(axis="y", alpha=0.5)

def annotate(S, xpos, side="right"):
    """Annotate 5-number summary on a single boxplot."""
    if side == "right":
        xt = xpos + 0.30
        ha = "left"
    else:
        xt = xpos - 0.30
        ha = "right"

    for y, lab, col, weight in [
        (S["xmin"], f"min = {S['xmin']:,.0f}", PALETTE["neutral"], "normal"),
        (S["q1"],   f"$Q_1$ = {S['q1']:,.0f}", PALETTE["neutral"], "normal"),
        (S["med"],  f"median = {S['med']:,.0f}", PALETTE["primary"], "bold"),
        (S["q3"],   f"$Q_3$ = {S['q3']:,.0f}", PALETTE["neutral"], "normal"),
    ]:
        ax1.annotate(lab, xy=(xpos + (0.225 if side == "right" else -0.225), y),
                     xytext=(xt, y),
                     arrowprops=dict(arrowstyle="-", color=PALETTE["muted"], lw=0.8),
                     ha=ha, va="center", fontsize=10, color=col, fontweight=weight)
    # whisker top label
    ax1.annotate(f"upper whisker = {S['w_hi']:,.0f}",
                 xy=(xpos, S["w_hi"]), xytext=(xt, S["w_hi"]),
                 arrowprops=dict(arrowstyle="-", color=PALETTE["muted"], lw=0.8),
                 ha=ha, va="center", fontsize=9.5, color=PALETTE["neutral"])

annotate(S_no,  positions[0], side="left")
annotate(S_yes, positions[1], side="right")

# IQR bracket on each box
for S, x in [(S_no, positions[0]), (S_yes, positions[1])]:
    bx = x - 0.34
    ax1.annotate("", xy=(bx, S["q1"]), xytext=(bx, S["q3"]),
                 arrowprops=dict(arrowstyle="<->", color=PALETTE["accent"], lw=1.6))
    ax1.text(bx - 0.03, (S["q1"] + S["q3"]) / 2,
             f"IQR\n= {S['iqr']:,.0f}",
             color=PALETTE["accent"], ha="right", va="center",
             fontsize=10, fontweight="bold")

# Outlier callout for "Yes" group
if len(S_yes["outliers"]):
    olist = ", ".join(f"{int(v):,}" for v in sorted(S_yes["outliers"]))
    ax1.annotate(f"outliers (Q3+1.5·IQR={S_yes['upper_fence']:,.0f}):\n{olist}",
                 xy=(positions[1], max(S_yes["outliers"])),
                 xytext=(positions[1] + 0.40, max(S_yes["outliers"])),
                 arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.0),
                 ha="left", va="center", fontsize=9.5,
                 color=PALETTE["warn"], fontweight="bold")

# Reference: medians as a horizontal line
ax1.axhline(S_no["med"],  color=PALETTE["primary"], ls=":", lw=0.8, alpha=0.55)
ax1.axhline(S_yes["med"], color="#7a6700",          ls=":", lw=0.8, alpha=0.55)

# ============================================================
# RIGHT: textual reading of the comparison
# ============================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)

ax2.text(0.5, 0.97,
         "Reading the conditional comparison",
         ha="center", va="top", fontsize=12.5, fontweight="bold",
         color=PALETTE["primary"])

# 1. Location
ax2.text(0.02, 0.905, "1. Location (median, mean)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.855,
         f"median(No)  = {S_no['med']:,.0f}      mean(No)  = {S_no['mean']:,.0f}\n"
         f"median(Yes) = {S_yes['med']:,.0f}      mean(Yes) = {S_yes['mean']:,.0f}\n"
         f"-> Sales are higher when SmokingArea = Yes\n"
         f"   (gap in medians \u2248 {S_yes['med']-S_no['med']:,.0f} EUR).",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# 2. Dispersion
ax2.text(0.02, 0.69, "2. Dispersion (IQR, range, sd)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.64,
         f"IQR(No)  = {S_no['iqr']:,.0f}     range(No)  = {S_no['xmax']-S_no['xmin']:,.0f}     sd(No)  = {S_no['sd']:,.0f}\n"
         f"IQR(Yes) = {S_yes['iqr']:,.0f}     range(Yes) = {S_yes['xmax']-S_yes['xmin']:,.0f}     sd(Yes) = {S_yes['sd']:,.0f}\n"
         f"-> 'Yes' group is consistently more variable\n"
         f"   in both the box (IQR) and the full range.",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# 3. Shape / skewness
no_lower_half = S_no["med"] - S_no["q1"]
no_upper_half = S_no["q3"] - S_no["med"]
yes_lower_half = S_yes["med"] - S_yes["q1"]
yes_upper_half = S_yes["q3"] - S_yes["med"]
ax2.text(0.02, 0.46, "3. Shape (box symmetry, tail)",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.41,
         f"No : median - Q1 = {no_lower_half:,.0f}  >  Q3 - median = {no_upper_half:,.0f}\n"
         f"     -> roughly symmetric, slightly long left half of box\n"
         f"Yes: median - Q1 = {yes_lower_half:,.0f}  <  Q3 - median = {yes_upper_half:,.0f}\n"
         f"     -> right-skewed: long upper half + outliers on top",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# 4. R command
ax2.text(0.02, 0.20, "4. The R command",
         fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax2.text(0.04, 0.15,
         "distr.plot.xy(x=Sales, y=SmokingArea,\n"
         "              plot.type=\"boxplot\",\n"
         "              data=pizzerie)",
         fontsize=10, family="monospace", va="top",
         color=PALETTE["warn"],
         bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  No : n={S_no['n']}  med={S_no['med']:.0f}  IQR={S_no['iqr']:.0f}  range=[{S_no['xmin']:.0f},{S_no['xmax']:.0f}]")
print(f"  Yes: n={S_yes['n']}  med={S_yes['med']:.0f}  IQR={S_yes['iqr']:.0f}  range=[{S_yes['xmin']:.0f},{S_yes['xmax']:.0f}]")
print(f"  Yes outliers: {sorted(int(v) for v in S_yes['outliers'])}")
