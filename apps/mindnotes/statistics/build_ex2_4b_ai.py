"""Build AI walkthrough plot for Ex 2.4b — are there lower outliers among
the consultants of an insurance company?

Strategy:
  * Reconstruct the ogive from the grouped frequencies in the prompt.
  * Show how Q1 and Q3 are obtained by linear interpolation on the ogive
    (Q1 = 22.5, Q3 = 72) so IQR = 49.5.
  * Display the Tukey lower fence Q1 - 1.5*IQR = -51.82 on the data axis,
    and the observed minimum = 0, to make it visually obvious that no
    observation can fall below the fence => NO lower outliers.

Two panels:
  LEFT:  cumulative proportion ogive with Q1, Q3, and 25%/75% guide lines.
  RIGHT: number-line of the data range, with the lower fence (clipped at
         the bottom), the box [Q1, Q3], the median (~31.7), and the data
         minimum at 0 -- to show min > lower fence by a wide margin.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_4b_ai.png"

# ---------------- Grouped frequencies from the prompt ----------------
endpoints = np.array([0, 10, 20, 30, 60, 90, 150], dtype=float)
cumfreq   = np.array([0, 200, 400, 800, 1300, 1800, 2000], dtype=float)
n         = cumfreq[-1]
cumprop   = cumfreq / n

# Quartiles via linear interpolation on the ogive
Q1 = 20 + (0.25 - 0.20) / 0.02          # = 22.5
Q3 = 60 + (0.75 - 0.65) / (0.25/30)     # 60 + 0.10/0.00833 = 72.0
IQR = Q3 - Q1                            # 49.5
lower_fence = Q1 - 1.5 * IQR             # -51.82...
minimum = 0.0

# Median (50-th percentile) for the boxplot panel
M = 30 + (0.50 - 0.40) / (0.40/30)       # 30 + 0.10/0.01333 = 37.5

fig, (axL, axR) = plt.subplots(1, 2, figsize=(13.2, 5.5),
                               gridspec_kw={"width_ratios": [1.15, 1.0]})

# ===================================================================
# LEFT: ogive with Q1, Q3 interpolation
# ===================================================================
axL.plot(endpoints, cumprop, marker="o", color=PALETTE["primary"],
         linewidth=2.0, markersize=6, label="Ogive (cum. proportion)")
axL.fill_between(endpoints, 0, cumprop, color=PALETTE["primary"], alpha=0.06)

# Guide lines for 25% and 75%
for q, lab, col in [(0.25, "0.25", PALETTE["accent"]),
                    (0.75, "0.75", PALETTE["accent"])]:
    axL.axhline(q, color=col, linestyle=":", linewidth=1.2, alpha=0.85)
    axL.text(155, q, lab, ha="left", va="center", fontsize=9.5,
             color=col, fontweight="bold")

# Mark Q1
axL.plot([Q1, Q1], [0, 0.25], color=PALETTE["warn"], linewidth=1.6,
         linestyle="--")
axL.plot([0, Q1], [0.25, 0.25], color=PALETTE["warn"], linewidth=1.0,
         linestyle="--", alpha=0.6)
axL.scatter([Q1], [0.25], s=80, color=PALETTE["warn"], zorder=5,
            edgecolor="#2a3142", linewidth=0.9)
axL.text(Q1 + 2, 0.04, fr"$Q_1 = {Q1:.1f}$", color=PALETTE["warn"],
         fontsize=11, fontweight="bold")

# Mark Q3
axL.plot([Q3, Q3], [0, 0.75], color=PALETTE["warn"], linewidth=1.6,
         linestyle="--")
axL.plot([0, Q3], [0.75, 0.75], color=PALETTE["warn"], linewidth=1.0,
         linestyle="--", alpha=0.6)
axL.scatter([Q3], [0.75], s=80, color=PALETTE["warn"], zorder=5,
            edgecolor="#2a3142", linewidth=0.9)
axL.text(Q3 + 2, 0.78, fr"$Q_3 = {Q3:.1f}$", color=PALETTE["warn"],
         fontsize=11, fontweight="bold")

axL.set_xlim(-5, 165)
axL.set_ylim(-0.02, 1.05)
axL.set_xlabel("Nr. of contracts")
axL.set_ylabel("Cumulative proportion")
axL.set_title("Step 1 - Read $Q_1, Q_3$ from the ogive")

axL.text(0.02, 0.97,
         fr"$Q_1 = 20 + \dfrac{{0.25-0.20}}{{0.02}} = {Q1:.2f}$" + "\n"
         fr"$Q_3 = 60 + \dfrac{{0.75-0.65}}{{0.00833}} = {Q3:.2f}$" + "\n"
         fr"$\mathrm{{IQR}} = Q_3 - Q_1 = {IQR:.2f}$",
         transform=axL.transAxes, ha="left", va="top", fontsize=10.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))
axL.legend(loc="lower right", framealpha=0.95)

# ===================================================================
# RIGHT: number-line / box-style picture to compare min vs. lower fence
# ===================================================================
axR.set_title("Step 2 - Compare min vs. Tukey lower fence")

# X-axis covers the data range plus a little past the (negative) fence
xmin, xmax = -65, 160
axR.set_xlim(xmin, xmax)
axR.set_ylim(-1.0, 1.4)
axR.set_yticks([])
axR.set_xlabel("Nr. of contracts")

# Shade the IQR box
axR.add_patch(plt.Rectangle((Q1, -0.25), IQR, 0.5,
                            facecolor=PALETTE["primary"], alpha=0.18,
                            edgecolor=PALETTE["primary"], linewidth=1.4))
axR.text((Q1 + Q3) / 2, 0.35, f"IQR = {IQR:.1f}",
         ha="center", va="bottom", fontsize=10.5,
         color=PALETTE["primary"], fontweight="bold")

# Median line inside the box
axR.plot([M, M], [-0.25, 0.25], color=PALETTE["primary"], linewidth=2.0)
axR.text(M, -0.45, fr"median$\,\approx{M:.1f}$",
         ha="center", va="top", fontsize=9.5, color=PALETTE["primary"])

# Q1 / Q3 labels
axR.text(Q1, 0.30, fr"$Q_1={Q1:.1f}$", ha="center", va="bottom",
         fontsize=10, color=PALETTE["warn"], fontweight="bold")
axR.text(Q3, 0.30, fr"$Q_3={Q3:.1f}$", ha="center", va="bottom",
         fontsize=10, color=PALETTE["warn"], fontweight="bold")

# Minimum (observed)
axR.scatter([minimum], [0], s=110, color=PALETTE["accent"],
            edgecolor="#2a3142", linewidth=1.0, zorder=5)
axR.annotate("observed min = 0",
             xy=(minimum, 0), xytext=(15, -0.85),
             fontsize=10.5, color=PALETTE["accent"], fontweight="bold",
             arrowprops=dict(arrowstyle="->", color=PALETTE["accent"],
                             lw=1.1))

# Lower fence
axR.axvline(lower_fence, color=PALETTE["warn"], linestyle="--",
            linewidth=1.8, alpha=0.9)
axR.text(lower_fence, 0.95,
         fr"lower fence" + "\n" + fr"$Q_1 - 1.5\,\mathrm{{IQR}} = {lower_fence:.2f}$",
         ha="center", va="bottom", fontsize=10, color=PALETTE["warn"],
         fontweight="bold")

# Gap arrow: from fence to min
axR.annotate("", xy=(minimum, -0.55), xytext=(lower_fence, -0.55),
             arrowprops=dict(arrowstyle="<->", color="#2a3142", lw=1.2))
axR.text((lower_fence + minimum) / 2, -0.7,
         f"gap = {minimum - lower_fence:.2f}",
         ha="center", va="top", fontsize=9.5, color="#2a3142")

# Vertical reference at x=0 to mark "no values below 0"
axR.axvline(0, color="#a0a4ab", linewidth=0.8, alpha=0.6)

# Verdict box
axR.text(0.98, 0.97,
         "Verdict: NO lower outliers.\n"
         "The observed minimum (0)\n"
         "lies well ABOVE the lower\n"
         "fence (-51.82), so no\n"
         "consultant qualifies as a\n"
         "lower outlier.",
         transform=axR.transAxes, ha="right", va="top", fontsize=10,
         bbox=dict(facecolor="#eaf6ea",
                   edgecolor=PALETTE.get("success", "#2c7a2c"),
                   boxstyle="round,pad=0.5", linewidth=1.1))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  Q1={Q1:.4f}  Q3={Q3:.4f}  IQR={IQR:.4f}")
print(f"  lower_fence={lower_fence:.4f}  min={minimum:.1f}  "
      f"=> min {'>'  if minimum > lower_fence else '<='} fence")
