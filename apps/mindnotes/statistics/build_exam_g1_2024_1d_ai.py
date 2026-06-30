"""AI walkthrough for G1-2024 Ex1.d — Lunch effect: marginal vs partial (Simpson-style)."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2024_1d_ai.png"

# numbers from R
marginal_diff = 32.31     # mean_nonfree - mean_free
marg_lo, marg_hi = 24.47, 40.16
partial_beta_free = -2.44  # Lunch:free coef in full model
part_se = 2.50
part_lo, part_hi = partial_beta_free - 1.96*part_se, partial_beta_free + 1.96*part_se

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 5.8))

# Left: side-by-side effect plot
yticks = [1, 0]
labels = ["MARGINAL\nEx 1.a2 (non-free − free)", "PARTIAL  (regr.A)\n−·Lunch_{free}"]
vals = [marginal_diff, -partial_beta_free]  # flip sign so both represent "advantage of non-free"
los = [marginal_diff - marg_lo, -partial_beta_free - (-(part_hi))]  # half-widths
his = [marg_hi - marginal_diff, (-(part_lo)) - (-partial_beta_free)]
colors = [PALETTE["primary"], PALETTE["warn"]]
for yi, v, lo, hi, c, ci_str in zip(yticks, vals, los, his, colors,
                                    [f"[{marg_lo:.2f}, {marg_hi:.2f}] (99%)",
                                     f"[{-part_hi:.2f}, {-part_lo:.2f}] (95%)"]):
    ax1.errorbar([v], [yi], xerr=[[lo],[hi]], fmt="o", color=c, ecolor=c,
                 elinewidth=3, capsize=12, markersize=12)
    ax1.text(v, yi+0.18, f"{v:+.2f}", ha="center",
             fontsize=12, color=c, fontweight="bold")
    ax1.text(v + hi + 1.5, yi, ci_str, ha="left", va="center",
             fontsize=9.5, color=c)
ax1.axvline(0, ls="--", color="black", lw=1.4, label="zero (no effect)")
ax1.set_yticks(yticks); ax1.set_yticklabels(labels)
ax1.set_xlabel("estimated advantage of non-free vs free on Read2")
ax1.set_xlim(-15, 50)
ax1.set_title("Two ways to read the 'Lunch' effect on Read2")
ax1.legend(loc="upper right", framealpha=0.95)

# Right: explanation text + arrow Simpson-style
ax2.axis("off")
ax2.text(0.05, 0.96,
         "MARGINAL (a2–a3)  =  total association\n"
         "   Reads off the raw difference in means.\n"
         "   Sees Lunch + everything correlated with Lunch:\n"
         "   • SchoolLoc  (free-lunch concentrated in inner-city)\n"
         "   • Read1      (free-lunch tend to start lower)\n"
         "   • Experience (possibly less experienced teachers)\n"
         "   => ~32 points, p < 2e-16.\n\n"
         "PARTIAL  (Ex 1.c, regr.A)  =  ceteris paribus\n"
         "   Holds Read1, Sex, SchoolLoc, Experience fixed.\n"
         "   Coef on Lunch_{free}:  −2.44   (SE 2.50,  p = 0.33)\n"
         "   => once we level the playing field, Lunch is\n"
         "      no longer a significant driver of Read2.\n\n"
         "INTERPRETATION\n"
         "   Lunch is a MARKER for other disadvantages, not\n"
         "   the causal driver.  Most of the marginal gap is\n"
         "   captured by Read1 + SchoolLoc.  Targeting Lunch\n"
         "   status alone would miss the real channels.",
         transform=ax2.transAxes, ha="left", va="top",
         fontsize=10.5, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.55", linewidth=1.0))

fig.suptitle("G1-2024 Ex1.d — Lunch effect: marginal CI vs partial (multiple regression)",
             fontsize=12.5, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
