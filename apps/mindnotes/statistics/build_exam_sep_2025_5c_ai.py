"""AI walkthrough for past-exam Sep-2025 Ex5c — pooled-variance 90% CI for mu_A - mu_B.

Two panels:
  Left  : Student-t(436) pdf with central 90% area shaded, t-quantile marked.
  Right : 90% pooled-variance CI [-6.09, -3.05] drawn on a number line,
          with the point estimate xbar_A - xbar_B = -4.57 and zero reference.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import t as student_t

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_sep_2025_5c_ai.png"

# given quantities
nA, sA = 58,  6.66
nB, sB = 380, 6.53
diff_mean = -4.57
df = nA + nB - 2  # 436
sp2 = ((nA - 1) * sA ** 2 + (nB - 1) * sB ** 2) / df
tc = student_t.ppf(0.95, df)
se = float(np.sqrt(sp2 * (1 / nA + 1 / nB)))
lo, hi = diff_mean - tc * se, diff_mean + tc * se

fig, axes = plt.subplots(1, 2, figsize=(13, 5))

# --- LEFT: Student-t(436) with central 90% shaded ---
x = np.linspace(-4, 4, 800)
pdf = student_t.pdf(x, df)
ax = axes[0]
ax.plot(x, pdf, color=PALETTE["primary"], linewidth=2.2,
        label=f"$t_{{{df}}}$ pdf  (~$N(0,1)$ for large df)")
mask = (x >= -tc) & (x <= tc)
ax.fill_between(x[mask], 0, pdf[mask], color=PALETTE["accent"], alpha=0.45,
                label=f"central 90% area")
ax.axvline(tc, color=PALETTE["warn"], linestyle="--", linewidth=1.4)
ax.axvline(-tc, color=PALETTE["warn"], linestyle="--", linewidth=1.4)
ax.annotate(f"$t_{{{df},\\,0.95}} = {tc:.3f}$",
            xy=(tc, student_t.pdf(tc, df)), xytext=(tc + 0.25, 0.22),
            fontsize=11, color=PALETTE["warn"],
            arrowprops=dict(arrowstyle="->", color=PALETTE["warn"]))
ax.annotate(f"$-t_{{{df},\\,0.95}} = {-tc:.3f}$",
            xy=(-tc, student_t.pdf(-tc, df)), xytext=(-3.95, 0.22),
            fontsize=11, color=PALETTE["warn"],
            arrowprops=dict(arrowstyle="->", color=PALETTE["warn"]))
ax.set_xlabel("standardized statistic")
ax.set_ylabel("density")
ax.set_title("Step 1 — pivot is $t_{n_A+n_B-2}$\n"
             f"$t_{{{df},\\,0.95}} \\approx {tc:.3f}$  (essentially $z_{{0.95}}=1.645$)")
ax.set_xlim(-4, 4); ax.set_ylim(0, 0.45)
ax.legend(loc="upper right", framealpha=0.95)

# --- RIGHT: number line with the CI ---
ax2 = axes[1]
ax2.axhline(0, color=PALETTE["primary"], linewidth=1.2)
# CI bar
ax2.hlines(0, lo, hi, color=PALETTE["accent"], linewidth=10, alpha=0.55,
           label=f"90% CI = [{lo:.2f}, {hi:.2f}]")
# endpoints + point estimate
ax2.plot([lo, hi], [0, 0], "|", color=PALETTE["primary"],
         markersize=18, markeredgewidth=2.2)
ax2.plot([diff_mean], [0], "o", color=PALETTE["warn"], markersize=11,
         label=f"$\\bar x_A - \\bar x_B = {diff_mean}$")
# zero reference (the null)
ax2.axvline(0, color=PALETTE["muted"], linestyle="--", linewidth=1.2,
            label="0 (no difference)")
# annotations
ax2.text(lo, 0.08, f"{lo:.2f}", ha="center", fontsize=10, color=PALETTE["primary"])
ax2.text(hi, 0.08, f"{hi:.2f}", ha="center", fontsize=10, color=PALETTE["primary"])
ax2.text(diff_mean, -0.12, f"{diff_mean}", ha="center", fontsize=10,
         color=PALETTE["warn"], fontweight="bold")
ax2.text(0, -0.12, "0", ha="center", fontsize=10, color=PALETTE["muted"])
# conclusion box
ax2.text(0.02, 0.92,
         "Whole CI lies left of 0\n=> A's mean Performance is\n   significantly lower than B's\n   at the 10% level.",
         transform=ax2.transAxes, ha="left", va="top",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6",
                   edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))
# R-command box
ax2.text(0.98, 0.05,
         "R reconstruction:\n"
         f"sp2 <- ((58-1)*6.66^2 + (380-1)*6.53^2)/436\n"
         f"tc  <- qt(0.95, df=436)   # = {tc:.3f}\n"
         f"se  <- sqrt(sp2*(1/58 + 1/380))   # = {se:.3f}\n"
         f"-4.57 + c(-1,1)*tc*se     # [{lo:.2f}, {hi:.2f}]",
         transform=ax2.transAxes, ha="right", va="bottom",
         fontsize=9, family="monospace",
         bbox=dict(facecolor="#f4f7fb",
                   edgecolor=PALETTE["primary"],
                   boxstyle="round,pad=0.45", linewidth=1.0))
ax2.set_xlim(-8, 2)
ax2.set_ylim(-0.5, 1.0)
ax2.set_yticks([])
ax2.set_xlabel("$\\mu_A - \\mu_B$  (difference in mean Performance)")
ax2.set_title("Step 2 — pooled-variance 90% CI for $\\mu_A-\\mu_B$\n"
              f"$\\bar x_A-\\bar x_B \\pm t_{{{df},0.95}}\\cdot SE = -4.57 \\pm {tc:.3f}\\cdot {se:.2f}$")
ax2.legend(loc="upper right", framealpha=0.95)

fig.suptitle("Sep-2025 Ex5c  —  90% pooled-variance CI for $\\mu_A-\\mu_B$  "
             f"$= [{lo:.2f},\\;{hi:.2f}]$  (does not include 0)",
             fontsize=12, y=1.02, color=PALETTE["primary"])

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  df={df}  tc={tc:.4f}  se={se:.4f}  CI=[{lo:.3f}, {hi:.3f}]")
