"""AI walkthrough for Ex 1.3f — Mean vs median for Age (skewness).

Age in customer_habits is mildly right-skewed: median=46, mean=47.13, sd=10.53,
range 16-96. The walkthrough shows:
  LEFT  — simulated right-skewed Age histogram with vertical lines at the
          median (navy) and mean (yellow). Mean sits to the right of median,
          a visual cue of right skewness.
  RIGHT — verbatim `distr.summary.x` R output, the rule mean > median for
          right-skew, and a short why-the-mean-shifts note.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex1/ex1_1_3f_ai.png"

# -----------------------------------------------------------------------------
# Simulate a right-skewed Age distribution roughly matching:
# median=46, mean=47.13, sd=10.53, range [16, 96]
# Use a shifted lognormal then rescale to hit the moments.
# -----------------------------------------------------------------------------
rng = np.random.default_rng(2026)
N = 34866
raw = rng.lognormal(mean=0.0, sigma=0.45, size=N)
# Standardize to mean 0, sd 1, then scale to target sd=10.53 and shift to target mean=47.13
raw = (raw - raw.mean()) / raw.std()
age = raw * 10.53 + 47.13
# Clip to [16, 96]
age = np.clip(age, 16, 96)
# Tiny shift so median lands at 46 (visual fidelity, not statistical exactness)
age = age + (46 - np.median(age))
age = np.clip(age, 16, 96)

med_real, mean_real = 46.0, 47.13

# -----------------------------------------------------------------------------
# Figure
# -----------------------------------------------------------------------------
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 6),
                               gridspec_kw={"width_ratios": [1.15, 1.0]})

# =============================================================================
# LEFT — histogram of Age with median + mean vlines
# =============================================================================
bins = np.arange(16, 97, 2)
counts, edges, patches = ax1.hist(
    age, bins=bins, color=PALETTE["secondary"],
    edgecolor=PALETTE["primary"], linewidth=0.7, alpha=0.85,
)

ymax = counts.max() * 1.18
ax1.set_ylim(0, ymax)

# Median (navy) and Mean (yellow) reference lines
ax1.axvline(med_real, color=PALETTE["primary"], linewidth=2.4, linestyle="-",
            label=f"median = {med_real:.0f}")
ax1.axvline(mean_real, color=PALETTE["accent"], linewidth=2.4, linestyle="--",
            label=f"mean = {mean_real:.2f}")

# Annotate gap
gap = mean_real - med_real
ax1.annotate("",
             xy=(mean_real, ymax * 0.93), xytext=(med_real, ymax * 0.93),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["neutral"], lw=1.4))
ax1.text((med_real + mean_real) / 2, ymax * 0.965,
         f"mean − median = +{gap:.2f}",
         ha="center", va="bottom", fontsize=10.5,
         color=PALETTE["neutral"], fontweight="bold")

# Tail emphasis
ax1.axvspan(70, 96, color=PALETTE["accent"], alpha=0.08)
ax1.text(83, ymax * 0.55, "long right tail\npulls mean up",
         ha="center", va="center", fontsize=10, style="italic",
         color=PALETTE["primary"])

ax1.set_xlabel("Age (years)")
ax1.set_ylabel("Frequency")
ax1.set_title("Age distribution — right-skewed, so mean > median")
ax1.set_xlim(16, 96)
ax1.legend(loc="upper right", framealpha=0.95)
ax1.grid(axis="y", alpha=0.55)

# =============================================================================
# RIGHT — logic / R output panel
# =============================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)

# Title
ax2.text(0.02, 0.97, "Mean vs median under skewness", fontsize=13.5,
         fontweight="bold", color=PALETTE["primary"], va="top")

# Rule box
rule_y0, rule_y1 = 0.78, 0.93
ax2.add_patch(plt.Rectangle((0.02, rule_y0), 0.96, rule_y1 - rule_y0,
                            facecolor="#fffbe6",
                            edgecolor=PALETTE["accent"], linewidth=1.2))
ax2.text(0.05, (rule_y0 + rule_y1) / 2,
         "Right-skew  →  mean > median\n"
         "Left-skew   →  mean < median\n"
         "Symmetric  →  mean ≈ median",
         fontsize=11, color=PALETTE["primary"], va="center",
         family="monospace")

# R output box
r_y0, r_y1 = 0.40, 0.74
ax2.add_patch(plt.Rectangle((0.02, r_y0), 0.96, r_y1 - r_y0,
                            facecolor="#f5f6fa",
                            edgecolor=PALETTE["muted"], linewidth=1.0))
ax2.text(0.04, r_y1 - 0.02, "R  distr.summary.x(Age, ...)",
         fontsize=10.5, fontweight="bold",
         color=PALETTE["primary"], va="top", family="monospace")
r_out = (
    "      n   n.a  min   q1  median   mean    q3   max     sd     var\n"
    "  34866    0   16   40    46    47.13    54    96   10.53  110.87"
)
ax2.text(0.04, r_y1 - 0.10, r_out, fontsize=9.2,
         color=PALETTE["neutral"], va="top", family="monospace")

# Highlight median and mean inside the R output area
ax2.text(0.04, r_y0 + 0.04,
         "median = 46    mean = 47.13   →   mean − median = +1.13",
         fontsize=10.2, color=PALETTE["primary"], va="bottom",
         fontweight="bold")

# Why the gap exists
ax2.text(0.02, 0.34,
         "Why mean > median here",
         fontsize=11.5, fontweight="bold",
         color=PALETTE["primary"], va="top")
ax2.text(0.02, 0.29,
         "• Age tail extends to 96 (max) while bulk sits 40–54.\n"
         "• Mean is a balance point: extreme high values lift it.\n"
         "• Median splits counts 50/50 and ignores tail magnitude.\n"
         "• Skew is mild (gap ≈ 1.1 yrs), so distribution is only\n"
         "  slightly asymmetric — not a heavy-tailed pathology.",
         fontsize=10.2, color=PALETTE["neutral"], va="top",
         linespacing=1.5)

# Footer
ax2.text(0.5, 0.02,
         "Same diagnostic logic applies to Revenue (ex 1.3i), but with a far larger gap.",
         fontsize=9.5, style="italic", ha="center",
         color=PALETTE["muted"])

fig.suptitle("Ex 1.3f — Age: mean vs median diagnosis of skewness",
             fontsize=14.5, fontweight="bold", color=PALETTE["primary"], y=1.00)

os.makedirs(os.path.dirname(OUT), exist_ok=True)
fig.tight_layout(rect=[0, 0, 1, 0.97])
fig.savefig(OUT, dpi=150, bbox_inches="tight")
print("Saved:", OUT)
