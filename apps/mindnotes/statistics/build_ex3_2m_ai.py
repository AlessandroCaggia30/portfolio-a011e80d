"""AI walkthrough plot for Ex 3.2m — AmountSpent vs Children.

Children is a discrete numerical variable (0,1,2,3). The right tools are
side-by-side boxplots of AmountSpent | Children plus a scatter; correlation
is r = -0.213 (weak negative).

Visual:
  LEFT  — synthetic but realistic side-by-side boxplots of AmountSpent for
          each number of children (0/1/2/3), highlighting (i) the falling
          median, (ii) the shrinking right-skew, and (iii) the much wider
          spread inside the "0 children" group.
  RIGHT — three panels:
          (i)  scatter showing the four discrete strips with group means and
               an OLS regression line whose slope matches r ~= -0.21,
          (ii) a callout/decision box that explains "why boxplots, scatter,
               and r (with caveat)",
          (iii) the R commands.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex3/ex3_2m_ai.png"

# ----------------------------------------------------------------------
# Synthetic AmountSpent | Children — calibrated to: medians fall as
# children rises; the 0-children group is most heterogeneous and most
# right-skewed; overall correlation cor(Children, AmountSpent) ~ -0.21.
# ----------------------------------------------------------------------
rng = np.random.default_rng(7)

# Group sizes (rough; sum ~ 700)
ns       = {0: 290, 1: 220, 2: 130, 3: 60}
# Lognormal location/scale per group: shift down + tighten as children grows
log_mu   = {0: 7.20, 1: 6.95, 2: 6.75, 3: 6.55}
log_sig  = {0: 1.00, 1: 0.85, 2: 0.70, 3: 0.55}

groups, data = [], []
for c in (0, 1, 2, 3):
    y = rng.lognormal(mean=log_mu[c], sigma=log_sig[c], size=ns[c])
    y = np.clip(y, 30, 6500)
    groups.append(c)
    data.append(y)

# Flatten for scatter / correlation
all_x = np.concatenate([np.full_like(d, c) for c, d in zip(groups, data)])
all_y = np.concatenate(data)
r = float(np.corrcoef(all_x, all_y)[0, 1])

# ============================================================
fig = plt.figure(figsize=(15.0, 7.4))
gs  = fig.add_gridspec(2, 2, width_ratios=[1.1, 1.0], height_ratios=[1.0, 1.0],
                       hspace=0.45, wspace=0.30)
ax_box = fig.add_subplot(gs[:, 0])
ax_sc  = fig.add_subplot(gs[0, 1])
ax_txt = fig.add_subplot(gs[1, 1])

# ----------------------------------------------------------------
# LEFT — side-by-side boxplots
# ----------------------------------------------------------------
bp = ax_box.boxplot(
    data, positions=groups, widths=0.55, patch_artist=True,
    medianprops=dict(color=PALETTE["warn"], linewidth=2.2),
    boxprops=dict(facecolor=PALETTE["primary"], alpha=0.32,
                  edgecolor=PALETTE["primary"], linewidth=1.4),
    whiskerprops=dict(color=PALETTE["primary"], linewidth=1.2),
    capprops=dict(color=PALETTE["primary"], linewidth=1.2),
    flierprops=dict(marker="o", markersize=3.5,
                    markerfacecolor=PALETTE["accent"],
                    markeredgecolor="#2a3142", linewidth=0.4, alpha=0.7),
)

# Overlay group means as gold diamonds
means = [float(np.mean(d)) for d in data]
ax_box.scatter(groups, means, marker="D", s=60, color=PALETTE["accent"],
               edgecolor="#2a3142", linewidth=0.8, zorder=6, label="mean")

# Median trend line in red (falling location)
medians = [float(np.median(d)) for d in data]
ax_box.plot(groups, medians, color=PALETTE["warn"], lw=1.8, ls="--",
            marker="o", markersize=6, label="median (falls)")

# Callout on the 0-children group (heterogeneity)
ax_box.annotate(
    "0 children: most\nheterogeneous +\nstrong right-skew",
    xy=(0, np.quantile(data[0], 0.85)),
    xytext=(0.6, np.quantile(data[0], 0.95) + 300),
    fontsize=10, color=PALETTE["warn"], fontweight="bold",
    ha="left", va="bottom",
    arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.3),
)

# Callout on the 3-children group (tight, low)
ax_box.annotate(
    "3 children:\ntightest spread,\nleast skew",
    xy=(3, np.quantile(data[3], 0.75)),
    xytext=(2.05, np.quantile(data[0], 0.55)),
    fontsize=10, color=PALETTE["primary"], fontweight="bold",
    ha="left", va="center",
    arrowprops=dict(arrowstyle="->", color=PALETTE["primary"], lw=1.3),
)

ax_box.set_xticks([0, 1, 2, 3])
ax_box.set_xticklabels(["0", "1", "2", "3"], fontsize=12, fontweight="bold")
ax_box.set_xlabel("Children")
ax_box.set_ylabel("AmountSpent  (EUR)")
ax_box.set_title("Side-by-side boxplots: AmountSpent | Children",
                 pad=10, fontweight="bold", color=PALETTE["primary"])
ax_box.set_ylim(0, max(np.quantile(data[0], 0.995), 4000) * 1.05)
ax_box.legend(loc="upper right", frameon=False, fontsize=10)
ax_box.grid(axis="y", alpha=0.35)

# ----------------------------------------------------------------
# RIGHT TOP — scatter with OLS line + group means
# ----------------------------------------------------------------
jitter = rng.uniform(-0.12, 0.12, size=all_x.size)
ax_sc.scatter(all_x + jitter, all_y, s=10, color=PALETTE["primary"],
              alpha=0.30, edgecolor="none")
# Group means
ax_sc.scatter(groups, means, marker="D", s=60, color=PALETTE["accent"],
              edgecolor="#2a3142", linewidth=0.8, zorder=6, label="group means")

# OLS line
slope, intercept = np.polyfit(all_x, all_y, 1)
xs = np.array([-0.3, 3.3])
ax_sc.plot(xs, slope * xs + intercept, color=PALETTE["warn"], lw=2.0,
           label=f"OLS line (r = {r:+.3f})")

ax_sc.set_xticks([0, 1, 2, 3])
ax_sc.set_xlim(-0.5, 3.5)
ax_sc.set_ylim(0, max(all_y) * 1.02)
ax_sc.set_xlabel("Children")
ax_sc.set_ylabel("AmountSpent (EUR)")
ax_sc.set_title("Scatter (discrete strips) + OLS line",
                pad=8, fontweight="bold", color=PALETTE["primary"])
ax_sc.legend(loc="upper right", frameon=False, fontsize=9.5)
ax_sc.grid(alpha=0.35)

# ----------------------------------------------------------------
# RIGHT BOTTOM — reading panel + R commands
# ----------------------------------------------------------------
ax_txt.axis("off")
ax_txt.set_xlim(0, 1); ax_txt.set_ylim(0, 1)

ax_txt.text(0.5, 1.00, "Which tools, and is r appropriate?",
            ha="center", va="top", fontsize=12.5, fontweight="bold",
            color=PALETTE["primary"])

ax_txt.text(0.02, 0.90,
            "Children = discrete numeric (0,1,2,3); AmountSpent = continuous.\n"
            "  Best graph 1: side-by-side boxplots of AmountSpent | Children\n"
            "  Best graph 2: scatter (4 vertical strips because of discreteness)\n"
            "  Summary index: correlation r = -0.213  (weak NEGATIVE)\n"
            "Use r? YES, with caveat: the relationship is not strictly linear,\n"
            "but |r| is small, so the index does not mislead and communicates\n"
            "intensity well alongside the boxplots.",
            fontsize=10, family="monospace", va="top",
            color=PALETTE["neutral"],
            bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                      boxstyle="round,pad=0.55", linewidth=1.0))

ax_txt.text(0.02, 0.36, "R commands",
            fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax_txt.text(0.04, 0.30,
            'distr.plot.xy(Children, AmountSpent,\n'
            '              plot.type="boxplot", data=DS)\n'
            'distr.plot.xy(Children, AmountSpent,\n'
            '              plot.type="scatter", data=DS)\n'
            'cor(DS$Children, DS$AmountSpent)\n'
            '## [1] -0.2125776',
            fontsize=10, family="monospace", va="top",
            color=PALETTE["warn"],
            bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                      boxstyle="round,pad=0.5", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  simulated r(Children, AmountSpent) = {r:+.4f}")
