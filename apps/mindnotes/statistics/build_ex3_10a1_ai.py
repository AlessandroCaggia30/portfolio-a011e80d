"""AI walkthrough plot for Ex 3.10 a1 — Marginal distribution of Prod.

Prod is qualitative ORDINAL (L < ML < M < MH < H). The frequency table is:
  L 72 (0.11), ML 204 (0.31), M 198 (0.30), MH 140 (0.21), H 54 (0.08).
For ordinal variables, only the MODE and the MEDIAN are appropriate central
tendency measures. Both equal ML — but ML and M each capture ~30% of the
clients, so the distribution is almost bi-modal, making the MODE not very
representative. The MEDIAN is the more suitable summary.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex3/ex3_10a1_ai.png"

levels = ["L", "ML", "M", "MH", "H"]
counts = np.array([72, 204, 198, 140, 54], dtype=float)
props  = counts / counts.sum()
cumprop = np.cumsum(props)

fig = plt.figure(figsize=(15.0, 7.4))
gs  = fig.add_gridspec(2, 2, width_ratios=[1.15, 1.0],
                       height_ratios=[1.0, 1.0],
                       hspace=0.45, wspace=0.30)
ax_bar = fig.add_subplot(gs[:, 0])
ax_cum = fig.add_subplot(gs[0, 1])
ax_txt = fig.add_subplot(gs[1, 1])

# LEFT — bar plot of P(Prod) with mode highlighted
x = np.arange(len(levels))
modal_idx = int(np.argmax(props))
runner_idx = int(np.argsort(props)[-2])

bar_colors = [PALETTE["primary"]] * len(levels)
bar_colors[modal_idx] = PALETTE["warn"]
bar_colors[runner_idx] = "#f0a23a"

bars = ax_bar.bar(x, props, color=bar_colors,
                  edgecolor="#2a3142", linewidth=0.8, alpha=0.85)

for xi, (lev, p, c) in enumerate(zip(levels, props, counts)):
    ax_bar.text(xi, p + 0.008, f"{p:.3f}\n(n={int(c)})",
                ha="center", va="bottom", fontsize=10,
                color=PALETTE["neutral"])

ax_bar.axhline(props[modal_idx], color=PALETTE["warn"], ls="--",
               lw=1.2, alpha=0.55)

ax_bar.annotate(
    "near tie:\nML 30.5% vs M 29.6%\n=> mode not representative",
    xy=(2, props[2]),
    xytext=(2.55, props[2] + 0.07),
    fontsize=10, fontweight="bold", color=PALETTE["warn"],
    ha="left", va="bottom",
    arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.3),
)

ax_bar.set_xticks(x)
ax_bar.set_xticklabels(levels, fontsize=12, fontweight="bold")
ax_bar.set_xlabel("Prod  (qualitative ORDINAL: L < ML < M < MH < H)")
ax_bar.set_ylabel("Relative frequency  P(Prod)")
ax_bar.set_title("Marginal distribution of Prod  (n = 668)",
                 pad=10, fontweight="bold", color=PALETTE["primary"])
ax_bar.set_ylim(0, max(props) * 1.35)
ax_bar.grid(axis="y", alpha=0.35)

# RIGHT TOP — cumulative staircase with median annotated
ax_cum.step(np.concatenate(([-0.5], x, [x[-1] + 0.5])),
            np.concatenate(([0.0], cumprop, [cumprop[-1]])),
            where="post", color=PALETTE["primary"], lw=2.0)
ax_cum.scatter(x, cumprop, s=55, color=PALETTE["primary"],
               edgecolor="#2a3142", zorder=6)

for xi, (lev, cp) in enumerate(zip(levels, cumprop)):
    ax_cum.text(xi, cp + 0.04, f"{cp:.2f}",
                ha="center", va="bottom", fontsize=10,
                color=PALETTE["neutral"])

ax_cum.axhline(0.5, color=PALETTE["warn"], ls="--", lw=1.2)
median_idx = int(np.searchsorted(cumprop, 0.5))
ax_cum.annotate(
    f"median = {levels[median_idx]}\n(first level with cum >= 0.5)",
    xy=(median_idx, 0.5),
    xytext=(median_idx + 0.6, 0.28),
    fontsize=10, fontweight="bold", color=PALETTE["warn"],
    ha="left",
    arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.3),
)

ax_cum.set_xticks(x)
ax_cum.set_xticklabels(levels, fontsize=11, fontweight="bold")
ax_cum.set_xlim(-0.6, len(levels) - 0.4)
ax_cum.set_ylim(0, 1.12)
ax_cum.set_ylabel("Cumulative proportion")
ax_cum.set_title("Cumulative distribution: where the median sits",
                 pad=8, fontweight="bold", color=PALETTE["primary"])
ax_cum.grid(alpha=0.35)

# RIGHT BOTTOM — reading panel + R commands
ax_txt.axis("off")
ax_txt.set_xlim(0, 1); ax_txt.set_ylim(0, 1)

ax_txt.text(0.5, 1.00, "Which central tendency measures, and why?",
            ha="center", va="top", fontsize=12.5, fontweight="bold",
            color=PALETTE["primary"])

ax_txt.text(0.02, 0.90,
            "Prod = qualitative ORDINAL (L, ML, M, MH, H).\n"
            "  Mean is NOT defined (no metric distance between levels).\n"
            "  Mode    = ML  (highest frequency, 30.5%)\n"
            "  Median  = ML  (first level with cum prop >= 0.5)\n"
            "Prefer the MEDIAN: ML and M almost tie at ~30%,\n"
            "so the distribution is nearly BI-MODAL and the mode\n"
            "alone is not representative of the whole sample.",
            fontsize=10, family="monospace", va="top",
            color=PALETTE["neutral"],
            bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                      boxstyle="round,pad=0.55", linewidth=1.0))

ax_txt.text(0.02, 0.36, "R commands",
            fontsize=11.5, fontweight="bold", color=PALETTE["primary"])
ax_txt.text(0.04, 0.30,
            'distr.table.x(Company$Prod)\n'
            '## L 72 0.11   ML 204 0.31   M 198 0.30\n'
            '## MH 140 0.21   H 54 0.08\n'
            'distr.summary.x(Company$Prod,\n'
            '                stats=c("median","mode"))\n'
            '## median = ML,  mode = ML  (mode% = 0.3054)',
            fontsize=10, family="monospace", va="top",
            color=PALETTE["warn"],
            bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                      boxstyle="round,pad=0.5", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
