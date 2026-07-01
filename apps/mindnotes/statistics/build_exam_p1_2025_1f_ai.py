"""AI walkthrough for P1-2025 Ex1.e (id 1f) — Grouped Out.Engage: midpoint approximation of mean & variance."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_p1_2025_1f_ai.png"

# Grouped table for Out.Engage (from R on Data_PI1_20242210_2.Rdata)
intervals = [(0, 1), (1, 5), (5, 10), (10, 50), (50, 200)]
labels    = ["[0,1)", "[1,5)", "[5,10)", "[10,50)", "[50,200]"]
n         = np.array([110, 231, 88, 110, 11])
mids      = np.array([(a + b) / 2 for a, b in intervals])
N         = n.sum()
m_bar     = float(np.sum(n * mids) / N)
var_g     = float(np.sum(n * (mids - m_bar)**2) / N)
sd_g      = np.sqrt(var_g)
mean_sq   = float(np.sum(n * mids**2) / N)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.5), gridspec_kw=dict(width_ratios=[1.4, 1]))

# Left: histogram (bar per class) with midpoint markers
widths  = np.array([b - a for a, b in intervals])
lefts   = np.array([a for a, _ in intervals])
freq    = n / N / widths     # density: prop / width
ax1.bar(lefts, freq, width=widths, align="edge",
        color=PALETTE["secondary"], alpha=0.55,
        edgecolor=PALETTE["primary"], linewidth=1.0,
        label="grouped density (freq / width)")
for m, cnt, w in zip(mids, n, widths):
    ax1.axvline(m, color=PALETTE["warn"], lw=1.2, ls=":", alpha=0.9)
    ax1.text(m, ax1.get_ylim()[1] * 0.02, f"m={m:g}\nn={cnt}",
             ha="center", va="bottom", fontsize=9,
             color=PALETTE["primary"], fontweight="bold")
ax1.axvline(m_bar, color=PALETTE["ok"], lw=2.4, ls="--",
            label=f"grouped mean = {m_bar:.2f}")
ax1.set_xlabel("Out.Engage")
ax1.set_ylabel("Density  (prop / width)")
ax1.set_xlim(0, 210)
ax1.set_title("Class density with midpoints (uniform-in-interval assumption)")
ax1.legend(loc="upper right", framealpha=0.95)

# Right: contribution to mean and to sum(n_i m_i^2)
contrib_mean = n * mids
contrib_var  = n * mids**2
x = np.arange(len(labels))
w = 0.38
ax2.bar(x - w/2, contrib_mean, width=w, color=PALETTE["secondary"], alpha=0.85,
        label=r"$n_i m_i$   (numerator of $\bar x$)")
ax2.bar(x + w/2, contrib_var / 20, width=w, color=PALETTE["warn"], alpha=0.85,
        label=r"$n_i m_i^2 / 20$   (scaled)")
ax2.set_xticks(x); ax2.set_xticklabels(labels, rotation=20)
ax2.set_title("Where the mean and variance come from")
ax2.legend(loc="upper left", framealpha=0.95, fontsize=9)
ax2.text(0.98, 0.98,
         f"$\\bar x \\approx \\Sigma n_i m_i / N = {int(round(np.sum(contrib_mean)))}/550 = {m_bar:.2f}$\n"
         f"$\\Sigma n_i m_i^2 / N = {mean_sq:.2f}$\n"
         f"$s^2 \\approx {mean_sq:.2f} - {m_bar:.2f}^2 \\approx {var_g:.2f}$\n"
         f"$s \\approx {sd_g:.2f}$",
         transform=ax2.transAxes, ha="right", va="top",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.suptitle("P1-2025 Ex1.e — Grouped mean & variance of Out.Engage", y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}   (mean={m_bar:.4f}, var={var_g:.4f}, sd={sd_g:.4f})")
