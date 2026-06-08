"""Build AI theory diagram for G13 — Confidence intervals.

Two-panel figure:
LEFT  — 95% CI band sketch around an estimate xbar with the +/- z*SE
        endpoints and the area under the sampling-distribution curve
        outside the critical values (alpha/2 each tail) shaded.
RIGHT — 100 simulated 95% CIs for the true mean mu: horizontal segments,
        most green (cover mu) and ~5 red (miss mu) — the long-run
        coverage interpretation.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/theory/th_g13_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)


def phi(z, mu=0.0, sigma=1.0):
    return np.exp(-0.5 * ((z - mu) / sigma) ** 2) / (sigma * np.sqrt(2 * np.pi))


fig = plt.figure(figsize=(13.8, 6.4))
gs = fig.add_gridspec(1, 2, width_ratios=[1.0, 1.1], wspace=0.22,
                      left=0.06, right=0.985, top=0.88, bottom=0.11)

# ============================================================
# LEFT — 95% CI band on the sampling distribution of xbar
# ============================================================
axL = fig.add_subplot(gs[0, 0])

mu0 = 0.0
se = 1.0
z = np.linspace(-4.2, 4.2, 1400)
fz = phi(z, mu0, se)

zc = 1.96   # 95% critical value

# Shaded middle 95% region
mask_in = (z >= -zc) & (z <= zc)
axL.fill_between(z[mask_in], 0, fz[mask_in],
                 color=PALETTE["secondary"], alpha=0.35, zorder=1,
                 label=r"$1-\alpha = 95\%$")

# Tails alpha/2 each
mask_lo = z <= -zc
mask_hi = z >= zc
axL.fill_between(z[mask_lo], 0, fz[mask_lo],
                 color=PALETTE["warn"], alpha=0.55, zorder=1)
axL.fill_between(z[mask_hi], 0, fz[mask_hi],
                 color=PALETTE["warn"], alpha=0.55, zorder=1)

# Density curve
axL.plot(z, fz, color=PALETTE["primary"], lw=2.4, zorder=3)

# Critical-value vertical markers
for k, lbl in [(-zc, r"$\bar x - z_{1-\alpha/2}\,\mathrm{SE}$"),
               (zc,  r"$\bar x + z_{1-\alpha/2}\,\mathrm{SE}$")]:
    axL.plot([k, k], [0, phi(k, mu0, se)], color=PALETTE["primary"],
             lw=1.4, ls="--", alpha=0.85, zorder=2)
    axL.text(k, -0.022, lbl, ha="center", va="top",
             fontsize=10, color=PALETTE["primary"])

# Center marker — point estimate xbar
peak = phi(0, mu0, se)
axL.plot([0, 0], [0, peak], color=PALETTE["accent"],
         lw=1.6, ls=":", alpha=0.9, zorder=2)
axL.plot(0, 0, "o", color=PALETTE["accent"], ms=9, zorder=4)
axL.text(0, -0.022, r"$\bar x$", ha="center", va="top",
         fontsize=12, color=PALETTE["accent"], fontweight="bold")

# Tail labels
axL.text(-2.8, 0.022, r"$\alpha/2$", ha="center", va="bottom",
         fontsize=10.5, color=PALETTE["warn"], fontweight="bold")
axL.text(2.8, 0.022, r"$\alpha/2$", ha="center", va="bottom",
         fontsize=10.5, color=PALETTE["warn"], fontweight="bold")

# Central label
axL.text(0, peak * 0.45, r"$95\%$",
         ha="center", va="center", fontsize=20, fontweight="bold",
         color=PALETTE["primary"])

# CI bracket below the axis
y_brk = -0.075
axL.annotate("", xy=(-zc, y_brk), xytext=(zc, y_brk),
             arrowprops=dict(arrowstyle="|-|", color=PALETTE["accent"],
                             lw=1.8))
axL.text(0, y_brk - 0.018,
         r"95% CI:  $\bar x \pm z_{1-\alpha/2}\cdot \mathrm{SE}$",
         ha="center", va="top", fontsize=11.5,
         color=PALETTE["primary"], fontweight="bold")

axL.set_xlim(-4.2, 4.2)
axL.set_ylim(-0.12, peak * 1.18)
axL.set_xticks([])
axL.set_yticks([])
axL.set_title("95% confidence interval  —  band around $\\bar x$",
              color=PALETTE["primary"])
for side in ("top", "right", "left"):
    axL.spines[side].set_visible(False)
axL.spines["bottom"].set_position(("data", 0))

# Formula card
axL.text(-4.1, peak * 1.10,
         r"$\Pr(\bar X - z\cdot\mathrm{SE} \leq \mu \leq \bar X + z\cdot\mathrm{SE})=1-\alpha$",
         ha="left", va="top", fontsize=10.5, color=PALETTE["neutral"],
         bbox=dict(boxstyle="round,pad=0.35", fc="#fff8d8",
                   ec=PALETTE["accent"], lw=1.0))

# ============================================================
# RIGHT — 100 simulated 95% CIs around true mu
# ============================================================
axR = fig.add_subplot(gs[0, 1])

rng = np.random.default_rng(7)
mu_true = 50.0
sigma_pop = 10.0
n = 30
N_REPS = 100
zc95 = 1.959964

samples = rng.normal(mu_true, sigma_pop, size=(N_REPS, n))
xbars = samples.mean(axis=1)
se_samp = sigma_pop / np.sqrt(n)
half = zc95 * se_samp
lows = xbars - half
highs = xbars + half
covers = (lows <= mu_true) & (mu_true <= highs)

# Force the count to exactly 5 misses (95 covers) for visual clarity:
# nudge misses to a target of 5 by re-seeding if too far off would be wrong;
# instead, just plot as-is and report the actual count.
n_miss = int((~covers).sum())
n_cov = int(covers.sum())

ys = np.arange(N_REPS)

# Plot each CI as a horizontal segment
col_cov = PALETTE["ok"]
col_miss = PALETTE["warn"]

for i in range(N_REPS):
    c = col_cov if covers[i] else col_miss
    axR.plot([lows[i], highs[i]], [ys[i], ys[i]],
             color=c, lw=1.4, alpha=0.85, zorder=2)
    axR.plot(xbars[i], ys[i], "o", color=c, ms=2.6, zorder=3)

# Vertical true-mean line
axR.axvline(mu_true, color=PALETTE["primary"], lw=2.0, ls="--",
            alpha=0.9, zorder=4, label=r"true $\mu$")
axR.text(mu_true, N_REPS + 1.5, r"true $\mu$",
         ha="center", va="bottom", fontsize=11.5,
         fontweight="bold", color=PALETTE["primary"])

# Annotation summary box
axR.text(0.02, 0.98,
         (f"100 samples of size $n={n}$ from $N({mu_true:g},\\,{sigma_pop:g}^2)$\n"
          f"95% CIs: {n_cov} cover  $\\mu$ (green),  "
          f"{n_miss} miss (red)\n"
          r"long-run coverage $\to 95\%$"),
         transform=axR.transAxes,
         ha="left", va="top", fontsize=10.5, color=PALETTE["neutral"],
         bbox=dict(boxstyle="round,pad=0.4", fc="#fff8d8",
                   ec=PALETTE["accent"], lw=1.0))

axR.set_xlim(mu_true - 4 * se_samp - 1.0, mu_true + 4 * se_samp + 1.0)
axR.set_ylim(-2, N_REPS + 6)
axR.set_xlabel(r"value of $\bar x$ and its CI endpoints")
axR.set_ylabel("simulated sample index (1–100)")
axR.set_title("Long-run coverage  —  100 simulated 95% CIs",
              color=PALETTE["primary"])

fig.suptitle("Confidence intervals  —  band around the estimate and long-run coverage",
             fontsize=14, fontweight="bold", color=PALETTE["primary"],
             y=0.985)

fig.savefig(OUT, dpi=140)
print(f"Saved {OUT}  (covers={n_cov}, misses={n_miss})")
