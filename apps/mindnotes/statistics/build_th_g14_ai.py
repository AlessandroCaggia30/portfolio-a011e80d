"""Build AI theory diagram for G14 — Hypothesis tests.

Two-panel figure:
LEFT  — H0 sampling distribution with two-sided rejection regions at level
        alpha, observed test statistic and shaded p-value (two-sided).
RIGHT — H0 vs H1 overlay showing Type I error (alpha) and Type II error
        (beta), power = 1 - beta, and the critical value boundary.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/theory/th_g14_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)


def phi(z, mu=0.0, sigma=1.0):
    return np.exp(-0.5 * ((z - mu) / sigma) ** 2) / (sigma * np.sqrt(2 * np.pi))


fig = plt.figure(figsize=(13.8, 6.4))
gs = fig.add_gridspec(1, 2, width_ratios=[1.0, 1.05], wspace=0.22,
                      left=0.055, right=0.985, top=0.86, bottom=0.13)

# ============================================================
# LEFT — Two-sided test: rejection regions + p-value
# ============================================================
axL = fig.add_subplot(gs[0, 0])

z = np.linspace(-4.2, 4.2, 1400)
fz = phi(z)
z_crit = 1.96    # two-sided alpha = 0.05
t_obs  = 2.35    # observed test statistic

# Acceptance region (light fill)
mask_acc = (z >= -z_crit) & (z <= z_crit)
axL.fill_between(z[mask_acc], 0, fz[mask_acc], color="#e9f0f8",
                 alpha=0.9, zorder=1, label="Acceptance region")

# Rejection regions (red)
mask_R = z >= z_crit
mask_L = z <= -z_crit
axL.fill_between(z[mask_R], 0, fz[mask_R], color=PALETTE["warn"],
                 alpha=0.45, zorder=2)
axL.fill_between(z[mask_L], 0, fz[mask_L], color=PALETTE["warn"],
                 alpha=0.45, zorder=2,
                 label=r"Rejection region  ($\alpha/2$ each tail)")

# p-value shading (darker red, tails beyond |t_obs|)
mask_pR = z >= t_obs
mask_pL = z <= -t_obs
axL.fill_between(z[mask_pR], 0, fz[mask_pR], color="#8a1f12",
                 alpha=0.85, zorder=3)
axL.fill_between(z[mask_pL], 0, fz[mask_pL], color="#8a1f12",
                 alpha=0.85, zorder=3, label=r"$p$-value (two-sided)")

# Density curve
axL.plot(z, fz, color=PALETTE["primary"], lw=2.4, zorder=4)

# Critical lines + observed stat line
peak = phi(0)
for k in (-z_crit, z_crit):
    axL.plot([k, k], [0, phi(k)], color=PALETTE["warn"], lw=1.2,
             ls="--", zorder=4)
axL.plot([t_obs, t_obs], [0, phi(t_obs)], color="#8a1f12", lw=1.8,
         zorder=5)

# Tail labels (alpha/2)
axL.text(z_crit + 1.0, phi(z_crit + 0.3) + 0.018,
         r"$\alpha/2$", color=PALETTE["warn"], fontsize=11.5,
         fontweight="bold", ha="center")
axL.text(-z_crit - 1.0, phi(-z_crit - 0.3) + 0.018,
         r"$\alpha/2$", color=PALETTE["warn"], fontsize=11.5,
         fontweight="bold", ha="center")

# Critical value annotations
axL.annotate(r"$-z_{1-\alpha/2}$", xy=(-z_crit, 0), xytext=(-z_crit, -0.035),
             ha="center", va="top", fontsize=10.5, color=PALETTE["warn"],
             fontweight="bold")
axL.annotate(r"$+z_{1-\alpha/2}$", xy=(z_crit, 0), xytext=(z_crit, -0.035),
             ha="center", va="top", fontsize=10.5, color=PALETTE["warn"],
             fontweight="bold")

# Observed t annotation
axL.annotate(r"$t_{\mathrm{obs}}$", xy=(t_obs, phi(t_obs)),
             xytext=(t_obs + 0.55, phi(t_obs) + 0.10),
             fontsize=11.5, color="#8a1f12", fontweight="bold",
             arrowprops=dict(arrowstyle="->", color="#8a1f12", lw=1.0))

# Center label
axL.text(0, peak * 0.55, r"Under $H_0$" + "\n" + r"$T \sim N(0,1)$",
         ha="center", va="center", fontsize=11,
         color=PALETTE["primary"], fontweight="bold")

# Decision card
axL.text(-4.1, peak * 1.08,
         r"Reject $H_0$ iff  $|t_{\mathrm{obs}}| > z_{1-\alpha/2}$"
         "\n"
         r"$\Leftrightarrow$  $p$-value $< \alpha$",
         ha="left", va="top", fontsize=10.5, color=PALETTE["primary"],
         bbox=dict(boxstyle="round,pad=0.35", fc="#fff8d8",
                   ec=PALETTE["accent"], lw=1.0))

axL.set_xlim(-4.2, 4.2)
axL.set_ylim(-0.05, peak * 1.18)
axL.set_xlabel(r"test statistic  $t$")
axL.set_ylabel(r"density under $H_0$")
axL.set_title("Two-sided test  —  rejection region & $p$-value",
              color=PALETTE["primary"])
axL.legend(loc="upper right", fontsize=9.5, framealpha=0.95)

# ============================================================
# RIGHT — H0 vs H1: Type I (alpha), Type II (beta), Power
# ============================================================
axR = fig.add_subplot(gs[0, 1])

# One-sided right-tail test for clarity of beta visualization
mu0 = 0.0
mu1 = 2.6      # true mean under H1 (in units of SE)
z_c = 1.645    # one-sided alpha = 0.05

x = np.linspace(-4.0, 6.5, 1600)
f0 = phi(x, mu0, 1.0)
f1 = phi(x, mu1, 1.0)

# H0 curve + alpha shading (right tail)
axR.plot(x, f0, color=PALETTE["primary"], lw=2.3, label=r"$H_0$ : $\mu=\mu_0$")
mask_a = x >= z_c
axR.fill_between(x[mask_a], 0, f0[mask_a], color=PALETTE["warn"],
                 alpha=0.55, zorder=3, label=r"Type I error  $\alpha$")

# H1 curve + beta shading (area of H1 to the left of z_c)
axR.plot(x, f1, color=PALETTE["ok"], lw=2.3, ls="-",
         label=r"$H_1$ : $\mu=\mu_1 > \mu_0$")
mask_b = x <= z_c
axR.fill_between(x[mask_b], 0, f1[mask_b], color=PALETTE["accent"],
                 alpha=0.55, zorder=2, label=r"Type II error  $\beta$")

# Power shading (H1 to the right of z_c) — light forest
mask_p = x >= z_c
axR.fill_between(x[mask_p], 0, f1[mask_p], color=PALETTE["ok"],
                 alpha=0.28, zorder=2, label=r"Power  $1-\beta$")

# Critical value vertical line
axR.axvline(z_c, color=PALETTE["neutral"], lw=1.3, ls="--", zorder=4)
axR.text(z_c, phi(0) * 1.08, r"critical value  $c$",
         ha="center", va="bottom", fontsize=10, color=PALETTE["neutral"],
         fontweight="bold")

# Curve labels
axR.text(mu0 - 1.8, phi(0) * 0.92, r"$H_0$",
         ha="center", fontsize=13, color=PALETTE["primary"],
         fontweight="bold")
axR.text(mu1 + 1.8, phi(0) * 0.92, r"$H_1$",
         ha="center", fontsize=13, color=PALETTE["ok"],
         fontweight="bold")

# Error truth table card
axR.text(-3.9, phi(0) * 1.32,
         r"$\alpha = \Pr(\text{reject } H_0 \mid H_0)$"
         "\n"
         r"$\beta  = \Pr(\text{accept } H_0 \mid H_1)$"
         "\n"
         r"Power $= 1-\beta = \Pr(\text{reject } H_0 \mid H_1)$",
         ha="left", va="top", fontsize=10, color=PALETTE["primary"],
         bbox=dict(boxstyle="round,pad=0.35", fc="#fff8d8",
                   ec=PALETTE["accent"], lw=1.0))

axR.set_xlim(-4.0, 6.5)
axR.set_ylim(0, phi(0) * 1.45)
axR.set_xlabel(r"test statistic  /  sample mean")
axR.set_ylabel("density")
axR.set_title(r"$H_0$ vs $H_1$  —  Type I, Type II, Power",
              color=PALETTE["primary"])
axR.legend(loc="upper right", fontsize=9.5, framealpha=0.95)

fig.suptitle("Hypothesis testing  —  rejection regions, $p$-value, error types",
             fontsize=14, fontweight="bold", color=PALETTE["primary"],
             y=0.975)

fig.savefig(OUT)
print(f"Saved {OUT}")
