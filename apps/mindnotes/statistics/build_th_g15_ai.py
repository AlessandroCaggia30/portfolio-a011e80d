"""Build AI theory diagram for G15 — Linear regression (simple + multiple).

Two-panel figure:
LEFT  — Simple OLS: scatter of (x, y), fitted line y = b0 + b1*x with
        residual segments dropped from each point, plus a corner gauge
        showing the R^2 decomposition SST = SS_reg + SSE.
RIGHT — Multivariate extension: 3-D regression plane y = b0 + b1*x1 + b2*x2
        fitted to a synthetic point cloud, with residual stems and a small
        coefficient/interpretation card.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D  # noqa: F401
from matplotlib.patches import FancyBboxPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/theory/th_g15_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

rng = np.random.default_rng(7)

# ============================================================
# LEFT — Simple OLS scatter + line + residuals + R^2 gauge
# ============================================================
fig = plt.figure(figsize=(14.0, 6.3))
gs = fig.add_gridspec(1, 2, width_ratios=[1.05, 1.0], wspace=0.20,
                      left=0.06, right=0.985, top=0.88, bottom=0.10)

axL = fig.add_subplot(gs[0, 0])

n = 22
beta0_true, beta1_true = 2.0, 1.4
x = np.linspace(1.0, 9.0, n)
y = beta0_true + beta1_true * x + rng.normal(0, 1.4, size=n)

# OLS
xbar, ybar = x.mean(), y.mean()
b1 = np.sum((x - xbar) * (y - ybar)) / np.sum((x - xbar) ** 2)
b0 = ybar - b1 * xbar
yhat = b0 + b1 * x

SST = np.sum((y - ybar) ** 2)
SSR = np.sum((yhat - ybar) ** 2)
SSE = np.sum((y - yhat) ** 2)
R2 = 1 - SSE / SST

# residual segments
for xi, yi, yhi in zip(x, y, yhat):
    col = PALETTE["warn"] if yi < yhi else PALETTE["ok"]
    axL.plot([xi, xi], [yi, yhi], color=col, lw=1.1, alpha=0.75, zorder=2)

# mean of y line
axL.axhline(ybar, color=PALETTE["neutral"], lw=0.9, ls=":", alpha=0.7,
            zorder=1)
axL.text(9.05, ybar, r"$\bar y$", fontsize=10, va="center",
         color=PALETTE["neutral"])

# scatter
axL.scatter(x, y, s=46, color=PALETTE["secondary"],
            edgecolor=PALETTE["primary"], linewidth=1.1, zorder=4)

# fitted line
xx = np.linspace(0.5, 9.6, 100)
axL.plot(xx, b0 + b1 * xx, color=PALETTE["primary"], lw=2.4, zorder=3,
         label=rf"$\hat y={b0:.2f}+{b1:.2f}\,x$")

# annotation card with formulas
axL.text(0.7, max(y) + 0.6,
         r"$\hat\beta_1=\dfrac{\sum(x_i-\bar x)(y_i-\bar y)}"
         r"{\sum(x_i-\bar x)^2}$"
         "\n"
         r"$\hat\beta_0=\bar y-\hat\beta_1\bar x$",
         ha="left", va="top", fontsize=10.5, color=PALETTE["primary"],
         bbox=dict(boxstyle="round,pad=0.35", fc="#fff8d8",
                   ec=PALETTE["accent"], lw=1.0))

# residual legend dots
axL.scatter([], [], color=PALETTE["ok"],
            label=r"residual $y_i-\hat y_i>0$")
axL.scatter([], [], color=PALETTE["warn"],
            label=r"residual $y_i-\hat y_i<0$")
axL.legend(loc="lower right", fontsize=9, frameon=True)

axL.set_xlabel("x")
axL.set_ylabel("y")
axL.set_title("Simple OLS  —  scatter, fit, residuals",
              color=PALETTE["primary"])
axL.set_xlim(0.3, 9.8)

# ---- R^2 decomposition gauge (inset bar) ----
gauge_x0, gauge_y0 = 5.6, min(y) - 0.4
gauge_w = 3.6
gauge_h = 0.55
# SS_reg (left) + SSE (right) = SST
frac_reg = SSR / SST
axL.add_patch(FancyBboxPatch((gauge_x0, gauge_y0), gauge_w * frac_reg,
                              gauge_h, boxstyle="round,pad=0.01",
                              fc=PALETTE["ok"], ec=PALETTE["primary"],
                              lw=0.8))
axL.add_patch(FancyBboxPatch((gauge_x0 + gauge_w * frac_reg, gauge_y0),
                              gauge_w * (1 - frac_reg), gauge_h,
                              boxstyle="round,pad=0.01",
                              fc=PALETTE["warn"], ec=PALETTE["primary"],
                              lw=0.8))
axL.text(gauge_x0 + gauge_w * frac_reg / 2, gauge_y0 + gauge_h / 2,
         f"SS$_{{reg}}$\n{SSR:.1f}",
         ha="center", va="center", fontsize=8.5, color="white",
         fontweight="bold")
axL.text(gauge_x0 + gauge_w * frac_reg + gauge_w * (1 - frac_reg) / 2,
         gauge_y0 + gauge_h / 2, f"SSE\n{SSE:.1f}",
         ha="center", va="center", fontsize=8.5, color="white",
         fontweight="bold")
axL.text(gauge_x0 + gauge_w / 2, gauge_y0 + gauge_h + 0.15,
         rf"$R^2=1-\dfrac{{SSE}}{{SST}}={R2:.2f}$",
         ha="center", va="bottom", fontsize=10.5,
         color=PALETTE["primary"], fontweight="bold")
axL.text(gauge_x0, gauge_y0 - 0.25,
         rf"SST $={SST:.1f}$", fontsize=9, color=PALETTE["neutral"])

# ============================================================
# RIGHT — Multivariate extension: 3D plane y = b0 + b1*x1 + b2*x2
# ============================================================
axR = fig.add_subplot(gs[0, 1], projection="3d")

n2 = 35
x1 = rng.uniform(0, 6, n2)
x2 = rng.uniform(0, 5, n2)
beta = (1.0, 0.9, -0.6)
y2 = beta[0] + beta[1] * x1 + beta[2] * x2 + rng.normal(0, 0.7, size=n2)

# OLS on design
X = np.column_stack([np.ones(n2), x1, x2])
bhat, *_ = np.linalg.lstsq(X, y2, rcond=None)
y2hat = X @ bhat

# plane mesh
g1, g2 = np.meshgrid(np.linspace(0, 6, 12), np.linspace(0, 5, 12))
gy = bhat[0] + bhat[1] * g1 + bhat[2] * g2
axR.plot_surface(g1, g2, gy, alpha=0.30, color=PALETTE["secondary"],
                 edgecolor=PALETTE["primary"], linewidth=0.4)

# residual stems
for a, b, yi, yhi in zip(x1, x2, y2, y2hat):
    col = PALETTE["warn"] if yi < yhi else PALETTE["ok"]
    axR.plot([a, a], [b, b], [yi, yhi], color=col, lw=0.9, alpha=0.8)

# points
above = y2 >= y2hat
axR.scatter(x1[above], x2[above], y2[above], s=28, color=PALETTE["ok"],
            edgecolor=PALETTE["primary"], linewidth=0.7, depthshade=True)
axR.scatter(x1[~above], x2[~above], y2[~above], s=28, color=PALETTE["warn"],
            edgecolor=PALETTE["primary"], linewidth=0.7, depthshade=True)

axR.set_xlabel(r"$x_1$", labelpad=-4)
axR.set_ylabel(r"$x_2$", labelpad=-4)
axR.set_zlabel(r"$y$", labelpad=-6)
axR.tick_params(axis="both", which="major", labelsize=8, pad=-2)
axR.view_init(elev=22, azim=-58)
axR.set_title("Multivariate OLS  —  regression plane in $\\mathbb{R}^3$",
              color=PALETTE["primary"], pad=2)

# coefficient card via 2D overlay
SSE2 = np.sum((y2 - y2hat) ** 2)
SST2 = np.sum((y2 - y2.mean()) ** 2)
R2m = 1 - SSE2 / SST2
R2adj = 1 - (SSE2 / (n2 - 2 - 1)) / (SST2 / (n2 - 1))

fig.text(0.985, 0.10,
         rf"$\hat y={bhat[0]:.2f}+{bhat[1]:.2f}\,x_1{bhat[2]:+.2f}\,x_2$"
         "\n"
         rf"$R^2={R2m:.2f}\quad R^2_{{adj}}={R2adj:.2f}$"
         "\n"
         rf"$n={n2},\;k=2,\;df=n-k-1={n2-3}$",
         ha="right", va="bottom", fontsize=10, color=PALETTE["primary"],
         bbox=dict(boxstyle="round,pad=0.4", fc="#fff8d8",
                   ec=PALETTE["accent"], lw=1.0))

fig.suptitle("Linear regression  —  simple OLS and multivariate extension",
             fontsize=14, fontweight="bold", color=PALETTE["primary"],
             y=0.985)

fig.savefig(OUT, dpi=140)
print(f"Saved {OUT}")
