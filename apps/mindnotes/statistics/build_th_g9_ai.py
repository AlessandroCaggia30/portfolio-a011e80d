"""Build AI theory diagram for G9 — Covariance and correlation.

Five-panel scatterplot grid showing how the Pearson correlation r
varies with the cloud's geometry:
    r = +0.95, +0.50, 0.00, -0.50, -0.95.

For each panel we draw the data cloud, the OLS fit, and annotate the
computed sample covariance and correlation alongside their formulas.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/theory/th_g9_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# ---------- sample generator with a target correlation ----------
def sample_with_r(r_target, n=120, sx=1.0, sy=1.0, seed=0):
    rng = np.random.default_rng(seed)
    z1 = rng.standard_normal(n)
    z2 = rng.standard_normal(n)
    # Construct y with exact target correlation to z1
    if r_target == 0:
        x = z1
        y = z2
    else:
        x = z1
        y = r_target * z1 + np.sqrt(max(1 - r_target**2, 0)) * z2
    # Standardise then rescale
    x = (x - x.mean()) / x.std(ddof=1) * sx
    y = (y - y.mean()) / y.std(ddof=1) * sy
    return x, y

def cov_cor(x, y):
    n = len(x)
    cov = np.sum((x - x.mean()) * (y - y.mean())) / (n - 1)
    cor = cov / (x.std(ddof=1) * y.std(ddof=1))
    return cov, cor

# ---------- target r values ----------
targets = [
    (+0.95, "Strong positive",  PALETTE["ok"]),
    (+0.50, "Moderate positive", PALETTE["secondary"]),
    ( 0.00, "No linear assoc.",  PALETTE["muted"]),
    (-0.50, "Moderate negative", PALETTE["accent"]),
    (-0.95, "Strong negative",   PALETTE["warn"]),
]

# ---------- figure layout: top row 5 scatters, bottom strip formulas ----------
fig = plt.figure(figsize=(15.0, 6.6))
gs = fig.add_gridspec(2, 5, height_ratios=[1.0, 0.30], hspace=0.55,
                      wspace=0.30, left=0.045, right=0.985,
                      top=0.89, bottom=0.05)

for i, (r_t, label, color) in enumerate(targets):
    ax = fig.add_subplot(gs[0, i])
    x, y = sample_with_r(r_t, seed=10 + i)
    cov, cor = cov_cor(x, y)

    ax.scatter(x, y, s=22, color=color, alpha=0.75,
               edgecolor="white", linewidth=0.6, zorder=3)

    # OLS line (only when |r| > 0.1, otherwise it's noisy)
    if abs(cor) > 0.1:
        slope = cov / x.var(ddof=1)
        intercept = y.mean() - slope * x.mean()
        xline = np.array([x.min() - 0.2, x.max() + 0.2])
        ax.plot(xline, intercept + slope * xline, "-",
                color=PALETTE["primary"], lw=2.0, zorder=4,
                label="OLS fit")
        ax.legend(loc="upper left", frameon=True, fancybox=True,
                  framealpha=0.92, fontsize=9)

    # Means as crosshair
    ax.axvline(x.mean(), color=PALETTE["grid"], lw=0.8, zorder=1)
    ax.axhline(y.mean(), color=PALETTE["grid"], lw=0.8, zorder=1)

    ax.set_title(f"$r \\approx {cor:+.2f}$  —  {label}",
                 color=color, fontsize=12)
    ax.set_xlabel("X")
    if i == 0:
        ax.set_ylabel("Y")
    ax.set_xlim(-3.4, 3.4)
    ax.set_ylim(-3.4, 3.4)
    ax.set_xticks([-2, 0, 2])
    ax.set_yticks([-2, 0, 2])

    # Numeric badge: cov + cor
    badge = (f"$\\widehat{{\\mathrm{{Cov}}}} = {cov:+.2f}$\n"
             f"$\\widehat{{r}} = {cor:+.2f}$")
    ax.text(0.97, 0.03, badge, transform=ax.transAxes,
            ha="right", va="bottom", fontsize=10,
            color=PALETTE["neutral"],
            bbox=dict(boxstyle="round,pad=0.35",
                      fc="#fff8d8", ec=PALETTE["accent"], lw=0.9))

# ---------- formulas strip ----------
ax_f = fig.add_subplot(gs[1, :])
ax_f.axis("off")
ax_f.add_patch(FancyBboxPatch((0.005, 0.05), 0.99, 0.92,
                              boxstyle="round,pad=0.02",
                              transform=ax_f.transAxes,
                              fc="#f6f7fb", ec=PALETTE["primary"],
                              lw=1.0, zorder=1))

ax_f.text(0.25, 0.62,
          r"$\mathrm{Cov}(X,Y) = \dfrac{1}{n-1}\sum_{i=1}^{n}(x_i-\bar x)(y_i-\bar y)$",
          ha="center", va="center", fontsize=14,
          color=PALETTE["primary"], transform=ax_f.transAxes)

ax_f.text(0.75, 0.62,
          r"$r \;=\; \rho_{X,Y} \;=\; \dfrac{\mathrm{Cov}(X,Y)}{\sigma_X\,\sigma_Y} \;\in\; [-1,\,+1]$",
          ha="center", va="center", fontsize=14,
          color=PALETTE["primary"], transform=ax_f.transAxes)

ax_f.text(0.5, 0.18,
          r"Sign $\Rightarrow$ direction · Magnitude of $r$ $\Rightarrow$ "
          r"strength of the $\mathbf{linear}$ association · "
          r"$r=0$ does $\mathbf{not}$ imply independence (non-linear ties).",
          ha="center", va="center", fontsize=10.5,
          color=PALETTE["neutral"], transform=ax_f.transAxes,
          style="italic")

fig.suptitle("Covariance and Pearson correlation — visual catalogue across five values of $r$",
             fontsize=14, fontweight="bold", color=PALETTE["primary"],
             y=0.975)

fig.savefig(OUT)
print(f"Saved {OUT}")
