"""Build AI theory diagram for G12 — Linear combinations of normals.

Two-panel figure illustrating how Var(X +/- Y) depends on Cov(X, Y):
LEFT  — X, Y independent normals; show densities of X, Y, and the
        sum S = X + Y and difference D = X - Y. Both S and D have the
        same variance Var(X) + Var(Y) because Cov(X, Y) = 0.
RIGHT — X, Y jointly normal with rho = 0.7 (positive correlation):
        the sum has inflated variance Var(X) + Var(Y) + 2 Cov,
        the difference has shrunken variance Var(X) + Var(Y) - 2 Cov.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/theory/th_g12_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)


def phi(x, mu=0.0, sigma=1.0):
    return np.exp(-0.5 * ((x - mu) / sigma) ** 2) / (sigma * np.sqrt(2 * np.pi))


# Parameters shared across both panels
muX, muY = 0.0, 0.0
sX, sY = 1.0, 1.5
varX, varY = sX ** 2, sY ** 2

fig = plt.figure(figsize=(13.8, 6.4))
gs = fig.add_gridspec(1, 2, width_ratios=[1.0, 1.0], wspace=0.20,
                      left=0.06, right=0.985, top=0.86, bottom=0.13)

# ============================================================
# LEFT — Independent case: Cov = 0
# ============================================================
axL = fig.add_subplot(gs[0, 0])

cov_ind = 0.0
var_sum_ind = varX + varY + 2 * cov_ind
var_dif_ind = varX + varY - 2 * cov_ind
s_sum_ind = np.sqrt(var_sum_ind)
s_dif_ind = np.sqrt(var_dif_ind)

x = np.linspace(-7, 7, 1200)

# Plot X and Y marginals
axL.plot(x, phi(x, muX, sX), color=PALETTE["primary"], lw=2.0,
         label=rf"$X\sim N(0,{varX:g})$")
axL.fill_between(x, 0, phi(x, muX, sX), color=PALETTE["primary"], alpha=0.10)

axL.plot(x, phi(x, muY, sY), color=PALETTE["accent"], lw=2.0,
         label=rf"$Y\sim N(0,{varY:g})$")
axL.fill_between(x, 0, phi(x, muY, sY), color=PALETTE["accent"], alpha=0.10)

# Plot S = X + Y and D = X - Y (identical variance under independence)
axL.plot(x, phi(x, 0, s_sum_ind), color=PALETTE["secondary"], lw=2.6,
         label=rf"$X+Y\sim N(0,{var_sum_ind:g})$")
axL.plot(x, phi(x, 0, s_dif_ind), color=PALETTE["warn"], lw=2.0, ls="--",
         label=rf"$X-Y\sim N(0,{var_dif_ind:g})$")

axL.set_xlim(-7, 7)
axL.set_ylim(0, 0.50)
axL.set_xlabel("value")
axL.set_ylabel(r"$f(\cdot)$")
axL.set_title(r"Independent:  $\mathrm{Cov}(X,Y)=0$",
              color=PALETTE["primary"])
axL.legend(loc="upper right", fontsize=9.5, frameon=False)

# Variance formula card
axL.text(-6.8, 0.46,
         r"$\mathrm{Var}(X\pm Y)=\mathrm{Var}(X)+\mathrm{Var}(Y)"
         r"\pm 2\,\mathrm{Cov}(X,Y)$" + "\n"
         rf"$={varX:g}+{varY:g}\pm 0 = {var_sum_ind:g}$",
         ha="left", va="top", fontsize=10.5, color=PALETTE["neutral"],
         bbox=dict(boxstyle="round,pad=0.35", fc="#fff8d8",
                   ec=PALETTE["accent"], lw=1.0))

# ============================================================
# RIGHT — Correlated case: rho = 0.7
# ============================================================
axR = fig.add_subplot(gs[0, 1])

rho = 0.7
cov_dep = rho * sX * sY
var_sum_dep = varX + varY + 2 * cov_dep
var_dif_dep = varX + varY - 2 * cov_dep
s_sum_dep = np.sqrt(var_sum_dep)
s_dif_dep = np.sqrt(var_dif_dep)

axR.plot(x, phi(x, muX, sX), color=PALETTE["primary"], lw=2.0,
         label=rf"$X\sim N(0,{varX:g})$")
axR.fill_between(x, 0, phi(x, muX, sX), color=PALETTE["primary"], alpha=0.10)

axR.plot(x, phi(x, muY, sY), color=PALETTE["accent"], lw=2.0,
         label=rf"$Y\sim N(0,{varY:g})$")
axR.fill_between(x, 0, phi(x, muY, sY), color=PALETTE["accent"], alpha=0.10)

# S = X + Y wider (variance inflated)
axR.plot(x, phi(x, 0, s_sum_dep), color=PALETTE["secondary"], lw=2.6,
         label=rf"$X+Y\sim N(0,{var_sum_dep:.2f})$")
# D = X - Y narrower (variance reduced)
axR.plot(x, phi(x, 0, s_dif_dep), color=PALETTE["warn"], lw=2.0, ls="--",
         label=rf"$X-Y\sim N(0,{var_dif_dep:.2f})$")

axR.set_xlim(-7, 7)
axR.set_ylim(0, 0.50)
axR.set_xlabel("value")
axR.set_ylabel(r"$f(\cdot)$")
axR.set_title(rf"Positively correlated:  $\rho={rho:g}$,  "
              rf"$\mathrm{{Cov}}(X,Y)={cov_dep:g}$",
              color=PALETTE["primary"])
axR.legend(loc="upper right", fontsize=9.5, frameon=False)

# Variance formula card
axR.text(-6.8, 0.46,
         rf"$\mathrm{{Var}}(X+Y)={varX:g}+{varY:g}+2\cdot{cov_dep:g}"
         rf"={var_sum_dep:.2f}$" + "\n"
         rf"$\mathrm{{Var}}(X-Y)={varX:g}+{varY:g}-2\cdot{cov_dep:g}"
         rf"={var_dif_dep:.2f}$",
         ha="left", va="top", fontsize=10.5, color=PALETTE["neutral"],
         bbox=dict(boxstyle="round,pad=0.35", fc="#fff8d8",
                   ec=PALETTE["accent"], lw=1.0))

# Annotate the spread comparison with a small note
axR.text(0, 0.02,
         r"sum spreads out, difference tightens",
         ha="center", va="bottom", fontsize=9.5,
         color=PALETTE["neutral"], style="italic")

fig.suptitle(r"Linear combinations of normals  —  $X\pm Y$ and the role of "
             r"$\mathrm{Cov}(X,Y)$",
             fontsize=14, fontweight="bold", color=PALETTE["primary"],
             y=0.975)

fig.savefig(OUT, dpi=150)
print(f"Saved {OUT}")
