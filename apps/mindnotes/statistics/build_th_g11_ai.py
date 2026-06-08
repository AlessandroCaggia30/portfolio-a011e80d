"""Build AI theory diagram for G11 — Sampling distributions / CLT.

Four-panel figure illustrating Central Limit Theorem convergence.
Population is a skewed Exponential(rate=1) (mu=1, sigma=1) — visibly
non-normal — and the sample-mean distribution of X-bar is shown for
increasing n. As n grows, the empirical histogram concentrates around mu
and approaches the normal density N(mu, sigma^2/n).

Top-left  : population density (highly skewed)
Top-right : sampling distribution of X-bar, n=10  (still skewed)
Bot-left  : sampling distribution of X-bar, n=30  (close to normal)
Bot-right : sampling distribution of X-bar, n=100 (essentially normal)

Each X-bar panel overlays the simulated histogram with the CLT normal
approximation N(mu, sigma^2/n) and annotates the standard error
sigma/sqrt(n).
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/theory/th_g11_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)


def normal_pdf(x, mu, sd):
    return np.exp(-0.5 * ((x - mu) / sd) ** 2) / (sd * np.sqrt(2 * np.pi))


# Population: Exponential(rate = 1)  =>  mu = 1, sigma = 1.
rng = np.random.default_rng(7)
MU, SIGMA = 1.0, 1.0
N_REP = 20000  # number of replicated samples per panel

fig = plt.figure(figsize=(13.6, 9.6))
gs = fig.add_gridspec(2, 2, hspace=0.42, wspace=0.22,
                      left=0.06, right=0.985, top=0.84, bottom=0.07)

# ------------------------------------------------------------------
# TOP-LEFT — population density (Exponential(1))
# ------------------------------------------------------------------
ax0 = fig.add_subplot(gs[0, 0])
x = np.linspace(0, 6, 600)
pop_pdf = np.exp(-x)
ax0.fill_between(x, 0, pop_pdf, color=PALETTE["secondary"], alpha=0.35)
ax0.plot(x, pop_pdf, color=PALETTE["primary"], lw=2.4)

ax0.axvline(MU, color=PALETTE["warn"], lw=1.4, ls="--")
ax0.text(MU + 0.08, 0.85, rf"$\mu = {MU:g}$", color=PALETTE["warn"],
         fontsize=11, fontweight="bold")
ax0.text(3.2, 0.55,
         "Population:  Exponential(1)\n"
         r"$\mu=1,\; \sigma=1$" "\n"
         "highly skewed,  NOT normal",
         fontsize=10.5, color=PALETTE["neutral"],
         bbox=dict(boxstyle="round,pad=0.4", fc="#fff8d8",
                   ec=PALETTE["accent"], lw=1.0))

ax0.set_xlim(0, 6)
ax0.set_ylim(0, 1.05)
ax0.set_xlabel("x")
ax0.set_ylabel("density")
ax0.set_title("Population distribution of $X$", color=PALETTE["primary"])


# ------------------------------------------------------------------
# Helper for sampling-distribution panels
# ------------------------------------------------------------------
def draw_xbar_panel(ax, n, title_tag):
    # Simulate N_REP sample means, each of size n, from Exp(1)
    samples = rng.exponential(scale=1.0, size=(N_REP, n))
    xbar = samples.mean(axis=1)

    se = SIGMA / np.sqrt(n)
    # x range centred on mu, +/- 4 SE, but clip at 0 (since X >= 0)
    lo = max(0.0, MU - 4 * se)
    hi = MU + 4 * se
    grid = np.linspace(lo, hi, 600)
    clt = normal_pdf(grid, MU, se)

    ax.hist(xbar, bins=60, range=(lo, hi), density=True,
            color=PALETTE["secondary"], alpha=0.55,
            edgecolor="white", linewidth=0.4)
    ax.plot(grid, clt, color=PALETTE["primary"], lw=2.4,
            label=rf"$N(\mu,\sigma^2/n)$")

    # mu reference
    ax.axvline(MU, color=PALETTE["warn"], lw=1.3, ls="--")
    # +/- 1 SE markers on the CLT curve
    for k in (-1, 1):
        xk = MU + k * se
        ax.plot([xk, xk], [0, normal_pdf(xk, MU, se)],
                color=PALETTE["accent"], lw=1.1, ls=":", alpha=0.9)

    peak = normal_pdf(MU, MU, se)
    ax.text(MU + 1.05 * se, peak * 0.55,
            rf"SE $=\dfrac{{\sigma}}{{\sqrt{{n}}}}={se:.3f}$",
            fontsize=10.5, color=PALETTE["primary"],
            bbox=dict(boxstyle="round,pad=0.3", fc="#fff8d8",
                      ec=PALETTE["accent"], lw=0.9))

    ax.set_xlim(lo, hi)
    ax.set_ylim(0, peak * 1.28)
    ax.set_xlabel(r"$\bar x$")
    ax.set_ylabel("density")
    ax.set_title(title_tag, color=PALETTE["primary"])
    ax.legend(loc="upper left", frameon=False, fontsize=10)


# ------------------------------------------------------------------
# TOP-RIGHT — n = 10  (still visibly skewed)
# ------------------------------------------------------------------
ax1 = fig.add_subplot(gs[0, 1])
draw_xbar_panel(ax1, n=10,
                title_tag=r"Sampling distribution of $\bar X$,  $n=10$"
                          "   (CLT not yet kicked in)")

# ------------------------------------------------------------------
# BOTTOM-LEFT — n = 30  (rule of thumb threshold)
# ------------------------------------------------------------------
ax2 = fig.add_subplot(gs[1, 0])
draw_xbar_panel(ax2, n=30,
                title_tag=r"Sampling distribution of $\bar X$,  $n=30$"
                          "   (CLT threshold)")

# ------------------------------------------------------------------
# BOTTOM-RIGHT — n = 100  (essentially normal, narrow)
# ------------------------------------------------------------------
ax3 = fig.add_subplot(gs[1, 1])
draw_xbar_panel(ax3, n=100,
                title_tag=r"Sampling distribution of $\bar X$,  $n=100$"
                          "   (essentially normal)")

# ------------------------------------------------------------------
# Suptitle + take-aways banner
# ------------------------------------------------------------------
fig.suptitle("Central Limit Theorem  —  $\\bar X$ approaches "
             "$N(\\mu,\\sigma^2/n)$ as $n$ grows",
             fontsize=15, fontweight="bold",
             color=PALETTE["primary"], y=0.965)

fig.text(0.5, 0.905,
         r"$E[\bar X]=\mu$  always   $\bullet$   "
         r"$\mathrm{Var}(\bar X)=\sigma^2/n$  shrinks with  $n$   $\bullet$   "
         r"shape  $\to$  normal  regardless of the population",
         ha="center", va="center", fontsize=11.5,
         color=PALETTE["neutral"])

fig.savefig(OUT)
print(f"Saved {OUT}")
