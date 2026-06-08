"""Build AI theory diagram for G10 — Normal distribution.

Two-panel figure:
LEFT  — N(0,1) density with the 68/95/99.7 (empirical) rule:
        nested shaded bands at +/- 1, 2, 3 sigma with their probability
        masses labelled.
RIGHT — Standardization Z = (X - mu)/sigma: an arbitrary N(mu, sigma^2)
        density mapped via the linear transform onto the standard
        normal, with matched z-scores annotated.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/theory/th_g10_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)


def phi(z, mu=0.0, sigma=1.0):
    return np.exp(-0.5 * ((z - mu) / sigma) ** 2) / (sigma * np.sqrt(2 * np.pi))


fig = plt.figure(figsize=(13.6, 6.2))
gs = fig.add_gridspec(1, 2, width_ratios=[1.05, 1.0], wspace=0.22,
                      left=0.06, right=0.985, top=0.88, bottom=0.12)

# ============================================================
# LEFT — 68 / 95 / 99.7 rule on N(0,1)
# ============================================================
axL = fig.add_subplot(gs[0, 0])

z = np.linspace(-4.2, 4.2, 1200)
fz = phi(z)

# Nested bands: outermost first so inner ones overlay
band_specs = [
    (3, "#d8e4f1", "99.7%"),
    (2, "#aac3df", "95%"),
    (1, "#5fa8d3", "68%"),
]
for k, col, _ in band_specs:
    mask = (z >= -k) & (z <= k)
    axL.fill_between(z[mask], 0, fz[mask], color=col, alpha=0.85, zorder=1)

# Density curve on top
axL.plot(z, fz, color=PALETTE["primary"], lw=2.4, zorder=3)

# Vertical sigma markers
for k in (-3, -2, -1, 1, 2, 3):
    axL.plot([k, k], [0, phi(k)], color=PALETTE["neutral"],
             lw=0.9, ls="--", alpha=0.65, zorder=2)

# Probability mass labels stacked above the peak
peak = phi(0)
label_specs = [
    (0.0, peak * 0.18, r"$68\%$  ($\pm 1\sigma$)",  "white"),
    (0.0, peak * 0.55, r"$95\%$  ($\pm 2\sigma$)",  PALETTE["primary"]),
    (0.0, peak * 0.88, r"$99.7\%$  ($\pm 3\sigma$)", PALETTE["primary"]),
]
for x, y, txt, col in label_specs:
    axL.text(x, y, txt, ha="center", va="center", fontsize=11.5,
             fontweight="bold", color=col, zorder=5)

# x-axis sigma tick labels
axL.set_xticks([-3, -2, -1, 0, 1, 2, 3])
axL.set_xticklabels([r"$\mu-3\sigma$", r"$\mu-2\sigma$", r"$\mu-\sigma$",
                     r"$\mu$", r"$\mu+\sigma$", r"$\mu+2\sigma$",
                     r"$\mu+3\sigma$"], fontsize=9.5)

axL.set_xlim(-4.2, 4.2)
axL.set_ylim(0, peak * 1.18)
axL.set_ylabel(r"$f(x)$")
axL.set_xlabel("x")
axL.set_title("Empirical rule  —  68 / 95 / 99.7  on  $N(\\mu, \\sigma^2)$",
              color=PALETTE["primary"])

# Density formula card
axL.text(-4.05, peak * 1.05,
         r"$f(x)=\dfrac{1}{\sigma\sqrt{2\pi}}\,"
         r"\exp\!\left[-\dfrac{(x-\mu)^2}{2\sigma^2}\right]$",
         ha="left", va="top", fontsize=11, color=PALETTE["neutral"],
         bbox=dict(boxstyle="round,pad=0.35", fc="#fff8d8",
                   ec=PALETTE["accent"], lw=1.0))

# ============================================================
# RIGHT — Standardization Z = (X - mu) / sigma
# ============================================================
axR = fig.add_subplot(gs[0, 1])

mu, sigma = 70.0, 8.0   # e.g. exam score
x = np.linspace(mu - 4 * sigma, mu + 4 * sigma, 1200)
fx = phi(x, mu, sigma)

# Top: N(mu, sigma^2) density
# Bottom: N(0,1) density on the same axes using a second x scale via twin.
# We will draw both stacked using a single axis: offset the lower one.
peakX = phi(mu, mu, sigma)
peakZ = phi(0)

# Normalize visually: scale both densities to the same display height
scaleX = 1.0 / peakX
scaleZ = 1.0 / peakZ

# Plot N(mu, sigma^2) on the top half
y_top_offset = 1.30
axR.plot(x, fx * scaleX + y_top_offset, color=PALETTE["primary"], lw=2.2)
axR.fill_between(x, y_top_offset, fx * scaleX + y_top_offset,
                 color=PALETTE["secondary"], alpha=0.25)

# Plot N(0,1) on the bottom half — but on a transformed x via z = (x-mu)/sigma
z_axis_x = mu + sigma * z   # display in the X coordinate system
axR.plot(z_axis_x, phi(z) * scaleZ, color=PALETTE["primary"], lw=2.2)
axR.fill_between(z_axis_x, 0, phi(z) * scaleZ,
                 color=PALETTE["accent"], alpha=0.25)

# Tick lines linking matching z-scores between the two
for k, col in zip([-2, -1, 0, 1, 2],
                  [PALETTE["warn"], PALETTE["ok"], PALETTE["neutral"],
                   PALETTE["ok"], PALETTE["warn"]]):
    xk = mu + k * sigma
    # vertical guides on each curve at matching points
    axR.plot([xk, xk], [0, phi(k) * scaleZ], color=col, lw=1.0,
             ls="--", alpha=0.7, zorder=2)
    axR.plot([xk, xk],
             [y_top_offset, phi(xk, mu, sigma) * scaleX + y_top_offset],
             color=col, lw=1.0, ls="--", alpha=0.7, zorder=2)
    # connector between matching x and z
    axR.add_patch(FancyArrowPatch(
        (xk, y_top_offset - 0.02), (xk, 1.05),
        arrowstyle="-", color=col, lw=0.9, alpha=0.5, zorder=1))
    # bottom (z) tick label
    axR.text(xk, -0.10, f"z = {k:+d}" if k != 0 else "z = 0",
             ha="center", va="top", fontsize=9.5, color=col)
    # top (x) tick label
    axR.text(xk, y_top_offset - 0.10,
             rf"$x={xk:g}$", ha="center", va="top", fontsize=9.5, color=col)

# Labels for the two densities
axR.text(mu - 4 * sigma + 1.0, y_top_offset + 1.05,
         r"$X \sim N(\mu,\sigma^2)$",
         fontsize=12, fontweight="bold", color=PALETTE["primary"])
axR.text(mu - 4 * sigma + 1.0, 1.05,
         r"$Z \sim N(0,1)$",
         fontsize=12, fontweight="bold", color=PALETTE["primary"])

# Transform arrow + formula between the two panels
axR.add_patch(FancyArrowPatch(
    (mu + 3.0 * sigma, y_top_offset - 0.05),
    (mu + 3.0 * sigma, 1.10),
    arrowstyle="->,head_length=10,head_width=6",
    color=PALETTE["accent"], lw=2.0))
axR.text(mu + 3.2 * sigma, (y_top_offset + 1.05) / 2 + 0.10,
         r"$Z=\dfrac{X-\mu}{\sigma}$",
         ha="left", va="center", fontsize=12.5,
         color=PALETTE["primary"], fontweight="bold",
         bbox=dict(boxstyle="round,pad=0.35", fc="#fff8d8",
                   ec=PALETTE["accent"], lw=1.0))

# Parameters card
axR.text(mu - 4 * sigma + 1.0, -0.45,
         rf"Here  $\mu={mu:g}$,  $\sigma={sigma:g}$  "
         r"$\;\Rightarrow\;$  e.g.  $x=86 \mapsto z=+2$.",
         ha="left", va="center", fontsize=10.5, color=PALETTE["neutral"])

axR.set_xlim(mu - 4.2 * sigma, mu + 4.5 * sigma)
axR.set_ylim(-0.55, y_top_offset + 1.30)
axR.set_yticks([])
axR.set_xticks([])
axR.set_title("Standardization  —  shift by $\\mu$, scale by $\\sigma$",
              color=PALETTE["primary"])
for side in ("top", "right", "left", "bottom"):
    axR.spines[side].set_visible(False)

fig.suptitle("Normal distribution  —  empirical rule and standardization",
             fontsize=14, fontweight="bold", color=PALETTE["primary"],
             y=0.985)

fig.savefig(OUT)
print(f"Saved {OUT}")
