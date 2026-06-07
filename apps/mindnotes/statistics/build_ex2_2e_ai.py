"""Build AI walkthrough plot for Ex 2.2e — Interquartile range from grouped data
(weekly hours of access to devices, n=300 primary-school pupils).

Two-panel figure:
  LEFT  — density histogram with the IQR band shaded between Q1 and Q3.
  RIGHT — empirical CDF (piecewise linear under uniform-on-class assumption)
          with the geometric interpolation construction for Q1 and Q3.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_2e_ai.png"

# --- Grouped table from the prompt ---
lower    = np.array([0.0, 10.0, 25.0, 30.0])
upper    = np.array([10.0, 25.0, 30.0, 40.0])
freq     = np.array([45,   90,   75,   90], dtype=float)
n        = int(freq.sum())            # 300
p        = freq / freq.sum()          # rel.freq:  0.15, 0.30, 0.25, 0.30
width    = upper - lower              # 10, 15, 5, 10
dens     = p / width                  # 0.015, 0.020, 0.050, 0.030
F_lower  = np.concatenate(([0.0], np.cumsum(p)))   # 0, 0.15, 0.45, 0.70, 1.00

# --- Quartiles by linear interpolation in the class containing F = 0.25, 0.75 ---
# Q1 lives in [10, 25):   F(10) = 0.15,  F(25) = 0.45
Q1 = 10.0 + (0.25 - 0.15) / 0.02       # = 15
# Q3 lives in [30, 40]:   F(30) = 0.70,  F(40) = 1.00
Q3 = 30.0 + (0.75 - 0.70) / 0.03       # = 31.667
IQR = Q3 - Q1                          # = 16.667

# --- Figure: two panels ---
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.4))

# ---------- LEFT: density histogram, IQR band, Q1/Q3 marked ----------
for lo, hi, d in zip(lower, upper, dens):
    ax1.bar(lo, d, width=hi - lo, align="edge",
            color=PALETTE["primary"], alpha=0.28,
            edgecolor=PALETTE["primary"], linewidth=1.3)

ymax_left = dens.max() * 1.45
ax1.axvspan(Q1, Q3, color=PALETTE["accent"], alpha=0.18,
            label=fr"central 50 %: $[Q_1,Q_3]$")
ax1.axvline(Q1, color=PALETTE["warn"], lw=2.0, ls="-",
            label=fr"$Q_1 = {Q1:.3f}$")
ax1.axvline(Q3, color=PALETTE["ok"], lw=2.0, ls="-",
            label=fr"$Q_3 = {Q3:.3f}$")
ax1.text(Q1, ymax_left * 0.92, f"  $Q_1={Q1:.0f}$",
         color=PALETTE["warn"], fontsize=11, ha="left", va="top", fontweight="bold")
ax1.text(Q3, ymax_left * 0.92, f"  $Q_3={Q3:.3f}$",
         color=PALETTE["ok"], fontsize=11, ha="left", va="top", fontweight="bold")
ax1.annotate(
    "", xy=(Q3, ymax_left * 0.45), xytext=(Q1, ymax_left * 0.45),
    arrowprops=dict(arrowstyle="<->", color="#7a6700", lw=1.6),
)
ax1.text((Q1 + Q3) / 2.0, ymax_left * 0.49,
         fr"$\mathrm{{IQR}} = Q_3 - Q_1 = {IQR:.3f}$",
         color="#7a6700", fontsize=11.5, ha="center", va="bottom", fontweight="bold")

# Class labels above each bar
for lo, hi, d, pk in zip(lower, upper, dens, p):
    ax1.text((lo + hi) / 2.0, d + ymax_left * 0.02,
             f"$p={pk:.2f}$", ha="center", va="bottom",
             fontsize=9.5, color=PALETTE["neutral"])

ax1.set_xlim(-1.5, 41.5)
ax1.set_ylim(0, ymax_left)
ax1.set_xlabel("Hours of access to devices per week")
ax1.set_ylabel("Density  $c_k = p_k / w_k$")
ax1.set_title("Histogram with $Q_1$, $Q_3$ and the $IQR$ band")
ax1.legend(loc="upper left", framealpha=0.95, fontsize=9.5)

# ---------- RIGHT: empirical CDF + interpolation construction ----------
# Piecewise-linear CDF under within-class uniform assumption
xs = np.array([0, 10, 25, 30, 40], dtype=float)
Fs = np.array([0, 0.15, 0.45, 0.70, 1.00])
ax2.plot(xs, Fs, color=PALETTE["primary"], lw=2.2, label="empirical CDF")
ax2.scatter(xs, Fs, color=PALETTE["primary"], s=42, zorder=5)

# Horizontal lines at 0.25 and 0.75 cutting the CDF
ax2.axhline(0.25, color=PALETTE["warn"], lw=1.2, ls="--", alpha=0.85)
ax2.axhline(0.75, color=PALETTE["ok"],   lw=1.2, ls="--", alpha=0.85)
ax2.text(40.5, 0.25, " $F=0.25$", color=PALETTE["warn"], va="center", fontsize=10)
ax2.text(40.5, 0.75, " $F=0.75$", color=PALETTE["ok"],   va="center", fontsize=10)

# Drop verticals to x-axis at Q1, Q3
ax2.vlines(Q1, 0, 0.25, color=PALETTE["warn"], lw=1.4, ls=":")
ax2.vlines(Q3, 0, 0.75, color=PALETTE["ok"],   lw=1.4, ls=":")
ax2.scatter([Q1, Q3], [0.25, 0.75],
            color=[PALETTE["warn"], PALETTE["ok"]],
            s=70, zorder=6, edgecolor="#2a3142", linewidth=0.9)
ax2.text(Q1, -0.045, f"$Q_1={Q1:.0f}$", color=PALETTE["warn"],
         ha="center", va="top", fontsize=10.5, fontweight="bold")
ax2.text(Q3, -0.045, f"$Q_3={Q3:.3f}$", color=PALETTE["ok"],
         ha="center", va="top", fontsize=10.5, fontweight="bold")

# Annotate the two interpolation formulas
ax2.text(13.5, 0.62,
         r"$Q_1 = 10 + \dfrac{0.25 - 0.15}{0.02} = 15$",
         color=PALETTE["warn"], fontsize=10.5,
         bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.35", linewidth=0.9))
ax2.text(13.5, 0.40,
         r"$Q_3 = 30 + \dfrac{0.75 - 0.70}{0.03} = 31.667$",
         color=PALETTE["ok"], fontsize=10.5,
         bbox=dict(facecolor="#eaf6ee", edgecolor=PALETTE["ok"],
                   boxstyle="round,pad=0.35", linewidth=0.9))
ax2.text(13.5, 0.20,
         fr"$\mathrm{{IQR}} = Q_3 - Q_1 = {IQR:.3f}$",
         color="#7a6700", fontsize=11, fontweight="bold",
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.35", linewidth=1.0))

ax2.set_xlim(-1.5, 41.5)
ax2.set_ylim(-0.02, 1.05)
ax2.set_xlabel("Hours of access to devices per week")
ax2.set_ylabel(r"$F(x)$  (cumulative relative frequency)")
ax2.set_title(r"Empirical CDF — linear interpolation for $Q_1$ and $Q_3$")
ax2.legend(loc="lower right", framealpha=0.95, fontsize=10)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  Q1  = {Q1:.6f}")
print(f"  Q3  = {Q3:.6f}")
print(f"  IQR = {IQR:.6f}")
