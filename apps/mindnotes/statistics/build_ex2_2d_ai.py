"""Build AI walkthrough plot for Ex 2.2d — third quartile from grouped data
(weekly hours of access to devices, n=300 primary-school pupils).

Also uses shutil to clone the recurring Exercise 2.2 textbook prompt image
into the ex2/questions/ and ex2/answers/ subfolders under the Ex 2.2d
filenames, so the snippet can reference them directly.
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
OUT  = f"{BASE}/ex2_2d_ai.png"

# ---------- 1. Clone the recurring textbook PNGs via shutil ----------
# The Exercise 2.2 prompt image is shared across sub-parts (already saved
# as ex2_1g_question.png and ex2_1g_answer.png in 1g — same source page).
src_q = f"{BASE}/questions/ex2_1g_question.png"
dst_q = f"{BASE}/questions/ex2_2d_question.png"
src_a = f"{BASE}/answers/ex2_1g_answer.png"
dst_a = f"{BASE}/answers/ex2_2d_answer.png"

if os.path.exists(src_q):
    shutil.copyfile(src_q, dst_q)
    print(f"copied -> {dst_q}")
if os.path.exists(src_a):
    shutil.copyfile(src_a, dst_a)
    print(f"copied -> {dst_a}")

# ---------- 2. Build the AI walkthrough figure ----------
# Grouped table from the prompt
lower    = np.array([0.0, 10.0, 25.0, 30.0])
upper    = np.array([10.0, 25.0, 30.0, 40.0])
freq     = np.array([45,   90,   75,   90], dtype=float)
n        = int(freq.sum())                # 300
p        = freq / freq.sum()              # 0.15, 0.30, 0.25, 0.30
width    = upper - lower                  # 10, 15, 5, 10
dens     = p / width                      # 0.015, 0.020, 0.050, 0.030
cum      = np.concatenate(([0.0], np.cumsum(p)))   # 0, 0.15, 0.45, 0.70, 1.00

# Q3 lives in the class containing F = 0.75 -> [30, 40]
F_lower  = cum[3]            # 0.70
d_q3     = dens[3]           # 0.030
Q3       = lower[3] + (0.75 - F_lower) / d_q3      # 31.6667

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.2))

# ---------- LEFT: density histogram with Q3 located on the [30,40] class ----------
for lo, hi, d in zip(lower, upper, dens):
    color = PALETTE["accent"] if lo == 30 else PALETTE["primary"]
    alpha = 0.45 if lo == 30 else 0.22
    ax1.bar(lo, d, width=hi - lo, align="edge",
            color=color, alpha=alpha,
            edgecolor=PALETTE["primary"], linewidth=1.3)

# Shade the area cumulated up to Q3 inside the [30,40] bar
ax1.bar(30, d_q3, width=Q3 - 30, align="edge",
        color=PALETTE["warn"], alpha=0.55,
        edgecolor=PALETTE["warn"], linewidth=1.0,
        label=fr"slice contributing 0.05 = $(Q_3-30)\cdot c_{{[30,40]}}$")

ymax_left = dens.max() * 1.45
ax1.axvline(Q3, color=PALETTE["warn"], lw=2.0, ls="-",
            label=fr"$Q_3 \approx {Q3:.3f}$")
ax1.text(Q3 + 0.4, ymax_left * 0.95, fr"$Q_3 = {Q3:.3f}$",
         color=PALETTE["warn"], fontsize=11, ha="left", va="top",
         fontweight="bold")

# Annotate class densities
for lo, hi, d, pk in zip(lower, upper, dens, p):
    ax1.annotate(f"$c={d:.3f}$\n$p={pk:.2f}$", xy=((lo+hi)/2, d), xytext=(0, 8),
                 textcoords="offset points", ha="center", va="bottom",
                 fontsize=9.5, color=PALETTE["neutral"])

ax1.set_xlim(-1.5, 41.5)
ax1.set_ylim(0, ymax_left)
ax1.set_xlabel("Hours of access to devices per week")
ax1.set_ylabel("Density  $c_k = p_k / w_k$")
ax1.set_title("Q3 by linear interpolation in class $[30,40]$")
ax1.legend(loc="upper left", framealpha=0.95, fontsize=10)

# ---------- RIGHT: cumulative-frequency ogive with Q3 lookup ----------
xs_ogive = np.array([0, 10, 25, 30, 40])
ys_ogive = cum                                    # 0, 0.15, 0.45, 0.70, 1.00
ax2.plot(xs_ogive, ys_ogive, "-o",
         color=PALETTE["primary"], lw=2.2, ms=7,
         markeredgecolor="#2a3142", markerfacecolor=PALETTE["primary"],
         label="cumulative distribution (piecewise-linear)")

# Locate Q3 graphically: horizontal at 0.75 -> hit ogive -> down to x-axis
ax2.axhline(0.75, color=PALETTE["accent"], lw=1.2, ls="--", alpha=0.9)
ax2.axvline(Q3,    color=PALETTE["warn"],   lw=1.6, ls="-")
ax2.scatter([Q3], [0.75], s=110, color=PALETTE["warn"],
            edgecolor="#2a3142", linewidth=0.9, zorder=6)

# Mark the bracket [0.70, 0.75] -> [30, Q3]
ax2.annotate("", xy=(30, 0.75), xytext=(30, 0.70),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["neutral"], lw=1.3))
ax2.text(30 - 0.6, 0.725, r"$0.05$",
         color=PALETTE["neutral"], fontsize=10, ha="right", va="center")
ax2.annotate("", xy=(Q3, 0.70), xytext=(30, 0.70),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["warn"], lw=1.3))
ax2.text((30 + Q3)/2, 0.695, fr"$Q_3-30={Q3-30:.3f}$",
         color=PALETTE["warn"], fontsize=10, ha="center", va="top")

ax2.text(Q3 + 0.4, 0.76, fr"$Q_3 \approx {Q3:.3f}$",
         color=PALETTE["warn"], fontsize=11, ha="left", va="bottom",
         fontweight="bold")
ax2.text(0.4, 0.755, r"$F = 0.75$",
         color=PALETTE["accent"], fontsize=10, ha="left", va="bottom")

# Annotate the cumulative values
for x, y in zip(xs_ogive, ys_ogive):
    ax2.annotate(f"F({int(x)})={y:.2f}", xy=(x, y), xytext=(4, -14),
                 textcoords="offset points", ha="left", va="top",
                 fontsize=9, color=PALETTE["neutral"])

ax2.set_xlim(-1.5, 41.5)
ax2.set_ylim(-0.02, 1.05)
ax2.set_xlabel("Hours of access to devices per week")
ax2.set_ylabel("Cumulative relative frequency  $F(x)$")
ax2.set_title("Linear interpolation: solve $F(Q_3)=0.75$")
ax2.legend(loc="upper left", framealpha=0.95, fontsize=10)

# Summary box: the formula
summary = (
    r"Within $[30,40)$ assume uniform density." + "\n" +
    r"$F(Q_3) = F(30) + (Q_3 - 30)\cdot c_{[30,40]}$" + "\n" +
    fr"$0.75 = 0.70 + (Q_3 - 30)\cdot 0.03$" + "\n" +
    fr"$\Rightarrow\; Q_3 = 30 + 0.05/0.03 = {Q3:.4f}$"
)
ax2.text(0.98, 0.02, summary, transform=ax2.transAxes,
         ha="right", va="bottom", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  Q3 = {Q3:.5f}")
