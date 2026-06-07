"""Build AI walkthrough plot for Ex 2.7b — 20th percentile of Nr_visits.

Visual: two-panel figure that makes the quantile reading on the empirical
ogive concrete.

LEFT panel
  Step-style empirical CDF (ogive) of Nr_visits with:
    - horizontal dashed guide at F = 0.20,
    - vertical dashed guide at x = 2 (where the step first reaches >= 0.20),
    - shaded sky-blue region [min, p20] = the bottom 20% of customers,
    - annotated tick on the x-axis under p20 = 2.

RIGHT panel
  Cumulative-frequency table card with the row that crosses 0.20
  highlighted, and a short formula card:
      p20 = min{ x : F_n(x) >= 0.20 }  =  2 visits.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_7b_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# --- Nr_visits distribution from the prompt (2200 customers) ---
x      = np.array([1, 2, 3, 4, 6, 8, 9, 10, 12, 14, 15, 16, 20, 24])
counts = np.array([193, 272, 138, 115, 92, 60, 118, 228, 309, 130, 113, 104, 122, 206])
n      = counts.sum()                          # 2200
prop   = counts / n                            # relative frequencies
cum    = np.cumsum(prop)                       # cumulative relative freq

# Find p20 = smallest x with cum >= 0.20
target = 0.20
idx_p20 = int(np.argmax(cum >= target))
p20 = int(x[idx_p20])
print(f"  n={n}  p20 index={idx_p20}  x[idx]={p20}  cum[idx]={cum[idx_p20]:.4f}")

# =====================================================================
# Layout
# =====================================================================
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.4),
                               gridspec_kw={"width_ratios": [2.2, 1]})

# =====================================================================
# LEFT — empirical CDF / ogive with quantile reading
# =====================================================================
# Build step coordinates: start at (x[0], 0) then jump to cum at each x
xs_step = np.concatenate([[x[0] - 0.5, x[0]], x])
ys_step = np.concatenate([[0.0,         0.0], cum])

ax1.step(xs_step, ys_step, where="post",
         color=PALETTE["primary"], lw=1.9, label="Empirical CDF $F_n$")
ax1.scatter(x, cum, color=PALETTE["primary"], s=22, zorder=4)

# Shaded "bottom 20%" region on the x-axis (up to p20)
ax1.axvspan(x[0] - 0.5, p20,
            color=PALETTE["secondary"], alpha=0.18,
            label="bottom 20% of customers")

# Horizontal guide at F = 0.20
ax1.axhline(target, color=PALETTE["accent"], lw=1.4, ls="--",
            label=fr"$F = {target:.2f}$")
# Vertical guide at x = p20
ax1.axvline(p20, color=PALETTE["ok"], lw=1.4, ls="--",
            label=fr"$p_{{20}} = {p20}$ visits")

# Mark the crossing point
ax1.scatter([p20], [cum[idx_p20]], s=85, zorder=5,
            color=PALETTE["accent"], edgecolor=PALETTE["primary"],
            linewidth=1.2)
ax1.annotate(fr"$F_n({p20}) = {cum[idx_p20]:.3f} \geq 0.20$",
             xy=(p20, cum[idx_p20]),
             xytext=(p20 + 3.5, cum[idx_p20] - 0.05),
             fontsize=10, color=PALETTE["primary"],
             arrowprops=dict(arrowstyle="->", color=PALETTE["primary"],
                             lw=1.0))

# x-axis tag under p20
ax1.text(p20, -0.06, fr"$p_{{20}}={p20}$",
         ha="center", va="top", fontsize=10.5,
         color=PALETTE["ok"], fontweight="bold")

ax1.set_xlim(0, 25)
ax1.set_ylim(0, 1.05)
ax1.set_xlabel("Nr_visits")
ax1.set_ylabel("Cumulative proportion  $F_n(x)$")
ax1.set_title("Ex 2.7b — reading $p_{20}$ off the ogive of Nr_visits")
ax1.legend(loc="lower right", framealpha=0.95, fontsize=9.5)

# =====================================================================
# RIGHT — cumulative-frequency table + formula card
# =====================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)
ax2.add_patch(FancyBboxPatch((0.02, 0.04), 0.96, 0.92,
                             boxstyle="round,pad=0.02",
                             linewidth=1.1,
                             facecolor="#fbfbfd",
                             edgecolor=PALETTE["primary"]))

# Header
ax2.text(0.50, 0.94, "Cumulative relative frequencies",
         ha="center", va="top", fontsize=11,
         color=PALETTE["primary"], fontweight="bold")

# Build a compact table: only first 7 values fit cleanly
show_x   = x[:7]
show_cum = cum[:7]
col_y    = 0.86
row_dy   = 0.055
header_x = 0.07
val_x    = 0.55

ax2.text(header_x, col_y, "Nr_visits",
         ha="left", va="center", fontsize=10,
         color=PALETTE["primary"], fontweight="bold")
ax2.text(val_x, col_y, "cum. prop.",
         ha="left", va="center", fontsize=10,
         color=PALETTE["primary"], fontweight="bold")

for i, (xv, cv) in enumerate(zip(show_x, show_cum)):
    yy = col_y - (i + 1) * row_dy
    # Highlight the p20 row
    is_p20_row = (xv == p20)
    if is_p20_row:
        ax2.add_patch(FancyBboxPatch((0.05, yy - 0.022), 0.90, 0.044,
                                     boxstyle="round,pad=0.005",
                                     linewidth=0.0,
                                     facecolor=PALETTE["secondary"],
                                     alpha=0.30))
    ax2.text(header_x, yy, f"{int(xv)}",
             ha="left", va="center", fontsize=10,
             color=PALETTE["primary"],
             fontweight="bold" if is_p20_row else "normal")
    ax2.text(val_x, yy, f"{cv:.3f}",
             ha="left", va="center", fontsize=10,
             color=PALETTE["primary"],
             fontweight="bold" if is_p20_row else "normal")
    if is_p20_row:
        ax2.text(0.93, yy, fr"$\geq 0.20$",
                 ha="right", va="center", fontsize=9.5,
                 color=PALETTE["ok"], fontweight="bold")

# Formula card at the bottom
ax2.text(0.50, 0.31,
         r"$p_{20} \;=\; \min\{\, x : F_n(x) \geq 0.20 \,\}$",
         ha="center", va="center", fontsize=11,
         color=PALETTE["primary"])
ax2.text(0.50, 0.23,
         fr"$\Rightarrow\; p_{{20}} = {p20}$ visits",
         ha="center", va="center", fontsize=11.5,
         color=PALETTE["ok"], fontweight="bold")

# R one-liner
ax2.text(0.50, 0.11,
         'R:  quantile(rep(x,counts), 0.20, type=1)\n'
         '##  20%\n'
         '##   2',
         ha="center", va="center", fontsize=8.8, family="monospace",
         color=PALETTE["neutral"])

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
