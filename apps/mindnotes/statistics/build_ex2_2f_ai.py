"""Build AI walkthrough plot for Ex 2.2f — compare SD and CV between primary
and middle school pupils' weekly hours of access to entertainment devices."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_2f_ai.png"

# --- Inputs from prompt / point c ---
# Primary school (from Ex 2.2c, grouped data with n/(n-1) correction)
n_p     = 300
xbar_p  = 23.375
var_p   = 106.1507
sd_p    = float(np.sqrt(var_p))   # 10.30295
cv_p    = sd_p / xbar_p            # 0.4407672

# Middle school (given in the prompt)
n_m     = 250
xbar_m  = 35.51
var_m   = 600.84
sd_m    = float(np.sqrt(var_m))   # 24.51204
cv_m    = sd_m / xbar_m            # 0.6902575

# --- Figure: two panels ---
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.2))

# ---------- LEFT: absolute scale — mean +/- 1 SD bands on the same axis ----------
y_p, y_m = 1, 0  # row positions
band_h   = 0.32

# Primary school band: [xbar - sd, xbar + sd]
ax1.barh(y_p, 2 * sd_p, left=xbar_p - sd_p, height=band_h,
         color=PALETTE["primary"], alpha=0.30,
         edgecolor=PALETTE["primary"], linewidth=1.4,
         label="primary school")
ax1.plot([xbar_p, xbar_p], [y_p - band_h/2, y_p + band_h/2],
         color=PALETTE["warn"], lw=2.3)
ax1.scatter([xbar_p], [y_p], s=70, color=PALETTE["warn"],
            edgecolor="#2a3142", zorder=5)
ax1.annotate(fr"$\bar x_P = {xbar_p:.3f}$", xy=(xbar_p, y_p + band_h/2 + 0.04),
             ha="center", va="bottom", fontsize=10.5, color=PALETTE["warn"],
             fontweight="bold")
ax1.annotate(f"{xbar_p - sd_p:.2f}", xy=(xbar_p - sd_p, y_p - band_h/2 - 0.05),
             ha="center", va="top", fontsize=9.5, color=PALETTE["primary"])
ax1.annotate(f"{xbar_p + sd_p:.2f}", xy=(xbar_p + sd_p, y_p - band_h/2 - 0.05),
             ha="center", va="top", fontsize=9.5, color=PALETTE["primary"])
ax1.annotate(fr"$s_P = {sd_p:.3f}$", xy=(xbar_p + sd_p + 1.4, y_p),
             ha="left", va="center", fontsize=10.5, color=PALETTE["primary"],
             fontweight="bold")

# Middle school band: [xbar - sd, xbar + sd]
ax1.barh(y_m, 2 * sd_m, left=xbar_m - sd_m, height=band_h,
         color=PALETTE["accent"], alpha=0.30,
         edgecolor=PALETTE["accent"], linewidth=1.4,
         label="middle school")
ax1.plot([xbar_m, xbar_m], [y_m - band_h/2, y_m + band_h/2],
         color=PALETTE["warn"], lw=2.3)
ax1.scatter([xbar_m], [y_m], s=70, color=PALETTE["warn"],
            edgecolor="#2a3142", zorder=5)
ax1.annotate(fr"$\bar x_M = {xbar_m:.2f}$", xy=(xbar_m, y_m + band_h/2 + 0.04),
             ha="center", va="bottom", fontsize=10.5, color=PALETTE["warn"],
             fontweight="bold")
ax1.annotate(f"{xbar_m - sd_m:.2f}", xy=(xbar_m - sd_m, y_m - band_h/2 - 0.05),
             ha="center", va="top", fontsize=9.5, color="#7a6700")
ax1.annotate(f"{xbar_m + sd_m:.2f}", xy=(xbar_m + sd_m, y_m - band_h/2 - 0.05),
             ha="center", va="top", fontsize=9.5, color="#7a6700")
ax1.annotate(fr"$s_M = {sd_m:.2f}$", xy=(xbar_m + sd_m + 1.4, y_m),
             ha="left", va="center", fontsize=10.5, color="#7a6700",
             fontweight="bold")

ax1.set_yticks([y_m, y_p])
ax1.set_yticklabels(["middle\n($n=250$)", "primary\n($n=300$)"])
ax1.set_xlim(0, 65)
ax1.set_ylim(-0.6, 1.7)
ax1.set_xlabel("Hours of access per week")
ax1.set_title(r"Absolute scale — $\bar x \pm 1\,s$ band (hours)")
ax1.axvline(0, color=PALETTE["neutral"], lw=0.6, ls=":")

# ---------- RIGHT: relative scale — CV bars (dimensionless) ----------
labels = ["primary\n($n=300$)", "middle\n($n=250$)"]
sds    = [sd_p, sd_m]
cvs    = [cv_p, cv_m]
colors_bars = [PALETTE["primary"], PALETTE["accent"]]

x = np.arange(len(labels))
bw = 0.36
b1 = ax2.bar(x - bw/2, sds, bw,
             color=[PALETTE["primary"], PALETTE["accent"]],
             alpha=0.55, edgecolor="#2a3142", linewidth=0.9,
             label=r"$s$ (hours, left axis)")
for bb, v in zip(b1, sds):
    ax2.text(bb.get_x() + bb.get_width()/2, bb.get_height() + 0.5,
             f"{v:.2f}", ha="center", va="bottom", fontsize=10.5,
             color="#2a3142", fontweight="bold")

ax2.set_xticks(x)
ax2.set_xticklabels(labels)
ax2.set_ylabel("Standard deviation $s$ (hours)",
               color=PALETTE["primary"])
ax2.tick_params(axis="y", labelcolor=PALETTE["primary"])
ax2.set_ylim(0, max(sds) * 1.45)

# Twin axis: CV
ax2b = ax2.twinx()
b2 = ax2b.bar(x + bw/2, cvs, bw,
              color=PALETTE["warn"], alpha=0.80,
              edgecolor="#2a3142", linewidth=0.9,
              label=r"CV $=s/\bar x$ (right axis)")
for bb, v in zip(b2, cvs):
    ax2b.text(bb.get_x() + bb.get_width()/2, bb.get_height() + 0.015,
              f"{v:.3f}", ha="center", va="bottom", fontsize=10.5,
              color="#7a6700", fontweight="bold")
ax2b.set_ylabel(r"Coefficient of variation  CV $=s/\bar x$",
                color="#7a6700")
ax2b.tick_params(axis="y", labelcolor="#7a6700")
ax2b.set_ylim(0, max(cvs) * 1.45)
# clean right spine so the twin axis is visible
ax2b.spines["top"].set_visible(False)

ax2.set_title("Absolute (hours) vs relative (CV) dispersion")

# Combined legend
h1, l1 = ax2.get_legend_handles_labels()
h2, l2 = ax2b.get_legend_handles_labels()
ax2.legend(h1 + h2, l1 + l2, loc="upper left", framealpha=0.95,
           fontsize=9.5)

# Summary box on the LEFT panel
summary = (
    r"$s_P = \sqrt{106.1507} = "
    f"{sd_p:.3f}$"
    "\n"
    r"$\mathrm{CV}_P = s_P/\bar x_P = "
    f"{sd_p:.3f}/{xbar_p:.3f} = {cv_p:.3f}$"
    "\n"
    r"$s_M = \sqrt{600.84} = "
    f"{sd_m:.2f}$"
    "\n"
    r"$\mathrm{CV}_M = s_M/\bar x_M = "
    f"{sd_m:.2f}/{xbar_m:.2f} = {cv_m:.3f}$"
)
ax1.text(0.99, 0.02, summary, transform=ax1.transAxes,
         ha="right", va="bottom", fontsize=9.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  primary: xbar={xbar_p}, s={sd_p:.5f}, CV={cv_p:.5f}")
print(f"  middle : xbar={xbar_m}, s={sd_m:.5f}, CV={cv_m:.5f}")
