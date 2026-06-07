"""Build AI walkthrough plot for Ex 2.2c — mean & standard deviation from grouped data
(weekly hours of access to devices, n=300 primary-school pupils)."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_2c_ai.png"

# --- Grouped table from the prompt ---
lower    = np.array([0.0, 10.0, 25.0, 30.0])
upper    = np.array([10.0, 25.0, 30.0, 40.0])
freq     = np.array([45,   90,   75,   90], dtype=float)
n        = int(freq.sum())            # 300
p        = freq / freq.sum()          # rel.freq:  0.15, 0.30, 0.25, 0.30
width    = upper - lower              # 10, 15, 5, 10
dens     = p / width                  # 0.015, 0.020, 0.050, 0.030
mid      = (lower + upper) / 2.0      # 5, 17.5, 27.5, 35

# --- Mean & SD from grouped data (midpoint discretization) ---
xbar     = float((mid * p).sum())                          # 23.375
# population-style sum of squared deviations weighted by p
ssd      = float(((mid - xbar) ** 2 * p).sum())            # 105.7959
var_corr = ssd * n / (n - 1)                               # 106.1507  (n/(n-1) correction)
sd       = float(np.sqrt(var_corr))                        # 10.30295

# --- Figure: two panels ---
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.2))

# ---------- LEFT: density histogram with midpoints, mean, ±1 SD band ----------
for lo, hi, d in zip(lower, upper, dens):
    ax1.bar(lo, d, width=hi - lo, align="edge",
            color=PALETTE["primary"], alpha=0.28,
            edgecolor=PALETTE["primary"], linewidth=1.3)

# Midpoints — the discretization step
ax1.scatter(mid, dens, s=72, color=PALETTE["accent"],
            edgecolor="#2a3142", linewidth=0.9, zorder=5,
            label="class midpoints $m_k$")
for m, d, pk in zip(mid, dens, p):
    ax1.annotate(f"$m={m}$\n$p={pk:.2f}$", xy=(m, d), xytext=(0, 10),
                 textcoords="offset points", ha="center", va="bottom",
                 fontsize=9.5, color=PALETTE["neutral"])

# Mean ± 1 SD band
ymax_left = dens.max() * 1.45
ax1.axvspan(xbar - sd, xbar + sd, color=PALETTE["accent"], alpha=0.10,
            label=r"$\bar x \pm 1\,\hat\sigma$")
ax1.axvline(xbar, color=PALETTE["warn"], lw=2.0, ls="-",
            label=fr"$\bar x = {xbar:.3f}$")
ax1.axvline(xbar - sd, color=PALETTE["accent"], lw=1.4, ls="--")
ax1.axvline(xbar + sd, color=PALETTE["accent"], lw=1.4, ls="--")
ax1.text(xbar, ymax_left * 0.62, f"  $\\bar x={xbar:.3f}$",
         color=PALETTE["warn"], fontsize=10.5, ha="left", va="top",
         fontweight="bold")
ax1.text(xbar + sd, ymax_left * 0.55, f"  $\\bar x + \\hat\\sigma \\approx {xbar+sd:.2f}$",
         color="#7a6700", fontsize=9.5, ha="left", va="top")
ax1.text(xbar - sd, ymax_left * 0.55, f"$\\bar x - \\hat\\sigma \\approx {xbar-sd:.2f}$  ",
         color="#7a6700", fontsize=9.5, ha="right", va="top")

ax1.set_xlim(-1.5, 41.5)
ax1.set_ylim(0, ymax_left)
ax1.set_xlabel("Hours of access to devices per week")
ax1.set_ylabel("Density  $c_k = p_k / w_k$")
ax1.set_title("Grouped distribution — midpoint discretization")
ax1.legend(loc="upper center", framealpha=0.95, fontsize=10, ncol=3)

# ---------- RIGHT: per-class contributions to mean and variance ----------
labels = [f"$[{int(lo)},{int(hi)}{')' if hi != 40 else ']'}$"
          for lo, hi in zip(lower, upper)]
mean_contrib = mid * p                          # contribution to xbar
ssd_contrib  = (mid - xbar) ** 2 * p            # contribution to weighted SSD

x = np.arange(len(labels))
bw = 0.36
b1 = ax2.bar(x - bw/2, mean_contrib, bw,
             color=PALETTE["primary"], edgecolor="#2a3142", linewidth=0.8,
             label=r"$m_k\, p_k$  (contributes to $\bar x$)")
b2 = ax2.bar(x + bw/2, ssd_contrib, bw,
             color=PALETTE["accent"], edgecolor="#2a3142", linewidth=0.8,
             label=r"$(m_k-\bar x)^2 p_k$  (contributes to $s^2$)")
for bb in b1:
    ax2.text(bb.get_x() + bb.get_width()/2, bb.get_height() + 0.25,
             f"{bb.get_height():.3f}",
             ha="center", va="bottom", fontsize=10, color=PALETTE["primary"])
for bb in b2:
    ax2.text(bb.get_x() + bb.get_width()/2, bb.get_height() + 0.25,
             f"{bb.get_height():.3f}",
             ha="center", va="bottom", fontsize=10, color="#7a6700")

ax2.set_xticks(x)
ax2.set_xticklabels(labels)
ax2.set_ylabel("Contribution to the moment")
ax2.set_title(r"Per-class contributions to $\bar x$ and $s^2$")
ax2.set_ylim(0, max(mean_contrib.max(), ssd_contrib.max()) * 1.22)
ax2.legend(loc="upper right", framealpha=0.95, fontsize=10)

# Summary box: the totals
summary = (
    r"$\bar x = \sum_k m_k\,p_k = "
    f"{xbar:.3f}$\n"
    r"$\sum_k (m_k - \bar x)^2 p_k = "
    f"{ssd:.4f}$\n"
    r"$s^2 = \frac{n}{n-1}\sum_k(m_k-\bar x)^2 p_k = "
    f"{var_corr:.4f}$\n"
    r"$s = \sqrt{s^2} = "
    f"{sd:.3f}$"
)
ax2.text(0.02, 0.97, summary, transform=ax2.transAxes,
         ha="left", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  xbar             = {xbar:.5f}")
print(f"  sum (m-xbar)^2 p = {ssd:.6f}")
print(f"  var (n/(n-1))    = {var_corr:.6f}")
print(f"  sd               = {sd:.6f}")
