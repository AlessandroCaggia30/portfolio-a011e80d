"""Build AI walkthrough plot for Ex 5.2b — SE with unknown vs known variance."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex5/ex5_2b_ai.png"

# --- Given data ---
n1, n2 = 15, 50
sum_x  = 2755
sum_x2 = 585203
sigma2_known = 6500

xbar = sum_x / n1                                            # 183.667
SE_known_15 = np.sqrt(sigma2_known / n1)                     # 20.817 (n=15)
SE_known_50 = np.sqrt(sigma2_known / n2)                     # 11.402 (n=50)

# Unknown variance — estimated from sample
s2_x        = (1.0/(n1-1)) * (sum_x2 - n1 * xbar**2)         # 5657.238
se_unk_15   = np.sqrt(s2_x / n1)                             # 19.43
s_unk_50    = 91.5                                           # given
se_unk_50   = s_unk_50 / np.sqrt(n2)                         # 12.94

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5))

# --- LEFT: bar chart comparing SE for known vs unknown, n=15 vs n=50 ---
labels = [r"$n=15$" + "\nknown $\\sigma^2$",
          r"$n=15$" + "\nunknown $\\sigma^2$",
          r"$n=50$" + "\nknown $\\sigma^2$",
          r"$n=50$" + "\nunknown $\\sigma^2$"]
vals = [SE_known_15, se_unk_15, SE_known_50, se_unk_50]
colors = [PALETTE["primary"], PALETTE["accent"], PALETTE["primary"], PALETTE["accent"]]
bars = ax1.bar(labels, vals, color=colors, edgecolor="#2a3142", linewidth=0.8)
for b, v in zip(bars, vals):
    ax1.text(b.get_x() + b.get_width()/2, v + 0.5, f"{v:.2f}",
             ha="center", va="bottom", fontsize=11, fontweight="bold")
ax1.set_ylabel(r"$SE(\bar X)$")
ax1.set_title(r"Standard error: known $\sigma^2=6500$  vs.  plug-in $s^2$")
ax1.set_ylim(0, 30)
# Annotate the n=15 estimated sigma^2 boxlet
ax1.text(0.35, 0.97,
         f"From sample (n=15):\n"
         r"$s^2 = \frac{1}{n-1}(\sum x_i^2 - n\bar x^2) = $"
         f"{s2_x:.1f}\n"
         f"vs. known $\\sigma^2 = 6500$",
         transform=ax1.transAxes, ha="left", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# --- RIGHT: sampling distributions of Xbar for n=15 vs n=50 (under known sigma2) ---
mu = xbar  # center on the point estimate (the true mu is unknown)
xs = np.linspace(mu - 4*SE_known_15, mu + 4*SE_known_15, 400)
def pdf(x, sd):
    return np.exp(-0.5*((x-mu)/sd)**2) / (sd*np.sqrt(2*np.pi))
ax2.plot(xs, pdf(xs, SE_known_15), color=PALETTE["primary"], lw=2.2,
         label=fr"$n=15$,  SE = {SE_known_15:.2f}")
ax2.plot(xs, pdf(xs, SE_known_50), color=PALETTE["accent"], lw=2.2,
         label=fr"$n=50$,  SE = {SE_known_50:.2f}")
ax2.axvline(mu, color=PALETTE["neutral"], lw=1.0, ls="--", alpha=0.6)
ax2.text(mu, ax2.get_ylim()[1]*0.97, r"  $\bar x = 183.67$",
         color=PALETTE["neutral"], fontsize=10, ha="left", va="top")
# Shade +/- 1 SE bands
for sd, col, alpha in [(SE_known_15, PALETTE["primary"], 0.10),
                        (SE_known_50, PALETTE["accent"], 0.18)]:
    xs_band = np.linspace(mu-sd, mu+sd, 80)
    ax2.fill_between(xs_band, 0, pdf(xs_band, sd), color=col, alpha=alpha)
ax2.set_xlabel(r"$\bar X$")
ax2.set_ylabel("density")
ax2.set_title(r"Sampling distribution of $\bar X$  —  larger $n$ gives tighter density")
ax2.legend(loc="upper right", framealpha=0.95)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  SE_known(n=15) = {SE_known_15:.4f}")
print(f"  SE_unkn (n=15) = {se_unk_15:.4f}  (from s^2 = {s2_x:.2f})")
print(f"  SE_known(n=50) = {SE_known_50:.4f}")
print(f"  SE_unkn (n=50) = {se_unk_50:.4f}  (from s = {s_unk_50})")
