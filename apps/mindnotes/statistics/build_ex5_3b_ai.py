"""Build AI walkthrough plot for Ex 5.3b — closeness claim (c) + SE with unknown variance (d)."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex5/ex5_3b_ai.png"

# --- Given data from Ex 5.3 ---
n1, n2 = 15, 50
sum_x  = 2755
sum_x2 = 585203
sigma2_true = 6500             # true (a) sigma^2 — used as reference in (d)

xbar15 = sum_x / n1                                              # 183.667
xbar50 = 205                                                     # given in (b)

# (d) unknown variance — estimate from each sample
s2_15  = (1.0/(n1-1)) * (sum_x2 - n1 * xbar15**2)                # 5657.238
se_unk_15 = np.sqrt(s2_15 / n1)                                  # ~19.42
s_50   = 91.5
s2_50  = s_50**2                                                 # 8372.25
se_unk_50 = s_50 / np.sqrt(n2)                                   # ~12.94

# Known-variance SEs (for comparison)
se_kn_15 = np.sqrt(sigma2_true / n1)                             # 20.82
se_kn_50 = np.sqrt(sigma2_true / n2)                             # 11.40

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5))

# =================================================================
# LEFT (c): two sampling distributions of Xbar (n=15 vs n=50) -
#          tighter density != any single draw is closer to mu
# =================================================================
mu_true = 200            # illustrative "true" mu (unknown in practice)
xs = np.linspace(mu_true - 4*se_kn_15, mu_true + 4*se_kn_15, 500)
def pdf(x, sd, mu=mu_true):
    return np.exp(-0.5*((x-mu)/sd)**2) / (sd*np.sqrt(2*np.pi))

ax1.plot(xs, pdf(xs, se_kn_15), color=PALETTE["primary"], lw=2.3,
         label=fr"$n=15$,  SE $\approx$ {se_kn_15:.2f}")
ax1.plot(xs, pdf(xs, se_kn_50), color=PALETTE["accent"], lw=2.3,
         label=fr"$n=50$,  SE $\approx$ {se_kn_50:.2f}")
ax1.axvline(mu_true, color=PALETTE["neutral"], lw=1.3, ls="--", alpha=0.7)
ax1.text(mu_true, ax1.get_ylim()[1]*0.97 if ax1.get_ylim()[1] else 0.03,
         r"  $\mu$ (unknown)", color=PALETTE["neutral"], fontsize=10,
         ha="left", va="top")

# Drop two illustrative realizations: a "lucky" small-n draw and an "unlucky" big-n draw
xbar_lucky_15  = mu_true - 4       # n=15 realization close to mu
xbar_unlucky50 = mu_true + 13      # n=50 realization farther from mu
ax1.plot([xbar_lucky_15], [pdf(xbar_lucky_15, se_kn_15)], "o",
         color=PALETTE["primary"], markersize=10, markeredgecolor="black",
         markeredgewidth=0.7, zorder=5)
ax1.annotate(fr"$\bar x_{{15}}$ close to $\mu$",
             xy=(xbar_lucky_15, pdf(xbar_lucky_15, se_kn_15)),
             xytext=(xbar_lucky_15 - 22, pdf(xbar_lucky_15, se_kn_15) + 0.003),
             fontsize=10, color=PALETTE["primary"],
             arrowprops=dict(arrowstyle="->", color=PALETTE["primary"], lw=1.0))
ax1.plot([xbar_unlucky50], [pdf(xbar_unlucky50, se_kn_50)], "o",
         color=PALETTE["accent"], markersize=10, markeredgecolor="black",
         markeredgewidth=0.7, zorder=5)
ax1.annotate(fr"$\bar x_{{50}}$ farther from $\mu$",
             xy=(xbar_unlucky50, pdf(xbar_unlucky50, se_kn_50)),
             xytext=(xbar_unlucky50 + 3, pdf(xbar_unlucky50, se_kn_50) + 0.004),
             fontsize=10, color=PALETTE["accent"],
             arrowprops=dict(arrowstyle="->", color=PALETTE["accent"], lw=1.0))

ax1.set_xlabel(r"$\bar X$"); ax1.set_ylabel("density")
ax1.set_title(r"(c) Tighter density $\neq$ any draw is closer to $\mu$")
ax1.legend(loc="upper right", framealpha=0.95)
ax1.text(0.02, 0.97,
         "Claim: 'estimate from (b) is closer to $\\mu$' — FALSE.\n"
         "Smaller SE = sampling distribution is more concentrated,\n"
         "but a SPECIFIC realization $\\bar x$ may sit further out.",
         transform=ax1.transAxes, ha="left", va="top", fontsize=9.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

# =================================================================
# RIGHT (d): SE with unknown variance — plug in s^2 for each sample
# =================================================================
labels = [r"$n=15$" + "\n" + r"$s^2 = 5657.2$",
          r"$n=50$" + "\n" + r"$s^2 = 8372.2$"]
vals_unk = [se_unk_15, se_unk_50]
vals_kn  = [se_kn_15,  se_kn_50]
x = np.arange(len(labels))
w = 0.36

b1 = ax2.bar(x - w/2, vals_kn,  w, color=PALETTE["primary"],
             edgecolor="#2a3142", linewidth=0.8,
             label=r"$SE = \sigma/\sqrt{n}$  (known $\sigma^2=6500$)")
b2 = ax2.bar(x + w/2, vals_unk, w, color=PALETTE["accent"],
             edgecolor="#2a3142", linewidth=0.8,
             label=r"$\widehat{SE} = s/\sqrt{n}$  (unknown $\sigma^2$)")
for b, v in list(zip(b1, vals_kn)) + list(zip(b2, vals_unk)):
    ax2.text(b.get_x() + b.get_width()/2, v + 0.4, f"{v:.2f}",
             ha="center", va="bottom", fontsize=10.5, fontweight="bold")

ax2.set_xticks(x); ax2.set_xticklabels(labels)
ax2.set_ylabel(r"SE of $\bar X$")
ax2.set_title(r"(d) Plug-in $s^2$ when $\sigma^2$ is unknown")
ax2.set_ylim(0, 28)
ax2.legend(loc="upper right", framealpha=0.95)
ax2.text(0.02, 0.97,
         f"$n=15$: $s^2={s2_15:.1f}$  vs.  $\\sigma^2=6500$  (under-shoots)\n"
         f"$n=50$: $s^2={s2_50:.1f}$  vs.  $\\sigma^2=6500$  (over-shoots)\n"
         "Larger $n$ shrinks the SE AND stabilises $s^2$.",
         transform=ax2.transAxes, ha="left", va="top", fontsize=9.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  se_unk (n=15) = {se_unk_15:.4f}   se_known(n=15) = {se_kn_15:.4f}")
print(f"  se_unk (n=50) = {se_unk_50:.4f}   se_known(n=50) = {se_kn_50:.4f}")
