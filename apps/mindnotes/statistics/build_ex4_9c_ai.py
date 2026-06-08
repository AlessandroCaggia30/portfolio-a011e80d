"""Build AI walkthrough plot for Ex 4.9c — P(23000 < Xbar < 27000), n=20; with d1/d2 at n=100."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex4/ex4_4_9c_ai.png"

mu = 25000.0
sigma = 9800.0

# Three sampling distributions: n=10, n=20, n=100
ns = [10, 20, 100]
ses = {n: sigma / np.sqrt(n) for n in ns}

# --- LEFT: n=20 with P(23000 < Xbar < 27000) shaded; overlay n=10 to show wider spread ---
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5))

n_main = 20
se20 = ses[20]
xs = np.linspace(mu - 4*se20, mu + 4*se20, 800)
pdf20 = norm.pdf(xs, mu, se20)
pdf10 = norm.pdf(xs, mu, ses[10])

p_main = norm.cdf(27000, mu, se20) - norm.cdf(23000, mu, se20)
p_n10  = norm.cdf(27000, mu, ses[10]) - norm.cdf(23000, mu, ses[10])

ax1.plot(xs, pdf20, color=PALETTE["primary"], lw=2.4,
         label=fr"$n=20$: $\bar X \sim N(25000,\,9800^2/20)$")
ax1.plot(xs, pdf10, color=PALETTE["warn"], lw=1.8, ls="--",
         label=fr"$n=10$: more dispersed (larger SE)")
mask = (xs >= 23000) & (xs <= 27000)
ax1.fill_between(xs[mask], 0, pdf20[mask], color=PALETTE["accent"], alpha=0.55,
                 label=fr"$P(23000<\bar X<27000)\approx{p_main:.4f}$")
ax1.fill_between(xs[~mask], 0, pdf20[~mask], color=PALETTE["primary"], alpha=0.08)

for x in (23000, 27000):
    ax1.axvline(x, color=PALETTE["neutral"], lw=1.0, ls=":", alpha=0.7)
ax1.axvline(mu, color=PALETTE["neutral"], lw=1.0, ls=":", alpha=0.5)

ax1.text(23000, ax1.get_ylim()[1]*0.93, "  23,000",
         color=PALETTE["neutral"], fontsize=10, ha="left", va="top")
ax1.text(27000, ax1.get_ylim()[1]*0.93, "27,000  ",
         color=PALETTE["neutral"], fontsize=10, ha="right", va="top")
ax1.text(mu, ax1.get_ylim()[1]*0.99, fr"  $\mu=25{{,}}000$",
         color=PALETTE["neutral"], fontsize=10, ha="left", va="top")

ax1.set_xlabel(r"sample mean $\bar X$ (Euro)")
ax1.set_ylabel("density")
ax1.set_title(r"Sampling distribution of $\bar X$ (turnover, $n=20$ vs $n=10$)")
ax1.legend(loc="upper left", framealpha=0.95, fontsize=9.5)
ax1.text(0.98, 0.55,
         f"At $n=10$ the same interval\ncaptures only $\\approx{p_n10:.4f}$",
         transform=ax1.transAxes, ha="right", va="top",
         fontsize=10, color=PALETTE["warn"],
         bbox=dict(facecolor="white", edgecolor=PALETTE["warn"], alpha=0.85, boxstyle="round,pad=0.4"))

# --- RIGHT: n=100 sampling distribution with d1 (P(Xbar > 23947)) shaded + d2 1% / 99% quantiles ---
se100 = ses[100]
xs100 = np.linspace(mu - 4*se100, mu + 4*se100, 800)
pdf100 = norm.pdf(xs100, mu, se100)

xbar_obs = 23946.99
p_d1 = 1 - norm.cdf(xbar_obs, mu, se100)
q01 = norm.ppf(0.01, mu, se100)
q99 = norm.ppf(0.99, mu, se100)

ax2.plot(xs100, pdf100, color=PALETTE["primary"], lw=2.4,
         label=fr"$n=100$: $\bar X \sim N(25000,\,9800^2/100)$")
mask_d1 = xs100 >= xbar_obs
ax2.fill_between(xs100[mask_d1], 0, pdf100[mask_d1], color=PALETTE["accent"], alpha=0.55,
                 label=fr"d1: $P(\bar X>{xbar_obs:.0f})\approx{p_d1:.4f}$")
ax2.fill_between(xs100[~mask_d1], 0, pdf100[~mask_d1], color=PALETTE["primary"], alpha=0.08)

# Mark the 1% tails for d2
left_tail  = xs100 <= q01
right_tail = xs100 >= q99
ax2.fill_between(xs100[left_tail],  0, pdf100[left_tail],  color=PALETTE["warn"], alpha=0.85)
ax2.fill_between(xs100[right_tail], 0, pdf100[right_tail], color=PALETTE["warn"], alpha=0.85)

ax2.axvline(xbar_obs, color=PALETTE["ok"], lw=1.6, ls="--")
ax2.axvline(q01, color=PALETTE["warn"], lw=1.2, ls=":")
ax2.axvline(q99, color=PALETTE["warn"], lw=1.2, ls=":")
ax2.axvline(mu, color=PALETTE["neutral"], lw=1.0, ls=":", alpha=0.5)

ax2.text(xbar_obs, norm.pdf(xbar_obs, mu, se100)*1.05,
         fr"  $\bar x_{{obs}}={xbar_obs:.0f}$",
         color=PALETTE["ok"], fontsize=10.5, ha="left", va="bottom", fontweight="bold")
ax2.text(q01, ax2.get_ylim()[1]*0.55,
         f"1%\n{q01:.0f}", color=PALETTE["warn"],
         fontsize=10, ha="center", va="bottom", fontweight="bold")
ax2.text(q99, ax2.get_ylim()[1]*0.55,
         f"99%\n{q99:.0f}", color=PALETTE["warn"],
         fontsize=10, ha="center", va="bottom", fontweight="bold")
ax2.text(mu, ax2.get_ylim()[1]*0.99, fr"  $\mu=25{{,}}000$",
         color=PALETTE["neutral"], fontsize=10, ha="left", va="top")

ax2.set_xlabel(r"sample mean $\bar X$ (Euro)")
ax2.set_ylabel("density")
ax2.set_title(r"$n=100$ (CLT): d1 right-tail at $\bar x_{obs}$ and d2 1%/99% extremes")
ax2.legend(loc="upper left", framealpha=0.95, fontsize=9.5)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  P(23000<Xbar<27000 | n=20)  = {p_main:.6f}")
print(f"  P(23000<Xbar<27000 | n=10)  = {p_n10:.6f}")
print(f"  P(Xbar > {xbar_obs} | n=100) = {p_d1:.6f}")
print(f"  q_0.01 = {q01:.2f}   q_0.99 = {q99:.2f}")
