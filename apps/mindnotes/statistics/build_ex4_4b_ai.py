"""Build AI walkthrough plot for Ex 4.4 d–g — call-center sampling distributions.

Three panels, all driven by sums / sample-proportions of the call duration X ~ N(180, 1600):
  d) S = X1 + ... + X5 ~ N(900, 8000)        Pr(S > 900)     = 0.5
  e) S = sum of 210 calls ~ N(37800, 336000)  90% central interval
  f) sample proportion of "no-operator" calls (Bernoulli(0.12), n=600)
     P_bar ~ N(0.12, 0.12*0.88/600)           Pr(P_bar > 0.15)
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex4/ex4_4b_ai.png"

fig, axes = plt.subplots(1, 3, figsize=(15, 4.6))

# ---------- d) Pr(S > 900),  S ~ N(900, 8000) ----------
ax = axes[0]
mu_d, sd_d = 900.0, np.sqrt(8000.0)
xs = np.linspace(mu_d - 4 * sd_d, mu_d + 4 * sd_d, 600)
pdf = norm.pdf(xs, mu_d, sd_d)
ax.plot(xs, pdf, color=PALETTE["primary"], lw=2.2)
mask = xs > 900
ax.fill_between(xs[mask], 0, pdf[mask], color=PALETTE["primary"], alpha=0.32)
p_d = 1 - norm.cdf(900, mu_d, sd_d)
ax.axvline(900, color=PALETTE["accent"], lw=1.4, ls="--")
ax.text(900, ax.get_ylim()[1] * 0.95, "  900 s  =  15 min",
        color=PALETTE["accent"], fontsize=10, ha="left", va="top",
        fontweight="bold")
ax.text(0.05, 0.55,
        r"$S\sim N(900,\,8000)$" + "\n"
        + fr"$\Pr(S>900)$" + "\n"
        + fr"$= {p_d:.2f}$",
        transform=ax.transAxes, fontsize=12, ha="left", va="center",
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))
ax.set_title(r"d)  sum of 5 calls  —  $\Pr(S>900)$")
ax.set_xlabel("total duration of 5 calls  (seconds)")
ax.set_ylabel("density")

# ---------- e) 90% central interval, S ~ N(37800, 336000) ----------
ax = axes[1]
mu_e, sd_e = 37800.0, np.sqrt(336000.0)
xs = np.linspace(mu_e - 4 * sd_e, mu_e + 4 * sd_e, 600)
pdf = norm.pdf(xs, mu_e, sd_e)
ax.plot(xs, pdf, color=PALETTE["primary"], lw=2.2)
q05 = norm.ppf(0.05, mu_e, sd_e)
q95 = norm.ppf(0.95, mu_e, sd_e)
mask = (xs >= q05) & (xs <= q95)
ax.fill_between(xs[mask], 0, pdf[mask], color=PALETTE["accent"], alpha=0.40)
ax.axvline(q05, color=PALETTE["accent"], lw=1.4, ls="--")
ax.axvline(q95, color=PALETTE["accent"], lw=1.4, ls="--")
ax.text(q05, ax.get_ylim()[1] * 0.95, f"  $q_{{0.05}}\\approx{q05:,.0f}$",
        color=PALETTE["accent"], fontsize=10, ha="right", va="top",
        fontweight="bold", rotation=0)
ax.text(q95, ax.get_ylim()[1] * 0.95, f"$q_{{0.95}}\\approx{q95:,.0f}$  ",
        color=PALETTE["accent"], fontsize=10, ha="left", va="top",
        fontweight="bold")
ax.text(0.02, 0.50,
        r"$S\sim N(37800,\,336000)$" + "\n"
        + r"central 90% mass" + "\n"
        + fr"$[{q05:,.0f},\,{q95:,.0f}]$ s",
        transform=ax.transAxes, fontsize=11, ha="left", va="center",
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))
ax.set_title(r"e)  sum of 210 calls  —  central 90% interval")
ax.set_xlabel("total duration of 210 calls  (seconds)")
ax.ticklabel_format(style="plain", axis="x")
ax.tick_params(axis="x", labelrotation=20)

# ---------- f) Pr(P_bar > 0.15) for P_bar ~ N(0.12, 0.12*0.88/600) ----------
ax = axes[2]
mu_f = 0.12
sd_f = np.sqrt(0.12 * 0.88 / 600)
xs = np.linspace(mu_f - 4 * sd_f, mu_f + 4 * sd_f, 600)
pdf = norm.pdf(xs, mu_f, sd_f)
ax.plot(xs, pdf, color=PALETTE["primary"], lw=2.2)
mask = xs > 0.15
ax.fill_between(xs[mask], 0, pdf[mask], color="#d4493a", alpha=0.55)
p_f = 1 - norm.cdf(0.15, mu_f, sd_f)
ax.axvline(0.15, color="#d4493a", lw=1.4, ls="--")
ax.text(0.15, ax.get_ylim()[1] * 0.95, "  0.15",
        color="#d4493a", fontsize=10, ha="left", va="top",
        fontweight="bold")
ax.text(0.04, 0.55,
        r"$\bar P\approx N\!\left(0.12,\,\frac{0.12\cdot 0.88}{600}\right)$" + "\n"
        + fr"$\Pr(\bar P>0.15)$" + "\n"
        + fr"$\approx {p_f:.4f}$",
        transform=ax.transAxes, fontsize=11, ha="left", va="center",
        bbox=dict(facecolor="#fffbe6", edgecolor="#d4493a",
                  boxstyle="round,pad=0.45", linewidth=1.0))
ax.set_title(r"f)  sample proportion  —  $\Pr(\bar P>0.15)$")
ax.set_xlabel(r"$\bar P$  (sample proportion of no-operator calls)")

# annotation: mean line on all panels
for i, ax in enumerate(axes):
    mu_i = [mu_d, mu_e, mu_f][i]
    ax.axvline(mu_i, color=PALETTE["neutral"], lw=1.0, ls=":", alpha=0.6)

fig.suptitle(r"Ex 4.4 d–f  —  sums and proportions on top of $X\sim N(180,\,40^2)$",
             fontsize=13, y=1.02)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  d) Pr(S>900)            = {p_d:.6f}")
print(f"  e) [q05, q95]           = [{q05:.4f}, {q95:.4f}]")
print(f"  f) Pr(P_bar>0.15)       = {p_f:.6f}")
