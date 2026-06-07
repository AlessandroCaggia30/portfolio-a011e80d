"""Build AI walkthrough plot for Ex 4.5 — take-away: T = X + Y.

X (order time) ~ has E=3.5, sd=1.5; Y (filling time) ~ has E=1.6, sd=0.2.
Linear correlation rho = +0.22.

Three panels:
  (left)   Variance decomposition Var(T) = Var(X) + Var(Y) + 2 Cov(X, Y)
           as a stacked horizontal bar — shows that the covariance bump,
           while small (2*0.22*1.5*0.2 = 0.132), is *not* negligible.
  (mid)    Assuming bivariate normality, T = X+Y ~ N(5.1, 1.5563^2).
           Shade the right tail Pr(T > 7) and report the value.
  (right)  Independence case: Cov = 0, Var(T) = 2.29, sd = 1.5133.
           Same right tail Pr(T > 7) for comparison. Both densities
           overlaid in a single mini-panel highlights how the correlation
           bumps the tail from 0.1046 to 0.1111.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex4/ex4_5_ai.png"

# ---- parameters ----
muX, sdX = 3.5, 1.5
muY, sdY = 1.6, 0.2
rho      = 0.22
thr      = 7.0

mu_T   = muX + muY                               # 5.1
covXY  = rho * sdX * sdY                         # 0.066
var_T  = sdX**2 + sdY**2 + 2*covXY               # 2.422
sd_T   = np.sqrt(var_T)                          # 1.5563

var_T0 = sdX**2 + sdY**2                         # 2.29 (independence)
sd_T0  = np.sqrt(var_T0)                         # 1.5133

p_tail  = 1 - norm.cdf(thr, mu_T, sd_T)          # 0.1111
p_tail0 = 1 - norm.cdf(thr, mu_T, sd_T0)         # 0.1046

# ---- figure ----
fig, axes = plt.subplots(1, 3, figsize=(15.5, 4.8))

# ============ LEFT: variance decomposition stacked bar ============
ax = axes[0]
parts  = [sdX**2, sdY**2, 2*covXY]
labels = [r"$\mathrm{Var}(X)=1.5^2=2.25$",
          r"$\mathrm{Var}(Y)=0.2^2=0.04$",
          fr"$2\,\mathrm{{Cov}}(X,Y)={2*covXY:.3f}$"]
colors = [PALETTE["primary"], PALETTE["accent"], PALETTE["warn"]]

left = 0.0
for w, lab, c in zip(parts, labels, colors):
    ax.barh(0, w, left=left, height=0.55, color=c, alpha=0.78,
            edgecolor="white", linewidth=1.4, label=lab)
    ax.text(left + w/2, 0, f"{w:.3f}", ha="center", va="center",
            color="white", fontweight="bold", fontsize=11)
    left += w
ax.set_xlim(-0.05, var_T*1.08)
ax.set_ylim(-1.0, 1.6)
ax.set_yticks([])
ax.set_xlabel(r"contribution to $\mathrm{Var}(T)$")
ax.set_title(r"a)  $\mathrm{Var}(T)=\mathrm{Var}(X)+\mathrm{Var}(Y)+2\,\mathrm{Cov}(X,Y)$")
ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.18),
          ncol=1, framealpha=0.95, fontsize=9.5)
ax.text(var_T, 0.55, fr"$\mathrm{{Var}}(T)={var_T:.3f}$" + "\n" +
        fr"$\sigma_T={sd_T:.4f}$",
        ha="right", va="bottom", fontsize=10.5, fontweight="bold",
        color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.4", linewidth=1.0))

# ============ MIDDLE: T ~ N(5.1, 1.5563^2), Pr(T>7) shaded ============
ax = axes[1]
ts = np.linspace(mu_T - 4*sd_T, mu_T + 4*sd_T, 600)
pdf = norm.pdf(ts, mu_T, sd_T)
ax.plot(ts, pdf, color=PALETTE["primary"], lw=2.2,
        label=fr"$T\sim N({mu_T},\,{sd_T:.4f}^2)$")
mask = ts > thr
ax.fill_between(ts[mask], 0, pdf[mask], color=PALETTE["warn"], alpha=0.42,
                label=fr"$\Pr(T>7)={p_tail:.4f}$")
ax.axvline(mu_T, color=PALETTE["neutral"], lw=1.0, ls=":", alpha=0.7)
ax.axvline(thr, color=PALETTE["warn"], lw=1.4, ls="--")
y_top = pdf.max()
ax.text(mu_T, y_top*1.03, fr"$\mu_T={mu_T}$", color=PALETTE["neutral"],
        ha="center", va="bottom", fontsize=10.5)
ax.text(thr, y_top*0.72, "  7 min", color=PALETTE["warn"],
        ha="left", va="center", fontsize=11, fontweight="bold")
ax.set_xlabel(r"$T = X+Y$  (minutes)")
ax.set_ylabel("density")
ax.set_title(r"b)  Bivariate-normal assumption  $\Rightarrow$  $\Pr(T>7)$")
ax.set_ylim(0, y_top*1.18)
ax.legend(loc="upper left", framealpha=0.95, fontsize=9.5)

# ============ RIGHT: correlated vs independent ============
ax = axes[2]
ts = np.linspace(mu_T - 4*max(sd_T, sd_T0), mu_T + 4*max(sd_T, sd_T0), 600)
pdf_corr = norm.pdf(ts, mu_T, sd_T)
pdf_indep = norm.pdf(ts, mu_T, sd_T0)
ax.plot(ts, pdf_indep, color=PALETTE["accent"], lw=2.0, ls="--",
        label=fr"indep.: $N({mu_T},\,{sd_T0:.4f}^2)$")
ax.plot(ts, pdf_corr,  color=PALETTE["primary"], lw=2.2,
        label=fr"$\rho=0.22$: $N({mu_T},\,{sd_T:.4f}^2)$")
# shade both right tails for visual comparison
mask = ts > thr
ax.fill_between(ts[mask], 0, pdf_indep[mask], color=PALETTE["accent"],
                alpha=0.22)
ax.fill_between(ts[mask], 0, pdf_corr[mask], color=PALETTE["warn"],
                alpha=0.35)
ax.axvline(thr, color=PALETTE["warn"], lw=1.3, ls="--", alpha=0.8)
ax.text(thr, ax.get_ylim()[1]*0.82, "  7 min",
        color=PALETTE["warn"], ha="left", va="top", fontsize=10.5,
        fontweight="bold")
ax.text(0.97, 0.97,
        f"$\\Pr(T>7)$\n"
        f"  $\\rho=0.22$:  {p_tail:.4f}\n"
        f"  indep.:    {p_tail0:.4f}\n"
        f"  $\\Delta$ =     {p_tail - p_tail0:+.4f}",
        transform=ax.transAxes, ha="right", va="top", fontsize=10.5,
        family="monospace",
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))
ax.set_xlabel(r"$T = X+Y$  (minutes)")
ax.set_ylabel("density")
ax.set_title(r"c)  Correlated vs independent — same $\mu_T$, different $\sigma_T$")
ax.legend(loc="upper left", framealpha=0.95, fontsize=9)

fig.suptitle(
    r"Ex 4.5  —  Take-away $T=X+Y$:  variance decomposition, tail "
    r"probability, and the effect of $\rho$",
    fontsize=13, y=1.02)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  E[T]  = {mu_T}")
print(f"  Var(T) correlated   = {var_T:.6f},  sd = {sd_T:.6f}")
print(f"  Var(T) independent  = {var_T0:.6f},  sd = {sd_T0:.6f}")
print(f"  Pr(T>7) correlated  = {p_tail:.6f}")
print(f"  Pr(T>7) independent = {p_tail0:.6f}")
