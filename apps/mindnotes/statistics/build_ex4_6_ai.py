"""Build AI walkthrough plot for Ex 4.6 — Bus travel: σ from quartiles, P(X>120),
30th percentile, CLT, and the Y = 1.15 X scenario."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex4/ex4_6_ai.png"

# --- Given / derived ---
mu_X    = 90.0
q1, q3  = 80.0, 100.0
z25     = norm.ppf(0.25)                       # -0.6745
sigma_X = (q1 - mu_X) / z25                    # 14.826
mu_Y    = 1.15 * mu_X                          # 103.5
sigma_Y = 1.15 * sigma_X                       # 17.05

p_x_gt_120 = 1 - norm.cdf(120, mu_X, sigma_X)
q30_X      = norm.ppf(0.30, mu_X, sigma_X)
p_y_gt_120 = 1 - norm.cdf(120, mu_Y, sigma_Y)
q30_Y      = norm.ppf(0.30, mu_Y, sigma_Y)

# CLT: proportion of 45 days with travel time < 82 min
p82   = norm.cdf(82, mu_X, sigma_X)            # 0.2947
n_clt = 45
se_pbar = np.sqrt(p82 * (1 - p82) / n_clt)
prob_pbar_gt_010 = 1 - norm.cdf(0.10, p82, se_pbar)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.2))

# ============ LEFT: N(90, 14.826) with q1, q3, 30th pct, P(X>120) ============
xs = np.linspace(mu_X - 4*sigma_X, mu_X + 4*sigma_X, 600)
pdf_X = norm.pdf(xs, mu_X, sigma_X)
ax1.plot(xs, pdf_X, color=PALETTE["primary"], lw=2.2,
         label=fr"$X \sim N(90,\, {sigma_X:.3f}^2)$")
ax1.axvline(mu_X, color=PALETTE["neutral"], lw=1.0, ls="--", alpha=0.6)

# Shade the right tail Pr(X > 120)
mask_tail = xs >= 120
ax1.fill_between(xs[mask_tail], 0, pdf_X[mask_tail],
                 color=PALETTE["accent"], alpha=0.55,
                 label=fr"$\Pr(X>120)={p_x_gt_120:.4f}$")

# Quartile markers
for q, lab, col in [(q1, r"$q_1=80$", PALETTE["primary"]),
                    (q3, r"$q_3=100$", PALETTE["primary"]),
                    (q30_X, fr"$q_{{0.30}}={q30_X:.2f}$", PALETTE["accent"])]:
    ax1.axvline(q, color=col, lw=1.1, ls=":", alpha=0.85)
    ax1.text(q, norm.pdf(q, mu_X, sigma_X) + 0.0015, lab,
             ha="center", va="bottom", fontsize=9.5, color=col)

# Annotate sigma derivation
ax1.text(0.02, 0.97,
         "Find $\\sigma$ from quartiles:\n"
         r"$(80-90)/\sigma = \Phi^{-1}(0.25)\approx -0.6745$"
         "\n"
         r"$\Rightarrow \sigma = 10/0.6745 \approx 14.826$",
         transform=ax1.transAxes, ha="left", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

ax1.set_xlabel("travel time $X$ (minutes)")
ax1.set_ylabel("density")
ax1.set_title("Step 1 — calibrate $X\\sim N(90,\\sigma^2)$ from $q_1, q_3$")
ax1.legend(loc="upper right", framealpha=0.95)

# ============ RIGHT: X vs Y = 1.15 X overlay, with P(>120) shaded ============
ys = np.linspace(min(mu_X - 4*sigma_X, mu_Y - 4*sigma_Y),
                 max(mu_X + 4*sigma_X, mu_Y + 4*sigma_Y), 600)
pdf_X2 = norm.pdf(ys, mu_X, sigma_X)
pdf_Y  = norm.pdf(ys, mu_Y, sigma_Y)

ax2.plot(ys, pdf_X2, color=PALETTE["primary"], lw=2.2,
         label=fr"$X\sim N(90,\,{sigma_X:.2f}^2)$")
ax2.plot(ys, pdf_Y, color=PALETTE["accent"], lw=2.2,
         label=fr"$Y=1.15X\sim N({mu_Y:.1f},\,{sigma_Y:.2f}^2)$")

# Shade right tails Pr(>120) under each
mask = ys >= 120
ax2.fill_between(ys[mask], 0, pdf_X2[mask], color=PALETTE["primary"], alpha=0.20)
ax2.fill_between(ys[mask], 0, pdf_Y[mask],  color=PALETTE["accent"], alpha=0.35)

# Annotation comparing the two tails
ax2.axvline(120, color=PALETTE["neutral"], lw=1.0, ls="--", alpha=0.6)
ax2.text(120, ax2.get_ylim()[1]*0.97 if ax2.get_ylim()[1]>0 else 0.025,
         "  $x=120$",
         color=PALETTE["neutral"], fontsize=10, ha="left", va="top")

ax2.text(0.98, 0.97,
         f"Tail above 120 min:\n"
         fr"  baseline  $\Pr(X>120)={p_x_gt_120:.4f}$"+"\n"
         fr"  +15% traffic  $\Pr(Y>120)={p_y_gt_120:.4f}$"+"\n\n"
         f"CLT (n=45 days):\n"
         fr"  $p=\Pr(X<82)={p82:.3f}$"+"\n"
         fr"  $\Pr(\bar P>0.10)\approx {prob_pbar_gt_010:.4f}$",
         transform=ax2.transAxes, ha="right", va="top", fontsize=9.5,
         bbox=dict(facecolor="#f4f6fb", edgecolor=PALETTE["primary"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

ax2.set_xlabel("travel time (minutes)")
ax2.set_ylabel("density")
ax2.set_title("Step 2 — shift to $Y=1.15X$ (special events) and tail comparison")
ax2.legend(loc="upper left", framealpha=0.95)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  sigma_X       = {sigma_X:.4f}")
print(f"  Pr(X>120)     = {p_x_gt_120:.6f}")
print(f"  q_0.30 (X)    = {q30_X:.4f}")
print(f"  Pr(X<82)=p    = {p82:.6f}")
print(f"  SE(Pbar)      = {se_pbar:.6f}")
print(f"  Pr(Pbar>0.10) = {prob_pbar_gt_010:.6f}")
print(f"  mu_Y          = {mu_Y:.4f}")
print(f"  sigma_Y       = {sigma_Y:.4f}")
print(f"  Pr(Y>120)     = {p_y_gt_120:.6f}")
print(f"  q_0.30 (Y)    = {q30_Y:.4f}")
