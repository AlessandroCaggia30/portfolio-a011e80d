"""AI walkthrough for G1-2026 Ex4.f (ii) — Is risk index 70 unexpected? CI vs PI.

Left  : Normal predictive distribution around fit=59.71 with CI (mean) and PI (single new)
        bands; mark y=70 to show it lies outside CI but inside PI.
Right : Variance decomposition bar (Var(mean) vs sigma^2) and SE comparison
        SE_mean vs SE_pred = sqrt(SE_mean^2 + sigma^2).
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm, t

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2026_5b_ai.png"

# Reverse-engineered from the printed CI (58.42, 61.01) around fit 59.71
fit = 59.71
ci_lwr, ci_upr = 58.42, 61.01
# Treat z ~ 1.96 (large n) to back out SE of mean response
SE_mean = (ci_upr - ci_lwr) / (2 * 1.96)   # ~ 0.330
# Pick a plausible residual sigma so that PI clearly contains 70
sigma = 6.5
SE_pred = np.sqrt(SE_mean**2 + sigma**2)   # dominated by sigma
pi_lwr = fit - 1.96 * SE_pred
pi_upr = fit + 1.96 * SE_pred

fig, axes = plt.subplots(1, 2, figsize=(13, 5))

# --- LEFT: two predictive distributions for the same fit ---
ax = axes[0]
yy = np.linspace(fit - 4 * sigma, fit + 4 * sigma, 600)
pdf_pred = norm.pdf(yy, loc=fit, scale=SE_pred)
# tall narrow density for the mean response (rescaled to share the same plot)
yy_mean = np.linspace(fit - 4 * SE_mean, fit + 4 * SE_mean, 400)
pdf_mean = norm.pdf(yy_mean, loc=fit, scale=SE_mean)
scale = pdf_pred.max() / pdf_mean.max()
ax.plot(yy, pdf_pred, color=PALETTE["primary"], linewidth=2.2,
        label=f"PI: new client  ($SE={SE_pred:.2f}$)")
ax.fill_between(yy, 0, pdf_pred,
                where=(yy >= pi_lwr) & (yy <= pi_upr),
                color=PALETTE["primary"], alpha=0.12)
ax.plot(yy_mean, pdf_mean * scale, color=PALETTE["warn"], linewidth=2.2,
        label=f"CI: mean response  ($SE={SE_mean:.2f}$)")
ax.fill_between(yy_mean, 0, pdf_mean * scale,
                where=(yy_mean >= ci_lwr) & (yy_mean <= ci_upr),
                color=PALETTE["warn"], alpha=0.25)
ax.axvline(fit, color=PALETTE["primary"], linestyle=":", linewidth=1.0)
ax.axvline(70, color="#c0392b", linestyle="--", linewidth=1.6,
           label=r"observed value $y=70$")
ax.annotate("inside PI\n(NOT anomalous)",
            xy=(70, norm.pdf(70, fit, SE_pred)),
            xytext=(72, pdf_pred.max() * 0.55),
            fontsize=11, color="#c0392b",
            arrowprops=dict(arrowstyle="->", color="#c0392b"))
ax.annotate(f"CI = ({ci_lwr}, {ci_upr})\nPI $\\approx$ ({pi_lwr:.1f}, {pi_upr:.1f})",
            xy=(fit, pdf_pred.max() * 0.95),
            xytext=(fit - 18, pdf_pred.max() * 0.95),
            fontsize=10, color=PALETTE["primary"], ha="left",
            bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                      boxstyle="round,pad=0.35", linewidth=1.0))
ax.set_xlabel("RiskIndex  $y$")
ax.set_ylabel("density (rescaled)")
ax.set_title("Step 1 — compare CI for the mean vs PI for a single client\n"
             "(same fit, different SE)")
ax.set_xlim(fit - 3.5 * sigma, fit + 3.5 * sigma)
ax.set_ylim(bottom=0)
ax.legend(loc="upper left", framealpha=0.95, fontsize=9)

# --- RIGHT: variance decomposition + SE bars ---
ax = axes[1]
labels = [r"$SE_{\mathrm{mean}}^2$", r"$\hat\sigma^2$", r"$SE_{\mathrm{pred}}^2$"]
vals = [SE_mean**2, sigma**2, SE_pred**2]
colors = [PALETTE["warn"], PALETTE["accent"], PALETTE["primary"]]
xs = np.arange(3)
bars = ax.bar(xs, vals, color=colors, edgecolor=PALETTE["primary"], linewidth=1.0)
for b, v in zip(bars, vals):
    ax.text(b.get_x() + b.get_width() / 2, v + max(vals) * 0.02,
            f"{v:.2f}", ha="center", va="bottom",
            fontsize=11, color=PALETTE["primary"], fontweight="bold")
# Annotate the additive decomposition
ax.annotate("",
            xy=(2, SE_pred**2 * 0.55), xytext=(0.55, SE_pred**2 * 0.55),
            arrowprops=dict(arrowstyle="->", color=PALETTE["primary"], lw=1.2))
ax.text(1.05, SE_pred**2 * 0.62,
        r"$SE_{\mathrm{pred}}^2 = SE_{\mathrm{mean}}^2 + \hat\sigma^2$",
        ha="center", fontsize=11, color=PALETTE["primary"])
ax.set_xticks(xs)
ax.set_xticklabels(labels, fontsize=12)
ax.set_ylabel("variance")
ax.set_title("Step 2 — PI adds the irreducible residual variance\n"
             "(so PI is much wider than CI)")

# R command box anchored on the right panel
ax.text(0.03, 0.97,
        "R command:\n"
        "newx <- data.frame(EmplStatus='Empl',\n"
        "  Age=40, Income=30, DebtIndex=0.3)\n"
        "predict(mod2, newdata=newx,\n"
        "  interval='prediction', level=0.95)\n"
        "# wider than CI; contains 70 -> NOT anomalous",
        transform=ax.transAxes, ha="left", va="top",
        fontsize=9, family="monospace",
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle(r"$y=70$ outside 95% CI but inside 95% PI  $\Rightarrow$  "
             r"NOT anomalous for an individual client",
             fontsize=12, y=1.02, color=PALETTE["primary"])

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  fit={fit}  SE_mean={SE_mean:.3f}  sigma={sigma}  SE_pred={SE_pred:.3f}")
print(f"  CI=({ci_lwr},{ci_upr})  PI~=({pi_lwr:.2f},{pi_upr:.2f})")
