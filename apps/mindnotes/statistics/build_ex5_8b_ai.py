"""Build AI walkthrough plot for Ex 5.8b — estimated SE of the sample mean
of Company$Profitability (n = 668, s_x = 365.3158, xbar = 930.27), the
point estimate, and why the SE does NOT tell us how far the realized xbar
is from the unknown mu_X. Three panels: (1) the sample with xbar and
+-s_x band, and the SE plug-in formula; (2) sampling distribution of Xbar
centred on the UNKNOWN mu (illustrated with several plausible mu values);
(3) many resampled xbars from one plausible mu, showing SE = typical
spread, not the deviation of OUR specific realized xbar."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex5/ex5_5_8b_ai.png"

# --- Numbers from the exercise ---
n      = 668
xbar   = 930.27
s_x    = 365.3158
se_hat = s_x / np.sqrt(n)               # ~ 14.1338

# --- Reconstruct a plausible sample for the LEFT panel (matched xbar, s_x) ---
rng     = np.random.default_rng(20260606)
raw     = rng.normal(loc=xbar, scale=s_x, size=n)
raw     = (raw - raw.mean()) / raw.std(ddof=1)   # affine rescale -> exact xbar, s_x
sample  = raw * s_x + xbar

fig, (ax1, ax2, ax3) = plt.subplots(1, 3, figsize=(16, 5))

# ====================================================================
# LEFT — sample histogram with xbar and +-s_x band
# ====================================================================
ax1.hist(sample, bins=40, color=PALETTE["primary"], alpha=0.55,
         edgecolor="#2a3142", linewidth=0.4,
         label=fr"sample, $n={n}$")
ax1.axvline(xbar, color=PALETTE["accent"], lw=2.0,
            label=fr"$\bar x = {xbar:.2f}$")
ax1.axvspan(xbar - s_x, xbar + s_x, color=PALETTE["accent"], alpha=0.10,
            label=fr"$\bar x \pm s_x$  ($s_x = {s_x:.2f}$)")
ax1.set_xlabel("Profitability")
ax1.set_ylabel("count")
ax1.set_title(r"Step 1 — Sample: read off  $\bar x$  and  $s_x$")
ax1.legend(loc="upper right", framealpha=0.95, fontsize=9)
ax1.text(0.03, 0.97,
         fr"$\widehat{{SE}}(\bar X) = s_x/\sqrt{{n}}$" + "\n"
         fr"$= {s_x:.2f}/\sqrt{{{n}}}$" + "\n"
         fr"$\approx {se_hat:.2f}$",
         transform=ax1.transAxes, ha="left", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.40", linewidth=1.0))

# ====================================================================
# MIDDLE — sampling distribution of Xbar centred on the UNKNOWN mu
# ====================================================================
mu_candidates = [910.0, 930.0, 950.0]
colors_mu     = [PALETTE["primary"], PALETTE["accent"], PALETTE["neutral"]]
xs            = np.linspace(870, 990, 600)
for mu_c, col in zip(mu_candidates, colors_mu):
    ax2.plot(xs, norm.pdf(xs, loc=mu_c, scale=se_hat),
             color=col, lw=2.0, alpha=0.85,
             label=fr"$\bar X \sim N(\mu={mu_c:.0f},\, {se_hat:.2f}^2)$")
    ax2.axvline(mu_c, color=col, ls="--", lw=1.0, alpha=0.7)
ax2.axvline(xbar, color="#b85c00", lw=2.2,
            label=fr"observed $\bar x = {xbar:.2f}$")
ax2.set_xlabel(r"$\bar x$")
ax2.set_ylabel("density")
ax2.set_title(r"Step 2 — Sampling distribution centred on UNKNOWN $\mu$")
ax2.legend(loc="upper right", framealpha=0.95, fontsize=8.5)
ax2.text(0.03, 0.97,
         r"$|\bar x - \mu|$ depends on" + "\n" + r"unknown $\mu$ $\Rightarrow$" + "\n"
         + r"deviation is unknown",
         transform=ax2.transAxes, ha="left", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.40", linewidth=1.0))

# ====================================================================
# RIGHT — many resample xbars from ONE plausible mu
# ====================================================================
mu_plot   = 930.0
B         = 2000
xbar_sim  = rng.normal(loc=mu_plot, scale=se_hat, size=B)
ax3.hist(xbar_sim, bins=40, color=PALETTE["primary"], alpha=0.55,
         edgecolor="#2a3142", linewidth=0.4,
         label=fr"$B={B}$ resampled $\bar x$")
ax3.axvline(mu_plot, color=PALETTE["neutral"], ls="--", lw=1.4,
            label=fr"assumed $\mu = {mu_plot:.0f}$")
ax3.axvspan(mu_plot - se_hat, mu_plot + se_hat, color=PALETTE["accent"],
            alpha=0.18, label=fr"$\mu \pm \widehat{{SE}}$  $(\widehat{{SE}}\approx{se_hat:.2f})$")
ax3.axvline(xbar, color="#b85c00", lw=2.2,
            label=fr"our $\bar x = {xbar:.2f}$")
ax3.set_xlabel(r"$\bar x$  (across hypothetical resamples)")
ax3.set_ylabel("count")
ax3.set_title(r"Step 3 — SE = typical spread, not  $|\bar x - \mu|$  for THIS sample")
ax3.legend(loc="upper right", framealpha=0.95, fontsize=8.5)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  xbar   = {xbar:.4f}")
print(f"  s_x    = {s_x:.4f}")
print(f"  SE_hat = {se_hat:.4f}")
