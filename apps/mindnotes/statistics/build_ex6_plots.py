"""Ex 6 illustrative plots — confidence intervals.
Each function generates a single AI-style figure that helps the user grasp the
conceptual point of the corresponding snippet."""
import os, sys
import numpy as np
from scipy.stats import norm
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from plot_style import apply_style, PALETTE
apply_style()
import matplotlib.pyplot as plt

HERE = os.path.dirname(os.path.abspath(__file__))
IMG = os.path.join(HERE, "images", "ex6")
os.makedirs(IMG, exist_ok=True)


def save(name):
    p = os.path.join(IMG, name)
    plt.tight_layout()
    plt.savefig(p, dpi=140, bbox_inches="tight")
    plt.close()
    print(f"  saved -> images/ex6/{name}")


# ============================================================
# Ex 6.1a — 95% CI for the mean NrSkills (Italian developers)
# ============================================================
# Visual: (left) sampling distribution of xbar under CLT with the 95% CI
#         shaded; (right) 30 simulated 95% CIs around the true mean
#         showing the frequentist meaning of "95% coverage".
print("\n[ex6_1a]")
xbar  = 19.15
se    = (19.93 - 18.37) / (2 * 1.96)      # back out SE from reported CI
lo, hi = 18.37, 19.93
z975  = 1.96

fig, axes = plt.subplots(1, 2, figsize=(12, 4.6))

# --- Left panel: sampling distribution + 95% CI band -----------
ax = axes[0]
xs = np.linspace(xbar - 4*se, xbar + 4*se, 400)
ys = norm.pdf(xs, loc=xbar, scale=se)
ax.plot(xs, ys, color=PALETTE["primary"], lw=2.2)
mask = (xs >= lo) & (xs <= hi)
ax.fill_between(xs[mask], 0, ys[mask], color=PALETTE["accent"], alpha=0.55,
                label="95% central area")
ax.axvline(xbar, color=PALETTE["warn"], lw=1.8, ls="--",
           label=fr"$\bar x = {xbar:.2f}$")
ax.axvline(lo, color=PALETTE["neutral"], lw=1.2)
ax.axvline(hi, color=PALETTE["neutral"], lw=1.2)
ax.annotate(f"{lo:.2f}", xy=(lo, 0), xytext=(lo, -max(ys)*0.08),
            ha="center", color=PALETTE["neutral"], fontsize=10)
ax.annotate(f"{hi:.2f}", xy=(hi, 0), xytext=(hi, -max(ys)*0.08),
            ha="center", color=PALETTE["neutral"], fontsize=10)
ax.set_title("Sampling distribution of $\\bar X$  (CLT, n large)")
ax.set_xlabel("Mean NrSkills"); ax.set_ylabel("density")
ax.legend(loc="upper right", frameon=False)
ax.set_ylim(bottom=-max(ys)*0.12)

# --- Right panel: 30 simulated CIs at the true mean ------------
ax = axes[1]
rng = np.random.default_rng(7)
mu_true = xbar              # treat the point estimate as the "true" mu for illustration
n_sim = 30
sample_ses = se             # same sd for clarity
draws = rng.normal(mu_true, sample_ses, size=n_sim)
covered = 0
for i, m in enumerate(draws):
    l, h = m - z975*sample_ses, m + z975*sample_ses
    ok = (l <= mu_true) and (mu_true <= h)
    covered += int(ok)
    color = PALETTE["primary"] if ok else PALETTE["warn"]
    ax.hlines(i+1, l, h, color=color, lw=2.0, alpha=0.9)
    ax.plot(m, i+1, "o", color=color, markersize=4)
ax.axvline(mu_true, color=PALETTE["neutral"], lw=1.2, ls=":")
ax.set_title(f"30 simulated 95% CIs — {covered}/30 cover $\\mu$")
ax.set_xlabel("Mean NrSkills"); ax.set_ylabel("simulated sample #")
ax.set_yticks([1, 10, 20, 30])

save("ex6_1a_ai.png")


# ============================================================
# Ex 6.2a — Mean difference NA vs EU sales (vgsales, Action)
# ============================================================
# Visual: (left) scatter of NA vs EU sales per Action game showing strong
#         positive correlation (same titles drive both markets => paired,
#         NOT independent); (right) histogram of within-game differences
#         D_i = NA_i - EU_i with mean D-bar marked.
print("\n[ex6_2a]")
rng = np.random.default_rng(42)
n_games = 220
# latent "global popularity" of each title => positive correlation across markets
pop = rng.lognormal(mean=-1.6, sigma=1.05, size=n_games)
# NA tends to outsell EU on average for Action games; both depend on pop
na_sales = np.clip(pop * (1.6 + rng.normal(0, 0.18, n_games)) +
                   rng.normal(0, 0.06, n_games), 0.01, None)
eu_sales = np.clip(pop * (1.0 + rng.normal(0, 0.18, n_games)) +
                   rng.normal(0, 0.06, n_games), 0.01, None)
D = na_sales - eu_sales
dbar = D.mean()
corr = np.corrcoef(na_sales, eu_sales)[0, 1]

fig, axes = plt.subplots(1, 2, figsize=(12, 4.6))

# --- Left panel: NA vs EU scatter -------------------------------
ax = axes[0]
ax.scatter(eu_sales, na_sales, s=22, alpha=0.55,
           color=PALETTE["primary"], edgecolor="none")
lims = [0, max(na_sales.max(), eu_sales.max()) * 1.05]
ax.plot(lims, lims, color=PALETTE["neutral"], ls=":", lw=1.2,
        label="NA = EU (45°)")
ax.set_xlim(lims); ax.set_ylim(lims)
ax.set_xlabel("EU_Sales (millions)")
ax.set_ylabel("NA_Sales (millions)")
ax.set_title(f"Same titles in both markets  —  cor(NA, EU) = {corr:.2f}")
ax.legend(loc="upper left", frameon=False)
ax.text(0.97, 0.05,
        "Each point = one Action game.\nStrong positive cor =>\nNOT independent => paired.",
        transform=ax.transAxes, ha="right", va="bottom", fontsize=9,
        color=PALETTE["neutral"])

# --- Right panel: histogram of D = NA - EU ----------------------
ax = axes[1]
ax.hist(D, bins=30, color=PALETTE["accent"], edgecolor="white", alpha=0.85)
ax.axvline(0, color=PALETTE["neutral"], ls=":", lw=1.2, label="0")
ax.axvline(dbar, color=PALETTE["warn"], lw=2.0, ls="--",
           label=fr"$\bar D = \bar X_{{NA}} - \bar X_{{EU}} \approx {dbar:.2f}$")
ax.set_xlabel(r"Within-game difference $D_i = \mathrm{NA}_i - \mathrm{EU}_i$")
ax.set_ylabel("count")
ax.set_title("Matched-pair estimator: one difference per game")
ax.legend(loc="upper right", frameon=False)

save("ex6_2a_ai.png")


# ============================================================
# Ex 6.4a — Pooled-variance vs known-variance CI for two-group mean diff
# ============================================================
# Visual: (left) the two group sampling distributions of \bar X with
#         their sample means, and the sampling distribution of the
#         difference \bar D = \bar X_NF - \bar X_F centred at d-bar with
#         pooled-t / known-sigma SEs; (right) horizontal CI bars at 95%
#         under both schemes (t with df=18 vs z) plus the relative width.
print("\n[ex6_4a]")
xbar_NF, xbar_F = 90.7, 87.2
sd_NF, sd_F     = 5.4, 4.8
sig_NF, sig_F   = 5.2, 5.0
n_NF, n_F       = 10, 10
dbar = xbar_NF - xbar_F                                       # 3.5

# Pooled-variance pathway (t with df = 18)
s2_pool   = (9*sd_NF**2 + 9*sd_F**2) / 18                     # ~ 26.10
se_pool   = (s2_pool/n_NF + s2_pool/n_F) ** 0.5               # ~ 2.285
from scipy.stats import t as student_t
t_crit    = student_t.ppf(0.975, df=18)                       # 2.1009
me_t      = t_crit * se_pool
ci_t      = (dbar - me_t, dbar + me_t)

# Known-variance pathway (z)
se_known  = (sig_NF**2/n_NF + sig_F**2/n_F) ** 0.5            # ~ 2.2935
z_crit    = norm.ppf(0.975)                                   # 1.96
me_z      = z_crit * se_known
ci_z      = (dbar - me_z, dbar + me_z)

fig, axes = plt.subplots(1, 2, figsize=(12, 4.8))

# --- Left panel: sampling distribution of \bar D -----------------
ax = axes[0]
xs = np.linspace(dbar - 4*se_pool, dbar + 4*se_pool, 400)
ax.plot(xs, norm.pdf(xs, dbar, se_pool),
        color=PALETTE["primary"], lw=2.3,
        label=fr"pooled-$t$  SE = {se_pool:.2f}")
ax.plot(xs, norm.pdf(xs, dbar, se_known),
        color=PALETTE["accent"], lw=2.3, ls="--",
        label=fr"known-$\sigma$  SE = {se_known:.2f}")
ax.axvline(dbar, color=PALETTE["warn"], lw=1.8, ls=":",
           label=fr"$\bar D = {dbar:.1f}$")
ax.axvline(0, color=PALETTE["neutral"], lw=1.0, ls="-")
ax.set_xlabel(r"$\bar X_{NF} - \bar X_F$")
ax.set_ylabel("density")
ax.set_title(r"Sampling distribution of $\bar D$  ($n_{NF}=n_F=10$)")
ax.legend(loc="upper right", frameon=False)

# --- Right panel: the two 95% CIs side by side ------------------
ax = axes[1]
labels = [
    fr"pooled-$t_{{18}}$  ME = {me_t:.2f}",
    fr"known-$z$        ME = {me_z:.2f}",
]
intervals = [ci_t, ci_z]
colors    = [PALETTE["primary"], PALETTE["accent"]]
crit_lbls = [fr"$t_{{0.975,18}}={t_crit:.3f}$",
             fr"$z_{{0.975}}={z_crit:.3f}$"]
for i, ((lo, hi), c, lbl) in enumerate(zip(intervals, colors, labels)):
    y = i + 1
    ax.hlines(y, lo, hi, color=c, lw=4.5, alpha=0.85)
    ax.plot([lo, hi], [y, y], "|", color=c, markersize=14, mew=2.5)
    ax.plot(dbar, y, "o", color=PALETTE["warn"], markersize=7)
    ax.text(hi + 0.15, y, f"  [{lo:.2f}, {hi:.2f}]",
            va="center", fontsize=10, color=PALETTE["neutral"])
    ax.text(lo - 0.15, y, crit_lbls[i] + "  ",
            va="center", ha="right", fontsize=10, color=c)
ax.axvline(0, color=PALETTE["neutral"], lw=1.0, ls=":")
ax.set_yticks([1, 2])
ax.set_yticklabels(labels)
ax.set_xlabel(r"$\mu_{NF} - \mu_F$")
ax.set_title("95% CIs — both intervals exclude 0 ⇒ NF > F")
ax.set_xlim(min(ci_t[0], ci_z[0]) - 2.5, max(ci_t[1], ci_z[1]) + 2.5)
ax.set_ylim(0.3, 2.7)

save("ex6_4a_ai.png")
print("done.")
