"""AI walkthrough for G1-2025 Ex3b — One-sided proportion test:
Doctors with any sleep disorder (Insomnia or Other) > 0.35."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2025_4b_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 1 2025/Data_G_20250108.RData"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Sleep[, c("Occupation","SleepDisorder")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
n, x = 0, 0
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        if row["Occupation"] == "Doctor":
            n += 1
            if row["SleepDisorder"] != "None":
                x += 1
os.unlink(tmp.name)

phat = x / n
p0 = 0.35
se0 = np.sqrt(p0 * (1 - p0) / n)
z_obs = (phat - p0) / se0
pval = 1 - norm.cdf(z_obs)
alpha = 0.01
zcrit = norm.ppf(1 - alpha)

fig = plt.figure(figsize=(12.5, 5.4))
gs = fig.add_gridspec(1, 2, width_ratios=[1.0, 1.2])

# Left: observed disorder rate vs p0
ax1 = fig.add_subplot(gs[0, 0])
labels = ["any disorder\n(Insomnia/Other)", "None"]
obs = [x, n - x]
exp = [n * p0, n * (1 - p0)]
xx = np.arange(2)
w = 0.36
b1 = ax1.bar(xx - w/2, obs, w, color=PALETTE["secondary"], alpha=0.85,
             edgecolor=PALETTE["primary"], label="Observed")
b2 = ax1.bar(xx + w/2, exp, w, color=PALETTE["neutral"], alpha=0.6,
             edgecolor=PALETTE["primary"],
             label=fr"Expected under $H_0$ ($p_0={p0}$)")
for b, v in zip(b1, obs):
    ax1.text(b.get_x() + b.get_width()/2, v + 1.5, f"{v}",
             ha="center", va="bottom", fontsize=10.5,
             color=PALETTE["primary"], fontweight="bold")
for b, v in zip(b2, exp):
    ax1.text(b.get_x() + b.get_width()/2, v + 1.5, f"{v:.1f}",
             ha="center", va="bottom", fontsize=10.5,
             color=PALETTE["neutral"])
ax1.set_xticks(xx); ax1.set_xticklabels(labels)
ax1.set_ylabel("count (Doctors)")
ax1.set_title(f"Doctors: $\\hat p = {x}/{n} = {phat:.3f}$  vs  $p_0 = 0.35$")
ax1.legend(framealpha=0.95)

# Right: N(0,1) null density, ONE-SIDED upper rejection region, observed z
ax2 = fig.add_subplot(gs[0, 1])
xs = np.linspace(-4, 4, 600)
ys = norm.pdf(xs)
ax2.plot(xs, ys, color=PALETTE["primary"], lw=2.0, label=r"$N(0,1)$ null density")
right = xs >= zcrit
ax2.fill_between(xs[right], 0, ys[right], color=PALETTE["warn"], alpha=0.35,
                 label=fr"Upper rejection region ($z>{zcrit:.2f}$, $\alpha=0.01$)")
# Plot a tiny dummy axis-fix to ensure layout
ax2.axvline(z_obs, color=PALETTE["accent"], lw=2.2,
            label=fr"$z_{{obs}} = {z_obs:+.3f}$")
ax2.axvline(zcrit, color=PALETTE["warn"], lw=1.0, ls="--")
ax2.set_xlabel("z")
ax2.set_ylabel("density")
ax2.set_title("One-sided one-proportion z-test (upper)")
ax2.legend(loc="upper right", framealpha=0.95, fontsize=9.5)
ax2.text(0.02, 0.96,
         f"$z = (\\hat p - p_0)/\\sqrt{{p_0(1-p_0)/n}}$\n"
         f"  $= ({phat:.3f}-{p0})/{se0:.4f}$\n"
         f"  $= {z_obs:+.3f}$\n"
         f"p-value $= P(Z\\geq z) = {pval:.3f}$\n"
         f"Decision: DO NOT reject $H_0$ ($p > {alpha}$).",
         transform=ax2.transAxes, ha="left", va="top",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

fig.suptitle(r"G1-2025 Ex3b — $H_0:\,p_{\rm Doctor,\,disorder}\leq 0.35$ vs $H_1:\,p>0.35$",
             fontsize=13, color=PALETTE["primary"], y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"n={n} x={x} phat={phat:.4f} z={z_obs:.4f} p={pval:.4f}")
