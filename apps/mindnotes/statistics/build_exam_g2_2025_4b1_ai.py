"""AI walkthrough for G2-2025 Ex4.b1 — modB Satisfaction coefficient + 99% CI."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import t as tdist

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g2_2025_4b1_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 2 2025/Data_G_250129.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
R_CMD = (
    f'load("{RDATA}"); '
    f'modB <- lm(Productivity ~ Training_Attended+Satisfaction+Hours_Worked+'
    f'Tenure+Remote_Work+Salary+Department+Role, data=Employee); '
    f'co <- summary(modB)$coefficients; '
    f'ci99 <- confint(modB, level=0.99); '
    f'out <- cbind(name=rownames(co), co, ci99[rownames(co), , drop=FALSE]); '
    f'write.csv(out, "{tmp.name}", row.names=FALSE)'
)
subprocess.run(["Rscript", "-e", R_CMD], check=True, capture_output=True)

rows = []
with open(tmp.name) as f:
    rd = csv.reader(f)
    header = next(rd)
    for r in rd:
        rows.append(r)
os.unlink(tmp.name)

# Find Satisfaction
sat = next(r for r in rows if r[0] == "Satisfaction")
beta = float(sat[1]); se = float(sat[2]); tval = float(sat[3]); pval = float(sat[4])
ci_lo = float(sat[5]); ci_hi = float(sat[6])
n = 500; k = 11
df = n - k - 1
tcrit99 = tdist.ppf(0.995, df)
tcrit95 = tdist.ppf(0.975, df)
ci95_lo = beta - tcrit95 * se
ci95_hi = beta + tcrit95 * se

# Build figure: left panel = coefficient with CIs, right panel = sampling distribution
fig = plt.figure(figsize=(12, 5.5))
gs = fig.add_gridspec(1, 2, width_ratios=[1.0, 1.3])

# Left: forest-style coefficient plot
ax1 = fig.add_subplot(gs[0, 0])
ax1.errorbar([beta], [1], xerr=[[beta - ci_lo], [ci_hi - beta]],
             fmt="o", color=PALETTE["accent"], ecolor=PALETTE["accent"],
             capsize=8, lw=2.2, markersize=10,
             label=f"99% CI = [{ci_lo:.3f}, {ci_hi:.3f}]")
ax1.errorbar([beta], [1], xerr=[[beta - ci95_lo], [ci95_hi - beta]],
             fmt="o", color=PALETTE["ok"], ecolor=PALETTE["ok"],
             capsize=5, lw=1.4, markersize=0,
             label=f"95% CI = [{ci95_lo:.3f}, {ci95_hi:.3f}]")
ax1.axvline(0, color=PALETTE["warn"], lw=1.6, ls="--", label=r"$\beta=0$")
ax1.axvline(beta, color=PALETTE["primary"], lw=1.0, ls=":",
            label=fr"$\hat\beta = {beta:.4f}$")
ax1.set_yticks([1]); ax1.set_yticklabels(["Satisfaction"])
ax1.set_xlabel(r"$\hat\beta_{\rm Sat}$  (modB)")
ax1.set_title("Coefficient with 95% and 99% CIs")
ax1.legend(loc="lower right", framealpha=0.95, fontsize=9)
ax1.set_ylim(0.5, 1.5)

# Right: t-distribution with observed t and 1% rejection regions
ax2 = fig.add_subplot(gs[0, 1])
xs = np.linspace(-4, 4, 600)
ys = tdist.pdf(xs, df)
ax2.plot(xs, ys, color=PALETTE["primary"], lw=2.0,
         label=fr"$t_{{{df}}}$ null density")
mask_l = xs <= -tcrit99; mask_r = xs >= tcrit99
ax2.fill_between(xs[mask_l], 0, ys[mask_l], color=PALETTE["warn"], alpha=0.30)
ax2.fill_between(xs[mask_r], 0, ys[mask_r], color=PALETTE["warn"], alpha=0.30,
                 label=fr"1% rejection ($|t|>{tcrit99:.3f}$)")
mask_l5 = xs <= -tcrit95; mask_r5 = xs >= tcrit95
ax2.fill_between(xs[mask_l5], 0, ys[mask_l5], color=PALETTE["accent"], alpha=0.15)
ax2.fill_between(xs[mask_r5], 0, ys[mask_r5], color=PALETTE["accent"], alpha=0.15,
                 label=fr"5% rejection ($|t|>{tcrit95:.3f}$)")
ax2.axvline(tval, color=PALETTE["accent"], lw=2.2,
            label=fr"$t_{{obs}} = {tval:.3f}$")
ax2.set_xlabel("t")
ax2.set_ylabel("density")
ax2.set_title(r"$t$-test for $\beta_{\rm Sat}=0$  (modB)")
ax2.legend(loc="upper right", framealpha=0.95, fontsize=9)
ax2.text(0.02, 0.97,
         f"$\\hat\\beta = {beta:.4f}$,  $SE = {se:.4f}$\n"
         f"$t = {tval:.3f}$,  p-value $= {pval:.4f}$\n"
         f"$t_{{0.995,{df}}} = {tcrit99:.3f}$\n"
         f"99% CI $= \\hat\\beta \\pm t_{{0.995,{df}}}\\,SE$\n"
         f"$\\quad = [{ci_lo:.3f},\\,{ci_hi:.3f}]$\n"
         r"$\Rightarrow$ contains 0: not sig. at 1%.",
         transform=ax2.transAxes, ha="left", va="top",
         fontsize=9.5, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

fig.suptitle("G2-2025 Ex4.b1 — Satisfaction in modB and its 99% CI",
             fontsize=13, color=PALETTE["primary"], y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"beta={beta:.4f} se={se:.4f} 99%CI=[{ci_lo:.4f},{ci_hi:.4f}]")
