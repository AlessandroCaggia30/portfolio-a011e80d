"""AI walkthrough for G2-2025 Ex1.b3 — Estimator of the proportion of Senior employees."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE, PALETTE_CYCLE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g2_2025_1b3_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 2 2025/Data_G_250129.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Employee[, c("Role"), drop=FALSE], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
roles = []
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        roles.append(row["Role"])
os.unlink(tmp.name)

n = len(roles)
counts = {r: roles.count(r) for r in ["Junior", "Senior", "Manager"]}
x_sen = counts["Senior"]
phat = x_sen / n
se = np.sqrt(phat * (1 - phat) / n)

fig = plt.figure(figsize=(11, 5.5))
gs = fig.add_gridspec(1, 2, width_ratios=[1.1, 1.4])

# Left: bar chart of role counts
ax1 = fig.add_subplot(gs[0, 0])
order = ["Junior", "Senior", "Manager"]
vals = [counts[r] for r in order]
colors = [PALETTE["neutral"], PALETTE["accent"], PALETTE["secondary"]]
bars = ax1.bar(order, vals, color=colors, edgecolor=PALETTE["primary"], linewidth=1.2, alpha=0.85)
for b, v in zip(bars, vals):
    ax1.text(b.get_x() + b.get_width()/2, v + 4, f"{v}\n({v/n:.1%})",
             ha="center", va="bottom", fontsize=10, color=PALETTE["primary"],
             fontweight="bold")
ax1.set_ylabel("count")
ax1.set_title("Role counts in Employee (n=500)")
ax1.set_ylim(0, max(vals) * 1.18)

# Right: sampling distribution of p-hat with Wald 95% band
ax2 = fig.add_subplot(gs[0, 1])
xs = np.linspace(phat - 4*se, phat + 4*se, 400)
ys = norm.pdf(xs, loc=phat, scale=se)
ax2.plot(xs, ys, color=PALETTE["primary"], lw=2.0,
         label=f"$N(\\hat p,\\,\\hat p(1-\\hat p)/n)$")
lo = phat - 1.96 * se; hi = phat + 1.96 * se
mask = (xs >= lo) & (xs <= hi)
ax2.fill_between(xs[mask], 0, ys[mask], color=PALETTE["accent"], alpha=0.35,
                 label=f"95% Wald CI = [{lo:.3f}, {hi:.3f}]")
ax2.axvline(phat, color=PALETTE["secondary"], lw=2.0,
            label=f"$\\hat p = {x_sen}/{n} = {phat:.3f}$")
ax2.set_xlabel(r"$\hat p$  (sample Senior proportion)")
ax2.set_ylabel("density")
ax2.set_title(r"Estimator $\hat p = X/n$ and its Wald 95% CI")
ax2.legend(loc="upper right", framealpha=0.95, fontsize=9.5)
ax2.text(0.02, 0.97,
         f"$X = {x_sen}$  Senior employees\n"
         f"$n = {n}$\n"
         f"$\\hat p = X/n = {phat:.3f}$\n"
         f"$\\widehat{{SE}}(\\hat p) = \\sqrt{{\\hat p(1-\\hat p)/n}} = {se:.4f}$",
         transform=ax2.transAxes, ha="left", va="top",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

fig.suptitle("G2-2025 Ex1.b3 — Estimator of P(Role = Senior)", fontsize=13,
             color=PALETTE["primary"], y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
