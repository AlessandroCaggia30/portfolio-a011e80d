"""AI walkthrough for G2-2025 Ex3 — One-sample z-test on Remote_Work share."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE, PALETTE_CYCLE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g2_2025_3a_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 2 2025/Data_G_250129.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Employee[, c("Remote_Work"), drop=FALSE], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
rw = []
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        rw.append(int(float(row["Remote_Work"])))
os.unlink(tmp.name)

n = len(rw)
x = sum(rw)
phat = x / n
p0 = 0.30
se0 = np.sqrt(p0 * (1 - p0) / n)
z_obs = (phat - p0) / se0
pval = 2 * norm.sf(abs(z_obs))
alpha = 0.05
zcrit = norm.ppf(1 - alpha / 2)

# Two-panel figure
fig = plt.figure(figsize=(12, 5.5))
gs = fig.add_gridspec(1, 2, width_ratios=[1.0, 1.4])

# Left: bar chart of Remote_Work counts vs H0 expected (under p0=0.30)
ax1 = fig.add_subplot(gs[0, 0])
labels = ["Remote (1)", "Non-remote (0)"]
obs = [x, n - x]
exp = [n * p0, n * (1 - p0)]
xx = np.arange(2)
w = 0.36
b1 = ax1.bar(xx - w/2, obs, w, color=PALETTE["secondary"], alpha=0.85,
             edgecolor=PALETTE["primary"], label="Observed")
b2 = ax1.bar(xx + w/2, exp, w, color=PALETTE["neutral"], alpha=0.7,
             edgecolor=PALETTE["primary"], label=fr"Expected under $H_0$ ($p_0={p0}$)")
for b, v in zip(b1, obs):
    ax1.text(b.get_x() + b.get_width()/2, v + 4, f"{v}",
             ha="center", va="bottom", fontsize=10, color=PALETTE["primary"],
             fontweight="bold")
for b, v in zip(b2, exp):
    ax1.text(b.get_x() + b.get_width()/2, v + 4, f"{v:.0f}",
             ha="center", va="bottom", fontsize=10, color=PALETTE["neutral"])
ax1.set_xticks(xx); ax1.set_xticklabels(labels)
ax1.set_ylabel("count")
ax1.set_title(f"Remote_Work: $\\hat p = {x}/{n} = {phat:.3f}$  vs  $p_0 = 0.30$")
ax1.legend(framealpha=0.95)

# Right: N(0,1) null density, rejection regions, observed z
ax2 = fig.add_subplot(gs[0, 1])
# Cap the x-axis at +/-6 even though z_obs may be larger; we annotate the spike at the edge.
xlim = max(6.0, abs(z_obs) + 0.5)
xs = np.linspace(-xlim, xlim, 800)
ys = norm.pdf(xs)
ax2.plot(xs, ys, color=PALETTE["primary"], lw=2.0, label=r"$N(0,1)$ null density")
# rejection regions
left = xs <= -zcrit; right = xs >= zcrit
ax2.fill_between(xs[left], 0, ys[left], color=PALETTE["warn"], alpha=0.35)
ax2.fill_between(xs[right], 0, ys[right], color=PALETTE["warn"], alpha=0.35,
                 label=fr"Rejection region ($|z|>{zcrit:.2f}$, $\alpha=0.05$)")
# observed z
ax2.axvline(z_obs, color=PALETTE["accent"], lw=2.2,
            label=fr"$z_{{obs}} = {z_obs:+.3f}$")
ax2.axvline(-z_obs, color=PALETTE["accent"], lw=1.0, ls=":")
# exam-stated z
z_exam = -1.624
ax2.axvline(z_exam, color=PALETTE["neutral"], lw=1.6, ls="--",
            label=fr"exam-stated $z = {z_exam:.3f}$")
ax2.set_xlabel("z")
ax2.set_ylabel("density")
ax2.set_title("Two-sided one-proportion z-test")
ax2.legend(loc="upper right", framealpha=0.95, fontsize=9.5)
ax2.text(0.02, 0.97,
         f"$z = (\\hat p - p_0)/\\sqrt{{p_0(1-p_0)/n}}$\n"
         f"  $= ({phat:.3f}-{p0})/{se0:.5f}$\n"
         f"  $= {z_obs:+.3f}$\n"
         f"p-value $= 2\\Phi(-|z|) = {pval:.2e}$\n"
         f"Decision: reject $H_0$ at 5%.",
         transform=ax2.transAxes, ha="left", va="top",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

fig.suptitle(r"G2-2025 Ex3 — $H_0: p_{\rm Remote}=0.30$ vs $H_1: p\ne 0.30$",
             fontsize=13, color=PALETTE["primary"], y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"n={n} x={x} phat={phat:.4f} z_obs={z_obs:.4f} p={pval:.4e}")
