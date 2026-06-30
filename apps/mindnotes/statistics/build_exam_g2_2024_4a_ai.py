"""AI walkthrough for G2-2024 Ex4.a — Chi-square test of independence Region x ClassPBlack."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import chi2

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g2_2024_4a_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 2 2024/Data_General_202402.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
R_CMD = (
    f'load("{RDATA}"); '
    f'tab <- table(CrimeUS$Region, CrimeUS$ClassPBlack); '
    f'm <- as.data.frame.matrix(tab); '
    f'write.csv(cbind(Region=rownames(m), m), "{tmp.name}", row.names=FALSE)'
)
subprocess.run(["Rscript", "-e", R_CMD], check=True, capture_output=True)
regions = []
counts = []
cols = None
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    cols = [c for c in rd.fieldnames if c != "Region"]
    for row in rd:
        regions.append(row["Region"])
        counts.append([float(row[c]) for c in cols])
os.unlink(tmp.name)
O = np.array(counts)
n = O.sum()
row_sum = O.sum(axis=1, keepdims=True)
col_sum = O.sum(axis=0, keepdims=True)
E = row_sum @ col_sum / n
contrib = (O - E)**2 / E
X2 = contrib.sum()
df = (O.shape[0]-1) * (O.shape[1]-1)
pval = 1 - chi2.cdf(X2, df=df)
crit = chi2.ppf(0.95, df=df)

# Reorder rows to NorthEast, NorthCentre, West, South for narrative clarity
desired_order = ["NorthEast", "NorthCentre", "West", "South"]
order_idx = [regions.index(r) for r in desired_order if r in regions]
regions_ord = [regions[i] for i in order_idx]
O_ord = O[order_idx, :]
E_ord = E[order_idx, :]

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.8, 6))

# Left: heatmap of (O - E) standardized residuals
std_res = (O_ord - E_ord) / np.sqrt(E_ord)
vmax = max(abs(std_res.min()), abs(std_res.max()))
im = ax1.imshow(std_res, cmap="RdBu_r", vmin=-vmax, vmax=vmax, aspect="auto")
ax1.set_xticks(range(len(cols))); ax1.set_xticklabels(cols, rotation=30, ha="right")
ax1.set_yticks(range(len(regions_ord))); ax1.set_yticklabels(regions_ord)
ax1.set_title("Standardized residuals  (O - E)/$\\sqrt{E}$")
for i in range(len(regions_ord)):
    for j in range(len(cols)):
        ax1.text(j, i, f"{int(O_ord[i,j])}\n({std_res[i,j]:+.1f})",
                 ha="center", va="center", fontsize=8.5,
                 color="black" if abs(std_res[i,j]) < 3 else "white")
fig.colorbar(im, ax=ax1, fraction=0.04, pad=0.04)

# Right: chi-square distribution under H0
xx = np.linspace(0, max(X2*1.05, crit*2), 600)
yy = chi2.pdf(xx, df=df)
ax2.plot(xx, yy, color=PALETTE["primary"], lw=2.2,
         label=f"$\\chi^2_{{{df}}}$ density under $H_0$")
mask = xx >= crit
ax2.fill_between(xx[mask], 0, yy[mask], color=PALETTE["warn"], alpha=0.4,
                 label=f"rejection region $X^2 > {crit:.2f}$")
ax2.axvline(X2, color=PALETTE["warn"], lw=2.5,
            label=f"$X^2_{{obs}} = {X2:.2f}$ (far in tail)")
ax2.set_xlabel("$X^2$ statistic")
ax2.set_ylabel("density")
ax2.set_title(f"Decision:  $X^2_{{obs}}={X2:.2f}\\ \\gg\\ {crit:.2f}$  $\\Rightarrow$  REJECT $H_0$")
ax2.legend(loc="upper right", framealpha=0.95)
ax2.text(0.02, 0.97,
         f"$H_0$: Region $\\perp$ ClassPBlack\n"
         f"$H_1$: not independent\n"
         f"$df=(4-1)(6-1)={df}$\n"
         f"$X^2_{{obs}}={X2:.2f}$, p-value $< 2.2\\!\\times\\! 10^{{-16}}$\n"
         f"crit. at 5%: $\\chi^2_{{0.95,{df}}}={crit:.3f}$\n\n"
         "South: heavy concentration in high\nClassPBlack classes\n"
         "(largest +residuals in (10,20] and up).",
         transform=ax2.transAxes, ha="left", va="top", fontsize=9.5,
         color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

fig.suptitle("G2-2024 Ex4.a — Chi-square independence:  Region vs ClassPBlack",
             fontsize=12.5, color=PALETTE["primary"], y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (X2={X2:.4f}, df={df}, p={pval:.3g})")
