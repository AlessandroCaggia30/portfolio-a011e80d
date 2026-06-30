"""AI walkthrough for G2-2024 Ex3.b — Size effect (dummy contrasts vs Large)."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import t as student_t

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g2_2024_3b_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 2 2024/Data_General_202402.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
R_CMD = (
    f'load("{RDATA}"); '
    f'modA <- lm(CrimeProperty ~ PctYoung+PctTertiary+PctDivorce+'
    f'IncomeWhite+IncomeBlack+Size, data=CrimeUS); '
    f'co <- summary(modA)$coefficients; '
    f'write.csv(cbind(name=rownames(co), co), "{tmp.name}", row.names=FALSE)'
)
subprocess.run(["Rscript", "-e", R_CMD], check=True, capture_output=True)
names, est, se, p = [], [], [], []
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        names.append(row["name"]); est.append(float(row["Estimate"]))
        se.append(float(row["Std. Error"])); p.append(float(row["Pr(>|t|)"]))
os.unlink(tmp.name)

b_med = est[names.index("SizeMedium")]; se_med = se[names.index("SizeMedium")]
p_med = p[names.index("SizeMedium")]
b_sm = est[names.index("SizeSmall")]; se_sm = se[names.index("SizeSmall")]
p_sm = p[names.index("SizeSmall")]
b_sm_vs_med = b_sm - b_med   # = -40.355

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.8))

# Left: bar chart of dummy contrasts vs Large reference (baseline at 0)
contrasts = ["Large\n(reference)", "Medium - Large", "Small - Large"]
vals = [0.0, b_med, b_sm]
errs = [0.0, 1.96*se_med, 1.96*se_sm]
ps_disp = [None, p_med, p_sm]
cols = [PALETTE["neutral"],
        PALETTE["ok"] if p_med < 0.05 else PALETTE["neutral"],
        PALETTE["ok"] if p_sm < 0.05 else PALETTE["neutral"]]
bars = ax1.bar(contrasts, vals, yerr=errs, capsize=5,
               color=cols, edgecolor=PALETTE["primary"], alpha=0.85,
               ecolor=PALETTE["primary"])
ax1.axhline(0, color=PALETTE["primary"], lw=1.0)
for b, v, pp in zip(bars, vals, ps_disp):
    if pp is None:
        ax1.text(b.get_x()+b.get_width()/2, 2, "ref.",
                 ha="center", va="bottom", fontsize=10, color=PALETTE["primary"])
    else:
        star = "*" if pp < 0.05 else "n.s."
        ax1.text(b.get_x()+b.get_width()/2,
                 v + (3 if v >= 0 else -3),
                 f"$\\hat b={v:.2f}$\np={pp:.3g} {star}",
                 ha="center", va="bottom" if v >= 0 else "top",
                 fontsize=10, color=PALETTE["primary"], fontweight="bold")
ax1.set_ylabel("dummy coefficient (CrimeProperty units)")
ax1.set_title("Size effect — dummy contrasts vs Large baseline")

# Right: the contrast we want: Small - Medium = b_sm - b_med, conceptual
ax2.barh([0], [b_sm_vs_med], color=PALETTE["warn"],
         edgecolor=PALETTE["primary"], alpha=0.85)
ax2.axvline(0, color=PALETTE["primary"], lw=1.0)
ax2.set_yticks([0]); ax2.set_yticklabels(["Small - Medium"])
ax2.set_xlabel("estimated difference (CrimeProperty units)")
ax2.text(b_sm_vs_med, 0.15,
         f"$b_{{Sm}} - b_{{Med}} = {b_sm:.2f} - ({b_med:.2f}) = {b_sm_vs_med:.2f}$",
         ha="center", va="bottom", fontsize=11, color=PALETTE["primary"],
         fontweight="bold")
ax2.text(0.02, 0.97,
         "This contrast is NOT a row of summary(modA).\n"
         "Test it by re-levelling Size to ref='Medium',\n"
         "or via multcomp::glht for the linear contrast.\n\n"
         "Re-levelled t for SizeSmall(ref=Medium) $\\approx -1.95$\n"
         "$\\Rightarrow$ p $\\approx 0.052$  (borderline, n.s. at 5%)",
         transform=ax2.transAxes, ha="left", va="top", fontsize=10,
         color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))
ax2.set_title("Derived contrast:  Small - Medium")

fig.suptitle("G2-2024 Ex3.b — Size effect on CrimeProperty (modA)",
             fontsize=12.5, color=PALETTE["primary"], y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (b_Sm-Large={b_sm:.3f}, b_Sm-Med={b_sm_vs_med:.3f})")
