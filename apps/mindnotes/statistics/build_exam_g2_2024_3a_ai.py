"""AI walkthrough for G2-2024 Ex3.a — modA coefficients & 5% significance."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g2_2024_3a_ai.png"
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
names, est, se, t, p = [], [], [], [], []
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        names.append(row["name"])
        est.append(float(row["Estimate"]))
        se.append(float(row["Std. Error"]))
        t.append(float(row["t value"]))
        p.append(float(row["Pr(>|t|)"]))
os.unlink(tmp.name)

order = ["PctYoung", "PctTertiary", "PctDivorce", "IncomeWhite",
         "IncomeBlack", "SizeMedium", "SizeSmall"]
idx = [names.index(n) for n in order]
est_s = [est[i] for i in idx]
se_s = [se[i] for i in idx]
p_s = [p[i] for i in idx]
sig = [pp < 0.05 for pp in p_s]

# Scale Income coefficients ( ~0.3, ~-0.15 ) up by 50 so bars are comparable
display_est, display_se, display_label = [], [], []
for nm, e, s in zip(order, est_s, se_s):
    if nm in ("IncomeWhite", "IncomeBlack"):
        display_est.append(e * 50)
        display_se.append(s * 50)
        display_label.append(nm + "\n(\u00d750)")
    else:
        display_est.append(e); display_se.append(s); display_label.append(nm)

fig, ax = plt.subplots(figsize=(12, 6.5))
colors = [PALETTE["ok"] if s else PALETTE["neutral"] for s in sig]
bars = ax.bar(display_label, display_est,
              yerr=[1.96 * s for s in display_se],
              color=colors, edgecolor=PALETTE["primary"], alpha=0.85,
              capsize=4, ecolor=PALETTE["primary"])
ax.axhline(0, color=PALETTE["primary"], lw=1.0)
for i, (b, pp, s) in enumerate(zip(bars, p_s, sig)):
    star = "*" if s else "n.s."
    e_disp = display_est[i]
    ax.text(b.get_x() + b.get_width()/2,
            e_disp + (1.5 if e_disp >= 0 else -2.5),
            f"$\\hat\\beta={est_s[i]:.4g}$\np={pp:.2g} {star}",
            ha="center", va="bottom" if e_disp >= 0 else "top",
            fontsize=9, color=PALETTE["primary"], fontweight="bold")
ax.set_ylabel(r"$\hat\beta_j$  (with 95% CI bars)")
ax.set_title("G2-2024 Ex3.a — modA: lm(CrimeProperty ~ ...)  "
             "green = sig. at 5%, grey = n.s.")
ax.text(0.02, 0.97,
        "modA: CrimeProperty ~ PctYoung+PctTertiary+PctDivorce\n"
        "                    + IncomeWhite+IncomeBlack+Size\n"
        f"n = 485,  df = 477,  $R^2$ = 0.4707,  adj $R^2$ = 0.4629\n"
        "F-test: F(7,477)=60.6, p<2e-16  (globally significant)\n"
        "Significant at 5%: PctYoung(+), PctDivorce(+), PctTertiary(-),\n"
        "  IncomeWhite(+), IncomeBlack(-), SizeSmall(-).\n"
        "Not significant: SizeMedium.",
        transform=ax.transAxes, ha="left", va="top",
        fontsize=9.5, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.4", linewidth=1.0))
plt.xticks(rotation=15, ha="right")
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
