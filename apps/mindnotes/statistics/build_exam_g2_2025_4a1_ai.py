"""AI walkthrough for G2-2025 Ex4.a — modA coefficients & 5% significance."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE, PALETTE_CYCLE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g2_2025_4a1_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 2 2025/Data_G_250129.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
R_CMD = (
    f'load("{RDATA}"); '
    f'modA <- lm(Productivity ~ Training_Attended+Satisfaction+Hours_Worked+'
    f'Tenure+Remote_Work+Salary, data=Employee); '
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

# Drop intercept for the bar chart of slope coefficients (scale issue)
order = ["Training_Attended", "Satisfaction", "Hours_Worked", "Tenure",
         "Remote_Work", "Salary"]
idx = [names.index(n) for n in order]
est_s = [est[i] for i in idx]
se_s = [se[i] for i in idx]
p_s = [p[i] for i in idx]
sig = [pp < 0.05 for pp in p_s]

# Scale Salary so bars are comparable (Salary coef is tiny: ~0.0015)
display_est = []
display_se = []
display_label = []
for nm, e, s in zip(order, est_s, se_s):
    if nm == "Salary":
        display_est.append(e * 1000)
        display_se.append(s * 1000)
        display_label.append("Salary\n(×1000)")
    else:
        display_est.append(e)
        display_se.append(s)
        display_label.append(nm)

fig, ax = plt.subplots(figsize=(11, 6))
colors = [PALETTE["ok"] if s else PALETTE["neutral"] for s in sig]
bars = ax.bar(display_label, display_est,
              yerr=[1.96 * s for s in display_se],
              color=colors, edgecolor=PALETTE["primary"], alpha=0.85,
              capsize=4, ecolor=PALETTE["primary"])
ax.axhline(0, color=PALETTE["primary"], lw=1.0)
for i, (b, e, pp, s) in enumerate(zip(bars, display_est, p_s, sig)):
    star = "*" if s else "n.s."
    ax.text(b.get_x() + b.get_width()/2,
            e + (0.08 if e >= 0 else -0.18),
            f"$\\hat\\beta={est_s[i]:.4g}$\np={pp:.3g} {star}",
            ha="center",
            va="bottom" if e >= 0 else "top",
            fontsize=9.5, color=PALETTE["primary"], fontweight="bold")
ax.set_ylabel(r"$\hat\beta_j$  (with 95% CI bars)")
ax.set_title("modA coefficients — green = significant at 5%, grey = not")
ax.text(0.02, 0.97,
        "modA: lm(Productivity ~ Train + Sat + Hours + Tenure + Remote + Salary)\n"
        f"n = 500,  df = 493,  $R^2$ = 0.0795,  adj $R^2$ = 0.0683\n"
        "Significant at 5%: Satisfaction (+), Hours_Worked (-),\n"
        "  Tenure (-), Salary (+).  Not significant: Training, Remote.",
        transform=ax.transAxes, ha="left", va="top",
        fontsize=10, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.4", linewidth=1.0))
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
