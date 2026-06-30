"""AI walkthrough for G1-2025 Ex3a — 99% CI for difference of mean SleepDuration,
Nurses vs Doctors."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2025_4a_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 1 2025/Data_G_20250108.RData"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Sleep[, c("Occupation","SleepDuration")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
nurse, doctor = [], []
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        v = float(row["SleepDuration"])
        if row["Occupation"] == "Nurse":
            nurse.append(v)
        elif row["Occupation"] == "Doctor":
            doctor.append(v)
os.unlink(tmp.name)
nurse, doctor = np.array(nurse), np.array(doctor)

n_N, m_N, s_N = len(nurse), nurse.mean(), nurse.std(ddof=1)
n_D, m_D, s_D = len(doctor), doctor.mean(), doctor.std(ddof=1)
diff = m_N - m_D
se = np.sqrt(s_N**2 / n_N + s_D**2 / n_D)
zc = norm.ppf(0.995)
ci_lo, ci_hi = diff - zc * se, diff + zc * se

fig = plt.figure(figsize=(12.5, 5.6))
gs = fig.add_gridspec(1, 2, width_ratios=[1.0, 1.1])

# Left: group means with 99% CI bars (per-group SE = s/sqrt(n))
ax1 = fig.add_subplot(gs[0, 0])
se_N = s_N / np.sqrt(n_N)
se_D = s_D / np.sqrt(n_D)
groups = ["Doctor", "Nurse"]
means = [m_D, m_N]
ses = [se_D, se_N]
colors = [PALETTE["warn"], PALETTE["ok"]]
x = np.arange(2)
ax1.bar(x, means, color=colors, alpha=0.55,
        edgecolor=PALETTE["primary"], width=0.55)
ax1.errorbar(x, means, yerr=[zc * s for s in ses], fmt="none",
             ecolor=PALETTE["primary"], elinewidth=2.0, capsize=10, capthick=2.0)
for i, (m, s, n_g) in enumerate(zip(means, ses, [n_D, n_N])):
    ax1.text(i, m + zc * s + 4, f"$\\bar x = {m:.2f}$\n$n = {n_g}$",
             ha="center", va="bottom", fontsize=10.5, color=PALETTE["primary"],
             fontweight="bold")
ax1.set_xticks(x); ax1.set_xticklabels(groups)
ax1.set_ylabel("SleepDuration (minutes)")
ax1.set_title("Group means ± 99% CI (per-group)")
ax1.set_ylim(350, 510)

# Right: number-line of the 99% CI for the DIFFERENCE
ax2 = fig.add_subplot(gs[0, 1])
xs = np.linspace(min(ci_lo - 5, -5), ci_hi + 8, 400)
ax2.axhline(0, color="#bbbbbb", lw=0.8)
ax2.axvline(0, color=PALETTE["neutral"], lw=1.0, ls="--",
            label="$\\Delta = 0$ (no difference)")
# CI bar
ax2.plot([ci_lo, ci_hi], [0, 0], color=PALETTE["ok"], lw=6, solid_capstyle="butt",
         label=f"99% CI: ({ci_lo:.2f}, {ci_hi:.2f}) min")
ax2.plot([ci_lo, ci_hi], [0, 0], "|", color=PALETTE["primary"],
         markersize=18, markeredgewidth=2.0)
ax2.plot(diff, 0, "o", color=PALETTE["accent"], markersize=11,
         markeredgecolor=PALETTE["primary"], markeredgewidth=1.4,
         label=f"$\\hat\\Delta = {diff:.2f}$ min")
ax2.set_ylim(-1, 1)
ax2.set_yticks([])
ax2.set_xlabel("Nurse − Doctor mean SleepDuration (min)")
ax2.set_title("99% CI strictly above 0 — reject $H_0$ at $\\alpha=0.01$")
ax2.legend(loc="upper right", framealpha=0.95)
ax2.text(0.02, 0.93,
         f"$SE = \\sqrt{{s_N^2/n_N + s_D^2/n_D}}$\n"
         f"$\\;\\;= \\sqrt{{{s_N:.2f}^2/{n_N} + {s_D:.2f}^2/{n_D}}} = {se:.3f}$\n"
         f"99% CI = $\\hat\\Delta \\pm z_{{0.995}}\\cdot SE$\n"
         f"$\\;\\;= {diff:.2f} \\pm {zc:.3f}\\cdot {se:.3f}$\n"
         f"$\\;\\;= ({ci_lo:.2f},\\ {ci_hi:.2f})$ min",
         transform=ax2.transAxes, ha="left", va="top",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

fig.suptitle(r"G1-2025 Ex3a — Mean SleepDuration: $\mu_{\rm Nurse} - \mu_{\rm Doctor}$, 99% CI",
             fontsize=13, color=PALETTE["primary"], y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"n_N={n_N} m_N={m_N:.3f} s_N={s_N:.3f}")
print(f"n_D={n_D} m_D={m_D:.3f} s_D={s_D:.3f}")
print(f"diff={diff:.3f} SE={se:.4f} 99%CI=({ci_lo:.3f}, {ci_hi:.3f})")
