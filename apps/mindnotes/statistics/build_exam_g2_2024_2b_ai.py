"""AI walkthrough for G2-2024 Ex2.b — 99% Welch CI for South-NorthEast CrimeProperty."""
import os, sys, subprocess, tempfile, csv
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import t as student_t, norm

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g2_2024_2b_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 2 2024/Data_General_202402.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
R_CMD = (
    f'load("{RDATA}"); '
    f'd <- subset(CrimeUS, Region %in% c("South","NorthEast")); '
    f'write.csv(data.frame(Region=as.character(d$Region), '
    f'CrimeProperty=d$CrimeProperty), "{tmp.name}", row.names=FALSE)'
)
subprocess.run(["Rscript", "-e", R_CMD], check=True, capture_output=True)

south, ne = [], []
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        v = float(row["CrimeProperty"])
        (south if row["Region"] == "South" else ne).append(v)
os.unlink(tmp.name)
south = np.array(south); ne = np.array(ne)

n1, m1, s1 = len(south), south.mean(), south.std(ddof=1)
n2, m2, s2 = len(ne), ne.mean(), ne.std(ddof=1)
diff = m1 - m2
se = np.sqrt(s1**2/n1 + s2**2/n2)
nu = (s1**2/n1 + s2**2/n2)**2 / ((s1**2/n1)**2/(n1-1) + (s2**2/n2)**2/(n2-1))
tq = student_t.ppf(0.995, df=nu)
me = tq * se
lo, hi = diff - me, diff + me

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 6))

# Left: histograms of the two samples
bins = np.linspace(min(south.min(), ne.min()), max(south.max(), ne.max()), 28)
ax1.hist(ne, bins=bins, alpha=0.55, color=PALETTE["secondary"],
         edgecolor=PALETTE["primary"], label=f"NorthEast  n={n2}, $\\bar x$={m2:.1f}")
ax1.hist(south, bins=bins, alpha=0.55, color=PALETTE["warn"],
         edgecolor=PALETTE["primary"], label=f"South     n={n1}, $\\bar x$={m1:.1f}")
ax1.axvline(m2, color=PALETTE["secondary"], lw=2.0, ls="--")
ax1.axvline(m1, color=PALETTE["warn"], lw=2.0, ls="--")
ax1.set_xlabel("CrimeProperty")
ax1.set_ylabel("count")
ax1.set_title("Two regional samples — South sits to the right")
ax1.legend(loc="upper right", framealpha=0.95)

# Right: sampling distribution of (X-bar - Y-bar) under H0 of equality vs the observed
xx = np.linspace(diff - 4*se, diff + 4*se, 600)
yy = student_t.pdf((xx - diff)/se, df=nu) / se
ax2.plot(xx, yy, color=PALETTE["primary"], lw=2.2,
         label="density of $\\bar X - \\bar Y$ (Welch t, scaled)")
ax2.axvline(diff, color=PALETTE["warn"], lw=2.0,
            label=f"point est. {diff:.2f}")
mask = (xx >= lo) & (xx <= hi)
ax2.fill_between(xx[mask], 0, yy[mask], color=PALETTE["ok"], alpha=0.35,
                 label=f"99% CI = [{lo:.2f}, {hi:.2f}]")
ax2.axvline(0, color=PALETTE["neutral"], lw=1.2, ls="--",
            label="$\\mu_S - \\mu_{NE} = 0$ (lies far below CI)")
ax2.set_xlabel("$\\bar X_S - \\bar X_{NE}$")
ax2.set_ylabel("density")
ax2.set_title(f"99% Welch CI:  ME = $t_{{0.005,{nu:.1f}}}\\cdot SE$ = {me:.2f}")
ax2.legend(loc="upper right", framealpha=0.95, fontsize=9)

fig.text(0.5, -0.02,
         f"Answer:  ($\\mu_S - \\mu_{{NE}}$) $\\in$ [{lo:.2f}, {hi:.2f}]  (99% CI) "
         f"— entirely positive $\\Rightarrow$ South > NorthEast",
         ha="center", fontsize=12, color=PALETTE["primary"], fontweight="bold",
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.5", linewidth=1.0))
fig.suptitle("G2-2024 Ex2.b — Welch 99% CI: South vs NorthEast",
             fontsize=12.5, color=PALETTE["primary"], y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (CI=[{lo:.2f}, {hi:.2f}])")
