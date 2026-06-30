"""AI walkthrough for G1-2024 Ex1.a2 — 99% CI for diff in mean Read2 (Lunch)."""
import os, sys, subprocess, tempfile, csv, collections
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2024_1a2_ai.png"
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/general 1 2024/Data_General_202401.Rdata"

tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(Primary[, c("Read2","Lunch")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
groups = collections.defaultdict(list)
with open(tmp.name) as f:
    for row in csv.DictReader(f):
        groups[row["Lunch"]].append(float(row["Read2"]))
os.unlink(tmp.name)

x1 = np.array(groups["non-free"]); x2 = np.array(groups["free"])
n1, n2 = len(x1), len(x2)
m1, m2 = x1.mean(), x2.mean()
s1, s2 = x1.std(ddof=1), x2.std(ddof=1)
sp = np.sqrt(((n1-1)*s1**2 + (n2-1)*s2**2)/(n1+n2-2))
se = sp*np.sqrt(1/n1 + 1/n2)
diff = m1 - m2
df = n1 + n2 - 2
tc = stats.t.ppf(0.995, df)
lo, hi = diff - tc*se, diff + tc*se

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.8),
                               gridspec_kw={"width_ratios":[1.0, 1.2]})

# Left: per-group means with SD whiskers
ax1.bar(["non-free", "free"], [m1, m2],
        color=[PALETTE["primary"], PALETTE["warn"]], alpha=0.75, edgecolor="black",
        yerr=[s1, s2], capsize=8, error_kw=dict(elinewidth=1.5, ecolor=PALETTE["neutral"]))
for i, (m, n) in enumerate([(m1, n1), (m2, n2)]):
    ax1.text(i, m + 4, f"$\\bar x$ = {m:.2f}\n(n = {n})",
             ha="center", fontsize=10.5, color=PALETTE["primary"], fontweight="bold")
ax1.set_ylabel("Read2 (mean ± 1 sd)")
ax1.set_title("Group means of Read2 by Lunch")
ax1.set_ylim(0, max(m1, m2) + 90)

# Right: difference + CI
yloc = 0
ax2.errorbar([diff], [yloc], xerr=[[diff-lo],[hi-diff]],
             fmt="o", color=PALETTE["primary"], ecolor=PALETTE["primary"],
             elinewidth=3, capsize=12, markersize=11)
ax2.axvline(0, ls="--", color=PALETTE["warn"], lw=1.6, label="zero (no diff)")
ax2.axvspan(lo, hi, color=PALETTE["accent"], alpha=0.25,
            label=f"99% CI = [{lo:.2f}, {hi:.2f}]")
ax2.text(diff, yloc + 0.35, f"$\\bar x_1 - \\bar x_2$ = {diff:.2f}",
         ha="center", fontsize=11.5, color=PALETTE["primary"], fontweight="bold")
ax2.set_ylim(-1, 1.5)
ax2.set_yticks([])
ax2.set_xlabel("difference in mean Read2  (non-free  −  free)")
ax2.set_title("Pooled 99% CI for $\\mu_{\\text{non-free}}-\\mu_{\\text{free}}$")
ax2.legend(loc="upper right", framealpha=0.95)
ax2.text(0.02, 0.97,
         f"$n_1$ = {n1},  $n_2$ = {n2}\n"
         f"$s_p$ = {sp:.3f},  SE = {se:.3f}\n"
         f"$t_{{0.995, {df}}}$ = {tc:.3f}\n"
         f"CI: {diff:.3f}  ±  {tc:.3f}·{se:.3f}\n"
         f"   =  [{lo:.2f},  {hi:.2f}]\n"
         f"0 ∉ CI  =>  reject $H_0$ at 1%",
         transform=ax2.transAxes, ha="left", va="top",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle("G1-2024 Ex1.a2 — 99% CI for difference in mean Read2 (Lunch: non-free vs free)",
             fontsize=12.5, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (diff={diff:.3f}, CI=[{lo:.3f},{hi:.3f}])")
