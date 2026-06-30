"""AI walkthrough for G1-2024 Ex1.c3 — SchoolLoc dummies (vs inner-city)."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2024_1c3_ai.png"

# (label, beta, SE, t, p)  -- baseline = inner-city
rows = [
    ("rural",    16.2779, 2.9795, 5.463, 6.19e-08),
    ("suburban",  8.8609, 3.3602, 2.637, 8.52e-03),
    ("urban",    13.6573, 5.2314, 2.611, 9.20e-03),
]
df_res = 824; tc = stats.t.ppf(0.975, df_res)
labels = [r[0] for r in rows]
beta = np.array([r[1] for r in rows])
se   = np.array([r[2] for r in rows])
lo, hi = beta - tc*se, beta + tc*se

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 5.8),
                               gridspec_kw={"width_ratios":[1.1, 1.0]})

x = np.arange(len(labels))
ax1.bar(x, beta, color=[PALETTE["accent"], PALETTE["secondary"], PALETTE["ok"]],
        alpha=0.8, edgecolor="black", width=0.55)
ax1.errorbar(x, beta, yerr=[beta-lo, hi-beta], fmt="none",
             ecolor=PALETTE["primary"], elinewidth=2.0, capsize=8)
for xi, b, p in zip(x, beta, [r[4] for r in rows]):
    stars = "***" if p < 0.001 else ("**" if p < 0.01 else "*")
    ax1.text(xi, b + 0.9, f"+{b:.2f}{stars}", ha="center",
             fontsize=12, color=PALETTE["primary"], fontweight="bold")
ax1.axhline(0, color="black", lw=1.0)
ax1.set_xticks(x); ax1.set_xticklabels([f"{l}\n(vs inner-city)" for l in labels])
ax1.set_ylabel(r"$\hat\beta$  (extra Read2 points vs inner-city baseline)")
ax1.set_title("G1-2024 Ex1.c3 — SchoolLoc dummies (95% CI bars)")
ax1.set_ylim(0, 28)

# Right: implied mean Read2 (intercept-anchored) for each loc, holding other vars at 0/baseline
base = 246.17 + 0.617*528 + 0.423*12.68  # ~ at avg Read1, avg Experience, male, non-free, inner-city
# Build for the four locs: inner-city (baseline), rural, suburban, urban
locs4 = ["inner-city", "rural", "suburban", "urban"]
adj = [0, 16.28, 8.86, 13.66]
means = [base + a for a in adj]
ax2.bar(locs4, means, color=[PALETTE["muted"], PALETTE["accent"], PALETTE["secondary"], PALETTE["ok"]],
        alpha=0.85, edgecolor="black")
for i, (l, m) in enumerate(zip(locs4, means)):
    ax2.text(i, m + 1.5, f"{m:.1f}", ha="center", fontsize=11,
             color=PALETTE["primary"], fontweight="bold")
ax2.set_ylim(min(means)-25, max(means)+15)
ax2.set_ylabel("implied $\\widehat{Read2}$  (avg Read1, avg Experience,\nmale, non-free lunch)")
ax2.set_title("Implied Read2 by SchoolLoc, other predictors fixed")
ax2.text(0.02, 0.97,
         "All 3 dummies positive AND significant at 1%.\n"
         "Inner-city = lowest baseline.\n"
         "Largest gap: rural (+16.3 points).\n"
         "Then: urban (+13.7), suburban (+8.9).",
         transform=ax2.transAxes, ha="left", va="top",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
