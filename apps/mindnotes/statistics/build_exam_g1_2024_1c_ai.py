"""AI walkthrough for G1-2024 Ex1.c — multiple regression Read2 ~ ... (R^2 = 0.586)."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2024_1c_ai.png"

# from R summary(regr.A) - exact numbers
coefs = [
    ("Read1",              0.6170,  0.0224, 27.579, 2.0e-300, True),
    ("Sex: female",        3.0761,  2.0992,  1.465, 0.1432,   False),
    ("Lunch: free",       -2.4412,  2.4983, -0.977, 0.3288,   False),
    ("SchoolLoc: rural", 16.2779,  2.9795,  5.463, 6.2e-08,   True),
    ("SchoolLoc: suburban", 8.8609, 3.3602,  2.637, 8.5e-03,  True),
    ("SchoolLoc: urban", 13.6573,  5.2314,  2.611, 9.2e-03,   True),
    ("Experience",        0.4227,  0.1227,  3.446, 6.0e-04,   True),
]
names = [c[0] for c in coefs]
est = np.array([c[1] for c in coefs])
se  = np.array([c[2] for c in coefs])
sig = [c[5] for c in coefs]
colors = [PALETTE["ok"] if s else PALETTE["muted"] for s in sig]

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14.5, 6),
                               gridspec_kw={"width_ratios":[1.6, 1.0]})

y = np.arange(len(names))[::-1]
ax1.errorbar(est, y, xerr=1.96*se, fmt="o", color="black",
             ecolor=PALETTE["neutral"], elinewidth=2.0, capsize=6, markersize=8)
for yi, e, n, s, c in zip(y, est, names, sig, colors):
    ax1.barh(yi, e, color=c, alpha=0.55, height=0.55, edgecolor="black")
    tag = " *" if s else ""
    ax1.text(e + (1.0 if e >= 0 else -1.0), yi, f"{e:+.3f}{tag}",
             ha="left" if e >= 0 else "right", va="center",
             fontsize=10.5, color=PALETTE["primary"], fontweight="bold")
ax1.axvline(0, color="black", lw=1.0)
ax1.set_yticks(y); ax1.set_yticklabels(names)
ax1.set_xlabel("estimated coefficient  ($\\pm$ 1.96·SE)")
ax1.set_title("G1-2024 Ex1.c — coefficients of regr.A  (Read2 ~ ...)\n"
              "green = significant at 5%,  grey = not significant")

# Right: R^2 panel
R2  = 0.586
adj = 0.5824
explained = R2; resid = 1 - R2
ax2.pie([explained, resid], labels=[f"explained\n{R2*100:.1f}%", f"residual\n{resid*100:.1f}%"],
        colors=[PALETTE["primary"], PALETTE["muted"]],
        wedgeprops=dict(width=0.45, edgecolor="white"),
        startangle=90, textprops=dict(fontsize=12, color=PALETTE["primary"], fontweight="bold"))
ax2.text(0, 0, f"$R^2$\n{R2:.3f}", ha="center", va="center",
         fontsize=18, color=PALETTE["primary"], fontweight="bold")
ax2.set_title(f"Explanatory power\nadj $R^2$ = {adj:.3f},  F$_{{7,824}}$ = 166.6,  p < 2.2e-16")

fig.suptitle("Estimated model: $\\widehat{Read2} = 246.17 + 0.617\\,Read1 + 3.08\\,Sex_F - 2.44\\,Lunch_{\\rm free} + 16.28\\,SL_{\\rm rural} + 8.86\\,SL_{\\rm sub} + 13.66\\,SL_{\\rm urb} + 0.423\\,Exp$",
             fontsize=11.5, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
