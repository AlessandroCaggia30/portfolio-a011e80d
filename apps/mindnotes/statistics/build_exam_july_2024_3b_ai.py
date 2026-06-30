"""AI walkthrough for Jul-2024 Ex3.b — coefficient bars with 99% CIs."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2024_3b_ai.png"

# Coefficients & 99% CIs from R (lm Outstate ~ Top10 + Region + Private + Room.Board)
terms = [
    ("Top10",         1.0679,   0.8677,   1.2682),
    ("RegionMidwest", -6.0501, -15.0111,   2.9110),
    ("RegionSouth",  -22.7901, -31.4836, -14.0965),
    ("RegionWest",    0.0195,  -10.8215,  10.8606),
    ("PrivateYes",   29.8038,  21.5679,  38.0396),
    ("Room.Board",    1.1596,   0.8014,   1.5178),
]
names = [t[0] for t in terms]
est   = np.array([t[1] for t in terms])
lo    = np.array([t[2] for t in terms])
hi    = np.array([t[3] for t in terms])
ypos  = np.arange(len(names))

fig, ax = plt.subplots(figsize=(11, 6.5))
colors = []
for n in names:
    if n == "Top10":            colors.append(PALETTE["accent"])
    elif n == "RegionSouth":    colors.append(PALETTE["warn"])
    elif est[names.index(n)] >= 0: colors.append(PALETTE["primary"])
    else:                          colors.append(PALETTE["neutral"])

ax.barh(ypos, est, color=colors, alpha=0.75,
        edgecolor=PALETTE["primary"], linewidth=1.1)
for i, (e, l, h) in enumerate(zip(est, lo, hi)):
    ax.plot([l, h], [i, i], color=PALETTE["primary"], lw=2.0)
    ax.plot([l, l], [i-0.18, i+0.18], color=PALETTE["primary"], lw=2.0)
    ax.plot([h, h], [i-0.18, i+0.18], color=PALETTE["primary"], lw=2.0)
    txt = f"{e:+.3f}  [{l:+.2f}, {h:+.2f}]"
    ha = "left" if e >= 0 else "right"
    dx = 1.0 if e >= 0 else -1.0
    ax.text(h + 1.5 if e >= 0 else l - 1.5, i, txt, ha=ha, va="center",
            fontsize=10, color=PALETTE["primary"])

ax.axvline(0, color=PALETTE["primary"], lw=1.2)
ax.set_yticks(ypos); ax.set_yticklabels(names)
ax.invert_yaxis()
ax.set_xlabel("coefficient estimate  (Outstate in 100 USD)  with 99% CI")
ax.set_title("Jul-2024 Ex3.b — lm(Outstate ~ Top10 + Region + Private + Room.Board)\n"
             "highlighted: Top10 (99% CI) and RegionSouth (dummy)")
ax.set_xlim(min(lo) - 18, max(hi) + 18)

ax.text(0.99, 0.02,
        "RegionSouth  hat_beta = -22.79\n"
        "  Southern colleges charge ~2280 USD\n"
        "  less out-of-state tuition than NE.\n\n"
        "Top10  99% CI = [0.868, 1.268]\n"
        "  excludes 0  => significant at 1%.",
        transform=ax.transAxes, ha="right", va="bottom",
        fontsize=10, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
