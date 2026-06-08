"""Build AI walkthrough plot for Ex 0 / 2b2 — spike plot of size.family.

size.family is a DISCRETE NUMERICAL variable (count of family members
travelling together on the Titanic). The observed support skips two integers
(6 and 7), so the rule of thumb is:

    DISCRETE NUMERICAL  ->  spike plot   (compulsory when missing integers
                                          fall between the min and max,
                                          because an honest plot must show
                                          that empty gap)

This figure makes the point visual by contrasting two plots side by side:
 - LEFT  : the COMPULSORY spike plot. Spikes sit at integer x = 0..8;
           absent integers 6 and 7 leave an obvious gap on the axis.
 - RIGHT : a bar-of-categories plot of the SAME proportions. By dropping
           empty categories, it visually compresses the support and HIDES
           the gap at {6,7} -> misleading for a numerical variable.

Also clones the textbook scans of the Ex 2 question page (the 2b
sub-questions) and the b2 answer page (textbook spike plot) into
apps/mindnotes/statistics/images/ex0/{questions,answers}/ under canonical
ex0_2b2_{question,answer}.png filenames, so the snippet can reference them.
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex0"
OUT  = f"{BASE}/ex0_2b2_ai.png"

# ---------- 1. Clone the real source PNGs (textbook scans) ----------
SRC = "/Users/Alessandro/Repos/my note taking app/statistics/ex0"
# macOS screenshot filenames use U+202F (NARROW NO-BREAK SPACE) before "PM".
NBSP = "\u202f"
pairs = [
    (f"{SRC}/questions/Screenshot 2026-06-05 at 3.47.12{NBSP}PM.png",
     f"{BASE}/questions/ex0_2b2_question.png"),
    (f"{SRC}/answers/Screenshot 2026-06-05 at 3.37.38{NBSP}PM.png",
     f"{BASE}/answers/ex0_2b2_answer.png"),
]
for src, dst in pairs:
    if os.path.exists(src):
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        shutil.copyfile(src, dst)
        print(f"copied -> {dst}")
    else:
        print(f"WARNING: source PNG not found at {src}")

# ---------- 2. Build the AI walkthrough figure ----------
# size.family distribution (n = 1309 Titanic passengers).
# Observed integers: 0,1,2,3,4,5,8.  Missing in the data: 6 and 7.
observed = np.array([0, 1, 2, 3, 4, 5, 8])
counts   = np.array([891, 319, 42, 20, 22, 6, 9])
n        = counts.sum()                  # 1309
prop_obs = counts / n
# Full integer axis 0..8 with proportion 0 at missing levels (6, 7)
full_x   = np.arange(0, 9)
full_p   = np.zeros_like(full_x, dtype=float)
for x, p in zip(observed, prop_obs):
    full_p[x] = p

# ---------- Figure ----------
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.4))

# =================== LEFT: compulsory spike plot ===================
present_mask = full_p > 0
# Solid spikes for observed integers
ax1.vlines(full_x[present_mask], 0, full_p[present_mask],
           color=PALETTE["primary"], lw=2.4, zorder=3)
ax1.scatter(full_x[present_mask], full_p[present_mask],
            s=55, color=PALETTE["primary"], zorder=4)
# Bullet markers AT ZERO for missing integers 6 and 7 (honest "no data" mark)
missing = full_x[~present_mask]
ax1.scatter(missing, np.zeros_like(missing), s=70,
            facecolor="white", edgecolor=PALETTE["warn"],
            linewidths=1.8, zorder=5,
            label="missing integers (6, 7): proportion = 0")

# Annotate the proportion above each spike
for x, p in zip(full_x[present_mask], full_p[present_mask]):
    ax1.text(x, p + 0.018, f"{p:.3f}", ha="center", va="bottom",
             fontsize=9.5, color=PALETTE["primary"], fontweight="bold")

# Shade the gap region at integers 6 and 7
ax1.axvspan(5.5, 7.5, color=PALETTE["warn"], alpha=0.08, zorder=1)
ax1.text(6.5, full_p.max() * 0.55, "gap\n{6, 7}\nnot in data",
         ha="center", va="center", fontsize=10, color=PALETTE["warn"],
         fontweight="bold",
         bbox=dict(facecolor="#fff5f3", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.35", linewidth=1.0))

ax1.set_xticks(full_x)
ax1.set_xlim(-0.6, 8.6)
ax1.set_ylim(0, full_p.max() * 1.22)
ax1.set_xlabel("size.family  (number of family members)")
ax1.set_ylabel("Relative frequency  $p_k$")
ax1.set_title("HONEST: spike plot keeps integers 6 & 7 visible at zero")
ax1.legend(loc="upper right", framealpha=0.95, fontsize=10)

# =================== RIGHT: misleading bar-of-categories ===================
cat_x = np.arange(len(observed))     # 0..6  (collapsed positions)
ax2.bar(cat_x, prop_obs, width=0.72,
        color=PALETTE["accent"], alpha=0.55,
        edgecolor="#7a6700", linewidth=1.2, zorder=2)
for cx, lbl, p in zip(cat_x, observed, prop_obs):
    ax2.text(cx, p + 0.018, f"{p:.3f}", ha="center", va="bottom",
             fontsize=9.5, color="#7a6700", fontweight="bold")

ax2.set_xticks(cat_x)
ax2.set_xticklabels([str(v) for v in observed])
ax2.set_xlim(-0.7, len(observed) - 0.3)
ax2.set_ylim(0, prop_obs.max() * 1.22)
ax2.set_xlabel("size.family  (treated as category -> equal spacing)")
ax2.set_ylabel("Relative frequency  $p_k$")
ax2.set_title("MISLEADING: bar chart hides the gap (8 looks adjacent to 5)")

# Arrow + caption pointing at the suspicious adjacency 5 -> 8
ax2.annotate("",
             xy=(6, 0.05), xytext=(5, 0.05),
             arrowprops=dict(arrowstyle="->", color=PALETTE["warn"],
                             lw=1.8, shrinkA=4, shrinkB=4))
ax2.text(5.5, 0.10, "looks adjacent\nbut 5 -> 8 skips two",
         ha="center", va="bottom", fontsize=10, color=PALETTE["warn"],
         fontweight="bold",
         bbox=dict(facecolor="#fff5f3", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.30", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  n = {n};  proportions sum to {prop_obs.sum():.6f}")
print(f"  observed support: {list(observed)};  missing integers in [0, 8]: [6, 7]")
