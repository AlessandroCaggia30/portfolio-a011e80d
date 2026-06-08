"""Build AI walkthrough plot for Ex 2.8c — dispersion measures of Margin_perc
(customer_habits, n = 34 866) and interpretation alongside the boxplot.

Also uses shutil to clone:
  * the recurring Exercise 2.8 textbook prompt PNG (shared across 2.8a/b/c/d)
    into ex2/questions/ex2_8c_question.png
  * the textbook answer crop for Ex 2.8c into ex2/answers/ex2_8c_answer.png

The snippet references only PNGs that actually exist after this script runs.
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
OUT  = f"{BASE}/ex2_8c_ai.png"

# ---------- 1. Clone the recurring textbook prompt + answer PNGs ----------
# macOS screenshot filenames contain a NARROW NO-BREAK SPACE (U+202F) between
# the time and "PM" / "AM", so we resolve the source by stem match instead of
# hard-coding the raw bytes.
SRC_DIR = "/Users/Alessandro/Repos/my note taking app/statistics/ex2"

def _resolve(folder: str, stem: str) -> str | None:
    """Return absolute path to a file in `folder` whose name (with the U+202F
    narrow no-break space normalized to a regular space) matches `stem`."""
    if not os.path.isdir(folder):
        return None
    for name in os.listdir(folder):
        if name.replace("\u202f", " ") == stem:
            return os.path.join(folder, name)
    return None

PAIRS = [
    (_resolve(f"{SRC_DIR}/questions",
              "Screenshot 2026-06-05 at 4.12.01 PM.png"),
     f"{BASE}/questions/ex2_8c_question.png"),
    (_resolve(f"{SRC_DIR}/answers",
              "Screenshot 2026-06-05 at 4.10.48 PM.png"),
     f"{BASE}/answers/ex2_8c_answer.png"),
]
for src, dst in PAIRS:
    if src and os.path.exists(src):
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        shutil.copyfile(src, dst)
        print(f"copied -> {dst}")
    else:
        print(f"WARNING: source PNG not found for -> {dst}")

# ---------- 2. Numbers from Ex 2.8b / 2.8c (R output in the textbook) ----------
n        = 34866
mn       = -40.71
q1       =   6.57
med      =  17.36
q3       =  29.15
mx       =  99.00
mean_x   =  18.20
sd_x     =  17.82
rng      = mx - mn          # 139.71
iqr      = q3 - q1          # 22.58
var_x    = sd_x ** 2        # 317.5524 (textbook prints 317.44 with 2-dec digits)
cv       = sd_x / mean_x    # 0.9791... (textbook prints 0.98)

# Whiskers (Tukey caps) and what the data actually reach
upper_cap  = q3 + 1.5 * iqr  # ~62.97
lower_cap  = q1 - 1.5 * iqr  # ~-27.30
upper_w    = min(mx, upper_cap)  # whisker stops inside the cap
lower_w    = max(mn, lower_cap)  # whisker stops inside the cap
# (in this dataset both tails extend past the Tukey caps -> many outliers shown)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.4))

# ---------- LEFT: vertical boxplot with the dispersion scales overlaid ----------
box_x = 0.0
box_w = 0.6

# IQR box
ax1.add_patch(Rectangle((box_x - box_w/2, q1), box_w, q3 - q1,
                        facecolor=PALETTE["primary"], alpha=0.28,
                        edgecolor=PALETTE["primary"], linewidth=1.6))
# Median line
ax1.plot([box_x - box_w/2, box_x + box_w/2], [med, med],
         color=PALETTE["warn"], lw=2.4)
# Whiskers (inside Tukey caps)
ax1.plot([box_x, box_x], [q3, upper_cap], color=PALETTE["primary"], lw=1.4)
ax1.plot([box_x, box_x], [q1, lower_cap], color=PALETTE["primary"], lw=1.4)
ax1.plot([box_x - box_w/4, box_x + box_w/4], [upper_cap]*2,
         color=PALETTE["primary"], lw=1.4)
ax1.plot([box_x - box_w/4, box_x + box_w/4], [lower_cap]*2,
         color=PALETTE["primary"], lw=1.4)

# Outlier marks (illustrative — both tails are heavily populated)
np.random.seed(1)
out_hi = np.linspace(upper_cap + 2, mx, 14)
out_lo = np.linspace(mn, lower_cap - 1, 9)
ax1.scatter([box_x]*len(out_hi), out_hi, s=16, color=PALETTE["accent"],
            edgecolor="#2a3142", linewidth=0.4, zorder=5)
ax1.scatter([box_x]*len(out_lo), out_lo, s=16, color=PALETTE["accent"],
            edgecolor="#2a3142", linewidth=0.4, zorder=5)

# Mean ± 1 SD band (the "standard" interval)
ax1.axhspan(mean_x - sd_x, mean_x + sd_x,
            xmin=0.55, xmax=0.95,
            color=PALETTE["accent"], alpha=0.13)
ax1.axhline(mean_x, color=PALETTE["warn"], lw=1.2, ls=":", alpha=0.55)

# Range bracket (left of the box)
ax1.annotate("", xy=(box_x - box_w/2 - 0.55, mx),
             xytext=(box_x - box_w/2 - 0.55, mn),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["muted"], lw=1.5))
ax1.text(box_x - box_w/2 - 0.62, (mn + mx)/2,
         f"range $= {rng:.2f}$",
         color=PALETTE["muted"], fontsize=10.5,
         ha="right", va="center", rotation=90, fontweight="bold")

# IQR bracket (closer to the box on the left)
ax1.annotate("", xy=(box_x - box_w/2 - 0.18, q3),
             xytext=(box_x - box_w/2 - 0.18, q1),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["accent"], lw=1.6))
ax1.text(box_x - box_w/2 - 0.25, (q1 + q3)/2,
         f"IQR $= {iqr:.2f}$",
         color=PALETTE["accent"], fontsize=10.5,
         ha="right", va="center", rotation=90, fontweight="bold")

# Annotations on the right
labels = [
    (mn,           f"min $= {mn}$",                PALETTE["neutral"]),
    (q1,           f"$Q_1 = {q1}$",                PALETTE["primary"]),
    (med,          f"Me $= {med}$",                PALETTE["warn"]),
    (mean_x,       f"$\\bar x = {mean_x}$",        PALETTE["warn"]),
    (q3,           f"$Q_3 = {q3}$",                PALETTE["primary"]),
    (mean_x+sd_x,  f"$\\bar x + \\hat\\sigma \\approx {mean_x+sd_x:.2f}$",
                                                    PALETTE["accent"]),
    (mx,           f"max $= {mx}$",                PALETTE["neutral"]),
]
for y, text, col in labels:
    ax1.annotate(text, xy=(box_x + box_w/2 + 0.05, y),
                 xytext=(box_x + box_w/2 + 0.35, y),
                 ha="left", va="center", fontsize=10, color=col,
                 fontweight="bold",
                 arrowprops=dict(arrowstyle="-", color=col, lw=0.8, alpha=0.7))

ax1.set_xlim(-1.7, 1.55)
ax1.set_ylim(mn - 8, mx + 8)
ax1.set_xticks([])
ax1.set_ylabel("Margin_perc  (%)")
ax1.set_title("Boxplot of Margin_perc  (n = 34 866)  with range and IQR")

# ---------- RIGHT: bar chart comparing the dispersion measures ----------
names  = ["range", "IQR", r"$\hat\sigma$ (sd)", r"$\bar x$",
          r"cv $= \hat\sigma / \bar x$"]
values = [rng, iqr, sd_x, mean_x, cv]
colors = [PALETTE["muted"], PALETTE["accent"], PALETTE["primary"],
          PALETTE["warn"],  PALETTE["secondary"] if "secondary" in PALETTE
                            else PALETTE["accent"]]
# Plot range / IQR / sd / mean on the left axis; cv on a twin axis (unitless)
xs = np.arange(len(names))
bars = ax2.bar(xs[:4], values[:4], color=colors[:4],
               edgecolor="#2a3142", linewidth=0.9, alpha=0.85)
for b, v in zip(bars, values[:4]):
    ax2.text(b.get_x() + b.get_width()/2, b.get_height() + 1.5,
             f"{v:.2f}", ha="center", va="bottom",
             fontsize=10.5, fontweight="bold", color="#2a3142")

ax2b = ax2.twinx()
b_cv = ax2b.bar(xs[4:], values[4:], color=colors[4],
                edgecolor="#2a3142", linewidth=0.9, alpha=0.85)
for b, v in zip(b_cv, values[4:]):
    ax2b.text(b.get_x() + b.get_width()/2, b.get_height() + 0.02,
              f"{v:.2f}", ha="center", va="bottom",
              fontsize=10.5, fontweight="bold", color="#2a3142")
ax2b.set_ylabel("cv (unitless)", color="#2a3142")
ax2b.set_ylim(0, 1.25)

ax2.set_xticks(xs)
ax2.set_xticklabels(names, fontsize=10)
ax2.set_ylabel("% of unit cost")
ax2.set_ylim(0, max(values[:4]) * 1.22)
ax2.set_title("Dispersion measures of Margin_perc")

# Verdict / interpretation box
verdict = (
    fr"$\bullet$ range / IQR $= {rng:.2f}/{iqr:.2f} \approx "
    fr"{rng/iqr:.1f}$  $\Rightarrow$ tails much wider" + "\n"
    fr"   than the central 50%: a concentrated middle." + "\n"
    fr"$\bullet$ $\bar x \pm \hat\sigma = [{mean_x-sd_x:.2f},\,{mean_x+sd_x:.2f}]$:" + "\n"
    fr"   'standard' transactions go from near 0%" + "\n"
    fr"   (sold at cost) to $\approx 36$% markup." + "\n"
    fr"$\bullet$ cv $= {cv:.2f} \approx 1$: scatter is of the same" + "\n"
    fr"   order as the mean $\Rightarrow$ very heterogeneous margins."
)
ax2.text(0.02, 0.97, verdict, transform=ax2.transAxes,
         ha="left", va="top", fontsize=9.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  range = {rng:.4f}   IQR = {iqr:.4f}")
print(f"  sd    = {sd_x:.4f}  var = {var_x:.4f}  cv  = {cv:.4f}")
print(f"  mean  = {mean_x:.4f}  [mean-sd, mean+sd] = "
      f"[{mean_x-sd_x:.4f}, {mean_x+sd_x:.4f}]")
