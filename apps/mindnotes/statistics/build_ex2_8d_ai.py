"""Build AI walkthrough plot for Ex 2.8d — three claims on the sign of Margin_perc.

Visual: a "checklist" diagram aligned to the boxplot / ECDF of Margin_perc so
each of the three textbook claims can be verified geometrically.

  Claim 1: P(Margin_perc > 0)   >= 0.75   -> TRUE   (84.97%)
  Claim 2: P(10 <= MP <= 30)    >= 0.60   -> FALSE  (44.18%)
  Claim 3: P(MP < -30)          <= 0.10   -> TRUE   ( 0.22%)

Also uses shutil to clone:
  * the recurring Exercise 2.8 textbook prompt PNG (shared across 2.8a/b/c/d)
    into ex2/questions/ex2_8d_question.png
  * the textbook answer crop for Ex 2.8d into ex2/answers/ex2_8d_answer.png

The snippet references only PNGs that actually exist after this script runs.
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle, FancyBboxPatch

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
OUT  = f"{BASE}/ex2_8d_ai.png"

# ---------- 1. Clone the recurring textbook prompt + answer PNGs ----------
# macOS Screenshot filenames use U+202F (narrow no-break space) between the
# time and "PM" / "AM", so resolve sources by stem match.
SRC_DIR = "/Users/Alessandro/Repos/my note taking app/statistics/ex2"

def _resolve(folder: str, stem: str):
    if not os.path.isdir(folder):
        return None
    for name in os.listdir(folder):
        if name.replace("\u202f", " ") == stem:
            return os.path.join(folder, name)
    return None

PAIRS = [
    (_resolve(f"{SRC_DIR}/questions",
              "Screenshot 2026-06-05 at 4.12.01 PM.png"),
     f"{BASE}/questions/ex2_8d_question.png"),
    (_resolve(f"{SRC_DIR}/answers",
              "Screenshot 2026-06-05 at 4.10.53 PM.png"),
     f"{BASE}/answers/ex2_8d_answer.png"),
]
for src, dst in PAIRS:
    if src and os.path.exists(src):
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        shutil.copyfile(src, dst)
        print(f"copied -> {dst}")
    else:
        print(f"WARNING: source PNG not found for -> {dst}")

# ---------- 2. Numbers from Ex 2.8b / 2.8c / 2.8d (R output) ----------
n        = 34866
mn       = -40.71
q1       =   6.57
med      =  17.36
q3       =  29.15
mx       =  99.00
mean_x   =  18.20
sd_x     =  17.82
p10      =  -3.89

p_pos    = 0.8497103   # mean(Margin_perc > 0)
p_in     = 0.4418345   # mean(Margin_perc in [10,30])
p_lo     = 0.002208455 # mean(Margin_perc < -30)

claims = [
    {"id": "1", "txt": r"$P(\mathrm{MP} > 0) \geq 75\%$",
     "p": p_pos, "thr": 0.75, "kind": "ge",
     "verdict": "TRUE",  "support": f"$Q_1 = {q1}\\% > 0 \\Rightarrow \\geq 75\\%$ already positive",
     "lo": 0, "hi": None},
    {"id": "2", "txt": r"$P(10 \leq \mathrm{MP} \leq 30) \geq 60\%$",
     "p": p_in,  "thr": 0.60, "kind": "ge",
     "verdict": "FALSE", "support": r"$Q_1=6.57$, $Q_3=29.15$ $\Rightarrow$ band [10,30] $\subset$ IQR $\Rightarrow$ $<50\%$",
     "lo": 10, "hi": 30},
    {"id": "3", "txt": r"$P(\mathrm{MP} < -30) \leq 10\%$",
     "p": p_lo,  "thr": 0.10, "kind": "le",
     "verdict": "TRUE",  "support": f"$p_{{10}}={p10}\\% \\Rightarrow P(\\mathrm{{MP}}<-30) < 10\\%$",
     "lo": None, "hi": -30},
]

# ---------- 3. Figure ----------
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.6, 6.4),
                               gridspec_kw={"width_ratios": [1.0, 1.1]})

# =========================================================================
# LEFT — boxplot of Margin_perc with the three threshold bands overlaid
# =========================================================================
box_x = 0.0
box_w = 0.55
iqr   = q3 - q1
upper_cap = q3 + 1.5 * iqr
lower_cap = q1 - 1.5 * iqr

# IQR box
ax1.add_patch(Rectangle((box_x - box_w/2, q1), box_w, q3 - q1,
                        facecolor=PALETTE["primary"], alpha=0.28,
                        edgecolor=PALETTE["primary"], linewidth=1.6))
ax1.plot([box_x - box_w/2, box_x + box_w/2], [med, med],
         color=PALETTE["warn"], lw=2.4)
# Whiskers (Tukey caps)
ax1.plot([box_x, box_x], [q3, upper_cap], color=PALETTE["primary"], lw=1.4)
ax1.plot([box_x, box_x], [q1, lower_cap], color=PALETTE["primary"], lw=1.4)
ax1.plot([box_x - box_w/4, box_x + box_w/4], [upper_cap]*2,
         color=PALETTE["primary"], lw=1.4)
ax1.plot([box_x - box_w/4, box_x + box_w/4], [lower_cap]*2,
         color=PALETTE["primary"], lw=1.4)
# Outlier marks
out_hi = np.linspace(upper_cap + 2, mx, 14)
out_lo = np.linspace(mn, lower_cap - 1, 9)
ax1.scatter([box_x]*len(out_hi), out_hi, s=14, color=PALETTE["accent"],
            edgecolor="#2a3142", linewidth=0.4, zorder=5)
ax1.scatter([box_x]*len(out_lo), out_lo, s=14, color=PALETTE["accent"],
            edgecolor="#2a3142", linewidth=0.4, zorder=5)

# --- Threshold bands aligned to the three claims ---
# Claim 1: MP > 0  (TRUE -> green tint above 0)
ax1.axhspan(0, mx + 8, xmin=0.62, xmax=0.78,
            color="#2e9e6a", alpha=0.18)
ax1.axhline(0, color="#2e9e6a", lw=1.2, ls="--")
ax1.text(1.05, mx + 4, "Claim 1\n$\\mathrm{MP}>0$\nTRUE 84.97%",
         color="#2e9e6a", fontsize=9.5, fontweight="bold",
         ha="left", va="top")

# Claim 2: 10 <= MP <= 30 (FALSE)
ax1.axhspan(10, 30, xmin=0.20, xmax=0.45,
            color=PALETTE["warn"], alpha=0.20)
ax1.axhline(10, color=PALETTE["warn"], lw=1.0, ls=":")
ax1.axhline(30, color=PALETTE["warn"], lw=1.0, ls=":")
ax1.text(-1.55, 20, "Claim 2\n$10 \\leq \\mathrm{MP} \\leq 30$\nFALSE 44.18%",
         color=PALETTE["warn"], fontsize=9.5, fontweight="bold",
         ha="left", va="center")

# Claim 3: MP < -30 (TRUE -> tiny tail)
ax1.axhspan(mn - 8, -30, xmin=0.62, xmax=0.78,
            color="#2e9e6a", alpha=0.18)
ax1.axhline(-30, color="#2e9e6a", lw=1.2, ls="--")
ax1.text(1.05, -32, "Claim 3\n$\\mathrm{MP}<-30$\nTRUE 0.22%",
         color="#2e9e6a", fontsize=9.5, fontweight="bold",
         ha="left", va="top")

# Annotations on the right (quantiles)
labels = [
    (q1,  f"$Q_1 = {q1}$",          PALETTE["primary"]),
    (med, f"Me $= {med}$",          PALETTE["warn"]),
    (q3,  f"$Q_3 = {q3}$",          PALETTE["primary"]),
    (p10, f"$p_{{10}} = {p10}$",    PALETTE["accent"]),
]
for y, text, col in labels:
    ax1.annotate(text, xy=(box_x + box_w/2 + 0.05, y),
                 xytext=(box_x + box_w/2 + 0.35, y),
                 ha="left", va="center", fontsize=10, color=col,
                 fontweight="bold",
                 arrowprops=dict(arrowstyle="-", color=col, lw=0.8, alpha=0.7))

ax1.set_xlim(-1.7, 2.1)
ax1.set_ylim(mn - 12, mx + 12)
ax1.set_xticks([])
ax1.set_ylabel("Margin_perc  (%)")
ax1.set_title("Boxplot with the three claim bands overlaid")

# =========================================================================
# RIGHT — checklist: observed proportion vs. threshold for each claim
# =========================================================================
ax2.set_xlim(0, 1.02)
ax2.set_ylim(0, 3.0)
ax2.set_yticks([])
ax2.set_xlabel("Proportion of transactions")
ax2.set_title("Observed proportion vs. claim threshold")

for i, c in enumerate(claims):
    y0 = 2.4 - i * 0.95
    # Observed proportion bar
    ax2.add_patch(Rectangle((0, y0), c["p"], 0.30,
                            facecolor=PALETTE["primary"], alpha=0.78,
                            edgecolor="#2a3142", linewidth=0.8))
    # Threshold marker
    ax2.axvline(c["thr"], ymin=(y0 - 0.05) / 3.0,
                ymax=(y0 + 0.40) / 3.0,
                color=PALETTE["warn"], lw=1.8, ls="--")
    ax2.text(c["thr"] + 0.005, y0 + 0.36,
             f"threshold {c['thr']:.0%}",
             color=PALETTE["warn"], fontsize=9, fontweight="bold")

    # Verdict badge
    badge_color = "#2e9e6a" if c["verdict"] == "TRUE" else "#c0392b"
    ax2.add_patch(FancyBboxPatch((0.86, y0 + 0.40), 0.14, 0.20,
                                 boxstyle="round,pad=0.02",
                                 facecolor=badge_color, alpha=0.85,
                                 edgecolor="#2a3142", linewidth=0.9))
    ax2.text(0.93, y0 + 0.50, c["verdict"],
             color="white", ha="center", va="center",
             fontsize=10.5, fontweight="bold")

    # Claim text
    ax2.text(0.0, y0 + 0.74, f"Claim {c['id']}:  {c['txt']}",
             fontsize=11, fontweight="bold", color=PALETTE["primary"])
    # Observed proportion label
    ax2.text(c["p"] + 0.01, y0 + 0.15, f"{c['p']*100:.2f}%",
             fontsize=10, fontweight="bold", color="#2a3142", va="center")
    # Support text
    ax2.text(0.0, y0 - 0.05, c["support"],
             fontsize=9, color=PALETTE["neutral"], va="top")

ax2.grid(axis="x", alpha=0.35)

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  P(MP > 0)        = {p_pos:.4f}   (claim 1: >= 0.75 -> "
      f"{'TRUE' if p_pos >= 0.75 else 'FALSE'})")
print(f"  P(10 <= MP <= 30) = {p_in:.4f}   (claim 2: >= 0.60 -> "
      f"{'TRUE' if p_in  >= 0.60 else 'FALSE'})")
print(f"  P(MP < -30)      = {p_lo:.4f}   (claim 3: <= 0.10 -> "
      f"{'TRUE' if p_lo  <= 0.10 else 'FALSE'})")
