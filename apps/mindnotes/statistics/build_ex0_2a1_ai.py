"""Build AI walkthrough plot for Ex 0 / 2a1 — exact proportion of fares in
[10, 60) from a grouped density table on the Titanic dataset.

Both endpoints (10 and 60) align with class boundaries, so the answer is
EXACT — no within-class assumption is required. The figure makes the point
visual:
 - LEFT  : density histogram with the [10,60) region highlighted; the three
           sub-bars contribute (width x density) = (10*.02002), (10*.01580),
           (30*.00419) summing to 0.4839.
 - RIGHT : cumulative proportion (ogive) showing that F(60) - F(10) = 0.4839
           reads directly off two endpoint values — no interpolation needed.

Also uses shutil to clone the real source PNGs (textbook scans of the Ex 2
question page and the a1 answer page) from the local note-taking workspace
into apps/mindnotes/statistics/images/ex0/{questions,answers}/ under the
canonical ex0_2a1_{question,answer}.png filenames, so the snippet can
reference them directly.
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex0"
OUT  = f"{BASE}/ex0_2a1_ai.png"

# ---------- 1. Clone the real source PNGs (textbook scans) ----------
SRC = "/Users/Alessandro/Repos/my note taking app/statistics/ex0"
# NB: macOS screenshot filenames use U+202F (NARROW NO-BREAK SPACE) before "PM",
# not a regular space — must be preserved literally for the lookup to resolve.
NBSP = "\u202f"
pairs = [
    (f"{SRC}/questions/Screenshot 2026-06-05 at 3.47.12{NBSP}PM.png",
     f"{BASE}/questions/ex0_2a1_question.png"),
    (f"{SRC}/answers/Screenshot 2026-06-05 at 3.36.49{NBSP}PM.png",
     f"{BASE}/answers/ex0_2a1_answer.png"),
]
for src, dst in pairs:
    if os.path.exists(src):
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        shutil.copyfile(src, dst)
        print(f"copied -> {dst}")
    else:
        print(f"WARNING: source PNG not found at {src}")

# ---------- 2. Build the AI walkthrough figure ----------
# Fare density table from the prompt (n = 1309 Titanic passengers)
lower = np.array([0.0, 10.0,  20.0, 30.0,  60.0, 100.0, 180.0])
upper = np.array([10.0, 20.0, 30.0, 60.0, 100.0, 180.0, 270.0])
dens  = np.array([0.03765, 0.02002, 0.01580, 0.00419, 0.00196, 0.00044, 0.00029])
width = upper - lower
prop  = dens * width                   # proportion per class
cum   = np.concatenate([[0.0], np.cumsum(prop)])  # cumulative at each endpoint

LO, HI = 10.0, 60.0                    # query interval
mask_in = (lower >= LO) & (upper <= HI)
exact   = float(prop[mask_in].sum())   # 0.4839

# ---------- Figure ----------
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.4))

# =================== LEFT: density histogram ===================
for lo, hi, d in zip(lower, upper, dens):
    is_in = (lo >= LO) and (hi <= HI)
    ax1.bar(lo, d, width=hi - lo, align="edge",
            color=PALETTE["accent"] if is_in else PALETTE["primary"],
            alpha=0.65 if is_in else 0.18,
            edgecolor="#7a6700" if is_in else PALETTE["primary"],
            linewidth=1.4 if is_in else 0.9, zorder=2)

# Threshold lines at 10 and 60
ax1.axvline(LO, color=PALETTE["warn"], lw=1.6, ls="--", zorder=4)
ax1.axvline(HI, color=PALETTE["warn"], lw=1.6, ls="--", zorder=4,
            label=r"endpoints align with class boundaries")

# Annotate each contributing sub-area with its (width x density) product
contribs = [(10, 20, 0.02002, "10$\\times$0.02002\n= 0.2002"),
            (20, 30, 0.01580, "10$\\times$0.01580\n= 0.1580"),
            (30, 60, 0.00419, "30$\\times$0.00419\n= 0.1257")]
for lo, hi, d, txt in contribs:
    ax1.text((lo + hi) / 2, d + 0.0018, txt,
             ha="center", va="bottom", fontsize=9.5,
             color="#7a6700", fontweight="bold")

ax1.set_xlim(-5, 280)
ax1.set_ylim(0, dens.max() * 1.35)
ax1.set_xlabel("fare")
ax1.set_ylabel(r"Density  $c_k = p_k / w_k$")
ax1.set_title(r"Exact: highlighted area = sum of (width $\times$ density)")
ax1.legend(loc="upper right", framealpha=0.95, fontsize=10)
ax1.annotate(fr"$\sum$ = {exact:.4f}",
             xy=(35, dens.max() * 1.18),
             ha="center", va="center",
             fontsize=12, color="#7a6700", fontweight="bold",
             bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                       boxstyle="round,pad=0.35", linewidth=1.0))

# =================== RIGHT: ogive (cumulative proportion) ===================
edges = np.concatenate([[lower[0]], upper])  # 0, 10, 20, 30, 60, 100, 180, 270
ax2.plot(edges, cum, "o-", color=PALETTE["primary"], lw=1.8, markersize=6, zorder=3)

# Mark the two query endpoints
F_LO = cum[1]  # F(10)  = 0.3765
F_HI = cum[4]  # F(60)  = 0.8604
ax2.axvline(LO, color=PALETTE["warn"], lw=1.4, ls="--", zorder=2)
ax2.axvline(HI, color=PALETTE["warn"], lw=1.4, ls="--", zorder=2)
ax2.hlines(F_LO, 0, LO, color=PALETTE["accent"], lw=1.4, ls=":", zorder=2)
ax2.hlines(F_HI, 0, HI, color=PALETTE["accent"], lw=1.4, ls=":", zorder=2)

# Brackets / arrow showing the difference F(60) - F(10)
ax2.annotate("", xy=(8, F_HI), xytext=(8, F_LO),
             arrowprops=dict(arrowstyle="<->", color=PALETTE["accent"], lw=1.8))
ax2.text(14, (F_LO + F_HI) / 2,
         fr"$F(60) - F(10)$" "\n" fr"$= {F_HI:.4f} - {F_LO:.4f}$"
         "\n" fr"$= {exact:.4f}$",
         ha="left", va="center", fontsize=10.5, color="#7a6700",
         fontweight="bold",
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.35", linewidth=1.0))

# Endpoint labels
ax2.scatter([LO, HI], [F_LO, F_HI], s=70,
            color=PALETTE["warn"], edgecolor="#2a3142", zorder=5)
ax2.text(LO, F_LO - 0.05, fr"$F(10)={F_LO:.4f}$",
         ha="left", va="top", fontsize=10, color=PALETTE["warn"])
ax2.text(HI, F_HI + 0.03, fr"$F(60)={F_HI:.4f}$",
         ha="left", va="bottom", fontsize=10, color=PALETTE["warn"])

ax2.set_xlim(-10, 285)
ax2.set_ylim(0, 1.08)
ax2.set_xlabel("fare")
ax2.set_ylabel("Cumulative proportion  $F$")
ax2.set_title("Ogive: F(60) - F(10) reads off directly (no interpolation)")

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  P(10 <= fare < 60) = {exact:.4f}  (exact)")
print(f"  F(10) = {F_LO:.4f},  F(60) = {F_HI:.4f},  diff = {F_HI - F_LO:.4f}")
