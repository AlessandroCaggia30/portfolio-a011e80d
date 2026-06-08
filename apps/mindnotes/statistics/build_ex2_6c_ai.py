"""Build AI walkthrough plot for Ex 2.6c — comparing dispersion of Unit_Price
and Revenue via the coefficient of variation, since the two variables have
different means and units.

Also uses shutil to clone the customer_habits dataset prompt page (already
saved as ex2_5c_question.png — same dataset description used by Ex 2.6 in
the textbook) into ex2/questions/ex2_6c_question.png, so the snippet can
reference it directly. No textbook answer-page PNG exists for 2.6c, so only
the question image is cloned.
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
OUT  = f"{BASE}/ex2_6c_ai.png"

# ---------- 1. Clone the recurring customer_habits prompt PNG via shutil ----------
src_q = f"{BASE}/questions/ex2_5c_question.png"   # same dataset description page
dst_q = f"{BASE}/questions/ex2_6c_question.png"

if os.path.exists(src_q):
    shutil.copyfile(src_q, dst_q)
    print(f"copied -> {dst_q}")

# ---------- 2. Build the AI walkthrough figure ----------
# Numbers verbatim from the snippet's R output
labels = ["Unit_Price", "Revenue"]
mean   = np.array([389.23, 909.72])
sd     = np.array([525.32, 1286.83])
cv     = sd / mean                          # 1.349688..., 1.414531...
# Sanity-checked: matches the textbook printout (1.35, 1.41)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.2))

# ---------- LEFT: mean +/- 1 sd bars showing the absolute-scale trap ----------
x = np.arange(len(labels))
colors = [PALETTE["primary"], PALETTE["accent"]]

ax1.bar(x, mean, width=0.55, color=colors, alpha=0.35,
        edgecolor="#2a3142", linewidth=1.1, label="mean")
ax1.errorbar(x, mean, yerr=sd, fmt="none",
             ecolor=PALETTE["warn"], elinewidth=2.0, capsize=10, capthick=2.0,
             label=r"$\pm 1\,$sd")

for xi, m, s in zip(x, mean, sd):
    ax1.text(xi, m + s + 50, f"sd = {s:.2f}",
             ha="center", va="bottom", fontsize=10.5, fontweight="bold",
             color=PALETTE["warn"])
    ax1.text(xi, m / 2, f"mean = {m:.2f}",
             ha="center", va="center", fontsize=10.5, fontweight="bold",
             color=PALETTE["primary"])

ax1.set_xticks(x)
ax1.set_xticklabels(labels)
ax1.set_ylabel("Value (USD)")
ax1.set_ylim(0, (mean + sd).max() * 1.22)
ax1.set_title("Absolute scale: Revenue has larger mean AND larger sd")
ax1.legend(loc="upper left", framealpha=0.95, fontsize=10)

# ---------- RIGHT: coefficient of variation -- the scale-free comparison ----------
bars = ax2.bar(x, cv, width=0.55, color=colors, alpha=0.85,
               edgecolor="#2a3142", linewidth=1.1)

for b, c in zip(bars, cv):
    ax2.text(b.get_x() + b.get_width()/2, b.get_height() + 0.02,
             f"CV = {c:.3f}", ha="center", va="bottom",
             fontsize=12, fontweight="bold",
             color=PALETTE["accent"] if c == cv.max() else PALETTE["primary"])

ax2.axhline(1.0, color=PALETTE["muted"], lw=1.0, ls=":")
ax2.text(1.45, 1.02, "CV = 1  (sd = mean)",
         color=PALETTE["muted"], fontsize=9.5, ha="right", va="bottom")

ax2.set_xticks(x)
ax2.set_xticklabels(labels)
ax2.set_ylabel(r"Coefficient of variation  CV $=\dfrac{\mathrm{sd}}{\mathrm{mean}}$")
ax2.set_ylim(0, cv.max() * 1.25)
ax2.set_title("Scale-free dispersion: CVs are very close")

# Summary box: the formula and verdict
summary = (
    r"CV $= \mathrm{sd}/\mathrm{mean}$  (unit-free)" + "\n"
    + fr"Unit_Price: $525.32 / 389.23 = {cv[0]:.3f}$" + "\n"
    + fr"Revenue:    $1286.83 / 909.72 = {cv[1]:.3f}$" + "\n"
    + fr"$\Delta$CV $= {cv[1]-cv[0]:+.3f}$  (Revenue only slightly higher)"
)
ax2.text(0.98, 0.02, summary, transform=ax2.transAxes,
         ha="right", va="bottom", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
for lab, m, s, c in zip(labels, mean, sd, cv):
    print(f"  {lab:>10}  mean={m:>8.2f}  sd={s:>8.2f}  CV={c:.4f}")
