"""Build the AI walkthrough plot for Ex 1.1d — choice of intervals for Sales histogram.

The plot makes the point of the answer **visual**:

    LEFT  -- Sales histogram with 15 equal-width breaks: y-axis can be
             Counts OR Density (proportional), the *height* of each bar
             is itself proportional to the class frequency.

    MIDDLE -- The same data binned with the custom widths
             (10,15,20,25,35,50,70) x1000: y-axis is **Density**
             f_i / w_i, so the *area* of each bar is the class frequency
             (the relative frequency f_i is annotated on top of each bar).

    RIGHT  -- The **same** custom-width bins but plotted as raw COUNTS
             on the y-axis. This is the classic mistake the question
             warns against: wider classes get tall bars that *visually*
             dominate, exaggerating their share.

A footer card spells out the rule:
    equal widths  ->  height proportional to frequency, ANY y-axis OK
    unequal widths -> only DENSITY (f_i / w_i) preserves the
                      "area = frequency" reading.
"""
import os, sys, shutil
import numpy as np
import matplotlib.pyplot as plt
import pyreadr

sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

apply_style()

# ----------------------------------------------------------------------
# 0. Paths
# ----------------------------------------------------------------------
NBSP    = "\u202f"
SRC_Q   = f"/Users/Alessandro/Repos/my note taking app/statistics/ex1/questions/Screenshot 2026-06-05 at 3.49.25{NBSP}PM.png"
SRC_A   = f"/Users/Alessandro/Repos/my note taking app/statistics/ex1/answers/Screenshot 2026-06-05 at 3.51.57{NBSP}PM.png"
DST_DIR = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex1"
os.makedirs(f"{DST_DIR}/questions", exist_ok=True)
os.makedirs(f"{DST_DIR}/answers",   exist_ok=True)

DST_Q   = f"{DST_DIR}/questions/ex1_1_1d_question.png"
DST_A   = f"{DST_DIR}/answers/ex1_1_1d_answer.png"
DST_AI  = f"{DST_DIR}/ex1_1_1d_ai.png"

# ----------------------------------------------------------------------
# 1. Clone Q + A screenshots (via /tmp staging, U+202F preserved)
# ----------------------------------------------------------------------
for src, dst in [(SRC_Q, DST_Q), (SRC_A, DST_A)]:
    tmp = f"/tmp/{os.path.basename(dst)}"
    shutil.copyfile(src, tmp)
    shutil.copyfile(tmp, dst)
    print(f"copied -> {dst}")

# ----------------------------------------------------------------------
# 2. Data
# ----------------------------------------------------------------------
RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata"
pizz  = pyreadr.read_r(RDATA)["pizzerie"]
sales = pizz["Sales"].to_numpy()

custom_breaks = np.array([0, 10_000, 15_000, 20_000, 25_000, 35_000, 50_000, 70_000])
custom_widths = np.diff(custom_breaks)   # 10k, 5k, 5k, 5k, 10k, 15k, 20k

# Class frequencies for custom bins
counts_c, _ = np.histogram(sales, bins=custom_breaks)
n           = counts_c.sum()
relfreq_c   = counts_c / n
density_c   = relfreq_c / custom_widths

print("\nCustom-bin table:")
print("  class                width    count    f_i      density")
for (lo, hi), w, c, f, d in zip(zip(custom_breaks[:-1], custom_breaks[1:]),
                                custom_widths, counts_c, relfreq_c, density_c):
    print(f"  [{lo:>5}, {hi:>5})   {w:>5}    {c:>5}    {f:.3f}    {d:.2e}")

# ----------------------------------------------------------------------
# 3. AI plot — 3 panels
# ----------------------------------------------------------------------
fig, axes = plt.subplots(1, 3, figsize=(15.5, 5.6),
                         gridspec_kw={"width_ratios": [1, 1.15, 1.15]})

# ----- LEFT: 15 equal-width bins (counts == proportional to density) -----
ax = axes[0]
ax.hist(sales, bins=15, color=PALETTE["secondary"],
        edgecolor=PALETTE["primary"], linewidth=0.8)
ax.set_xlabel("Sales")
ax.set_ylabel("Counts")
ax.set_title("15 equal-width bins\n(too noisy, but heights = frequencies)",
             fontsize=11.5, color=PALETTE["primary"])
ax.text(0.97, 0.95,
        "Equal widths $w_i = w$:\n"
        r"$\;$counts, percent, density"
        "\n"
        r"$\;$all give the same shape.",
        transform=ax.transAxes, ha="right", va="top",
        fontsize=9.5, color=PALETTE["neutral"], style="italic",
        bbox=dict(boxstyle="round,pad=0.35", facecolor="#fbfbfd",
                  edgecolor=PALETTE["neutral"], linewidth=0.7))

# ----- MIDDLE: custom widths, y = DENSITY (correct) -----
ax = axes[1]
for (lo, hi), d, f in zip(zip(custom_breaks[:-1], custom_breaks[1:]),
                          density_c, relfreq_c):
    ax.bar(lo, d, width=hi - lo, align="edge",
           color=PALETTE["secondary"],
           edgecolor=PALETTE["primary"], linewidth=0.9)
    ax.text((lo + hi) / 2, d + density_c.max() * 0.03,
            f"$f={f:.2f}$",
            ha="center", va="bottom", fontsize=9,
            color=PALETTE["primary"], fontweight="bold")
ax.set_xlabel("Sales")
ax.set_ylabel("Density  $f_i / w_i$")
ax.set_ylim(0, density_c.max() * 1.22)
ax.set_title("Custom widths — y = DENSITY (correct)\n"
             "AREA of each bar = relative frequency",
             fontsize=11.5, color=PALETTE["ok"])
ax.text(0.97, 0.95,
        "Unequal widths $w_i$:\n"
        r"only $f_i / w_i$ keeps "
        r"area $=$ freq.",
        transform=ax.transAxes, ha="right", va="top",
        fontsize=9.5, color=PALETTE["ok"], style="italic",
        bbox=dict(boxstyle="round,pad=0.35", facecolor="#f4faf6",
                  edgecolor=PALETTE["ok"], linewidth=0.7))

# ----- RIGHT: custom widths but y = COUNTS (the mistake) -----
ax = axes[2]
for (lo, hi), c in zip(zip(custom_breaks[:-1], custom_breaks[1:]), counts_c):
    ax.bar(lo, c, width=hi - lo, align="edge",
           color="#fad7c8",
           edgecolor=PALETTE["warn"], linewidth=0.9, hatch="///")
    ax.text((lo + hi) / 2, c + counts_c.max() * 0.025,
            f"{c}",
            ha="center", va="bottom", fontsize=9.5,
            color=PALETTE["warn"], fontweight="bold")
ax.set_xlabel("Sales")
ax.set_ylabel("Counts  (WRONG axis for unequal widths)")
ax.set_ylim(0, counts_c.max() * 1.18)
ax.set_title("Custom widths — y = COUNTS (misleading)\n"
             "wide bins look 'tall', distorting the shape",
             fontsize=11.5, color=PALETTE["warn"])
ax.text(0.97, 0.95,
        "Same area $\\neq$ same\n"
        "height when $w_i$ differ\n"
        "$\\rightarrow$ visual overstatement\n"
        "of the wide tail bins.",
        transform=ax.transAxes, ha="right", va="top",
        fontsize=9.5, color=PALETTE["warn"], style="italic",
        bbox=dict(boxstyle="round,pad=0.35", facecolor="#fdecea",
                  edgecolor=PALETTE["warn"], linewidth=0.7))

# ----- Footer card with the rule -----
fig.text(0.5, -0.04,
         r"Rule.  Histogram bar area $= f_i$ always; bar height $= f_i / w_i$.  "
         r"If $w_i \equiv w$, height $\propto f_i$, so counts / percent / density all look identical.  "
         r"If widths differ, only the DENSITY axis preserves area $=$ frequency.",
         ha="center", va="top", fontsize=10.5,
         color=PALETTE["primary"],
         bbox=dict(boxstyle="round,pad=0.55",
                   facecolor="#fbfbfd",
                   edgecolor=PALETTE["primary"], linewidth=1.0))

plt.tight_layout(rect=[0, 0.04, 1, 1])
plt.savefig(DST_AI, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {DST_AI}")
