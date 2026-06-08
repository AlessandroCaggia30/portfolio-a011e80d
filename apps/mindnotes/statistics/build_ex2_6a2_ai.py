"""Build AI walkthrough plot for Ex 2.6a2 — five-number summary + boxplot of
`Revenue` (customer_habits, n = 34 866).

The snippet already shows the textbook boxplot (statistics/images/
ex2_6a2-revenue-boxplot.png). This AI walkthrough makes two extra points
visual, both of which are essentially impossible to read off the original
graph alone:

LEFT
  The five-number summary is drawn AS A BOXPLOT in the original orientation
  (horizontal) but overlaid on the data density (a thin histogram strip),
  so the viewer sees WHY the right whisker collapses to the upper fence and
  WHY the dots stretch out to the right.  Annotations:
    * Q1, Me, Q3 with their numerical values from the snippet text;
    * IQR bracket above the box;
    * lower fence (= Q1 - 1.5*IQR, clipped at min = 0.67);
    * upper fence (= Q3 + 1.5*IQR);
    * shaded right-tail band [upper fence, max] = "extreme values".

RIGHT
  A zoomed-out "outlier census" using a log-scaled count axis, so the
  asymmetry is no longer hidden inside the right tail.  We split Revenue
  into four meaningful bands and show the count and share of each:
      [min, Q1)     bottom 25 %
      [Q1, Q3]      central 50 % (the box)
      (Q3, fence]   top 25 %, "regular" upper whisker
      (fence, max]  Tukey outliers (the dots on the boxplot)
  This is the answer to the snippet's closing remark — "the boxplot cannot
  show the density of the right tail; the deciles in a1 can".

The script also uses shutil to copy the existing textbook boxplot PNG into
the ex2/ subfolder under the canonical ex2_6a2 filename, so it can be
referenced from the snippet at the same logical path as every other 2.x
snippet.  We only reference PNGs that actually got copied / produced.
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import pyreadr
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
OUT  = f"{BASE}/ex2_6a2_ai.png"
os.makedirs(BASE, exist_ok=True)

# ---------------------------------------------------------------
# 1.  Use shutil to make the existing textbook boxplot available
#     under the ex2/ subfolder structure used by every other 2.x
#     snippet.  Only the copies that succeed are referenced below.
# ---------------------------------------------------------------
COPIED = []
src_box = ("/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/"
           "statistics/images/ex2_6a2-revenue-boxplot.png")
dst_box = f"{BASE}/ex2_6a2_boxplot.png"
if os.path.exists(src_box):
    shutil.copyfile(src_box, dst_box)
    COPIED.append(dst_box)
    print(f"copied -> {dst_box}")
else:
    print(f"skip (missing source) -> {src_box}")

# ---------------------------------------------------------------
# 2.  Load Revenue and recompute the five-number summary.
# ---------------------------------------------------------------
result = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata")
rev = result["customer_habits"]["Revenue"].to_numpy()
n   = len(rev)

mn  = float(np.min(rev))
mx  = float(np.max(rev))
q1  = float(np.quantile(rev, 0.25))
med = float(np.median(rev))
q3  = float(np.quantile(rev, 0.75))
iqr = q3 - q1
lower_fence = max(mn, q1 - 1.5 * iqr)
upper_fence = q3 + 1.5 * iqr
n_out = int((rev > upper_fence).sum() + (rev < q1 - 1.5 * iqr).sum())
pct_out = 100 * n_out / n
print(f"  n={n}  min={mn}  Q1={q1}  Me={med}  Q3={q3}  max={mx}")
print(f"  IQR={iqr}  lower_fence={lower_fence}  upper_fence={upper_fence}")
print(f"  n_outliers={n_out}  pct={pct_out:.4f}%")

# ---------------------------------------------------------------
# 3.  Figure.
# ---------------------------------------------------------------
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.4),
                               gridspec_kw={"width_ratios": [1.55, 1]})

# =============== LEFT — annotated boxplot on top of histogram strip ===============
x_max_view = min(mx * 1.02, upper_fence * 3.2)   # keep extremes visible but readable
bins = np.linspace(0, x_max_view, 80)
counts, edges = np.histogram(rev[rev <= x_max_view], bins=bins)
bin_w = edges[1] - edges[0]
# histogram strip drawn at the BOTTOM of the panel
strip_h = 0.45
strip_top = 0.55
ax1.bar(edges[:-1], counts / counts.max() * strip_h,
        width=bin_w, align="edge",
        bottom=strip_top - strip_h,
        color=PALETTE["secondary"], alpha=0.35,
        edgecolor=PALETTE["secondary"], linewidth=0.0,
        label="Revenue density (truncated view)")

# Tukey box at y = 1.10
y_box = 1.20
box_h = 0.25
ax1.add_patch(plt.Rectangle((q1, y_box - box_h / 2), q3 - q1, box_h,
                            facecolor=PALETTE["accent"], alpha=0.55,
                            edgecolor=PALETTE["primary"], linewidth=1.4,
                            zorder=5,
                            label=fr"box $[Q_1, Q_3]$, IQR $= {iqr:.1f}$"))
# Median line
ax1.vlines(med, y_box - box_h / 2, y_box + box_h / 2,
           color=PALETTE["primary"], lw=2.4, zorder=6,
           label=fr"median $= {med:.0f}$")
# Whiskers
ax1.hlines(y_box, lower_fence, q1, color=PALETTE["primary"], lw=1.6, zorder=4)
ax1.hlines(y_box, q3, min(upper_fence, x_max_view),
           color=PALETTE["primary"], lw=1.6, zorder=4)
ax1.vlines(lower_fence, y_box - box_h / 4, y_box + box_h / 4,
           color=PALETTE["primary"], lw=1.6, zorder=4)
ax1.vlines(min(upper_fence, x_max_view), y_box - box_h / 4, y_box + box_h / 4,
           color=PALETTE["primary"], lw=1.6, zorder=4)
# Outlier band
ax1.axvspan(upper_fence, x_max_view, ymin=0.62, ymax=0.92,
            color=PALETTE["warn"], alpha=0.10)
# Sample of outlier dots
extreme = rev[rev > upper_fence]
jitter_y = y_box + (np.random.default_rng(1).uniform(-0.05, 0.05, size=len(extreme)))
ax1.scatter(np.clip(extreme, None, x_max_view), jitter_y,
            s=8, color=PALETTE["warn"], alpha=0.30, zorder=7,
            label=fr"outliers: Revenue $> {upper_fence:.1f}$  ({pct_out:.2f}% of $n$)")

# Tick labels for 5-number summary
ax1.set_yticks([])
for x, tag, col in [
    (mn,  f"min\n{mn:.2f}",                 PALETTE["primary"]),
    (q1,  fr"$Q_1$" + f"\n{q1:.0f}",        PALETTE["primary"]),
    (med, "Me\n" + f"{med:.0f}",            PALETTE["primary"]),
    (q3,  fr"$Q_3$" + f"\n{q3:.1f}",        PALETTE["primary"]),
    (upper_fence, "upper\nfence\n" + f"{upper_fence:.0f}",
                                            PALETTE["warn"]),
]:
    ax1.text(x, 0.05, tag, ha="center", va="bottom",
             fontsize=9.2, color=col, fontweight="bold")

# IQR bracket above box
y_br = y_box + box_h / 2 + 0.10
ax1.annotate("", xy=(q3, y_br), xytext=(q1, y_br),
             arrowprops=dict(arrowstyle="|-|", color=PALETTE["primary"],
                             lw=1.2, shrinkA=0, shrinkB=0))
ax1.text(0.5 * (q1 + q3), y_br + 0.04,
         fr"IQR $= Q_3 - Q_1 = {iqr:.1f}$",
         ha="center", va="bottom", fontsize=10,
         color=PALETTE["primary"], fontweight="bold")

ax1.set_xlim(-x_max_view * 0.02, x_max_view * 1.02)
ax1.set_ylim(0, 1.85)
ax1.set_xlabel("Revenue")
ax1.set_title("Ex 2.6a2 — Boxplot of Revenue, 5-number summary annotated")
ax1.legend(loc="upper right", framealpha=0.95, fontsize=9.0)
ax1.grid(False)

# =============== RIGHT — outlier census table on log-count scale ===============
bands = [
    (mn,   q1,                "[min, $Q_1$)",     PALETTE["secondary"]),
    (q1,   q3,                "[$Q_1$, $Q_3$]",   PALETTE["accent"]),
    (q3,   upper_fence,       "(${Q_3}$, fence]", PALETTE["ok"]),
    (upper_fence, mx + 1,     "(fence, max]",     PALETTE["warn"]),
]
labels, counts_b, shares = [], [], []
for lo, hi, lab, _ in bands:
    if lab.endswith("$Q_3$]"):
        m = (rev >= lo) & (rev <= hi)
    elif lab.startswith("[min"):
        m = (rev >= lo) & (rev < hi)
    elif lab.startswith("(${Q_3}$"):
        m = (rev > lo) & (rev <= hi)
    else:
        m = (rev > lo)
    c = int(m.sum())
    labels.append(lab)
    counts_b.append(c)
    shares.append(100 * c / n)

xpos = np.arange(len(bands))
colors = [b[3] for b in bands]
bars = ax2.bar(xpos, counts_b, width=0.62, color=colors,
               edgecolor="#2a3142", linewidth=0.9, alpha=0.85)
ax2.set_yscale("log")
ax2.set_ylim(50, max(counts_b) * 4)

for b, c, s in zip(bars, counts_b, shares):
    ax2.text(b.get_x() + b.get_width() / 2, c * 1.18,
             f"{c:,}\n({s:.2f}%)",
             ha="center", va="bottom", fontsize=10.0,
             color="#2a3142", fontweight="bold")

ax2.set_xticks(xpos)
ax2.set_xticklabels(labels, fontsize=10.2)
ax2.set_ylabel("Number of transactions  (log scale)")
ax2.set_title("Why the boxplot hides the right-tail density")

note = (
    r"$\bf{Five\!-\!number\ summary}$"
    "\n"
    fr"min $= {mn:.2f}$,  $Q_1 = {q1:.0f}$,  Me $= {med:.0f}$,"
    "\n"
    fr"$Q_3 = {q3:.1f}$,  max $= {mx:.0f}$"
    "\n"
    fr"IQR $= {iqr:.1f}$"
    "\n"
    fr"upper fence $= Q_3 + 1.5\,$IQR $= {upper_fence:.1f}$"
)
ax2.text(0.02, 0.98, note, transform=ax2.transAxes,
         ha="left", va="top", fontsize=9.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
COPIED.append(OUT)
print(f"saved -> {OUT}")
print(f"PNGs available for snippet: {COPIED}")
