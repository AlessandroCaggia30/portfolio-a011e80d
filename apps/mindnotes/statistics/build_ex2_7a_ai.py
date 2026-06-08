"""Build AI walkthrough plot for Ex 2.7a — reading the ogive of Nr_visits.

Visual: two-panel figure that makes the *meaning* of the ogive concrete —
the y-axis carries the cumulative relative frequency, and each upward jump
at x = x_k equals the relative frequency f_k of that bin.

LEFT panel
  Step-style empirical CDF (ogive) of Nr_visits with:
    - filled circles at each (x_k, F_n(x_k)),
    - vertical "jump arrows" at each x_k highlighting that the increment
      Delta F_n(x_k) is exactly the relative frequency f_k,
    - the largest jump (at x = 12, f = 0.140) coloured in warm yellow and
      annotated,
    - a horizontal dashed guide at F = 0.274 = F_n(3) with the worked
      example "F_n(3) = (193+272+138)/2200 = 0.274" written on the panel.

RIGHT panel
  Card with the (x_k, f_k) table and a short formula card explaining the
  ogive: F_n(x) = sum_{x_k <= x} f_k, and Delta F_n(x_k) = f_k.

Also uses shutil to clone the recurring Ex 2.7 textbook prompt + textbook
answer PNGs (already saved as ex2_7d_question.png / ex2_7d_answer.png,
which contain the full Exercise 2.7 statement + the (a) answer paragraph)
into ex2/{questions,answers}/ex2_7a_*.png so the snippet can reference
them.
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
OUT  = f"{BASE}/ex2_7a_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# ---------- 1. Clone the recurring Ex 2.7 prompt + answer PNGs via shutil ----------
clones = [
    (f"{BASE}/questions/ex2_7d_question.png", f"{BASE}/questions/ex2_7a_question.png"),
    (f"{BASE}/answers/ex2_7d_answer.png",     f"{BASE}/answers/ex2_7a_answer.png"),
]
for src, dst in clones:
    if os.path.exists(src):
        shutil.copyfile(src, dst)
        print(f"copied -> {dst}")
    else:
        print(f"WARN: source missing, skipped -> {src}")

# ---------- 2. Build the AI walkthrough figure ----------
# Nr_visits distribution from the prompt (2200 customers)
x      = np.array([1, 2, 3, 4, 6, 8, 9, 10, 12, 14, 15, 16, 20, 24])
counts = np.array([193, 272, 138, 115, 92, 60, 118, 228, 309, 130, 113, 104, 122, 206])
n      = counts.sum()                          # 2200
prop   = np.round(counts / n, 3)               # f_k
cum    = np.cumsum(counts) / n                 # F_n(x_k)

# Highlight indices
i_big   = int(np.argmax(prop))                 # largest jump -> x = 2, f = 0.124
i_three = int(np.where(x == 3)[0][0])          # worked example crossing
print(f"  n={n}  largest jump at x={x[i_big]} (f={prop[i_big]:.3f})")
print(f"  F_n(3) = {cum[i_three]:.3f}")

# =====================================================================
# Layout
# =====================================================================
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.6),
                               gridspec_kw={"width_ratios": [2.2, 1]})

# =====================================================================
# LEFT — ogive with jump-arrows at every bin
# =====================================================================
xs_step = np.concatenate([[x[0] - 0.5, x[0]], x])
ys_step = np.concatenate([[0.0,         0.0], cum])

ax1.step(xs_step, ys_step, where="post",
         color=PALETTE["primary"], lw=1.9, label="Ogive  $F_n(x)$")
ax1.scatter(x, cum, color=PALETTE["primary"], s=24, zorder=4)

# Worked-example horizontal guide at F_n(3) = 0.274
ax1.axhline(cum[i_three], color=PALETTE["accent"], lw=1.2, ls="--", alpha=0.85)
ax1.text(25.2, cum[i_three], fr"$F_n(3)={cum[i_three]:.3f}$",
         va="center", ha="left", fontsize=9.5, color=PALETTE["accent"])
ax1.axvline(3, color=PALETTE["accent"], lw=1.0, ls=":", alpha=0.6)

# Jump arrows at every bin: from (x_k, F_n(x_{k-1})) to (x_k, F_n(x_k))
prev_cum = np.concatenate([[0.0], cum[:-1]])
for k in range(len(x)):
    is_big = (k == i_big)
    col = PALETTE["warn"] if is_big else PALETTE["neutral"]
    lw  = 2.2 if is_big else 1.0
    alpha = 1.0 if is_big else 0.55
    arrow = FancyArrowPatch((x[k], prev_cum[k]), (x[k], cum[k]),
                            arrowstyle="->", mutation_scale=11 if is_big else 7,
                            color=col, lw=lw, alpha=alpha, zorder=3)
    ax1.add_patch(arrow)

# Annotate the biggest jump
ax1.annotate(fr"$\Delta F_n({x[i_big]})=f_{{{x[i_big]}}}={prop[i_big]:.3f}$",
             xy=(x[i_big], (prev_cum[i_big] + cum[i_big]) / 2),
             xytext=(x[i_big] - 7.5, (prev_cum[i_big] + cum[i_big]) / 2 + 0.08),
             fontsize=10.5, color=PALETTE["warn"], fontweight="bold",
             arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.1))

# Worked example caption
ax1.text(0.5, 0.93,
         r"$F_n(3)=\dfrac{193+272+138}{2200}=0.274$",
         transform=ax1.transAxes, ha="left", va="top",
         fontsize=10.2, color=PALETTE["primary"],
         bbox=dict(boxstyle="round,pad=0.35", facecolor="#fbfbfd",
                   edgecolor=PALETTE["primary"], linewidth=0.9))

ax1.set_xlim(0, 25)
ax1.set_ylim(0, 1.05)
ax1.set_xlabel("Nr_visits")
ax1.set_ylabel("Cumulative relative frequency  $F_n(x)$")
ax1.set_title("Ex 2.7a — the ogive: jumps = relative frequencies")
ax1.legend(loc="lower right", framealpha=0.95, fontsize=9.5)

# =====================================================================
# RIGHT — (x_k, f_k) table + formula card
# =====================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)
ax2.add_patch(FancyBboxPatch((0.02, 0.04), 0.96, 0.92,
                             boxstyle="round,pad=0.02",
                             linewidth=1.1,
                             facecolor="#fbfbfd",
                             edgecolor=PALETTE["primary"]))

# Header
ax2.text(0.50, 0.945, "Relative frequencies  $f_k = n_k / n$",
         ha="center", va="top", fontsize=11,
         color=PALETTE["primary"], fontweight="bold")

# Compact two-column table: show first 7 bins (fits cleanly)
show_x   = x[:7]
show_f   = prop[:7]
col_y    = 0.86
row_dy   = 0.055
header_x = 0.10
val_x    = 0.58

ax2.text(header_x, col_y, "$x_k$",
         ha="left", va="center", fontsize=10,
         color=PALETTE["primary"], fontweight="bold")
ax2.text(val_x, col_y, "$f_k$",
         ha="left", va="center", fontsize=10,
         color=PALETTE["primary"], fontweight="bold")

for i, (xv, fv) in enumerate(zip(show_x, show_f)):
    yy = col_y - (i + 1) * row_dy
    is_big_row = (xv == x[i_big])
    if is_big_row:
        ax2.add_patch(FancyBboxPatch((0.05, yy - 0.022), 0.90, 0.044,
                                     boxstyle="round,pad=0.005",
                                     linewidth=0.0,
                                     facecolor=PALETTE["warn"],
                                     alpha=0.30))
    ax2.text(header_x, yy, f"{int(xv)}",
             ha="left", va="center", fontsize=10,
             color=PALETTE["primary"],
             fontweight="bold" if is_big_row else "normal")
    ax2.text(val_x, yy, f"{fv:.3f}",
             ha="left", va="center", fontsize=10,
             color=PALETTE["primary"],
             fontweight="bold" if is_big_row else "normal")
    if is_big_row:
        ax2.text(0.93, yy, r"max $f_k$",
                 ha="right", va="center", fontsize=9.5,
                 color=PALETTE["warn"], fontweight="bold")

ax2.text(0.50, 0.46,
         r"(... remaining bins through $x_{14}=24$)",
         ha="center", va="center", fontsize=8.5,
         color=PALETTE["neutral"], style="italic")

# Formula card
ax2.text(0.50, 0.36,
         r"$F_n(x) \;=\; \sum_{x_k \,\leq\, x} f_k$",
         ha="center", va="center", fontsize=11.5,
         color=PALETTE["primary"])
ax2.text(0.50, 0.27,
         r"$\Delta F_n(x_k) \;=\; f_k$",
         ha="center", va="center", fontsize=11.5,
         color=PALETTE["warn"], fontweight="bold")

# R helper at the bottom
ax2.text(0.50, 0.12,
         'R:  x      <- c(1,2,3,4,6,8,9,10,12,14,15,16,20,24)\n'
         '    counts <- c(193,272,138,115,92,60,118,228,309,130,113,104,122,206)\n'
         '    prop   <- round(counts/sum(counts), 3)\n'
         '    cum    <- cumsum(prop)',
         ha="center", va="center", fontsize=8.4, family="monospace",
         color=PALETTE["neutral"])

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
