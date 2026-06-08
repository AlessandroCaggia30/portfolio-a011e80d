"""Build AI walkthrough plot for Ex 2.7c — five-number summary of Nr_visits.

Visual: two-panel figure that makes the five quantile readings on the
empirical ogive completely concrete.

LEFT panel
  Step-style empirical CDF (ogive) of Nr_visits with:
    - horizontal dashed guides at F = 0.25, 0.50, 0.75,
    - vertical dashed guides at Q1 = 3, Median = 10, Q3 = 14 (each defined
      as the smallest x with F_n(x) >= the target probability),
    - shaded sky-blue strip for the IQR region [Q1, Q3],
    - colored ticks under the x-axis for min, Q1, Me, Q3, max.

RIGHT panel
  Card with the cumulative-frequency table excerpt around each quantile
  crossing (Q1, Me, Q3), the five-number summary box, and a one-line R
  helper showing how to recover the same numbers from the raw vector.

Also uses shutil to clone the Ex 2.7 textbook prompt + textbook answer
PNGs (already saved as ex2_7d_question.png / ex2_7d_answer.png, which
contain the full Exercise 2.7 statement + the (c) answer paragraph) into
ex2/{questions,answers}/ex2_7c_*.png so the snippet can reference them.
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
OUT  = f"{BASE}/ex2_7c_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# ---------- 1. Clone the recurring Ex 2.7 prompt + answer PNGs via shutil ----------
clones = [
    (f"{BASE}/questions/ex2_7d_question.png", f"{BASE}/questions/ex2_7c_question.png"),
    (f"{BASE}/answers/ex2_7d_answer.png",     f"{BASE}/answers/ex2_7c_answer.png"),
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
prop   = counts / n
cum    = np.cumsum(prop)

# Definition used here: q_p = min{ x : F_n(x) >= p }
def q_at(p):
    idx = int(np.argmax(cum >= p))
    return int(x[idx]), float(cum[idx]), idx

q1, c_q1, i_q1 = q_at(0.25)
me, c_me, i_me = q_at(0.50)
q3, c_q3, i_q3 = q_at(0.75)
mn, mx = int(x.min()), int(x.max())

print(f"  n={n}")
print(f"  min={mn}  Q1={q1} (F={c_q1:.3f})  Me={me} (F={c_me:.3f})  Q3={q3} (F={c_q3:.3f})  max={mx}")

# =====================================================================
# Layout
# =====================================================================
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.6),
                               gridspec_kw={"width_ratios": [2.2, 1]})

# =====================================================================
# LEFT — empirical CDF / ogive with five-number quantile reading
# =====================================================================
xs_step = np.concatenate([[x[0] - 0.5, x[0]], x])
ys_step = np.concatenate([[0.0,         0.0], cum])

ax1.step(xs_step, ys_step, where="post",
         color=PALETTE["primary"], lw=1.9, label="Empirical CDF $F_n$")
ax1.scatter(x, cum, color=PALETTE["primary"], s=22, zorder=4)

# Shaded IQR strip along the x-axis
ax1.axvspan(q1, q3,
            color=PALETTE["secondary"], alpha=0.16,
            label=fr"IQR region $[Q_1, Q_3] = [{q1}, {q3}]$")

# Horizontal guides at 0.25, 0.50, 0.75
targets = [(0.25, PALETTE["accent"], r"$F=0.25$"),
           (0.50, PALETTE["ok"],     r"$F=0.50$"),
           (0.75, PALETTE["warn"],   r"$F=0.75$")]
for p, col, lab in targets:
    ax1.axhline(p, color=col, lw=1.2, ls="--", alpha=0.85)
    ax1.text(25.2, p, lab, va="center", ha="left",
             fontsize=9.5, color=col)

# Vertical guides + markers at Q1, Me, Q3
for q, F, col, lab in [(q1, c_q1, PALETTE["accent"], fr"$Q_1={q1}$"),
                       (me, c_me, PALETTE["ok"],     fr"Me$={me}$"),
                       (q3, c_q3, PALETTE["warn"],   fr"$Q_3={q3}$")]:
    ax1.axvline(q, color=col, lw=1.2, ls="--", alpha=0.85)
    ax1.scatter([q], [F], s=80, zorder=5,
                color=col, edgecolor=PALETTE["primary"], linewidth=1.1)
    ax1.text(q, -0.06, lab, ha="center", va="top",
             fontsize=10.2, color=col, fontweight="bold")

# Mark min and max on the x-axis too (the other two of the five)
ax1.text(mn, -0.06, fr"min$={mn}$", ha="center", va="top",
         fontsize=10.2, color=PALETTE["neutral"], fontweight="bold")
ax1.text(mx, -0.06, fr"max$={mx}$", ha="center", va="top",
         fontsize=10.2, color=PALETTE["neutral"], fontweight="bold")

ax1.set_xlim(0, 25)
ax1.set_ylim(0, 1.05)
ax1.set_xlabel("Nr_visits")
ax1.set_ylabel("Cumulative proportion  $F_n(x)$")
ax1.set_title("Ex 2.7c — reading the five-number summary off the ogive")
ax1.legend(loc="upper left", framealpha=0.95, fontsize=9.5)

# =====================================================================
# RIGHT — table excerpt + five-number summary card + R helper
# =====================================================================
ax2.axis("off")
ax2.set_xlim(0, 1); ax2.set_ylim(0, 1)
ax2.add_patch(FancyBboxPatch((0.02, 0.04), 0.96, 0.92,
                             boxstyle="round,pad=0.02",
                             linewidth=1.1,
                             facecolor="#fbfbfd",
                             edgecolor=PALETTE["primary"]))

# Header
ax2.text(0.50, 0.94, "Cumulative crossings  $F_n(x) \\geq p$",
         ha="center", va="top", fontsize=11,
         color=PALETTE["primary"], fontweight="bold")

# Compact table of the three quantile crossings
rows = [(0.25, q1, c_q1, "$Q_1$", PALETTE["accent"]),
        (0.50, me, c_me, "Me",    PALETTE["ok"]),
        (0.75, q3, c_q3, "$Q_3$", PALETTE["warn"])]

col_y    = 0.86
row_dy   = 0.07
hx, vx, lx = 0.07, 0.36, 0.66

ax2.text(hx, col_y, "p",       ha="left", va="center", fontsize=10,
         color=PALETTE["primary"], fontweight="bold")
ax2.text(vx, col_y, "$x$",     ha="left", va="center", fontsize=10,
         color=PALETTE["primary"], fontweight="bold")
ax2.text(lx, col_y, "$F_n(x)$",ha="left", va="center", fontsize=10,
         color=PALETTE["primary"], fontweight="bold")

for i, (p, xv, cv, lab, col) in enumerate(rows):
    yy = col_y - (i + 1) * row_dy
    ax2.add_patch(FancyBboxPatch((0.05, yy - 0.026), 0.90, 0.052,
                                 boxstyle="round,pad=0.005",
                                 linewidth=0.0,
                                 facecolor=col, alpha=0.16))
    ax2.text(hx, yy, f"{p:.2f}",
             ha="left", va="center", fontsize=10,
             color=PALETTE["primary"], fontweight="bold")
    ax2.text(vx, yy, f"{xv}  ({lab})",
             ha="left", va="center", fontsize=10,
             color=col, fontweight="bold")
    ax2.text(lx, yy, f"{cv:.3f}",
             ha="left", va="center", fontsize=10,
             color=PALETTE["primary"])

# Five-number summary box
ax2.text(0.50, 0.43,
         "Five-number summary",
         ha="center", va="center", fontsize=10.5,
         color=PALETTE["primary"], fontweight="bold")
ax2.text(0.50, 0.355,
         fr"(min, $Q_1$, Me, $Q_3$, max) $= ({mn},\,{q1},\,{me},\,{q3},\,{mx})$",
         ha="center", va="center", fontsize=11.2,
         color=PALETTE["primary"])

# R helper at the bottom
ax2.text(0.50, 0.20,
         'R:  v <- rep(x, counts)\n'
         '    quantile(v, c(0,.25,.5,.75,1), type=1)\n'
         '##   0%  25%  50%  75% 100%\n'
         '##    1    3   10   14   24',
         ha="center", va="center", fontsize=8.8, family="monospace",
         color=PALETTE["neutral"])

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
