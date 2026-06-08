"""Build AI walkthrough plot for Ex 0 / 2b3 — step diagram of the cumulative
frequencies of `size.family` (discrete number of family components travelling
together on the Titanic).

For DISCRETE variables the cumulative distribution is a STEP function:
constant on each open interval between observed values, jumping by f_i at
each observed value x_i. There is no interpolation — the step diagram is the
correct visual (an ogive would be wrong here since size.family is discrete).

 - LEFT  : spike plot of the relative frequencies f_i with cumulative running
           totals annotated above each spike (visual mass accumulation).
 - RIGHT : step diagram F(x) drawn with explicit risers (open circles below
           the jump, filled circles at the new value) — emphasises the
           right-continuous, integer-only nature of the distribution.

Also uses shutil to clone the real source PNGs (textbook scans of the Ex 2
question page and the b3 answer page) from the local note-taking workspace
into apps/mindnotes/statistics/images/ex0/{questions,answers}/ under the
canonical ex0_2b3_{question,answer}.png filenames.
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex0"
OUT  = f"{BASE}/ex0_2b3_ai.png"

# ---------- 1. Clone the real source PNGs (textbook scans) ----------
SRC = "/Users/Alessandro/Repos/my note taking app/statistics/ex0"
# NB: macOS screenshot filenames use U+202F (NARROW NO-BREAK SPACE) before "PM",
# not a regular space — must be preserved literally for the lookup to resolve.
NBSP = "\u202f"
pairs = [
    (f"{SRC}/questions/Screenshot 2026-06-05 at 3.47.12{NBSP}PM.png",
     f"{BASE}/questions/ex0_2b3_question.png"),
    (f"{SRC}/answers/Screenshot 2026-06-05 at 3.37.44{NBSP}PM.png",
     f"{BASE}/answers/ex0_2b3_answer.png"),
]
for src, dst in pairs:
    if os.path.exists(src):
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        shutil.copyfile(src, dst)
        print(f"copied -> {dst}")
    else:
        print(f"WARNING: source PNG not found at {src}")

# ---------- 2. Build the AI walkthrough figure ----------
# size.family distribution (Titanic, n = 1309)
x       = np.array([0, 1, 2, 3, 4, 5, 8])
count   = np.array([891, 319, 42, 20, 22, 6, 9])
n       = count.sum()
prop    = count / n                       # relative frequencies f_i
cumprop = np.cumsum(prop)                 # F(x_i)

# ---------- Figure ----------
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.4))

# =================== LEFT: spike plot with running cumulative ===================
markerline, stemlines, baseline = ax1.stem(
    x, prop, basefmt=" ", linefmt="-", markerfmt="o")
plt.setp(stemlines, color=PALETTE["primary"], linewidth=2.2)
plt.setp(markerline, color=PALETTE["primary"], markersize=8,
         markeredgecolor="#2a3142")

# Annotate each spike with f_i (above) and the running cum F_i (below, in amber).
# Stagger BOTH the f-labels above (so the tiny spikes 2-5 don't smash text
# together) and the F-labels below.
f_dy = [0.025, 0.025, 0.180, 0.115, 0.180, 0.115, 0.025]   # vertical offsets above spike
F_dy = [-0.065, -0.135, -0.065, -0.135, -0.065, -0.135, -0.065]
for xi, fi, Fi, dy_f, dy_F in zip(x, prop, cumprop, f_dy, F_dy):
    ax1.text(xi, fi + dy_f, f"$f={fi:.3f}$",
             ha="center", va="bottom", fontsize=9.5,
             color=PALETTE["primary"], fontweight="bold")
    ax1.text(xi, dy_F, f"$F={Fi:.3f}$",
             ha="center", va="top", fontsize=9,
             color="#7a6700", fontweight="bold",
             bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                       boxstyle="round,pad=0.18", linewidth=0.8))

# Highlight the missing integer values (6 and 7) — important pedagogical point
for missing in (6, 7):
    ax1.axvline(missing, color=PALETTE["warn"], lw=1.1, ls=":",
                alpha=0.55, zorder=1)
ax1.text(6.5, prop.max() * 0.78, "no obs\nat 6, 7",
         ha="center", va="center", fontsize=9.5, color=PALETTE["warn"],
         fontweight="bold",
         bbox=dict(facecolor="#fff", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.25", linewidth=1.0))

ax1.set_xlim(-0.7, 8.7)
ax1.set_ylim(-0.22, prop.max() * 1.40)
ax1.set_xticks(np.arange(0, 9))
ax1.set_yticks([0.0, 0.2, 0.4, 0.6, 0.8])
ax1.set_xlabel("size.family")
ax1.set_ylabel(r"Relative frequency  $f_i$")
ax1.set_title(r"Spike plot — jumps $f_i$ accumulate into $F$")

# =================== RIGHT: step diagram (CDF) ===================
# Build piecewise-constant CDF: starts at 0 for x < 0, jumps at each x_i.
# Draw horizontal segments + explicit risers with open/filled markers.
xs_left  = np.concatenate([[-1.0], x])               # left edges of plateaus
xs_right = np.concatenate([x, [9.5]])                # right edges of plateaus
levels   = np.concatenate([[0.0], cumprop])          # height on each plateau

for xl, xr, y in zip(xs_left, xs_right, levels):
    ax2.hlines(y, xl, xr, color=PALETTE["primary"], lw=2.3, zorder=3)

# Risers (vertical jumps) and open/filled circles
for xi, y_prev, y_new in zip(x, levels[:-1], levels[1:]):
    ax2.vlines(xi, y_prev, y_new, color=PALETTE["primary"],
               lw=1.4, ls="--", alpha=0.7, zorder=2)
    # open circle at the lower (NOT attained) value — right-continuous CDF
    ax2.plot(xi, y_prev, "o", markerfacecolor="white",
             markeredgecolor=PALETTE["primary"], markersize=8,
             markeredgewidth=1.6, zorder=4)
    # filled circle at the new (attained) value
    ax2.plot(xi, y_new, "o", color=PALETTE["primary"],
             markersize=8, markeredgecolor="#2a3142", zorder=5)

# Label only the two BIG jumps inline (at x=0 and x=1); list the tiny ones
# in a clean side-box so they don't collide with the step markers.
for xi, y_prev, y_new, fi in zip(x[:2], levels[:-1][:2], levels[1:][:2], prop[:2]):
    ax2.annotate("", xy=(xi - 0.22, y_new), xytext=(xi - 0.22, y_prev),
                 arrowprops=dict(arrowstyle="<->", color=PALETTE["accent"],
                                 lw=1.4))
    ax2.text(xi - 0.32, (y_prev + y_new) / 2, f"+{fi:.3f}",
             ha="right", va="center", fontsize=9.5,
             color="#7a6700", fontweight="bold")

# Side-box: list the small tail jumps (x=2..8) for completeness
tail_lines = [f"+{fi:.3f}  at x={xi}" for xi, fi in zip(x[2:], prop[2:])]
ax2.text(5.2, 0.45, "tail jumps:\n" + "\n".join(tail_lines),
         ha="left", va="center", fontsize=9,
         color="#7a6700", fontweight="bold",
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.35", linewidth=1.0))

ax2.text(0, 1.085, "right-continuous: F(x_i) attained at the jump",
         ha="left", va="bottom", fontsize=9.5, color=PALETTE["neutral"],
         style="italic")

ax2.set_xlim(-1.0, 9.5)
ax2.set_ylim(-0.05, 1.12)
ax2.set_xticks(np.arange(0, 10))
ax2.set_xlabel("size.family")
ax2.set_ylabel(r"Cumulative proportion  $F(x)$")
ax2.set_title("Step diagram: F constant between integers, jumps by $f_i$")

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
for xi, fi, Fi in zip(x, prop, cumprop):
    print(f"  x={xi}  f={fi:.4f}  F={Fi:.4f}")
