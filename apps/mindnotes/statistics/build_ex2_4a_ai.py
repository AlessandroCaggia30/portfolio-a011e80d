"""Build AI walkthrough plot for Ex 2.4a — identifying the graph as an ogive
(cumulative-frequency polygon) for the `nr_contracts` distribution.

Also uses shutil to clone the recurring Exercise 2.4 textbook prompt image
(already saved as ex2_4b_question.png — the Ex 2.4 page is shared across
sub-parts 4a, 4b, 4c, 4d; identical MD5 hash) into the ex2/questions/
folder under the Ex 2.4a filename, so the snippet can reference it directly.

No specific textbook answer crop exists for Ex 2.4a in this repo, so the
snippet does NOT reference an answer PNG (we only reference what we copy).
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
OUT  = f"{BASE}/ex2_4a_ai.png"

# ---------- 1. Clone the recurring Exercise 2.4 textbook prompt PNG ----------
# The Ex 2.4 prompt page is shared across 4a/4b/4c/4d (identical MD5 hash
# already verified between ex2_4b_question.png and ex2_4d_question.png).
src_q = f"{BASE}/questions/ex2_4b_question.png"
dst_q = f"{BASE}/questions/ex2_4a_question.png"

if os.path.exists(src_q):
    shutil.copyfile(src_q, dst_q)
    print(f"copied -> {dst_q}")
else:
    print(f"WARNING: source question PNG not found at {src_q}")

# ---------- 2. Build the AI walkthrough figure ----------
# Class endpoints and absolute cumulative frequencies from the prompt's ogive
endpoints = np.array([0, 10, 20, 30, 60, 90, 150], dtype=float)
cumfreq   = np.array([0, 200, 400, 800, 1300, 1800, 2000], dtype=float)
n         = cumfreq[-1]                       # = 2000
cumprop   = cumfreq / n                       # relative cumulative frequencies
abs_freq  = np.diff(cumfreq)                  # class absolute frequencies
mids      = (endpoints[:-1] + endpoints[1:]) / 2

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.2))

# ---------- LEFT: the ogive itself with key reads annotated ----------
ax1.plot(endpoints, cumfreq, marker="o", color=PALETTE["primary"],
         lw=1.8, markersize=6, markerfacecolor=PALETTE["primary"],
         markeredgecolor="#2a3142", label="Ogive  $F(x)$")

# Shade the area under the ogive lightly to suggest "cumulative mass"
ax1.fill_between(endpoints, 0, cumfreq, color=PALETTE["primary"], alpha=0.10)

# Annotate each cumulative endpoint with (x, F)
for x, F in zip(endpoints, cumfreq):
    ax1.annotate(f"({int(x)}, {int(F)})", xy=(x, F),
                 xytext=(6, -12), textcoords="offset points",
                 fontsize=9, color="#2a3142")

# Highlight a quartile read: Q1 from F = 0.25 * n = 500 (in [20, 30))
F_q1 = 0.25 * n
# Linear interpolation on [20,30): 400 + 200/10 * (x-20) = 500 -> x = 25 wait
# Actually class [20,30) has f = 400, so density = 40/unit, F(x) = 400 + 40*(x-20)
# F = 500 -> x = 22.5
x_q1 = 22.5
ax1.plot([x_q1, x_q1, 0], [0, F_q1, F_q1], color=PALETTE["warn"],
         lw=1.2, ls="--", alpha=0.9)
ax1.scatter([x_q1], [F_q1], color=PALETTE["warn"], s=55,
            edgecolor="#2a3142", zorder=6)
ax1.annotate(rf"$Q_1 \approx {x_q1}$  (read at $F=500$)",
             xy=(x_q1, F_q1), xytext=(x_q1 + 12, F_q1 - 180),
             fontsize=10, color=PALETTE["warn"], fontweight="bold",
             arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.0))

ax1.set_xlim(-5, 160)
ax1.set_ylim(0, n * 1.08)
ax1.set_xlabel("Nr contracts")
ax1.set_ylabel("Cumulative frequency  $F$")
ax1.set_title("Ogive — cumulative frequency polygon (n = 2000)")
ax1.legend(loc="lower right", frameon=False)

# ---------- RIGHT: class-by-class absolute frequencies (histogram-style) ----------
# To emphasise that the ogive is the running sum of these class counts.
widths  = np.diff(endpoints)
density = abs_freq / widths   # frequency density (height for unequal-width bins)

bars = ax2.bar(endpoints[:-1], density, width=widths, align="edge",
               color=PALETTE["primary"], alpha=0.35,
               edgecolor=PALETTE["primary"], linewidth=1.2,
               label="Density  $f/w$")

# Annotate each bar with its absolute frequency
for x_left, w, h, f in zip(endpoints[:-1], widths, density, abs_freq):
    ax2.text(x_left + w/2, h + 1.2, f"$f = {int(f)}$",
             ha="center", va="bottom", fontsize=9.5,
             color="#2a3142", fontweight="bold")

ax2.set_xlim(-5, 160)
ax2.set_ylim(0, max(density) * 1.35)
ax2.set_xlabel("Nr contracts")
ax2.set_ylabel("Frequency density")
ax2.set_title("Underlying classes — running sum gives the ogive")
ax2.legend(loc="upper right", frameon=False)

# Verdict box on the right panel
verdict = (
    r"$\bullet$ Graph type: \textbf{ogive} (cumulative-frequency polygon)." + "\n"
    r"$\bullet$ Plots cumulative $F$ at each \textbf{class upper endpoint}." + "\n"
    r"$\bullet$ Monotone non-decreasing; reaches $n = 2000$ at $x = 150$." + "\n"
    r"$\bullet$ Use: read any quantile $x_p$ from $F = p \cdot n$ by inverse" + "\n"
    r"   linear interpolation (shown: $Q_1 = 22.5$ from $F = 500$)."
)
# Use plain text (no \textbf inside mathtext) — convert to mathtext-safe form:
verdict = verdict.replace(r"\textbf{ogive}", "ogive")
verdict = verdict.replace(r"\textbf{class upper endpoint}", "class upper endpoint")
ax2.text(0.02, 0.97, verdict, transform=ax2.transAxes,
         ha="left", va="top", fontsize=9.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  n = {int(n)};  classes = {len(abs_freq)}")
print(f"  class freqs = {abs_freq.astype(int).tolist()}")
print(f"  Q1 (interpolated on ogive) = {x_q1}")
