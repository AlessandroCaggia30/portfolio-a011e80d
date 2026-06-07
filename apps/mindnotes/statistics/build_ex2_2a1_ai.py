"""Build AI walkthrough plot for Ex 2.2a1 — exact vs. approximation under the
uniform-on-interval assumption (grouped hours-of-access data, n=300 pupils).

The two-panel figure makes the key point visual:
 - LEFT  : within class [10,25) we *assume* mass is uniformly spread, so the
           share above 14 is a *linear* split of the bar at x=14. The true
           (unknown) within-class CDF could be ANY non-decreasing curve from
           (10,0) to (25,1) — three plausible alternatives are overlaid to
           show the answer 0.77 is approximation-dependent.
 - RIGHT : sensitivity — what 'Freq(X>14)' would be under each alternative
           within-class shape, contrasted with the uniform answer 0.77.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_2a1_ai.png"

# --- Grouped table from the prompt ---
lower = np.array([0.0, 10.0, 25.0, 30.0])
upper = np.array([10.0, 25.0, 30.0, 40.0])
freq  = np.array([45,   90,   75,   90], dtype=float)
n     = int(freq.sum())               # 300
p     = freq / freq.sum()             # 0.15, 0.30, 0.25, 0.30
width = upper - lower                 # 10, 15,  5, 10
dens  = p / width                     # 0.015, 0.020, 0.050, 0.030

# Class of interest is the second one, [10,25), with mass 0.30
LO, HI, P_CLASS, D_CLASS = lower[1], upper[1], p[1], dens[1]
THR = 14.0
frac_above_uniform = (HI - THR) / (HI - LO)      # 11/15 = 0.7333..
mass_above_uniform = P_CLASS * frac_above_uniform  # 0.22
freq_gt14_uniform  = mass_above_uniform + p[2] + p[3]  # 0.77

# --- Figure ---
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.4))

# ====================== LEFT panel ======================
# Draw the density histogram for ALL classes (faded)
for lo, hi, d, k in zip(lower, upper, dens, range(4)):
    is_focus = (k == 1)
    ax1.bar(lo, d, width=hi - lo, align="edge",
            color=PALETTE["primary"],
            alpha=0.35 if is_focus else 0.14,
            edgecolor=PALETTE["primary"],
            linewidth=1.4 if is_focus else 0.9, zorder=2)

# Highlight the sub-area of [10,25) that lies above 14 (the part we claim is 0.22)
ax1.add_patch(Rectangle((THR, 0), HI - THR, D_CLASS,
                        facecolor=PALETTE["accent"], alpha=0.55,
                        edgecolor="#7a6700", linewidth=1.3, zorder=3,
                        label=fr"uniform share above 14: "
                              fr"$(25-14)\cdot c_2 = 11\cdot 0.020 = 0.22$"))

# Mark the threshold x=14
ax1.axvline(THR, color=PALETTE["warn"], lw=1.8, ls="--", zorder=4,
            label="threshold $x = 14$")

# --- Three plausible within-class densities (all integrate to p=0.30 on [10,25])
# A "right-loaded" alternative: linearly increasing on [10,25]
# A "left-loaded" alternative : linearly decreasing on [10,25]
# A "centered" alternative    : triangular centred at midpoint 17.5
xx = np.linspace(LO, HI, 400)

def normalize(y, total_mass=P_CLASS):
    area = np.trapz(y, xx)
    return y * (total_mass / area)

# Right-loaded (more mass at upper end)
y_right = normalize(np.linspace(0.2, 1.0, xx.size))
# Left-loaded (more mass at lower end)
y_left  = normalize(np.linspace(1.0, 0.2, xx.size))
# Triangular at the midpoint 17.5
y_tri   = normalize(1.0 - np.abs(xx - 17.5) / 7.5 + 0.05)

ax1.plot(xx, y_right, color=PALETTE["secondary"], lw=1.8, ls="-",
         label="alt. shape: right-loaded", zorder=5)
ax1.plot(xx, y_left,  color=PALETTE["ok"],        lw=1.8, ls="-",
         label="alt. shape: left-loaded",  zorder=5)
ax1.plot(xx, y_tri,   color="#9b59b6",            lw=1.8, ls="-",
         label="alt. shape: triangular",    zorder=5)

# Uniform reference line (the assumed density)
ax1.hlines(D_CLASS, LO, HI, color=PALETTE["primary"], lw=2.2,
           label=fr"uniform density assumed:  $c_2 = 0.020$", zorder=6)

ax1.set_xlim(-1.5, 41.5)
ax1.set_ylim(0, max(dens.max(), y_right.max()) * 1.30)
ax1.set_xlabel("Hours of access to devices per week")
ax1.set_ylabel(r"Density  $c_k = p_k / w_k$")
ax1.set_title(r"Why 0.77 is an approximation: within $[10,25)$ the shape is unknown")
ax1.legend(loc="upper right", framealpha=0.95, fontsize=9, ncol=1)

# Annotate the uniform share
ax1.annotate(r"$\approx 0.22$ of total mass",
             xy=((THR + HI) / 2, D_CLASS * 0.55),
             ha="center", va="center",
             fontsize=10.5, color="#7a6700", fontweight="bold")

# ====================== RIGHT panel ======================
# Compute the mass above 14 under each alternative, plus the exact-uniform value
def mass_above(y, threshold=THR):
    mask = xx >= threshold
    return float(np.trapz(y[mask], xx[mask]))

m_right = mass_above(y_right) + p[2] + p[3]
m_left  = mass_above(y_left)  + p[2] + p[3]
m_tri   = mass_above(y_tri)   + p[2] + p[3]
m_unif  = freq_gt14_uniform                   # 0.77

scenarios = [
    ("uniform\n(assumed)",   m_unif,  PALETTE["primary"]),
    ("right-loaded",         m_right, PALETTE["secondary"]),
    ("left-loaded",          m_left,  PALETTE["ok"]),
    ("triangular",           m_tri,   "#9b59b6"),
]
labels = [s[0] for s in scenarios]
values = [s[1] for s in scenarios]
colors = [s[2] for s in scenarios]

xpos = np.arange(len(scenarios))
bars = ax2.bar(xpos, values, width=0.62, color=colors,
               edgecolor="#2a3142", linewidth=0.9, alpha=0.85)

for b, v in zip(bars, values):
    ax2.text(b.get_x() + b.get_width() / 2, v + 0.012,
             f"{v:.3f}", ha="center", va="bottom",
             fontsize=11, fontweight="bold", color="#2a3142")

# Reference horizontal line at the uniform answer
ax2.axhline(m_unif, color=PALETTE["warn"], ls="--", lw=1.3,
            label=fr"uniform answer = {m_unif:.2f}")

ax2.set_xticks(xpos)
ax2.set_xticklabels(labels, fontsize=10.5)
ax2.set_ylabel(r"$\mathrm{Freq}(X > 14)$  under each within-class shape")
ax2.set_title("Sensitivity: the answer depends on the unknown shape")
ax2.set_ylim(0, max(values) * 1.20)
ax2.legend(loc="upper right", framealpha=0.95, fontsize=10)

# Summary box on the right panel
summary = (
    r"Uniform-on-interval split of [10,25):"
    "\n"
    r"  $\mathrm{Freq}(X>14) = (25-14)\,c_2 + p_3 + p_4$"
    "\n"
    r"  $\quad = 11(0.020) + 0.25 + 0.30 = 0.77$"
    "\n\n"
    r"Range across plausible shapes: "
    fr"$[{min(values):.3f},\, {max(values):.3f}]$"
    "\n"
    r"$\Rightarrow$ 0.77 is an APPROXIMATION."
)
ax2.text(0.02, 0.97, summary, transform=ax2.transAxes,
         ha="left", va="top", fontsize=9.5,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  uniform Freq(X>14)       = {m_unif:.4f}")
print(f"  right-loaded Freq(X>14)  = {m_right:.4f}")
print(f"  left-loaded  Freq(X>14)  = {m_left:.4f}")
print(f"  triangular   Freq(X>14)  = {m_tri:.4f}")
