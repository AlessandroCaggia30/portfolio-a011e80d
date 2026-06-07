"""Build AI walkthrough plot for Ex 2.4d - variability from the ogive
(insurance consultants, n=2000):
  * IQR = Q3 - Q1 = 49.5 (from 2.4b)
  * Mean approximated from midpoints: xbar = 49
  * Variance with n/(n-1) Bessel correction: var = 1102.051
  * SD = sqrt(var) = 33.197

Two panels:
  LEFT:  midpoint-mass diagram (lollipop with midpoints) showing how
         xbar = sum(m_k * p_k) = 49 is the centre of mass; deviations
         (m_k - xbar) shaded as red/blue rectangles for negative/positive.
  RIGHT: SD-band "thermometer" on the data axis: the IQR box, the mean
         line, the +/- 1 SD band around the mean, and the comparison
         |---R--->| range bracket vs IQR vs SD bracket so the three
         dispersion indices are visually contrasted.
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_4d_ai.png"

# ---------- Grouped table from the prompt ----------
endpoints = np.array([0, 10, 20, 30, 60, 90, 150], dtype=float)
widths    = np.diff(endpoints)                            # 10,10,10,30,30,60
midpts    = np.array([5, 15, 25, 45, 75, 120], dtype=float)
relfreq   = np.array([0.10, 0.10, 0.20, 0.25, 0.25, 0.10])
n         = 2000

# ---------- Summaries ----------
xbar      = float(np.sum(midpts * relfreq))                       # 49
ex2_      = float(np.sum((midpts ** 2) * relfreq))                # 3902 (E[X^2])
m2_about_mean = float(np.sum(((midpts - xbar) ** 2) * relfreq))   # 1101.5
# Bessel correction n/(n-1)
var_approx = m2_about_mean * n / (n - 1)                          # 1102.051
sd_approx  = float(np.sqrt(var_approx))                           # 33.197
# Quartiles (from 2.4b)
Q1, Q3 = 22.5, 72.0
IQR    = Q3 - Q1                                                  # 49.5
R_min, R_max = 0.0, 150.0
R      = R_max - R_min                                            # 150

fig, (axL, axR) = plt.subplots(1, 2, figsize=(13.6, 5.6),
                               gridspec_kw={"width_ratios": [1.05, 1.15]})

# =====================================================================
# LEFT: midpoint-mass diagram for the mean and deviations
# =====================================================================
# Bars at each midpoint with height = relative frequency
bar_colors = [PALETTE["primary"] if m >= xbar else PALETTE["accent"] for m in midpts]
axL.bar(midpts, relfreq, width=widths * 0.65, color=PALETTE["primary"],
        alpha=0.22, edgecolor=PALETTE["primary"], linewidth=1.2,
        label="rel. freq $p_k$ at midpoint $m_k$")

# Lollipops: vertical sticks with markers + numeric labels
for m, p in zip(midpts, relfreq):
    axL.plot([m, m], [0, p], color=PALETTE["primary"], lw=1.8, zorder=3)
    axL.scatter([m], [p], s=70, color=PALETTE["primary"],
                edgecolor="#2a3142", lw=0.9, zorder=4)
    axL.text(m, p + 0.012, f"{p:.2f}", ha="center", va="bottom",
             fontsize=9.5, color=PALETTE["primary"], fontweight="bold")

# Mean line + label
axL.axvline(xbar, color=PALETTE["warn"], lw=2.0, ls="-",
            label=fr"$\bar x = \sum m_k p_k = {xbar:.0f}$")
axL.text(xbar + 1.6, 0.275,
         fr"$\bar x = {xbar:.0f}$",
         color=PALETTE["warn"], fontweight="bold", fontsize=11)

# Deviation arrows from xbar to each midpoint (illustrate (m_k - xbar))
y_arrow = -0.018
for m in midpts:
    col = PALETTE["accent"] if m < xbar else "#2c7a2c"
    axL.annotate("", xy=(m, y_arrow), xytext=(xbar, y_arrow),
                 arrowprops=dict(arrowstyle="->", color=col, lw=1.1,
                                 alpha=0.75))

# Recipe box
recipe = (
    fr"$\bar x \approx \sum_k m_k\,p_k = {xbar:.0f}$" + "\n"
    fr"$E[X^2] \approx \sum_k m_k^2\,p_k = {ex2_:.1f}$" + "\n"
    fr"$\sigma^2 \approx (E[X^2] - \bar x^2)\cdot \frac{{n}}{{n-1}}$" + "\n"
    fr"$\;\;\;= ({ex2_:.1f} - {xbar:.0f}^2)\cdot \frac{{2000}}{{1999}} = {var_approx:.3f}$" + "\n"
    fr"$\sigma \approx \sqrt{{{var_approx:.3f}}} = {sd_approx:.3f}$"
)
axL.text(0.98, 0.97, recipe, transform=axL.transAxes,
         ha="right", va="top", fontsize=9.8,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

axL.set_xlim(-5, 160)
axL.set_ylim(-0.04, 0.32)
axL.set_xlabel("Nr. of contracts (midpoint $m_k$)")
axL.set_ylabel("Relative frequency $p_k$")
axL.set_title("Step 1 - Mean & variance from midpoints (grouped data)")
axL.legend(loc="upper right", framealpha=0.95, fontsize=9.5)

# =====================================================================
# RIGHT: dispersion thermometer - Range vs IQR vs SD band
# =====================================================================
axR.set_title("Step 2 - Three dispersion indices on the data axis")
axR.set_xlim(-10, 165)
axR.set_ylim(-0.5, 4.2)
axR.set_yticks([])
axR.set_xlabel("Nr. of contracts")

# Light-gray data axis from 0 to 150
axR.axhline(0, color="#a0a4ab", lw=0.8, alpha=0.6, zorder=1)
for x in [0, 30, 60, 90, 120, 150]:
    axR.plot([x, x], [-0.05, 0.05], color="#a0a4ab", lw=0.8)
    axR.text(x, -0.20, f"{x:.0f}", ha="center", va="top",
             fontsize=9, color="#5a5f6b")

# IQR box centred on row y=1
y_box = 1.0
axR.add_patch(plt.Rectangle((Q1, y_box - 0.22), IQR, 0.44,
                            facecolor=PALETTE["primary"], alpha=0.20,
                            edgecolor=PALETTE["primary"], linewidth=1.4))
axR.text((Q1 + Q3) / 2, y_box + 0.32,
         fr"$\mathrm{{IQR}} = Q_3 - Q_1 = {IQR:.1f}$",
         ha="center", va="bottom", fontsize=10.5,
         color=PALETTE["primary"], fontweight="bold")
axR.text(Q1, y_box - 0.40, fr"$Q_1={Q1:.1f}$", ha="center", va="top",
         fontsize=9.5, color=PALETTE["primary"])
axR.text(Q3, y_box - 0.40, fr"$Q_3={Q3:.0f}$", ha="center", va="top",
         fontsize=9.5, color=PALETTE["primary"])

# SD band centred on row y=2.2
y_sd = 2.2
lo, hi = xbar - sd_approx, xbar + sd_approx
axR.add_patch(plt.Rectangle((lo, y_sd - 0.22), 2 * sd_approx, 0.44,
                            facecolor="#2c7a2c", alpha=0.18,
                            edgecolor="#2c7a2c", linewidth=1.4))
axR.plot([xbar, xbar], [y_sd - 0.30, y_sd + 0.30],
         color=PALETTE["warn"], lw=2.2)
axR.text(xbar, y_sd + 0.32,
         fr"$\bar x \pm \sigma = {xbar:.0f}\pm{sd_approx:.2f}$",
         ha="center", va="bottom", fontsize=10.5,
         color="#2c7a2c", fontweight="bold")
axR.text(xbar, y_sd - 0.40, fr"$\bar x={xbar:.0f}$",
         ha="center", va="top", fontsize=9.5, color=PALETTE["warn"])

# Range bracket on row y=3.3
y_R = 3.3
axR.annotate("", xy=(R_max, y_R), xytext=(R_min, y_R),
             arrowprops=dict(arrowstyle="<->", color="#5a5f6b", lw=1.6))
axR.text((R_min + R_max) / 2, y_R + 0.18,
         fr"range $R = \max - \min = {R:.0f}$   (not recommended -- min unreliable)",
         ha="center", va="bottom", fontsize=10.3, color="#5a5f6b")
axR.text((R_min + R_max) / 2, y_R - 0.22,
         "(0 is just the lower class endpoint, not a true observed minimum)",
         ha="center", va="top", fontsize=8.8, color="#8a8f9b", style="italic")

# Verdict box (bottom-right)
verdict = (
    fr"$\mathrm{{IQR}} = {IQR:.1f}$"          "\n"
    fr"$\sigma^2 \approx {var_approx:.3f}$"   "\n"
    fr"$\sigma   \approx {sd_approx:.3f}$"    "\n"
    fr"$\mathrm{{CV}} = \sigma/\bar x \approx {sd_approx/xbar:.3f}$"
)
axR.text(0.99, 0.03, verdict, transform=axR.transAxes,
         ha="right", va="bottom", fontsize=10.5, fontweight="bold",
         bbox=dict(facecolor="#eaf6ea", edgecolor="#2c7a2c",
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  xbar={xbar:.4f}  E[X^2]={ex2_:.4f}")
print(f"  var(Bessel)={var_approx:.4f}  sd={sd_approx:.4f}")
print(f"  IQR={IQR:.4f}  CV={sd_approx/xbar:.4f}")
