"""AI walkthrough for G2-2024 Ex2.a — Analytical Welch CI for difference of two means."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import t as student_t

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g2_2024_2a_ai.png"

# Use the actual South vs NorthEast summary numbers
n1, m1, s1 = 163, 312.77, 110.13   # South
n2, m2, s2 = 134, 226.26, 72.94    # NorthEast
diff = m1 - m2
se = np.sqrt(s1**2/n1 + s2**2/n2)
nu = (s1**2/n1 + s2**2/n2)**2 / ((s1**2/n1)**2/(n1-1) + (s2**2/n2)**2/(n2-1))
alpha = 0.01
tq = student_t.ppf(1 - alpha/2, df=nu)
me = tq * se
lo, hi = diff - me, diff + me

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.8))

# Left: schematic of the formula — t pivot distribution
zz = np.linspace(-4, 4, 600)
yy = student_t.pdf(zz, df=nu)
ax1.plot(zz, yy, color=PALETTE["primary"], lw=2.2,
         label=f"$T_\\nu$  ($\\nu={nu:.1f}$)")
mask = (zz >= -tq) & (zz <= tq)
ax1.fill_between(zz[mask], 0, yy[mask], color=PALETTE["ok"], alpha=0.35,
                 label=f"central 99%  (t = $\\pm$ {tq:.3f})")
ax1.axvline(-tq, color=PALETTE["warn"], lw=1.5, ls="--")
ax1.axvline(tq, color=PALETTE["warn"], lw=1.5, ls="--")
ax1.set_xlabel("Studentized pivot  $(\\bar X - \\bar Y - (\\mu_1-\\mu_2))/SE$")
ax1.set_ylabel("density")
ax1.set_title(f"Welch t pivot:  $\\nu$ from Welch-Satterthwaite")
ax1.legend(loc="upper right", framealpha=0.95)

# Right: formula box + CI on x-axis
ax2.axhline(0, color=PALETTE["neutral"], lw=0.8)
ax2.plot([lo, hi], [0, 0], color=PALETTE["primary"], lw=4)
ax2.scatter([diff], [0], s=120, color=PALETTE["warn"], zorder=5,
            label=f"$\\bar x_S - \\bar x_{{NE}}$ = {diff:.2f}")
ax2.scatter([lo, hi], [0, 0], s=70, color=PALETTE["primary"], zorder=5)
ax2.text(diff, 0.05, f"point\nestimate\n{diff:.2f}", ha="center",
         va="bottom", fontsize=9, color=PALETTE["warn"])
ax2.text(lo, -0.05, f"{lo:.2f}", ha="center", va="top", fontsize=10,
         color=PALETTE["primary"])
ax2.text(hi, -0.05, f"{hi:.2f}", ha="center", va="top", fontsize=10,
         color=PALETTE["primary"])
ax2.set_xlim(lo - 25, hi + 25)
ax2.set_ylim(-0.4, 0.7)
ax2.set_yticks([])
ax2.set_xlabel("$\\mu_S - \\mu_{NE}$")
ax2.set_title("99% Welch CI on $\\mu_S - \\mu_{NE}$  (S = South, NE = NorthEast)")

formula = (
    r"$(\bar X - \bar Y) \pm t_{\alpha/2,\nu}\sqrt{s_1^2/n_1 + s_2^2/n_2}$"
    + f"\n\n$n_S={n1},\\ \\bar x_S={m1:.2f},\\ s_S={s1:.2f}$"
    + f"\n$n_{{NE}}={n2},\\ \\bar x_{{NE}}={m2:.2f},\\ s_{{NE}}={s2:.2f}$"
    + f"\n$SE={se:.2f},\\ t_{{0.005,{nu:.1f}}}={tq:.3f}$"
    + f"\nCI = [{lo:.2f}, {hi:.2f}]"
)
ax2.text(0.02, 0.97, formula, transform=ax2.transAxes,
         ha="left", va="top", fontsize=10.5, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle("G2-2024 Ex2.a — Welch t-CI: formula & geometry",
             fontsize=12.5, color=PALETTE["primary"], y=1.02)
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (CI=[{lo:.2f}, {hi:.2f}], nu={nu:.2f}, t={tq:.3f})")
