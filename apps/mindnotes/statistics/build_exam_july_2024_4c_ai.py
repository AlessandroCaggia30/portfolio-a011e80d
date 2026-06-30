"""AI walkthrough for Jul-2024 Ex4.c — required n vs margin of error for 95% CI on p."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2024_4c_ai.png"

z = 1.959964  # z_{0.975}
ms = np.linspace(0.01, 0.10, 200)
n_worst = (z**2) / (4 * ms**2)
n_p30   = (z**2 * 0.30 * 0.70) / (ms**2)
n_p10   = (z**2 * 0.10 * 0.90) / (ms**2)

m_star = 0.02
n_star_worst = int(np.ceil((z**2)/(4*m_star**2)))
n_star_30    = int(np.ceil((z**2 * 0.30 * 0.70)/(m_star**2)))

fig, ax = plt.subplots(figsize=(11, 6.5))
ax.plot(ms, n_worst, color=PALETTE["warn"],   lw=2.4,
        label=r"worst case  $\hat p = 0.5$  (conservative)")
ax.plot(ms, n_p30,   color=PALETTE["primary"], lw=2.2,
        label=r"prior  $\hat p = 0.30$")
ax.plot(ms, n_p10,   color=PALETTE["ok"],      lw=2.0, ls="--",
        label=r"prior  $\hat p = 0.10$")
ax.axvline(m_star, color=PALETTE["neutral"], lw=1.2, ls=":",
           label=f"target ME = {m_star}")
ax.scatter([m_star], [n_star_worst], color=PALETTE["warn"], s=85, zorder=5,
           edgecolor=PALETTE["primary"])
ax.scatter([m_star], [n_star_30],    color=PALETTE["primary"], s=85, zorder=5,
           edgecolor=PALETTE["primary"])
ax.annotate(f"n = {n_star_worst}\n(worst case)",
            xy=(m_star, n_star_worst), xytext=(m_star+0.015, n_star_worst+700),
            arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.5),
            fontsize=11.5, color=PALETTE["warn"], fontweight="bold")
ax.annotate(f"n = {n_star_30}\n(p_hat = 0.30)",
            xy=(m_star, n_star_30), xytext=(m_star+0.02, n_star_30-1200),
            arrowprops=dict(arrowstyle="->", color=PALETTE["primary"], lw=1.5),
            fontsize=11.5, color=PALETTE["primary"], fontweight="bold")

ax.set_xlabel("margin of error  m")
ax.set_ylabel("required sample size  n")
ax.set_title("Jul-2024 Ex4.c — Required n for 95% CI on p  (Wald, $z_{0.975}=1.96$)")
ax.set_yscale("log")
ax.set_ylim(60, 70000)
ax.legend(loc="upper right", framealpha=0.95)
ax.text(0.02, 0.97,
        r"$n \geq \dfrac{z_{1-\alpha/2}^2 \, \hat p (1-\hat p)}{m^2}$"
        "\n\nworst case  =>  $\\hat p(1-\\hat p) \\leq 1/4$"
        f"\n\nfor m = {m_star}:  n_worst = {n_star_worst}",
        transform=ax.transAxes, ha="left", va="top",
        fontsize=11, color=PALETTE["primary"],
        bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                  boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (n_worst={n_star_worst}, n_p30={n_star_30})")
