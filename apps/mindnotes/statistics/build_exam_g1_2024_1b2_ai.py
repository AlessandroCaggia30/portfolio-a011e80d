"""AI walkthrough for G1-2024 Ex1.b2 — Sample proportion non-free in rural vs suburban."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt

apply_style()
OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_g1_2024_1b2_ai.png"

# From R: tab[rural,  c(non-free, free)] = (233, 146);  tab[suburban, ] = (155, 56)
rural = [233, 146]; suburban = [155, 56]
n_r = sum(rural); n_s = sum(suburban)
p_r_nf = rural[0]/n_r;       p_r_f = rural[1]/n_r
p_s_nf = suburban[0]/n_s;    p_s_f = suburban[1]/n_s

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.8),
                               gridspec_kw={"width_ratios":[1.2, 1.0]})

# Left: stacked-100% bars Lunch | SchoolLoc(rural, suburban)
locs = [f"rural\n(n = {n_r})", f"suburban\n(n = {n_s})"]
nf = np.array([p_r_nf, p_s_nf])*100
fr = np.array([p_r_f, p_s_f])*100
b1 = ax1.bar(locs, nf, color=PALETTE["primary"], alpha=0.85, edgecolor="black", label="non-free")
b2 = ax1.bar(locs, fr, bottom=nf, color=PALETTE["warn"], alpha=0.75, edgecolor="black", label="free")
for i, p in enumerate(nf):
    ax1.text(i, p/2, f"non-free\n{p:.1f}%", ha="center", va="center",
             fontsize=11, color="white", fontweight="bold")
for i, (p, f) in enumerate(zip(nf, fr)):
    ax1.text(i, p + f/2, f"free\n{f:.1f}%", ha="center", va="center",
             fontsize=10.5, color="white", fontweight="bold")
ax1.set_ylim(0, 105)
ax1.set_ylabel("conditional relative frequency  (%)")
ax1.set_title("Lunch distribution within each SchoolLoc")
ax1.legend(loc="lower right", framealpha=0.95)

# Right: side-by-side bars of P(non-free | loc)
ax2.bar(["rural", "suburban"], [p_r_nf*100, p_s_nf*100],
        color=[PALETTE["primary"], PALETTE["secondary"]], alpha=0.85, edgecolor="black", width=0.55)
for i, v in enumerate([p_r_nf, p_s_nf]):
    ax2.text(i, v*100 + 1.5, f"{v*100:.1f}%", ha="center",
             fontsize=12, color=PALETTE["primary"], fontweight="bold")
ax2.set_ylabel("P(Lunch = non-free | SchoolLoc)  (%)")
ax2.set_title("Sample proportion of non-free  (rural vs suburban)")
ax2.set_ylim(0, 90)

verdict = ("Claim: P(non-free | rural)  >  P(non-free | suburban)\n\n"
           f"P(non-free | rural)    = 233 / 379 = {p_r_nf:.4f}  ({p_r_nf*100:.1f}%)\n"
           f"P(non-free | suburban) = 155 / 211 = {p_s_nf:.4f}  ({p_s_nf*100:.1f}%)\n\n"
           "{0:.1f}%  <  {1:.1f}%  =>  claim is FALSE.".format(p_r_nf*100, p_s_nf*100))
ax2.text(0.02, 0.97, verdict, transform=ax2.transAxes, ha="left", va="top",
         fontsize=10, color=PALETTE["primary"],
         bbox=dict(facecolor="#fdecea", edgecolor=PALETTE["warn"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle("G1-2024 Ex1.b2 — Is P(non-free | rural) > P(non-free | suburban)?",
             fontsize=12.5, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}  (rural={p_r_nf:.4f}, suburban={p_s_nf:.4f})")
