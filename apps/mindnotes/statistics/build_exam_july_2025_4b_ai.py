"""AI walkthrough for Jul-2025 Ex4b — mod2 estimated equation and the Cards slope.

mod2: Investments ~ Branch + AgeC + Cards + Tenure
Estimated coefficients (from R):
  (Intercept) 262.1846
  BranchB      68.6722  ***
  AgeCsenior   73.7904  *
  AgeCyoung   -43.8684
  Cards         7.1993  ***
  Tenure        2.0665  ***
  R^2 = 0.1085

Two panels:
  Left  : Cards slope visualised as partial regression line (after adjusting
          for Branch, AgeC, Tenure) -- scatter of Investments vs Cards with
          OLS line, slope label 7.20.
  Right : Coefficient bars from summary(mod2).
"""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
import subprocess, tempfile, csv

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/past_exams/exam_july_2025_4b_ai.png"

RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/past exams/july 2025/Exam202507.RData"
tmp = tempfile.NamedTemporaryFile(suffix=".csv", delete=False); tmp.close()
subprocess.run(["Rscript", "-e",
                f'load("{RDATA}"); write.csv(BankClients[, c("Investments","Branch","AgeC","Cards","Tenure")], "{tmp.name}", row.names=FALSE)'],
               check=True, capture_output=True)
inv, cards = [], []
with open(tmp.name) as f:
    rd = csv.DictReader(f)
    for row in rd:
        inv.append(float(row["Investments"]))
        cards.append(float(row["Cards"]))
os.unlink(tmp.name)
inv = np.array(inv); cards = np.array(cards)

# coefficients from summary(mod2)
b0 = 262.1846
bBranchB = 68.6722
bSen = 73.7904
bYoung = -43.8684
bCards = 7.1993
bTenure = 2.0665
R2 = 0.1085

fig, axes = plt.subplots(1, 2, figsize=(13, 5.5),
                         gridspec_kw={"width_ratios": [1.1, 1.0]})

# --- LEFT: Investments vs Cards scatter + OLS fit ---
ax = axes[0]
# clip outliers above the 99th percentile for cleaner display
inv_cap = float(np.quantile(inv, 0.99))
mask = inv <= inv_cap
ax.scatter(cards[mask], inv[mask], s=10, alpha=0.35,
           color=PALETTE["secondary"], edgecolor="none",
           label=f"clients (n shown = {mask.sum()})")
xs = np.linspace(cards.min(), cards.max(), 50)
# baseline (Branch A, adult) prediction line varying Cards (Tenure at mean)
tenure_mean = 57.5  # approx mean of Tenure
ys = b0 + bCards * xs + bTenure * tenure_mean
ax.plot(xs, ys, color=PALETTE["warn"], linewidth=2.2,
        label=f"fitted: $\\hat\\beta_0 + {bCards:.2f}\\cdot\\mathrm{{Cards}} + \\hat\\beta_{{Tenure}} \\bar t$\n"
              f"(Branch=A, AgeC=adult, Tenure at mean)")
# slope indicator: +1 Cards -> +7.20 Investments
xt = 30
ax.annotate("", xy=(xt + 1, b0 + bCards * (xt + 1) + bTenure * tenure_mean),
            xytext=(xt, b0 + bCards * xt + bTenure * tenure_mean),
            arrowprops=dict(arrowstyle="->", color=PALETTE["primary"], lw=1.6))
ax.annotate(f"$+1$ Card $\\Rightarrow +{bCards:.2f}$\nInvestments",
            xy=(xt + 1, b0 + bCards * (xt + 1) + bTenure * tenure_mean),
            xytext=(xt + 6, b0 + bCards * (xt + 1) + bTenure * tenure_mean + 220),
            fontsize=10, color=PALETTE["primary"], fontweight="bold",
            arrowprops=dict(arrowstyle="->", color=PALETTE["primary"]))
ax.set_xlabel("Cards (intensity of card usage)")
ax.set_ylabel("Investments (\u20ac)")
ax.set_title("Step 1 — partial slope on Cards inside mod2\n"
             f"$\\hat\\beta_{{\\mathrm{{Cards}}}} = {bCards:.2f}$ (***), p = 8.4e-9 — "
             "ceteris paribus")
ax.set_xlim(0, 95)
ax.set_ylim(0, inv_cap * 1.05)
ax.legend(loc="upper right", framealpha=0.95, fontsize=9.5)

# --- RIGHT: coefficient bars for mod2 ---
ax2 = axes[1]
names = ["(Intercept)", "BranchB", "AgeCsenior", "AgeCyoung", "Cards", "Tenure"]
vals  = [b0, bBranchB, bSen, bYoung, bCards, bTenure]
sigs  = ["***", "***", "*", "", "***", "***"]
colors_c = [PALETTE["primary"] if v >= 0 else PALETTE["warn"] for v in vals]
ypos = np.arange(len(names))
ax2.barh(ypos, vals, color=colors_c, alpha=0.7,
         edgecolor=PALETTE["primary"], linewidth=1.1)
for i, (v, sg) in enumerate(zip(vals, sigs)):
    ha = "left" if v >= 0 else "right"
    dx = 8 if v >= 0 else -8
    ax2.text(v + dx, i, f"{v:+.2f} {sg}", ha=ha, va="center",
             fontsize=10.5, color=PALETTE["primary"], fontweight="bold")
ax2.set_yticks(ypos); ax2.set_yticklabels(names)
ax2.invert_yaxis()
ax2.axvline(0, color=PALETTE["primary"], linewidth=1.1)
ax2.set_xlabel("coefficient estimate")
ax2.set_title(f"Step 2 — summary(mod2)\n$R^2 = {R2:.4f}$  (10.85% of variance explained)")
ax2.set_xlim(min(vals) - 50, max(vals) + 80)

ax2.text(0.98, 0.05,
         f"Estimated equation:\n"
         f"$\\widehat{{Inv}} = {b0:.2f} + {bBranchB:.2f}\\cdot 1_{{B=B}}$\n"
         f"$+ {bSen:.2f}\\cdot 1_{{senior}} {bYoung:.2f}\\cdot 1_{{young}}$\n"
         f"$+ {bCards:.2f}\\cdot Cards + {bTenure:.2f}\\cdot Tenure$",
         transform=ax2.transAxes, ha="right", va="bottom",
         fontsize=9.5, color=PALETTE["primary"],
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

fig.suptitle("Jul-2025 Ex4b  —  mod2: Investments ~ Branch + AgeC + Cards + Tenure  "
             f"($R^2 = {R2:.4f}$)",
             fontsize=12, y=1.02, color=PALETTE["primary"])
plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=160, bbox_inches="tight")
print(f"saved -> {OUT}")
