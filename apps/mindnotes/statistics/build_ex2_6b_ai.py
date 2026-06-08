"""Build AI walkthrough plot for Ex 2.6b — share of transactions sold below
unit cost (margin < 0) and 95-th percentile of (Unit_Price - Unit_Cost).

Two facts to convey in one figure:
  1. About 14.06% of the transactions have Unit_Price < Unit_Cost. This is
     just the empirical mass on the left of 0 in the distribution of
     m = Unit_Price - Unit_Cost. Mathematically:
         P(m < 0) = E[1{Unit_Cost > Unit_Price}] = mean(Unit_Cost > Unit_Price).
  2. The 95-th percentile of m is 211 USD, i.e. only 5% of transactions earn
     a per-unit margin above 211 USD. Mass between 0 and 211 is 1 - 0.1406 -
     0.05 = ~80% of transactions (small but positive margin), so most sales
     hover just above cost.

Uses shutil to clone the Ex 2.6 textbook prompt/answer pages from a sibling
sub-part (2.6a1) into ex2/{questions,answers}/ex2_6b_*.png IF those source
PNGs already exist; otherwise it skips silently and the snippet simply omits
the corresponding image references (per "only reference PNGs you actually
copy").
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import pyreadr
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
OUT  = f"{BASE}/ex2_6b_ai.png"
os.makedirs(os.path.dirname(OUT), exist_ok=True)

# ---------- 1. Optional shutil clone of recurring Ex 2.6 textbook pages -------
# The textbook prompt/answer pages for Ex 2.6 are shared across 2.6a*..2.6c.
# Copy from a sibling sub-part when available; never reference a missing file.
copied = []
candidates = [
    ("questions/ex2_6a1_question.png", "questions/ex2_6b_question.png"),
    ("questions/ex2_6c_question.png",  "questions/ex2_6b_question.png"),
    ("answers/ex2_6a1_answer.png",     "answers/ex2_6b_answer.png"),
]
for rel_src, rel_dst in candidates:
    src = f"{BASE}/{rel_src}"
    dst = f"{BASE}/{rel_dst}"
    if os.path.exists(src) and not os.path.exists(dst):
        shutil.copyfile(src, dst)
        copied.append(rel_dst)
        print(f"copied -> {dst}")
if not copied:
    print("note: no Ex 2.6 textbook source PNG found; skipping shutil clone")

# ---------- 2. Load and compute the margin variable --------------------------
result = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata")
ch     = result["customer_habits"].copy()
m      = (ch["Unit_Price"] - ch["Unit_Cost"]).to_numpy()
n      = len(m)
share_neg = float((m < 0).mean()) * 100      # 14.06%
p95       = float(np.percentile(m, 95))      # 211 USD per textbook
print(f"  n={n}  P(m<0)={share_neg:.4f}%  p95(m)={p95:.2f}")

# ---------- 3. Figure: empirical CDF + commentary card -----------------------
fig = plt.figure(figsize=(14.4, 5.6))
gs  = fig.add_gridspec(1, 2, width_ratios=[1.55, 1.0])
axA = fig.add_subplot(gs[0, 0])
axB = fig.add_subplot(gs[0, 1])

# ---------- PANEL A: ECDF of m, zoomed around the action area ----------------
m_sorted = np.sort(m)
F        = np.arange(1, n + 1) / n            # ECDF heights
# Clip x to where the interesting features live: [-500, 600] covers <0 mass,
# the median (~30), and the p95 (=211).
xlo, xhi = -500.0, 600.0
mask     = (m_sorted >= xlo) & (m_sorted <= xhi)
axA.plot(m_sorted[mask], F[mask],
         color=PALETTE["primary"], lw=2.0, label="ECDF of  $m = $ Unit\\_Price $-$ Unit\\_Cost")

# Threshold m = 0
axA.axvline(0, color=PALETTE["warn"], lw=1.6, ls="--", alpha=0.8)
axA.fill_between([xlo, 0], 0, share_neg / 100,
                 color=PALETTE["warn"], alpha=0.15)
axA.scatter([0], [share_neg / 100], color=PALETTE["warn"], s=55, zorder=5)
axA.annotate(f"$F(0) = {share_neg/100:.4f}$\n$\\Rightarrow$ {share_neg:.2f}% sold below cost",
             xy=(0, share_neg / 100),
             xytext=(-470, share_neg / 100 + 0.18),
             fontsize=11, color=PALETTE["warn"], fontweight="bold",
             arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.1))

# 95-th percentile of m
axA.axvline(p95, color=PALETTE["accent"], lw=1.6, ls="--", alpha=0.9)
axA.scatter([p95], [0.95], color=PALETTE["accent"], s=55, zorder=5,
            edgecolor=PALETTE["primary"], linewidth=1.0)
axA.annotate(f"$p_{{95}} = {p95:.0f}$ USD\n$\\Rightarrow$ only 5% earn more",
             xy=(p95, 0.95),
             xytext=(p95 - 360, 0.62),
             fontsize=11, color=PALETTE["primary"], fontweight="bold",
             arrowprops=dict(arrowstyle="->", color=PALETTE["accent"], lw=1.2))

# Horizontal guides at 0.1406 and 0.95
axA.axhline(share_neg / 100, color=PALETTE["warn"],   lw=0.8, ls=":", alpha=0.6)
axA.axhline(0.95,            color=PALETTE["accent"], lw=0.8, ls=":", alpha=0.6)
axA.set_xlim(xlo, xhi)
axA.set_ylim(0, 1.02)
axA.set_xlabel("$m$ = Unit\\_Price $-$ Unit\\_Cost  (USD)")
axA.set_ylabel("$F(m)$  (empirical CDF)")
axA.set_title("Ex 2.6b  -  ECDF of per-unit margin with key thresholds")
axA.legend(loc="lower right", frameon=True)

# ---------- PANEL B: commentary + R ------------------------------------------
axB.axis("off")
axB.set_xlim(0, 1); axB.set_ylim(0, 1)
axB.add_patch(FancyBboxPatch((0.02, 0.04), 0.96, 0.92,
                             boxstyle="round,pad=0.02",
                             linewidth=1.1,
                             facecolor="#fbfbfd",
                             edgecolor=PALETTE["primary"]))

n_below = int((m < 0).sum())
txt = (
    r"$\bf{Two\ quantities}$"
    "\n"
    r"   1) share of transactions with $m<0$"
    "\n"
    r"   2) 95-th percentile of $m$"
    "\n\n"
    r"$\bf{Identity\ used}$"
    "\n"
    r"$\Pr(\,U_{\rm cost}>U_{\rm price}\,)$"
    "\n"
    r"   $= E[\mathbf{1}\{U_{\rm cost}>U_{\rm price}\}]$"
    "\n"
    r"   $=$ mean of the logical vector"
    "\n\n"
    r"$\bf{Result}$"
    "\n"
    fr"   $n = {n:,}$"
    "\n"
    fr"   $\#\{{m<0\}} = {n_below:,}$"
    "\n"
    fr"   $\Rightarrow$ {share_neg:.2f}% sold below cost"
    "\n"
    fr"   $p_{{95}}(m) = {p95:.0f}$ USD"
    "\n\n"
    r"$\bf{Interpretation}$"
    "\n"
    r"   $\approx 80\%$ of sales sit in"
    "\n"
    r"   the narrow band $0 \leq m \leq 211$"
    "\n"
    r"   $\Rightarrow$ thin margins on the bulk,"
    "\n"
    r"   losses on $\sim 1/7$ of orders."
)
axB.text(0.06, 0.97, txt, ha="left", va="top", fontsize=10.2,
         color=PALETTE["primary"])

axB.text(0.06, 0.04,
         "R: 100*mean(customer_habits$Unit_Cost\n"
         "          > customer_habits$Unit_Price)\n"
         "   diff.price.cost <- customer_habits$Unit_Price\n"
         "                    - customer_habits$Unit_Cost\n"
         "   distr.summary.x(diff.price.cost, stats=\"p95\")",
         ha="left", va="bottom", fontsize=8.4, family="monospace",
         color=PALETTE["neutral"])

plt.tight_layout()
plt.savefig(OUT, dpi=140, bbox_inches="tight")
plt.close()
print(f"saved -> {OUT}")
