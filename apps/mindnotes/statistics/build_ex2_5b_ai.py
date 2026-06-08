"""Build AI walkthrough plot for Ex 2.5b — two threshold values (p5, p95)
identifying the youngest 5% and the oldest 5% of customer_habits$Age.

Also uses shutil to clone the recurring Exercise 2.5 textbook prompt image
(already saved as ex2_5c_question.png — the Ex 2.5 page is shared across
sub-parts 5a, 5b, 5c, 5d, 5f, 5g) into the ex2/questions/ folder under the
Ex 2.5b filename, so the snippet can reference it directly.

No specific textbook answer crop exists for Ex 2.5b in this repo, so the
snippet does NOT reference an answer PNG (we only reference what we copy).
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
OUT  = f"{BASE}/ex2_5b_ai.png"

# ---------- 1. Clone the recurring Exercise 2.5 textbook prompt PNG ----------
# The Ex 2.5 prompt page is shared across 5a/5b/5c/5d/5f (identical MD5 hash).
src_q = f"{BASE}/questions/ex2_5c_question.png"
dst_q = f"{BASE}/questions/ex2_5b_question.png"

if os.path.exists(src_q):
    shutil.copyfile(src_q, dst_q)
    print(f"copied -> {dst_q}")
else:
    print(f"WARNING: source question PNG not found at {src_q}")

# ---------- 2. Build the AI walkthrough figure ----------
# Five-number summary of customer_habits$Age (n = 34 866) from 2.5a:
n   = 34866
Mn  = 16
Q1  = 40
Med = 46
Q3  = 54
Mx  = 96

# The two thresholds returned by distr.summary.x(Age, c("p5","p95")):
p5  = 32
p95 = 66

# Approximate Age density on [16, 96]. We use a smooth right-skewed bump
# anchored on the known quartiles (illustrative — not the empirical PDF).
xs = np.linspace(Mn, Mx, 1200)
# Mixture: a dominant bell around Med plus a long upper-tail bump
bell  = np.exp(-0.5 * ((xs - 47) / 9.2) ** 2)
tail  = 0.18 * np.exp(-0.5 * ((xs - 70) / 13.0) ** 2)
left  = 0.07 * np.exp(-0.5 * ((xs - 24) / 6.0) ** 2)
pdf   = bell + tail + left
pdf  /= np.trapz(pdf, xs)            # normalise so total mass = 1

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.2))

# ---------- LEFT: Age distribution with the 5% / 95% tails shaded ----------
ax1.plot(xs, pdf, color=PALETTE["primary"], lw=1.8)
ax1.fill_between(xs, 0, pdf, where=(xs <= p5),
                 color=PALETTE["accent"], alpha=0.55, label="lower 5%")
ax1.fill_between(xs, 0, pdf, where=(xs >= p95),
                 color=PALETTE["warn"],   alpha=0.55, label="upper 5%")
ax1.fill_between(xs, 0, pdf, where=((xs > p5) & (xs < p95)),
                 color=PALETTE["primary"], alpha=0.10)

# Threshold lines
for x, lab, col in [(p5,  f"$p_5 = {p5}$",   PALETTE["accent"]),
                    (p95, f"$p_{{95}} = {p95}$", PALETTE["warn"])]:
    ax1.axvline(x, color=col, lw=1.6, ls="--")
    ax1.text(x, ax1.get_ylim()[1] * 0.92 if ax1.get_ylim()[1] > 0 else max(pdf)*0.95,
             lab, color=col, fontsize=11, fontweight="bold",
             ha="center", va="top",
             bbox=dict(facecolor="white", edgecolor=col,
                       boxstyle="round,pad=0.25", linewidth=0.9))

# Mass annotations
ax1.text((Mn + p5) / 2, max(pdf) * 0.18,
         r"$5\%$ youngest" "\n" r"$\{Age\leq p_5\}$",
         ha="center", va="center", fontsize=10.5,
         color="#2a3142", fontweight="bold")
ax1.text((p95 + Mx) / 2, max(pdf) * 0.18,
         r"$5\%$ oldest" "\n" r"$\{Age\geq p_{95}\}$",
         ha="center", va="center", fontsize=10.5,
         color="#2a3142", fontweight="bold")
ax1.text((p5 + p95) / 2, max(pdf) * 0.55,
         r"central $90\%$ of customers",
         ha="center", va="center", fontsize=11,
         color=PALETTE["primary"], fontweight="bold")

ax1.set_xlim(Mn - 2, Mx + 2)
ax1.set_ylim(0, max(pdf) * 1.18)
ax1.set_xlabel("Age (years)")
ax1.set_ylabel("Density (illustrative)")
ax1.set_title("Age distribution with the two 5% thresholds")

# ---------- RIGHT: cumulative-mass view — defining condition of the percentiles ----------
# Same illustrative pdf -> empirical CDF
cdf = np.cumsum(pdf)
cdf /= cdf[-1]
ax2.plot(xs, cdf, color=PALETTE["primary"], lw=1.9)

# Horizontal levels at 0.05 and 0.95
ax2.axhline(0.05, color=PALETTE["accent"], lw=1.2, ls=":")
ax2.axhline(0.95, color=PALETTE["warn"],   lw=1.2, ls=":")
# Vertical thresholds
ax2.axvline(p5,  color=PALETTE["accent"], lw=1.4, ls="--")
ax2.axvline(p95, color=PALETTE["warn"],   lw=1.4, ls="--")

# Intersection markers + labels
ax2.scatter([p5,  p95], [0.05, 0.95], s=60, zorder=6,
            color=[PALETTE["accent"], PALETTE["warn"]],
            edgecolor="#2a3142", linewidth=0.8)
ax2.annotate(r"$F(p_5)=0.05$", xy=(p5, 0.05),
             xytext=(p5 + 6, 0.18), fontsize=10.5, color=PALETTE["accent"],
             fontweight="bold",
             arrowprops=dict(arrowstyle="->", color=PALETTE["accent"], lw=1.0))
ax2.annotate(r"$F(p_{95})=0.95$", xy=(p95, 0.95),
             xytext=(p95 - 30, 0.78), fontsize=10.5, color=PALETTE["warn"],
             fontweight="bold",
             arrowprops=dict(arrowstyle="->", color=PALETTE["warn"], lw=1.0))

# Axis ticks for the two thresholds
ax2.set_xticks(sorted(set([Mn, p5, Med, p95, Mx])))
ax2.set_xlabel("Age (years)")
ax2.set_ylabel(r"$F(a) = $ Freq$\{Age \leq a\}$")
ax2.set_title("Defining conditions of $p_5$ and $p_{95}$")
ax2.set_xlim(Mn - 2, Mx + 2)
ax2.set_ylim(0, 1.05)

# Verdict box summarising the R output and interpretation
verdict = (
    r"R: distr.summary.x(Age, c('p5','p95'))" + "\n"
    r"   $\to\;p_5 = 32,\;\; p_{95} = 66$" + "\n"
    r"$\bullet$ $5\%$ youngest customers are $\leq 32$ years old." + "\n"
    r"$\bullet$ $5\%$ oldest customers are $\geq 66$ years old." + "\n"
    r"$\bullet$ Central $90\%$ of customers lie in $[32,\,66]$."
)
ax2.text(0.02, 0.98, verdict, transform=ax2.transAxes,
         ha="left", va="top", fontsize=9.8,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  thresholds: p5 = {p5}, p95 = {p95}")
print(f"  central mass between thresholds = 90%")
