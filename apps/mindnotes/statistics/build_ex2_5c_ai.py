"""Build AI walkthrough plot for Ex 2.5c — proportions of customers in
overlapping age intervals from customer_habits, and the resulting target choice."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2/ex2_5c_ai.png"

# --- Proportions reported in the textbook answer (from mean(Age in [a,b])) ---
intervals = [(20, 40), (30, 50), (40, 60), (50, 96)]  # last one = ">50" (Age max = 96)
labels    = ["[20, 40]", "[30, 50]", "[40, 60]", "> 50"]
props     = np.array([0.2801296, 0.6256812, 0.6470487, 0.3459244])

# Five-number summary of Age (from the dataset; reused from 2.5a)
Q1, Med, Q3, Mn, Mx = 40, 46, 54, 16, 96

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5.2))

# ---------- LEFT: age axis with overlapping intervals as horizontal bands ----------
y_positions = np.arange(len(intervals))[::-1]  # top-to-bottom = first-to-last
band_h = 0.55

for y, (lo, hi), p, lab in zip(y_positions, intervals, props, labels):
    # Show the actual interval span on the age axis
    ax1.add_patch(Rectangle((lo, y - band_h/2), hi - lo, band_h,
                            facecolor=PALETTE["primary"], alpha=0.18,
                            edgecolor=PALETTE["primary"], linewidth=1.1))
    # Mark the interval label and the proportion
    ax1.text(lo - 0.7, y, lab, ha="right", va="center",
             fontsize=11, color=PALETTE["neutral"])
    ax1.text((lo + hi)/2, y, f"$p = {p:.3f}$", ha="center", va="center",
             fontsize=11.5, fontweight="bold", color=PALETTE["primary"])

# Reference quartile lines from the boxplot in 2.5a
for x, lab, col in [(Q1, f"$Q_1={Q1}$", PALETTE["accent"]),
                    (Med, f"Me$={Med}$", PALETTE["warn"]),
                    (Q3, f"$Q_3={Q3}$", PALETTE["accent"])]:
    ax1.axvline(x, color=col, lw=1.4, ls="--", alpha=0.9)
    ax1.text(x, len(intervals) - 0.05, lab, color=col,
             fontsize=10, ha="center", va="bottom", fontweight="bold")

ax1.set_yticks([])
ax1.set_xlim(15, 97)
ax1.set_ylim(-0.7, len(intervals) - 0.1)
ax1.set_xlabel("Age (years)")
ax1.set_title("Overlapping age intervals on the Age axis")
ax1.grid(axis="y", visible=False)

# ---------- RIGHT: bar chart of the four proportions, winner highlighted ----------
colors = [PALETTE["primary"]] * len(intervals)
winner = int(np.argmax(props))
colors[winner] = PALETTE["accent"]

bars = ax2.bar(labels, props, color=colors,
               edgecolor="#2a3142", linewidth=0.9)
for b, p in zip(bars, props):
    ax2.text(b.get_x() + b.get_width()/2, b.get_height() + 0.012,
             f"{p:.3f}", ha="center", va="bottom",
             fontsize=11, fontweight="bold",
             color=PALETTE["accent"] if b is bars[winner] else PALETTE["primary"])

ax2.axhline(0.5, color=PALETTE["muted"], lw=1.0, ls=":")
ax2.text(3.45, 0.51, "50%", color=PALETTE["muted"], fontsize=9.5,
         ha="right", va="bottom")

ax2.set_ylim(0, max(props) * 1.22)
ax2.set_ylabel(r"Proportion  mean$(\mathbb{1}\{a \leq $Age$ \leq b\})$")
ax2.set_title("Sample shares — target = densest group")

# Annotation explaining the rule
note = (
    r"$p = $ mean$(\mathbb{1}\{a \leq $Age$ \leq b\})$" + "\n"
    + r"Target: $\arg\max_{[a,b]} p \;=\; [40,60]$ ($\approx 65\%$)"
)
ax2.text(0.02, 0.97, note, transform=ax2.transAxes,
         ha="left", va="top", fontsize=10,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
for lab, p in zip(labels, props):
    print(f"  {lab:>10}  p = {p:.4f}")
print(f"  best target  = {labels[winner]}  (p = {props[winner]:.4f})")
