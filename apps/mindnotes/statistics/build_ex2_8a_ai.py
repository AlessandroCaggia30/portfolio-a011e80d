"""Build AI walkthrough plot for Ex 2.8a — Build the Margin_perc variable.

Visual: a single-row "transformation pipeline" that turns the two raw columns
`Unit_Price` and `Unit_Cost` from `customer_habits` into `Margin_perc` via
    Margin_perc = 100 * (Unit_Price / Unit_Cost - 1)
A side panel walks the reader through three illustrative rows
(loss / break-even / profit) so the algebraic step is concretely grounded.

Also uses shutil to clone:
  * the shared Exercise 2.8 textbook prompt PNG into
    ex2/questions/ex2_8a_question.png   (same source as 2.8c)
  * the textbook answer crop for Ex 2.8a (label "a)" — variable creation)
    into ex2/answers/ex2_8a_answer.png

The snippet references only PNGs that actually exist after this script runs.
"""
import os, sys, shutil
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch, Rectangle

apply_style()

BASE = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex2"
OUT  = f"{BASE}/ex2_8a_ai.png"

# ---------- 1. Clone the recurring textbook prompt + answer PNGs ----------
# Note: macOS Screenshot filenames use U+202F (narrow no-break space)
# between the time and "PM", NOT a regular ASCII space.
NBSP = "\u202f"
SRC_DIR = "/Users/Alessandro/Repos/my note taking app/statistics/ex2"
PAIRS = [
    (f"{SRC_DIR}/questions/Screenshot 2026-06-05 at 4.12.01{NBSP}PM.png",
     f"{BASE}/questions/ex2_8a_question.png"),
    (f"{SRC_DIR}/answers/Screenshot 2026-06-05 at 4.10.41{NBSP}PM.png",
     f"{BASE}/answers/ex2_8a_answer.png"),
]
for src, dst in PAIRS:
    if os.path.exists(src):
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        shutil.copyfile(src, dst)
        print(f"copied -> {dst}")
    else:
        print(f"WARNING: source PNG not found for -> {dst}")

# ---------- 2. Three illustrative rows (loss / break-even / profit) ----------
rows = [
    ("loss",       20.00, 25.00),
    ("break-even", 30.00, 30.00),
    ("profit",     45.00, 30.00),
]
def margin(up, uc):
    return 100.0 * (up / uc - 1.0)

# ---------- 3. Figure ----------
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.6, 5.4),
                               gridspec_kw={"width_ratios": [1.15, 1.0]})

# =========================================================================
# LEFT — the transformation pipeline (boxes + arrow)
# =========================================================================
ax1.axis("off")
ax1.set_xlim(0, 1); ax1.set_ylim(0, 1)

def box(x, y, w, h, title, body, face, edge):
    ax1.add_patch(Rectangle((x, y), w, h, facecolor=face, edgecolor=edge,
                            linewidth=1.4, alpha=0.92,
                            transform=ax1.transAxes))
    ax1.text(x + w/2, y + h - 0.06, title,
             ha="center", va="top", fontsize=11.5, fontweight="bold",
             color=edge, transform=ax1.transAxes)
    ax1.text(x + w/2, y + h/2 - 0.05, body,
             ha="center", va="center", fontsize=10,
             family="monospace", color=PALETTE["neutral"],
             transform=ax1.transAxes)

# Left block — raw columns
box(0.02, 0.42, 0.30, 0.40,
    "customer_habits  (raw)",
    "Unit_Price :  20.00, 30.00, 45.00, ...\n"
    "Unit_Cost  :  25.00, 30.00, 30.00, ...\n"
    "(n = 34 866 transactions)",
    face="#eef3fb", edge=PALETTE["primary"])

# Arrow with the formula
arrow = FancyArrowPatch((0.33, 0.62), (0.53, 0.62),
                        arrowstyle="->", mutation_scale=22,
                        color=PALETTE["accent"], linewidth=2.2,
                        transform=ax1.transAxes)
ax1.add_patch(arrow)
ax1.text(0.43, 0.74,
         r"$\mathrm{Margin\_perc} = 100 \cdot \left(\dfrac{\mathrm{Unit\_Price}}{\mathrm{Unit\_Cost}} - 1\right)$",
         ha="center", va="center", fontsize=11.5,
         color=PALETTE["primary"], fontweight="bold",
         transform=ax1.transAxes,
         bbox=dict(facecolor="#fffbe6", edgecolor=PALETTE["accent"],
                   boxstyle="round,pad=0.45", linewidth=1.1))
ax1.text(0.43, 0.50,
         "R: a single vectorised\nassignment ($<-$)",
         ha="center", va="center", fontsize=9.5,
         color=PALETTE["muted"], transform=ax1.transAxes,
         style="italic")

# Right block — the new variable
box(0.54, 0.42, 0.44, 0.40,
    "Margin_perc  (new object)",
    "row 1 :  100*(20/25 - 1) = -20.00\n"
    "row 2 :  100*(30/30 - 1) =   0.00\n"
    "row 3 :  100*(45/30 - 1) = +50.00\n"
    "stored as a stand-alone vector\n"
    "in the workspace (length n)",
    face="#fffbe6", edge=PALETTE["accent"])

# Bottom note — workspace vs $-attachment
ax1.text(0.5, 0.30,
         "Note. Because the assignment uses '<-' (not '\\$' on customer_habits),\n"
         "the dataframe customer_habits is left untouched: Margin_perc lives\n"
         "as a separate object in the global workspace.",
         ha="center", va="top", fontsize=9.8,
         color=PALETTE["neutral"], transform=ax1.transAxes,
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.4", linewidth=1.0))

# Headline title
ax1.text(0.5, 0.965,
         "Building Margin_perc from Unit_Price and Unit_Cost",
         ha="center", va="top", fontsize=12.8, fontweight="bold",
         color=PALETTE["primary"], transform=ax1.transAxes)

# =========================================================================
# RIGHT — three worked rows: loss / break-even / profit
# =========================================================================
labels   = [r[0] for r in rows]
ups      = np.array([r[1] for r in rows])
ucs      = np.array([r[2] for r in rows])
margins  = np.array([margin(r[1], r[2]) for r in rows])

bar_colors = []
for m in margins:
    if m < 0:
        bar_colors.append(PALETTE["warn"])
    elif abs(m) < 1e-6:
        bar_colors.append(PALETTE["muted"])
    else:
        bar_colors.append(PALETTE["ok"])

xs = np.arange(len(rows))
bars = ax2.bar(xs, margins, width=0.55,
               color=bar_colors, edgecolor="#2a3142", linewidth=1.1,
               alpha=0.90)

# Annotate each bar with the arithmetic
for i, (m, up, uc) in enumerate(zip(margins, ups, ucs)):
    sign = "+" if m > 0 else ""
    ax2.text(i, m + (3 if m >= 0 else -3),
             f"{sign}{m:.2f}%",
             ha="center", va="bottom" if m >= 0 else "top",
             fontsize=11.5, fontweight="bold",
             color=PALETTE["primary"])
    ax2.text(i, -68,
             f"UP = {up:.2f}\nUC = {uc:.2f}\n"
             f"100*({up:.0f}/{uc:.0f}-1)",
             ha="center", va="top", fontsize=9.2,
             family="monospace", color=PALETTE["neutral"])

# Zero reference line — "sold at cost"
ax2.axhline(0, color=PALETTE["primary"], lw=1.2, ls="--", alpha=0.7)
ax2.text(len(rows) - 0.5, 2, "Margin_perc $= 0$  (sold at cost)",
         ha="right", va="bottom", fontsize=9.5,
         color=PALETTE["primary"], style="italic")

ax2.set_xticks(xs)
ax2.set_xticklabels(labels, fontsize=11)
ax2.set_ylabel("Margin_perc  (%)")
ax2.set_title("Three illustrative rows — sign of Margin_perc")
ax2.set_ylim(-90, 70)
ax2.grid(axis="y", alpha=0.5)

# Legend / takeaway
takeaway = (
    "Sign reading:\n"
    "  $\\bullet$  $< 0$  -> sold below cost (loss)\n"
    "  $\\bullet$  $= 0$  -> break-even\n"
    "  $\\bullet$  $> 0$  -> positive markup"
)
ax2.text(0.02, 0.97, takeaway, transform=ax2.transAxes,
         ha="left", va="top", fontsize=9.8,
         color=PALETTE["neutral"],
         bbox=dict(facecolor="#f6f7fb", edgecolor=PALETTE["grid"],
                   boxstyle="round,pad=0.45", linewidth=1.0))

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
for lbl, up, uc, m in zip(labels, ups, ucs, margins):
    print(f"  {lbl:10s}  UP={up:6.2f}  UC={uc:6.2f}  -> Margin_perc = {m:+.2f}%")
