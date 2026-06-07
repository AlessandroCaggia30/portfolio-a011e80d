"""Build AI walkthrough plot for Ex 4.7 — airport security Y = B + D, 90th percentile."""
import os, sys
sys.path.insert(0, "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics")
from plot_style import apply_style, PALETTE
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

apply_style()

OUT = "/Users/Alessandro/Repos/portfolio-a011e80d/apps/mindnotes/statistics/images/ex4/ex4_7_ai.png"

# --- Given ---
mu_B, var_B = 10.0, 4.0          # B ~ N(10, 4)
mu_D, var_D = 3.0,  1.0          # D ~ N(3, 1)
rho         = 0.4375
sd_B, sd_D  = np.sqrt(var_B), np.sqrt(var_D)
cov_BD      = rho * sd_B * sd_D
mu_Y        = mu_B + mu_D                     # 13
var_Y       = var_B + var_D + 2 * cov_BD      # 6.75
sd_Y        = np.sqrt(var_Y)                  # ~2.598
q90         = norm.ppf(0.9, loc=mu_Y, scale=sd_Y)   # ~16.33

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13, 5))

# --- LEFT: density of Y with 90th percentile shaded ---
xs = np.linspace(mu_Y - 4*sd_Y, mu_Y + 4*sd_Y, 600)
pdf = norm.pdf(xs, mu_Y, sd_Y)
ax1.plot(xs, pdf, color=PALETTE["primary"], lw=2.4, label=fr"$Y \sim N(13,\,6.75)$")
mask = xs >= q90
ax1.fill_between(xs[mask], 0, pdf[mask], color=PALETTE["accent"], alpha=0.55,
                 label=fr"$P(Y \geq {q90:.2f}) = 0.10$")
ax1.fill_between(xs[~mask], 0, pdf[~mask], color=PALETTE["primary"], alpha=0.10)
ax1.axvline(q90, color=PALETTE["warn"], lw=1.6, ls="--")
ax1.axvline(mu_Y, color=PALETTE["neutral"], lw=1.0, ls=":", alpha=0.7)
ax1.text(mu_Y, ax1.get_ylim()[1]*0.97, r"  $E[Y]=13$",
         color=PALETTE["neutral"], fontsize=10, ha="left", va="top")
ax1.text(q90, norm.pdf(q90, mu_Y, sd_Y)*1.05,
         fr"  $q_{{0.90}} \approx {q90:.2f}$ min",
         color=PALETTE["warn"], fontsize=11, ha="left", va="bottom", fontweight="bold")
ax1.set_xlabel(r"$Y = B + D$  (minutes)")
ax1.set_ylabel("density")
ax1.set_title(r"Total screening time $Y=B+D$ and its 90-th percentile")
ax1.legend(loc="upper right", framealpha=0.95)

# --- RIGHT: timeline showing arrival -> security -> 45-min buffer -> flight ---
ax2.set_xlim(0, 100)
ax2.set_ylim(-1.0, 1.0)
ax2.set_yticks([])
ax2.grid(False)
for s in ("left", "right"):
    ax2.spines[s].set_visible(False)

# Timeline events (minutes before 17:00)
flight = 0           # 17:00
buffer_end = 45      # 16:15 — done with security by here
arrival = 45 + q90   # ~61.33 min before flight = 15:58.7

# Main timeline bar
ax2.plot([0, arrival + 8], [0, 0], color=PALETTE["neutral"], lw=2.0, alpha=0.4)

# Spans
ax2.add_patch(plt.Rectangle((flight, -0.18), buffer_end, 0.36,
              color=PALETTE["primary"], alpha=0.18, lw=0))
ax2.add_patch(plt.Rectangle((buffer_end, -0.18), q90, 0.36,
              color=PALETTE["accent"], alpha=0.55, lw=0))

# Event markers
events = [
    (flight,     "17:00\nflight",      PALETTE["warn"]),
    (buffer_end, "16:15\ndone w/ sec.", PALETTE["primary"]),
    (arrival,    f"{15}:{int(60 - (arrival-45-15)):02d}\narrive",  PALETTE["ok"]),
]
# Build label for arrival in HH:MM
total_min_before = arrival  # minutes before 17:00
hh = 17 - 1
mm = 60 - (total_min_before - 60)
# Convert to actual clock time
flight_minutes = 17 * 60
arrival_clock_minutes = flight_minutes - total_min_before
ah = int(arrival_clock_minutes // 60)
am = int(round(arrival_clock_minutes - ah * 60))
arrival_label = f"{ah:02d}:{am:02d}\narrive"

events = [
    (flight,     "17:00\nflight",       PALETTE["warn"]),
    (buffer_end, "16:15\ndone w/ sec.", PALETTE["primary"]),
    (arrival,    arrival_label,         PALETTE["ok"]),
]

for x, lbl, col in events:
    ax2.plot([x], [0], "o", ms=11, color=col, zorder=5,
             markeredgecolor="#2a3142", markeredgewidth=0.8)
    ax2.annotate(lbl, xy=(x, 0), xytext=(x, 0.55),
                 ha="center", va="bottom", fontsize=10.5,
                 color=col, fontweight="bold")

# Span labels
ax2.text(buffer_end/2, -0.45, "45-min buffer",
         ha="center", va="top", color=PALETTE["primary"], fontsize=11)
ax2.text(buffer_end + q90/2, -0.45,
         fr"security $\leq q_{{0.90}}\approx{q90:.2f}$ min",
         ha="center", va="top", color="#7a6800", fontsize=11)

# Direction note
ax2.annotate("", xy=(-1, 0), xytext=(arrival + 8, 0),
             arrowprops=dict(arrowstyle="->", color=PALETTE["neutral"], lw=1.0))
ax2.text(arrival + 8, 0.08, "time", color=PALETTE["neutral"],
         fontsize=10, ha="right", va="bottom")

ax2.invert_xaxis()  # so time flows left -> right toward the flight
ax2.set_xlabel("minutes before 17:00")
ax2.set_title("Arrival time so that  $P(\\text{secured 45+ min before flight}) \\geq 0.90$")

plt.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
plt.savefig(OUT, dpi=140, bbox_inches="tight")
print(f"saved -> {OUT}")
print(f"  E[Y] = {mu_Y}, Var(Y) = {var_Y}, SD(Y) = {sd_Y:.4f}")
print(f"  Cov(B,D) = {cov_BD}")
print(f"  q_{{0.90}}(Y) = {q90:.4f} min")
print(f"  arrival clock = {ah:02d}:{am:02d}  ({arrival:.2f} min before 17:00)")
