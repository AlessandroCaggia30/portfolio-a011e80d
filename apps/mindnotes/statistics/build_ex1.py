"""
Recompute Ex 1 numbers and regenerate plots for the Statistics workflow.

Outputs:
- PNG plots under apps/mindnotes/statistics/images/
- All numerical results printed for verification against the official solutions
  (which are in /Users/Alessandro/Repos/my note taking app/statistics/ex1/answers/).
"""
import os
import pyreadr
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = os.path.dirname(os.path.abspath(__file__))
IMG = os.path.join(HERE, "images")
os.makedirs(IMG, exist_ok=True)

RDATA = "/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata"
result = pyreadr.read_r(RDATA)
DS = result["DS"].copy()
pizz = result["pizzerie"].copy()
ch = result["customer_habits"].copy()
print("DS:", DS.shape, "| pizzerie:", pizz.shape, "| customer_habits:", ch.shape)


def save(name):
    p = os.path.join(IMG, name)
    plt.tight_layout()
    plt.savefig(p, dpi=120, bbox_inches="tight")
    plt.close()
    print(f"  saved -> images/{name}")


# ===================== EXERCISE 1.1 (pizzerie) =====================

print("\n" + "=" * 60)
print("EXERCISE 1.1 — pizzerie")
print("=" * 60)

# --- 1.1a: SmokingArea pie chart ---
print("\n[1.1a] SmokingArea")
sa = pizz["SmokingArea"].value_counts()
print(sa)
fig, ax = plt.subplots(figsize=(5, 5))
ax.pie(sa.values, labels=sa.index, autopct="%1.0f%%",
       colors=["#e74c3c", "#5fc0c5"], startangle=90)
ax.set_title("Pie chart: SmokingArea")
save("ex1_1a-smoking-pie.png")

# --- 1.1b: District bar plot ---
print("\n[1.1b] District")
dist = pizz["District"].value_counts()
print(dist)
fig, ax = plt.subplots(figsize=(6, 4))
ax.bar(dist.index, dist.values, color="#5fa8d3", edgecolor="black")
ax.set_xlabel("District"); ax.set_ylabel("Counts")
ax.set_title("Bar plot: District")
save("ex1_1b-district-bar.png")

# --- 1.1c: Sales histogram (suitable plot — verified via 1.1d below) ---

# --- 1.1d: Sales histograms (breaks=15 and custom) ---
print("\n[1.1d] Sales histograms")
custom_breaks = [10000, 15000, 20000, 25000, 35000, 50000, 70000]
fig, axes = plt.subplots(1, 2, figsize=(11, 4))
axes[0].hist(pizz["Sales"], bins=15, color="#5fa8d3", edgecolor="black")
axes[0].set_title("Histogram: Sales, breaks=15")
axes[0].set_xlabel("Sales"); axes[0].set_ylabel("Counts")
axes[1].hist(pizz["Sales"], bins=custom_breaks, density=True,
             color="#5fa8d3", edgecolor="black")
axes[1].set_title("Histogram: Sales, custom (density)")
axes[1].set_xlabel("Sales"); axes[1].set_ylabel("Density")
save("ex1_1d-sales-hists.png")

# --- 1.1e: proportion Sales in [15000, 30000) ---
print("\n[1.1e] P(Sales in [15000, 30000))")
prop_1e = ((pizz["Sales"] >= 15000) & (pizz["Sales"] < 30000)).mean()
print(f"  mean(Sales >= 15000 & Sales < 30000) = {prop_1e:.4f}  ({prop_1e*100:.1f}%)")

# --- 1.1f: Mode of District ---
print("\n[1.1f] Mode of District")
mode_district = dist.idxmax()
mode_count = dist.max()
print(f"  mode = {mode_district}  ({mode_count}/100 = {mode_count/100:.2f})")
print("  full table (percent):")
print((dist / dist.sum() * 100).round(0))
# Pie chart for District
fig, ax = plt.subplots(figsize=(5, 5))
ax.pie(dist.values, labels=dist.index, autopct="%1.0f%%",
       colors=["#e74c3c", "#5fc0c5", "#3498db"], startangle=90)
ax.set_title("Pie chart: District")
save("ex1_1f-district-pie.png")

# --- 1.1g: proportion Employees < 5 ---
print("\n[1.1g] P(Employees < 5)")
prop_1g = (pizz["Employees"] < 5).mean()
print(f"  mean(Employees < 5) = {prop_1g:.4f}  ({prop_1g*100:.1f}%)")
emp_table = pizz["Employees"].value_counts().sort_index()
print("  Employees freq:")
emp_cum = emp_table.cumsum()
emp_prop = emp_table / emp_table.sum()
for v in emp_table.index:
    print(f"    {v}: count={emp_table[v]}  prop={emp_prop[v]:.2f}  cum.count={emp_cum[v]}  cum.prop={emp_cum[v]/emp_table.sum():.2f}")

# --- 1.1h: mean & median of Sales ---
print("\n[1.1h] mean / median of Sales")
print(f"  mean   = {pizz['Sales'].mean():.2f}")
print(f"  median = {pizz['Sales'].median():.1f}")

# --- 1.1i: approximate mean & median from grouped data (Brescia survey) ---
print("\n[1.1i] Approx mean & median from grouped Brescia data")
brescia_props = {"[0,15000)": 0.21, "[15000,30000)": 0.63, "[30000,90000)": 0.16}
midpoints = {"[0,15000)": 7500, "[15000,30000)": 22500, "[30000,90000)": 60000}
mean_brescia = sum(midpoints[k] * brescia_props[k] for k in brescia_props)
print(f"  Approx mean = 7500*0.21 + 22500*0.63 + 60000*0.16 = {mean_brescia}")
# Median: cumulative 0.21, 0.84, 1.00 -> median class is [15000, 30000)
# p50 = 15000 + (0.5 - 0.21) * 15000 / 0.63
p50_brescia = 15000 + (0.5 - 0.21) * 15000 / 0.63
print(f"  Approx median = 15000 + 29 * 15000 / 63 = {p50_brescia:.2f}")

# ===================== EXERCISE 1.2 (DS) =====================

print("\n" + "=" * 60)
print("EXERCISE 1.2 — DS")
print("=" * 60)

# --- 1.2a: % Age <= Middle ---
print("\n[1.2a] Age 'at most Middle' percent")
ds_age_levels = ["Young", "Middle", "Senior"]
DS["Age_recode"] = pd.Categorical(DS["Age"], categories=ds_age_levels, ordered=True)
age_table = DS["Age_recode"].value_counts().reindex(ds_age_levels)
age_perc = (age_table / age_table.sum() * 100).round(0)
age_cum = age_perc.cumsum()
print("  Age table (Percent, Cum.Percent):")
for lvl in ds_age_levels:
    print(f"    {lvl}: {age_perc[lvl]:.0f}  {age_cum[lvl]:.0f}")
print(f"  P(Age <= Middle) = {age_cum['Middle']:.0f}%")

# --- 1.2b: summary measures for Age (mode + median) ---
print("\n[1.2b] mode & median of Age")
print(f"  mode = {age_table.idxmax()}  ({age_table.max()/age_table.sum()*100:.0f}%)")
median_pos = (age_cum >= 50).idxmax()
print(f"  median (first level with cum.percent >= 50%) = {median_pos}")

# --- 1.2c: spike plot for Children ---
print("\n[1.2c] Children spike plot")
ch_table = DS["Children"].value_counts().sort_index()
ch_perc = (ch_table / ch_table.sum() * 100).round(0)
print("  Children table:")
for v in ch_table.index:
    print(f"    {v}: Count={ch_table[v]}  Percent={ch_perc[v]:.0f}")
fig, ax = plt.subplots(figsize=(6, 4))
ax.vlines(ch_table.index, 0, ch_table.values, color="black")
ax.plot(ch_table.index, ch_table.values, "o", color="black")
ax.set_xlabel("DS$Children"); ax.set_ylabel("Counts")
ax.set_title("Spike plot: DS$Children")
ax.set_ylim(0, ch_table.values.max() * 1.1)
ax.set_xticks(ch_table.index)
save("ex1_2c-children-spike.png")

# --- 1.2d: History pie chart + bar plot alternative ---
print("\n[1.2d] History pie & bar")
hist_order = ["None", "Low", "Medium", "High"]
DS["History_recode"] = pd.Categorical(DS["History"], categories=hist_order, ordered=True)
h_table = DS["History_recode"].value_counts().reindex(hist_order)
print(h_table)
fig, axes = plt.subplots(1, 2, figsize=(11, 5))
axes[0].pie(h_table.dropna().values, labels=h_table.dropna().index, autopct="%1.0f%%",
            colors=["#5fc0c5", "#5fa8d3", "#3498db", "#1b6ca8"])
axes[0].set_title("Pie chart: DS$History")
axes[1].bar(h_table.index, h_table.values, color="#5fa8d3", edgecolor="black")
axes[1].set_title("Bar plot: DS$History_recode")
axes[1].set_xlabel("DS$History_recode"); axes[1].set_ylabel("Counts")
save("ex1_2d-history-pie-bar.png")

# ===================== EXERCISE 1.3 (customer_habits) =====================

print("\n" + "=" * 60)
print("EXERCISE 1.3 — customer_habits")
print("=" * 60)

# --- 1.3a: data inventory ---
print("\n[1.3a] customer_habits structure")
print(f"  rows = {len(ch)}, cols = {len(ch.columns)}")
print("  dtypes:")
for c in ch.columns:
    print(f"    {c}: {ch[c].dtype}")

# --- 1.3b: Sex composition ---
print("\n[1.3b] Sex composition")
sex_table = ch["Sex"].value_counts()
sex_prop = sex_table / sex_table.sum()
print(sex_table)
print("Prop:", sex_prop.round(2).to_dict())

# --- 1.3c: Sex pie + bar ---
fig, axes = plt.subplots(1, 2, figsize=(11, 4.5))
axes[0].pie(sex_table.values, labels=sex_table.index, autopct="%1.0f%%",
            colors=["#5fc0c5", "#e74c3c"])
axes[0].set_title("Pie chart: customer_habits$Sex")
axes[1].bar(sex_table.index, sex_table.values, color="#5fa8d3", edgecolor="black")
axes[1].set_title("Bar plot: customer_habits$Sex")
axes[1].set_xlabel("customer_habits$Sex"); axes[1].set_ylabel("Counts")
save("ex1_3c-sex-pie-bar.png")

# --- 1.3d: Country: are most European? Mode? ---
print("\n[1.3d] Country composition")
country_table = ch["Country"].value_counts()
country_perc = (country_table / country_table.sum() * 100).round(1)
print(country_table)
print("Percent:", country_perc.to_dict())
eu_share = country_perc.drop("United States").sum()
print(f"  European share (France + Germany + UK) = {eu_share:.1f}%")
print(f"  mode of Country = {country_table.idxmax()}  ({country_perc.max():.1f}%)")

fig, axes = plt.subplots(1, 2, figsize=(12, 5))
axes[0].pie(country_table.values, labels=country_table.index, autopct="%1.1f%%",
            colors=["#e74c3c", "#5fc0c5", "#9b59b6", "#3498db"])
axes[0].set_title("Pie chart: customer_habits$Country")
axes[1].bar(country_table.index, country_table.values, color="#5fa8d3", edgecolor="black")
axes[1].set_title("Bar plot: customer_habits$Country")
axes[1].set_xlabel("customer_habits$Country"); axes[1].set_ylabel("Counts")
axes[1].tick_params(axis="x", rotation=20)
save("ex1_3d-country-pie-bar.png")

# --- 1.3e: spike plot of Age ---
print("\n[1.3e] Age spike plot")
age_summ = ch["Age"].describe()
print(age_summ)
age_perc_table = ch["Age"].value_counts(normalize=True) * 100
age_perc_table = age_perc_table.sort_index()
fig, ax = plt.subplots(figsize=(8, 4))
ax.vlines(age_perc_table.index, 0, age_perc_table.values, color="black", linewidth=0.6)
ax.set_xlabel("customer_habits$Age"); ax.set_ylabel("Percentages")
ax.set_title("Spike plot: customer_habits$Age")
save("ex1_3e-age-spike.png")

# --- 1.3f: mean vs median of Age ---
print("\n[1.3f] Age mean vs median")
print(f"  median(Age) = {ch['Age'].median():.0f}")
print(f"  mean(Age)   = {ch['Age'].mean():.2f}")
print(f"  q1 = {ch['Age'].quantile(0.25):.0f}  q3 = {ch['Age'].quantile(0.75):.0f}")
print(f"  sd = {ch['Age'].std():.2f}")

# --- 1.3g: Revenue histogram, breaks=10 ---
print("\n[1.3g] Revenue histogram")
fig, ax = plt.subplots(figsize=(6, 4))
ax.hist(ch["Revenue"], bins=10, density=True, color="#5fa8d3", edgecolor="black")
ax.set_xlabel("Revenue"); ax.set_ylabel("Densities")
ax.set_title("Histogram: Revenue")
save("ex1_3g-revenue-hist.png")

# --- 1.3h: Revenue histograms with custom widths ---
print("\n[1.3h] Revenue custom histograms")
breaks_1000 = [x * 1000 for x in [0, 1, 2, 3, 4, 5, 9, 16]]
breaks_500 = [x * 500 for x in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 18, 32]]
fig, axes = plt.subplots(1, 2, figsize=(11, 4))
axes[0].hist(ch["Revenue"], bins=breaks_1000, density=True, color="#5fa8d3", edgecolor="black")
axes[0].set_title("Histogram: Revenue, 1000-width")
axes[0].set_xlabel("Revenue"); axes[0].set_ylabel("Densities")
axes[1].hist(ch["Revenue"], bins=breaks_500, density=True, color="#5fa8d3", edgecolor="black")
axes[1].set_title("Histogram: Revenue, 500-width")
axes[1].set_xlabel("Revenue"); axes[1].set_ylabel("Densities")
save("ex1_3h-revenue-hist-comparison.png")

# --- 1.3i: Revenue mean vs median (heavy right skew) ---
print("\n[1.3i] Revenue summary")
print(ch["Revenue"].describe())
print(f"  median(Revenue) = {ch['Revenue'].median():.2f}")
print(f"  mean(Revenue)   = {ch['Revenue'].mean():.2f}")

# ===================== EXERCISE 1.4 (Quantity_New — given in the prompt) =====================

print("\n" + "=" * 60)
print("EXERCISE 1.4 — Quantity_New (2022-2023 hypothetical survey)")
print("=" * 60)

# 2022-2023 table from prompt/answer:
qn_vals  = [1, 2, 3, 4, 6]
qn_count = [5401, 7340, 8238, 2561, 700]
qn_N = sum(qn_count)
qn_perc = [c / qn_N * 100 for c in qn_count]
qn_cumperc = list(np.cumsum(qn_perc))
print("  Quantity_New 2022-2023:")
for v, c, p, cp in zip(qn_vals, qn_count, qn_perc, qn_cumperc):
    print(f"    {v}: Count={c}  Percent={p:.1f}  Cum.Percent={cp:.1f}")
print(f"  N = {qn_N}")

# --- 1.4a1: spike plot (with counts and percentages), and contrast with bar plot ---
print("\n[1.4a1] Quantity_New spike vs (wrong) bar")
fig, axes = plt.subplots(1, 3, figsize=(15, 4))
axes[0].vlines(qn_vals, 0, qn_count, color="black")
axes[0].plot(qn_vals, qn_count, "o", color="black")
axes[0].set_xlabel("Quantity_New"); axes[0].set_ylabel("Counts")
axes[0].set_title("Spike plot: Quantity_New")
axes[0].set_xticks(range(0, 7))
axes[1].vlines(qn_vals, 0, qn_perc, color="black")
axes[1].plot(qn_vals, qn_perc, "o", color="black")
axes[1].set_xlabel("Quantity_New"); axes[1].set_ylabel("Percentages")
axes[1].set_title("Spike plot: Quantity_New (perc.)")
axes[1].set_xticks(range(0, 7))
# Wrong bar plot (illustrative)
axes[2].bar(range(len(qn_vals)), qn_perc, color="#5fa8d3", edgecolor="black")
axes[2].set_xticks(range(len(qn_vals)))
axes[2].set_xticklabels([str(v) for v in qn_vals])
axes[2].set_xlabel("Quantity_New"); axes[2].set_ylabel("Percentages")
axes[2].set_title("Bar plot: Quantity_New (misleading — gap '5')")
save("ex1_4a1-quantity-spike-bar.png")

# --- 1.4a2: cumulative freq curve (step) ---
print("\n[1.4a2] Cumulative freq curve for Quantity_New")
fig, ax = plt.subplots(figsize=(6, 4))
x_step, y_step = [], []
prev = 0
for v, cp in zip(qn_vals, qn_cumperc):
    x_step += [v, v]; y_step += [prev, cp]
    prev = cp
x_step += [qn_vals[-1] + 1]; y_step += [prev]
ax.plot(x_step, y_step, color="black")
ax.set_xlabel("Quantity_New"); ax.set_ylabel("Cumulative Percentages")
ax.set_title("Cumulative freq: Quantity_New")
ax.set_ylim(0, 105)
ax.set_xticks(range(0, 8))
save("ex1_4a2-quantity-cumulative.png")

# --- 1.4a3: mode, median, mean for Quantity_New ---
print("\n[1.4a3] Mode/median/mean of Quantity_New")
mode_qn = qn_vals[qn_count.index(max(qn_count))]
print(f"  mode = {mode_qn}")
# median: first value where cumulative >= 50%
median_qn = next(v for v, cp in zip(qn_vals, qn_cumperc) if cp >= 50)
print(f"  median = {median_qn}  (first value with cum.percent >= 50)")
mean_qn = sum(v * c for v, c in zip(qn_vals, qn_count)) / qn_N
print(f"  mean   = ({' + '.join(f'{c}*{v}' for v,c in zip(qn_vals,qn_count))}) / {qn_N} = {mean_qn:.3f}")

# --- 1.4b: comparison 2015-2016 vs 2022-2023 ---
print("\n[1.4b] 2015-2016 vs 2022-2023 comparison")
# 2015-2016 = whole customer_habits Quantity
q15 = ch["Quantity"].value_counts().sort_index()
print("  2015-2016 (from customer_habits$Quantity):")
print("    counts:", q15.to_dict())
total_15 = q15.sum()
perc_15 = (q15 / total_15 * 100).round(1)
print("    perc:", perc_15.to_dict())
mode_15 = int(q15.idxmax())
median_15 = int(ch["Quantity"].median())
mean_15 = ch["Quantity"].mean()
print(f"    mode={mode_15}  median={median_15}  mean={mean_15:.2f}")
print("  2022-2023 (from given Quantity_New):")
print(f"    mode={mode_qn}  median={median_qn}  mean={mean_qn:.2f}")

# Side-by-side spike plots
fig, axes = plt.subplots(1, 2, figsize=(11, 4))
axes[0].vlines(q15.index, 0, q15.values / total_15 * 100, color="black")
axes[0].plot(q15.index, q15.values / total_15 * 100, "o", color="black")
axes[0].set_title("Spike plot: Quantity (2015-2016)")
axes[0].set_xlabel("Quantity"); axes[0].set_ylabel("Percent")
axes[0].set_xticks(range(0, 7)); axes[0].set_ylim(0, max(qn_perc) * 1.2)
axes[1].vlines(qn_vals, 0, qn_perc, color="black")
axes[1].plot(qn_vals, qn_perc, "o", color="black")
axes[1].set_title("Spike plot: Quantity_New (2022-2023)")
axes[1].set_xlabel("Quantity_New"); axes[1].set_ylabel("Percent")
axes[1].set_xticks(range(0, 7)); axes[1].set_ylim(0, max(qn_perc) * 1.2)
save("ex1_4b-quantity-periods.png")

# ===================== EXERCISE 1.5 (Time — given in prompt) =====================

print("\n" + "=" * 60)
print("EXERCISE 1.5 — Time (data given in question, 1800 customers)")
print("=" * 60)

t_classes = [(0, 10), (10, 20), (20, 30), (30, 60), (60, 90), (90, 150)]
t_count = [122, 420, 294, 176, 571, 217]
t_N = sum(t_count)
t_widths = [b - a for a, b in t_classes]
t_relfreq = [c / t_N for c in t_count]
t_density = [r / w for r, w in zip(t_relfreq, t_widths)]
t_cumfreq = list(np.cumsum(t_relfreq))

print("  Time table:")
print(f"  {'class':12s} {'count':>6s} {'relfreq':>10s} {'cum':>8s} {'width':>6s} {'density':>10s}")
for (a, b), c, r, cu, w, d in zip(t_classes, t_count, t_relfreq, t_cumfreq, t_widths, t_density):
    print(f"  [{a},{b})  {c:>6d}  {r:>10.4f}  {cu:>8.4f}  {w:>6d}  {d:>10.5f}")
print(f"  TOTAL: {t_N}")

# --- 1.5a: P(Time <= 5) under uniform on [0,10) ---
print("\n[1.5a] P(Time <= 5) under uniform-on-[0,10)")
freq_le5 = 61 / 1800 * 100
print(f"  = 61/1800 * 100 = {freq_le5:.2f}%")

# --- 1.5b: P(Time <= 30) exact ---
print("\n[1.5b] P(Time <= 30) exact")
freq_le30 = sum(t_count[:3]) / t_N * 100
print(f"  = (122+420+294)/1800 * 100 = {freq_le30:.2f}%")

# --- 1.5c: P(15 <= Time <= 50) (approx, uniform-on-interval) ---
print("\n[1.5c] P(15 <= Time <= 50) approx")
# half of [10,20) class -> 420/2 = 210
# all of [20,30) -> 294
# 20/30 of [30,60) class -> 20/30*176 = 117.33
freq_15_50 = (210 + 294 + 20 / 30 * 176) / t_N * 100
print(f"  = (210 + 294 + 117.33)/1800 * 100 = {freq_15_50:.2f}%")

# --- 1.5d: ogive + median estimation ---
print("\n[1.5d] Ogive and median")
fig, ax = plt.subplots(figsize=(6, 4.5))
edges = [0] + [b for _, b in t_classes]
cum_pts = [0] + t_cumfreq
ax.plot(edges, cum_pts, "o-", color="black")
ax.set_xlabel("Time"); ax.set_ylabel("Cumulative Proportions")
ax.set_title("Ogive: Time")
ax.set_ylim(0, 1.05)
save("ex1_5d-time-ogive.png")
# Median p50: cum at 30 is 0.464, cum at 60 is 0.562 -> median in [30,60)
# p50 = 30 + (0.5 - 0.464) / d[3] where d[3] = 0.00327
p50 = 30 + (0.5 - t_cumfreq[2]) / t_density[3]
print(f"  cum at 30 = {t_cumfreq[2]:.3f}, at 60 = {t_cumfreq[3]:.3f}")
print(f"  p50 = 30 + (0.5 - 0.464)/0.00327 = {p50:.2f}")
# Alternative form
p50_alt = 30 + (0.5 - t_cumfreq[2]) * 30 / t_relfreq[3]
print(f"  alt: 30 + (0.5 - 0.464) * 30 / 0.098 = {p50_alt:.2f}")

# --- 1.5e: Modal class using densities ---
print("\n[1.5e] Modal class via densities")
modal_idx = int(np.argmax(t_density))
ca, cb = t_classes[modal_idx]
print(f"  Densities: {[round(d,5) for d in t_density]}")
print(f"  Modal class: [{ca},{cb}) (density {t_density[modal_idx]:.5f})")

# --- 1.5f: mean using midpoints ---
print("\n[1.5f] Approx mean of Time using midpoints")
mids = [(a + b) / 2 for a, b in t_classes]
mean_t = sum(c * m for c, m in zip(t_count, mids)) / t_N
print(f"  midpoints = {mids}")
print(f"  mean = (122*5 + 420*15 + 294*25 + 176*45 + 571*75 + 217*120)/1800 = {mean_t:.2f}")

# --- 1.5g: histogram of Time with densities ---
fig, ax = plt.subplots(figsize=(6, 4))
for (a, b), d in zip(t_classes, t_density):
    ax.bar(a, d, width=b - a, align="edge", color="#5fa8d3", edgecolor="black")
ax.set_xlabel("Time"); ax.set_ylabel("Densities")
ax.set_title("Histogram: Time")
save("ex1_5g-time-hist.png")

# --- 1.5h: split into Time <= 30 (n=836) and Time > 30 (n=964) ---
print("\n[1.5h] Subgroup mean/median (split at 30 min)")
# Group A: Time <= 30
ga_count = t_count[:3]
ga_classes = t_classes[:3]
ga_widths = t_widths[:3]
ga_N = sum(ga_count)
ga_rel = [c / ga_N for c in ga_count]
ga_cum = list(np.cumsum(ga_rel))
print(f"  Group A (Time<=30): N={ga_N}")
for (a, b), c, r, cu in zip(ga_classes, ga_count, ga_rel, ga_cum):
    print(f"    [{a},{b}): count={c} rel={r:.3f} cum={cu:.3f}")
# median in [10,20)
p50_a = 10 + (0.5 - ga_cum[0]) / (ga_rel[1] / ga_widths[1])
print(f"  Median_A = 10 + (0.5 - 0.146)/0.502 * 10 = {p50_a:.2f}")
# mean by midpoints
mean_a = sum(c * m for c, m in zip(ga_count, [5, 15, 25])) / ga_N
print(f"  Mean_A   = (122*5 + 420*15 + 294*25)/836 = {mean_a:.2f}")

# Group B: Time > 30
gb_count = t_count[3:]
gb_classes = t_classes[3:]
gb_widths = t_widths[3:]
gb_N = sum(gb_count)
gb_rel = [c / gb_N for c in gb_count]
gb_cum = list(np.cumsum(gb_rel))
print(f"  Group B (Time>30): N={gb_N}")
for (a, b), c, r, cu in zip(gb_classes, gb_count, gb_rel, gb_cum):
    print(f"    [{a},{b}): count={c} rel={r:.3f} cum={cu:.3f}")
# median: cum after [30,60) is 0.183, after [60,90) is 0.775 -> median in [60,90)
p50_b = 60 + (0.5 - gb_cum[0]) / (gb_rel[1] / gb_widths[1])
print(f"  Median_B = 60 + (0.5 - 0.183)/0.592 * 30 = {p50_b:.2f}")
mean_b = sum(c * m for c, m in zip(gb_count, [45, 75, 120])) / gb_N
print(f"  Mean_B   = (176*45 + 571*75 + 217*120)/964 = {mean_b:.2f}")

# ===================== EXERCISE 1.6 (Expenses — histogram in prompt) =====================

print("\n" + "=" * 60)
print("EXERCISE 1.6 — Expenses (histogram given in prompt)")
print("=" * 60)

exp_classes = [(0, 20), (20, 40), (40, 60), (60, 80), (80, 120), (120, 160), (160, 300)]
exp_density = [0.022, 0.01, 0.005, 0.003, 0.002, 0.001, 0.0006]
# Recompute prop from density × width, but the [120,160) one is "inferred to make sum=1":
exp_widths = [b - a for a, b in exp_classes]
# Compute the implied frequency for the unclear interval [120,160):
prop_others = sum(d * w for (d, w, (a, b)) in zip(exp_density, exp_widths, exp_classes)
                  if not (a == 120 and b == 160))
# But actually we know d for [120,160) was inferred from "1 - sum(others without it)"
# Source says: density [120,160) is 0.001, freq = 0.04 (1 minus others)
# Let me check by computing all directly
print("  expense table from densities:")
expflag = []
for d, w, (a, b) in zip(exp_density, exp_widths, exp_classes):
    freq = d * w
    expflag.append(freq)
    print(f"    [{a},{b}): width={w}  density={d}  freq={freq:.4f}")
print(f"  Sum of freq = {sum(expflag):.4f}  (rounded densities -> not exactly 1)")
# Source uses freq for [120,160) = 1 - sum(others) = 1 - (0.44+0.2+0.1+0.06+0.08+0.084) = 1 - 0.964 = 0.036
# but source says 0.04. Let's match what source does:
exp_freq_source = [0.44, 0.20, 0.10, 0.06, 0.08, 0.04, 0.084]
print(f"  Source freqs: {exp_freq_source}, sum = {sum(exp_freq_source):.4f}")

# --- 1.6b: approx median and mean ---
print("\n[1.6b] Approx median and mean for Expenses (source rule)")
cum_e = list(np.cumsum(exp_freq_source))
print("  cumulative:", [round(c, 3) for c in cum_e])
# median: cum at 20 is 0.44 -> median in [20,40)? Actually 0.44 > 0.5? No, 0.44 < 0.5.
# After [20,40) cum = 0.64 >= 0.5 -> median in [20,40)
# Wait, source said approximated median = 26
# Density in [20,40) = 0.01, so p50 = 20 + (0.5 - 0.44)/0.01 = 26
median_e = 20 + (0.5 - 0.44) / 0.01
print(f"  median = 20 + (0.5 - 0.44)/0.01 = {median_e:.2f}")
# mean via midpoints
mids_e = [(a + b) / 2 for a, b in exp_classes]
mean_e = sum(f * m for f, m in zip(exp_freq_source, mids_e))
print(f"  midpoints = {mids_e}")
print(f"  mean = sum(f_i * m_i) = {mean_e:.2f}")

print("\nALL DONE.")
