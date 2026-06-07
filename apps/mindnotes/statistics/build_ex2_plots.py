"""
Regenerate plots for Statistics Ex 2 snippets and verify against source numbers.
Saves PNGs to apps/mindnotes/statistics/images/ for the snippets to reference.
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

# Ex 1's Rdata holds pizzerie, DS, customer_habits — same source as Ex2 uses
result = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex1/Exe1_Data.Rdata")
pizz = result["pizzerie"].copy()
DS = result["DS"].copy()
ch = result["customer_habits"].copy()
DS["History_recode"] = pd.Categorical(DS["History"], categories=["None","Low","Medium","High"], ordered=True)

print("=" * 70); print("EX 2 — verify + generate plots"); print("=" * 70)

def save(name):
    p = os.path.join(IMG, name)
    plt.tight_layout()
    plt.savefig(p, dpi=120, bbox_inches="tight")
    plt.close()
    print(f"  saved -> images/{name}")

# ---- 2.1b boxplot of Sales (pizzerie) ----
print("\n[2.1b] Sales (pizzerie)")
fns = pizz["Sales"].describe(percentiles=[0.25,0.5,0.75])
print(f"  min={pizz['Sales'].min()}, q1={fns['25%']}, med={fns['50%']}, q3={fns['75%']}, max={pizz['Sales'].max()}")
fig, ax = plt.subplots(figsize=(5, 5))
ax.boxplot(pizz["Sales"], vert=True, patch_artist=True,
           boxprops=dict(facecolor="#5fa8d3"))
ax.set_ylabel("Sales"); ax.set_title("Boxplot: Sales (pizzerie)")
save("ex2_1b-sales-boxplot.png")

# ---- 2.3b/c boxplot + histogram of AmountSpent (DS) ----
print("\n[2.3b/c] AmountSpent (DS)")
print(f"  mean={DS['AmountSpent'].mean():.2f}, var={DS['AmountSpent'].var():.2f}, "
      f"q1={DS['AmountSpent'].quantile(0.25):.2f}, q3={DS['AmountSpent'].quantile(0.75):.2f}")
iqr = DS['AmountSpent'].quantile(0.75) - DS['AmountSpent'].quantile(0.25)
upper_lim = DS['AmountSpent'].quantile(0.75) + 1.5*iqr
n_outl = (DS['AmountSpent'] > upper_lim).sum()
print(f"  upper outlier limit={upper_lim:.2f}, n upper outliers={n_outl}")

fig, axes = plt.subplots(1, 2, figsize=(11, 5))
axes[0].boxplot(DS["AmountSpent"], vert=True, patch_artist=True,
                boxprops=dict(facecolor="#5fa8d3"))
axes[0].set_title("Boxplot: AmountSpent (DS)"); axes[0].set_ylabel("AmountSpent")
axes[1].hist(DS["AmountSpent"], bins=30, color="#5fa8d3", edgecolor="black")
axes[1].set_title("Histogram: AmountSpent (DS)")
axes[1].set_xlabel("AmountSpent"); axes[1].set_ylabel("Counts")
save("ex2_3bc-amountspent-box-hist.png")

# ---- 2.5a/d boxplot of Age (customer_habits) ----
print("\n[2.5a/d] Age (customer_habits)")
age_summary = ch["Age"].describe()
iqr_age = ch["Age"].quantile(0.75) - ch["Age"].quantile(0.25)
upper = ch["Age"].quantile(0.75) + 1.5*iqr_age
lower = ch["Age"].quantile(0.25) - 1.5*iqr_age
n_out_age = ((ch["Age"] < lower) | (ch["Age"] > upper)).sum()
pct_out_age = 100 * n_out_age / len(ch)
print(f"  q1={ch['Age'].quantile(0.25):.0f}, q3={ch['Age'].quantile(0.75):.0f}, IQR={iqr_age:.0f}")
print(f"  upper={upper}, lower={lower}, n_outliers={n_out_age}, pct={pct_out_age:.4f}%")
fig, ax = plt.subplots(figsize=(5, 5))
ax.boxplot(ch["Age"], vert=True, patch_artist=True,
           boxprops=dict(facecolor="#5fa8d3"))
ax.set_ylabel("Age"); ax.set_title("Boxplot: customer_habits$Age")
save("ex2_5a-age-boxplot.png")

# ---- 2.5f histogram of Age with breaks=20 ----
print("\n[2.5f] Age histogram with breaks=20")
fig, ax = plt.subplots(figsize=(6, 4))
ax.hist(ch["Age"], bins=20, density=True, color="#5fa8d3", edgecolor="black")
ax.set_xlabel("Age"); ax.set_ylabel("Density")
ax.set_title("Histogram: Age")
save("ex2_5f-age-hist.png")

# ---- 2.5g histogram of Age with 5-num breaks ----
print("\n[2.5g] Age histogram with 5-num breaks")
breaks = [16, 40, 46, 54, 96]
fig, ax = plt.subplots(figsize=(6, 4))
ax.hist(ch["Age"], bins=breaks, density=True, color="#5fa8d3", edgecolor="black")
ax.set_xlabel("Age"); ax.set_ylabel("Density")
ax.set_title("Histogram: Age (5-num summary breaks)")
save("ex2_5g-age-hist-5num.png")

# ---- 2.6a2 boxplot of Revenue ----
print("\n[2.6a2] Revenue boxplot")
fig, ax = plt.subplots(figsize=(5, 5))
ax.boxplot(ch["Revenue"], vert=True, patch_artist=True,
           boxprops=dict(facecolor="#5fa8d3"))
ax.set_ylabel("Revenue"); ax.set_title("Boxplot: Revenue (customer_habits)")
save("ex2_6a2-revenue-boxplot.png")

# ---- 2.6a3 Revenue histograms ----
print("\n[2.6a3] Revenue histograms")
fig, axes = plt.subplots(1, 2, figsize=(11, 4))
axes[0].hist(ch["Revenue"], bins=10, density=True, color="#5fa8d3", edgecolor="black")
axes[0].set_title("Histogram: Revenue, breaks=10")
axes[0].set_xlabel("Revenue"); axes[0].set_ylabel("Densities")
axes[1].hist(ch["Revenue"], bins=[0,51,107,179,276,428,654,957,1412,2336,15548],
             density=True, color="#5fa8d3", edgecolor="black")
axes[1].set_title("Histogram: Revenue, custom breaks (deciles)")
axes[1].set_xlabel("Revenue"); axes[1].set_ylabel("Densities")
save("ex2_6a3-revenue-hist-comparison.png")

# ---- 2.7e boxplot of Nr_visits (from given table — 2200 customers) ----
print("\n[2.7e] Nr_visits boxplot")
nr_x = [1,2,3,4,6,8,9,10,12,14,15,16,20,24]
nr_c = [193,272,138,115,92,60,118,228,309,130,113,104,122,206]
data_nr = np.repeat(nr_x, nr_c)
fig, ax = plt.subplots(figsize=(5, 5))
ax.boxplot(data_nr, vert=True, patch_artist=True,
           boxprops=dict(facecolor="#5fa8d3"))
ax.set_ylabel("Nr_visits"); ax.set_title("Boxplot: Nr_visits")
save("ex2_7e-nrvisits-boxplot.png")

# ---- 2.8b/c boxplot of Margin_perc ----
print("\n[2.8b] Margin_perc boxplot")
margin = 100*(ch["Unit_Price"]/ch["Unit_Cost"] - 1)
print(f"  mean={margin.mean():.2f}, sd={margin.std():.2f}, "
      f"q1={margin.quantile(0.25):.2f}, med={margin.median():.2f}, q3={margin.quantile(0.75):.2f}")
fig, ax = plt.subplots(figsize=(5, 5))
ax.boxplot(margin, vert=True, patch_artist=True,
           boxprops=dict(facecolor="#5fa8d3"))
ax.set_ylabel("Margin_perc"); ax.set_title("Boxplot: Margin_perc")
save("ex2_8b-margin-boxplot.png")

print("\nALL DONE.")
