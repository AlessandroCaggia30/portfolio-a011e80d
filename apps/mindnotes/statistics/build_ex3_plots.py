"""
Regenerate plots for Statistics Ex 3 snippets (bivariate statistics).
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

result = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex3/Exe3_Data.Rdata")
pizz = result["pizzerie"].copy()
DS = result["DS"].copy()
ch = result["customer_habits"].copy()
Satisfaction = result["Satisfaction"].copy()
Services = result["Services"].copy()
Campaign = result["Campaign"].copy()
Company = result["Company"].copy()
LoL = result["LoL"].copy()

DS["History_recode"] = pd.Categorical(DS["History"], categories=["None","Low","Medium","High"], ordered=True)
DS["Age_recode"]     = pd.Categorical(DS["Age"],     categories=["Young","Middle","Senior"],   ordered=True)

print("=" * 70); print("EX 3 — bivariate plots"); print("=" * 70)

def save(name):
    p = os.path.join(IMG, name)
    plt.tight_layout()
    plt.savefig(p, dpi=120, bbox_inches="tight")
    plt.close()
    print(f"  saved -> images/{name}")


# ---------- 3.1a boxplot Sales | SmokingArea ----------
print("\n[3.1a] boxplot Sales by SmokingArea")
fig, ax = plt.subplots(figsize=(6, 5))
groups = [pizz.loc[pizz["SmokingArea"]==v, "Sales"] for v in ["No","Yes"]]
bp = ax.boxplot(groups, labels=["No","Yes"], patch_artist=True,
                boxprops=dict(facecolor="#5fa8d3"))
ax.set_xlabel("SmokingArea"); ax.set_ylabel("Sales")
ax.set_title("Boxplot: Sales | SmokingArea")
save("ex3_1a-sales-by-smokingarea.png")

# ---------- 3.1b stacked bar SmokingArea | District ----------
print("\n[3.1b] stacked bar SmokingArea by District")
ct = pd.crosstab(pizz["District"], pizz["SmokingArea"], normalize="index") * 100
print(ct.round(0))
fig, ax = plt.subplots(figsize=(6, 5))
districts = list(ct.index)
no_pct  = ct["No"].values
yes_pct = ct["Yes"].values
ax.bar(districts, no_pct,  color="#5fc0c5", label="No")
ax.bar(districts, yes_pct, bottom=no_pct, color="#e74c3c", label="Yes")
ax.set_ylabel("Percent"); ax.set_title("Stacked bar: SmokingArea | District")
ax.legend(title="SmokingArea")
save("ex3_1b-smoking-by-district.png")

# ---------- 3.1e scatter Price vs Sales ----------
print("\n[3.1e] scatter Price vs Sales + cor")
print(f"  cov = {pizz['Price'].cov(pizz['Sales']):.2f}, cor = {pizz['Price'].corr(pizz['Sales']):.4f}")
fig, ax = plt.subplots(figsize=(6, 5))
ax.scatter(pizz["Price"], pizz["Sales"], color="#5fa8d3", alpha=0.6, edgecolor="black", linewidth=0.4)
m, b = np.polyfit(pizz["Price"], pizz["Sales"], 1)
xs = np.linspace(pizz["Price"].min(), pizz["Price"].max(), 50)
ax.plot(xs, m*xs + b, color="red", linestyle="--", alpha=0.7)
ax.set_xlabel("Price"); ax.set_ylabel("Sales"); ax.set_title("Scatter: Price vs Sales")
save("ex3_1e-price-sales-scatter.png")

# ---------- 3.2a boxplot AmountSpent | History ----------
print("\n[3.2a] boxplot AmountSpent by History")
fig, ax = plt.subplots(figsize=(7, 5))
groups = [DS.loc[DS["History_recode"]==v, "AmountSpent"].dropna() for v in ["None","Low","Medium","High"]]
ax.boxplot(groups, labels=["None","Low","Medium","High"], patch_artist=True,
           boxprops=dict(facecolor="#5fa8d3"))
ax.set_xlabel("History"); ax.set_ylabel("AmountSpent")
ax.set_title("Boxplot: AmountSpent | History")
save("ex3_2a-amountspent-by-history.png")

# ---------- 3.2m boxplot + scatter AmountSpent | Children ----------
print("\n[3.2m] boxplot + scatter AmountSpent vs Children")
print(f"  cor(Children, AmountSpent) = {DS['Children'].corr(DS['AmountSpent']):.4f}")
fig, axes = plt.subplots(1, 2, figsize=(11, 5))
groups = [DS.loc[DS["Children"]==v, "AmountSpent"].dropna() for v in sorted(DS["Children"].unique())]
axes[0].boxplot(groups, labels=[str(v) for v in sorted(DS["Children"].unique())],
                patch_artist=True, boxprops=dict(facecolor="#5fa8d3"))
axes[0].set_xlabel("Children"); axes[0].set_ylabel("AmountSpent")
axes[0].set_title("Boxplot: AmountSpent | Children")
axes[1].scatter(DS["Children"], DS["AmountSpent"], color="#5fa8d3", alpha=0.4, edgecolor="black", linewidth=0.2)
axes[1].set_xlabel("Children"); axes[1].set_ylabel("AmountSpent")
axes[1].set_title("Scatter: AmountSpent vs Children")
save("ex3_2m-amountspent-children.png")

# ---------- 3.3a scatter satisfaction vs age/distance/expenses ----------
print("\n[3.3a] scatter satisfaction vs age, distance, expenses")
for v in ["age", "distance", "expenses"]:
    print(f"  cor({v}, satisfaction) = {Satisfaction[v].corr(Satisfaction['satisfaction']):.4f}")
fig, axes = plt.subplots(1, 3, figsize=(13, 4))
for ax, v in zip(axes, ["age", "distance", "expenses"]):
    ax.scatter(Satisfaction[v], Satisfaction["satisfaction"], color="#5fa8d3", alpha=0.7, edgecolor="black", linewidth=0.3)
    ax.set_xlabel(v); ax.set_ylabel("satisfaction"); ax.set_title(f"Scatter: {v} vs satisfaction")
save("ex3_3a-satisfaction-scatters.png")

# ---------- 3.4a boxplot EXPENSES | TYPE ----------
print("\n[3.4a] boxplot EXPENSES by TYPE")
fig, ax = plt.subplots(figsize=(6, 5))
groups = [Services.loc[Services["TYPE"]==v, "EXPENSES"] for v in Services["TYPE"].unique()]
labels = list(Services["TYPE"].unique())
ax.boxplot(groups, labels=labels, patch_artist=True, boxprops=dict(facecolor="#5fa8d3"))
ax.set_xlabel("TYPE"); ax.set_ylabel("EXPENSES")
ax.set_title("Boxplot: EXPENSES | TYPE")
save("ex3_4a-expenses-by-type.png")

# ---------- 3.6e stacked bar Sex | Country ----------
print("\n[3.6e] stacked bar Sex by Country")
ct = pd.crosstab(ch["Country"], ch["Sex"], normalize="index") * 100
print(ct.round(2))
fig, ax = plt.subplots(figsize=(7, 5))
countries = list(ct.index)
ax.bar(countries, ct["F"].values, color="#e74c3c", label="F")
ax.bar(countries, ct["M"].values, bottom=ct["F"].values, color="#5fc0c5", label="M")
ax.set_ylabel("Percent"); ax.set_title("Stacked bar: Sex | Country")
ax.legend()
plt.xticks(rotation=15)
save("ex3_6f-country-sex-stacked.png")  # kept name for backward compat (used by 3.6e)

# ---------- 3.6g side-by-side bar Country | Sex ----------
print("\n[3.6g] side-by-side bar Country by Sex")
ct2 = pd.crosstab(ch["Sex"], ch["Country"], normalize="index") * 100
print(ct2.round(2))
fig, ax = plt.subplots(figsize=(7, 5))
sexes = list(ct2.index)              # ['F','M']
countries = list(ct2.columns)        # ['France','Germany','UK','US']
x = np.arange(len(sexes))
width = 0.2
colors = ["#e74c3c", "#27ae60", "#5fc0c5", "#8e44ad"]
for i, c in enumerate(countries):
    ax.bar(x + (i - 1.5)*width, ct2[c].values, width, label=c, color=colors[i])
ax.set_xticks(x)
ax.set_xticklabels(sexes)
ax.set_ylabel("Percent"); ax.set_xlabel("Sex")
ax.set_title("Side-by-side bar: Country | Sex")
ax.legend(title="Country")
save("ex3_6g-country-by-sex-side.png")

# ---------- 3.7a1 stacked bar Product_Category | Sex ----------
print("\n[3.7a1] stacked bar Product_Category by Sex")
ct = pd.crosstab(ch["Sex"], ch["Product_Category"], normalize="index") * 100
print(ct.round(2))
fig, ax = plt.subplots(figsize=(7, 5))
sexes = list(ct.index)
prev = np.zeros(len(sexes))
colors = ["#5fc0c5","#e74c3c","#9b59b6"]
for c, col in zip(ct.columns, colors):
    ax.bar(sexes, ct[c].values, bottom=prev, color=col, label=c)
    prev = prev + ct[c].values
ax.set_ylabel("Percent"); ax.set_title("Stacked bar: Product_Category | Sex")
ax.legend(loc="upper right")
save("ex3_7a1-product-by-sex.png")

# ---------- 3.7a3 Product_Category | Sex within each Country (facets) ----------
print("\n[3.7a3] facets: Product by Sex × Country")
countries = sorted(ch["Country"].unique())
fig, axes = plt.subplots(1, len(countries), figsize=(4*len(countries), 5), sharey=True)
for ax, country in zip(axes, countries):
    sub = ch[ch["Country"]==country]
    ct = pd.crosstab(sub["Sex"], sub["Product_Category"], normalize="index") * 100
    sexes = list(ct.index)
    prev = np.zeros(len(sexes))
    for c, col in zip(ct.columns, colors):
        ax.bar(sexes, ct[c].values, bottom=prev, color=col, label=c)
        prev = prev + ct[c].values
    ax.set_title(country); ax.set_ylabel("Percent" if ax is axes[0] else "")
axes[-1].legend(loc="upper right")
save("ex3_7a3-product-sex-by-country.png")

# ---------- 3.7b1 boxplot Age | Country ----------
print("\n[3.7b1] boxplot Age by Country")
countries_b1 = sorted(ch["Country"].unique())
groups_b1 = [ch.loc[ch["Country"]==c, "Age"].dropna() for c in countries_b1]
fig, ax = plt.subplots(figsize=(7, 5))
ax.boxplot(groups_b1, labels=countries_b1, patch_artist=True,
           boxprops=dict(facecolor="#a8d0e6"))
ax.set_xlabel("Country"); ax.set_ylabel("Age")
ax.set_title("Boxplot: Age | Country")
save("ex3_7b1-age-by-country.png")

# ---------- 3.8a boxplot Quantity | Product_Category ----------
print("\n[3.8a] boxplot Quantity by Product_Category")
cats_8a = ["Accessories","Bikes","Clothing"]
groups_8a = [ch.loc[ch["Product_Category"]==c, "Quantity"].dropna() for c in cats_8a]
fig, ax = plt.subplots(figsize=(7, 5))
ax.boxplot(groups_8a, labels=cats_8a, patch_artist=True,
           boxprops=dict(facecolor="#5fa8d3"))
ax.set_xlabel("Product_Category"); ax.set_ylabel("Quantity")
ax.set_title("Boxplot: Quantity | Product_Category")
save("ex3_8a-quantity-by-category.png")

# ---------- 3.9a1 stacked bar LoL tier by class ----------
print("\n[3.9a1] stacked bar LoL tier by class")
LoL["tier_f"]  = pd.Categorical(LoL["tier"],  categories=["F","E","D","C","B","A","S"], ordered=True)
LoL["class_f"] = pd.Categorical(LoL["class"], categories=["Assassin","Fighter","Mage","Marksman","Support","Tank"], ordered=True)
ct = pd.crosstab(LoL["class_f"], LoL["tier_f"], normalize="index") * 100
print(ct.round(1))
fig, ax = plt.subplots(figsize=(8, 5))
classes = list(ct.index)
prev = np.zeros(len(classes))
tier_colors = ["#888","#aaa","#ccc","#fdd","#fbb","#f88","#f44"]
for t, col in zip(ct.columns, tier_colors):
    ax.bar(classes, ct[t].values, bottom=prev, color=col, label=t)
    prev = prev + ct[t].values
ax.set_xlabel("class"); ax.set_ylabel("Percent")
ax.set_title("Stacked bar: tier | class (LoL)")
ax.legend(title="tier", bbox_to_anchor=(1, 1), loc="upper left")
plt.xticks(rotation=15)
save("ex3_9a1-lol-tier-by-class.png")

# ---------- 3.9b KDA | role boxplot ----------
print("\n[3.9b] boxplot KDA by role")
roles = sorted(LoL["role"].unique())
groups = [LoL.loc[LoL["role"]==r, "KDA"] for r in roles]
fig, ax = plt.subplots(figsize=(7, 5))
ax.boxplot(groups, labels=roles, patch_artist=True, boxprops=dict(facecolor="#5fa8d3"))
ax.set_xlabel("role"); ax.set_ylabel("KDA")
ax.set_title("Boxplot: KDA | role")
save("ex3_9b-kda-by-role.png")

# ---------- 3.9c scatter score vs pick_perc by role ----------
print("\n[3.9c] scatter score vs pick_perc colored by role")
fig, ax = plt.subplots(figsize=(7, 5))
for r in roles:
    sub = LoL[LoL["role"]==r]
    ax.scatter(sub["score"], sub["pick_perc"], label=r, alpha=0.7, edgecolor="black", linewidth=0.2)
ax.set_xlabel("score"); ax.set_ylabel("pick_perc")
ax.set_title("Scatter: score vs pick_perc by role")
ax.legend(loc="upper left")
save("ex3_9c-score-pick-by-role.png")

# ---------- 3.10a2 stacked bar Prod | Channel (Company) ----------
print("\n[3.10a2] stacked bar Prod by Channel (Company)")
Company["Prod_f"] = pd.Categorical(Company["Prod"], categories=["L","ML","M","MH","H"], ordered=True)
ct = pd.crosstab(Company["Channel"], Company["Prod_f"], normalize="index") * 100
print(ct.round(2))
fig, ax = plt.subplots(figsize=(8, 5))
channels = list(ct.index)
prev = np.zeros(len(channels))
prod_colors = ["#9ecae1","#6baed6","#3182bd","#08519c","#08306b"]
for p, col in zip(ct.columns, prod_colors):
    ax.bar(channels, ct[p].values, bottom=prev, color=col, label=p)
    prev = prev + ct[p].values
ax.set_xlabel("Channel"); ax.set_ylabel("Percent")
ax.set_title("Stacked bar: Prod | Channel (Company)")
ax.legend(title="Prod", loc="upper right")
save("ex3_10a2-prod-by-channel.png")

# ---------- 3.11b boxplot Revenues (Campaign) ----------
print("\n[3.11b] Revenues boxplot (Campaign)")
fig, ax = plt.subplots(figsize=(5, 5))
ax.boxplot(Campaign["Revenues"], vert=True, patch_artist=True,
           boxprops=dict(facecolor="#5fa8d3"))
ax.set_ylabel("Revenues"); ax.set_title("Boxplot: Revenues (Campaign)")
save("ex3_11b-revenues-boxplot.png")

# ---------- 3.11c scatter Sales vs Costs, Sales vs Revenues ----------
print("\n[3.11c] scatters Sales-Costs, Sales-Revenues")
print(f"  cor(Costs,Sales)    = {Campaign['Costs'].corr(Campaign['Sales']):.4f}")
print(f"  cor(Revenues,Sales) = {Campaign['Revenues'].corr(Campaign['Sales']):.4f}")
fig, axes = plt.subplots(1, 2, figsize=(12, 5))
axes[0].scatter(Campaign["Costs"], Campaign["Sales"], color="#5fa8d3", alpha=0.3, edgecolor="black", linewidth=0.1)
axes[0].set_xlabel("Costs"); axes[0].set_ylabel("Sales"); axes[0].set_title("Scatter: Costs vs Sales")
axes[1].scatter(Campaign["Revenues"], Campaign["Sales"], color="#5fa8d3", alpha=0.3, edgecolor="black", linewidth=0.1)
axes[1].set_xlabel("Revenues"); axes[1].set_ylabel("Sales"); axes[1].set_title("Scatter: Revenues vs Sales")
save("ex3_11c-sales-scatter-pair.png")

print("\nALL DONE.")
