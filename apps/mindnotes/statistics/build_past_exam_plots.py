"""Past-exam plots — one or two key visuals per exam, picked from the
exam's source content. Uses the unified plot_style.py."""
import os, sys
import pyreadr
import numpy as np
import pandas as pd
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from plot_style import apply_style, PALETTE
apply_style()
import matplotlib.pyplot as plt

HERE = os.path.dirname(os.path.abspath(__file__))
IMG = os.path.join(HERE, "images")
os.makedirs(IMG, exist_ok=True)
BASE = "/Users/Alessandro/Repos/my note taking app/statistics/past exams"

def save(name):
    p = os.path.join(IMG, name)
    plt.tight_layout()
    plt.savefig(p, dpi=140, bbox_inches="tight")
    plt.close()
    print(f"  saved -> images/{name}")

def load(exam_folder, rdata_name):
    return pyreadr.read_r(os.path.join(BASE, exam_folder, rdata_name))

# ============== 1st partial 2024 — Company + Campaign ==============
print("\n[p1 2024]")
d1 = load("1st partial 2024", "Data_Exam1.Rdata")["Company"]
d2 = load("1st partial 2024", "Data_Exam2.Rdata")["Campaign"]
# Profitability histogram + boxplot
fig, axes = plt.subplots(1, 2, figsize=(11, 4))
axes[0].hist(d1["Profitability"], bins=20, color=PALETTE["secondary"], edgecolor="black")
axes[0].set_title("Company: Profitability — histogram")
axes[0].set_xlabel("Profitability"); axes[0].set_ylabel("Count")
axes[1].boxplot(d1["Profitability"], vert=True, patch_artist=True,
                boxprops=dict(facecolor=PALETTE["secondary"]))
axes[1].set_title("Company: Profitability — boxplot"); axes[1].set_ylabel("Profitability")
save("exam_p1_2024_profitability.png")
# Revenues boxplot
fig, ax = plt.subplots(figsize=(5, 5))
ax.boxplot(d2["Revenues"], vert=True, patch_artist=True,
           boxprops=dict(facecolor=PALETTE["accent"]))
ax.set_title("Campaign: Revenues — boxplot"); ax.set_ylabel("Revenues")
save("exam_p1_2024_revenues.png")
# Costs vs Sales scatter
fig, ax = plt.subplots(figsize=(6, 5))
ax.scatter(d2["Costs"], d2["Sales"], alpha=0.4, color=PALETTE["secondary"], edgecolor="black", linewidth=0.2)
m, b = np.polyfit(d2["Costs"], d2["Sales"], 1)
xs = np.linspace(d2["Costs"].min(), d2["Costs"].max(), 50)
ax.plot(xs, m*xs+b, color=PALETTE["warn"], linestyle="--")
ax.set_xlabel("Costs"); ax.set_ylabel("Sales"); ax.set_title("Campaign: Costs vs Sales")
save("exam_p1_2024_costs_sales.png")

# ============== 1st partial 2025 — Metrics2 ==============
print("\n[p1 2025]")
m2 = load("1st partial 2025", "Data_PI1_20242210_2.Rdata")["Metrics2"]
# Reach by Engagement bins boxplot
if "Out.Engage" in m2.columns:
    fig, ax = plt.subplots(figsize=(8, 5))
    groups = []; labels = []
    for lvl in sorted(m2["Out.Engage"].dropna().unique(), key=lambda x: str(x)):
        groups.append(m2.loc[m2["Out.Engage"]==lvl, "Reach"].dropna())
        labels.append(str(lvl))
    ax.boxplot(groups, labels=labels, patch_artist=True,
               boxprops=dict(facecolor=PALETTE["secondary"]))
    ax.set_xlabel("Out.Engage"); ax.set_ylabel("Reach")
    ax.set_title("Metrics2: Reach by Engagement bin")
    save("exam_p1_2025_reach_by_engage.png")

# Impressions by Paid (right-skewed) — Ex1.a2
if "Paid" in m2.columns and "Impressions" in m2.columns:
    fig, axes = plt.subplots(1, 2, figsize=(11, 4.5))
    levels = sorted(m2["Paid"].dropna().unique(), key=lambda x: str(x))
    groups = [m2.loc[m2["Paid"]==lvl, "Impressions"].dropna() for lvl in levels]
    # left: side-by-side boxplots (horizontal makes skew obvious)
    axes[0].boxplot(groups, labels=[str(l) for l in levels], patch_artist=True,
                    boxprops=dict(facecolor=PALETTE["secondary"]),
                    medianprops=dict(color=PALETTE["accent"], linewidth=2))
    axes[0].set_xlabel("Paid"); axes[0].set_ylabel("Impressions")
    axes[0].set_title("Impressions by Paid — boxplots (right-skewed)")
    # right: overlaid histograms to show skewness
    for lvl, g, col in zip(levels, groups, [PALETTE["secondary"], PALETTE["accent"]]):
        axes[1].hist(g, bins=30, alpha=0.55, label=f"Paid = {lvl}", color=col, edgecolor="black", linewidth=0.3)
    # mark medians and means
    for lvl, g, col in zip(levels, groups, [PALETTE["secondary"], PALETTE["accent"]]):
        axes[1].axvline(float(g.median()), color=col, linestyle="--", linewidth=1.5)
    axes[1].set_xlabel("Impressions"); axes[1].set_ylabel("Count")
    axes[1].set_title("Impressions distributions — medians (dashed)")
    axes[1].legend()
    save("exam_p1_2025_impressions_by_paid.png")
    # Print the actual medians/means for the snippet text
    for lvl, g in zip(levels, groups):
        print(f"  Paid={lvl}: median={g.median():.0f}  mean={g.mean():.0f}  Q25={g.quantile(.25):.0f}  Q75={g.quantile(.75):.0f}  n={len(g)}")

# ============== 1st partial 2026 — Bidding ==============
print("\n[p1 2026]")
b1 = load("1st partial 2026", "Exam20251014_1.Rdata")["Bidding"]
b2 = load("1st partial 2026", "Exam20251014_2.Rdata")["Bidding"]
# Channel vs Bid boxplots (numeric Bid by Channel)
if "Channel" in b1.columns and "Bid" in b1.columns:
    fig, ax = plt.subplots(figsize=(7, 5))
    chans = sorted([str(c) for c in b1["Channel"].dropna().unique()])
    groups = [b1.loc[b1["Channel"].astype(str)==c, "Bid"].dropna() for c in chans]
    ax.boxplot(groups, labels=chans, patch_artist=True,
               boxprops=dict(facecolor=PALETTE["secondary"]))
    ax.set_xlabel("Channel"); ax.set_ylabel("Bid")
    ax.set_title("Bidding: Bid by Channel")
    save("exam_p1_2026_bid_by_channel.png")
# Bid distribution
fig, ax = plt.subplots(figsize=(5, 5))
ax.boxplot(b1["Bid"].dropna(), vert=True, patch_artist=True,
           boxprops=dict(facecolor=PALETTE["accent"]))
ax.set_title("Bidding: Bid — boxplot"); ax.set_ylabel("Bid")
save("exam_p1_2026_bid_boxplot.png")
# Ex5: Channel x LeadTime — row % (wrong) vs joint % (correct)
if "LeadTime" in b1.columns:
    order_lt = ["Early", "MediumTerm", "LastMinute"]
    chans_ex5 = ["Aggregator", "Agency", "Airline"]
    b1["LeadTime"] = pd.Categorical(b1["LeadTime"], categories=order_lt, ordered=True)
    ct5 = pd.crosstab(b1["Channel"], b1["LeadTime"]).reindex(index=chans_ex5, columns=order_lt)
    row_pct = 100 * ct5.div(ct5.sum(1), 0)
    joint_pct = 100 * ct5 / ct5.values.sum()
    fig, axes = plt.subplots(1, 2, figsize=(12, 4.6))
    x = np.arange(len(chans_ex5)); w = 0.27
    cols = [PALETTE["secondary"], PALETTE["accent"], PALETTE["warn"]]
    for i, lvl in enumerate(order_lt):
        axes[0].bar(x + (i-1)*w, row_pct[lvl].values, w, label=lvl, color=cols[i], edgecolor="black")
        axes[1].bar(x + (i-1)*w, joint_pct[lvl].values, w, label=lvl, color=cols[i], edgecolor="black")
    for ax_, ttl, ylab in [(axes[0], "Row % (y|x): WRONG for the question", "% (row, conditional on Channel)"),
                            (axes[1], "Joint %: CORRECT — answers the question", "% (joint, of all 668 customers)")]:
        ax_.set_xticks(x); ax_.set_xticklabels(chans_ex5)
        ax_.set_ylabel(ylab); ax_.set_title(ttl); ax_.legend(title="LeadTime", fontsize=9)
    for i, ch in enumerate(chans_ex5):
        axes[0].text(x[i], row_pct.loc[ch,"MediumTerm"]+1.5, f"{row_pct.loc[ch,'MediumTerm']:.0f}%", ha="center", fontsize=9, fontweight="bold")
        axes[1].text(x[i], joint_pct.loc[ch,"MediumTerm"]+0.4, f"{joint_pct.loc[ch,'MediumTerm']:.0f}%", ha="center", fontsize=9, fontweight="bold")
    plt.suptitle("Channel x LeadTime — share of MediumTerm customers", fontsize=12, y=1.02)
    save("exam_p1_2026_5_leadtime_channel.png")
# PaidFare vs Bid scatter (Ex2: inverse, non-linear; Pearson r = -0.7947)
if "PaidFare" in b1.columns and "Bid" in b1.columns:
    sub = b1[["PaidFare", "Bid"]].dropna()
    fig, ax = plt.subplots(figsize=(6.5, 5))
    ax.scatter(sub["PaidFare"], sub["Bid"], alpha=0.5,
               color=PALETTE["secondary"], edgecolor="black", linewidth=0.2)
    m, c = np.polyfit(sub["PaidFare"], sub["Bid"], 1)
    xs = np.linspace(sub["PaidFare"].min(), sub["PaidFare"].max(), 100)
    ax.plot(xs, m*xs + c, color=PALETTE["warn"], linestyle="--", label="OLS fit")
    r = sub["PaidFare"].corr(sub["Bid"])
    ax.set_xlabel("PaidFare"); ax.set_ylabel("Bid")
    ax.set_title(f"Bidding: PaidFare vs Bid (Pearson r = {r:.4f})")
    ax.legend()
    save("exam_p1_2026_paidfare_vs_bid.png")

# ============== general 1 2024 — Primary (Read2, Math2) ==============
print("\n[g1 2024]")
pr = load("general 1 2024", "Data_General_202401.Rdata")["Primary"]
fig, ax = plt.subplots(figsize=(6, 5))
ax.scatter(pr["Read2"], pr["Math2"], alpha=0.4, color=PALETTE["secondary"], edgecolor="black", linewidth=0.2)
m, b = np.polyfit(pr["Read2"].dropna(), pr["Math2"].dropna(), 1)
xs = np.linspace(pr["Read2"].min(), pr["Read2"].max(), 50)
ax.plot(xs, m*xs+b, color=PALETTE["warn"], linestyle="--")
ax.set_xlabel("Read2"); ax.set_ylabel("Math2"); ax.set_title("Primary: Read2 vs Math2 (r≈0.77)")
save("exam_g1_2024_read_math.png")

# ============== general 1 2025 — Sleep ==============
print("\n[g1 2025]")
sl = load("general 1 2025", "Data_G_20250108.RData")["Sleep"]
# SleepQuality histogram with quantile lines
fig, axes = plt.subplots(1, 2, figsize=(11, 4))
axes[0].hist(sl["SleepQuality"].dropna(), bins=20, color=PALETTE["secondary"], edgecolor="black")
axes[0].axvline(sl["SleepQuality"].quantile(0.95), color=PALETTE["warn"], linestyle="--",
                label="p95 (top 5%)")
axes[0].legend(); axes[0].set_title("Sleep: SleepQuality — histogram"); axes[0].set_xlabel("SleepQuality")
axes[1].boxplot(sl["SleepQuality"].dropna(), vert=True, patch_artist=True,
                boxprops=dict(facecolor=PALETTE["secondary"]))
axes[1].set_title("Sleep: SleepQuality — boxplot"); axes[1].set_ylabel("SleepQuality")
save("exam_g1_2025_sleepquality.png")

# ============== general 1 2026 — Credit ==============
print("\n[g1 2026]")
cr = load("general 1 2026", "General_20260901.Rdata")["Credit"]
if "PurposeLoan" in cr.columns:
    counts = cr["PurposeLoan"].value_counts()
    fig, ax = plt.subplots(figsize=(6, 5))
    ax.bar(counts.index.astype(str), counts.values, color=PALETTE["accent"], edgecolor="black")
    ax.set_xlabel("PurposeLoan"); ax.set_ylabel("Count")
    ax.set_title("Credit: PurposeLoan composition")
    plt.xticks(rotation=15)
    save("exam_g1_2026_purposeloan.png")

# ============== general 2 2024 — CrimeUS ==============
print("\n[g2 2024]")
cu = load("general 2 2024", "Data_General_202402.Rdata")["CrimeUS"]
# Histogram of some crime variable
crime_cols = [c for c in cu.columns if "rime" in c.lower() or "Crime" in c]
if crime_cols:
    col = crime_cols[0]
    fig, ax = plt.subplots(figsize=(6, 4.5))
    vals = pd.to_numeric(cu[col], errors="coerce").dropna()
    if len(vals):
        ax.hist(vals, bins=30, color=PALETTE["secondary"], edgecolor="black")
        ax.set_xlabel(col); ax.set_ylabel("Count"); ax.set_title(f"CrimeUS: {col} — histogram")
        save("exam_g2_2024_crime.png")

# ============== general 2 2025 — Employee ==============
print("\n[g2 2025]")
em = load("general 2 2025", "Data_G_250129.Rdata")["Employee"]
fig, ax = plt.subplots(figsize=(7, 5))
depts = sorted(em["Department"].dropna().unique())
groups = [em.loc[em["Department"]==d, "Productivity"].dropna() for d in depts]
ax.boxplot(groups, labels=depts, patch_artist=True,
           boxprops=dict(facecolor=PALETTE["secondary"]))
ax.set_xlabel("Department"); ax.set_ylabel("Productivity")
ax.set_title("Employee: Productivity by Department")
plt.xticks(rotation=15)
save("exam_g2_2025_productivity_by_dept.png")

# Ex1 — Salary by Employment_type (Role: Junior / Senior / Manager)
fig, ax = plt.subplots(figsize=(7, 5))
order = ["Junior", "Senior", "Manager"]
groups = [em.loc[em["Role"]==r, "Salary"].dropna() for r in order]
ax.boxplot(groups, labels=order, patch_artist=True,
           boxprops=dict(facecolor=PALETTE["secondary"]),
           medianprops=dict(color=PALETTE["accent"], linewidth=2))
ax.set_xlabel("Employment_type"); ax.set_ylabel("Salary")
ax.set_title("Employee: Salary by Employment_type")
save("exam_g2_2025_salary_by_emptype.png")

# ============== general 2 2026 — retail ==============
print("\n[g2 2026]")
rt = load("general 2 2026", "retail.rdata")["retail"]
# Price scatter
fig, ax = plt.subplots(figsize=(6, 5))
ax.scatter(rt["price1"], rt["price2"], alpha=0.4, color=PALETTE["secondary"], edgecolor="black", linewidth=0.2)
ax.set_xlabel("price1"); ax.set_ylabel("price2"); ax.set_title("retail: price1 vs price2")
save("exam_g2_2026_prices.png")

# ============== july 2024 — Colleges ==============
print("\n[july 2024]")
co = load("july 2024", "Data_General_202406.Rdata")["Colleges"]
# Apps boxplot by Private
if "Private" in co.columns and "Apps" in co.columns:
    fig, ax = plt.subplots(figsize=(6, 5))
    vals_priv = co.loc[co["Private"]=="Yes", "Apps"].dropna()
    vals_pub  = co.loc[co["Private"]=="No",  "Apps"].dropna()
    if len(vals_priv) and len(vals_pub):
        ax.boxplot([vals_pub, vals_priv], labels=["Public","Private"], patch_artist=True,
                   boxprops=dict(facecolor=PALETTE["secondary"]))
        ax.set_ylabel("Apps"); ax.set_title("Colleges: Apps by Private/Public")
        save("exam_july_2024_apps_by_private.png")

# ============== july 2025 — BankClients ==============
print("\n[july 2025]")
bc = load("july 2025", "Exam202507.RData")["BankClients"]
# Savings histogram
fig, axes = plt.subplots(1, 2, figsize=(11, 4))
axes[0].hist(bc["Savings"].dropna(), bins=30, color=PALETTE["secondary"], edgecolor="black")
axes[0].set_title("BankClients: Savings — histogram"); axes[0].set_xlabel("Savings")
axes[1].boxplot(bc["Savings"].dropna(), vert=True, patch_artist=True,
                boxprops=dict(facecolor=PALETTE["accent"]))
axes[1].set_title("BankClients: Savings — boxplot"); axes[1].set_ylabel("Savings")
save("exam_july_2025_savings.png")

# ============== september 2024 — Credit ==============
print("\n[sep 2024]")
cd = load("september 2024", "Credit.RData")["Credit"]
# Income or Score histogram by some category
if "Total_income" in cd.columns:
    fig, ax = plt.subplots(figsize=(6, 4.5))
    vals = pd.to_numeric(cd["Total_income"], errors="coerce").dropna()
    ax.hist(vals, bins=30, color=PALETTE["secondary"], edgecolor="black")
    ax.set_xlabel("Total_income"); ax.set_ylabel("Count")
    ax.set_title("Credit: Income — histogram")
    save("exam_sep_2024_income.png")
elif "Account_length" in cd.columns:
    fig, ax = plt.subplots(figsize=(6, 4.5))
    vals = pd.to_numeric(cd["Account_length"], errors="coerce").dropna()
    ax.hist(vals, bins=30, color=PALETTE["secondary"], edgecolor="black")
    ax.set_xlabel("Account_length"); ax.set_ylabel("Count")
    ax.set_title("Credit: Account_length — histogram")
    save("exam_sep_2024_account_length.png")

# ============== september 2025 — Performance ==============
print("\n[sep 2025]")
pf = load("september 2025", "Exam202509(1).Rdata")["Performance"]
# Ex1.a — scatter VO2.max vs Performance with fit line (r ~ 0.593)
if "VO2.max" in pf.columns and "Performance" in pf.columns:
    import numpy as np
    fig, ax = plt.subplots(figsize=(6, 5))
    x = pd.to_numeric(pf["VO2.max"], errors="coerce")
    y = pd.to_numeric(pf["Performance"], errors="coerce")
    m = x.notna() & y.notna()
    x, y = x[m].values, y[m].values
    ax.scatter(x, y, alpha=0.5, color=PALETTE["secondary"], edgecolor="black", linewidth=0.2)
    # OLS fit line
    b1, b0 = np.polyfit(x, y, 1)
    xs = np.linspace(x.min(), x.max(), 100)
    ax.plot(xs, b0 + b1 * xs, color=PALETTE["primary"], linewidth=1.8)
    r = np.corrcoef(x, y)[0, 1]
    ax.set_xlabel("VO2.max"); ax.set_ylabel("Performance")
    ax.set_title(f"Performance: VO2.max vs Performance (r = {r:.3f})")
    save("exam_sep_2025_vo2max_performance.png")

print("\nALL DONE.")
