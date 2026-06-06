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

# Ex4-b3 — Normality: histogram of standardized residuals from modB
import statsmodels.formula.api as smf
em_fit = em.dropna(subset=["Productivity","Training_Attended","Satisfaction",
                            "Hours_Worked","Tenure","Remote_Work","Salary","Department"]).copy()
modB = smf.ols("Productivity ~ Training_Attended + Satisfaction + Hours_Worked + "
               "Tenure + Remote_Work + Salary + C(Department)", data=em_fit).fit()
infl = modB.get_influence()
rstd = infl.resid_studentized_internal
fig, ax = plt.subplots(figsize=(6, 4.5))
ax.hist(rstd, bins=20, color=PALETTE["secondary"], edgecolor="black")
ax.set_xlabel("rstandard(modB)"); ax.set_ylabel("Frequency")
ax.set_title("Histogram of rstandard(modB)")
save("exam_g2_2025_modB_resid_hist.png")

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
# Ex2a — Top10 vs Phd scatter with correlation
if {"Top10", "Phd"}.issubset(co.columns):
    x = pd.to_numeric(co["Top10"], errors="coerce")
    y = pd.to_numeric(co["Phd"],   errors="coerce")
    mask = x.notna() & y.notna()
    x = x[mask].to_numpy(); y = y[mask].to_numpy()
    r = float(np.corrcoef(x, y)[0, 1])
    fig, ax = plt.subplots(figsize=(6, 5))
    ax.scatter(x, y, alpha=0.45, color=PALETTE["secondary"],
               edgecolor="black", linewidth=0.2)
    m, b = np.polyfit(x, y, 1)
    xs = np.linspace(x.min(), x.max(), 50)
    ax.plot(xs, m*xs + b, color=PALETTE["warn"], linestyle="--",
            label=f"OLS fit  (r = {r:.4f})")
    ax.set_xlabel("Top10 (% students from top 10% of HS class)")
    ax.set_ylabel("Phd")
    ax.set_title("Colleges: Top10 vs Phd")
    ax.legend(loc="lower right", frameon=False)
    save("exam_july_2024_top10_phd.png")

# ============== july 2025 — BankClients ==============
print("\n[july 2025]")
bc = load("july 2025", "Exam202507.RData")["BankClients"]
# Ex1 — Savings by Branch: side-by-side boxplots + group means (two-sample one-sided t-test, mu_A < mu_B)
if {"Savings", "Branch"}.issubset(bc.columns):
    sv_a = pd.to_numeric(bc.loc[bc["Branch"] == "A", "Savings"], errors="coerce").dropna()
    sv_b = pd.to_numeric(bc.loc[bc["Branch"] == "B", "Savings"], errors="coerce").dropna()
    fig, axes = plt.subplots(1, 2, figsize=(11, 4.5))
    # Left: boxplots A vs B
    bp = axes[0].boxplot([sv_a, sv_b], labels=["Branch A", "Branch B"],
                         patch_artist=True, widths=0.55)
    for patch, col in zip(bp["boxes"], [PALETTE["secondary"], PALETTE["accent"]]):
        patch.set_facecolor(col)
    axes[0].set_title("BankClients: Savings by Branch")
    axes[0].set_ylabel("Savings (€)")
    # Right: group means with SE bars (visualise the difference + pooled SE)
    means = [sv_a.mean(), sv_b.mean()]
    n_a, n_b = len(sv_a), len(sv_b)
    s_a, s_b = sv_a.std(ddof=1), sv_b.std(ddof=1)
    s_p = (((n_a - 1) * s_a ** 2 + (n_b - 1) * s_b ** 2) / (n_a + n_b - 2)) ** 0.5
    se_diff = s_p * (1.0 / n_a + 1.0 / n_b) ** 0.5
    se_means = [s_a / n_a ** 0.5, s_b / n_b ** 0.5]
    axes[1].bar([0, 1], means, yerr=se_means, capsize=8,
                color=[PALETTE["secondary"], PALETTE["accent"]],
                edgecolor="black", width=0.55)
    axes[1].set_xticks([0, 1]); axes[1].set_xticklabels(["Branch A", "Branch B"])
    axes[1].set_ylabel("Mean Savings (€)")
    diff = means[0] - means[1]
    t_stat = diff / se_diff
    axes[1].set_title(
        f"Mean diff (A - B) = {diff:.2f}\nSE(diff) = {se_diff:.2f},  t = {t_stat:.2f}"
    )
    save("exam_july_2025_savings.png")
else:
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

# Ex3d — residuals vs fitted for Score ~ Account_length (homoscedasticity check)
if {"Score", "Account_length"}.issubset(cd.columns):
    y = pd.to_numeric(cd["Score"], errors="coerce")
    x = pd.to_numeric(cd["Account_length"], errors="coerce")
    mask = y.notna() & x.notna()
    y = y[mask].to_numpy(); x = x[mask].to_numpy()
    b1, b0 = np.polyfit(x, y, 1)
    fitted = b0 + b1 * x
    resid = y - fitted
    fig, ax = plt.subplots(figsize=(6.5, 4.8))
    ax.axhline(0, color="gray", linewidth=0.8, linestyle="--")
    ax.scatter(fitted, resid, alpha=0.45, color=PALETTE["secondary"],
               edgecolor="black", linewidth=0.2)
    # Highlight observations #15, #362, #359 (1-indexed as in R)
    highlights = [15, 362, 359]
    for obs in highlights:
        i = obs - 1
        if 0 <= i < len(fitted):
            ax.scatter(fitted[i], resid[i], s=70, facecolor="none",
                       edgecolor=PALETTE["warn"], linewidth=1.6, zorder=5)
            ax.annotate(str(obs), (fitted[i], resid[i]),
                        xytext=(6, 4), textcoords="offset points",
                        fontsize=9, color=PALETTE["warn"])
    order = np.argsort(fitted)
    ax.plot(fitted[order], np.poly1d(np.polyfit(fitted, resid, 1))(fitted[order]),
            color="crimson", linewidth=1.2)
    ax.set_xlabel("Fitted values"); ax.set_ylabel("Residuals")
    ax.set_title("Residuals vs Fitted — Score ~ Account_length")
    save("exam_sep_2024_resid_fitted.png")

# ----- Sep-2024 Ex2a: specific-branch Score histogram (unequal widths) -----
breaks = [0, 200, 300, 600, 1000]
pct    = [0.30, 0.20, 0.30, 0.20]                 # relative frequencies
widths = [breaks[i+1] - breaks[i] for i in range(4)]
dens   = [p / w for p, w in zip(pct, widths)]     # densities = freq / width

fig, ax = plt.subplots(figsize=(7, 4.8))
for i in range(4):
    ax.bar(breaks[i], dens[i], width=widths[i], align="edge",
           color=PALETTE["secondary"], edgecolor="black", linewidth=0.8)
    ax.text(breaks[i] + widths[i] / 2, dens[i] + 0.00005,
            f"{dens[i]:.4f}", ha="center", va="bottom",
            fontsize=9, fontweight="bold", color=PALETTE["primary"])
# Highlight modal class [200, 300)
ax.bar(breaks[1], dens[1], width=widths[1], align="edge",
       color=PALETTE["warn"], edgecolor="black", linewidth=1.2,
       label="Modal class: [200, 300)")
ax.set_xticks(breaks)
ax.set_xlabel("Score"); ax.set_ylabel("Density")
ax.set_title("Sep-2024 Ex2a — Score histogram, specific branch\n(unequal widths -> density = freq / width)")
ax.set_ylim(0, max(dens) * 1.25)
ax.legend(loc="upper right", fontsize=9)
save("exam_sep_2024_2a_hist.png")

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

# Ex3.a — stacked bar: Fr(Effort | Rain)
if "Effort" in pf.columns and "Rain" in pf.columns:
    order = ["Low", "MediumLow", "MediumHigh", "High"]
    ct = pd.crosstab(pf["Effort"], pf["Rain"]).reindex(order)
    prop = ct.div(ct.sum(axis=0), axis=1)  # column proportions: Effort | Rain
    fig, ax = plt.subplots(figsize=(6, 5))
    colors = [PALETTE["secondary"], PALETTE["accent"], PALETTE["warn"], PALETTE["primary"]]
    bottom = np.zeros(len(prop.columns))
    for i, lvl in enumerate(prop.index):
        vals = prop.loc[lvl].values
        ax.bar(prop.columns.astype(str), vals, bottom=bottom,
               color=colors[i % len(colors)], edgecolor="black", linewidth=0.4, label=lvl)
        for j, v in enumerate(vals):
            if v > 0.03:
                ax.text(j, bottom[j] + v / 2, f"{v:.2f}", ha="center", va="center", fontsize=9)
        bottom += vals
    ax.set_xlabel("Rain"); ax.set_ylabel("Fr(Effort | Rain)")
    ax.set_title("Performance: Effort by Rain — stacked proportions")
    ax.legend(title="Effort", loc="center left", bbox_to_anchor=(1.02, 0.5), frameon=False)
    ax.set_ylim(0, 1)
    save("exam_sep_2025_effort_by_rain.png")

print("\nALL DONE.")
