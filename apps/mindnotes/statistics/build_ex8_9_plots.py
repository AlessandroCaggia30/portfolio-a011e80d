"""Regenerate regression diagnostic plots for Ex8 (simple regression) and Ex9 (multiple regression)."""
import os
import pyreadr
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression

HERE = os.path.dirname(os.path.abspath(__file__))
IMG = os.path.join(HERE, "images")
os.makedirs(IMG, exist_ok=True)

ex8 = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex8/Exe8_Data.RData")
ex9 = pyreadr.read_r("/Users/Alessandro/Repos/my note taking app/statistics/ex9/Exe9_Data.RData")

def save(name):
    p = os.path.join(IMG, name)
    plt.tight_layout()
    plt.savefig(p, dpi=120, bbox_inches="tight")
    plt.close()
    print(f"  saved -> images/{name}")

def fit_and_plot(df, x_cols, y_col, title, fname, scatter_x=None):
    """Fit y ~ x_cols (multiple cols possible), produce 1x2 figure: scatter (if 1 predictor) + residuals."""
    df_clean = df[[y_col] + x_cols].dropna()
    X = df_clean[x_cols].values
    y = df_clean[y_col].values
    mod = LinearRegression().fit(X, y)
    yhat = mod.predict(X)
    resid = y - yhat
    n_pred = len(x_cols)
    if n_pred == 1:
        fig, axes = plt.subplots(1, 2, figsize=(11, 4))
        ax = axes[0]
        ax.scatter(X[:,0], y, color="#5fa8d3", alpha=0.4, edgecolor="black", linewidth=0.2)
        xs = np.linspace(X[:,0].min(), X[:,0].max(), 50).reshape(-1,1)
        ax.plot(xs, mod.predict(xs), color="red", linestyle="--")
        ax.set_xlabel(x_cols[0]); ax.set_ylabel(y_col); ax.set_title(f"Scatter + fit: {y_col} ~ {x_cols[0]}")
        axR = axes[1]
    else:
        fig, axR = plt.subplots(figsize=(6, 4))
    axR.scatter(yhat, resid, color="#5fa8d3", alpha=0.4, edgecolor="black", linewidth=0.2)
    axR.axhline(0, color="red", linestyle="--", alpha=0.7)
    axR.set_xlabel("Fitted"); axR.set_ylabel("Residual")
    axR.set_title(f"Residuals vs fitted: {title}")
    save(fname)
    print(f"  R² = {mod.score(X,y):.4f}, coef = {mod.coef_}, intercept = {mod.intercept_:.4f}")

print("="*60); print("EX 8 — simple regression diagnostics"); print("="*60)

# Ex 8.1: TeleDebt: Debt ~ Television
TeleDebt = ex8["TeleDebt"].copy()
fit_and_plot(TeleDebt, ["Television"], "Debt", "Debt ~ Television (TeleDebt)", "ex8_1-debt-television.png")

# Ex 8.2: DS: AmountSpent ~ Salary, then ~ Catalogs
DS = ex8["DS"].copy()
fit_and_plot(DS, ["Salary"],   "AmountSpent", "AmountSpent ~ Salary (DS)",   "ex8_2a-amountspent-salary.png")
fit_and_plot(DS, ["Catalogs"], "AmountSpent", "AmountSpent ~ Catalogs (DS)", "ex8_2b-amountspent-catalogs.png")

# Ex 8.3: NewHired: Weeks ~ Age
NewHired = ex8["NewHired"].copy()
fit_and_plot(NewHired, ["Age"], "Weeks", "Weeks ~ Age (NewHired)", "ex8_3-weeks-age.png")

# Ex 8.4: restaurants: revenues ~ surface
restaurants = ex8["restaurants"].copy()
fit_and_plot(restaurants, ["surface"], "revenues", "revenues ~ surface (restaurants)", "ex8_4-revenues-surface.png")

print("="*60); print("EX 9 — multiple regression diagnostics"); print("="*60)

# Ex 9.1: Baseball: Major ~ Minor + Age
Baseball = ex9["Baseball"].copy()
fit_and_plot(Baseball, ["Minor", "Age"], "Major", "Baseball: Major ~ Minor + Age", "ex9_1-baseball.png")

# Ex 9.3: Competition: Performance ~ Competition + Quality
Competition = ex9["Competition"].copy()
fit_and_plot(Competition, ["Competition", "Quality"], "Performance",
             "Competition: Performance ~ Competition + Quality", "ex9_3-competition.png")

# Ex 9.4: superstore: MntMeatProducts ~ IncomeK + Age
superstore = ex9["superstore"].copy()
fit_and_plot(superstore, ["IncomeK", "Age"], "MntMeatProducts",
             "superstore: MntMeatProducts ~ IncomeK + Age", "ex9_4-superstore.png")

# Ex 9.5: restaurants: revenues ~ seats + days_open
rest = ex9["restaurants"].copy()
fit_and_plot(rest, ["seats", "days_open"], "revenues",
             "restaurants: revenues ~ seats + days_open", "ex9_5-restaurants-multi.png")

# Ex 9.6: MBA.1: MBA.GPA ~ UnderGPA + GMAT + Work
MBA1 = ex9["MBA.1"].copy()
fit_and_plot(MBA1, ["UnderGPA", "GMAT", "Work"], "MBA.GPA",
             "MBA.1: MBA.GPA ~ UnderGPA + GMAT + Work", "ex9_6-mba1.png")

# Ex 9.7: Performance: Market.Value ~ Assets + Sales + Profits + Cash.Flow + Employees
if "Performance" in ex9:
    Performance = ex9["Performance"].copy()
    perf_clean = Performance.dropna(subset=["Market.Value", "Assets", "Sales", "Profits", "Cash.Flow", "Employees"])
    fit_and_plot(perf_clean, ["Assets", "Sales", "Profits", "Cash.Flow", "Employees"], "Market.Value",
                 "Performance: Market.Value ~ predictors", "ex9_7-performance.png")

# Ex 9.8: Lotteries: Amount ~ Education + Age + Children + Income
Lotteries = ex9["Lotteries"].copy()
# Drop rows with NaN in any of the columns used
Lotteries_clean = Lotteries.dropna(subset=["Amount", "Education", "Age", "Children", "Income"])
# Education might be categorical; encode if so
if Lotteries_clean["Education"].dtype == "object":
    Lotteries_clean = pd.get_dummies(Lotteries_clean, columns=["Education"], drop_first=True)
    edu_cols = [c for c in Lotteries_clean.columns if c.startswith("Education_")]
else:
    edu_cols = ["Education"]
cols = ["Age", "Children", "Income"] + edu_cols
fit_and_plot(Lotteries_clean, cols, "Amount",
             "Lotteries: Amount ~ predictors", "ex9_8-lotteries.png")

# Ex 9.9: GS: salary ~ grade + course (course is categorical)
GS = ex9["GS"].copy()
GS_clean = GS.dropna()
if str(GS_clean["course"].dtype) in ("object", "category"):
    GS_clean["course"] = GS_clean["course"].astype(str)
    GS_dum = pd.get_dummies(GS_clean, columns=["course"], drop_first=True)
    course_cols = [c for c in GS_dum.columns if c.startswith("course_")]
    cols = ["grade"] + course_cols
    fit_and_plot(GS_dum, cols, "salary", "GS: salary ~ grade + course", "ex9_9-gs.png")

# Ex 9.10: Severance: Weeks ~ Age + Length + Salary
Severance = ex9["Severance"].copy()
fit_and_plot(Severance, ["Age", "Length", "Salary"], "Weeks",
             "Severance: Weeks ~ Age+Length+Salary", "ex9_10-severance.png")

# Ex 9.11: Absence: Days ~ Wage + PartTime + Union + Shift + GoodRel
Absence = ex9["Absence"].copy()
fit_and_plot(Absence, ["Wage", "PartTime", "Union", "Shift", "GoodRel"], "Days",
             "Absence: Days ~ predictors", "ex9_11-absence.png")

# Ex 9.12: Visitors: Visitors ~ Visitors_Prev + I1 + I2 + I3
Visitors = ex9["Visitors"].copy()
fit_and_plot(Visitors, ["Visitors_Prev", "I1", "I2", "I3"], "Visitors",
             "Visitors: Visitors ~ lag + I1..I3", "ex9_12-visitors.png")

# Ex 9.13: Loans: Bad ~ Loan + Recommendation
Loans = ex9["Loans"].copy()
if "Recommendation" in Loans.columns and Loans["Recommendation"].dtype != "object":
    Loans["Recommendation"] = Loans["Recommendation"].astype(str)
Loans_dum = pd.get_dummies(Loans, columns=["Recommendation"], drop_first=True)
rec_cols = [c for c in Loans_dum.columns if c.startswith("Recommendation_")]
fit_and_plot(Loans_dum, ["Loan"] + rec_cols, "Bad",
             "Loans: Bad ~ Loan + Recommendation", "ex9_13-loans.png")

print("\nALL DONE.")
