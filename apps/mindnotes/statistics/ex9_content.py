"""Ex 9 — Multiple linear regression and diagnostics across many datasets."""

ex9 = {}

ex9["9_1"] = {"title": "Ex 9.1 — Baseball: Major ~ Minor + Age + diagnostic plots",
"content": """**Question.** Regress `Major` league hits on `Minor` and `Age`; interpret coefficients; check assumptions.

---

**Answer.**
```r
mod <- lm(Major ~ Minor + Age, data=Baseball)
summary(mod)
# Predict at Age=25, Minor=22 with prediction interval
predict(mod, newdata=data.frame(Age=25, Minor=22), interval="prediction")
2*confint(mod)[3,]

# Add Years and Age (multicollinearity check)
mod1 <- lm(Major ~ Minor + Years + Age, data=Baseball)
summary(mod1)
cor(Baseball$Age, Baseball$Years)
mod2 <- lm(Major ~ Minor + Years, data=Baseball)
summary(mod2)

# Diagnostics
plot(mod2, which=1)
distr.plot.xy(x=Minor, y=rstandard(mod2), plot.type="scatter", data=Baseball)
distr.plot.xy(x=Years, y=rstandard(mod2), plot.type="scatter", data=Baseball)
```
""", "images": ["statistics/images/ex9_1-baseball.png"]}

ex9["9_2"] = {"title": "Ex 9.2 — ANOVA-style comparison; CI for marginal effects",
"content": """**Question.** Compute t-tests and CIs for coefficients; compare $-50 \\cdot \\hat\\beta_1$.

---

**Answer.**
```r
b.ch1 <- 0.1398; se.1 <- 0.0814
b.ch2 <- 0.0313; se.2 <- 0.0067
tstat1 <- b.ch1/se.1; pval1 <- 2*(1 - pt(abs(tstat1), df=(n-k-1)))
tstat2 <- b.ch2/se.2; pval2 <- 2*(1 - pt(abs(tstat2), df=(n-k-1)))

qt(0.995, df=(n-k-1))
ME <- se.2 * qt(0.995, df=(n-k-1))
c(b.ch2 - ME, b.ch2 + ME)

# Marginal effect: -50 * b1
-50*b.ch1
qt(0.975, df=(n-k-1))
-50*c(b.ch1 - ME, b.ch1 + ME)

# Adjusted R^2
# Adj.R2 = 1 - s_eps^2/var.y
```
""", "images": []}

ex9["9_3"] = {"title": "Ex 9.3 — Competition: Performance ~ Competition (+ Quality)",
"content": """**Question.** Fit a simple regression, then add `Quality` as a second predictor.

---

**Answer.**
```r
mod  <- lm(Performance ~ Competition, data=Competition)
summary(mod)
mod1 <- lm(Performance ~ Competition + Quality, data=Competition)
summary(mod1)
```
""", "images": ["statistics/images/ex9_3-competition.png"]}

ex9["9_4"] = {"title": "Ex 9.4 — superstore: MntMeatProducts ~ IncomeK + Age (+ KidsAtHome)",
"content": """**Question.** Fit multiple regression in `superstore`; add `KidsAtHome` as a categorical predictor; predict at specified values.

---

**Answer.**
```r
mod     <- lm(MntMeatProducts ~ IncomeK + Age, data=superstore)
summary(mod)
moddisc <- lm(MntMeatProducts ~ IncomeK + KidsAtHome, data=superstore)
summary(moddisc)
modter  <- lm(MntMeatProducts ~ IncomeK + Age + KidsAtHome, data=superstore)
predict(modter,
        newdata=data.frame(IncomeK=75, Age=40, KidsAtHome="Yes"),
        interval="confidence")
```
""", "images": ["statistics/images/ex9_4-superstore.png"]}

ex9["9_5"] = {"title": "Ex 9.5 — Restaurants: revenues ~ multiple predictors + diagnostics",
"content": """**Question.** Fit a multiple regression for restaurant revenues; check diagnostics.

---

**Answer.**
```r
mod  <- lm(revenues ~ seats + area + days_open + evening_only, data=restaurants)
summary(mod)
mod2 <- lm(revenues ~ surface + avg_daily, data=restaurants)
summary(mod2)
plot(mod, which=1)
plot(mod, which=3)
distr.plot.x(x=rstandard(mod), plot.type="histogram")
```
""", "images": ["statistics/images/ex9_5-restaurants-multi.png"]}

ex9["9_6"] = {"title": "Ex 9.6 — MBA: MBA.GPA ~ UnderGPA + GMAT + Work",
"content": """**Question.** Fit multiple regression of `MBA.GPA` on its three predictors; test individual coefficients and check residuals.

---

**Answer.**
```r
mod <- lm(MBA.GPA ~ UnderGPA + GMAT + Work, data=MBA.1)
summary(mod)
# t-test for a coefficient (e.g. UnderGPA), example with t=2.996 and df=85
2*(1 - pt(2.996, df=85))
# Hypothesis test for a slope: e.g. H0: beta_GMAT = 0.16
t.stat <- (0.092595 - 0.16) / 0.030909
plot(mod, which=1)
```
""", "images": ["statistics/images/ex9_6-mba1.png"]}

ex9["9_7"] = {"title": "Ex 9.7 — MBA.2: add TypeDegree (categorical) to the model",
"content": """**Question.** Extend the MBA model with the `TypeDegree` factor; interpret coefficients for each level relative to the reference category.

---

**Answer.** R's `lm(... + TypeDegree)` automatically creates indicator variables for each category vs the reference. Each coefficient is the **deviation of that category from the reference**, holding other predictors constant.
""", "images": []}

ex9["9_8"] = {"title": "Ex 9.8 — Lotteries: Amount ~ Education + Age + Children + Income",
"content": """**Question.** Build models for lottery `Amount`. Compare specifications.

---

**Answer.**
```r
mod.Income   <- lm(Amount ~ Income,   data=Lotteries)
mod.Children <- lm(Amount ~ Children, data=Lotteries)
mod.mult     <- lm(Amount ~ Education + Age + Children + Income, data=Lotteries)
summary(mod.mult); summary(mod.Income); summary(mod.Children)
plot(mod.mult, which=1)
distr.plot.x(x=rstandard(mod.mult), plot.type="histogram")
distr.plot.xy(x=Education, y=Amount, plot.type="scatter", data=Lotteries)
distr.plot.xy(x=Income,    y=Amount, plot.type="scatter", data=Lotteries)
```
""", "images": ["statistics/images/ex9_8-lotteries.png"]}

ex9["9_9"] = {"title": "Ex 9.9 — GS: salary ~ grade + gender + course; predictions + diagnostics",
"content": """**Question.** Fit several regressions on `salary` using `grade`, `gender` (sex), and `course`. Predict for specific profiles.

---

**Answer.**
```r
mod.1 <- lm(salary ~ grade + sex, data=GS); summary(mod.1)
predict(mod.1, newdata=data.frame(grade=105, sex="F"))
predict(mod.1, newdata=data.frame(grade=105, sex="F"), interval="confidence")

mod.2 <- lm(salary ~ course, data=GS); summary(mod.2)
mod.3 <- lm(salary ~ grade + course, data=GS); summary(mod.3)

# Predictions at grade=100, course="d"
predict(mod.3, newdata=data.frame(grade=100, course="d"),
        interval="confidence", level=0.99)
predict(mod.3, newdata=data.frame(grade=100, course="d"),
        interval="prediction", level=0.99)
distr.summary.x(salary, data=GS)

# Diagnostics
plot(mod.3, which=1)
distr.plot.xy(x=GS$grade, y=rstandard(mod.1), plot.type="scatter")
distr.plot.xy(x=GS$grade, y=rstandard(mod.3), plot.type="scatter")
```
""", "images": []}

ex9["9_10"] = {"title": "Ex 9.10 — Severance: Weeks ~ Age + Length + Salary",
"content": """**Question.** Predict `Weeks` of severance from `Age`, `Length`, `Salary`. Compare with simpler models. Predict at given values.

---

**Answer.**
```r
mod  <- lm(Weeks ~ Age + Length + Salary, data=Severance); summary(mod)
mod1 <- lm(Weeks ~ Age, data=Severance); summary(mod1)
cor(Severance$Age, Severance$Length)
cor(Severance$Age, Severance$Weeks)
cor(Severance$Weeks, Severance$Length)
mod1 <- lm(Weeks ~ Length, data=Severance); summary(mod1)

newdata <- data.frame(Age=36, Length=10, Salary=32)
predict(mod,  newdata, interval="confidence")
predict(mod,  newdata, interval="prediction")
predict(mod1, newdata, interval="confidence")
predict(mod1, newdata, interval="prediction")

plot(mod1, which=1)
plot(mod1, which=3)
distr.plot.x(x=rstandard(mod1), plot.type="histogram", breaks=10)
```
""", "images": ["statistics/images/ex9_10-severance.png"]}

ex9["9_11"] = {"title": "Ex 9.11 — Absence: Days ~ Wage + PartTime + Union + Shift + GoodRel",
"content": """**Question.** Regression for absence days; compute CIs; predict at specific profile.

---

**Answer.**
```r
mod <- lm(Days ~ Wage + PartTime + Union + Shift + GoodRel, data=Absence); summary(mod)
confint(mod, level=0.99)
sd(Absence$Wage)
sd(Absence$Wage)*(-0.20330)
sd(Absence$Wage)*confint(mod, level=0.95)[2,]

newdata <- data.frame(Wage=20, PartTime=10, Union=68, Shift=1, GoodRel=0)
predict(mod, newdata, interval="predict")
predict(mod, newdata, interval="confidence")

plot(mod, which=1)
distr.plot.x(x=rstandard(mod), plot.type="histogram")
```
""", "images": ["statistics/images/ex9_11-absence.png"]}

ex9["9_12"] = {"title": "Ex 9.12 — Visitors: lagged regression with seasonal indicators",
"content": """**Question.** Regression of `Visitors` on lagged `Visitors_Prev` and seasonal indicators `I1, I2, I3`.

---

**Answer.**
```r
mod  <- lm(Visitors ~ Visitors_Prev + I1 + I2 + I3, data=Visitors); summary(mod)
mod1 <- lm(Visitors ~ Visitors_Prev + I1 + I3 + I4, data=Visitors); summary(mod1)
```

The interpretation of each indicator coefficient is the *seasonal effect on `Visitors`, holding lagged Visitors constant*.
""", "images": ["statistics/images/ex9_12-visitors.png"]}

ex9["9_13"] = {"title": "Ex 9.13 — Loans: Bad ~ Loan + Recommendation (factor)",
"content": """**Question.** Regression on credit-scoring data for the percentage of `Bad` loans.

---

**Answer.**
```r
Rec_ <- factor(Loans$Recommendation, levels=1:3,
               labels=c("No-follow", "Over>10%", "Over<10%"))
mod <- lm(Bad ~ Loan + Rec_, data=Loans); summary(mod)
plot(mod, which=1)
plot(mod, which=3)
distr.plot.x(x=rstandard(mod), plot.type="histogram")
```
""", "images": ["statistics/images/ex9_13-loans.png"]}
