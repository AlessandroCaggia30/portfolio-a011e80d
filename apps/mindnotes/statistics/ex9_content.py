"""Ex 9 — Multiple linear regression and diagnostics across many datasets."""

ex9 = {}

ex9["9_1"] = {"title": "Ex 9.1 — Baseball: Major ~ Minor + Age (prediction, multicollinearity)",
"content": """**Question (dataframe `Baseball`).** Major-league home-run hits in the first two full years (`Major`), modelled on `Minor` (home runs in last full minor-league year) and `Age`. **a)** Fit `Major ~ Minor + Age`; assess fit; interpret coefficients. **b)** Test the intercept. **c)** Predict for a 25-yr-old with 22 minor-league hits (point + interval). **d)** Predict the *difference* in `Major` for two players differing by 2 years of `Age`. **e)** Does the model change if `Years` (years of pro experience) is added? **f)** Are LR assumptions satisfied?

---

**Answer.**
```r
mod <- lm(Major ~ Minor + Age, data=Baseball); summary(mod)
# Interpretation: +1 minor-league HR -> +beta_Minor major hits (cet. par.);
# +1 yr of age -> +beta_Age major hits (cet. par.).
# Intercept t-test: p-value gives the test of H0: beta_0 = 0.

# c) Point + 95% prediction interval at Age=25, Minor=22
predict(mod, newdata=data.frame(Age=25, Minor=22), interval="prediction")

# d) Difference for +2 years of Age: 2 * beta_Age, CI = 2 * confint(beta_Age)
2*confint(mod)[3,]

# e) Add Years (correlated with Age -> multicollinearity)
mod1 <- lm(Major ~ Minor + Years + Age, data=Baseball); summary(mod1)
cor(Baseball$Age, Baseball$Years)        # high -> drop one
mod2 <- lm(Major ~ Minor + Years, data=Baseball); summary(mod2)

# f) Diagnostics: residuals vs fitted + vs each predictor
plot(mod2, which=1)
distr.plot.xy(x=Minor, y=rstandard(mod2), plot.type="scatter", data=Baseball)
distr.plot.xy(x=Years, y=rstandard(mod2), plot.type="scatter", data=Baseball)
```
""", "images": ["statistics/images/ex9_1-baseball.png"]}

ex9["9_2"] = {"title": "Ex 9.2 — Promotional channels: t-tests, CIs and marginal effects",
"content": """**Question.** For 30 promotional campaigns the *Success* indicator is regressed on `Channel1` and `Channel2` amounts. Output: $\\hat\\beta_0=164.01\\ (35.87)$, $\\hat\\beta_1=0.1398\\ (0.0814)$, $\\hat\\beta_2=0.0313\\ (0.0067)$, $s_\\varepsilon = 63.08$ on $df=27$. Sample SDs: $S_{\\text{Success}}=84.02$, $S_{\\text{Ch1}}=144$, $S_{\\text{Ch2}}=1737$. **a)** Global fit + global F-test. **b1)** Significance of coefficients. **b2)** 99% CI for the variation of `Success` for a 1-unit increase in `Channel2`; CI for `Channel1` at 250. **b3)** Reduction of 50 units on `Channel1`: 95% CI for the variation in `Success`. **c)** Reliability of state conclusions? **d)** Predict success when Ch1=100, Ch2=1000. **e)** Compare model with `Channel2` only.

---

**Answer.**
```r
n <- 30; k <- 2; df <- n - k - 1     # 27
b0 <- 164.0100; se0 <- 35.8676
b.ch1 <- 0.1398; se.1 <- 0.0814
b.ch2 <- 0.0313; se.2 <- 0.0067
s.eps <- 63.08; s.y <- 84.02

# b1) t-tests on individual coefficients
tstat1 <- b.ch1/se.1; pval1 <- 2*(1 - pt(abs(tstat1), df=df))
tstat2 <- b.ch2/se.2; pval2 <- 2*(1 - pt(abs(tstat2), df=df))

# b2) 99% CI for beta_2 (1-unit increase in Channel2)
qt(0.995, df=df)                     # critical t
ME2 <- se.2 * qt(0.995, df=df)
c(b.ch2 - ME2, b.ch2 + ME2)
# Channel1 spend = 250 (point + 99% interval estimate for delta-Success)
250*b.ch1
250*c(b.ch1 - se.1*qt(0.995, df=df), b.ch1 + se.1*qt(0.995, df=df))

# b3) 95% CI for the change in Success when Channel1 falls by 50
-50*b.ch1
ME1.95 <- se.1 * qt(0.975, df=df)
-50*c(b.ch1 + ME1.95, b.ch1 - ME1.95)   # sign flipped by -50

# d) Prediction at (Ch1=100, Ch2=1000)
b0 + b.ch1*100 + b.ch2*1000

# a, e) Adjusted R^2 and global F
# Adj.R^2 = 1 - s_eps^2 / var(y)
adjR2 <- 1 - s.eps^2 / s.y^2
# Reduced model (Ch2 only) has s_eps = 65.24 -> compare adj.R^2 / F-test
```
""", "images": []}

ex9["9_3"] = {"title": "Ex 9.3 — Competition: Performance ~ Competition (+ Quality)",
"content": """**Question (dataframe `Competition`).** Clothing-and-accessories retailer with stores in central areas. `Performance` measured by a proper index; `Competition` is the perceived level of competition (proper index); `Quality` is an aggregated indicator of staff/policy quality. **a)** Fit a *simple* linear model `Performance ~ Competition`; estimate it; provide and interpret coefficients; comment on fit. **b)** Add `Quality`; estimate the new model and interpret coefficients. Compare with the previous model; explain the result.

---

**Answer.**
```r
# a) Simple model
mod  <- lm(Performance ~ Competition, data=Competition); summary(mod)
# Sign of beta_Competition: typically negative (stronger competition
# -> lower performance). Significance from t-test / p-value; fit from R^2.

# b) Add Quality
mod1 <- lm(Performance ~ Competition + Quality, data=Competition); summary(mod1)
# Quality controls for an omitted-variable bias: stores facing more
# competition may also invest in higher quality. Adding Quality typically
# (i) shrinks |beta_Competition| toward zero (or even changes its sign),
# (ii) raises R^2 / adjusted R^2.

# Compare nested models (anova or via adjusted R^2)
anova(mod, mod1)

# Diagnostics
plot(mod1, which=1)
distr.plot.x(x=rstandard(mod1), plot.type="histogram")
```
""", "images": ["statistics/images/ex9_3-competition.png"]}

ex9["9_4"] = {"title": "Ex 9.4 — superstore: MntMeatProducts ~ IncomeK + Age (+ KidsAtHome)",
"content": """**Question (dataframe `superstore`).** Customers of a food retailer through different channels. Variables include `Age`, `IncomeK` (k euro), `KidsAtHome` (No/Yes), amounts spent on `MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds`, deals/web/catalog/store purchases. **a)** Estimate `MntMeatProducts ~ IncomeK + Age`; report. **a1)** Is `Age` significant? Test it. **a2)** Interpret the coefficient of `Age`. **b)** Estimate `MntMeatProducts ~ IncomeK + KidsAtHome`; write the equations with/without children. **c)** Estimate `MntMeatProducts ~ IncomeK + Age + KidsAtHome`; is `Age` globally significant ($\\alpha=0.05$)? **c1)** Prediction for a 40-year-old client with `IncomeK=75` and children. **c2)** 95% interval for the average amount spent (clients aged 40, `IncomeK=75`, with children). **c3)** Reliability of predictions?

---

**Answer.**
```r
# a) Continuous predictors
mod <- lm(MntMeatProducts ~ IncomeK + Age, data=superstore); summary(mod)
# a1) H0: beta_Age = 0 vs H1: != 0 -> read t-stat / p-value from summary
# a2) +1 yr of age -> +beta_Age euros spent on meat (cet. par.).

# b) Categorical KidsAtHome (factor: No = reference, Yes = indicator)
moddisc <- lm(MntMeatProducts ~ IncomeK + KidsAtHome, data=superstore)
summary(moddisc)
# Equations:
#   KidsAtHome="No":  MntMeat = b0       + b_Income*IncomeK
#   KidsAtHome="Yes": MntMeat = (b0+b_K) + b_Income*IncomeK

# c) All three predictors
modter <- lm(MntMeatProducts ~ IncomeK + Age + KidsAtHome, data=superstore)
summary(modter)
# Test H0: beta_Age = 0 -> p-value vs alpha=0.05.

# c1) Point prediction at (IncomeK=75, Age=40, KidsAtHome="Yes")
predict(modter, newdata=data.frame(IncomeK=75, Age=40, KidsAtHome="Yes"))
# c2) 95% confidence interval for the mean response
predict(modter, newdata=data.frame(IncomeK=75, Age=40, KidsAtHome="Yes"),
        interval="confidence")
# Same for "No" to compare
predict(modter, newdata=data.frame(IncomeK=75, Age=40, KidsAtHome="No"),
        interval="confidence")

# c3) Diagnostics for reliability
plot(modter, which=1)
distr.plot.x(x=rstandard(modter), plot.type="histogram")
```
""", "images": ["statistics/images/ex9_4-superstore.png"]}

ex9["9_5"] = {"title": "Ex 9.5 — Restaurants: revenues ~ seats + area + days_open + evening_only",
"content": """**Question (dataframe `restaurants`).** Model `revenues` on `seats`, `area` (factor: North, NorthWest, SouthEast, Center), `days_open`, `evening_only` (1/0). **a)** Write the estimated equation; interpret the coefficient of `days_open`. **c)** Is it convenient to open only for dinner? Other things equal? **d)** Does the model say nothing about restaurants in the *Center*? **e)** Are revenues in the *Center* lower, ceteris paribus? **f)** What can you say about restaurants in the *NorthWest*? **g)** Compare with a simpler 2-predictor model `surface + avg_daily`. **h)** Residual diagnostics at point a).

---

**Answer.**
```r
mod <- lm(revenues ~ seats + area + days_open + evening_only,
          data=restaurants); summary(mod)

# Example fitted equation (signs/levels as in summary):
# rev_hat = b0 + b_seats * seats + b_NW * AreaNorthWest
#               + b_SE * AreaSouthEast + b_days * days_open
#               + b_eve * evening_only
# - b_days: +1 extra day open -> +b_days euros in revenues (cet. par.).
# - evening_only=1 vs 0: revenues shift by b_eve (negative -> less convenient).
# - Center is the *reference* level of `area` (its effect is absorbed in b0):
#   so the model DOES describe Center; coefficients on NW/SE are
#   contrasts vs Center. Significance of those tells whether NW/SE
#   differ from Center.

# g) Reduced 2-predictor model
mod2 <- lm(revenues ~ surface + avg_daily, data=restaurants); summary(mod2)
anova(mod2, mod)        # nested F-test (when models are nested)

# h) Residual diagnostics
plot(mod, which=1)      # residuals vs fitted (linearity, homoscedasticity)
plot(mod, which=3)      # scale-location (variance)
distr.plot.x(x=rstandard(mod), plot.type="histogram")  # normality
```
""", "images": ["statistics/images/ex9_5-restaurants-multi.png"]}

ex9["9_6"] = {"title": "Ex 9.6 — MBA.1 / MBA.2: MBA.GPA ~ UnderGPA + GMAT + Work (+ TypeDegree)",
"content": """**Question (dataframes `MBA.1` and `MBA.2`).** Admissions to a 1-year MBA require 3 years of work experience and an undergraduate degree with a B-average. The dean wants to predict performance using `UnderGPA` (undergrad GPA), `GMAT` (test score) and `Work` (years of experience). **a)** Estimate the model on `MBA.1`; report it; assess global significance and goodness of fit; clarify the statistic used. **b)** Focus on `Work`. **b1)** Build the test of $H_0:\\beta_{\\text{Work}}=0$ vs $H_1\\neq 0$, report the output and the R functions used. **b2)** Other things equal, can we conclude that the average variation in MBA GPA for a change of 5 years of `Work` is significantly lower than 0.8? **c)** With the model in (a), draw conclusions on each of the three variables. **d)** Are the assumptions guaranteeing the reliability of the results met? How to check them? Then `MBA.2` adds `TypeDegree` with levels BA, BBA, BEng.BSc, Other. **e)** Include `TypeDegree` to assess differences between Other (reference) and the rest. **f)** Test the statement: "applicants with a BBA degree perform significantly worse than the others". **g)** Predict the MBA performance of a BBA applicant with `UnderGPA=19`, `Work=5`, `GMAT=560`.

---

**Answer.**
```r
mod <- lm(MBA.GPA ~ UnderGPA + GMAT + Work, data=MBA.1); summary(mod)
# Estimated: MBA.GPA_hat = 0.466 + 0.0628*UnderGPA + 0.0113*GMAT + 0.0926*Work
# F = 24.48 (p ~ 0) -> reject H0: all beta = 0 -> globally significant
# R^2 = 0.4635 -> the 3 predictors explain 46.35% of the variance in MBA.GPA

# b1) H0: beta_Work = 0 vs H1 != 0  (n = 89, K = 3, df = n - K - 1 = 85)
# t = b_Work / se(b_Work) = 0.092595 / 0.030909 = 2.996
2*(1 - pt(2.996, df=85))   # p = 0.00359 -> reject H0 at usual alpha (< 0.003)

# b2) H0: 5*beta_Work >= 0.8 (i.e. beta_Work >= 0.16) vs H1: beta_Work < 0.16
t.stat <- (0.092595 - 0.16) / 0.030909   # = -2.180756
pt(t.stat, df=85)          # p = 0.016 -> reject at 5%, not at 1%

# c) GMAT and Work significantly contribute; UnderGPA does NOT (p > 0.05).

# d) Assumptions: E(eps)=0, Var(eps)=sigma^2 constant, Cor(eps_i,eps_j)=0
#    (normality for inference; here n is large so CLT applies).
plot(mod, which=1)         # residuals vs fitted: slight curvature, no severe issue

# e) Add TypeDegree (factor, Other = alphabetical reference)
mod2 <- lm(MBA.GPA ~ UnderGPA + GMAT + Work + TypeDegree, data=MBA.2); summary(mod2)
# MBA.GPA = 0.1898 - 0.006*UnderGPA + 0.0112*GMAT + 0.0982*Work
#           - 0.345*BA + 0.7057*BBA + 0.0348*BEng_BSc
# R^2 up to 0.5566; GMAT and Work still significant, t-stats higher.
# BA holders: 0.345 lower than Other (NOT significant at population level).
# BBA holders: 0.706 higher than Other (significant).
# BEng_BSc:   0.0348 higher than Other (NOT significant).

# f) Re-level so BBA is the reference -> BBA vs each of the others
MBA.2$TypeDegree <- relevel(MBA.2$TypeDegree, ref="BBA")
mod3 <- lm(MBA.GPA ~ UnderGPA + GMAT + Work + TypeDegree, data=MBA.2); summary(mod3)
# MBA.GPA = 0.1898 - 0.006*UnderGPA + 0.0112*GMAT + 0.0982*Work
#           - 1.0507*BA + 0.6709*BEng_BSc - 0.7057*Other
# All three are NEGATIVE: BA highly significant; BEng_BSc & Other significant
# only at 0.009 / 0.004 (depends on chosen alpha).

# g) 95% CI for the mean MBA.GPA at UnderGPA=19, Work=5, GMAT=560, TypeDegree=BBA
predict(mod3,
        newdata=data.frame(UnderGPA=19, GMAT=560, Work=5, TypeDegree="BBA"),
        interval="confidence")
# 95% CI ~ (6.4701, 10.4404)
```
""", "images": ["statistics/images/ex9_6-mba1.png"]}

ex9["9_7"] = {"title": "Ex 9.7 — Performance: Market.Value ~ Assets + Sales + Profits + Cash.Flow + Employees (+ Sector)",
"content": """**Question (dataframe `Performance`).** Large companies. Focus on `Market.Value` (M$) and its drivers `Assets`, `Sales`, `Profits`, `Cash.Flow` (M$) and `Employees` (thousands). **a)** Estimate the linear regression; write its equation. **a1)** Write the expression of the estimated model. **a2)** Is `Assets` significant? State the hypotheses, the realisation of the statistic, the test (with R), interpret the coefficient. **b)** Consider two models for `Market.Value`: first on `Asset, Profits, Sales`; second on `Assets, Employees, Cash.Flow`. How do you explain the obtained results? **c)** Based on the results and the considerations at (b), which model would you use to explain `Market.Value`? Why? What measure could you refer to to support your conclusions, reporting its definition. **d)** Include the factor `Sector` (companies' economic sector). Propose at (c) a model that controls for whether the company is financial (`Sector=Finance`) or energy (`Sector=Energy`); verify based on a properly defined model, explaining the estimated effects. **e)** Using the model estimated at (e), obtain point and 95% interval predictions for the market value of a manufacturing company with `Assets=3000`, `Sales=2500`, `Profits=200`, `Cash.Flow=300`, `Employees=12`. How do predictions change if the model at (d) (not including info on the considered sectors) is used? **f)** Based on the analysis of residuals, would you consider as reliable the model proposed at (d)?

---

**Answer.**
```r
mod <- lm(Market.Value ~ Assets + Sales + Profits + Cash.Flow + Employees,
          data=Performance); summary(mod)
# Coefficients (rounded):
# (Intercept) Assets  Sales  Profits  Employees  Cash.Flow
#  153.3194   0.0298  0.0581 -0.3348  10.4565    3.2356

# a2) H0: beta_Assets = 0 vs H1 != 0
# t = 0.02980 / 0.03222 = 0.925
2*pt(-0.925, df=57)        # p ~ 0.36 -> do NOT reject H0
# An average change of 0.925 M$ in Market.Value per +1 M$ of Assets is NOT significant.

# b) Compare correlations and adjusted R^2 of two specifications
cor(Performance[, c("Assets","Sales","Market.Value","Profits","Cash.Flow","Employees")])
# Sales & Employees strongly correlated; Profits & Cash.Flow strongly correlated.
mod.b1 <- lm(Market.Value ~ Assets + Profits + Sales,          data=Performance)
mod.b2 <- lm(Market.Value ~ Assets + Employees + Cash.Flow,    data=Performance)
summary(mod.b1)$adj.r.squared; summary(mod.b2)$adj.r.squared

# c) Reduced model on Employees + Cash.Flow is ~equivalent to full -> simpler is preferred.
mod.c <- lm(Market.Value ~ Assets + Employees + Cash.Flow, data=Performance); summary(mod.c)
# Assets here is weakly significant -> can be removed (drops Adj-R^2 only marginally).

# d) Add Sector dummies (Finance, Energy; Manufacturing = reference)
mod.d <- lm(Market.Value ~ Employees + Cash.Flow + I(Sector=="Finance")
                                                + I(Sector=="Energy"),
            data=Performance); summary(mod.d)
# I_FinanceTRUE = -223.6694 (se 226.1479, t = -0.989, p = 0.327)
# I_EnergyTRUE  = -106.9894 (se 186.3535, t = -0.574, p = 0.568)
# At sample level Finance companies have on average ~223 M$ lower market value than
# Manufacturing; Energy ~107 M$ lower. Neither is significant -> insignificant at population.

# e) Prediction at Assets=3000, Sales=2500, Profits=200, Cash.Flow=300, Employees=12
predict(mod.d,
        newdata=data.frame(Employees=12, Cash.Flow=300, Sector="Manufacturing"),
        interval="prediction")
# Model without dummies:    1365.168  364.0941  2366.242
# Model with dummies:       1440.240  414.7598  2465.721
# Forecasts differ: in the no-dummy model the intercept is 193.15 regardless of sector;
# with dummies the manufacturing intercept rises to 249.094 (since fin/energy are lower).
# Dummies are NOT significant -> the no-dummy model is more adequate.

# f) Residuals vs fitted
plot(mod.d, which=1)
# Quadratic / non-linear pattern + locally different averages of residuals
# + outliers with very high cash flow / employees -> reliability concerns.
```
""", "images": ["statistics/images/ex9_7-performance.png"]}

ex9["9_8"] = {"title": "Ex 9.8 — Lotteries: Amount ~ Education + Age + Children + Income",
"content": """**Question (dataframe `Lotteries`).** Lotteries are sometimes considered a tax on the poor / uneducated. Is it of interest to test the beliefs: **1)** less educated people spend more than more educated; **2)** older people spend more than younger; **3)** people with more children spend more than people with less children; **4)** poorer people spend more than richer. Random sample of 100 adults; `Amount` = % of total household income (`Income`) spent on lottery tickets; other variables are `Education` (years), `Age`, `Children`. **a)** Compare multiple vs four simple linear regressions; how do the interpretations of the coefficients differ when moving from simple to multiple? **b)** Based on (a), what are your considerations about the considered beliefs? **c)** Are the conclusions reliable? Explain clearly what are the problems possibly affecting your conclusions and what type of consequences they could have. **(\\*\\*)** Based on the tools used to answer the question, can you make a guess about the population of the obtained results?

---

**Answer.**
```r
# a) Multiple model + four simple models
mod.mult     <- lm(Amount ~ Age + Education + Income + Children, data=Lotteries)
mod.Age      <- lm(Amount ~ Age,       data=Lotteries)
mod.Edu      <- lm(Amount ~ Education, data=Lotteries)
mod.Income   <- lm(Amount ~ Income,    data=Lotteries)
mod.Children <- lm(Amount ~ Children,  data=Lotteries)
summary(mod.mult)
# Multiple: Amount = 12.3504 + 0.02689*Age - 0.4261*Education - 0.07512*Income + 0.12884*Children
# Simple:   Amount = 3.49641 + 0.05377*Age
#           Amount = 14.71067 - 0.69517*Education
#           Amount = 10.50393 - 0.14209*Income
#           Amount = 5.88393 - 0.03233*Children
# Multiple-model coefficients reflect *unique* effect of each X holding others fixed.
# Simple-model coefficients do NOT control for the other variables.
# Age and Children are NOT significant in the multiple model at any usual alpha;
# in the simple model on Age the p-value is 0.09 (10% rejection only).
# Adjusted R^2 favours the multiple model -> superior to every simple model.

# b) Beliefs assessment
# Simple models -> Edu and Income coefficients have the SAME signs as the multiple model
#   AND are significant -> beliefs 1 and 4 supported.
# Belief 2 (Age) and belief 3 (Children) -> NOT supported (insignificant).

# c) Reliability: check assumptions of the linear model
plot(mod.mult, which=1)    # residuals vs fitted: clear violation of homoscedasticity!
distr.plot.x(x=rstandard(mod.mult), plot.type="histogram")
# Variance of residuals grows with Amount -> SE of beta_hats and t-tests are NOT trustable.
# Heteroscedasticity from heterogeneity in spending behaviour at high Education / Income.
distr.plot.xy(x=Education, y=Amount, plot.type="scatter", data=Lotteries)
distr.plot.xy(x=Income,    y=Amount, plot.type="scatter", data=Lotteries)

# (**) Conclusions on the *population* depend on inference (t-tests), which here
# is compromised by the heteroscedasticity.
```
""", "images": ["statistics/images/ex9_8-lotteries.png"]}

ex9["9_9"] = {"title": "Ex 9.9 — GS: salary ~ grade + sex + course; predictions + diagnostics",
"content": """**Question (dataframe `GS`).** Italian employees with similar positions; `salary` (annual at 5 yrs from graduation, k euros), `grade` (graduation grade), `sex` (F/M on id document), `course` (degree course, 4 categories: a, b, c, d). **a)** Estimate `salary ~ grade + sex`. Based on the model, can you predict a sex-based effect of the grade on the salary? **a1)** Based on the estimated model, can you predict sex-based differences in the average salary for graduates with the same `grade`? **a2)** What is the standard error of the model, what does it summarize and how is it calculated? **a3)** Predict the average salary for females with `grade=105`. The prediction of the salary of one specific female with `grade=105`. Do you think such predictions are reliable? Why? If not, what tools would you use instead? **b)** Estimate `salary ~ course`. What indications based on the model? **c)** Estimate `salary ~ course + grade`. For which course the highest average salary is predicted, for a given graduation grade? **c1)** Build the 99% interval for the average salary of graduates who attended course d and have a `grade=100`, and interpret it. **c2)** Will the interval at the previous point change if you are interested to predict the salary for a specific graduate with the described characteristics? If yes, in what respect? Verify numerically your answer. **d)** Based on the results obtained above, to explain/predict salary, would you refer to the model based on `grade + sex` or on `grade + course`? Explain why. **e)** Does the analysis of residuals emphasise any violations of the assumptions? Plot the standardised residuals against `grade`. Considerations?

---

**Answer.**
```r
# a) salary ~ grade + sex   (n = 96, K = 2, df = 93)
mod.1 <- lm(salary ~ grade + sex, data=GS); summary(mod.1)
# salary_hat = b0 + 0.9044*grade - 8.3332*sexM   (intercept depends on F/M reference)
# The grade effect is the SAME for both sexes (additive model, no interaction).
# a1) At equal grade, women earn on average 8.33 lower than men (p = 0.003 -> significant
# at any alpha > 0.003).

# a2) Standard error of the model = sqrt( SSE / (n - K - 1) ) = 13.5
# It is the estimate of the std deviation of the residuals, computed from the SSE.
n <- 96; K <- 2
# s_eps = sqrt(sum(residuals(mod.1)^2) / (n - K - 1))

# a3) Prediction for the average salary for F, grade=105
predict(mod.1, newdata=data.frame(grade=105, sex="F"), interval="confidence")
# salaring_g_hat = -37.9397 + 0.9044*105 - 8.3332 (= 48.69 k euros)
# 95% CI for the mean: (44.0406, 53.3429)
# 95% prediction interval (for a SINGLE person): (21.4902, 75.8934)
# R^2 ~ 0.3 -> non-negligible dispersion -> prefer interval predictions over point.
predict(mod.1, newdata=data.frame(grade=105, sex="F"), interval="prediction")

# b) salary ~ course   (course = a is the reference)
mod.2 <- lm(salary ~ course, data=GS); summary(mod.2)
# course b vs a: +1.547  (p = 0.7  -> NOT significant)
# course c vs a: +11.522 (p = 0.011 -> significant at 1.07%)
# course d vs a: +14.988 (p ~ 4e-4 -> highly significant)

# c) salary ~ course + grade
mod.3 <- lm(salary ~ grade + course, data=GS); summary(mod.3)
# Highest average salary for course d (largest course coefficient).

# c1) 99% confidence interval at grade=100, course="d"
predict(mod.3, newdata=data.frame(grade=100, course="d"),
        interval="confidence", level=0.99)        # (48.17 ; 63.31)

# c2) 99% prediction interval for a SPECIFIC graduate
predict(mod.3, newdata=data.frame(grade=100, course="d"),
        interval="prediction", level=0.99)        # (19.02 ; 92.45)
# Wider, because adds uncertainty around the average. Range too wide to be informative
# as salaries in the sample vary between 18 and 86.
distr.summary.x(salary, data=GS)

# d) Compare via Adjusted R^2 (different #predictors)
# mod.1 (grade + sex):    Adj-R^2 = 0.288
# mod.3 (grade + course): Adj-R^2 = 0.2713
# Both low and aligned -> neither very useful, slight preference for mod.1.

# e) Diagnostics
plot(mod.3, which=1)                  # no extreme violation, increasing dispersion at top
distr.plot.xy(x=GS$grade, y=rstandard(mod.1), plot.type="scatter")
distr.plot.xy(x=GS$grade, y=rstandard(mod.3), plot.type="scatter")
# Curvilinear trend + increased dispersion at high grade -> model NOT fully reliable.
```
""", "images": ["statistics/images/ex9_9-gs.png"]}

ex9["9_10"] = {"title": "Ex 9.10 — Severance: Weeks ~ Age + Length + Salary",
"content": """**Question (dataframe `Severance`).** After a restructuring a company offers severance packages to terminated employees. A terminated employee wants to verify the relation between `Weeks` (weeks of severance), `Age` (yrs), `Length` (yrs of employment), `Salary` (k$). **a)** Estimate `Weeks ~ Age + Length + Salary`. Is the statement "older and high-salary employees are penalised" (other things equal) confirmed? **b)** Based on (a), is it correct to claim that `Age` does NOT contribute significantly to a model that already includes `Length` and `Salary`? **c)** Compare the model based only on `Age` with the model at (a). Is one preferable? Refer to a measure used to compare regression models with a different number of explanatory variables, and report its definition. **d)** Would you consider the model based only on `Length` rather than the model estimated at (a)? Why? **e)** The employee is 36 years old, worked 10 years, currently earns 32 k$/yr, and the package offered is 5 weeks' pay. Point prediction of weeks of severance using the models at (a) and (d). Do you think the predictions are reliable? Why? **f)** Could the employee claim that their package is not aligned with what is applied based on the obtained results? **g)** Are there reasons to suspect that the results obtained are unreliable because some of the model assumptions are not met?

---

**Answer.**
```r
# a) Full model
mod <- lm(Weeks ~ Age + Length + Salary, data=Severance); summary(mod)
# The statement "older/high-salary employees are penalised" would require beta_Age < 0
# AND beta_Salary < 0 to be significant. At the sample level Age and Salary have
# negative point estimates, but the p-values of t-tests are higher than any standard
# significance level -> can NOT conclude penalisation at the population.

# b) Compare with the model adding only Age
mod.b <- lm(Weeks ~ Age, data=Severance); summary(mod.b)
# Coefficient of Age = 6.253 with p ~ 0 -> reject H0 that beta_Age = 0!
# Explanation: Age is highly correlated with Length (0.80796) and with Weeks (0.67).
cor(Severance$Age, Severance$Length)         # 0.80796
cor(Severance$Age, Severance$Weeks)          # 0.67
cor(Severance$Weeks, Severance$Length)       # 0.83
# Length has a stronger relation with Weeks -> incremental Age contribution is small.

# c) Adjusted R^2 (definition):
#    AdjR^2 = 1 - SSE/(n-K-1) / ( SST/(n-1) )
# SST = total sum of squares of Weeks (deviations of Weeks from their mean)
# SSE = sum of squared residuals (squared diffs of obs Weeks vs predicted)
# K = number of explanatory variables.
# mod (Age + Length + Salary): Adj-R^2 = 0.6825
# mod.b (Age only):            Adj-R^2 = 0.4374
# -> Full model preferred.

# d) Compare full vs Length-only model
mod.d <- lm(Weeks ~ Length, data=Severance); summary(mod.d)
# The increase in R^2 of (a) over (d) is mostly due to extra predictors, NOT to a
# real improvement -> mod.d is preferable (parsimony).

# e) Point and 95% CI / prediction intervals at Age=36, Length=10, Salary=32
newdata <- data.frame(Age=36, Length=10, Salary=32)
predict(mod,   newdata, interval="confidence")
predict(mod,   newdata, interval="prediction")
predict(mod.d, newdata, interval="confidence")
predict(mod.d, newdata, interval="prediction")
#                Point   95% CI mean     95% prediction interval
# Model (a)    9.56709  (8.86 ; 10.27)  (5.64 ; 13.50)
# Model (d)    9.36413  (8.79 ; 9.94)   (5.47 ; 13.26)
# Point predictions NOT recommendable (do not account for dispersion).

# f) Offered 5 weeks < lower bound of either 95% prediction interval -> NOT aligned.
# Caveat: no info on previously terminated employees who refused to disclose -> the
# sample might be non-representative; cautious conclusions only.

# g) Residual diagnostics
plot(mod.d, which=1)                                       # no evident violations
plot(mod.d, which=3)
distr.plot.x(x=rstandard(mod.d), plot.type="histogram", breaks=10)
# A few cases with particularly negative residuals -> employees offered a low number of
# weeks given their Length of employment.
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
predict(mod, newdata, interval="prediction")
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
Visitors$I4 <- -1 - Visitors$I2 - Visitors$I3
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
               labels=c("No-follow", "Over>10", "Over<=10"))
mod <- lm(Bad ~ Loan + Rec_, data=Loans); summary(mod)
plot(mod, which=1)
plot(mod, which=3)
distr.plot.x(x=rstandard(mod), plot.type="histogram")
```
""", "images": ["statistics/images/ex9_13-loans.png"]}
