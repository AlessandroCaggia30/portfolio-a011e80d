"""Ex 8 — Simple linear regression."""

ex8 = {}

ex8["8_1a"] = {"title": "Ex 8.1a — TeleDebt: regression of Debt on Television and significance test",
"content": """<span class="exam-question-text">**EXERCISE 8.1.** (dataframe `TeleDebt`) A sociologist theorized that people who watch television frequently are exposed to many commercials, which in turn leads them to buy more, finally resulting in increasing debt. To test this belief, a sample of $n=430$ families was drawn. For each, the total debt (`Debt`) and the number of hours the television is turned on in a week (`Television`) were recorded.

**a)** Estimate the regression model of `Debt` on `Television`. Report the estimated regression coefficients and interpret them. Assess whether the variable `Television` is significant. Explain exactly what it means, what hypotheses are referred to, what is the test statistic that the test is based upon and what is the procedure followed to run the test.</span>

---

**Answer.**
```r
mod <- lm(Debt ~ Television, data=TeleDebt)
summary(mod)
distr.summary.x(Television, stats="summary", data=TeleDebt)
```

The estimated model is

$$\\widehat{\\text{Debt}} = 1479.262 + 99.7471 \\cdot \\text{Television}.$$

- $\\hat\\beta_0 = 1479.262$: average debt for a family that never watches TV.
- $\\hat\\beta_1 = 99.7471$: for one extra hour of TV per week, the average debt increases by about \\$99.75.

To test significance of `Television` we test $H_0:\\beta_1=0$ vs $H_1:\\beta_1\\neq 0$ with statistic $t=\\hat\\beta_1/\\text{se}(\\hat\\beta_1)$, $t\\sim t_{n-2}=t_{428}$ under $H_0$. The reported $t\\approx 12.57$ gives $p$-value $\\approx 0$, so we **reject $H_0$**: `Television` is a strongly significant predictor of `Debt`.
""", "images": ["statistics/images/ex8_1-debt-television.png"]}

ex8["8_1b"] = {"title": "Ex 8.1b — TeleDebt: goodness-of-fit index ($R^2$)",
"content": """<span class="exam-question-text">**b)** How would you assess the quality of the model? Write down the expression of the index you would consider and interpret its value, also describing the rationale behind it.</span>

---

**Answer.** The goodness-of-fit index is the **coefficient of determination**

$$R^2 = \\frac{SSR}{SST} = 1 - \\frac{SSE}{SST},\\qquad SST=\\sum_{i=1}^{n}(y_i-\\bar y)^2,\\; SSR=\\sum(\\hat y_i-\\bar y)^2,\\; SSE=\\sum(y_i-\\hat y_i)^2.$$

It measures the share of variability of $y$ that is **explained** by the linear regression. From the printout $R^2 = 0.7784$: about $77.84\\%$ of the variation in `Debt` is explained by `Television` — a high value, the model fits well.
""", "images": []}

ex8["8_1c"] = {"title": "Ex 8.1c — TeleDebt: 99% prediction interval at Television=33",
"content": """<span class="exam-question-text">**c)** Build the 99% interval for the debt predicted for a family whose television was turned on for 33 hours in a week; interpret the obtained interval and list the quantities determining its width.

**d)** At the same significance level, do you expect a wider or a narrower interval for the prediction of the average debt of families whose television was turned on for 33 hours in a week? Explain your answer and verify it numerically.

**e)** Can you conclude based on the obtained results that a decrease in the number of hours the television is turned on will lead to a decrease of the debt, at least on average?</span>

---

**Answer.**
```r
predict(mod, newdata=data.frame(Television=33),
        interval="prediction", level=0.99)
predict(mod, newdata=data.frame(Television=33),
        interval="confidence", level=0.99)
```

**c)** Prediction interval at $x_g=33$: $\\hat y_g \\pm t_{0.995,n-2}\\cdot s_\\epsilon\\sqrt{1+\\tfrac{1}{n}+\\tfrac{(x_g-\\bar x)^2}{(n-1)s^2_x}}$. The width depends on $s_\\epsilon$, $n$, $s^2_x$ and the distance $(x_g-\\bar x)^2$.

**d)** For the **average** debt (confidence interval for $E(Y\\mid X=33)$) we drop the "1+" inside the root, so the CI is **narrower** than the PI — the CI captures only the uncertainty of the estimated mean, the PI also the irreducible noise of a new observation.

**e)** **No.** The model only shows an association in the sample; we cannot infer that intervening on TV hours would *cause* a decrease in debt — this is an observational study, possible confounders (income, household size, …) and reverse causality are not controlled for.
""", "images": []}

ex8["8_2a"] = {"title": "Ex 8.2a — DS: OLS criterion and AmountSpent on Salary",
"content": """<span class="exam-question-text">**EXERCISE 8.2.** (dataframe `DS`)

**a)** What is the optimality criterion used to determine the estimates of the coefficients in a regression model?

**b)** Consider the model explaining `AmountSpent` based on `Salary`.

**b1)** Write the estimated equation of the regression line.

**b2)** Define what is meant by "homoscedasticity of errors" in a regression model, and assess whether the assumption of homoscedasticity is met for the considered model, explaining your reasoning and reporting the tools supporting your conclusions.</span>

---

**Answer.**

**a)** OLS — **ordinary least squares**: $(\\hat\\beta_0,\\hat\\beta_1)=\\arg\\min_{\\beta_0,\\beta_1}\\sum_{i=1}^n (y_i-\\beta_0-\\beta_1 x_i)^2$.

**b1)**
```r
mod <- lm(AmountSpent ~ Salary, data=DS)
summary(mod)
```
$$\\widehat{\\text{AmountSpent}} = \\hat\\beta_0 + \\hat\\beta_1\\cdot\\text{Salary}.$$

**b2) Homoscedasticity** means $\\Var(\\epsilon_i\\mid X)=\\sigma^2$ — constant error variance.
```r
plot(mod, which=1)   # residuals vs fitted
```
The residuals-vs-fitted plot shows a clear **cone/funnel** shape: dispersion grows with $\\hat y$ ⇒ assumption **violated** (heteroscedasticity).
""", "images": ["statistics/images/ex8_2a-amountspent-salary.png"]}

ex8["8_2b"] = {"title": "Ex 8.2b — DS: significance test and (in)appropriate prediction interval",
"content": """<span class="exam-question-text">**b3)** Would you suggest using the estimated model to assess the significance of the slope of the regression line? If yes, draw your conclusions on significance; otherwise, explain why it is not recommendable.

**b4)** Would you suggest using the estimated model to build a prediction interval for the amount spent by a customer with a salary equal to 0? If yes, obtain the 99% interval; otherwise explain why it is not recommendable.</span>

---

**Answer.**

**b3)** **Not recommended.** Because of the detected heteroscedasticity, the standard errors of $\\hat\\beta_1$ printed by `summary(mod)` are inconsistent estimators of the true variance; the $t$-statistic does not follow $t_{n-2}$ and the $p$-value is unreliable. Significance conclusions based on it would be invalid.

**b4)** **Not recommended either.** A salary of 0 is **outside the observed range** of `Salary` (extrapolation), and again heteroscedasticity invalidates the SE used in $s_\\epsilon\\sqrt{1+\\tfrac{1}{n}+\\tfrac{(x_g-\\bar x)^2}{(n-1)s^2_x}}$, so the PI is not trustworthy.

```r
# Diagnostic comparison with an alternative predictor
plot(DS$Catalogs, DS$AmountSpent)
mod1 <- lm(AmountSpent ~ Catalogs, data=DS)
summary(mod1)
plot(mod1, which=1)
```
""", "images": ["statistics/images/ex8_2b-amountspent-catalogs.png"]}

ex8["8_3a"] = {"title": "Ex 8.3a — NewHired: Weeks on Age, CI for slope, $R^2$, prediction at Age=36",
"content": """<span class="exam-question-text">**EXERCISE 8.3.** (dataframe `NewHired`)

**a)** Based on the information available for the workers who were able to change their jobs (relying on the job agency), evaluate the relation between the number of weeks needed to find a new job (`Weeks`) and age (`Age`) and assess whether it is significant. Report the sample results you refer to and clarify what are your conclusions on the relation between the two variables.

**b)** Propose a 0.9 interval estimate for the variation in the average number of weeks needed to find a new job corresponding to an increase of 5 years of age.

**c)** Assess what it the proportion of the variance of the number of weeks needed to find a new job explained by age.

**d)** Predict the number of weeks needed to find a new job for an individual aged 36 (with characteristics similar to those of the job agency's workers) using a 99% interval.

**e)** Verify, based on the analysis of residuals, whether the assumptions at the basis of the linear model are fulfilled or not.</span>

---

**Answer.**
```r
mod <- lm(Weeks ~ Age, data=NewHired)
summary(mod)
confint(mod, level=0.9)                        # 90% CI for slope
predict(mod, newdata=data.frame(Age=36),
        interval="prediction", level=0.99)
distr.plot.xy(x=Age, y=Weeks, plot.type="scatter",
              fitline=T, data=NewHired)
plot(mod, which=1)                              # residuals vs fitted
plot(mod, which=3)                              # scale-location
distr.plot.x(x=rstandard(mod), plot.type="histogram")
```

**a)** OLS estimation gives the fitted line $\\widehat{\\text{Weeks}} = -19.5262 + 1.6098\\cdot\\text{Age}$: each extra year of age raises the expected number of weeks needed to find a new job by $\\approx 1.61$. Test $H_0:\\beta_1=0$ vs $H_1:\\beta_1\\neq 0$ with $t=\\hat\\beta_1/\\text{se}(\\hat\\beta_1)\\sim t_{n-2}$ under $H_0$. The printout returns a very small $p$-value ⇒ **reject $H_0$** at any conventional level: the relation between `Age` and `Weeks` is **significant** — older workers tend to need on average more weeks to find a new job.

**b)** 90% CI for $\\beta_1$ from `confint(mod, level=0.9)`: $[1.502505,\\,1.877065]$. Multiplying the extremes by $5$ gives the 90% CI for the change in expected `Weeks` corresponding to $+5$ years of age: $[7.512523,\\,9.385327]$ weeks.

**c)** Goodness-of-fit measured by $R^2 = SSR/SST$: in this case $R^2 \\approx 0.40$ ⇒ `Age` alone explains about $40\\%$ of the variability of `Weeks`.

**d)** 99% prediction interval at $\\text{Age}=36$: $[22.14825,\\,60.46381]$ weeks.

**e)** **Linearity:** the scatterplot of `Weeks` vs `Age` is well approximated by a straight line. **Homoscedasticity:** the residuals-vs-fitted and scale-location plots show somewhat **higher dispersion at central predicted values**, due to the lower number of very young and very old workers — the assumption is only **approximately** satisfied. **Normality:** the histogram of standardized residuals is approximately **symmetric** and bell-shaped ⇒ broadly consistent with a normal erratic component. Overall the linear-model assumptions are **reasonably fulfilled**.
""", "images": ["statistics/images/ex8_3-weeks-age.png"]}

ex8["8_4a"] = {"title": "Ex 8.4a — Restaurants: revenues ~ surface + diagnostics",
"content": """<span class="exam-question-text">**EXERCISE 8.4.** (dataframe `restaurants`)

**d1)** Report the equation of the regression line, provide an interpretation of the slope of the line and assess its significance. Evaluate the model with reference to its explanatory ability.

**d2)** Revenues are measured in thousands of Euros. If the revenues were measured in Euros instead, would the estimated line or its goodness of fit change?

**d3)** Assess whether the considered model (refer to the model at point d1) can be used to make predictions, reporting the tools (graphs — in this case only a sketch is required — and/or summary measures) you rely on to answer.

**d4)** Consider the relation between the standardized residuals and the variable `evening_only` using a suitable graphical tool (report a sketch of the graph). What are your considerations on the model and on the assumptions it is based upon?</span>

---

**Answer.**
```r
mod  <- lm(revenues ~ surface, data=restaurants)
summary(mod)
# d2: rescaling revenues to euros (×1000) only multiplies intercept and
#      slope by 1000; R^2, t-statistics and p-values are unchanged
mod1000 <- lm(revenues*1000 ~ surface, data=restaurants)
summary(mod1000)
# d1/d3: scatter + residuals vs fitted (cone shape → heteroscedasticity)
distr.plot.xy(x=surface, y=revenues, plot.type="scatter",
              fitline=T, data=restaurants)
plot(mod, which=1)
# d4: standardized residuals vs evening_only (structural shift in dispersion)
distr.plot.xy(x=restaurants$evening_only, y=rstandard(mod),
              plot.type="boxplot")
```

**d1)** $\\widehat{\\text{revenues}} = 246.812 + 0.4049\\cdot\\text{surface}$: each additional $1\\,m^2$ raises expected revenues by $\\approx 0.4$ thousand euros. The $p$-value on the slope is close to zero so the effect is **significant at any level**, but $R^2 \\approx 0.119$ ⇒ **poor fit**.

**d2)** Only the coefficients rescale: $\\widehat{1000\\cdot\\text{revenues}} = 246812 + 404.9\\cdot\\text{surface}$. Significance and $R^2$ are unaffected.

**d3)** **Not recommended.** The scatter shows a near-quadratic relation and the residuals-vs-fitted plot has a clear cone shape (heteroscedasticity); the linear model systematically over/under-estimates.

**d4)** Boxplots of standardized residuals split by `evening_only` show **different medians and dispersions** ⇒ the erratic component depends structurally on `evening_only`; a multiple regression model adding this covariate should be considered.
""", "images": ["statistics/images/ex8_4-revenues-surface.png"]}

ex8["8_5a"] = {"title": "Ex 8.5a — Salary on experience: manual estimation, $R^2$, significance, PI at $x=5,7$",
"content": """<span class="exam-question-text">**EXERCISE 8.5.** It is of interest to estimate a model explaining the initial monthly salary ($Y$) offered to a company's new hired based on the years of work experience ($X$). The following statistics have been measured on a random sample of $n=47$ employees:

$$\\sum_{i=1}^{47} y_i = 99150,\\quad s^2_y = 345722,\\quad \\sum_{i=1}^{47} x_i = 297,\\quad s^2_x = 27.048,\\quad s_{xy} = 2697.96.$$

**a)** Estimate the parameters of the linear regression model clarifying the procedure followed (round to 4 digits) and interpret the estimated values.

**b)** Assess the model's goodness of fit using a suitable index (round to 4 digits). Interpret the value.

**c)** Based on the obtained results, can you conclude that the slope of the regression line at the population level is significantly different from zero? Answer and then verify numerically (report the intermediate results and the functions in RStudio possibly used to calculate probabilities).

**d)** Build the 99% interval for the initial monthly salary of two new hired having respectively 5 and 7 years of work experience. Explain why the lengths of the two intervals are different.</span>

---

**Answer.**
```r
sum.y <- 99150; s2_y <- 345722
sum.x <- 297;   s2_x <- 27.048
s_xy  <- 2697.96
n     <- 47

# a) slope, intercept
b1   <- s_xy / s2_x; b1                     # 99.7471
xbar <- sum.x/n; ybar <- sum.y/n            # 6.3191 ; 2109.574
b0   <- ybar - b1*xbar; b0                  # 1479.262

# b) R^2 from correlation squared
R2   <- s_xy^2 / (s2_x*s2_y); R2            # 0.7784

# c) significance test on beta_1
SSE        <- (1 - R2)*(n-1)*s2_y           # 3524152
s2_epsilon <- SSE/(n-2)                     # 78314.48
se_beta1   <- sqrt(s2_epsilon / ((n-1)*s2_x))   # 7.933
tstat      <- b1/se_beta1                   # 12.57
2*(1 - pt(tstat, df=n-2))                   # p-value ≈ 0

# d) 99% prediction interval at x=5 and x=7
yhat_5 <- b0 + b1*5                         # 1978.997
yhat_7 <- b0 + b1*7                         # 2177.492
ME_5 <- qt(0.995, df=n-2)*sqrt(s2_epsilon)*
        sqrt(1 + 1/n + ((5-xbar)^2)/((n-1)*s2_x))
ME_7 <- qt(0.995, df=n-2)*sqrt(s2_epsilon)*
        sqrt(1 + 1/n + ((7-xbar)^2)/((n-1)*s2_x))
c(yhat_5 - ME_5, yhat_5 + ME_5)             # ≈ (1217.83 ; 2740.16)
c(yhat_7 - ME_7, yhat_7 + ME_7)             # ≈ (1417.49 ; 2937.49)
```

**a)** $\\hat\\beta_1 = 2697.96/27.048 = 99.7471$ ⇒ each additional year of experience raises the expected starting salary by $\\approx 99.75$ euros. $\\hat\\beta_0 = 1479.262$ would be the predicted salary for $0$ years of experience.

**b)** $R^2 = 0.7784$ ⇒ the model explains $\\approx 78\\%$ of the variability of starting salaries.

**c)** $t = 12.57$ with $45$ df, $p\\approx 0$ ⇒ **reject $H_0:\\beta_1=0$**: experience is significant at any level.

**d)** The interval at $x=7$ is **wider** because $7$ is farther from $\\bar x = 6.32$ than $5$, so the leverage term $(x_g-\\bar x)^2/((n-1)s^2_x)$ is larger.
""", "images": []}

ex8["8_8a"] = {"title": "Ex 8.8a — Advertising → efficacy: estimation, significance and 99% PI at $x=48$",
"content": """<span class="exam-question-text">**EXERCISE 8.8.** The marketing department of a company presents data on $n=36$ campaigns: advertising expenses ($X$) and efficacy ($Y$). The aggregated statistics are:

$$\\bar x = 50.5,\\quad \\bar y = 12.4,\\quad s^2_x = 660,\\quad s^2_y = 3.5,\\quad r_{xy} = 0.8.$$

**a)** Build the least squares regression line.

**e)** Test the significance of the slope of the line at $5\\%$ (report the realization of the test statistic and the procedure used to compute it).

**f1)** Assume the company is planning to spend $48$ in the next campaign. Which interval would you use to predict the efficacy of the campaign with a $99\\%$ level of confidence?</span>

---

**Answer.**
```r
n      <- 36
xbar   <- 50.5; ybar  <- 12.4
var.x  <- 660;  var.y <- 3.5
cor.xy <- 0.8

# a) slope, intercept from covariance = r*sqrt(var.x*var.y)
cov.xy <- cor.xy*sqrt(var.x*var.y)          # 38.39
b1 <- cov.xy/var.x                          # 0.0583
b0 <- ybar - b1*xbar                        # 9.458
R2 <- cor.xy^2                              # 0.64

# e) significance test on beta_1
SSE        <- (1 - R2)*var.y*(n-1)          # 44.1
s2_e       <- SSE/(n-2)                     # 1.297
se_beta1   <- sqrt(s2_e/((n-1)*var.x))      # 0.0075
tstat      <- b1/se_beta1                   # 7.77
2*(1 - pt(tstat, df=n-2))                   # p-value ≈ 0

# f1) 99% PREDICTION interval at x_g = 48 (single new campaign)
x_g       <- 48
yhat_g    <- b0 + b1*x_g                    # 12.2546
se_yhat_g <- sqrt(s2_e)*sqrt(1 + 1/n + ((x_g - xbar)^2)/((n-1)*var.x))
MEg       <- se_yhat_g * qt(0.995, df=n-2)  # 3.1506
c(yhat_g - MEg, yhat_g + MEg)               # (9.1038 ; 15.4050)
```

**a)** $\\widehat{y} = 9.458 + 0.0583\\,x$. Each extra unit of advertising raises expected efficacy by $\\approx 0.058$ — the slope value depends on the units of $X$ and $Y$ so on its own it is not a measure of "strength".

**e)** $H_0:\\beta_1=0$ vs $H_1:\\beta_1\\neq 0$. $t = 0.0583/0.0075 = 7.77$ with $34$ df, $p\\approx 0$ ⇒ **reject $H_0$**: the slope is significant at any level.

**f1)** Predicting a **single new campaign** ⇒ use the **prediction interval** (with the extra $+1$ inside the square root). The $99\\%$ PI at $x_g=48$ is $\\approx (9.10,\\,15.40)$.
""", "images": []}

ex8["8_10a"] = {"title": "Ex 8.10a — Sales on discount: CI for $\\beta_1$, PI at $x=12$, extrapolation at $x=30$",
"content": """<span class="exam-question-text">**EXERCISE 8.10.** A company analyses the relation between sales $Y$ (thousands of units) and the applied percent discount $X$ on $n=50$ shops. The sample statistics are:

$$\\sum_{i=1}^{50} x_i = 500,\\quad \\sum_{i=1}^{50} y_i = 3000,\\quad \\sum_{i=1}^{50}(y_i-\\bar y)^2 = 40000,\\quad \\sum_{i=1}^{50}(x_i-\\bar x)^2 = 25000,\\quad SSR = 36000.$$

**a)** Estimate the parameters of the linear model of $Y$ on $X$. **b)** Estimate the variance of the model. **c)** Build the $95\\%$ confidence interval for $\\beta_1$ and interpret it. **d)** Obtain the point prediction of sales for a shop with a $12\\%$ discount and build the $95\\%$ prediction interval. **e)** What is the $95\\%$ interval for the **expected** (mean) amount of sales at $12\\%$? **f)** Would you use the model to predict sales for a shop where a $30\\%$ discount will be applied? Why?</span>

---

**Answer.**
```r
n <- 50; sum.x <- 500; sum.y <- 3000
sum.dev2.y <- 40000; sum.dev2.x <- 25000
SSR <- 36000

# a) regression coefficients
var.y  <- sum.dev2.y/(n-1); var.x <- sum.dev2.x/(n-1)
R2     <- SSR/sum.dev2.y                 # 0.9
cov.xy <- sqrt(R2*var.y*var.x)           # 612.2449
b1     <- cov.xy/var.x                   # 1.2
xbar   <- sum.x/n; ybar <- sum.y/n       # 10 ; 60
b0     <- ybar - b1*xbar                 # 48

# b) variance of the model
SSE        <- sum.dev2.y - SSR           # 4000
s2_epsilon <- SSE/(n-2)                  # 83.3333

# c) 95% CI for beta_1
se2_hat1 <- s2_epsilon/sum.dev2.x        # 0.0033
ME       <- sqrt(se2_hat1) * qt(0.975, df=n-2)
c(b1 - ME, b1 + ME)                      # (1.0839 ; 1.3161)

# d-e) point prediction at x=12, 95% PI (single shop) and 95% CI (mean)
x_g  <- 12
yhat <- b0 + b1*x_g                      # 62.4
ME_p <- qt(0.975, df=n-2)*sqrt(s2_epsilon)*
        sqrt(1 + 1/n + ((x_g-xbar)^2)/sum.dev2.x)
ME_c <- qt(0.975, df=n-2)*sqrt(s2_epsilon)*
        sqrt(    1/n + ((x_g-xbar)^2)/sum.dev2.x)
c(yhat - ME_p, yhat + ME_p)              # PI ≈ (43.33 ; 81.47)
c(yhat - ME_c, yhat + ME_c)              # CI ≈ (57.23 ; 67.57)

# f) x=30 is far outside the observed range (mean discount = 10%) → extrapolation
x_g  <- 30
yhat <- b0 + b1*x_g                      # 84
ME_p <- qt(0.975, df=n-2)*sqrt(s2_epsilon)*
        sqrt(1 + 1/n + ((x_g-xbar)^2)/sum.dev2.x)
c(yhat - ME_p, yhat + ME_p)              # very wide interval
```

**a)** $\\widehat{Y} = 48 + 1.2\\,X$: a $1\\%$ increase in the discount is associated with $+1.2$ thousand units of sales.

**b)** $s^2_\\epsilon = 83.33$.

**c)** $CI_{0.95}(\\beta_1) = (1.084,\\,1.316)$: with $95\\%$ confidence a $1\\%$ extra discount raises average sales by $1.08$ to $1.32$ thousand units.

**d)** Point prediction at $x=12$: $\\hat y = 62.4$. $95\\%$ PI $\\approx (43.33,\\,81.47)$.

**e)** $95\\%$ CI for the **expected** sales at $x=12$: $\\approx (57.23,\\,67.57)$ — narrower because it does not include the irreducible variability of an individual shop.

**f)** **Not recommended.** $30\\%$ is far outside the observed range (the average is $10\\%$): predicting there is an extrapolation, the linearity assumption may not hold and the prediction interval becomes very wide because of the large leverage $(x_g-\\bar x)^2$.
""", "images": []}
