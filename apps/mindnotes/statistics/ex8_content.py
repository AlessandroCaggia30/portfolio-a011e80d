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

**Estimated line.** OLS minimises $\\sum_{i=1}^{n}(y_i-\\beta_0-\\beta_1 x_i)^2$ and gives

$$\\widehat{\\text{Debt}} = 1479.262 + 99.7471 \\cdot \\text{Television}.$$

- $\\hat\\beta_0 = 1479.262$: average debt (in \\$) for a family that never watches TV ($x=0$) — meaningful here since $x=0$ is in the observed range.
- $\\hat\\beta_1 = 99.7471$: for **one extra hour of TV per week**, the average weekly debt increases by about \\$99.75. Positive sign ⇒ the theorized direction is confirmed in the sample.

**Significance of `Television`** means testing whether the population slope is non-zero — i.e. whether there is a *linear* effect of TV hours on debt in the population.

- **Hypotheses.** $H_0:\\beta_1=0$ (no linear relation) vs $H_1:\\beta_1\\neq 0$ (two-sided).
- **Test statistic.** Under the Gauss–Markov + normality assumptions,

$$t = \\frac{\\hat\\beta_1 - 0}{\\widehat{\\text{se}}(\\hat\\beta_1)} \\;\\overset{H_0}{\\sim}\\; t_{n-2}=t_{428},\\qquad \\widehat{\\text{se}}(\\hat\\beta_1)=\\frac{s_\\epsilon}{\\sqrt{(n-1)s^2_x}}.$$

- **Procedure.** (i) Fit the model and read $\\hat\\beta_1$ and $\\widehat{\\text{se}}(\\hat\\beta_1)$; (ii) compute $t_{\\text{obs}}$; (iii) compare with the critical value $\\pm t_{1-\\alpha/2,\\,428}$ (reject if $|t_{\\text{obs}}|>t_{1-\\alpha/2,\\,428}$), or equivalently use the two-sided $p$-value $= 2\\cdot P(T_{428}>|t_{\\text{obs}}|)$ and reject if $p<\\alpha$.
- **Conclusion.** `summary(mod)` reports $t_{\\text{obs}}\\approx 12.57$ with $p$-value $\\approx 0$ ⇒ we **reject $H_0$** at any usual $\\alpha$ (1%, 5%, 10%): `Television` is a **strongly significant** predictor of `Debt`, confirming a positive linear association in the population.
""", "images": ["statistics/images/ex8_1-debt-television.png"]}

ex8["8_1b"] = {"title": "Ex 8.1b — TeleDebt: goodness-of-fit index ($R^2$)",
"content": """<span class="exam-question-text">**b)** How would you assess the quality of the model? Write down the expression of the index you would consider and interpret its value, also describing the rationale behind it.</span>

---

**Answer.**
```r
summary(mod)$r.squared        # 0.7784
# or, from the variance decomposition:
SST <- sum((TeleDebt$Debt - mean(TeleDebt$Debt))^2)
SSE <- sum(resid(mod)^2)
SSR <- SST - SSE
R2  <- SSR/SST                # 0.7784
```

**Rationale — variance decomposition.** Under OLS the total variability of $y$ splits orthogonally into an *explained* and a *residual* part:

$$\\underbrace{\\sum_{i=1}^{n}(y_i-\\bar y)^2}_{SST}\\;=\\;\\underbrace{\\sum_{i=1}^{n}(\\hat y_i-\\bar y)^2}_{SSR\\ (\\text{explained})}\\;+\\;\\underbrace{\\sum_{i=1}^{n}(y_i-\\hat y_i)^2}_{SSE\\ (\\text{residual})}.$$

The **coefficient of determination** is the share of $SST$ attributable to the regression:

$$R^2 \\;=\\; \\frac{SSR}{SST} \\;=\\; 1-\\frac{SSE}{SST}\\;\\in\\;[0,1].$$

Properties: $R^2=0$ when the fitted line is flat ($\\hat\\beta_1=0$, model explains nothing beyond $\\bar y$); $R^2=1$ when residuals vanish (perfect fit); in simple linear regression $R^2=r_{xy}^2$, the squared sample correlation.

**Interpretation on TeleDebt.** From `summary(mod)` we read $R^2=0.7784$: about $\\mathbf{77.84\\%}$ of the variability of `Debt` across families is explained by hours of `Television` per week — a high value, the linear model fits the data well. The remaining $\\approx 22\\%$ is residual variability driven by factors not captured by the single regressor `Television`.
""", "images": []}

ex8["8_1c"] = {"title": "Ex 8.1c — TeleDebt: 99% prediction interval at Television=33, PI vs CI, causality",
"content": """<span class="exam-question-text">**c)** Build the 99% interval for the debt predicted for a family whose television was turned on for 33 hours in a week; interpret the obtained interval and list the quantities determining its width.

**d)** At the same significance level, do you expect a wider or a narrower interval for the prediction of the average debt of families whose television was turned on for 33 hours in a week? Explain your answer and verify it numerically.

**e)** Can you conclude based on the obtained results that a decrease in the number of hours the television is turned on will lead to a decrease of the debt, at least on average?</span>

---

**Answer.**
```r
predict(mod, newdata=data.frame(Television=33),
        interval="prediction", level=0.99)
##        fit       lwr        upr
## 1 4770.917  3037.142   6504.692
predict(mod, newdata=data.frame(Television=33),
        interval="confidence", level=0.99)
##        fit       lwr        upr
## 1 4770.917  4691.336   4850.498
```

**c)** Point prediction $\\hat y_g = \\hat\\beta_0 + \\hat\\beta_1\\cdot 33 = 1479.262 + 99.7471\\cdot 33 \\approx 4770.92$ dollars. The 99% **prediction interval** is
$$\\hat y_g \\;\\pm\\; t_{0.995,\\,n-2}\\cdot s_\\epsilon\\sqrt{1+\\tfrac{1}{n}+\\tfrac{(x_g-\\bar x)^2}{(n-1)s^2_x}} \\;\\Rightarrow\\; (3037.14,\\;6504.69).$$
*Interpretation.* With 99% confidence, the debt of a **single** family that watches TV $33$ hours/week falls in $[\\$3037,\\,\\$6505]$. Width drivers: residual standard error $s_\\epsilon$, sample size $n$, dispersion of $X$ ($s^2_x$), distance $(x_g-\\bar x)^2$ (leverage), and the t-quantile (here $t_{0.995,\\,428}\\approx 2.587$).

**d)** **Narrower.** The CI for $E(Y\\mid X=33)$ drops the "$1+$" inside the root, removing the irreducible noise of a new observation:
$$\\hat y_g \\;\\pm\\; t_{0.995,\\,n-2}\\cdot s_\\epsilon\\sqrt{\\tfrac{1}{n}+\\tfrac{(x_g-\\bar x)^2}{(n-1)s^2_x}} \\;\\Rightarrow\\; (4691.34,\\;4850.50).$$
Numerically: PI half-width $\\approx 1733.78$ vs CI half-width $\\approx 79.58$ — the PI is about $22\\times$ wider, confirming the inequality.

**e)** **No.** The fitted model only documents an **association** between `Television` and `Debt` in the observed sample. Causal claims require an experiment or strong identifying assumptions: here the study is **observational**, with likely **confounders** (income, household size, age) and possible **reverse causality** (indebted families staying home more), neither of which are controlled for. Reducing TV hours would not necessarily lower debt on average.
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
$$\\widehat{\\text{AmountSpent}} = -8.0264 + 0.022\\cdot\\text{Salary}.$$

**b2) Homoscedasticity** means $\\Var(\\epsilon_i\\mid X)=\\sigma_\\epsilon^2$ — the error variance is the **same for every observation**, so the $Y_i$ form a *homogeneous sample in terms of variance* and a single common $\\sigma_\\epsilon^2$ can be estimated.
```r
plot(mod, which=1)   # residuals vs fitted
```
The residuals-vs-fitted plot shows a clear **cone** shape: dispersion of the residuals grows with the fitted value $\\hat y$ ⇒ the assumption is **violated** (heteroscedasticity). Consequence: the residuals are not a homogeneous sample, so they cannot be pooled to estimate one common variance — this will undermine SEs, CIs and prediction intervals in b3–b4.
""", "images": ["statistics/images/ex8_2a-amountspent-salary.png"]}

ex8["8_2b"] = {"title": "Ex 8.2b — DS: significance test and (in)appropriate prediction interval",
"content": """<span class="exam-question-text">**b3)** Would you suggest using the estimated model to assess the significance of the slope of the regression line? If yes, draw your conclusions on significance; otherwise, explain why it is not recommendable.

**b4)** Would you suggest using the estimated model to build a prediction interval for the amount spent by a customer with a salary equal to 0? If yes, obtain the 99% interval; otherwise explain why it is not recommendable.</span>

---

**Answer.** Both questions inherit the **heteroscedasticity** flagged in b2: the residuals-vs-fitted plot of `lm(AmountSpent ~ Salary)` shows a clear cone — error variance grows with $\\hat y$, violating $\\Var(\\epsilon_i\\mid X)=\\sigma^2$. Every classical inferential tool downstream of $\\widehat{\\text{se}}(\\hat\\beta_1)$ inherits that bias.

**b3) Significance test on the slope — NOT recommendable.** The $t$-statistic reported by `summary(mod)` relies on
$$\\widehat{\\text{se}}(\\hat\\beta_1)=\\frac{s_\\epsilon}{\\sqrt{(n-1)s^2_x}},\\qquad t=\\frac{\\hat\\beta_1}{\\widehat{\\text{se}}(\\hat\\beta_1)} \\overset{H_0}{\\sim} t_{n-2}.$$
This formula assumes constant $\\sigma^2$. Under heteroscedasticity, $s_\\epsilon^2$ is a **biased** estimator of the (non-existent) common variance, so $\\widehat{\\text{se}}(\\hat\\beta_1)$ is **inconsistent**, $t$ does not follow $t_{n-2}$, and the printed $p$-value is unreliable — usually **too small** (overstating significance) because the OLS SE underestimates the true sampling variability of $\\hat\\beta_1$ at high-leverage points. Any "reject $H_0$" claim built on it is therefore not trustworthy. **Remedy:** use heteroscedasticity-robust (White / HC) standard errors, e.g. `lmtest::coeftest(mod, vcov.=sandwich::vcovHC(mod, type="HC3"))`, then re-read the $p$-value.

**b4) 99% PI at Salary $= 0$ — NOT recommendable, two strikes.**

1. **Extrapolation.** From b1, observed `Salary` runs roughly in $[10\\,000,\\,170\\,000]$; **0 is far below** $\\min(\\text{Salary})$. The PI formula
$$\\hat y_g \\;\\pm\\; t_{0.995,\\,n-2}\\;s_\\epsilon\\sqrt{1+\\tfrac{1}{n}+\\tfrac{(x_g-\\bar x)^2}{(n-1)s^2_x}}$$
penalises distance from $\\bar x$ via the leverage term, but **only inside the modelled range** does the linear-mean assumption $E[Y\\mid X]=\\beta_0+\\beta_1 X$ have empirical support. At $x_g=0$ we have no idea whether the relationship is still linear (or even monotone), so the *point* prediction $\\hat y_g$ is already unsafe.
2. **Heteroscedasticity (again).** Even setting (1) aside, the same $s_\\epsilon$ used as a *single* dispersion measure misrepresents the true variance — large at high salaries, small at low ones — so the half-width is wrong regardless of $x_g$.

**Worked numeric example.** With $\\bar x_\\text{Sal}\\approx 56{,}104$, $s_x\\approx 30{,}616$, $s_\\epsilon\\approx 663$, $\\hat\\beta_0\\approx -15.7$, $\\hat\\beta_1\\approx 0.0218$ and $n=1000$:
$$\\hat y_g(0) = -15.7 + 0.0218\\cdot 0 = -15.7 \\;\\;\\text{\\$}\\;(<0!),\\quad t_{0.995,\\,998}\\approx 2.581.$$
$$\\sqrt{1+\\tfrac{1}{1000}+\\tfrac{(0-56\\,104)^2}{999\\cdot 30\\,616^2}}\\approx\\sqrt{1+0.001+3.36}\\approx 2.087,$$
$$\\Rightarrow \\text{PI}_{99\\%} \\approx -15.7 \\;\\pm\\; 2.581\\cdot 663\\cdot 2.087 \\;\\approx\\; [-3587,\\;3555]\\,\\text{\\$}.$$
The interval is **centred on a negative point prediction** (a customer cannot spend a negative amount) and is **enormously wide** — a textbook tell that the model is being asked something it cannot answer.

**What to do instead.** (i) Re-anchor the prediction inside the observed support, or (ii) refit with a variance-stabilising transform (e.g. `log(AmountSpent)`), or (iii) use a richer predictor. The R block below illustrates (iii): compare `Salary` with `Catalogs` (number of catalogs the customer received), where the relation is more compact and the cone is less pronounced.

```r
# Inspect Salary's range -> confirm x_g = 0 is extrapolation
range(DS$Salary)                                       # ~ [10000, 170000]
mean(DS$Salary); sd(DS$Salary)                         # ~ 56104, 30616

# Naive (and untrustworthy) 99% PI at Salary = 0
predict(mod, newdata=data.frame(Salary=0),
        interval="prediction", level=0.99)             # negative lower bound

# Heteroscedasticity-robust significance test on the slope
library(sandwich); library(lmtest)
coeftest(mod, vcov. = vcovHC(mod, type="HC3"))         # robust t for beta1

# Diagnostic comparison with an alternative predictor
plot(DS$Catalogs, DS$AmountSpent)
mod1 <- lm(AmountSpent ~ Catalogs, data=DS)
summary(mod1)
plot(mod1, which=1)                                    # less pronounced cone
```
""", "images": ["statistics/images/ex8_2b-amountspent-catalogs.png"]}

ex8["8_3a"] = {"title": "Ex 8.3a — NewHired: Weeks on Age, CI for slope, $R^2$, prediction at Age=36",
"content": """<span class="exam-question-text">**EXERCISE 8.3.** (dataframe `NewHired`)

**a)** Based on the information available for the workers who were able to change their jobs (relying on the job agency), evaluate the relation between the number of weeks needed to find a new job (`Weeks`) and age (`Age`) and assess whether it is significant. Report the sample results you refer to and clarify what are your conclusions on the relation between the two variables.

**b)** Propose a 0.9 interval estimate for the variation in the average number of weeks needed to find a new job corresponding to an increase of 5 years of age.

**c)** Assess what it the proportion of the variance of the number of weeks needed to find a new job explained by age.

**d)** Predict the number of weeks needed to find a new job for an individual aged 36 (with characteristics similar to those of the job agency's workers) using a 99% interval.

**e)** Verify, based on the analysis of residuals, whether the assumptions at the basis of the linear model are fulfilled or not.</span>

![Ex 8.3a question](statistics/images/ex8/questions/ex8_3a_question.png)

---

**Answer.**
```r
mod <- lm(Weeks ~ Age, data=NewHired)
summary(mod)                                    # coef, t-stats, R^2
confint(mod, level=0.9)                         # 90% CI for slope
predict(mod, newdata=data.frame(Age=36),
        interval="prediction", level=0.99)
distr.plot.xy(x=Age, y=Weeks, plot.type="scatter",
              fitline=T, data=NewHired)         # linearity check
plot(mod, which=1)                              # residuals vs fitted
plot(mod, which=3)                              # scale-location
distr.plot.x(x=rstandard(mod), plot.type="histogram")
```

**a) Estimation + significance.** OLS minimises $\\sum_i(y_i-\\beta_0-\\beta_1 x_i)^2$ and yields the fitted line
$$\\widehat{\\text{Weeks}} \\;=\\; -19.5262 \\;+\\; 1.6898\\cdot\\text{Age}.$$
*Interpretation.* A one-year increase in age corresponds, on average, to an estimated $\\approx 1.69$ extra weeks needed to find a new job (positive slope ⇒ older workers struggle more). The intercept $-19.53$ is not meaningful on its own: $\\text{Age}=0$ is far outside the observed range.

*Significance test.* $H_0:\\beta_1=0$ (no linear relation) vs $H_1:\\beta_1\\neq 0$, with
$$t \\;=\\; \\frac{\\hat\\beta_1}{\\widehat{\\text{se}}(\\hat\\beta_1)} \\;\\overset{H_0}{\\sim}\\; t_{n-2}.$$
From `summary(mod)`: $t_{\\text{obs}} = 15.153$ with a $p$-value $\\approx 0$ ⇒ **reject $H_0$** at any conventional level ($\\alpha = 1\\%,\\,5\\%,\\,10\\%$). The relation between `Age` and `Weeks` is **strongly significant**: on the basis of the estimated model, a one-year increase in age corresponds on average to a $\\approx 1.69$ week increase in job-search duration.

**b) 90% interval for $5\\beta_1$.** A $+5$-year change in age shifts expected `Weeks` by $5\\beta_1$. By linearity of CIs, $CI_{0.9}(5\\beta_1) = 5\\cdot CI_{0.9}(\\beta_1)$. From `confint(mod, level=0.9)`:
$$CI_{0.9}(\\beta_1) = [1.502525,\\,1.877065] \\;\\Rightarrow\\; CI_{0.9}(5\\beta_1) = [7.512623,\\,9.385327]\\text{ weeks}.$$
With 90% confidence, a 5-year age increase raises the average job-search duration by between $\\approx 7.5$ and $\\approx 9.4$ weeks.

**c) Goodness of fit.** With a single predictor, $R^2 = r_{xy}^2 = SSR/SST$ — the share of `Weeks` variability *explained* by the regression. From `summary(mod)`: $R^2 \\approx 0.40$ ⇒ `Age` alone explains about **40%** of the variability of `Weeks` — moderate fit, leaving $\\approx 60\\%$ unexplained (qualification, sector, regional labour market, etc.).

**d) 99% prediction interval at $\\text{Age}=36$.** A *single* future individual ⇒ use the **prediction** interval (not the CI for the mean), which carries the extra "$1+$" inside the root for irreducible noise:
$$\\hat y_g \\;\\pm\\; t_{0.995,\\,n-2}\\cdot s_\\epsilon\\sqrt{1+\\tfrac{1}{n}+\\tfrac{(x_g-\\bar x)^2}{(n-1)s^2_x}} \\;\\Rightarrow\\; [22.14825,\\,60.46381]\\text{ weeks}.$$
With 99% confidence, a 36-year-old worker (with characteristics similar to the agency's pool) needs between $\\approx 22$ and $\\approx 60$ weeks to find a new job. The interval is wide because $R^2 \\approx 0.40$ ⇒ $s_\\epsilon$ is large ⇒ the irreducible noise component dominates.

**e) Residual diagnostics.**
- **Linearity:** the scatterplot of `Weeks` vs `Age` with the OLS line shows points well approximated by a straight line.
- **Homoscedasticity:** the residuals-vs-fitted and scale-location plots show somewhat **higher dispersion at central predicted values**, reflecting fewer very young and very old workers in the sample — the assumption is only **approximately** satisfied.
- **Normality of errors:** the histogram of standardized residuals is approximately **symmetric** and bell-shaped ⇒ broadly consistent with a normal erratic component.

Overall the linear-model assumptions are **reasonably fulfilled**, so the inference at points (a)–(d) is trustworthy.

---

**Reference answer.**

![Ex 8.3a answer (part 1)](statistics/images/ex8/answers/ex8_3a_answer.png)

![Ex 8.3a answer (part 2)](statistics/images/ex8/answers/ex8_3a_answer2.png)
""", "images": [
    "statistics/images/ex8/questions/ex8_3a_question.png",
    "statistics/images/ex8/answers/ex8_3a_answer.png",
    "statistics/images/ex8/answers/ex8_3a_answer2.png",
]}

ex8["8_4a"] = {"title": "Ex 8.4a — Restaurants: revenues ~ surface, rescaling, diagnostics, evening_only",
"content": """<span class="exam-question-text">**EXERCISE 8.4.** (dataframe `restaurants`)

**d1)** Report the equation of the regression line, provide an interpretation of the slope of the line and assess its significance. Evaluate the model with reference to its explanatory ability.

**d2)** Revenues are measured in thousands of Euros. If the revenues were measured in Euros instead, would the estimated line or its goodness of fit change?

**d3)** Assess whether the considered model (refer to the model at point d1) can be used to make predictions, reporting the tools (graphs — in this case only a sketch is required — and/or summary measures) you rely on to answer.

**d4)** Consider the relation between the standardized residuals and the variable `evening_only` using a suitable graphical tool (report a sketch of the graph). What are your considerations on the model and on the assumptions it is based upon?</span>

---

**Answer.**
```r
mod <- lm(revenues ~ surface, data=restaurants)
summary(mod)

# d1: scatter with OLS line + R^2
distr.plot.xy(x=surface, y=revenues, plot.type="scatter",
              fitline=T, data=restaurants)
summary(mod)$r.squared                         # 0.1192

# d2: rescaling revenues to euros (x 1000) only multiplies intercept and
#     slope by 1000; R^2, t-statistics and p-values are unchanged
mod1000 <- lm(I(revenues*1000) ~ surface, data=restaurants)
summary(mod1000)

# d3: residuals-vs-fitted (look for cone shape / curvature)
plot(mod, which=1)
plot(mod, which=3)                             # scale-location
distr.plot.x(x=rstandard(mod), plot.type="histogram")

# d4: standardized residuals split by evening_only
distr.plot.xy(x=restaurants$evening_only,
              y=rstandard(mod),
              plot.type="boxplot")
```

**d1) Estimated line, slope interpretation, significance, fit.** OLS minimises $\\sum_i(y_i-\\beta_0-\\beta_1 x_i)^2$ and gives

$$\\widehat{\\text{revenues}} \\;=\\; 246.812 \\;+\\; 0.4049\\cdot\\text{surface}.$$

- $\\hat\\beta_1 = 0.4049$ (thousand euros per $m^2$): each additional square metre of surface raises expected weekly revenues by $\\approx 405$ euros — positive sign, consistent with economic intuition.
- $\\hat\\beta_0 = 246.812$ (thousand euros) is the intercept at $\\text{surface}=0$ — outside the observed range, no useful economic meaning.
- **Significance of the slope.** Test $H_0:\\beta_1=0$ vs $H_1:\\beta_1\\neq 0$ with $t=\\hat\\beta_1/\\text{se}(\\hat\\beta_1)\\sim t_{n-2}$ under $H_0$. `summary(mod)` returns a $p$-value $\\approx 0$ ⇒ **reject $H_0$** at any conventional level: `surface` is highly significant.
- **Explanatory ability.** $R^2 \\approx 0.1192$ ⇒ surface alone explains only $\\approx 12\\%$ of the variability of revenues: the slope is **statistically significant but poorly explanatory** — the fit is weak.

**d2) Effect of changing the unit of $Y$.** Multiplying $y$ by $c=1000$ multiplies both intercept and slope by $c$ (and $SSE$, $SST$ by $c^2$), leaving $R^2$, $t$-statistics, $p$-values and confidence/prediction-interval *coverage* unchanged. In Euros:

$$\\widehat{\\text{revenues}_{\\text{€}}} \\;=\\; 246\\,812 \\;+\\; 404.9\\cdot\\text{surface},\\qquad R^2 = 0.1192 \\text{ unchanged}.$$

Only the *units* of the predicted value and of the residual standard error change — the estimated line is the same relation, re-scaled, and **goodness of fit does not change**.

**d3) Can the model be used for prediction? — No, not as it stands.** Two diagnostics matter:

- *Scatter of `revenues` vs `surface`* shows a clearly **non-linear (curved)** pattern that the straight line misses ⇒ the linearity assumption is violated.
- *Residuals-vs-fitted plot* exhibits a pronounced **cone shape**: dispersion grows with the fitted value ⇒ **heteroscedasticity**, the assumption $\\text{Var}(\\epsilon\\mid X)=\\sigma^2$ fails.
- Combined with $R^2=0.12$, prediction intervals from this model would be **biased** (systematic over/under-estimation in different regions of $X$) and have **incorrect width** (too narrow where variance is large). **Not recommended for prediction.** Remedies: polynomial term in `surface`, log-transform of $Y$, or weighted least squares.

**d4) Standardized residuals vs `evening_only`.** Side-by-side boxplots of $r^{\\text{std}}_i=\\hat\\epsilon_i/(s_\\epsilon\\sqrt{1-h_{ii}})$ split by the dummy `evening_only` ∈ {no, yes} show:

- **Different medians** — residuals tend to be systematically positive in one group and negative in the other ⇒ the linear-in-`surface` model **under-predicts** revenues for one type of restaurant and **over-predicts** for the other.
- **Different dispersions** — boxes have visibly different widths ⇒ another sign of heteroscedasticity, here with a structural (discrete-group) source.

**Interpretation.** The erratic component depends structurally on `evening_only`: $E(\\epsilon\\mid X)=0$ does not hold once we condition on this covariate. A specification with `evening_only` included — e.g. $\\text{revenues}\\sim\\text{surface}+\\text{evening\\_only}$, possibly with an interaction $\\text{surface}\\times\\text{evening\\_only}$ — is required to absorb this systematic effect and recover unbiased inference.
""", "images": ["statistics/images/ex8_4-revenues-surface.png"]}

ex8["8_5a"] = {"title": "Ex 8.5a — Salary on experience: manual estimation, $R^2$, significance, PI at $x=5,7$",
"content": """<span class="exam-question-text">**EXERCISE 8.5.** It is of interest to estimate a model explaining the initial monthly salary ($Y$) offered to a company's new hired based on the years of work experience ($X$). The following statistics have been measured on a random sample of $n=47$ employees:

$$\\sum_{i=1}^{47} y_i = 99150,\\quad s^2_y = 345722,\\quad \\sum_{i=1}^{47} x_i = 297,\\quad s^2_x = 27.048,\\quad s_{xy} = 2697.96.$$

**a)** Estimate the parameters of the linear regression model clarifying the procedure followed (round to 4 digits) and interpret the estimated values.

**b)** Assess the model's goodness of fit using a suitable index (round to 4 digits). Interpret the value.

**c)** Based on the obtained results, can you conclude that the slope of the regression line at the population level is significantly different from zero? Answer and then verify numerically (report the intermediate results and the functions in RStudio possibly used to calculate probabilities).

**d)** Build the 99% interval for the initial monthly salary of two new hired having respectively 5 and 7 years of work experience. Explain why the lengths of the two intervals are different.

![Ex 8.5a question](statistics/images/ex8/questions/ex8_5a_question.png)</span>

---

**AI walkthrough.** Closed-form OLS straight from the five summary stats — no raw data required.

1. **Slope first.** The OLS normal equations give $\\hat\\beta_1 = s_{xy}/s^2_x$. Both numerator and denominator are *sample* moments already, so just divide: $\\hat\\beta_1 = 2697.96/27.048 = 99.7471$ €/year.
2. **Intercept by anchoring.** The fitted line always passes through $(\\bar x,\\bar y)$, hence $\\hat\\beta_0 = \\bar y - \\hat\\beta_1\\bar x$. Compute $\\bar x = 297/47 = 6.3191$ and $\\bar y = 99150/47 = 2109.574$, then $\\hat\\beta_0 = 1479.262$ €.
3. **Goodness of fit via squared correlation.** In simple regression $R^2 = r_{xy}^2 = s_{xy}^2/(s^2_x s^2_y) = 2697.96^2/(27.048\\cdot 345722) = 0.7784$ — about $78\\%$ of $\\Var(Y)$ is captured by the line.
4. **Residual variance.** Back out $SSE = (1-R^2)(n-1)s^2_y = 0.2216\\cdot 46\\cdot 345722 \\approx 3{,}524{,}152$, so $s^2_\\epsilon = SSE/(n-2) = 78{,}314.5$ and $s_\\epsilon \\approx 279.8$ €. This is the irreducible noise of one new salary.
5. **Significance.** Plug into the $t$-statistic on $n-2=45$ df: $t = \\hat\\beta_1/\\widehat{\\text{se}}(\\hat\\beta_1)$ with $\\widehat{\\text{se}}(\\hat\\beta_1) = s_\\epsilon/\\sqrt{(n-1)s^2_x} \\approx 7.93$, giving $t \\approx 12.57 \\gg t_{0.995,45}\\approx 2.69$ ⇒ reject $H_0$.
6. **99% prediction interval at a generic $x_g$.** $\\hat y_g \\pm t_{0.995,45}\\, s_\\epsilon \\sqrt{1+\\tfrac{1}{n}+\\tfrac{(x_g-\\bar x)^2}{(n-1)s^2_x}}$. The leverage term grows quadratically with $|x_g-\\bar x|$, so $x_g=7$ (farther from $\\bar x=6.32$) gives a strictly wider PI than $x_g=5$.

![Ex 8.5a AI walkthrough](statistics/images/ex8/ex8_5a_ai.png)

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

---

**Reference answer.**

![Ex 8.5a answer](statistics/images/ex8/answers/ex8_5a_answer.png)
""", "images": [
    "statistics/images/ex8/questions/ex8_5a_question.png",
    "statistics/images/ex8/ex8_5a_ai.png",
    "statistics/images/ex8/answers/ex8_5a_answer.png",
]}

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

# a) OLS estimates via cov(x,y) = r*sx*sy
cov.xy <- cor.xy*sqrt(var.x*var.y)          # 38.4500
b1 <- cov.xy/var.x                          # 0.0583
b0 <- ybar - b1*xbar                        # 9.4583
R2 <- cor.xy^2                              # 0.64

# e) significance test on beta_1
SSE        <- (1 - R2)*var.y*(n-1)          # 44.10
s2_e       <- SSE/(n-2)                     # 1.2971
se_beta1   <- sqrt(s2_e/((n-1)*var.x))      # 0.00749
tstat      <- b1/se_beta1                   # 7.775
t_crit     <- qt(0.975, df=n-2)             # 2.0322
2*(1 - pt(tstat, df=n-2))                   # p-value ≈ 0

# f1) 99% PREDICTION interval at x_g = 48 (single new campaign)
x_g       <- 48
yhat_g    <- b0 + b1*x_g                    # 12.2546
se_yhat_g <- sqrt(s2_e)*sqrt(1 + 1/n + ((x_g - xbar)^2)/((n-1)*var.x))
MEg       <- se_yhat_g * qt(0.995, df=n-2)  # 3.1506
c(yhat_g - MEg, yhat_g + MEg)               # (9.1040 ; 15.4052)
```

**a)** Closed-form OLS estimates: $\\hat\\beta_1=s_{xy}/s^2_x$ with $s_{xy}=r_{xy}\\,s_x s_y = 0.8\\sqrt{660\\cdot 3.5}=38.45$, so $\\hat\\beta_1=38.45/660=0.0583$ and $\\hat\\beta_0=\\bar y-\\hat\\beta_1\\bar x = 12.4-0.0583\\cdot 50.5 = 9.458$. Regression line:
$$\\widehat{y} = 9.458 + 0.0583\\,x.$$
Each extra unit of advertising raises expected efficacy by $\\approx 0.058$ units; since this depends on the units of $X,Y$, it is **not** itself a measure of association strength — for that look at $R^2=r_{xy}^2=0.64$ (64% of the variability of efficacy is explained by the regression).

**e)** Test $H_0:\\beta_1=0$ vs $H_1:\\beta_1\\neq 0$ at $\\alpha=5\\%$ with statistic
$$t=\\frac{\\hat\\beta_1}{\\text{se}(\\hat\\beta_1)},\\qquad \\text{se}(\\hat\\beta_1)=\\sqrt{\\frac{s^2_\\epsilon}{(n-1)s^2_x}},\\qquad s^2_\\epsilon=\\frac{SSE}{n-2}=\\frac{(1-R^2)(n-1)s^2_y}{n-2}=\\frac{0.36\\cdot 35\\cdot 3.5}{34}=1.297.$$
So $\\text{se}(\\hat\\beta_1)=\\sqrt{1.297/(35\\cdot 660)}=0.00749$ and $t=0.0583/0.00749=7.78$ on $n-2=34$ df. Critical value $t_{0.975,34}=2.03$: $|t|=7.78\\gg 2.03$ (equivalently $p\\approx 0$) ⇒ **reject $H_0$**, the slope is highly significant.

**f1)** A *single future campaign* is a new observation, **not** a mean ⇒ use the **prediction interval** (CI would underestimate uncertainty by dropping the irreducible noise term):
$$\\hat y_g \\pm t_{0.995,\\,34}\\cdot s_\\epsilon\\sqrt{1+\\tfrac{1}{n}+\\tfrac{(x_g-\\bar x)^2}{(n-1)s^2_x}}.$$
At $x_g=48$: $\\hat y_g=12.255$, $s_\\epsilon=1.139$, leverage term $1+1/36+(2.5)^2/(35\\cdot 660)=1.0281$, $t_{0.995,34}=2.728$ ⇒ margin $3.151$. The 99% PI is $\\approx (9.10,\\,15.40)$ — with 99% confidence the efficacy of the next campaign at expense $48$ falls in this range.
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
c(yhat - ME_p, yhat + ME_p)              # PI ≈ (43.86 ; 80.94)
c(yhat - ME_c, yhat + ME_c)              # CI ≈ (59.79 ; 65.01)

# f) x=30 is far outside the observed range (mean discount = 10%) → extrapolation
x_g  <- 30
yhat <- b0 + b1*x_g                      # 84
ME_p <- qt(0.975, df=n-2)*sqrt(s2_epsilon)*
        sqrt(1 + 1/n + ((x_g-xbar)^2)/sum.dev2.x)
c(yhat - ME_p, yhat + ME_p)              # ≈ (65.32 ; 102.68), very wide
```

**a)** Use $R^2 = SSR/SST = 36000/40000 = 0.9$, then $|s_{xy}|=\\sqrt{R^2\\,s^2_x s^2_y}=\\sqrt{0.9\\cdot(25000/49)\\cdot(40000/49)}=612.24$. With $\\bar x=10$, $\\bar y=60$:
$$\\hat\\beta_1 = \\frac{s_{xy}}{s^2_x} = \\frac{612.24}{510.20} = 1.2,\\qquad \\hat\\beta_0 = \\bar y-\\hat\\beta_1\\bar x = 60-1.2\\cdot 10 = 48.$$
So $\\widehat Y = 48 + 1.2\\,X$: a $+1\\%$ discount is associated with $+1.2$ thousand units of expected sales.

**b)** $s^2_\\epsilon = \\dfrac{SSE}{n-2} = \\dfrac{SST-SSR}{48} = \\dfrac{4000}{48} = 83.33$ (root MSE $s_\\epsilon=9.129$).

**c)** $\\text{se}(\\hat\\beta_1)=\\sqrt{s^2_\\epsilon/\\sum(x_i-\\bar x)^2}=\\sqrt{83.33/25000}=0.0577$; with $t_{0.975,48}=2.011$ the margin is $0.116$:
$$CI_{0.95}(\\beta_1) = 1.2\\pm 0.116 = (1.084,\\,1.316).$$
The interval is far from $0$ ⇒ slope highly significant; with $95\\%$ confidence each extra discount point raises mean sales by $1.08$–$1.32$ thousand units.

**d)** Point prediction at $x=12$: $\\hat y = 48 + 1.2\\cdot 12 = 62.4$. Using $s_\\epsilon=\\sqrt{83.33}=9.129$, $t_{0.975,48}=2.011$ and leverage $1+1/50+(12-10)^2/25000 = 1.02016$, the margin is $2.011\\cdot 9.129\\cdot\\sqrt{1.02016}=18.54$, so $95\\%$ PI $\\approx (43.86,\\,80.94)$.

**e)** $95\\%$ CI for the **expected** sales at $x=12$: drop the $1$ inside the square root (no irreducible noise) ⇒ margin $2.011\\cdot 9.129\\cdot\\sqrt{0.02016}=2.61$, so $(59.79,\\,65.01)$ — much narrower than the PI because it does not include the individual-shop variability.

**f)** **Not recommended.** $30\\%$ is far outside the observed range (mean $\\bar x=10\\%$, $s_x=\\sqrt{25000/49}\\approx 22.6$): predicting there is an **extrapolation**. The linearity assumption may break down beyond the data, and the leverage term $(30-10)^2/25000=0.016$ blows up the interval: $\\hat y=84$ with $95\\%$ PI $\\approx (65.32,\\,102.68)$ — a $\\pm 18.7$ window that is hardly informative for decision-making.
""", "images": []}
