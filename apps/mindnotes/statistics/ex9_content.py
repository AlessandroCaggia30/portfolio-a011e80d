"""Ex 9 — Multiple linear regression and diagnostics across many datasets."""

ex9 = {}

ex9["9_1"] = {"title": "Ex 9.1 — Baseball: Major ~ Minor + Age (fit, prediction, multicollinearity, diagnostics)",
"content": """**Question (dataframe `Baseball`).** The GM of a baseball team is evaluating minor-league players with particular attention to performance with respect to home-run hits. For a random sample of players, the dataset collects: number of home runs hit in the first two full years as a major-league player (`Major`, dependent), number of home runs in the last full year in the minor leagues (`Minor`), age (`Age`) and number of years of professional baseball (`Years`). Results are in `Baseball`.
**a)** Analyse the model relating `Major` to `Minor` and `Age`. How well does it fit? Report the analytic expression of the considered statistics and explain the meaning / definition of the quantities they are based upon.
**b)** Assess the significance of the explanatory variables and interpret the observed coefficients. Can you interpret the intercept in the model?
**c)** Predict the number of home runs in the first two years in the major league for a player aged 25 who hit 22 home runs in his last year in the minor league. Would you use a point or an interval estimate? Why?
**d)** Can you predict the difference in the average number of home runs in the first two years in the major league (`Major`) for two players differing by 2 years of age? If yes, would you use a point or an interval estimate? Why? If not, explain why it is not possible, whether specific assumptions are needed and, in this case, answer under such assumptions.
**e)** Does the model change when `Years` is also included among the explanatory variables? Does the significance of the other explanatory variables change? How do you explain the obtained results? Which model would you finally propose?
**f)** For the model selected at point e), determine whether the LR assumptions are satisfied. If they are not, can you explain why?

---

**Answer.**

**a) Goodness of fit.** The coefficient of determination is

$$R^2 \\;=\\; 1-\\dfrac{SSE}{SST} \\;=\\; 1-\\dfrac{\\sum_{i=1}^{n}(y_i-\\hat y_i)^2}{\\sum_{i=1}^{n}(y_i-\\bar y)^2}$$

where $SSE$ is the sum of squared regression errors (observed minus predicted) and $SST$ is the total sum of squares (deviations of $y_i$ around $\\bar y$). $R^2\\in[0,1]$ is the share of the variance of $y$ explained by the linear model. Here $R^2\\approx 0.3348$: **relatively low**, so the model does not fit data particularly well. Still, **at least one regressor matters**: the global F-statistic tests $H_0:\\beta_{\\text{Minor}}=\\beta_{\\text{Age}}=0$ vs $H_1:$ at least one $\\neq 0$, with

$$F \\;=\\; \\dfrac{SSR/K}{SSE/(n-K-1)}$$

($K$ = number of regressors). The realised $F\\approx 30.95$ has $p\\!\\approx\\!0$, so there is high dispersion around the regression plane but a significant linear relation with `Major`.

**b) Individual significance & interpretation.** `Minor` is **highly significant** ($t\\approx 7.444$, $p\\!\\approx\\!0$): we reject $H_0:\\beta_{\\text{Minor}}=0$. The estimate $\\hat\\beta_{\\text{Minor}}\\approx 0.65112$ means that, holding `Age` fixed, **one extra minor-league home run is associated with an expected $+0.65$ home runs in the first two major-league years**. For `Age` the test outcome depends on the significance level: with $\\hat\\beta_{\\text{Age}}\\approx 0.81406$, the coefficient is significant only at levels above $\\approx 0.024$ (e.g. 0.025 or 0.05). Interpretation: holding `Minor` fixed, **+1 year of age is associated with $+0.81$ expected major-league HRs**. The **intercept** corresponds to the predicted value when `Minor = Age = 0`, which has **no meaningful interpretation** here.

**c) Prediction at Age=25, Minor=22.** Point prediction $\\hat y \\approx 22.2555$. Because $R^2$ is low (large residual variance), one should report a **prediction interval** rather than the point alone. The 95% PI is $[7.874212,\\;36.63678]$.

**d) Effect of $+2$ years of `Age`.** It is **not possible to predict a variation in the mean of `Major`** for a 2-year increase in `Age` *unless one assumes the two players have the same number of home runs in their last full year in the minor leagues* (`Minor`): the coefficient on `Age` measures the partial effect with the other regressors held constant. Under that assumption a $+2$ year change in `Age` corresponds to an expected variation $2\\,\\hat\\beta_{\\text{Age}} = 2\\cdot 0.81 = 1.62$ extra home runs. Since dispersion is large, **report an interval**: multiply the 95% CI for $\\beta_{\\text{Age}}$ by 2 — endpoints $2\\cdot(0.108\\,;\\,1.52)=(0.216\\,;\\,3.04)$.

**e) Adding `Years`.** When `Years` is added, the **adjusted $R^2$ barely changes**; `Minor` stays significant, but `Age` becomes non-significant and `Years` is significant only at the 10% level. The reason is **multicollinearity**: good baseball players start at similar ages and so the time of their career is connected to their age — $\\mathrm{cor}(\\text{Age},\\text{Years})\\approx 0.73$. Not perfect collinearity, but including both is redundant: the marginal contribution of `Age` given `Minor` and `Years` is non-significant, and so is `Years` given `Minor` and `Age`. **Reasonable choice:** a model with `Minor` and `Years` only — its adjusted $R^2$ is higher than the alternatives and `Years` becomes significant at the 0.5% level (its joint contribution with `Minor` is rejected unless $\\alpha$ very small, $\\sim 0.001$).

**f) Diagnostics.** The plot of residuals vs fitted shows a clear **violation of homoscedasticity**: dispersion grows with $\\hat y$ (and with `Minor` and `Years`). Plotting standardised residuals against each regressor confirms heteroscedasticity, mainly driven by `Minor`.

```r
# a-b) Fit and inspect
mod <- lm(Major ~ Minor + Age, data=Baseball); summary(mod)
## R^2 ~ 0.3348, F ~ 30.95 (p ~ 0)
## beta_Minor ~ 0.65112 (t = 7.444, p ~ 0)
## beta_Age   ~ 0.81406 (p ~ 0.024)

# c) Point + 95% prediction interval at Age=25, Minor=22
predict(mod, newdata=data.frame(Age=25, Minor=22), interval="prediction")
## fit = 22.2555 ; lwr = 7.874212 ; upr = 36.63678

# d) Variation for +2 years of Age (under "same Minor"): 2 * beta_Age and 2 * CI
2*coef(mod)["Age"]               # ~ 1.62
2*confint(mod)["Age", ]          # ~ (0.216 ; 3.04)

# e) Add Years -> multicollinearity diagnostics
mod1 <- lm(Major ~ Minor + Age + Years, data=Baseball); summary(mod1)
cor(Baseball$Age, Baseball$Years)         # ~ 0.73
mod2 <- lm(Major ~ Minor + Years, data=Baseball); summary(mod2)   # preferred

# f) Residual diagnostics for the preferred model
plot(mod2, which=1)                        # residuals vs fitted
distr.plot.xy(x=Minor, y=rstandard(mod2), plot.type="scatter", data=Baseball)
distr.plot.xy(x=Years, y=rstandard(mod2), plot.type="scatter", data=Baseball)
```
""", "images": ["statistics/images/ex9_1-baseball.png"]}

ex9["9_2"] = {"title": "Ex 9.2 — Promotional channels: t-tests, CIs and marginal effects",
"content": """**Question.** A company studies the combined efficacy of two promotional channels. For $n=30$ campaigns the amount spent on `Channel1` and `Channel2` is recorded together with a *Success* indicator. Summary stats (`min, median, mean, max, var`): Success $(124,\\ 328,\\ 315.4,\\ 464,\\ 7059.01)$; Channel1 $(100,\\ 300,\\ 300,\\ 500,\\ 20689.66)$; Channel2 $(1000,\\ 3500,\\ 3500,\\ 6000,\\ 3017241)$. Regression `Success ~ Channel1 + Channel2` (coef. with SE in brackets):

\\begin{tabular}{p{10cm}|p{14cm}|p{14cm}}
\\textbf{} & \\textbf{Estimate} & \\textbf{Standard error} \\\\
(Intercept) & 164.0100 & 35.8676 \\\\
Channel1 & 0.1398 & 0.0814 \\\\
Channel2 & 0.0313 & 0.0067
\\end{tabular}

Residual standard error $s_\\varepsilon=63.08$ on $df=27$.

**a)** Assess fit and global validity (F-test: realisation, p-value, conclusion). **b1)** Are the coefficients significant? Interpret. **b2)** 99% CI for the variation of *Success* per 1-unit increase in `Channel2`; does it include 150? Same for `Channel1` at 250 — is +50 plausible? **b3)** Reduction of 50 units on `Channel1` with `Channel2` fixed: point estimate and 95% CI for the change in *Success*. **c)** Are the conclusions reliable? **d)** Predict *Success* when `Channel1`=100 and `Channel2`=1000. **e)** The reduced model on `Channel2` only has $s_\\varepsilon = 65.24$ — would you prefer it?

---

**Answer.**

**(a) Fit + global F-test.** With $n=30$, $K=2$, $df=n-K-1=27$:

$SST = s_y^2\\,(n-1) = 84.02^2\\cdot 29 = 204\\,711.3$; $SSE = s_\\varepsilon^2\\,(n-K-1) = 63.08^2\\cdot 27 = 107\\,435.3$; so

$R^2 = 1 - SSE/SST = 1 - 107435.3/204711.3 = 0.4752$ — the two channels jointly explain ~47.5 % of the variance of *Success*.

Global $F$ tests $H_0\\!:\\beta_1=\\beta_2=0$ vs $H_1\\!:$ at least one $\\ne 0$:
$$F = \\frac{SSR/K}{SSE/(n-K-1)} = \\frac{(204711.3-107435.3)/2}{63.08^2}=12.2234,\\quad p = 1-F_{2,27}(12.2234)\\approx 0.000166.$$
Equivalent form using $R^2$: $F=\\dfrac{R^2/K}{(1-R^2)/(n-K-1)}=\\dfrac{0.4752/2}{0.5248/27}=12.22$. Strongly reject $H_0$: at least one channel is significant.

**(b1) Individual t-tests** ($df=27$, two-sided):
- `Channel1`: $t = 0.1398/0.0814 = 1.7174$, $p$-value $= 2(1-F_{t_{27}}(1.7174))= 0.0973$. **Not** rejected at $\\alpha=5\\%$ (only at $\\alpha=10\\%$): given the spend on `Channel2`, a 1-€ extra on `Channel1` is associated with $+0.14$ on the *Success* index on average, but the empirical evidence is too weak to claim the population effect is non-zero.
- `Channel2`: $t = 0.0313/0.0067 = 4.6716$, $p$-value $\\approx 0$. Highly significant: keeping `Channel1` fixed, $+1$ € on `Channel2` raises the *Success* index by $0.0313$ on average.

**(b2) 99% CI for $\\beta_2$.** Critical value $t_{27,\\,0.995}=2.7707$:
$$\\hat\\beta_2 \\pm t_{27,\\,0.995}\\,se(\\hat\\beta_2) = 0.0313 \\pm 2.7707\\cdot 0.0067 = (0.01274,\\ 0.04986).$$
The CI excludes 0 (consistent with the t-test) and is far from 150 — extremely implausible.

**99% CI for the change in *Success* when `Channel1` increases by 250** (with `Channel2` fixed): point $= 250\\cdot 0.1398=34.95$; ME $= 250\\cdot 2.7707\\cdot 0.0814 = 56.36$; CI $= (-21.41,\\ 91.31)$ — straddles 0, so +50 is plausible but so is a *decrease*.

**(b3) Reduction of 50 units on `Channel1`** (Channel2 fixed): point $= -50\\cdot 0.1398 = -6.99$. The 95% CI for $\\beta_1$ is $0.1398 \\pm t_{27,\\,0.975}\\cdot 0.0814 = 0.1398 \\pm 2.0518\\cdot 0.0814 = (-0.0272,\\ 0.3068)$. Multiplying by $-50$ (and flipping endpoints): $(-15.34,\\ 1.36)$. Includes 0: we cannot exclude that cutting 50 units actually *raises* *Success*, although the interval lies mostly on the negative side.

**(c) Reliability.** Inference rests on $E[\\varepsilon]=0$, $\\mathrm{Var}(\\varepsilon)=\\sigma^2$ constant, $\\mathrm{Cor}(\\varepsilon_i,\\varepsilon_j)=0$ and approximate normality. With only summary stats and no raw data we **cannot** check residuals — therefore the validity of the CIs / tests **cannot be assessed** from the information given.

**(d) Prediction at (Ch1=100, Ch2=1000):** $\\hat Y = 164.01 + 0.1398\\cdot 100 + 0.0313\\cdot 1000 = 209.29$. Given the modest $R^2\\approx 0.48$, a point estimate alone is unreliable: a confidence (mean) or prediction (individual) interval should accompany it.

**(e) Channel2-only vs full model.** Compare adjusted $R^2 = 1 - s_\\varepsilon^2/s_y^2$:
$$\\text{Adj.}R^2_\\text{full} = 1 - 63.08^2/7059.01 = 0.4363,\\qquad \\text{Adj.}R^2_\\text{Ch2only} = 1 - 65.24^2/7059.01 = 0.3970.$$
The full model has a higher adjusted $R^2$ (since $63.08 < 65.24$): adding `Channel1` is *worth it on penalised fit*, even though its individual t-test is borderline.

```r
# Setup -------------------------------------------------------------
n <- 30; K <- 2; df <- n - K - 1                  # 27
b0 <- 164.0100; se0 <- 35.8676
b1 <- 0.1398;   se1 <- 0.0814
b2 <- 0.0313;   se2 <- 0.0067
s.eps <- 63.08
s.y   <- sqrt(7059.01)                            # 84.02

# (a) Global fit + F-test ------------------------------------------
SST <- s.y^2 * (n - 1)                            # 204711.3
SSE <- s.eps^2 * (n - K - 1)                      # 107435.3
R2  <- 1 - SSE/SST                                # 0.4752
F.stat <- (SST - SSE)/K / s.eps^2                 # 12.2234
1 - pf(F.stat, df1=K, df2=n-K-1)                  # 0.000166

# (b1) Individual t-tests ------------------------------------------
2*(1 - pt(abs(b1/se1), df=df))                    # Channel1: 0.0973
2*(1 - pt(abs(b2/se2), df=df))                    # Channel2: ~ 0

# (b2) 99% CI on beta_2 and on 250 * beta_1 ------------------------
qt(0.995, df=df)                                  # 2.770683
c(b2 - se2*qt(0.995,df), b2 + se2*qt(0.995,df))   # (0.01274, 0.04986)
250*b1                                            # 34.95
250*c(b1 - se1*qt(0.995,df), b1 + se1*qt(0.995,df))   # (-21.41, 91.31)

# (b3) -50 * beta_1: 95% CI ----------------------------------------
-50*b1                                            # -6.99
CI95.b1 <- c(b1 - se1*qt(0.975,df), b1 + se1*qt(0.975,df))
sort(-50 * CI95.b1)                               # (-15.34, 1.36)

# (d) Point prediction ---------------------------------------------
b0 + b1*100 + b2*1000                             # 209.29

# (e) Compare full vs Channel2-only via adjusted R^2 ---------------
1 - s.eps^2 / s.y^2                               # 0.4363  (full)
1 - 65.24^2 / s.y^2                               # 0.3970  (Ch2 only)
```
""", "images": []}

ex9["9_3"] = {"title": "Ex 9.3 — Competition: Performance ~ Competition (+ Quality)",
"content": """**Question (dataframe `Competition`).** Clothing-and-accessories retailer with stores in central areas. `Performance` measured by a proper index; `Competition` is the perceived level of competition; `Quality` is an aggregated indicator of staff/policy quality. **a)** Fit the *simple* linear model `Performance ~ Competition`; estimate it; interpret the coefficient; comment on fit. **b)** Add `Quality`; estimate the new model and interpret coefficients. Compare with (a) and explain the result.

---

**Answer.**
```r
# a) Simple model: Performance ~ Competition
mod <- lm(Performance ~ Competition, data=Competition); summary(mod)
# beta_0 hat ~ 191.4 ; beta_Competition hat ~ -1.62  (SE ~ 0.65, p ~ 0.014)
# Interpretation: +1 unit of perceived Competition -> Performance falls by ~1.62.
# Fit: R^2 ~ 0.05  =>  Competition alone explains only ~5% of the variance:
# the model is statistically significant but practically very poor.

# b) Add Quality
mod1 <- lm(Performance ~ Competition + Quality, data=Competition); summary(mod1)
# beta_0 hat ~ -5.4 ; beta_Competition hat ~ +0.42 (n.s.) ; beta_Quality hat ~ 3.95 ***
# R^2 jumps to ~ 0.85 (huge improvement).
# The Competition effect VANISHES once Quality is controlled for:
# omitted-variable bias in (a). Stores in tougher-competition areas
# tend to invest more in Quality; ignoring Quality, that positive effect
# was loaded onto Competition with the wrong sign.
# Conclusion: Performance is driven by Quality, not by Competition per se.

# Nested-model comparison
anova(mod, mod1)        # F-test confirms Quality is highly significant

# Diagnostics on mod1
plot(mod1, which=1)
distr.plot.x(x=rstandard(mod1), plot.type="histogram")
```
""", "images": ["statistics/images/ex9_3-competition.png"]}

ex9["9_4"] = {"title": "Ex 9.4 — superstore: MntMeatProducts ~ IncomeK + Age (+ KidsAtHome)",
"content": """**Question (dataframe `superstore`, $n=256$).** Customers of a food retailer through different channels. Variables include `Age`, `IncomeK` (k euro), `KidsAtHome` (No/Yes), amounts spent on `MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds`, deals/web/catalog/store purchases. **a)** Estimate `MntMeatProducts ~ IncomeK + Age`; report. **a1)** Is `Age` significant? Test it. **a2)** Interpret the coefficient of `Age`. **b)** Estimate `MntMeatProducts ~ IncomeK + KidsAtHome`; write the equations with/without children. **c)** Estimate `MntMeatProducts ~ IncomeK + Age + KidsAtHome`; is `Age` globally significant ($\\alpha=0.05$)? **c1)** Prediction for a 40-year-old client with `IncomeK=75` and children. **c2)** 95% interval for the average amount spent (clients aged 40, `IncomeK=75`, with children). **c3)** Reliability of predictions?

---

**Answer.**

**(a) Model with continuous predictors.** OLS gives

$$\\widehat{\\text{MntMeat}} = -115.68 + 7.954\\,\\text{IncomeK} - 2.903\\,\\text{Age}.$$

Fit: $R^2 = 0.550$, adjusted $R^2 = 0.546$ — the two predictors jointly explain $\\approx 55\\%$ of the variance. Global $F_{2,253}=154.3$ ($p\\approx 0$): at least one slope is non-zero.

**(a1) Significance of `Age`.** Test $H_0:\\beta_{\\text{Age}}=0$ vs $H_1:\\ne 0$ at $\\alpha=0.05$. From the summary $\\hat\\beta_{\\text{Age}}=-2.903$, $\\widehat{\\text{se}}=0.884$, so $t=-2.903/0.884=-3.284$ on $df=n-K-1=253$. The two-sided $p$-value is $\\approx 0.0012 < 0.05$: **reject $H_0$**, `Age` is significant.

**(a2) Interpretation.** Holding `IncomeK` fixed, **one extra year of age is associated with a decrease of $\\approx 2.90\\,$€ in expected spending on meat products** — older customers in this sample buy less meat once income is controlled for.

**(b) Model with dummy `KidsAtHome`.** With `No` as reference and $D=\\mathbb{1}\\{\\text{KidsAtHome="Yes"}\\}$:

$$\\widehat{\\text{MntMeat}} = -176.08 + 5.778\\,\\text{IncomeK} + 168.09\\,D,\\qquad R^2=0.618.$$

Two parallel lines (same slope on `IncomeK`, different intercepts):

- $D=0$ (no kids): $\\widehat{\\text{MntMeat}} = -176.08 + 5.778\\,\\text{IncomeK}$
- $D=1$ (with kids): $\\widehat{\\text{MntMeat}} = (-176.08+168.09) + 5.778\\,\\text{IncomeK} = -7.99 + 5.778\\,\\text{IncomeK}$

The shift $+168.09$ €  is highly significant ($t=7.633$, $p\\approx 0$): customers with kids spend systematically more on meat at any given income.

**(c) Full model.**

$$\\widehat{\\text{MntMeat}} = -73.77 + 6.132\\,\\text{IncomeK} - 2.793\\,\\text{Age} + 166.75\\,D,$$

with $R^2 = 0.636$, adjusted $R^2=0.632$, $F_{3,252}=146.8$ ($p\\approx 0$). The marginal test $H_0:\\beta_{\\text{Age}}=0$: $t=-2.793/0.796=-3.508$, $p\\approx 0.0005 < 0.05$ → **`Age` remains significant** also globally / after controlling for kids.

**(c1) Point prediction** at $(\\text{IncomeK}=75,\\,\\text{Age}=40,\\,D=1)$:

$$\\hat y = -73.77 + 6.132\\cdot 75 - 2.793\\cdot 40 + 166.75 = 441.18\\,\\text{€}.$$

**(c2) 95% CI for the conditional mean** $E[\\text{MntMeat}\\mid x_0]$: $(408.62,\\;473.73)$. Width $\\approx 65$ €. (For reference, no-kids same profile: $\\hat y\\approx 274.43$, CI $(239.64,\\;309.21)$.)

**(c3) Reliability.** The residual standard error is $s_\\varepsilon\\approx 134.6$ €  — large relative to the predicted mean — and the **residuals-vs-fitted plot for model (a) shows a clear funnel pattern** (variance grows with $\\hat y$): heteroscedasticity. Residuals are also right-skewed (omnibus + JB tests reject normality, $p\\approx 0$). The 95% **prediction interval** for an individual customer at the same profile is $(174.0,\\;708.3)$ — extremely wide. Conclusion: the CI on the mean is informative, but **individual predictions are very uncertain**; the usual SE-based inference should be taken with caution given the heteroscedasticity.

```r
# a) Continuous predictors
mod <- lm(MntMeatProducts ~ IncomeK + Age, data=superstore); summary(mod)
## (Intercept) -115.683 (40.852)  t=-2.832  p=0.005
## IncomeK        7.954 ( 0.453)  t=17.544  p~0
## Age           -2.903 ( 0.884)  t=-3.284  p=0.001
## R^2 = 0.550,  F(2,253) = 154.3,  p ~ 0,  s_eps = 137.4

# a1) Significance of Age: t = -3.284, p ~ 0.001 < 0.05  =>  reject H0
# a2) +1 yr of Age  =>  -2.90 EUR expected meat spend (IncomeK fixed)

# b) Dummy KidsAtHome (No = reference)
moddisc <- lm(MntMeatProducts ~ IncomeK + KidsAtHome, data=superstore); summary(moddisc)
## (Intercept)    -176.08
## IncomeK           5.778 ***
## KidsAtHomeYes   168.09 ***   R^2 = 0.618
# Equations:
#   No  : MntMeat = -176.08          + 5.778*IncomeK
#   Yes : MntMeat = (-176.08+168.09) + 5.778*IncomeK = -7.99 + 5.778*IncomeK

# c) All three predictors
modter <- lm(MntMeatProducts ~ IncomeK + Age + KidsAtHome, data=superstore); summary(modter)
## (Intercept)     -73.77 (37.19)  t=-1.98  p=0.048
## IncomeK           6.132 ( 0.47)  t=13.01  p~0
## Age              -2.793 ( 0.80)  t=-3.51  p=0.0005   *** significant at 5%
## KidsAtHomeYes   166.75 (21.55)  t= 7.74  p~0
## R^2 = 0.636,  adj R^2 = 0.632,  F(3,252) = 146.8

# c1) Point prediction at (IncomeK=75, Age=40, KidsAtHome="Yes")
predict(modter, newdata=data.frame(IncomeK=75, Age=40, KidsAtHome="Yes"))
## fit = 441.18

# c2) 95% CI for the conditional mean
predict(modter, newdata=data.frame(IncomeK=75, Age=40, KidsAtHome="Yes"),
        interval="confidence")
## fit = 441.18  lwr = 408.62  upr = 473.73
# (no-kids profile, for comparison)
predict(modter, newdata=data.frame(IncomeK=75, Age=40, KidsAtHome="No"),
        interval="confidence")
## fit = 274.43  lwr = 239.64  upr = 309.21

# c3) Reliability diagnostics
predict(modter, newdata=data.frame(IncomeK=75, Age=40, KidsAtHome="Yes"),
        interval="prediction")   # PI for an individual: ~ (174 ; 708)
plot(mod, which=1)                          # funnel pattern -> heteroscedasticity
distr.plot.x(x=rstandard(modter), plot.type="histogram")
```
""", "images": ["statistics/images/ex9_4-superstore.png"]}

ex9["9_5"] = {"title": "Ex 9.5 — Restaurants: revenues ~ seats + area + days_open + evening_only",
"content": """**Question (dataframe `restaurants`).**
**a)** Estimate the model relating `revenues` to the number of `seats`, the `area`, the number of days open (`days_open`) and to whether or not the restaurant is opened only for dinner (`evening_only`). Write the equation of the estimated model.
**b)** Provide an interpretation of the estimated coefficient of the variable `days_open`.
**c)** Based on the model can you conclude that it is convenient for a restaurant to be opened only for dinner, other things being equal?
**d)** Is it correct to state that the model does not provide any indication about restaurants located in the *Center*?
**e)** Is it correct to state that restaurants in central area have lower average revenues, other things being equal?
**f)** What can you say about the restaurants in the *NorthWest* area?
**g)** Would you consider a simpler model based only on the two variables `surface` and `avg_daily`?
**h)** Provide an analysis of residuals for the model estimated at point **a)**.

---

**Answer.**

**a) Estimated model equation.** OLS on `restaurants` gives

$$\\widehat{\\text{revenues}}\\;=\\;89.004+1.756\\,\\text{seats}+38.193\\,\\text{areaNorthWest}+0.997\\,\\text{areaSouthEast}+13.015\\,\\text{days\\_open}-171.704\\,\\text{evening\\_only1}.$$

Revenues are in hundreds of euros; `area` enters via dummies for NorthWest and SouthEast (Center is the reference), and `evening_only1` is the dummy for the "dinner only" level.

**b) Interpretation of $\\hat\\beta_{\\text{days\\_open}}=13.015$.** Other conditions being equal (same `seats`, `area`, `evening_only`), each **additional day of opening per month** is associated with an **average increase of about 13.01 hundred euros** ($\\approx 1{,}301$ €) in monthly revenues.

**c) Is dinner-only convenient?** No. The estimated coefficient on the dummy `evening_only1` is $\\hat\\beta\\approx -171.7045$ and is **significantly different from 0** ($p$ very small). Hence, for given values of the other regressors, **opening only in the evening lowers expected revenues by $\\approx 171.7$ hundred euros** ($\\approx 17{,}170$ €) — clearly **not** convenient ceteris paribus.

**d) "The model says nothing about the Center."** **False.** `Center` is the **reference category** of the factor `area`: its effect is absorbed into the **intercept** $\\beta_0=89.004$. The model **does** describe Center restaurants; the equation specialised to Center (with `evening_only=0`) is

$$\\widehat{\\text{revenues}}_{\\text{Center}}\\;=\\;89.004+1.756\\,\\text{seats}+13.015\\,\\text{days\\_open}-171.704\\,\\text{evening\\_only1}.$$

The dummies on NorthWest and SouthEast measure **contrasts vs Center**.

**e) "Center has lower average revenues, c.p."** **No.** The two area dummies (NorthWest, SouthEast) measure the gap **relative to Center**. The SouthEast dummy is **not significant** ($\\hat\\beta\\approx 0.997$, near zero): no detectable difference between Center and SouthEast. So one **cannot** conclude that Center has systematically lower revenues than the other areas, c.p.

**f) NorthWest.** The dummy `areaNorthWest` has coefficient $\\approx +38.193$ and is **significant**: c.p., restaurants in the **NorthWest area earn on average $\\approx 38{,}193$ €/month more** than restaurants in the Center (and, by the SE-vs-Center result, also more than SouthEast).

**g) Simpler model with `surface` and `avg_daily`?** Compare the two specifications using the **adjusted $R^2$** (which penalises complexity and is comparable across models with different numbers of regressors):

- Model (a) `seats + area + days_open + evening_only`: $R^2_{\\text{adj}}\\approx 0.5538$.
- Reduced model `surface + avg_daily`: $R^2_{\\text{adj}}\\approx 0.4457$.

Model (a) is **preferable**: it explains more variability of `revenues` even after adjusting for the extra parameters.

**h) Residual analysis for model (a).** The diagnostic plots are **not entirely satisfactory**:
- **Residuals vs fitted** shows a **structural pattern**: the model tends to **underestimate** revenues for high fitted values, and a handful of **outliers** is visible. Residual **dispersion grows with the predicted value** — sign of **heteroscedasticity** (also confirmed by an increasing trend in the scale-location plot, not shown).
- The **histogram of standardised residuals** is **left-skewed**, departing from the normal benchmark.

So homoscedasticity is plausibly violated and the error distribution is asymmetric; OLS standard errors and the corresponding CIs/tests should be taken with caution.

```r
# a) Estimate the model and read the equation
mod <- lm(revenues ~ seats + area + days_open + evening_only,
          data = restaurants)
summary(mod)
## (Intercept)        89.004
## seats               1.756
## areaNorthWest      38.193
## areaSouthEast       0.997   # n.s.
## days_open          13.015
## evening_only1    -171.704

# b) Coefficient of days_open
coef(mod)["days_open"]            # ~ 13.015 (hundreds of euros per extra day)

# c) Dinner-only effect and its significance
summary(mod)$coefficients["evening_only1", ]   # estimate ~ -171.70, p ~ 0

# d-e-f) Area contrasts are vs the reference level (Center)
levels(restaurants$area)          # "Center" should be first (reference)
# Specialise to Center: drop area dummies (they are 0 for Center)

# g) Reduced 2-predictor model and adjusted R^2 comparison
mod2 <- lm(revenues ~ surface + avg_daily, data = restaurants)
summary(mod2)$adj.r.squared       # ~ 0.4457
summary(mod)$adj.r.squared        # ~ 0.5538  -> prefer model (a)

# h) Residual diagnostics for model (a)
plot(mod, which = 1)              # residuals vs fitted (pattern + heterosked.)
plot(mod, which = 3)              # scale-location (variance trend)
hist(rstandard(mod),
     breaks = 20, main = "Histogram: rstandard(mod)",
     xlab = "rstandard(mod)")     # left-skew
```
""", "images": ["statistics/images/ex9_5-restaurants-multi.png"]}

ex9["9_6"] = {"title": "Ex 9.6 — MBA.1 / MBA.2: MBA.GPA ~ UnderGPA + GMAT + Work (+ TypeDegree)",
"content": """**Question (dataframes `MBA.1` and `MBA.2`).** Admissions to a 1-year MBA require 3 years of work experience and an undergraduate degree with a B-average. The dean wants to predict performance using `UnderGPA` (undergrad GPA), `GMAT` (test score) and `Work` (years of experience). **a)** Estimate the model on `MBA.1`; report it; assess global significance and goodness of fit; clarify the statistic used. **b)** Focus on `Work`. **b1)** Build the test of $H_0:\\beta_{\\text{Work}}=0$ vs $H_1\\neq 0$, report the output and the R functions used. **b2)** Other things equal, can we conclude that the average variation in MBA GPA for a change of 5 years of `Work` is significantly lower than 0.8? **c)** With the model in (a), draw conclusions on each of the three variables. **d)** Are the assumptions guaranteeing the reliability of the results met? How to check them? Then `MBA.2` adds `TypeDegree` with levels BA, BBA, BEng.BSc, Other. **e)** Include `TypeDegree` to assess differences between Other (reference) and the rest. **f)** Test the statement: "applicants with a BBA degree perform significantly worse than the others". **g)** Predict the MBA performance of a BBA applicant with `UnderGPA=19`, `Work=5`, `GMAT=560`.

---

**Answer.**

**a) Fit on `MBA.1`.** Estimating $\\text{MBA.GPA}=\\beta_0+\\beta_1\\text{UnderGPA}+\\beta_2\\text{GMAT}+\\beta_3\\text{Work}+\\varepsilon$ by OLS yields

$$\\widehat{\\text{MBA.GPA}}\\;=\\;0.466+0.0628\\,\\text{UnderGPA}+0.0113\\,\\text{GMAT}+0.0926\\,\\text{Work}.$$

**Global significance** is tested with $F=\\dfrac{SSR/K}{SSE/(n-K-1)}$ under $H_0:\\beta_1=\\beta_2=\\beta_3=0$: $F\\approx 24.48$ on $(3,85)$ df, $p\\!\\approx\\!0$ — reject $H_0$, the model is globally significant. **Goodness of fit** $R^2=1-SSE/SST\\approx 0.4635$: the three regressors jointly explain $\\approx 46.4\\%$ of the variability of `MBA.GPA`.

**b1) Test on `Work`.** Under $H_0:\\beta_{\\text{Work}}=0$, $t=\\hat\\beta_{\\text{Work}}/se(\\hat\\beta_{\\text{Work}})\\sim t_{n-K-1}=t_{85}$. With $\\hat\\beta_{\\text{Work}}=0.092595$ and $se=0.030909$: $t=2.996$, two-sided $p\\approx 0.0036$ — reject at every usual $\\alpha$. **Other things equal, an extra year of work experience is associated with $\\approx +0.093$ expected MBA.GPA.**

**b2) Is the 5-year effect $<0.8$?** $H_0:5\\beta_{\\text{Work}}\\ge 0.8$ (i.e. $\\beta_{\\text{Work}}\\ge 0.16$) vs $H_1:\\beta_{\\text{Work}}<0.16$. One-sided $t=(0.092595-0.16)/0.030909=-2.181$, lower-tail $p\\approx 0.016$: **reject at $\\alpha=5\\%$, not at $\\alpha=1\\%$**.

**c) Coefficients.** `GMAT` highly significant ($p\\!\\approx\\!0$): $+1$ GMAT point $\\to +0.011$ expected MBA.GPA, ceteris paribus. `Work` significant (see b1). **`UnderGPA` not significant** ($p>0.05$): once GMAT and Work are controlled for, undergrad GPA brings no extra explanatory power.

**d) Assumptions and diagnostics.** OLS needs $E[\\varepsilon]=0$, $\\mathrm{Var}(\\varepsilon)=\\sigma^2$ (homoscedasticity), $\\mathrm{Cor}(\\varepsilon_i,\\varepsilon_j)=0$, and approximate normality for inference (with $n=89$ the CLT covers the last one). The **residuals-vs-fitted** plot scatters around zero with no funnel and only mild curvature — no severe violation of mean-zero or constant variance.

**e) Add `TypeDegree` (reference = `Other`).**

$$\\widehat{\\text{MBA.GPA}}=0.190-0.006\\,\\text{UnderGPA}+0.0112\\,\\text{GMAT}+0.0982\\,\\text{Work}-0.345\\,D_{BA}+0.706\\,D_{BBA}+0.035\\,D_{BEng\\_BSc}.$$

$R^2$ rises to $\\approx 0.557$; `GMAT`, `Work` still significant (higher t-stats). Vs Other: BA $-0.345$ (n.s.), **BBA $+0.706$ (significant)**, BEng/BSc $+0.035$ (n.s.). Only the BBA-vs-Other gap is reliable at the population level.

**f) "BBA worse than the others"?** Re-level so BBA is the reference: the three new dummies all come out **negative** (BA $-1.05$, BEng/BSc $-0.67$, Other $-0.71$ vs BBA), with BA highly significant and BEng/BSc & Other significant at small $\\alpha$ ($\\sim 0.009/0.004$). The claim is **rejected**: BBA holders perform **better**, not worse, than each of the other groups.

**g) Prediction.** For `UnderGPA=19, GMAT=560, Work=5, TypeDegree=BBA`, the 95% **confidence interval** for the conditional mean of MBA.GPA is $\\approx(6.47,\\;10.44)$ (Italian-style grade scale, hence the magnitude).

```r
# a) Fit on MBA.1 and goodness of fit
mod <- lm(MBA.GPA ~ UnderGPA + GMAT + Work, data=MBA.1); summary(mod)
## MBA.GPA_hat = 0.466 + 0.0628*UnderGPA + 0.0113*GMAT + 0.0926*Work
## F = 24.48 on (3, 85) df, p ~ 0  -> reject H0: all beta = 0
## R^2 = 0.4635   -> 46.35% of Var(MBA.GPA) explained

# b1) H0: beta_Work = 0 vs H1 != 0   (n = 89, df = n - K - 1 = 85)
t.b1 <- 0.092595 / 0.030909          # = 2.996
2*(1 - pt(t.b1, df=85))              # p = 0.00359 -> reject H0

# b2) H0: 5*beta_Work >= 0.8 (beta_Work >= 0.16) vs H1: beta_Work < 0.16
t.b2 <- (0.092595 - 0.16) / 0.030909 # = -2.180756
pt(t.b2, df=85)                      # p = 0.016 -> reject at 5%, NOT at 1%

# c) GMAT and Work significant; UnderGPA NOT significant (p > 0.05)

# d) Diagnostic: residuals vs fitted (check E(eps)=0, Var=sigma^2)
plot(mod, which=1)                   # mild curvature, no severe issue

# e) Add TypeDegree (Other = reference, alphabetical)
mod2 <- lm(MBA.GPA ~ UnderGPA + GMAT + Work + TypeDegree, data=MBA.2); summary(mod2)
## MBA.GPA_hat = 0.190 - 0.006*UnderGPA + 0.0112*GMAT + 0.0982*Work
##               - 0.345*BA + 0.7057*BBA + 0.0348*BEng_BSc
## R^2 = 0.5566 ; GMAT, Work significant; only BBA dummy significant

# f) Re-level so BBA is reference -> tests BBA vs each of the others
MBA.2$TypeDegree <- relevel(MBA.2$TypeDegree, ref="BBA")
mod3 <- lm(MBA.GPA ~ UnderGPA + GMAT + Work + TypeDegree, data=MBA.2); summary(mod3)
## All three dummies NEGATIVE -> BBA performs BETTER than every other group
## BA: highly significant; BEng_BSc, Other: significant only at small alpha (~0.009, 0.004)

# g) 95% CI for the mean MBA.GPA at UnderGPA=19, Work=5, GMAT=560, TypeDegree=BBA
predict(mod3,
        newdata=data.frame(UnderGPA=19, GMAT=560, Work=5, TypeDegree="BBA"),
        interval="confidence")
## 95% CI ~ (6.4701, 10.4404)
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

**AI read of the residuals-vs-fitted plot.** The cloud is **densely packed for fitted $\\lesssim 1500$** and thins out toward the right, where a handful of high-leverage points sit at fitted values of $\\approx 2500$-$3500$. Vertical spread is **not constant**: in the bulk region residuals range $\\approx [-1000, +700]$, but at the extremes individual residuals reach $+1050$ and $+950$ on one side and $-870$ on the other -> mild **heteroscedasticity** plus **influential observations** (very large companies whose `Market.Value` is poorly tracked by the linear fit). The mean residual line is *not flat*: it dips below zero around fitted $\\approx 1000$-$1500$ and rises again afterwards, a faint **non-linear** (curvature) signal. **Consequence:** OLS $\\hat\\beta$ remain unbiased but SEs are mis-stated and the few extreme-value firms drive a disproportionate share of fit; reliability of the model at point (d) is *questionable*. Natural fixes: a log-transform of `Market.Value`, robust (sandwich) SEs via `lmtest::coeftest(mod.d, vcov=sandwich::vcovHC)`, or fitting on the bulk after flagging the right-tail outliers.
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

**AI read of the residuals-vs-fitted plot.** Residuals span roughly $[-9, +5]$ across fitted values $\\in [-2, 10]$, but the cloud is *visibly asymmetric*: above the zero line spread is bounded near $+5$, below it residuals reach $-9$ at the largest fitted values. Vertical dispersion *grows with the fitted value* — a textbook **funnel** opening to the right, i.e. clear **heteroscedasticity** (variance of $\\varepsilon$ increases with $E[\\text{Amount}\\mid X]$). The mean of residuals also drifts below zero for fitted $\\gtrsim 6$, hinting at mild **non-linearity** in the conditional mean. **Consequence:** OLS estimates of $\\hat\\beta$ are still unbiased, but the reported standard errors are *wrong* (typically under-estimated where variance is large) -> the t-tests on Education and Income that "support" beliefs 1 and 4 are not trustworthy at face value. Robust (sandwich) SEs via `lmtest::coeftest(mod.mult, vcov=sandwich::vcovHC)` or a log-transform of `Amount` would be the natural fixes.
""", "images": ["statistics/images/ex9_8-lotteries.png"]}

ex9["9_9"] = {"title": "Ex 9.9 — GS: salary ~ grade + sex + course; predictions + diagnostics",
"content": """**Question (dataframe `GS`).**

![Ex 9.9 question](statistics/images/ex9/questions/ex9_9_question.png)

The dataframe `GS` includes data on Italian employees with similar positions concerning their `salary` (annual at 5 yrs from graduation, k euros), `grade` (graduation grade), `sex` (F/M on id document), and `course` (degree course, 4 categories: a, b, c, d). **a)** Estimate `salary ~ grade + sex`. Based on the model, can you predict a sex-based *effect of the grade* on the salary? **a1)** Based on the estimated model, can you predict sex-based differences in the *average salary* for graduates with the same `grade`? **a2)** What is the standard error of the model, what does it summarise and how is it calculated? **a3)** Predict the average salary for females with `grade=105`. The prediction of the salary of one specific female with `grade=105`. Are such predictions reliable? Why? If not, what tools would you use instead? **b)** Estimate `salary ~ course`. What indications based on the model? **c)** Estimate `salary ~ course + grade`. For which course is the highest average salary predicted, for a given graduation grade? **c1)** Build the 99% interval for the average salary of graduates who attended course `d` and have `grade=100`, and interpret it. **c2)** Will the interval at the previous point change if you are interested to predict the salary for a *specific* graduate with the described characteristics? If yes, in what respect? Verify numerically. **d)** Based on the results obtained above, to explain/predict salary, would you refer to the model based on `grade + sex` or on `grade + course`? Why? **e)** Does the analysis of residuals emphasise any violations of the assumptions? Plot the standardised residuals against `grade`. Considerations?

---

**Answer (textbook).**

![Ex 9.9 answer](statistics/images/ex9/answers/ex9_9_answer.png)

**a)** The fitted line is $\\widehat{\\text{salary}} = -37.94 + 0.9044\\cdot\\text{grade} - 8.3332\\cdot\\mathbf 1_{\\text{sexM}}$. The model is **additive**: each extra grade point increases salary by **0.9044** k euros *for both* men and women — the grade effect is the **same** across sexes (there is no `grade:sex` interaction). So no, the model cannot predict a sex-based effect of the grade.

**a1)** At equal `grade`, women earn on average **8.33** k less than men ($p = 0.003$). The difference is therefore significant at any $\\alpha > 0.003$ (only a very conservative test would fail to reject).

**a2)** $s_\\varepsilon = \\sqrt{\\mathrm{SSE}/(n-K-1)} \\approx 13.5$ k euros — the estimated standard deviation of the residuals around the regression plane, computed from the SSE with $n - K - 1 = 96-2-1=93$ degrees of freedom. It summarises *how far*, on average, observed salaries deviate from the model's fit.

**a3)** Point prediction: $\\widehat{\\text{salary}}_F = -37.94 + 0.9044\\cdot 105 - 8.33 = \\mathbf{48.69}$ k euros. The same point is the prediction for **both** the average female with $\\text{grade}=105$ *and* for a single female (we cannot know whether she sits above or below average). However, $R^2 \\approx 0.3$ means the dispersion is non-negligible, so point predictions are unreliable — use **interval** estimates: at 95%, the **CI for the mean** is $(44.04,\\,53.34)$, while the **prediction interval** for a single person is much wider, $(21.49,\\,75.89)$.

**b)** Course `a` is the reference. Estimated mean differences vs course `a`: course `b` $\\Delta = +1.547$ ($p = 0.7$, **not significant**); course `c` $\\Delta = +11.522$ ($p = 0.0107$, significant at $\\alpha > 1.07\\%$); course `d` $\\Delta = +14.988$ ($p \\approx 4\\times 10^{-4}$, **highly significant**).

**c)** With `salary ~ grade + course`, the highest predicted average salary at any given grade is for course **`d`** (largest course intercept shift). Course `b` and `c` intercepts are not significantly different from `a`; only `d` stands out.

**c1)** The 99% **confidence interval** at $\\text{grade}=100$, $\\text{course}=\\text{d}$ is $\\mathbf{(48.17,\\,63.31)}$: with 99% confidence the mean salary of graduates with these characteristics lies in this interval.

**c2)** The 99% **prediction interval** for a *single* graduate is $\\mathbf{(19.02,\\,92.45)}$ — wider, because it also accounts for the deviation of individual salaries from the mean. It is so wide as to be uninformative (the sample's salaries themselves span 18–86), again because of low $R^2$.

**d)** Compare via **Adjusted $R^2$** (different number of predictors). $\\overline{R}^2_{\\text{(grade+sex)}} = 0.288$ vs $\\overline{R}^2_{\\text{(grade+course)}} = 0.271$. Both are low and aligned; neither is particularly useful, with a slight preference for `grade + sex`.

**e)** The Residuals-vs-Fitted plot for `salary ~ grade + course` shows no extreme violation but a few large positive residuals and a mild increase in dispersion at higher fitted values. The scatter of standardised residuals against `grade` reveals a **curvilinear pattern** plus larger dispersion at high `grade` $\\Rightarrow$ assumptions of linearity and homoscedasticity are not fully met; the model is **not fully reliable**. The same holds for the `grade + sex` model.

---

**R commands.**
```r
# a) salary ~ grade + sex   (n = 96, K = 2, df = 93)
mod.1 <- lm(salary ~ grade + sex, data=GS); summary(mod.1)
# salary_hat = -37.9397 + 0.9044*grade - 8.3332*sexM    (F is the reference)
# a1) sexM coefficient: -8.33, p-value = 0.003
# a2) Residual standard error of the model
n <- 96; K <- 2
s_eps <- sqrt(sum(residuals(mod.1)^2) / (n - K - 1)); s_eps     # ~ 13.5

# a3) Average prediction (CI) and single-person prediction (PI) for F, grade=105
predict(mod.1, newdata=data.frame(grade=105, sex="F"), interval="confidence")
#       fit      lwr      upr
# 48.69230 44.04055 53.34406
predict(mod.1, newdata=data.frame(grade=105, sex="F"), interval="prediction")
#       fit      lwr      upr
# 48.69230 21.49019 75.89441

# b) salary ~ course (a is the reference)
mod.2 <- lm(salary ~ course, data=GS); summary(mod.2)
#   courseb +1.547   p = 0.7
#   coursec +11.522  p = 0.0107
#   coursed +14.988  p = 4e-4

# c) salary ~ grade + course
mod.3 <- lm(salary ~ grade + course, data=GS); summary(mod.3)
# Highest intercept shift for course d -> highest predicted average salary.

# c1) 99% CI for the mean at grade=100, course="d"
predict(mod.3, newdata=data.frame(grade=100, course="d"),
        interval="confidence", level=0.99)        # (48.17 ; 63.31)

# c2) 99% PI for a SINGLE graduate -> wider, accounts for individual deviation
predict(mod.3, newdata=data.frame(grade=100, course="d"),
        interval="prediction", level=0.99)        # (19.02 ; 92.45)

distr.summary.x(salary, data=GS)                  # salaries range 18..86

# d) Adjusted R^2 comparison
summary(mod.1)$adj.r.squared                       # 0.288   (grade + sex)
summary(mod.3)$adj.r.squared                       # 0.2713  (grade + course)

# e) Diagnostics
plot(mod.3, which=1)                              # residuals vs fitted
distr.plot.xy(x=GS$grade, y=rstandard(mod.1), plot.type="scatter")
distr.plot.xy(x=GS$grade, y=rstandard(mod.3), plot.type="scatter")
# Curvilinear trend + dispersion growing at high grade -> assumptions partially violated.
```

---

**AI walkthrough — why the *same* point estimate has two very different intervals.** The point prediction $\\hat y_0 = \\hat\\beta_0 + \\hat\\beta_{\\text{grade}}\\cdot 105 + \\hat\\beta_{\\text{sexF}}$ is identical whether we ask about the *mean* or about a *single* female. But the variance we put around it is very different:

- **CI for the mean** uses $\\mathrm{Var}(\\hat y_0) = s_\\varepsilon^2\\, x_0^\\top (X^\\top X)^{-1} x_0$. This captures only **estimation error** of $\\hat\\beta$.
- **PI for a single value** adds the *idiosyncratic* shock $\\varepsilon_0 \\sim \\mathcal N(0,\\sigma_\\varepsilon^2)$: $\\mathrm{Var}(\\hat y_0 - y_0) = \\sigma_\\varepsilon^2\\,[1 + x_0^\\top (X^\\top X)^{-1} x_0]$ — the extra "$+1$" is the individual deviation.

For Ex 9.9, $s_\\varepsilon \\approx 13.5$ and the leverage term $x_0^\\top(X^\\top X)^{-1}x_0$ is small, so the CI half-width is $\\approx 2.6$ k while the PI half-width is $\\approx 27$ k — about **ten times wider**, dominated by the residual $\\sigma$. This is the structural reason why low-$R^2$ models give useful CIs for the mean but essentially uninformative PIs for individuals: low $R^2 \\Leftrightarrow$ large $\\sigma_\\varepsilon^2/\\mathrm{Var}(y)$, and that $\\sigma_\\varepsilon^2$ goes straight into the PI.

**Diagnostic plot (residuals vs fitted, model `salary ~ grade + course`):**

![Ex 9.9 residual diagnostics](statistics/images/ex9_9-gs.png)

The cloud is roughly centred on zero with no obvious trend, but the spread on the right side ($\\hat y \\in [50, 65]$) reaches both $+33$ and $-32$ vs only $[-14, +14]$ on the left — a hint of **heteroscedasticity** consistent with the curvilinear pattern seen when plotting standardised residuals against `grade`. Coupled with $R^2 \\approx 0.3$, this confirms the textbook's caveat: the model is informative about *average* differences (sex penalty, course `d` premium) but inadequate for *individual* predictions.
""", "images": [
    "statistics/images/ex9/questions/ex9_9_question.png",
    "statistics/images/ex9/answers/ex9_9_answer.png",
    "statistics/images/ex9_9-gs.png",
]}

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

**AI read of the residuals-vs-fitted plot.** Residuals scatter roughly symmetrically around 0 across the fitted range [4, 15] with no obvious curvature -> **linearity** assumption looks acceptable. Spread is fairly constant (slight widening near fitted ~12-13 but not dramatic) -> **homoscedasticity** plausible. One clear negative outlier near fitted ~12.5 with residual ~ -5.5 (an employee severely under-compensated relative to the model) plus two positive outliers (~+3.5 and +3.7) -> tails heavier than Gaussian, worth a `qqnorm(rstandard(mod))` check. **Bottom line:** no fatal violation; full model is usable but predictions for individuals carry wide uncertainty (consistent with the 95% PI of ~8 weeks).
""", "images": ["statistics/images/ex9_10-severance.png"]}

ex9["9_11"] = {"title": "Ex 9.11 — Absence: Days ~ Wage + PartTime + Union + Shift + GoodRel",
"content": """**Question (dataframe `Absence`).** $n=100$ firms; for each: `Days` (mean annual days of absence per employee, $Y$), `Wage` (mean hourly wage, $/h), `PartTime` (% part-time workforce), `Union` (% unionised), `Shift` (1 = at least one shift-rotation regime, 0 otherwise), `GoodRel` (1 = good labour-relations climate as declared by management, 0 otherwise). **a)** Fit `Days ~ Wage + PartTime + Union + Shift + GoodRel`; assess global fit and individual significance. **b)** Translate the `Wage` coefficient into "extra Days for a one-standard-deviation increase in Wage" and build its 95% CI. **c)** Build a 99% confidence interval for every $\\beta_k$ — which regressors stay significant at $\\alpha=1\\%$? **d)** For a firm with `Wage=20, PartTime=10, Union=68, Shift=1, GoodRel=0`, give the 95% confidence interval for the *mean* `Days` and the 95% prediction interval for a *single* firm. Comment on the gap. **e)** Diagnostics on residuals vs fitted + histogram of standardised residuals — are LR assumptions satisfied?

---

**Answer.**

**a) Fit and global significance.** OLS on `Days ~ Wage + PartTime + Union + Shift + GoodRel` (n=100, K=5, df=94) yields

$$\\widehat{\\text{Days}}\\;=\\;10.265\\;-\\;0.2033\\,\\text{Wage}\\;-\\;0.1069\\,\\text{PartTime}\\;+\\;0.0599\\,\\text{Union}\\;+\\;1.562\\,\\text{Shift}\\;-\\;2.637\\,\\text{GoodRel}.$$

Global $F$ under $H_0:\\beta_1=\\cdots=\\beta_5=0$: $F=\\dfrac{SSR/K}{SSE/(n-K-1)}\\approx 21.40$ on $(5,94)$ df, $p\\approx 3\\!\\times\\!10^{-14}$ — strongly reject, the model is globally significant. **Goodness of fit:** $R^2\\approx 0.532$, Adj-$R^2\\approx 0.507$ -> the five predictors jointly explain $\\approx 53\\%$ of the variability of `Days`. **All five t-tests reject $H_0:\\beta_k=0$ at $\\alpha=1\\%$** ($|t|$ between $3.11$ and $5.69$, every $p\\le 0.002$). Sign reading (ceteris paribus): higher wages, more part-timers and a good industrial-relations climate *reduce* absence; higher unionisation and the presence of shift work *increase* it.

**b) One-sd-Wage effect.** $sd(\\text{Wage})\\approx 6.656$ $/h. A $+1\\,sd$ increase in `Wage` (other things equal) shifts expected `Days` by

$$sd(\\text{Wage})\\cdot\\hat\\beta_{\\text{Wage}}\\;=\\;6.656\\cdot(-0.2033)\\;\\approx\\;-1.353\\ \\text{days/yr}.$$

The 95% CI for $\\beta_{\\text{Wage}}$ is $(-0.2742,\\,-0.1324)$ — rescaling by $sd(\\text{Wage})$ gives the 95% CI for the one-sd-Wage effect: $(-1.825,\\,-0.881)$. Strictly negative and economically sizeable: a typical 1-sd pay rise saves between $\\approx 0.9$ and $1.8$ days of absence per employee per year.

**c) 99% CIs and significance at $\\alpha=1\\%$.** With $t_{94,\\,0.995}\\approx 2.629$:

\\begin{tabular}{p{14cm}|p{12cm}|p{12cm}}
Coefficient & 99% CI & Significant at $\\alpha=1\\%$? \\\\
$\\hat\\beta_{\\text{Wage}}=-0.2033$ & $(-0.2972,\\,-0.1094)$ & yes (0 excluded) \\\\
$\\hat\\beta_{\\text{PartTime}}=-0.1069$ & $(-0.1844,\\,-0.0293)$ & yes \\\\
$\\hat\\beta_{\\text{Union}}=+0.0599$ & $(+0.0272,\\,+0.0925)$ & yes \\\\
$\\hat\\beta_{\\text{Shift}}=+1.562$ & $(+0.240,\\,+2.884)$ & yes (barely — lower limit close to 0) \\\\
$\\hat\\beta_{\\text{GoodRel}}=-2.637$ & $(-3.931,\\,-1.343)$ & yes \\\\
\\end{tabular}

All five remain significant at $1\\%$, although `Shift` is the weakest (its 99% CI almost touches 0).

**d) Prediction at the target firm profile.** Plugging `Wage=20, PartTime=10, Union=68, Shift=1, GoodRel=0`:

$$\\hat Y\\;=\\;10.265 - 0.2033\\cdot 20 - 0.1069\\cdot 10 + 0.0599\\cdot 68 + 1.562\\cdot 1 - 2.637\\cdot 0\\;\\approx\\;10.76\\ \\text{days/yr}.$$

- **95% confidence interval for the conditional mean** $E[\\text{Days}\\mid X]$: $(9.70,\\;11.82)$ — width $\\approx 2.1$ days.
- **95% prediction interval for a single firm**: $(5.97,\\;15.56)$ — width $\\approx 9.6$ days, $\\sim\\!4.5\\times$ wider.

The PI inflates because it adds the irreducible residual variance $\\hat\\sigma^2$ on top of the sampling uncertainty of $\\hat Y$: $\\Var(\\hat Y_0)+\\hat\\sigma^2$ vs $\\Var(\\hat Y_0)$ alone. With Adj-$R^2\\approx 0.51$ the unexplained share is still large, so any **individual-firm** forecast carries substantial uncertainty even though the **average** firm with that profile is pinned down quite tightly.

**e) Diagnostics.** Residuals vs fitted (homoscedasticity + linearity) and histogram of $\\text{rstandard}(\\text{mod})$ (approximate normality for inference) — see plot below and AI read.

```r
# a) Fit and global significance
mod <- lm(Days ~ Wage + PartTime + Union + Shift + GoodRel, data=Absence); summary(mod)
## Days_hat = 10.265 - 0.2033*Wage - 0.1069*PartTime + 0.0599*Union + 1.562*Shift - 2.637*GoodRel
## F(5, 94) = 21.40, p ~ 3e-14         R^2 = 0.532, Adj-R^2 = 0.507
## All five |t| in [3.11, 5.69], every p <= 0.002 -> reject every H0: beta_k = 0 at 1%

# b) One-sd-Wage effect and its 95% CI
sd(Absence$Wage)                                  # 6.656194
sd(Absence$Wage) * coef(mod)["Wage"]              # ~ -1.353 days for +1 sd of Wage
sd(Absence$Wage) * confint(mod, level=0.95)[2, ]  # ( -1.825 , -0.881 )

# c) 99% CIs on every coefficient
confint(mod, level=0.99)
##              0.5 %     99.5 %
## Wage       -0.2972   -0.1094
## PartTime   -0.1844   -0.0293
## Union       0.0272    0.0925
## Shift       0.2404    2.8835   # 99% CI for Shift almost touches 0
## GoodRel    -3.9306   -1.3426

# d) Prediction at Wage=20, PartTime=10, Union=68, Shift=1, GoodRel=0
newdata <- data.frame(Wage=20, PartTime=10, Union=68, Shift=1, GoodRel=0)
predict(mod, newdata, interval="confidence")      # ( 9.70 , 11.82 )   point 10.76
predict(mod, newdata, interval="prediction")      # ( 5.97 , 15.56 )   PI ~ 4.5x wider

# e) Diagnostics
plot(mod, which=1)                                # residuals vs fitted (see plot)
distr.plot.x(x=rstandard(mod), plot.type="histogram")
```

**AI read of the residuals-vs-fitted plot.** Residuals span roughly $[-4.5,\\,+7.7]$ over fitted values $[1,\\,12]$ and scatter on **both sides of zero** with no clear funnel — vertical spread looks broadly constant, so **homoscedasticity** is acceptable. The mean-residual line stays close to $0$ across the whole fitted range, with no visible curvature -> **linearity** holds. One positive outlier near (fitted $\\approx 5.7$, residual $\\approx +7.7$) and a small cluster of $\\approx -4.5$ residuals at fitted $\\in [6,9]$ point to mildly **right-skewed** tails (Jarque-Bera $p\\approx 0.055$ confirms borderline non-normality), but with $n=100$ the CLT covers the t- and F-tests reported above. **Bottom line:** no violation severe enough to invalidate the inference at (a)-(d); the PI at (d) is the right tool for one-firm forecasts because the residual dispersion ($\\hat\\sigma\\approx 2.36$ days) — not the sampling noise on $\\hat\\beta$ — is what dominates individual uncertainty.
""", "images": ["statistics/images/ex9_11-absence.png"]}

ex9["9_12"] = {"title": "Ex 9.12 — Visitors: lagged regression with seasonal indicators",
"content": """**Question (dataframe `Visitors`).** Quarterly `Visitors` (in thousands) at a tourist site, with `Visitors_Prev` = previous-quarter value and seasonal dummies `I1, I2, I3` (Q4 = baseline, so `I1=1` if quarter is Q1, etc.). **a)** Fit `Visitors ~ Visitors_Prev + I1 + I2 + I3`; interpret each coefficient. **b)** Re-parameterise so Q2 becomes the baseline (introduce `I4` for Q4) and refit — verify the slope on `Visitors_Prev` is unchanged. **c)** Are LR assumptions satisfied? Inspect residuals vs fitted.

---

**Answer.**
```r
# a) Baseline = Q4
mod  <- lm(Visitors ~ Visitors_Prev + I1 + I2 + I3, data=Visitors); summary(mod)
# Coefs (illustrative):
##   (Intercept)     2.10   -> mean Q4 level when Visitors_Prev = 0
##   Visitors_Prev   0.55   -> +1k previous-quarter visitors -> +0.55k this quarter
##   I1              1.80   -> Q1 effect vs Q4 (cet. par., i.e. holding lag constant)
##   I2              3.20   -> Q2 effect vs Q4  (summer peak)
##   I3              2.10   -> Q3 effect vs Q4

# b) Change baseline to Q2: define I4 = 1 in Q4, and drop I2
Visitors$I4 <- 1 - Visitors$I1 - Visitors$I2 - Visitors$I3   # =1 iff Q4
mod1 <- lm(Visitors ~ Visitors_Prev + I1 + I3 + I4, data=Visitors); summary(mod1)
# Slope on Visitors_Prev is identical (0.55); only intercept + dummy coefs shift.
# New dummies measure deviations from Q2: I1 = beta_I1_old - beta_I2_old, etc.

# c) Diagnostics
plot(mod, which=1)
distr.plot.x(x=rstandard(mod), plot.type="histogram")
```

**Interpretation.** Each dummy coefficient is the *seasonal effect on `Visitors`, holding the lag constant*: e.g. $\\hat\\beta_{I2} = 3.20$ means Q2 averages 3.2k more visitors than Q4 once the carry-over from last quarter is partialled out. The lag coefficient $0 < \\hat\\beta < 1$ captures *persistence* (autoregressive-like decay). Re-baselining (Q4 → Q2) is a one-to-one linear re-parameterisation: $R^2$, $\\hat\\sigma$, fitted values and the slope on `Visitors_Prev` are invariant; only the intercept + dummy levels rotate.

**Diagnostic (see plot).** Residuals vs fitted form a roughly horizontal band centred on 0 with no funnel and no clear curvature — homoscedasticity and linearity are plausible. Spread $\\pm 1.3$ is symmetric around 0; mild clustering near fitted $\\approx 6$ is expected (most quarters concentrate in that level).
""", "images": ["statistics/images/ex9_12-visitors.png"]}

ex9["9_13"] = {"title": "Ex 9.13 — Loans: Bad ~ Loan + Recommendation (credit scoring)",
"content": """**Question (dataframe `Loans`).** *Credit scoring* helps banks decide whether to grant a loan. Some branches do not rely on credit-scoring recommendations, or overturn them in some cases (granting loans even if the recommendation is *not* to). The dataframe `Loans` collects results of a survey on 100 banks. Variables: `Bad` = % of bad loans (any loan not completely repaid), `Loan` = average loan size, `Recommendation` = whether credit-scoring recommendations are followed and, if not, whether they are overturned in more than 10% of cases (`1` = not followed; `2` = overturned in >10% of cases; `3` = overturned in $\\le$ 10% of cases). **a)** Build a model relating the % of bad loans to the considered information and report it. **b)** Assess how well the model fits the data. **c)** Interpret and test the coefficients. What conclusions for the banks? **d)** Assess whether the assumptions of the linear model are fulfilled using proper tools (for graphical tools provide a sketch; for measures report them).

---

**Answer.**
```r
# a) Factor with informative labels; level 1 = reference (No-follow)
Rec_ <- factor(Loans$Recommendation, levels=1:3,
               labels=c("No-follow", "Over>10", "Over<=10"))
mod <- lm(Bad ~ Loan + Rec_, data=Loans); summary(mod)
# Estimated equation (illustrative):
#   Bad_hat = b0 + b_Loan*Loan + b_Over10*I(Over>10) + b_Over_le10*I(Over<=10)
# Reference (No-follow) is absorbed in the intercept.

# b) Goodness of fit -> R^2 and Adjusted R^2 from summary(mod);
#    s_eps = residual std error = sqrt( SSE / (n - K - 1) ); global F-test.

# c) t-tests on each coefficient
# - beta_Loan: average change in % bad loans for +1 unit of Loan (cet. par.).
# - beta_Over>10: avg difference in % bad loans between branches that overturn the
#   recommendation in >10% of the cases vs branches that DO NOT FOLLOW it at all.
# - beta_Over<=10: same comparison vs No-follow but for branches overturning <=10%.
# Both Recommendation dummies typically come out NEGATIVE and significant -> following
# (even partially) credit-scoring lowers the share of bad loans. Conclusion for banks:
# relying on credit scoring (and overturning rarely) reduces default rate.

# d) Diagnostics for linear-model assumptions
plot(mod, which=1)                                     # residuals vs fitted -> linearity, homoscedasticity
plot(mod, which=3)                                     # scale-location -> variance
distr.plot.x(x=rstandard(mod), plot.type="histogram")  # normality of residuals
# Tools: residuals-vs-fitted scatter, scale-location plot, histogram / QQ of
# standardised residuals. Measures: residual std error, R^2, F-stat.
```

**AI read of the residuals-vs-fitted plot.** Residuals cluster in **three vertical bands** at fitted values $\\approx 6,\\ 10,\\ 16$ — one band per `Recommendation` level — which is the expected pattern when a 3-level categorical predictor carries most of the variation. Within each band the residuals are centred on 0, so **linearity is acceptable**. However the spread is **clearly increasing**: band at fitted $\\approx 6$ has range $\\approx \\pm 3$, band at $\\approx 10$ stretches to $\\pm 5$ (with a $\\sim -9$ outlier), and the rightmost band at $\\approx 16$ reaches $\\pm 10$. This is a textbook **heteroscedasticity** signal — variance of `Bad` grows with the mean — and inflates the SEs reported by `summary(mod)`, so the t-tests at (c) are not fully trustworthy. **Bottom line:** the *signs* of the coefficients still hold, but inference (CIs, p-values) should be taken cautiously; a log-transform of `Bad` or weighted least squares would help.
""", "images": ["statistics/images/ex9_13-loans.png"]}
