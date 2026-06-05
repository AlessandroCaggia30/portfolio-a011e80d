"""Ex 8 — Simple linear regression."""

ex8 = {}

ex8["8_1a"] = {"title": "Ex 8.1a — Simple regression Debt on Television + summary",
"content": """**Question.** Fit a simple linear regression of `Debt` on `Television` in `TeleDebt`.

---

**Answer.**
```r
mod <- lm(Debt ~ Television, data=TeleDebt)
summary(mod)
distr.summary.x(Television, stats="summary", data=TeleDebt)
```
""", "images": []}

ex8["8_1b"] = {"title": "Ex 8.1b — Prediction and confidence intervals at Television=33",
"content": """**Question.** Build prediction and confidence intervals at `Television = 33`.

---

**Answer.**
```r
predict(mod, newdata=data.frame(Television=33),
        interval="prediction", level=0.99)
predict(mod, newdata=data.frame(Television=33),
        interval="confidence", level=0.99)
```

**Prediction interval** is wider (predicts a single new observation including its noise) than the **confidence interval** for $E(Y \\mid X=33)$.
""", "images": []}

ex8["8_1c"] = {"title": "Ex 8.1c — Manual computation of regression quantities",
"content": """**Question.** From summary stats, compute SSE, $s^2_\\epsilon$, SE($\\hat\\beta_1$), t-stat, and 95% CI.

---

**Answer.**
```r
# Given R^2 = 0.7784 (example), n = 47
SSE <- (1 - 0.7784) * (47 - 1) * s2_y
s2_epsilon <- SSE / 45
se_beta1   <- sqrt(s2_epsilon / (46 * s2_x))
# t-statistic for beta1
tstat <- -99.7471 / 7.933
# p-value
2*(1 - pt(abs(tstat), df=45))
```
""", "images": []}

ex8["8_2a"] = {"title": "Ex 8.2a — AmountSpent on Salary (DS) + residual plot",
"content": """**Question.** Fit `lm(AmountSpent ~ Salary, data=DS)` and look at its residual plot.

---

**Answer.**
```r
mod <- lm(AmountSpent ~ Salary, data=DS)
summary(mod)
plot(mod, which=1)   # residuals vs fitted
```

A pattern in residuals indicates **heteroscedasticity** or **non-linearity** — both violate assumptions of OLS.
""", "images": []}

ex8["8_2b"] = {"title": "Ex 8.2b — AmountSpent on Catalogs + scatter",
"content": """**Question.** Compare a second regression using `Catalogs` as predictor.

---

**Answer.**
```r
plot(DS$Catalogs, DS$AmountSpent)
mod1 <- lm(AmountSpent ~ Catalogs, data=DS)
summary(mod1)
plot(mod1, which=1)
```
""", "images": []}

ex8["8_3a"] = {"title": "Ex 8.3a — Weeks on Age (NewHired) + CI + prediction",
"content": """**Question.** Regress `Weeks` on `Age` in `NewHired`; build CI for slope; predict at Age=36; plot diagnostics.

---

**Answer.**
```r
mod <- lm(Weeks ~ Age, data=NewHired)
summary(mod)
confint(mod, level=0.9)            # 90% CI for slope
predict(mod, newdata=data.frame(Age=36),
        interval="prediction", level=0.99)
distr.plot.xy(x=Age, y=Weeks, plot.type="scatter",
              fitline=T, data=NewHired)
plot(mod, which=1)   # residuals vs fitted
plot(mod, which=3)   # scale-location
distr.plot.x(x=rstandard(mod), plot.type="histogram")
```
""", "images": []}

ex8["8_4a"] = {"title": "Ex 8.4a — Restaurants: revenues ~ surface + diagnostics",
"content": """**Question.** Fit `lm(revenues ~ surface, data=restaurants)`; rescale; check heteroscedasticity by evening-only.

---

**Answer.**
```r
mod  <- lm(revenues ~ surface, data=restaurants)
summary(mod)
# Multiply revenues by 1000 → same fit but rescaled coefs
mod1000 <- lm(revenues*1000 ~ surface, data=restaurants)
summary(mod1000)
distr.plot.xy(x=surface, y=revenues, plot.type="scatter",
              fitline=T, data=restaurants)
plot(mod, which=1)
distr.plot.xy(x=restaurants$evening_only, y=rstandard(mod),
              plot.type="boxplot")
```
""", "images": []}

ex8["8_5a"] = {"title": "Ex 8.5a — Manual regression from summary stats",
"content": """**Question.** Given $\\sum y = 99150$, $s^2_y = 345722$, $\\sum x = 297$, $s^2_x = 27.048$, $s_{xy} = 2697.96$, compute $\\hat\\beta_1$, $\\hat\\beta_0$, $R^2$.

---

**Answer.**
```r
sum.y <- 99150; s2_y <- 345722
sum.x <- 297;   s2_x <- 27.048
s_xy  <- 2697.96
b1 <- s_xy / s2_x; b1
xbar <- sum.x/47; ybar <- sum.y/47
b0   <- ybar - b1*xbar; b0
R2   <- s_xy^2 / (s2_x*s2_y); R2
```
""", "images": []}

ex8["8_8a"] = {"title": "Ex 8.8a — Regression from population covariance/correlation",
"content": """**Question.** From $n=36$, $\\bar x=50.5$, $\\bar y=12.4$, $\\text{var}(x)=660$, $\\text{cor}(x,y)=0.8$, compute $\\hat\\beta_1, \\hat\\beta_0, R^2, \\text{SE}(\\hat\\beta_1)$, then build a CI and predict at $x=48$.

---

**Answer.**
```r
n <- 36
xbar <- 50.5; ybar <- 12.4
var.x <- 660; var.y <- 3.5    # corrected sign
cor.xy <- 0.8
COV.xy <- cor.xy*sqrt(var.x*var.y)
b1 <- COV.xy/var.x
b0 <- ybar - b1*xbar
R2 <- cor.xy^2
SSE <- (1-R2)*var.y*(n-1)
s2_e <- SSE/(n-2)
s2_beta1 <- (SSE/(n-2))/((n-1)*var.x)
se_beta1 <- sqrt(s2_beta1)
b1/se_beta1                                # t-stat
2*(1 - pt(b1/se_beta1, df=n-2))            # p-value

x_g <- 48
yhat_g <- b0 + b1*x_g
se_yhat_g <- sqrt(s2_e) * sqrt(1 + 1/n + ((x_g - xbar)^2)/((n-1)*var.x))
MEg <- se_yhat_g * qt(0.995, df=n-2)
c(yhat_g - MEg, yhat_g + MEg)
```
""", "images": []}

ex8["8_10a"] = {"title": "Ex 8.10a — Margin of error in regression predictions",
"content": """**Question.** From summary stats $n=50$, $\\sum x=500$, $\\sum y=3000$, $\\sum (y - \\bar y)^2 = 40000$, $\\sum (x - \\bar x)^2 = 25000$, $SSR = 36000$, compute regression quantities, CI for the slope, and prediction intervals at $x = 12$ and $x = 30$.

---

**Answer.**
```r
n <- 50; sum.x <- 500; sum.y <- 3000
sum.dev2.y <- 40000; sum.dev2.x <- 25000
SSR <- 36000

var.y <- sum.dev2.y/(n-1); var.x <- sum.dev2.x/(n-1)
R2 <- SSR/sum.dev2.y
cov.xy <- sqrt(R2*var.y*var.x)
b1 <- cov.xy/var.x
b0 <- (sum.y/n) - b1*(sum.x/n)
SSE <- sum.dev2.y - SSR
s2_epsilon <- SSE/(n-2)
se_hat1 <- s2_epsilon/sum.dev2.x

ME <- sqrt(se_hat1) * qt(0.975, df=48)
c(b1 - ME, b1 + ME)

# Prediction at x=12 and x=30
yhat <- b0 + b1*12
ME_c <- qt(0.975, df=48)*sqrt(s2_epsilon)*sqrt( 1/50    + ((12-xbar)^2)/sum.dev2.x )
ME_p <- qt(0.975, df=48)*sqrt(s2_epsilon)*sqrt( 1 + 1/50+ ((12-xbar)^2)/sum.dev2.x )
c(yhat - ME_c, yhat + ME_c)
c(yhat - ME_p, yhat + ME_p)
```
""", "images": []}
