"""Ex 5 — Inferential statistics: confidence intervals & first hypothesis tests."""

ex5 = {}

ex5["5_1a"] = {"title": "Ex 5.1a — Point estimate + SE of mean AmountSpent (DS)",
"content": """**Question.** Provide a point estimate and standard error for the mean amount spent in the dataframe `DS`.

---

**Answer.**
```r
distr.summary.x(AmountSpent, digits=3, data=DS)
xbar <- 1228.437
s2_x <- 940900.9
# or
xbar <- mean(DS$AmountSpent); xbar
s2_x <- var(DS$AmountSpent);  s2_x
se_Xbar <- sqrt(s2_x / 750); se_Xbar
```

The standard error $SE(\\bar X) = \\sigma/\\sqrt{n}$ measures the precision of $\\bar X$ as an estimator of the population mean $\\mu$.
""", "images": []}

ex5["5_1b"] = {"title": "Ex 5.1b — Same summaries for the competitor sample",
"content": """**Question.** Repeat the calculation for the competitor company (total = 682 000, $n = 620$, $s^2 = 921\\,486$).

---

**Answer.**
```r
ybar <- 682000/620; ybar
s2_y <- 921486
se_Ybar <- sqrt(s2_y / 620); se_Ybar
```
""", "images": []}

ex5["5_1f"] = {"title": "Ex 5.1f — SE of difference in means: known/unknown/equal variances",
"content": """**Question.** Compute the standard error of $\\bar X - \\bar Y$ under three different assumptions.

---

**Answer.**
```r
xbar - ybar
# SE of Xbar - Ybar: no assumptions on variances (Welch)
sqrt(s2_x/750 + s2_y/620)
# SE of Xbar - Ybar: equal (unknown) variances → pooled
s2_pool <- (749*s2_x + 619*s2_y) / (749 + 619)
sqrt(s2_pool/750 + s2_pool/620)
# SE of Xbar - Ybar: known and equal variances
sqrt(810000/750 + 810000/620)
```

The choice between **Welch** (separate variances) and **pooled** SE matters when the two samples have different variances. With $n_x, n_y$ both large, the difference is small.
""", "images": []}

ex5["5_2a"] = {"title": "Ex 5.2a — Standard error with known population variance",
"content": """**Question.** Compute SE of the sample mean from raw summary stats: $n=15$, $\\sum x = 2755$, $\\sum x^2 = 585\\,203$, known $\\sigma^2 = 6500$.

---

**Answer.**
```r
n <- 15
sum.x <- 2755
sum.x2 <- 585203
xbar <- sum.x/n
sigma2.x <- 6500
SE_Xbar <- sqrt(6500/15); SE_Xbar
# n = 50 case:
SE_Xbar.b <- sqrt(6500/50)
```
""", "images": []}

ex5["5_2b"] = {"title": "Ex 5.2b — SE with unknown population variance",
"content": """**Question.** Same setting, but $\\sigma^2$ is unknown — estimate it from data.

---

**Answer.**
```r
SE_Xbar.b
# Unknown variance — estimate from sample
s2_x <- (1/(n-1)) * (sum.x2 - n*xbar^2); s2_x
se_xbar <- sqrt(s2_x/n); se_xbar
se_xbar.b <- 91.5/sqrt(50)
```
""", "images": []}

ex5["5_3a"] = {"title": "Ex 5.3a — SE of difference in proportions (2-sample)",
"content": """**Question.** Compute the SE of $\\hat p_F - \\hat p_M$ given $\\hat p_F = 391/850$, $\\hat p_M = 221/650$.

---

**Answer.**
```r
p_F <- 391/850; p_M <- 221/650
p_F; p_M
# SE of phat_F - phat_M (separate variances)
sqrt(p_F*(1-p_F)/850 + p_M*(1-p_M)/650)
```
""", "images": []}

ex5["5_3b"] = {"title": "Ex 5.3b — Pooled vs separate SE for proportion comparison",
"content": """**Question.** Compute SE under the pooled-proportion null vs separate-variance form.

---

**Answer.**
```r
# Pooled estimate of the common proportion
p_2022 <- (391+221)/(850+650); p_2022
p_2015 <- 0.45
# SE of phat_2015 - phat_2022 (separate variances)
sqrt(p_2022*(1-p_2022)/1500 + p_2015*(1-p_2015)/1000)
```
""", "images": []}

ex5["5_4"] = {"title": "Ex 5.4 — Mean difference for paired data (pre/post)",
"content": """**Question.** Paired-data study: $\\bar x_{\\text{pre}}=28.3$, $s_{\\text{pre}}=7.5$; $\\bar x_{\\text{post}}=37.5$, $s_{\\text{post}}=8.4$; $\\rho_{pre,post}=0.65$; $n=315$.

---

**Answer.**
```r
xbar_pre <- 28.3;  s_pre  <- 7.5
xbar_post <- 37.5; s_post <- 8.4
cor_pre_post <- 0.65; n <- 315
# Mean difference
xbar_post - xbar_pre
# SE of the difference (paired formula)
(s_pre^2 + s_post^2 - 2*cor_pre_post*s_pre*s_post) / n
```

For paired data, the SE of the difference uses the **covariance** between pre and post — usually MUCH smaller than the independent-samples form (since $\\rho > 0$).
""", "images": []}

ex5["5_5a"] = {"title": "Ex 5.5a — Hypothesis test on difference of proportions",
"content": """**Question.** Test $H_0: p_F = p_M$ vs $H_1: p_F \\ne p_M$ using the data from 5.3a.

---

**Answer.**
```r
p_F <- 391/850; p_M <- 221/650
sqrt(p_F*(1-p_F)/850 + p_M*(1-p_M)/650)
# Test statistic:
zstat <- (p_F - p_M) / sqrt(p_F*(1-p_F)/850 + p_M*(1-p_M)/650)
2*(1 - pnorm(abs(zstat)))   # two-sided p-value
```
""", "images": []}

ex5["5_5b"] = {"title": "Ex 5.5b — Test for proportion change over time (pooled)",
"content": """**Question.** Test if the 2022 sample proportion differs from the 2015 value of 0.45.

---

**Answer.**
```r
p_2022 <- (391+221)/(850+650); p_2022
p_2015 <- 0.45
sqrt(p_2022*(1-p_2022)/1500 + p_2015*(1-p_2015)/1000)
```

Under $H_0: p_{2022} = p_{2015}$, compute the z-statistic and compare to standard-normal quantiles.
""", "images": []}

ex5["5_6a"] = {"title": "Ex 5.6a — Conditional summary of AmountSpent by Sex",
"content": """**Question.** Get the variance of `AmountSpent` for each Sex group in `DS`.

---

**Answer.**
```r
distr.summary.x(x=AmountSpent, by1=Sex, stats="var", digits=3, data=DS)
s2_F <- 826529.2; s2_M <- 996823.7
n_F <- 389;       n_M <- 361
```
""", "images": []}

ex5["5_6b"] = {"title": "Ex 5.6b — Mean spending diff, Sex × Children",
"content": """**Question.** Build the CI for the difference in mean `AmountSpent` between two subgroups defined by `Sex × Children`.

---

**Answer.**
```r
distr.summary.x(x=AmountSpent, by1=Children, by2=Sex, stats=c("mean","var"), data=DS)
n <- 162
xbar_pre <- 1375.99
s2_pre <- 1094690.2
xbar_post <- 1450; s2_post <- 1100^2
cor_pre_post <- 0.58
# SE of (Xbar_post - Xbar_pre)
sqrt(s2_prec + s2_post - 2*0.58*sqrt(s2_prec*s2_post)) / sqrt(n)

# Alternative direct computation
Amount.Pre <- DS$AmountSpent[DS$Sex=="Female" & DS$Children==0]
n <- length(Amount.Pre)
xbar_post <- mean(Amount.Pre)
s2_prec <- var(Amount.Pre)
sqrt(s2_prec + s2_post - 2*0.58*sqrt(s2_prec*s2_post)) / sqrt(n)
```
""", "images": []}

ex5["5_6d"] = {"title": "Ex 5.6d — CI for proportion from Sex × Age table",
"content": """**Question.** Build the CI for a proportion from a two-way categorical table.

---

**Answer.**
```r
distr.table.x(Age, data=DS)
# SE of phat:
sqrt(0.52*(1-0.52)/750)
```
""", "images": []}

ex5["5_7a"] = {"title": "Ex 5.7a — Pooled-variance SE: AmountSpent by Sex",
"content": """**Question.** Compute the pooled-variance SE of $\\bar X_F - \\bar X_M$ from `DS$AmountSpent`.

---

**Answer.**
```r
Amount.F <- DS$AmountSpent[DS$Sex=="Female"]
Amount.M <- DS$AmountSpent[DS$Sex=="Male"]
s2_F <- var(Amount.F); s2_M <- var(Amount.M)
n_F <- length(Amount.F); n_M <- length(Amount.M)
# SE: equal variances
s2_pool <- ((n_F-1)*s2_F + (n_M-1)*s2_M) / (n_F + n_M - 2)
sqrt(s2_pool/n_F + s2_pool/n_M)
# SE: different variances (Welch)
sqrt(s2_F/n_F + s2_M/n_M)
```
""", "images": []}

ex5["5_7b"] = {"title": "Ex 5.7b — Known vs unknown variances for difference",
"content": """**Question.** Compute the SE of $\\bar X_{LE4} - \\bar X_{GT4}$ under (i) known equal variances, (ii) known different variances, (iii) unknown variances (pooled and Welch).

---

**Answer.**
```r
# Known equal variances
sigma2.GT4 <- 2.2; sigma2.LE4 <- 2.2
sqrt(sigma2.GT4/n.GT4 + sigma2.LE4/n.LE4)
# Known different variances
sigma2.GT4 <- 1.7; sigma2.LE4 <- 1.2
sqrt(sigma2.GT4/n.GT4 + sigma2.LE4/n.LE4)
# Unknown variances - separate (Welch)
s2_GT4 <- var(Price.GT4); s2_LE4 <- var(Price.LE4)
sqrt(s2.GT4/n.GT4 + s2.LE4/n.LE4)
# Unknown variances - pooled
s2_pool <- ((n_GT4-1)*s2.GT4 + (n.LE4-1)*s2.LE4) / (n.GT4 + n.LE4 - 2)
sqrt(s2_pool/n.GT4 + s2_pool/n.LE4)
```
""", "images": []}

ex5["5_8a"] = {"title": "Ex 5.8a — Subset means for pizzerie (Employees split)",
"content": """**Question.** Split `pizzerie$Price` by `Employees > 4` vs `Employees ≤ 4` and compute mean difference.

---

**Answer.**
```r
Price.GT4 <- pizzerie$Price[pizzerie$Employees > 4]
Price.LE4 <- pizzerie$Price[pizzerie$Employees <= 4]
mean(Price.GT4)
mean(Price.LE4)
mean(Price.GT4) - mean(Price.LE4)
round(mean(Price.GT4), 4) - round(mean(Price.LE4), 4)
```
""", "images": []}

ex5["5_8b"] = {"title": "Ex 5.8b — Variance comparison in the two pizzeria groups",
"content": """**Question.** Compute sample sizes and run the SE formulas above on `Price.GT4` and `Price.LE4`.

---

**Answer.**
```r
n.GT4 <- length(Price.GT4); n.LE4 <- length(Price.LE4)
n.GT4; n.LE4
# (Then apply the SE formulas from 5.7b with these n values.)
```
""", "images": []}

ex5["5_10b"] = {"title": "Ex 5.10b — SD and SE of profitability (Company)",
"content": """**Question.** Compute the SD of `Profitability` in `Company` and its SE.

---

**Answer.**
```r
sd(Company$Profitability)
sd(Company$Profitability) / sqrt(668)
```
""", "images": []}

ex5["5_13a1"] = {"title": "Ex 5.13 a1 — Milano pizzeria mean + SE",
"content": """**Question.** Compute the mean and SE for `pizzerie$Sales[pizzerie$District=="Milano"]`.

---

**Answer.**
```r
mean(pizzerie$Sales[pizzerie$District=="Milano"])
11500 / sqrt(nrow(pizzerie))
```
""", "images": []}

ex5["5_13a2"] = {"title": "Ex 5.13 a2 — Two-sided tail area near z = ±1",
"content": """**Question.** Compute the tail probability beyond $\\pm 1$.

---

**Answer.**
```r
2*(1 - pnorm(1))
```
""", "images": []}

ex5["5_13a3"] = {"title": "Ex 5.13 a3 — SE of sample mean from `pizzerie$Sales`",
"content": """**Question.** Compute the SE of the sample mean of `pizzerie$Sales`.

---

**Answer.**
```r
var(pizzerie$Sales)
sqrt(var(pizzerie$Sales) / nrow(pizzerie))
```
""", "images": []}

ex5["5_13b"] = {"title": "Ex 5.13b — Two-way table: District × SmokingArea",
"content": """**Question.** Build a contingency table for `District × SmokingArea`.

---

**Answer.**
```r
distr.table.xy(x=District, y=SmokingArea, data=pizzerie)
```
""", "images": []}
