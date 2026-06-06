"""Ex 7 — Hypothesis tests (one and two-sample), chi-squared, intro to regression."""

ex7 = {}

ex7["7_1a"] = {"title": "Ex 7.1a — Multiple regression: Weeks ~ Age + Educ + Tenure (NewHired)",
"content": """**Question.** Fit a linear regression of `Weeks` on `Age`, `Educ`, `Tenure` using the `NewHired` data (n=47).

---

**Answer.**
```r
mod <- lm(Weeks ~ Age + Educ + Tenure, data=NewHired)
summary(mod)
```

Read the **coefficient estimates**, their **standard errors**, **t-statistics**, **p-values**, and **R²** from `summary(mod)`. The intercept estimates the expected `Weeks` when all predictors are zero; each slope is the change in expected `Weeks` per one-unit increase in the predictor, *holding other predictors constant*.
""", "images": []}

ex7["7_1b"] = {"title": "Ex 7.1b — Hypothesis tests on regression coefficients",
"content": """**Question.** Test $H_0: \\beta_j = 0$ for each coefficient.

---

**Answer.** From the `summary(mod)` output, each coefficient's t-statistic = estimate / SE follows a $t_{n-k-1}$ distribution under $H_0$. The corresponding **p-values** are reported (two-sided). Reject $H_0$ at level $\\alpha$ if p-value < $\\alpha$.

```r
# Individual t-tests are already in summary(mod)
# Manual computation:
bhat <- coef(mod)[2]; se <- summary(mod)$coef[2,2]
tstat <- bhat/se; tstat
2*(1 - pt(abs(tstat), df=47-3-1))  # two-sided p-value
```
""", "images": []}

ex7["7_1c"] = {"title": "Ex 7.1c — CI for regression coefficients (90/95/99%)",
"content": """**Question.** Construct 90%, 95%, 99% CIs for the coefficients. Which contain zero?

---

**Answer.**
```r
confint(mod, level=0.90)
confint(mod, level=0.95)
confint(mod, level=0.99)
```

A coefficient that has p-value > $\\alpha$ in the two-sided t-test corresponds to a CI at level $1-\\alpha$ that **contains zero**. The 99% CI is widest (most likely to contain 0); the 90% is narrowest.
""", "images": []}

ex7["7_3a"] = {"title": "Ex 7.3a — Two-sample t-test: AmountSpent by Sex (DS)",
"content": """**Question.** Test whether mean `AmountSpent` differs by `Sex` in `DS`.

---

**Answer.**
```r
TEST.diffmean(AmountSpent, by=Sex, type="independent", mdiff0=0,
              alternative="two.sided", var.test=TRUE, data=DS)
# Manual computation:
se.diff <- sqrt((909.14^2/389) + (998.41^2/361))
diff.bar <- 1051.91 - 1418.66
(diff.bar - 0) / se.diff
1 - pnorm(13.3, mean=10, sd=se.diff)
1 - pt((13.3 - 10)/se.diff, df=198)
```
""", "images": []}

ex7["7_3b"] = {"title": "Ex 7.3b — Stratified t-tests: female by Location, Children, Married",
"content": """**Question.** Test mean spending difference for female customers, by Location, conditioning on Children=0 and Married=Single.

---

**Answer.**
```r
sel.Close <- DS$Sex=="Female" & DS$Location=="Close"
sel.Far   <- DS$Sex=="Female" & DS$Location=="Far"
TEST.diffmean(x=DS$AmountSpent[sel.Close], y=DS$AmountSpent[sel.Far],
              alternative="less", var.test=T)

sel.Close <- DS$Sex=="Female" & DS$Children==0 & DS$Location=="Close"
sel.Far   <- DS$Sex=="Female" & DS$Children==0 & DS$Location=="Far"
TEST.diffmean(x=DS$AmountSpent[sel.Close], y=DS$AmountSpent[sel.Far],
              alternative="less", var.test=T)

sel.Close <- DS$Sex=="Female" & DS$Married=="Single" & DS$Location=="Close"
sel.Far   <- DS$Sex=="Female" & DS$Married=="Single" & DS$Location=="Far"
TEST.diffmean(x=DS$AmountSpent[sel.Close], y=DS$AmountSpent[sel.Far],
              alternative="less", var.test=T)
```
""", "images": []}

ex7["7_4a"] = {"title": "Ex 7.4a — Two-proportion z-test: Role-Playing share 2006 vs 2016",
"content": """**Question.** Compare the proportion of Role-Playing games sold in 2006 vs 2016 (`vgsales`).

---

**Answer.**
```r
Genre_2006 <- vgsales$Genre[vgsales$Year=="2006"]
Genre_2016 <- vgsales$Genre[vgsales$Year=="2016"]
phat_2006 <- mean(Genre_2006=="Role-Playing")
phat_2016 <- mean(Genre_2016=="Role-Playing")
n_2006 <- length(Genre_2006); n_2016 <- length(Genre_2016)

# Pooled proportion under H0
phat_pooled <- (n_2006*phat_2006 + n_2016*phat_2016) / (n_2006 + n_2016)
se_0 <- sqrt(phat_pooled*(1-phat_pooled) * (1/n_2006 + 1/n_2016))
(phat_2016 - phat_2006) / se_0   # z-statistic
qnorm(0.95)
TEST.diffprop(x=Genre_2006, y=Genre_2016, success.x="Role-Playing",
              pdiff=0, alternative="two.sided", digits=4)
```
""", "images": []}

ex7["7_4b"] = {"title": "Ex 7.4b — CI for proportion difference of Action games",
"content": """**Question.** Construct a 95% CI for the proportion difference of Action games between 2006 and 2016.

---

**Answer.**
```r
TEST.diffprop(Genre_2016, Genre_2006, success.x="Action",
              pdiff=0.10, alternative="greater", digits=4)
```
""", "images": []}

ex7["7_5a"] = {"title": "Ex 7.5a — Chi-squared goodness of fit (DS$History)",
"content": """**Question.** Test whether the distribution of `History` follows a stated set of proportions (0.25, 0.25, 0.25, 0.25).

---

**Answer.**
```r
chisq.test(x=table(DS$History), p=c(0.25, 0.25, 0.25, 0.25))
```

Reject $H_0$ if p-value < $\\alpha$. Confirms whether the proposed proportions are compatible with the observed counts.
""", "images": []}

ex7["7_5b"] = {"title": "Ex 7.5b — Chi-squared independence: History × Age",
"content": """**Question.** Test independence between `History` and `Age` in `DS`.

---

**Answer.**
```r
distr.table.xy(History, Age, freq="counts", freq.type="joint", data=DS)
chisq.test(x=DS$History, y=DS$Age)
```
""", "images": []}

ex7["7_5c"] = {"title": "Ex 7.5c — Fisher's exact test (2×2 tables)",
"content": """**Question.** When is Fisher's exact test preferred to chi-squared?

---

**Answer.** Use **Fisher's exact test** when sample sizes are small or expected frequencies in some cells are <5 (so the chi-squared approximation is unreliable). For larger samples, the chi-squared test is usually adequate.

```r
# Check expected counts; if any are < 5, prefer fisher.test
tab <- table(DS$History, DS$Age)
chisq.test(tab)$expected     # inspect expected cell counts
fisher.test(tab)             # exact test, no large-sample approximation
```
""", "images": []}

ex7["7_6a"] = {"title": "Ex 7.6a — p-value calculation for one-proportion z-test",
"content": """**Question.** Compute the p-value for a one-proportion test ($\\hat p = 26/1000$ vs $p_0 = 0.03$).

---

**Answer.**
```r
phat <- 26/1000
# p-value (one-sided, lower)
pnorm(0.026, mean=0.03, sd=sqrt(0.03*(1-0.03)/1000))
# or
pnorm((0.026-0.03)/sqrt(0.03*(1-0.03)/1000))

# p-value for the (c) variant
1 - pnorm(0.0211, mean=0.02, sd=sqrt(0.02*(1-0.02)/1000))
1 - pnorm((0.0211-0.02)/sqrt(0.02*(1-0.02)/1000))
```
""", "images": []}

ex7["7_6b"] = {"title": "Ex 7.6b — CI vs hypothesis test: containment of test value",
"content": """**Question.** Considering CIs at 90%, 95%, 99%, which will contain the value 0.0065?

---

**Answer.** A two-sided $\\alpha$-level test rejects $H_0: p = p_0$ iff $p_0$ is **outside** the corresponding $(1-\\alpha)$ CI. So whether 0.0065 is in a 95% CI is determined by whether a two-sided test at $\\alpha = 0.05$ would *not* reject $H_0: p = 0.0065$. With $p$-value below 5%, 0.0065 falls outside the 95% CI (and the 90% CI, but inside the 99% CI which is wider).

```r
# Equivalent two-sided test of H0: p = 0.0065 with phat = 26/1000, n = 1000
phat <- 26/1000; p0 <- 0.0065; n <- 1000
z    <- (phat - p0) / sqrt(p0 * (1 - p0) / n)
pval <- 2 * (1 - pnorm(abs(z)))
pval
# Build two-sided CIs at 90%, 95%, 99% and check if 0.0065 is inside
se   <- sqrt(phat * (1 - phat) / n)
for (conf in c(0.90, 0.95, 0.99)) {
  z.c <- qnorm(1 - (1 - conf)/2)
  ci  <- phat + c(-1, 1) * z.c * se
  cat(conf, ":", ci, " contains 0.0065? ", 0.0065 >= ci[1] && 0.0065 <= ci[2], "\n")
}
```
""", "images": []}

ex7["7_7a"] = {"title": "Ex 7.7a — One-proportion test on AI Search adoption",
"content": """**Question.** Test whether the proportion of `AISearch != "None"` developers differs from a stated value.

---

**Answer.**
```r
table(Developers_ITA$AISearch)
UseAI <- 1 - (Developers_ITA$AISearch=="None")
table(UseAI)
TEST.diffprop(prop, alternative="greater")
```
""", "images": []}

ex7["7_7b"] = {"title": "Ex 7.7b — Chi-squared: Age_Class × LearnTool",
"content": """**Question.** Test independence between `Age_Class` and learning tools used.

---

**Answer.**
```r
chisq.test(Developers_ITA$Age_Class, Developers_ITA$LearnTool)
qchisq(0.9, df=16)
```
""", "images": []}

ex7["7_8a"] = {"title": "Ex 7.8a — Subgroup t-test: AmountSpent by Location (Female)",
"content": """**Question.** Among Female customers, test whether mean `AmountSpent` is higher in Close vs Far locations.

---

**Answer.**
```r
sel.Close <- DS$Sex=="Female" & DS$Location=="Close"
sel.Far   <- DS$Sex=="Female" & DS$Location=="Far"
TEST.diffmean(x=DS$AmountSpent[sel.Close],
              y=DS$AmountSpent[sel.Far],
              alternative="greater")
```
""", "images": []}

ex7["7_9a"] = {"title": "Ex 7.9a — Chi-squared goodness of fit (DS$Children)",
"content": """**Question.** Test the distribution of `DS$Children` against the stated probabilities.

---

**Answer.**
```r
distr.table.x(DS$Children)
chisq.test(c(360, 184, 111, 95), p=c(0.76, 0.13, 0.09, 0.02))
```
""", "images": []}

ex7["7_9b"] = {"title": "Ex 7.9b — Chi-squared goodness of fit (DS$Age)",
"content": """**Question.** Test the distribution of `DS$Age` (Young/Middle/Senior) against stated probabilities.

---

**Answer.**
```r
distr.table.x(DS$Age)
chisq.test(c(216, 390, 144), p=c(0.3, 0.5, 0.2))
```
""", "images": []}

ex7["7_10a"] = {"title": "Ex 7.10a — Two-sample t-test on pooled summary stats",
"content": """**Question.** Compute the pooled-variance test statistic for two samples with summary statistics.

---

**Answer.**
```r
xbar <- 1228.44; s2.x <- 940900.9; n.x <- 750
ybar <- 1300;    s2.y <- 960^2;    n.y <- 800
s2.pool <- ((n.x-1)*s2.x + (n.y-1)*s2.y) / (n.x + n.y - 2)
t.stat  <- (xbar - ybar) / sqrt(s2.pool/n.x + s2.pool/n.y)
t.stat
```
""", "images": []}
