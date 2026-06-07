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
xbar <- sum.x/n; xbar
# known variance:
sigma2_x <- 6500
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

ex5["5_3a"] = {"title": "Ex 5.3a — Mean customers, known variance ($n=15$ vs $n=50$)",
"content": """**Question.** A store manager wants to estimate the mean number of customers $\\mu$ on weekdays. From $n=15$ observations we have $\\sum x_i = 2755$, $\\sum x_i^2 = 585\\,203$. Provide an unbiased estimator, state assumptions, and give the SE assuming $\\sigma^2 = 6500$ is **known**. Then a sample of $n=50$ yields $\\bar x = 205$ and $s = 91.5$ — give the estimate of $\\mu$ and its SE for this larger sample.

---

**Answer.**
```r
# (a) Sample of size n = 15: sample mean is the unbiased estimator (no
#     distributional assumption is needed for unbiasedness)
n <- 15
sum.x  <- 2755
sum.x2 <- 585203
xbar <- sum.x / n; xbar              # 183.667
# Known variance sigma^2 = 6500 -> SE is exact
SE_Xbar <- sqrt(6500 / 15); SE_Xbar  # 20.8167

# (b) Larger sample n = 50, xbar = 205
xbar.b   <- 205
SE_Xbar.b <- sqrt(6500 / 50); SE_Xbar.b  # 11.40175
```

With $\\sigma^2$ known, $SE(\\bar X) = \\sigma/\\sqrt{n}$ is exact. A larger $n$ shrinks the SE by $1/\\sqrt{n}$.
""", "images": []}

ex5["5_3b"] = {"title": "Ex 5.3b — Closeness claim + SE with unknown variance",
"content": """**Question.** **(c)** Comment on the statement: "the estimate obtained in (b) is closer to $\\mu$ than the estimate obtained in (a)" — true or false? **(d)** Re-estimate the SE for both samples ($n=15$ and $n=50$) assuming the variance is now **unknown**.

---

**Answer.**
```r
# (c) FALSE. A smaller SE means the sampling distribution of Xbar is more
#     CONCENTRATED around mu (larger n -> tighter distribution), but it
#     does NOT mean any specific realization xbar is closer to mu than
#     a realization from the smaller sample.

# (d) Unknown variance -> estimate it from the sample, se(Xbar) = sqrt(s^2/n)
# Sample of size n = 15:
n <- 15
s2_x <- (1/(n-1)) * (sum.x2 - n*xbar^2); s2_x   # 5657.238
se_xbar <- sqrt(s2_x / n); se_xbar              # ~19.43

# Sample of size n = 50 (s = 91.5):
se_xbar.b <- 91.5 / sqrt(50); se_xbar.b         # 12.94
```

The $n=50$ variance estimate ($91.5^2 = 8372.25$) is closer to the assumed true $\\sigma^2 = 6500$ than the $n=15$ estimate ($5657.24$) — the larger sample also yields a more reliable variance estimate.
""", "images": []}

ex5["5_4"] = {"title": "Ex 5.4 — Mean difference for paired data (pre/post)",
"content": """**Question.** Supermarket targeted survey of regularly shopping customers. A sample of $n=315$ customers reports online weekly spending pre-covid (2019) and post-covid (2023). Weekly pre-pandemic: $\\bar x_{\\text{pre}} = 28.3$, $s_{\\text{pre}} = 7.5$. Weekly post-pandemic: $\\bar x_{\\text{post}} = 37.5$, $s_{\\text{post}} = 8.4$. Sample correlation $\\rho_{pre,post} = 0.65$. **(a)** Estimator of the increase in mean amount spent online. **(b)** SE of that estimator (paired formula). **(c)** What can be said about the difference between estimate and actual change?

---

**Answer.**
```r
xbar_pre <- 28.3;  s_pre  <- 7.5
xbar_post <- 37.5; s_post <- 8.4
cor_pre_post <- 0.65; n <- 315

# (a) Mean difference (point estimate of the increase)
xbar_post - xbar_pre                # 9.2

# (b) SE of the paired difference: sqrt( (s_pre^2 + s_post^2
#     - 2*rho*s_pre*s_post) / n )
var_diff <- (s_pre^2 + s_post^2 - 2*cor_pre_post*s_pre*s_post) / n
se_diff  <- sqrt(var_diff); se_diff   # ~0.378

# (c) Without further assumptions the deviation of the estimate (9.2)
#     from the true increase cannot be quantified -- only its SE is known.
```

For paired data, the SE of the difference uses the **covariance** between pre and post — usually MUCH smaller than the independent-samples form when $\\rho > 0$.
""", "images": []}

ex5["5_5a"] = {"title": "Ex 5.5a — Difference between male/female proportions (bookstore)",
"content": """**Question.** A bookstore chain surveys 650 male and 850 female customers in 2022 about whether they bought $\\ge 2$ books. Heavy readers: 221 males, 391 females. Estimate the difference between the proportions of male and female heavy readers, and the standard error of the estimator.

---

**Answer.**
```r
# Sample proportions
p_F <- 391/850; p_M <- 221/650
p_F; p_M                       # 0.46, 0.34
# Point estimate of the difference
p_F - p_M                      # 0.12

# SE of phat_F - phat_M (separate variances; true p_F, p_M unknown
# so the SE itself can only be ESTIMATED from the data):
se_diff <- sqrt(p_F*(1-p_F)/850 + p_M*(1-p_M)/650)
se_diff                        # ~0.02525
```

For a CI/test, use $\\hat p_F - \\hat p_M \\pm z_{1-\\alpha/2} \\cdot se$.
""", "images": []}

ex5["5_5b"] = {"title": "Ex 5.5b — Change in proportion 2015 vs 2022",
"content": """**Question.** A previous survey conducted on $n=1000$ customers in **2015** found the proportion of heavy readers to be $p_{2015} = 0.45$. Propose an estimator for the change in the percentage of heavy readers between 2015 and 2022, and evaluate the estimate and its standard error.

---

**Answer.**
```r
# 2022 pooled sample (M + F): 221 + 391 successes out of 650 + 850 = 1500
p_2022 <- (391 + 221) / (850 + 650); p_2022    # 0.408
p_2015 <- 0.45                                  # given, treated as point estimate

# Point estimate of the change p_2015 - p_2022
p_2015 - p_2022                                 # 0.042

# SE of (phat_2015 - phat_2022) -- two independent samples, separate variances
se_change <- sqrt(p_2015*(1-p_2015)/1000 + p_2022*(1-p_2022)/1500)
se_change                                       # ~0.02021
```

Independent-samples SE; same logic as 5.5a but across two years instead of two sexes.
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

ex5["5_6b"] = {"title": "Ex 5.6b — Unbiased estimator: fraction of employees who improved",
"content": """**Question.** Propose an unbiased estimator for the fraction of employees in the company whose productivity improved after the transition ($\\text{Post} - \\text{Pre} > 0$). State the required assumptions.

---

**Answer.** Define $Y_i = \\mathbb{1}\\{\\text{Post}_i - \\text{Pre}_i > 0\\}$ for each sampled employee. Then $Y_i \\sim \\text{Bernoulli}(p)$ where $p$ is the fraction of the population that improved, and the sample proportion $\\hat p = \\bar Y$ is unbiased for $p$.

$$\\hat p = \\frac{1}{n}\\sum_{i=1}^{n} \\mathbb{1}\\{\\text{Post}_i - \\text{Pre}_i > 0\\}$$

**Assumptions.** The $n$ employees are sampled independently and the sample is representative of the company population (i.i.d. Bernoulli draws).

```r
# Using the Transition dataframe
Diff <- Transition$Post - Transition$Pre
Y    <- as.numeric(Diff > 0)
phat <- mean(Y); phat
n    <- length(Y); n
# SE of phat
se_phat <- sqrt(phat*(1 - phat) / n); se_phat
```
""", "images": []}

ex5["5_6d"] = {"title": "Ex 5.6d — SE of mean Post$-$Pre difference with cor = 0.58",
"content": """**Question.** Estimate the standard error of the estimator $\\bar D = \\bar X_{\\text{Post}} - \\bar X_{\\text{Pre}}$ under the assumption that the variances of Pre and Post productivity are equal and that $\\operatorname{cor}(\\text{Pre},\\text{Post}) = 0.58$. Use the subsample of employees with at least 2 children, 2015–2022, from `Exe5_Data.Rdata`.

---

**Answer.** Because Pre and Post are measured on the **same** $n$ employees, $\\bar X_{\\text{Post}} - \\bar X_{\\text{Pre}} = \\bar D$ with $D_i = \\text{Post}_i - \\text{Pre}_i$ and

$$\\Var(\\bar D) = \\frac{\\sigma^2_{\\text{Post}} + \\sigma^2_{\\text{Pre}} - 2\\,\\rho\\,\\sigma_{\\text{Post}}\\sigma_{\\text{Pre}}}{n}.$$

With equal variances $\\sigma^2_{\\text{Post}} = \\sigma^2_{\\text{Pre}} = \\sigma^2$ this collapses to $\\Var(\\bar D) = \\frac{2\\sigma^2(1-\\rho)}{n}$.

```r
# Restrict to the requested subsample
sub  <- subset(Transition,
               Children >= 2 & Year >= 2015 & Year <= 2022)
xPre  <- sub$Pre;  xPost <- sub$Post
n     <- length(xPre); n
s2    <- var(c(xPre, xPost))      # pooled sample variance estimate
rho   <- 0.58
# SE of (Xbar_Post - Xbar_Pre) under equal variances + cor = 0.58
se_D  <- sqrt(2 * s2 * (1 - rho) / n); se_D

# Alternative: compute on the paired differences directly
D     <- xPost - xPre
se_D2 <- sd(D) / sqrt(n); se_D2
```
""", "images": []}

ex5["5_7a"] = {"title": "Ex 5.7a — Pizzeria price difference, known equal variances",
"content": """**Question.** Pizzerias are split by workforce size: **LE4** = at most 4 employees, **GT4** = more than 4 employees. Independent random samples yield $n_{\\text{LE4}} = 55$ and $n_{\\text{GT4}} = 45$. Assume that the population variances of `Price` are **known and equal** to $\\sigma^2 = 2.2$. Estimate the standard error of the estimator $\\bar X_{\\text{GT4}} - \\bar X_{\\text{LE4}}$.

---

**Answer.** With independent samples and a common known variance $\\sigma^2$,

$$\\operatorname{SE}\\!\\bigl(\\bar X_{\\text{GT4}} - \\bar X_{\\text{LE4}}\\bigr) \\;=\\; \\sqrt{\\sigma^2\\!\\left(\\frac{1}{n_{\\text{GT4}}} + \\frac{1}{n_{\\text{LE4}}}\\right)} \\;=\\; \\sqrt{\\frac{2.2}{45} + \\frac{2.2}{55}} \\;\\approx\\; 0.2981.$$

```r
sigma2  <- 2.2
n_GT4   <- 45
n_LE4   <- 55
se_diff_known_eq <- sqrt(sigma2/n_GT4 + sigma2/n_LE4)
se_diff_known_eq
## [1] 0.2981
```

**Assumptions.** Prices within each group are i.i.d.; the two samples are independent; the common variance $\\sigma^2 = 2.2$ is treated as known.
""", "images": []}

ex5["5_7b"] = {"title": "Ex 5.7b — Pizzeria price difference: known unequal & unknown variances",
"content": """**Question.** Same setup as 5.7a ($n_{\\text{LE4}} = 55$, $n_{\\text{GT4}} = 45$). Estimate the standard error of $\\bar X_{\\text{GT4}} - \\bar X_{\\text{LE4}}$ in three more scenarios: (i) **known unequal** variances $\\sigma^2_{\\text{LE4}} = 1.2$, $\\sigma^2_{\\text{GT4}} = 1.7$; (ii) **unknown** variances that are **not** assumed equal (Welch — sample variances $s^2_{\\text{LE4}} = 1.6$, $s^2_{\\text{GT4}} = 1.7$); (iii) **unknown** variances that **are** assumed equal (pooled variance).

---

**Answer.**

**(i) Known, unequal variances.**

$$\\operatorname{SE} = \\sqrt{\\frac{\\sigma^2_{\\text{GT4}}}{n_{\\text{GT4}}} + \\frac{\\sigma^2_{\\text{LE4}}}{n_{\\text{LE4}}}} = \\sqrt{\\frac{1.7}{45} + \\frac{1.2}{55}} \\approx 0.2441.$$

**(ii) Unknown variances, NOT assumed equal (Welch).**

$$\\widehat{\\operatorname{SE}} = \\sqrt{\\frac{s^2_{\\text{GT4}}}{n_{\\text{GT4}}} + \\frac{s^2_{\\text{LE4}}}{n_{\\text{LE4}}}} = \\sqrt{\\frac{1.7}{45} + \\frac{1.6}{55}} \\approx 0.2586.$$

**(iii) Unknown variances, ASSUMED equal (pooled).** The pooled variance is

$$s^2_{\\text{pool}} = \\frac{(n_{\\text{GT4}}-1)\\,s^2_{\\text{GT4}} + (n_{\\text{LE4}}-1)\\,s^2_{\\text{LE4}}}{n_{\\text{GT4}} + n_{\\text{LE4}} - 2} = \\frac{44\\cdot 1.7 + 54\\cdot 1.6}{98} \\approx 1.6449,$$

and

$$\\widehat{\\operatorname{SE}} = \\sqrt{s^2_{\\text{pool}}\\!\\left(\\frac{1}{n_{\\text{GT4}}} + \\frac{1}{n_{\\text{LE4}}}\\right)} \\approx 0.2578.$$

```r
n_GT4 <- 45; n_LE4 <- 55

# (i) Known, unequal variances
sigma2_GT4 <- 1.7; sigma2_LE4 <- 1.2
se_known_uneq <- sqrt(sigma2_GT4/n_GT4 + sigma2_LE4/n_LE4); se_known_uneq
## [1] 0.2441

# (ii) Unknown variances - Welch (separate sample variances)
s2_GT4 <- 1.7; s2_LE4 <- 1.6
se_welch <- sqrt(s2_GT4/n_GT4 + s2_LE4/n_LE4); se_welch
## [1] 0.2586

# (iii) Unknown variances - pooled
s2_pool <- ((n_GT4-1)*s2_GT4 + (n_LE4-1)*s2_LE4) / (n_GT4 + n_LE4 - 2)
s2_pool
se_pool  <- sqrt(s2_pool * (1/n_GT4 + 1/n_LE4)); se_pool
```

**Take-away.** When sample variances are similar, Welch and pooled estimators give nearly identical SEs; pooled is more efficient if the equal-variance assumption holds, Welch is safer otherwise.
""", "images": []}

ex5["5_8a"] = {"title": "Ex 5.8a — Unbiased estimator of mean profitability ($Company$)",
"content": """**Question.** Let $X$ be the random variable describing the profitability of clients in the population (`Company` dataframe). Propose an unbiased estimator of the population mean $\\mu_X$ and prove unbiasedness.

---

**Answer.** The natural estimator is the **sample mean**

$$\\bar X = \\frac{1}{n}\\sum_{i=1}^{n} X_i,$$

with $X_1,\\dots,X_n$ an i.i.d. sample from the same distribution as $X$. Unbiasedness follows from linearity of expectation:

$$\\Exp{\\bar X} = \\frac{1}{n}\\sum_{i=1}^{n} \\Exp{X_i} = \\frac{1}{n}\\,n\\,\\mu_X = \\mu_X,$$

so $\\bar X$ is unbiased for $\\mu_X$ for **any** sample size $n$.

```r
# Sample mean as estimator of mu_X
xbar <- mean(Company$Profitability); xbar
n    <- length(Company$Profitability); n
```

**Assumptions.** The clients in the dataframe form an i.i.d. sample from the population of all clients (representative sampling).
""", "images": []}

ex5["5_8b"] = {"title": "Ex 5.8b — Estimated SE and point estimate of $\\mu_X$ (Company)",
"content": """**Question.** The standard error of $\\bar X$ cannot be computed exactly because the population standard deviation $\\sigma_X$ is unknown. Obtain an estimate of $\\operatorname{SE}(\\bar X)$ from the sample, report the observed point estimate of $\\mu_X$, and comment on what we can (and cannot) say about how close the realized $\\bar x$ is to $\\mu_X$.

---

**Answer.** Because $\\sigma_X$ is unknown, plug in the sample standard deviation $s_X$:

$$\\widehat{\\operatorname{SE}}(\\bar X) = \\frac{s_X}{\\sqrt{n}} = \\frac{365.3158}{\\sqrt{668}} \\approx 14.13.$$

The point estimate from the sample is $\\bar x = 930.27$.

```r
n    <- nrow(Company)               # 668
xbar <- mean(Company$Profitability); xbar   # 930.27
s_x  <- sd(Company$Profitability);   s_x    # 365.3158
se_hat <- s_x / sqrt(n); se_hat              # 14.13
```

**Interpretation.** The SE quantifies the **typical** sampling variability of $\\bar X$ across hypothetical resamples — it does **not** tell us how far the realized $\\bar x = 930.27$ is from the unknown $\\mu_X$ for this particular sample. To bound that distance probabilistically, we need a confidence interval (which uses $\\bar x$, $\\widehat{\\operatorname{SE}}$ and a normal/$t$ quantile).
""", "images": []}

ex5["5_10b"] = {"title": "Ex 5.10b — SE of mean profitability + interpretation",
"content": """**Question.** (b) For the dataframe `Company` (sample of $n=668$ clients), the standard error of the estimator of the mean profitability $\\mu_X$ cannot be obtained exactly, because the population's standard deviation is unknown. Obtain an estimate of $SE(\\bar X)$ as the ratio between the sample standard deviation and $\\sqrt{n}$. (c) Report the point estimate $\\bar X$ from the available sample. (d) Can we draw conclusions about how far this specific estimate lies from $\\mu_X$?

---

**Answer.**

**(b)** The SE is estimated by
$$
se(\\bar X) \\;=\\; \\frac{s_x}{\\sqrt{n}} \\;=\\; \\frac{365.3158}{\\sqrt{668}} \\;=\\; 14.13.
$$

```r
sd(Company$Profitability)
sd(Company$Profitability) / sqrt(668)
```

**(c)** The point estimate of $\\mu_X$ based on the available sample is
$$
\\bar X \\;=\\; 930.27,
$$
obtained with `mean(Company$Profitability)`.

**(d)** It is **not** possible to draw conclusions about this specific estimate from the population's characteristics. The SE only quantifies the dispersion of estimates produced by *generic* random samples of size $668$ around $\\mu_X$; it does **not** tell us the deviation of the *specific* available estimate, because that deviation $|\\bar x - \\mu_X|$ depends on the unknown parameter $\\mu_X$ and is therefore unknown.
""", "images": []}

ex5["5_13a1"] = {"title": "Ex 5.13 a1 — Unbiased estimator of mean monthly turnover (Milano)",
"content": """**Question.** (dataframe `pizzerie`). Assume the monthly turnover `Sales` of pizzerias in Milan has known standard deviation $\\sigma = €11\\,500$. Denote by $\\mu$ the mean monthly turnover in the population of Milan pizzerias. **(a1)** Propose an unbiased estimator for $\\mu$, justify why it is unbiased (giving the definition of the property), and compute the estimate of $\\mu$ obtained from the sample in `pizzerie`.

---

**Answer.**

An **unbiased** estimator for $\\mu$ is the **sample mean**
$$
\\bar X \\;=\\; \\frac{1}{n}\\sum_{i=1}^n X_i.
$$
An estimator $\\hat\\theta$ is *unbiased* for $\\theta$ iff $\\E[\\hat\\theta] = \\theta$ for every value of $\\theta$. Since $\\E[\\bar X] = \\mu$ for any distribution of $X$ and any sample size, $\\bar X$ is unbiased for $\\mu$.

The point estimate obtained from the Milan sub-sample is computed in R as:

```r
mean(pizzerie$Sales[pizzerie$District=="Milano"])
# SE with known sigma:
11500 / sqrt(nrow(pizzerie))
```

The SE is $\\sigma/\\sqrt{n} = 11500/\\sqrt{n}$, since $\\sigma$ is known.
""", "images": []}

ex5["5_13a2"] = {"title": "Ex 5.13 a2 — P(|estimate − μ| > SE) under Normality",
"content": """**Question.** **(a2)** Is it possible to evaluate the probability that the distance (the absolute deviation) of our generic estimator from $\\mu$ is greater than the standard error? Specify whether specific assumptions are needed and, if so, provide the answer under those assumptions.

---

**Answer.**

We need to compute $\\Prob{|\\bar X - \\mu| > SE(\\bar X)}$. Standardising,
$$
Z \\;=\\; \\frac{\\bar X - \\mu}{SE(\\bar X)},
$$
so the event becomes $\\{|Z|>1\\}$.

To evaluate this probability we need the **distribution** of $Z$. With $\\sigma$ known and assuming $X \\sim N(\\mu, \\sigma^2)$ (or invoking the **CLT** for large $n$), $Z \\sim N(0,1)$. Then:
$$
\\Prob{|Z|>1} \\;=\\; 2\\bigl(1 - \\Phi(1)\\bigr) \\;\\approx\\; 2(1-0.8413) \\;\\approx\\; 0.3173.
$$

```r
2*(1 - pnorm(1))
# ~ 0.3173
```

Without the Normality / CLT assumption, the probability cannot be computed exactly — Chebyshev only gives the loose bound $\\Prob{|Z|>1} \\le 1$.
""", "images": []}

ex5["5_13a3"] = {"title": "Ex 5.13 a3 — Proportion of no-smoking pizzerias + reliability",
"content": """**Question.** **(a3)** Estimate the proportion of pizzerias in Milan in which smoking is **not** allowed (`SmokingArea = No`), specifying whether and what assumptions are needed to answer the question. What measures would you use to effectively communicate the *reliability* of the estimate?

---

**Answer.**

Let $p$ be the population proportion of Milan pizzerias with `SmokingArea = No`. Modelling each pizzeria as an i.i.d. Bernoulli($p$) draw, the **sample proportion**
$$
\\hat p \\;=\\; \\frac{1}{n}\\sum_{i=1}^n \\mathbb{1}\\{\\text{SmokingArea}_i = \\text{No}\\}
$$
is unbiased: $\\E[\\hat p] = p$. Its standard error, since $\\Var(\\hat p) = p(1-p)/n$, is estimated by
$$
se(\\hat p) \\;=\\; \\sqrt{\\dfrac{\\hat p (1-\\hat p)}{n}}.
$$

```r
# Restrict to Milan pizzerias
smoke_Mi <- pizzerie$SmokingArea[pizzerie$District=="Milano"]
n_Mi  <- length(smoke_Mi); n_Mi
phat  <- mean(smoke_Mi == "No"); phat
# Estimated SE of phat
se_phat <- sqrt(phat*(1-phat) / n_Mi); se_phat
# Approximate 95% CI (CLT-based)
phat + c(-1, 1) * qnorm(0.975) * se_phat
```

**Reliability** of the estimate is best communicated through (i) the standard error $se(\\hat p)$, and (ii) an approximate **confidence interval** $\\hat p \\pm z_{1-\\alpha/2}\\, se(\\hat p)$, valid under the CLT when $n\\hat p \\ge 5$ and $n(1-\\hat p) \\ge 5$.
""", "images": []}

ex5["5_13b"] = {"title": "Ex 5.13b — Difference of proportions: Milano vs Pavia",
"content": """**Question.** **(b)** Estimate the **difference** between the proportion of pizzerias in which smoking is not allowed (`SmokingArea = No`) in **Milano** vs **Pavia**. Specify whether any assumptions are needed.

---

**Answer.**

Let $p_{\\text{Mi}}$ and $p_{\\text{Pa}}$ be the population proportions of no-smoking pizzerias in Milan and Pavia. With two **independent** samples of size $n_{\\text{Mi}}$ and $n_{\\text{Pa}}$, an unbiased estimator of $p_{\\text{Mi}} - p_{\\text{Pa}}$ is the difference of sample proportions
$$
\\hat p_{\\text{Mi}} - \\hat p_{\\text{Pa}}.
$$
By independence,
$$
se\\!\\left(\\hat p_{\\text{Mi}} - \\hat p_{\\text{Pa}}\\right)
\\;=\\;
\\sqrt{\\dfrac{\\hat p_{\\text{Mi}}(1-\\hat p_{\\text{Mi}})}{n_{\\text{Mi}}}
       + \\dfrac{\\hat p_{\\text{Pa}}(1-\\hat p_{\\text{Pa}})}{n_{\\text{Pa}}}}.
$$

Build the contingency table to read off counts:

```r
distr.table.xy(x=District, y=SmokingArea, data=pizzerie)
# Then, with the cell counts:
# phat_Mi <- (No in Milano) / n_Mi
# phat_Pa <- (No in Pavia)  / n_Pa
# diff    <- phat_Mi - phat_Pa
# se_diff <- sqrt(phat_Mi*(1-phat_Mi)/n_Mi + phat_Pa*(1-phat_Pa)/n_Pa)
```

**Assumptions:** the two samples are independent random samples; each district's sample is i.i.d. Bernoulli. For inference (CI / z-test on the difference) we additionally invoke the **CLT**, valid when both $n_d \\hat p_d \\ge 5$ and $n_d(1-\\hat p_d) \\ge 5$ for $d \\in \\{\\text{Mi}, \\text{Pa}\\}$.
""", "images": []}
