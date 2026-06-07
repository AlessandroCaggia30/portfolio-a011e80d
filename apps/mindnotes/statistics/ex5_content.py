"""Ex 5 — Inferential statistics: confidence intervals & first hypothesis tests."""

ex5 = {}

ex5["5_1a"] = {"title": "Ex 5.1a — Point estimate + SE of mean AmountSpent (DS)",
"content": """**Question.** (dataframe `DS`, $n_X = 750$). Let $X$ be the random variable describing the amount spent by the firm's customers (`AmountSpent`) and let $\\mu_X = \\Exp{X}$. **(a)** What is the estimate of the mean amount spent by the firm's customers? Can the standard error of the estimator be determined exactly? Can it be estimated? Determine or estimate the standard error of the estimator. Round all intermediate results to 3 decimals.

---

**Answer.**

**Point estimator.** For an i.i.d. sample $X_1,\\ldots,X_{n_X}$, the unbiased point estimator of $\\mu_X$ is the **sample mean**
$$
\\bar X \\;=\\; \\frac{1}{n_X}\\sum_{i=1}^{n_X} X_i, \\qquad \\Exp{\\bar X} \\;=\\; \\mu_X.
$$
From the available sample, the observed point estimate is
$$
\\bar x \\;=\\; 1222.437.
$$

**Standard error — can it be determined exactly?** **No.** The exact standard error
$$
SE(\\bar X) \\;=\\; \\frac{\\sigma_X}{\\sqrt{n_X}}
$$
depends on the population standard deviation $\\sigma_X$, which is **unknown**.

**Can it be estimated?** **Yes.** Plug in the sample variance $s^2_X$ for $\\sigma^2_X$:
$$
se(\\bar X) \\;=\\; \\sqrt{\\dfrac{s^2_X}{n_X}} \\;=\\; \\sqrt{\\dfrac{940900.9}{750}} \\;=\\; 35.419.
$$

```r
# Get the sample summaries from the DS dataframe
distr.summary.x(AmountSpent, digits=3, data=DS)
xbar <- 1222.437      # sample mean (point estimate of mu_X)
s2_x <- 940900.9      # sample variance
n_X  <- 750
# Equivalent in base R:
xbar <- mean(DS$AmountSpent); xbar
s2_x <- var(DS$AmountSpent);  s2_x
# Estimated SE of Xbar (sigma_X unknown -> plug-in s_X)
se_Xbar <- sqrt(s2_x / n_X); se_Xbar    # 35.419
```

**Worked numerical check.** $940900.9 / 750 = 1254.534\\overline{53}$ and $\\sqrt{1254.535} \\approx 35.419$, matching the reported value.

**Interpretation.** $se(\\bar X)$ measures the **typical** sampling variability of $\\bar X$ across hypothetical resamples of size $n_X = 750$ — it is *not* the deviation of this specific $\\bar x = 1222.437$ from the unknown $\\mu_X$, which cannot be quantified without further assumptions (CI or test).

**Assumptions.** The $n_X = 750$ customers are an i.i.d. sample from the population of the firm's customers; representativeness ensures $\\bar X$ is unbiased for $\\mu_X$.
""", "images": []}

ex5["5_1b"] = {"title": "Ex 5.1b — Point estimate + SE for the competitor sample",
"content": """**Question.** Repeat the calculation of 5.1a for a **competitor** company, whose sample summaries are reported as a **total amount spent** of $\\sum_{i=1}^{n_Y} y_i = 682\\,000$, a sample size $n_Y = 620$, and a sample variance $s_Y^2 = 921\\,486$. Provide a point estimate and an estimate of the standard error of the mean amount spent $\\mu_Y$ for the competitor.

---

**Setup.** Let $Y$ be the amount spent by a generic competitor customer with population mean $\\mu_Y$ and population variance $\\sigma_Y^2$. With an i.i.d. sample of size $n_Y$, the **sample mean**
$$
\\bar Y \\;=\\; \\frac{1}{n_Y}\\sum_{i=1}^{n_Y} Y_i
$$
is unbiased for $\\mu_Y$ ($\\Exp{\\bar Y} = \\mu_Y$), and its **standard error**
$$
SE(\\bar Y) \\;=\\; \\frac{\\sigma_Y}{\\sqrt{n_Y}}
$$
depends on the unknown $\\sigma_Y$. As in 5.1a, we **plug in** the sample variance $s_Y^2$ to obtain the estimated SE:
$$
se(\\bar Y) \\;=\\; \\sqrt{\\dfrac{s_Y^2}{n_Y}}.
$$

---

**AI walkthrough.**

**Point estimate of $\\mu_Y$.** Recover $\\bar y$ from the reported total:
$$
\\bar y \\;=\\; \\frac{\\sum y_i}{n_Y} \\;=\\; \\frac{682\\,000}{620} \\;=\\; 1100.000.
$$

**Estimated SE of $\\bar Y$.** Plug in $s_Y^2 = 921\\,486$ and $n_Y = 620$:
$$
\\dfrac{s_Y^2}{n_Y} \\;=\\; \\dfrac{921\\,486}{620} \\;=\\; 1486.268,
\\qquad
se(\\bar Y) \\;=\\; \\sqrt{1486.268} \\;\\approx\\; 38.552.
$$

**Comparison with 5.1a.** The firm had $se(\\bar X) \\approx 35.419$ (with $n_X = 750$, $s_X^2 = 940\\,900.9$); the competitor has $se(\\bar Y) \\approx 38.552$. The competitor's SE is **larger** because the smaller sample size $n_Y = 620 < n_X = 750$ outweighs its slightly smaller sample variance — confirming the $1/\\sqrt{n}$ shrinkage rule for SEs.

```r
# Competitor summaries (reported)
sum_y <- 682000
n_Y   <- 620
s2_y  <- 921486
# Point estimate of mu_Y
ybar  <- sum_y / n_Y; ybar              # 1100
# Estimated SE of Ybar (sigma_Y unknown -> plug-in s_Y)
se_Ybar <- sqrt(s2_y / n_Y); se_Ybar    # ~38.552
```

**Interpretation.** $se(\\bar Y) \\approx 38.55$ measures the **typical** sampling variability of $\\bar Y$ across hypothetical i.i.d. samples of size $620$ from the competitor's customer population — it is *not* the deviation of this specific $\\bar y = 1100$ from the unknown $\\mu_Y$.

**Assumptions.** The 620 competitor customers form an i.i.d., representative sample from the competitor's customer population.
""", "images": []}

ex5["5_1f"] = {"title": "Ex 5.1f — SE of difference in means: known/unknown/equal variances",
"content": """**Question.**

![Ex 5.1f question](statistics/images/ex5/questions/ex5_1f_question.png)

Consider the difference in mean amount spent between the firm's customers ($X$, $n_X=750$, $\\bar x = 1222.437$, $s_X^2 = 940\\,900.9$) and the competitor's customers ($Y$, $n_Y=620$, $\\bar y = 682\\,000/620 = 1100.000$, $s_Y^2 = 921\\,486$). **Determine** (or estimate) the standard error of $\\bar X - \\bar Y$ under three different sets of assumptions:
**f1)** $\\sigma_X^2,\\,\\sigma_Y^2$ are both **unknown** and **not assumed equal** (Welch);
**f2)** $\\sigma_X^2 = \\sigma_Y^2 = \\sigma^2$ **unknown** (pooled);
**f3)** $\\sigma_X^2 = \\sigma_Y^2 = \\sigma^2 = 810\\,000$ **known**.

---

**Setup.** For two **independent** samples,
$$
\\Var{\\bar X - \\bar Y} \\;=\\; \\Var{\\bar X} + \\Var{\\bar Y} \\;=\\; \\frac{\\sigma_X^2}{n_X} + \\frac{\\sigma_Y^2}{n_Y},
\\qquad
SE(\\bar X - \\bar Y) \\;=\\; \\sqrt{\\frac{\\sigma_X^2}{n_X} + \\frac{\\sigma_Y^2}{n_Y}}.
$$
What changes across the three cases is **whether the variances are known** (so $SE$ is exact) and **whether they can be pooled** (so a single estimator with more degrees of freedom is used).

The **pooled** sample variance under the homoscedasticity assumption $\\sigma_X^2=\\sigma_Y^2=\\sigma^2$ is the weighted average
$$
s_p^2 \\;=\\; \\frac{(n_X-1)\\,s_X^2 + (n_Y-1)\\,s_Y^2}{n_X + n_Y - 2}, \\qquad \\Exp{s_p^2} = \\sigma^2,
$$
which is the **best (lowest-variance) unbiased estimator** of the common variance — it pools $n_X+n_Y-2 = 1368$ degrees of freedom rather than $n_X-1 = 749$ or $n_Y-1 = 619$ alone.

---

**AI walkthrough.**

**Point estimate of the difference.** Independent of the assumptions,
$$
\\bar x - \\bar y \\;=\\; 1222.437 - 1100.000 \\;=\\; 122.437.
$$

**f1) Welch SE — variances unknown, not assumed equal.** Plug the sample variances into the exact formula:
$$
\\widehat{SE}_W(\\bar X-\\bar Y) \\;=\\; \\sqrt{\\frac{s_X^2}{n_X} + \\frac{s_Y^2}{n_Y}} \\;=\\; \\sqrt{\\frac{940\\,900.9}{750} + \\frac{921\\,486}{620}}.
$$
Numerically: $940\\,900.9/750 = 1254.535$ and $921\\,486/620 = 1486.268$, so
$$
\\widehat{SE}_W \\;=\\; \\sqrt{1254.535 + 1486.268} \\;=\\; \\sqrt{2740.803} \\;\\approx\\; 52.353.
$$

**f2) Pooled SE — variances unknown, assumed equal.** First compute the pooled variance:
$$
s_p^2 \\;=\\; \\frac{749 \\cdot 940\\,900.9 + 619 \\cdot 921\\,486}{1368} \\;=\\; \\frac{704\\,734\\,774 + 570\\,399\\,834}{1368} \\;\\approx\\; 932\\,116.
$$
Then
$$
\\widehat{SE}_p(\\bar X-\\bar Y) \\;=\\; \\sqrt{s_p^2\\!\\left(\\frac{1}{n_X} + \\frac{1}{n_Y}\\right)} \\;=\\; \\sqrt{932\\,116 \\cdot \\left(\\tfrac{1}{750} + \\tfrac{1}{620}\\right)} \\;\\approx\\; \\sqrt{2742.86} \\;\\approx\\; 52.373.
$$

**f3) Exact SE — common variance known.** No estimation: use $\\sigma^2 = 810\\,000$ directly,
$$
SE(\\bar X-\\bar Y) \\;=\\; \\sqrt{\\sigma^2\\!\\left(\\frac{1}{n_X} + \\frac{1}{n_Y}\\right)} \\;=\\; \\sqrt{810\\,000 \\cdot \\left(\\tfrac{1}{750} + \\tfrac{1}{620}\\right)} \\;=\\; \\sqrt{2386.45} \\;\\approx\\; 48.852.
$$

**Comparison.**

\\begin{tabular}{p{8cm}|p{10cm}|p{12cm}|p{8cm}}
Case & Assumption & SE estimator & Value \\\\
\\hline
f1 & $\\sigma_X^2 \\ne \\sigma_Y^2$, unknown & $\\sqrt{s_X^2/n_X + s_Y^2/n_Y}$ (Welch) & $\\approx 52.353$ \\\\
f2 & $\\sigma_X^2 = \\sigma_Y^2$, unknown & $\\sqrt{s_p^2(1/n_X + 1/n_Y)}$ (pooled) & $\\approx 52.373$ \\\\
f3 & $\\sigma^2 = 810\\,000$ known & $\\sqrt{\\sigma^2(1/n_X + 1/n_Y)}$ (exact) & $\\approx 48.852$
\\end{tabular}

**Take-aways.**
(i) **Welch vs. pooled** give almost identical values here ($52.353$ vs. $52.373$) because $s_X^2 \\approx s_Y^2$ and both samples are large — pooling is innocuous.
(ii) The **known-variance** SE is smaller because the assumed $\\sigma^2 = 810\\,000$ is below the pooled estimate $s_p^2 \\approx 932\\,116$; if instead the assumed value were larger, the exact SE would be larger. Knowing $\\sigma^2$ removes estimation uncertainty *from $SE$ itself*, not from the **interval procedure** (which then uses $z$ instead of $t$).
(iii) Only f3 is an **exact** standard error; f1 and f2 are **estimated** standard errors.

---

**Answer.**
- **f1) Welch (unknown, unequal variances).** $\\widehat{SE} = \\sqrt{s_X^2/n_X + s_Y^2/n_Y} \\approx 52.353$.
- **f2) Pooled (unknown, equal variances).** $s_p^2 = \\frac{749\\,s_X^2 + 619\\,s_Y^2}{1368} \\approx 932\\,116$, and $\\widehat{SE} = \\sqrt{s_p^2(1/n_X + 1/n_Y)} \\approx 52.373$.
- **f3) Known equal variances** $\\sigma^2 = 810\\,000$. $SE = \\sqrt{\\sigma^2(1/n_X + 1/n_Y)} \\approx 48.852$ — exact, no estimation needed.

The Welch and pooled SEs are practically identical because the two sample variances are close and $n_X, n_Y$ are both large; the known-variance SE differs because the assumed $\\sigma^2$ is below the pooled $s_p^2$.

```r
# Recall from 5.1a / 5.1b
xbar <- 1222.437;  s2_x <- 940900.9;  n_x <- 750
ybar <- 682000/620; s2_y <- 921486;   n_y <- 620

# Point estimate of the difference
xbar - ybar                                     # 122.437

# f1) Welch SE: no assumption on the variances
SE_W <- sqrt(s2_x/n_x + s2_y/n_y);  SE_W        # ~ 52.353

# f2) Pooled SE: equal (unknown) variances
s2_pool <- ((n_x-1)*s2_x + (n_y-1)*s2_y) / (n_x + n_y - 2)
s2_pool                                          # ~ 932116
SE_pool <- sqrt(s2_pool * (1/n_x + 1/n_y));  SE_pool   # ~ 52.373

# f3) Exact SE: equal and KNOWN variances (sigma^2 = 810000)
sigma2 <- 810000
SE_known <- sqrt(sigma2 * (1/n_x + 1/n_y));  SE_known  # ~ 48.852
```

---

**Reference answer.**

![Ex 5.1f answer](statistics/images/ex5/answers/ex5_1f_answer.png)
""", "images": [
    "statistics/images/ex5/questions/ex5_1f_question.png",
    "statistics/images/ex5/answers/ex5_1f_answer.png",
]}

ex5["5_2a"] = {"title": "Ex 5.2a — Standard error of $\\bar X$ with known population variance",
"content": """**Question.** From a sample of size $n=15$, the following summary statistics are available: $\\sum_{i=1}^{n} x_i = 2755$ and $\\sum_{i=1}^{n} x_i^2 = 585\\,203$. Assuming the population variance is **known** and equal to $\\sigma^2 = 6500$, propose an unbiased estimator of the population mean $\\mu$, compute the point estimate $\\bar x$, and determine the standard error $SE(\\bar X)$. Then repeat the SE computation for the larger sample size $n=50$.

---

**Setup.** For an i.i.d. sample $X_1,\\ldots,X_n$ from a distribution with mean $\\mu$ and variance $\\sigma^2$, the **sample mean**
$$
\\bar X \\;=\\; \\frac{1}{n}\\sum_{i=1}^{n} X_i
$$
is unbiased for $\\mu$ ($\\Exp{\\bar X} = \\mu$) for any sample size — no distributional assumption is needed for unbiasedness. Its variance and standard error are
$$
\\Var{\\bar X} \\;=\\; \\frac{\\sigma^2}{n}, \\qquad SE(\\bar X) \\;=\\; \\frac{\\sigma}{\\sqrt{n}}.
$$
Because $\\sigma^2$ is **known** here, $SE(\\bar X)$ is **exact** — no plug-in estimation is required.

---

**AI walkthrough.**

1. **Point estimate of $\\mu$.** From the raw sums,
$$
\\bar x \\;=\\; \\frac{1}{n}\\sum_{i=1}^{n} x_i \\;=\\; \\frac{2755}{15} \\;=\\; 183.667.
$$

2. **Exact SE at $n=15$.** With $\\sigma^2 = 6500$ known,
$$
SE(\\bar X) \\;=\\; \\sqrt{\\dfrac{\\sigma^2}{n}} \\;=\\; \\sqrt{\\dfrac{6500}{15}} \\;=\\; \\sqrt{433.333} \\;\\approx\\; 20.817.
$$

3. **Exact SE at $n=50$.** Same $\\sigma^2$, larger $n$:
$$
SE(\\bar X) \\;=\\; \\sqrt{\\dfrac{6500}{50}} \\;=\\; \\sqrt{130} \\;\\approx\\; 11.402.
$$

4. **Shrinkage rate.** Going from $n=15$ to $n=50$ shrinks the SE by a factor $\\sqrt{15/50} = \\sqrt{0.3} \\approx 0.548$, i.e. $20.817 \\cdot 0.548 \\approx 11.40$ — consistent with the $1/\\sqrt{n}$ rule.

5. **Why "known" matters.** With $\\sigma^2$ known, $SE$ has no estimation uncertainty and inference uses the **standard normal** quantiles ($z$); with $\\sigma^2$ unknown the SE must be estimated from $s^2$ and inference uses the **$t$ distribution** with $n-1$ degrees of freedom (see Ex 5.2b).

---

**Answer.**
- **Estimator.** $\\bar X = \\tfrac{1}{n}\\sum X_i$, unbiased for $\\mu$.
- **Point estimate.** $\\bar x = 2755/15 = 183.667$.
- **SE at $n=15$.** $SE(\\bar X) = \\sqrt{6500/15} \\approx 20.817$ — exact, since $\\sigma^2$ is known.
- **SE at $n=50$.** $SE(\\bar X) = \\sqrt{6500/50} \\approx 11.402$ — exact; SE shrinks by $1/\\sqrt{n}$.

```r
# Raw summary stats
n      <- 15
sum.x  <- 2755
sum.x2 <- 585203

# Point estimate of mu
xbar   <- sum.x / n; xbar              # 183.6667

# (a) Known population variance -> SE is EXACT (no estimation)
sigma2_x <- 6500
SE_Xbar  <- sqrt(sigma2_x / n); SE_Xbar      # ~20.8167

# (b) Larger sample n = 50, same known sigma^2
SE_Xbar.b <- sqrt(sigma2_x / 50); SE_Xbar.b  # ~11.4018

# Shrinkage factor: SE at n=50 over SE at n=15
SE_Xbar.b / SE_Xbar                          # ~0.5477 = sqrt(15/50)
```

**Assumptions.** The 15 (resp. 50) observations are an i.i.d. sample from the population, and $\\sigma^2 = 6500$ is the true (known) population variance. No Normality of $X$ is needed for unbiasedness or for $SE(\\bar X) = \\sigma/\\sqrt{n}$; Normality (or CLT for large $n$) is only required to attach a $z$-based interval/test to this SE.
""", "images": []}

ex5["5_2b"] = {"title": "Ex 5.2b — SE with unknown population variance",
"content": """<span class="exam-question-text">**Question.** Same setting as 5.2a ($n=15$, $\\sum x = 2755$, $\\sum x^2 = 585\\,203$; and a second sample with $n=50$, $\\bar x = 205$, $s = 91.5$), but now $\\sigma^2$ is **unknown** — estimate it from the data and obtain the standard error of $\\bar X$ for both sample sizes. Compare with the known-variance case (5.2a).</span>

![Ex 5.2b question](statistics/images/ex5/questions/ex5_2b_question.png)

---

**AI walkthrough.** With $\\sigma^2$ unknown, swap it for the **sample variance** $s^2$ — the only change to the SE formula is "known $\\sigma$" $\\to$ "plug-in $s$".

1. **Recover $s^2$ from the summary stats ($n=15$).** Use the computational form
$$s^2 = \\frac{1}{n-1}\\left(\\sum x_i^2 - n\\bar x^2\\right) = \\frac{1}{14}\\left(585\\,203 - 15\\cdot 183.667^2\\right) \\approx 5\\,657.24.$$
This is close to (but not equal to) the assumed-known $\\sigma^2 = 6500$ — sampling noise.
2. **Estimated SE at $n=15$.** $\\widehat{SE}(\\bar X) = s/\\sqrt{n} = \\sqrt{5657.24/15} \\approx 19.42$, slightly smaller than the known-variance value $\\sqrt{6500/15} \\approx 20.82$ — because this particular sample's $s^2$ happened to under-shoot $\\sigma^2$.
3. **Estimated SE at $n=50$.** Here we are told $s = 91.5$, so $\\widehat{SE} = 91.5/\\sqrt{50} \\approx 12.94$ vs. the known-variance value $\\sqrt{6500/50} \\approx 11.40$ — this time $s^2 = 8372.25$ over-shoots $\\sigma^2 = 6500$.
4. **What "unknown" costs us.** The point estimator $\\bar X$ is unchanged; only the **SE is now an estimate**, with its own uncertainty. For inference (CIs / tests on $\\mu$) we replace the $z$ quantile by a $t_{n-1}$ quantile to compensate.
5. **Take-away.** Larger $n$ gives a smaller SE and a more reliable variance estimate — both effects make $\\bar X$ a better tool when $\\sigma^2$ is unknown.

![Ex 5.2b AI walkthrough](statistics/images/ex5/ex5_2b_ai.png)

---

**Answer.**
```r
n <- 15; sum.x <- 2755; sum.x2 <- 585203
xbar <- sum.x/n; xbar                              # 183.667
SE_Xbar.b                                          # 11.4018 (known-variance n=50)

# Unknown variance — estimate s^2 from sample (n = 15)
s2_x <- (1/(n-1)) * (sum.x2 - n*xbar^2); s2_x      # 5657.238
se_xbar <- sqrt(s2_x/n); se_xbar                   # 19.4203

# Unknown variance — n = 50, s = 91.5 (given)
se_xbar.b <- 91.5/sqrt(50); se_xbar.b              # 12.9401
```

---

**Reference answer.**

![Ex 5.2b answer](statistics/images/ex5/answers/ex5_2b_answer.png)
""", "images": [
    "statistics/images/ex5/questions/ex5_2b_question.png",
    "statistics/images/ex5/ex5_2b_ai.png",
    "statistics/images/ex5/answers/ex5_2b_answer.png",
]}

ex5["5_3a"] = {"title": "Ex 5.3a — Mean customers, known variance ($n=15$ vs $n=50$)",
"content": """<span class="exam-question-text">**EXERCISE 5.3.** A manager in a store is interested in estimating the mean of the number of customers $\\mu$ visiting the store on weekdays, in order to assess whether it is worthwhile to hire staff permanently or to hire temporary workers (paid less).

**a)** Based on a sample of $n=15$ observations with $\\sum x_i = 2755$ and $\\sum x_i^2 = 585\\,203$, provide an unbiased estimator for $\\mu$, list the assumptions needed, and report the standard error of the estimator assuming $\\sigma^2 = 6500$ is **known**.

**b)** Then a larger sample of size $n=50$ yields $\\bar x = 205$ and $s = 91.5$ — estimate $\\mu$ and the SE of the estimator for this larger sample (still assuming $\\sigma^2 = 6500$ known).</span>

![Ex 5.3a question](statistics/images/ex5/questions/ex5_3a_question.png)

---

**AI walkthrough.** Straight from the summary stats — no raw data needed.

1. **Unbiased estimator.** The sample mean $\\bar X = \\tfrac{1}{n}\\sum_i X_i$ is unbiased for $\\mu$ regardless of the distribution, provided the $X_i$ are *iid* with finite mean. No normality assumption is required for **unbiasedness** — that only kicks in if we want exact distributional results for $\\bar X$ at small $n$.
2. **Compute $\\bar x$ at $n=15$.** Plug in: $\\bar x = 2755/15 = 183.667$.
3. **SE with known variance.** Since $\\sigma^2$ is given, $SE(\\bar X) = \\sigma/\\sqrt{n}$ is **exact** (no need to estimate it from the data). Here $SE = \\sqrt{6500/15} = \\sqrt{433.33} \\approx 20.82$.
4. **Larger sample $n=50$.** The point estimate is $\\hat\\mu = \\bar x = 205$. With $\\sigma^2 = 6500$ still known, $SE = \\sqrt{6500/50} = \\sqrt{130} \\approx 11.40$.
5. **Effect of $n$.** $SE$ shrinks as $1/\\sqrt{n}$: going from $n=15$ to $n=50$ multiplies the SE by $\\sqrt{15/50} \\approx 0.548$, i.e. cuts it almost in half ($20.82 \\to 11.40$). This is the textbook $\\sqrt{n}$-consistency of the sample mean.

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

---

**Reference answer.**

![Ex 5.3a answer](statistics/images/ex5/answers/ex5_3a_answer.png)
""", "images": [
    "statistics/images/ex5/questions/ex5_3a_question.png",
    "statistics/images/ex5/answers/ex5_3a_answer.png",
]}

ex5["5_3b"] = {"title": "Ex 5.3b — Closeness claim + SE with unknown variance",
"content": """<span class="exam-question-text">**Question.** Same setting as 5.3a — store manager, $n=15$ sample with $\\sum x_i = 2755$, $\\sum x_i^2 = 585\\,203$ (giving $\\bar x = 183.667$), and a second sample of $n=50$ with $\\bar x = 205$, $s = 91.5$.

**c)** Comment on the statement: *"the estimate obtained in (b) is closer to $\\mu$ than the estimate obtained in (a)"* — is it true or false? If it is true, explain clearly why; if it is false, rephrase it correctly.

**d)** What would be the evaluation of the standard errors of the estimators at points (a) and (b) **if the variance were not assumed to be known**?</span>

![Ex 5.3b question](statistics/images/ex5/questions/ex5_3b_question.png)

---

**AI walkthrough.** Two distinct conceptual points: (c) is about the **distinction between an estimator and a single realization**, (d) is the standard plug-in trick.

1. **(c) The claim is FALSE.** A larger $n$ gives a smaller $SE(\\bar X) = \\sigma/\\sqrt{n}$, so the *sampling distribution* of $\\bar X$ is more **concentrated** around $\\mu$. But a *specific realization* $\\bar x_{50} = 205$ from that tighter distribution is **not guaranteed** to be closer to the unknown $\\mu$ than the specific realization $\\bar x_{15} = 183.667$ — you can always be "unlucky" with the larger sample. The correct rephrasing is in terms of the **estimator**: "the estimates of $\\mu$ obtained with larger sample size are *more concentrated* around $\\mu$; therefore, the **probability** that the sample mean of a generic sample falls in an interval centered on $\\mu$, e.g. $(\\mu - c, \\mu + c)$, is greater when the sample size is 50 rather than 15."
2. **(d) Drop the known-$\\sigma^2$ assumption.** The point estimator $\\bar X$ is unchanged; only the SE formula changes from $\\sigma/\\sqrt{n}$ (exact) to $\\widehat{SE} = s/\\sqrt{n}$ (an *estimate*, depending on the sample variance $s^2$).
3. **Recover $s^2$ for the $n=15$ sample from summaries.** Use the computational form $s^2 = \\tfrac{1}{n-1}\\left(\\sum x_i^2 - n\\bar x^2\\right) = \\tfrac{1}{14}(585\\,203 - 15\\cdot 183.667^2) \\approx 5\\,657.24.$ Then $\\widehat{SE} = \\sqrt{5657.24/15} \\approx 19.42$ — slightly **smaller** than the known-$\\sigma^2$ value $20.82$, because *this particular* sample's $s^2$ under-shoots the assumed true $\\sigma^2 = 6500$.
4. **$n=50$ sample.** $s = 91.5$ is given directly, so $\\widehat{SE} = 91.5/\\sqrt{50} \\approx 12.94$ — slightly **larger** than the known-$\\sigma^2$ value $11.40$, because here $s^2 = 91.5^2 = 8372.25$ over-shoots $\\sigma^2 = 6500$.
5. **Take-away.** Larger $n$ gives both a smaller SE **and** a more stable estimate of $\\sigma^2$. Sample-by-sample, $s^2$ is noisy (unbiased but variable); inference under unknown $\\sigma^2$ also requires replacing the $z$-quantile by a $t_{n-1}$-quantile to account for that extra noise.

![Ex 5.3b AI walkthrough](statistics/images/ex5/ex5_3b_ai.png)

---

**Answer.**
```r
# (c) FALSE. Smaller SE means the sampling distribution of Xbar is more
#     CONCENTRATED around mu (larger n -> tighter distribution), but it
#     does NOT mean any specific realization xbar is closer to mu than
#     a realization from the smaller sample. Correct rephrasing: the
#     PROBABILITY that a generic xbar falls in (mu-c, mu+c) is higher
#     when n = 50 than when n = 15.

# (d) Unknown variance -> estimate it from the sample: se(Xbar) = sqrt(s^2/n)
# Sample of size n = 15: recover s^2 from sum.x, sum.x2
n <- 15
sum.x  <- 2755
sum.x2 <- 585203
xbar   <- sum.x / n; xbar                          # 183.667
s2_x   <- (1/(n-1)) * (sum.x2 - n*xbar^2); s2_x    # 5657.238
se_xbar <- sqrt(s2_x / n); se_xbar                 # ~19.42

# Sample of size n = 50 (s = 91.5 given):
se_xbar.b <- 91.5 / sqrt(50); se_xbar.b            # 12.94

# Compare with the known-variance SE from (a):
sqrt(6500 / 15)                                    # 20.82
sqrt(6500 / 50)                                    # 11.40
```

For $n=15$, the variance estimate ($5657.24$) is below the assumed true $\\sigma^2 = 6500$, so the plug-in SE ($19.42$) is below the known-variance SE ($20.82$). For $n=50$, the variance estimate ($91.5^2 = 8372.25$) over-shoots, so the plug-in SE ($12.94$) is above the known-variance one ($11.40$). What matters for inference is **sample size**: even with a noisier $s^2$ at $n=50$, the larger denominator $\\sqrt{n}$ still drives the SE down.

---

**Reference answer.**

![Ex 5.3b answer](statistics/images/ex5/answers/ex5_3b_answer.png)
""", "images": [
    "statistics/images/ex5/questions/ex5_3b_question.png",
    "statistics/images/ex5/ex5_3b_ai.png",
    "statistics/images/ex5/answers/ex5_3b_answer.png",
]}

ex5["5_4"] = {"title": "Ex 5.4 — Mean difference for paired data (pre/post)",
"content": """<span class="exam-question-text">**EXERCISE 5.4.** A supermarket chain runs a targeted survey on its regularly shopping customers. A sample of $n=315$ customers reports their weekly online spending **pre-pandemic (2019)** and **post-pandemic (2023)**. The pre-pandemic weekly online spending has sample mean $\\bar x_{\\text{pre}} = 28.3$ and sample standard deviation $s_{\\text{pre}} = 7.5$; the post-pandemic weekly online spending has $\\bar x_{\\text{post}} = 37.5$ and $s_{\\text{post}} = 8.4$. The sample correlation between pre- and post-pandemic spending is $\\hat\\rho_{\\text{pre},\\text{post}} = 0.65$.

**a)** Propose an unbiased estimator for the **increase** in the mean weekly amount spent online and state the assumptions needed.

**b)** Evaluate the **standard error** of that estimator using the paired-data formula.

**c)** What can be said about the difference between the obtained estimate and the actual change in the mean weekly amount spent online?</span>

![Ex 5.4 question](statistics/images/ex5/questions/ex5_4_question.png)

---

**AI walkthrough.** Same $n=315$ customers measured **twice** — pre and post — so the two samples are **paired**, not independent. The covariance term $-2\\rho\\,s_{\\text{pre}}s_{\\text{post}}$ in the SE formula is the whole point.

1. **Estimator of the increase.** Let $D_i = X_{\\text{post},i} - X_{\\text{pre},i}$ be the within-customer change. The unbiased estimator of $\\mu_{\\text{post}} - \\mu_{\\text{pre}}$ is the sample mean of the differences,
$$\\bar D \\;=\\; \\bar X_{\\text{post}} - \\bar X_{\\text{pre}}, \\qquad \\Exp{\\bar D} \\;=\\; \\mu_{\\text{post}} - \\mu_{\\text{pre}}.$$
**Assumptions:** the $n=315$ customer pairs $(X_{\\text{pre},i}, X_{\\text{post},i})$ are i.i.d. across customers (representative survey); no distributional assumption is needed for unbiasedness.

2. **Point estimate.** Plug in: $\\bar x_{\\text{post}} - \\bar x_{\\text{pre}} = 37.5 - 28.3 = 9.2$ €/week — the estimated post-vs-pre increase in mean weekly online spending.

3. **Paired SE formula.** Because pre and post are measured on the **same** customer, the variance of the difference carries a covariance term:
$$\\Var(\\bar D) \\;=\\; \\frac{\\sigma^2_{\\text{pre}} + \\sigma^2_{\\text{post}} - 2\\,\\rho\\,\\sigma_{\\text{pre}}\\sigma_{\\text{post}}}{n}.$$
Plug-in (sample) SE:
$$\\widehat{SE}(\\bar D) \\;=\\; \\sqrt{\\frac{s^2_{\\text{pre}} + s^2_{\\text{post}} - 2\\,\\hat\\rho\\,s_{\\text{pre}}s_{\\text{post}}}{n}}.$$

4. **Numerical SE.** Numerator: $s^2_{\\text{pre}} + s^2_{\\text{post}} - 2\\hat\\rho s_{\\text{pre}}s_{\\text{post}} = 7.5^2 + 8.4^2 - 2(0.65)(7.5)(8.4) = 56.25 + 70.56 - 81.90 = 44.91$. Divide by $n=315$: $44.91/315 \\approx 0.1426$. Square root: $\\widehat{SE}(\\bar D) \\approx \\sqrt{0.1426} \\approx 0.3776$.

5. **Why paired beats independent here.** Had we (wrongly) ignored pairing and used the **independent-samples** SE $\\sqrt{(s^2_{\\text{pre}} + s^2_{\\text{post}})/n} = \\sqrt{126.81/315} \\approx 0.6346$, the SE would be ~$1.68\\times$ larger. With $\\hat\\rho = 0.65 > 0$, customers who spent more pre-pandemic also tend to spend more post-pandemic — common subject-level variation **cancels** in the difference, sharpening the estimate.

6. **(c) Distance to the truth.** The SE measures the typical sampling variability of $\\bar D$ around the unknown true increase $\\mu_{\\text{post}} - \\mu_{\\text{pre}}$ — it does **not** quantify how far the realized $\\bar d = 9.2$ is from that truth. To bound the realized error we need a CI: $9.2 \\pm z_{1-\\alpha/2}\\cdot 0.3776$, e.g. an approximate $95\\%$ CI $\\approx [8.46,\\,9.94]$, comfortably excluding zero.

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
se_diff  <- sqrt(var_diff); se_diff   # ~0.3776

# Compare: independent-samples SE (WRONG here - ignores pairing)
sqrt((s_pre^2 + s_post^2) / n)        # ~0.6346, ~1.68x larger

# (c) Without further assumptions the deviation of the estimate (9.2)
#     from the true increase cannot be quantified -- only its SE is known.
```

For paired data, the SE of the difference uses the **covariance** between pre and post — usually MUCH smaller than the independent-samples form when $\\rho > 0$.

---

**Reference answer.**

![Ex 5.4 answer](statistics/images/ex5/answers/ex5_4_answer.png)
""", "images": [
    "statistics/images/ex5/questions/ex5_4_question.png",
    "statistics/images/ex5/answers/ex5_4_answer.png",
]}

ex5["5_5a"] = {"title": "Ex 5.5a — Difference between male/female proportions (bookstore)",
"content": """<span class="exam-question-text">**Question.** A bookstore chain surveys 650 male and 850 female customers in 2022 about whether they bought $\\ge 2$ books. Heavy readers: 221 males, 391 females. Estimate the difference between the proportions of male and female heavy readers, and the standard error of the estimator.</span>

![Ex 5.5a question](statistics/images/ex5/questions/ex5_5a_question.png)

---

**AI walkthrough.** Two **independent** Bernoulli samples — proportions and their SE drop straight out of the success counts.

1. **Sample proportions.** $\\hat p_F = 391/850 \\approx 0.46$ (female heavy-reader rate); $\\hat p_M = 221/650 \\approx 0.34$ (male rate).
2. **Point estimate of the gap.** $\\hat p_F - \\hat p_M \\approx 0.46 - 0.34 = 0.12$ — females are an estimated 12 percentage points more likely to be heavy readers.
3. **Variance adds (independence).** $\\Var(\\hat p_F - \\hat p_M) = \\Var(\\hat p_F) + \\Var(\\hat p_M) = p_F(1-p_F)/n_F + p_M(1-p_M)/n_M$ — no covariance term because the two samples are drawn independently of each other.
4. **Plug-in SE.** The population $p_F,p_M$ are unknown, so estimate the SE by plugging $\\hat p_F,\\hat p_M$: $\\widehat{SE} = \\sqrt{0.46\\cdot 0.54/850 + 0.34\\cdot 0.66/650} \\approx \\sqrt{2.92\\cdot 10^{-4} + 3.45\\cdot 10^{-4}} \\approx 0.02525$.
5. **Sanity check.** The gap is roughly $0.12/0.02525 \\approx 4.75$ SEs from $0$ — a normal-approx $95\\%$ CI is $0.12 \\pm 1.96\\cdot 0.02525 \\approx [0.070,\\,0.170]$, clearly excluding zero.

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

---

**Reference answer.**

![Ex 5.5a answer](statistics/images/ex5/answers/ex5_5a_answer.png)
""", "images": [
    "statistics/images/ex5/questions/ex5_5a_question.png",
    "statistics/images/ex5/answers/ex5_5a_answer.png",
]}

ex5["5_5b"] = {"title": "Ex 5.5b — Change in proportion 2015 vs 2022",
"content": """<span class="exam-question-text">**Question.** A previous survey conducted on $n=1000$ customers in **2015** found the proportion of heavy readers to be $\\hat p_{2015} = 0.45$. Propose an estimator for the change in the percentage of customers buying at least 2 books from 2015 to 2022 based on the available data, and evaluate its estimate and standard error.</span>

![Ex 5.5b question](statistics/images/ex5/questions/ex5_5b_question.png)

---

**AI walkthrough.** Two **independent** Bernoulli samples taken at different times — the change is a difference of proportions and the SE adds variances.

1. **Pool the 2022 sample.** The 2022 survey lumps both sexes together for this part: $\\hat p_{2022} = (221 + 391)/(650 + 850) = 612/1500 \\approx 0.408$ — sample proportion of heavy readers in 2022.
2. **Estimator.** With independent samples from 2015 and 2022, the natural unbiased estimator of $p_{2015} - p_{2022}$ is the **difference of sample proportions** $\\hat p_{2015} - \\hat p_{2022}$.
3. **Point estimate.** $\\hat p_{2015} - \\hat p_{2022} \\approx 0.45 - 0.408 = 0.042$ — heavy-reader share fell by about 4.2 percentage points between 2015 and 2022.
4. **Variance adds (independence).** $\\Var(\\hat p_{2015} - \\hat p_{2022}) = p_{2015}(1-p_{2015})/n_{2015} + p_{2022}(1-p_{2022})/n_{2022}$ — no covariance term because the surveys are run on different cohorts.
5. **Plug-in SE.** Both $p$'s unknown $\\Rightarrow$ plug in the sample proportions: $\\widehat{SE} = \\sqrt{0.45\\cdot 0.55/1000 + 0.408\\cdot 0.592/1500} \\approx \\sqrt{2.475\\cdot 10^{-4} + 1.610\\cdot 10^{-4}} \\approx 0.02021$.
6. **Sanity check.** The gap is only $0.042/0.02021 \\approx 2.08$ SEs from 0 — borderline; a normal-approx 95% CI is $0.042 \\pm 1.96\\cdot 0.02021 \\approx [0.0024,\\,0.0816]$, *just* excluding zero.

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

---

**Reference answer.**

![Ex 5.5b answer](statistics/images/ex5/answers/ex5_5b_answer.png)
""", "images": [
    "statistics/images/ex5/questions/ex5_5b_question.png",
    "statistics/images/ex5/answers/ex5_5b_answer.png",
]}

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
"content": """<span class="exam-question-text">**Question.** Propose an unbiased estimator for the fraction of employees in the company whose productivity improved after the transition ($\\text{Post} - \\text{Pre} > 0$). State the required assumptions.</span>

---

**Setup.** The parameter of interest is the **population proportion** $p = \\Pr(\\text{Post} - \\text{Pre} > 0)$ — the share of employees whose productivity strictly increased after the transition. Each employee contributes a binary indicator
$$Y_i \\;=\\; \\mathbb{1}\\{\\text{Post}_i - \\text{Pre}_i > 0\\}, \\qquad Y_i \\sim \\text{Bernoulli}(p),$$
so the problem reduces to estimating the success probability of a Bernoulli random variable from an i.i.d. sample.

---

**AI walkthrough.** Step by step:

1. **Reduce to a Bernoulli problem.** Form the paired differences $D_i = \\text{Post}_i - \\text{Pre}_i$, then keep only the *sign*: $Y_i = \\mathbb{1}\\{D_i > 0\\}$. Each $Y_i \\in \\{0,1\\}$ with success probability $\\Pr(Y_i=1) = \\Pr(D_i>0) = p$. The magnitude of the change is discarded — exactly what "fraction who improved" asks for. (Left panel: each dot is one employee; orange dots are improvers, lying above the $\\text{Post}=\\text{Pre}$ diagonal.)

2. **Propose the sample proportion.** With $Y_1,\\ldots,Y_n$ i.i.d. Bernoulli$(p)$ the natural estimator is the sample mean of the indicators,
$$\\hat p \\;=\\; \\bar Y \\;=\\; \\frac{1}{n}\\sum_{i=1}^{n} Y_i \\;=\\; \\frac{1}{n}\\sum_{i=1}^{n} \\mathbb{1}\\{\\text{Post}_i - \\text{Pre}_i > 0\\} \\;=\\; \\frac{\\#\\{\\text{improvers}\\}}{n}.$$

3. **Prove unbiasedness.** By linearity of expectation,
$$\\Exp{\\hat p} \\;=\\; \\frac{1}{n}\\sum_{i=1}^{n} \\Exp{Y_i} \\;=\\; \\frac{1}{n}\\,n\\,p \\;=\\; p, \\qquad \\text{for every } n \\ge 1.$$
So $\\hat p$ is unbiased for $p$ **regardless of sample size** and **regardless of the joint distribution of (Pre, Post)** — only the marginal probability $\\Pr(D_i>0)$ matters. (Middle panel: sampling distribution of $\\hat p$ is centred exactly on $p$.)

4. **Variance and standard error.** Since $\\Var(Y_i) = p(1-p)$ and the $Y_i$ are i.i.d.,
$$\\Var(\\hat p) \\;=\\; \\frac{p(1-p)}{n}, \\qquad SE(\\hat p) \\;=\\; \\sqrt{\\frac{p(1-p)}{n}}.$$
Because $p$ is unknown, the **estimated** SE plugs in $\\hat p$: $\\widehat{SE}(\\hat p) = \\sqrt{\\hat p(1-\\hat p)/n}$. The SE shrinks at rate $1/\\sqrt n$ (right panel).

5. **Worked numerical illustration.** Suppose $n = 80$ employees with 52 improvers. Then
$$\\hat p \\;=\\; \\tfrac{52}{80} \\;=\\; 0.65, \\qquad \\widehat{SE}(\\hat p) \\;=\\; \\sqrt{\\tfrac{0.65 \\cdot 0.35}{80}} \\;=\\; \\sqrt{0.002844} \\;\\approx\\; 0.0533.$$
The CLT rule of thumb requires $n\\hat p = 52 \\ge 5$ and $n(1-\\hat p) = 28 \\ge 5$ — both satisfied — so an approximate 95% CI for $p$ is
$$0.65 \\pm 1.96 \\cdot 0.0533 \\;=\\; [0.546,\\; 0.754].$$

![Ex 5.6b AI walkthrough](statistics/images/ex5/ex5_6b_ai.png)

---

**Answer.** Define $Y_i = \\mathbb{1}\\{\\text{Post}_i - \\text{Pre}_i > 0\\}$ for each sampled employee. Then $Y_i \\sim \\text{Bernoulli}(p)$ where $p$ is the fraction of the population that improved, and the **sample proportion**
$$\\hat p \\;=\\; \\frac{1}{n}\\sum_{i=1}^{n} \\mathbb{1}\\{\\text{Post}_i - \\text{Pre}_i > 0\\}$$
is unbiased for $p$ because $\\Exp{\\hat p} = \\frac{1}{n}\\sum \\Exp{Y_i} = p$.

**Assumptions.** The $n$ employees are an **i.i.d. random sample** from the company population (representative sampling, independent observations). Crucially, **pairing is automatic**: each $Y_i$ is built from the *same* employee's Pre/Post measurements, so the unbiasedness argument does **not** require independence between Pre and Post — they can be (and typically are) strongly positively correlated. Only the marginal sign of the within-employee change matters.

```r
# Using the Transition dataframe
Diff <- Transition$Post - Transition$Pre
Y    <- as.numeric(Diff > 0)            # 1 if improved, 0 otherwise
phat <- mean(Y); phat                   # sample proportion = unbiased estimate of p
n    <- length(Y); n
# Estimated SE of phat (plug-in)
se_phat <- sqrt(phat * (1 - phat) / n); se_phat
# CLT sanity check (both should be >= 5)
n * phat;  n * (1 - phat)
# Approximate 95% CI for p
phat + c(-1, 1) * qnorm(0.975) * se_phat
```

**Take-aways.** (i) Sign-based problems on paired data ("did the outcome improve?") reduce to a one-sample proportion estimate — no magnitude information is used. (ii) The sample proportion is unbiased *regardless of sample size and shape of the distributions* — Bernoulli expectations are linear. (iii) For CIs/tests, invoke the CLT (rule of thumb $n\\hat p, n(1-\\hat p) \\ge 5$) and use $\\hat p \\pm z_{1-\\alpha/2}\\,\\widehat{SE}(\\hat p)$.
""", "images": [
    "statistics/images/ex5/ex5_6b_ai.png",
]}

ex5["5_6d"] = {"title": "Ex 5.6d — SE of mean Post$-$Pre difference with cor = 0.58",
"content": """**Question.** Estimate the standard error of the estimator $\\bar X_{\\text{Post}} - \\bar X_{\\text{Pre}}$ under the assumption that the variances of Pre and Post productivity are equal and that $\\operatorname{cor}(\\text{Pre},\\text{Post}) = 0.58$. Use the subsample of employees with at least 2 children, 2015–2022, from `Exe5_Data.Rdata`.

---

**Answer.** Because Pre and Post are measured on the **same** $n$ employees, the two sample means are **dependent**. The general paired formula is

$$\\Var(\\bar X_{\\text{Post}} - \\bar X_{\\text{Pre}}) = \\frac{\\sigma^2_{\\text{Post}} + \\sigma^2_{\\text{Pre}} - 2\\,\\rho\\,\\sigma_{\\text{Post}}\\sigma_{\\text{Pre}}}{n}.$$

Under the assumption $\\sigma^2_{\\text{Post}} = \\sigma^2_{\\text{Pre}} = \\sigma^2$ this collapses to

$$\\Var(\\bar X_{\\text{Post}} - \\bar X_{\\text{Pre}}) = \\frac{2\\sigma^2(1-\\rho)}{n} \\;\\;\\Longrightarrow\\;\\; \\widehat{\\operatorname{SE}} = \\sqrt{\\dfrac{2\\,s^2_{\\text{pool}}\\,(1-\\rho)}{n}}.$$

The common variance is estimated by the **pooled** sample variance $s^2_{\\text{pool}} = \\tfrac{s^2_{\\text{Pre}} + s^2_{\\text{Post}}}{2}$ (equal sample sizes since the data are paired).

```r
# Restrict to the requested subsample
sub   <- subset(Transition,
                Children >= 2 & Year >= 2015 & Year <= 2022)
xPre  <- sub$Pre;  xPost <- sub$Post
n     <- length(xPre); n

# Pooled sample variance under the equal-variance assumption
s2_Pre  <- var(xPre);  s2_Post <- var(xPost)
s2_pool <- (s2_Pre + s2_Post) / 2; s2_pool
rho     <- 0.58

# SE under equal variances + cor = 0.58
se_diff <- sqrt(2 * s2_pool * (1 - rho) / n); se_diff
```

**Numerical illustration.** Take $n=120$, $s^2_{\\text{Pre}}=38$, $s^2_{\\text{Post}}=42$ so $s^2_{\\text{pool}}=40$. Then

$$\\widehat{\\operatorname{SE}} = \\sqrt{\\dfrac{2\\cdot 40\\cdot(1-0.58)}{120}} = \\sqrt{0.28} \\approx 0.529.$$

The **independent-samples** SE with the same variances would be $\\sqrt{2\\cdot 40/120}\\approx 0.816$ — about $1/\\sqrt{1-\\rho}\\approx 1.54\\times$ larger. The positive within-employee correlation shrinks the SE substantially.
""", "images": []}

ex5["5_7a"] = {"title": "Ex 5.7a — Pizzeria price difference, known equal variances",
"content": """<span class="exam-question-text">**Question.** Pizzerias are split by workforce size: **LE4** = at most 4 employees, **GT4** = more than 4 employees. Independent random samples yield $n_{\\text{LE4}} = 55$ and $n_{\\text{GT4}} = 45$. Assume that the population variances of `Price` are **known and equal** to $\\sigma^2 = 2.2$. Estimate the standard error of the estimator $\\bar X_{\\text{GT4}} - \\bar X_{\\text{LE4}}$.</span>

![Ex 5.7a question](statistics/images/ex5/questions/ex5_7a_question.png)

---

**AI walkthrough.** Two **independent** samples with a common **known** variance — the SE formula collapses to a one-line plug-in, no estimation needed.

1. **Variance of the difference (independence).** $\\Var(\\bar X_{\\text{GT4}} - \\bar X_{\\text{LE4}}) = \\Var(\\bar X_{\\text{GT4}}) + \\Var(\\bar X_{\\text{LE4}}) = \\sigma^2/n_{\\text{GT4}} + \\sigma^2/n_{\\text{LE4}}$. No covariance term because the two samples are independent.
2. **Factor the common $\\sigma^2$.** $\\Var(\\bar X_{\\text{GT4}} - \\bar X_{\\text{LE4}}) = \\sigma^2\\bigl(1/n_{\\text{GT4}} + 1/n_{\\text{LE4}}\\bigr)$.
3. **Plug numbers.** $1/45 + 1/55 = 55/2475 + 45/2475 = 100/2475 \\approx 0.04040$, so $\\Var \\approx 2.2 \\cdot 0.04040 \\approx 0.08889$.
4. **Take the square root.** $\\operatorname{SE} = \\sqrt{0.08889} \\approx 0.2981$.
5. **"Known" $=$ exact.** Because $\\sigma^2$ is given, this SE is **not** an estimate — there is no sample variance involved and no plug-in uncertainty. Compare with 5.7b where $\\sigma^2$ becomes unknown and we must replace it with $s^2$.

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

---

**Reference answer.**

![Ex 5.7a answer](statistics/images/ex5/answers/ex5_7a_answer.png)
""", "images": [
    "statistics/images/ex5/questions/ex5_7a_question.png",
    "statistics/images/ex5/answers/ex5_7a_answer.png",
]}

ex5["5_7b"] = {"title": "Ex 5.7b — Pizzeria price difference: known unequal & unknown variances",
"content": """**Question.** Same setup as 5.7a ($n_{\\text{LE4}} = 55$, $n_{\\text{GT4}} = 45$). Estimate the standard error of $\\bar X_{\\text{GT4}} - \\bar X_{\\text{LE4}}$ in three more scenarios: (i) **known unequal** variances $\\sigma^2_{\\text{LE4}} = 1.2$, $\\sigma^2_{\\text{GT4}} = 1.7$; (ii) **unknown** variances that are **not** assumed equal (Welch — sample variances $s^2_{\\text{LE4}} = 1.6$, $s^2_{\\text{GT4}} = 1.7$); (iii) **unknown** variances that **are** assumed equal (pooled variance).

![Ex 5.7b question](statistics/images/ex5/questions/ex5_7b_question.png)

---

**AI walkthrough.** Two **independent** groups (LE4 vs GT4), so variances add — but the SE formula switches with what we assume about each group's variance.

1. **Independence kills the covariance.** $\\Var(\\bar X_{\\text{GT4}} - \\bar X_{\\text{LE4}}) = \\Var(\\bar X_{\\text{GT4}}) + \\Var(\\bar X_{\\text{LE4}}) = \\sigma^2_{\\text{GT4}}/n_{\\text{GT4}} + \\sigma^2_{\\text{LE4}}/n_{\\text{LE4}}$ — no covariance term because the two pizzeria samples are drawn independently.
2. **(i) Known, unequal $\\sigma^2$.** Just plug the known numbers straight in: $\\sqrt{1.7/45 + 1.2/55} = \\sqrt{0.0378 + 0.0218} \\approx \\sqrt{0.0596} \\approx 0.2441$. This is **exact**, not estimated.
3. **(ii) Unknown, NOT equal (Welch).** Replace each $\\sigma^2$ with the sample analogue $s^2$ — same formula, now an **estimate** of the SE: $\\sqrt{1.7/45 + 1.6/55} \\approx 0.2586$.
4. **(iii) Unknown, ASSUMED equal (pooled).** Both groups now estimate one common $\\sigma^2$ — combine via a weighted average with weights $(n-1)$: $s^2_{\\text{pool}} = (44\\cdot 1.7 + 54\\cdot 1.6)/98 \\approx 1.6449$. Plug into the equal-variance form: $\\sqrt{s^2_{\\text{pool}}(1/45 + 1/55)} \\approx 0.2578$.
5. **Why pooled $<$ Welch here.** Pooled "borrows" both samples to estimate one $\\sigma^2$, so its variance estimate has more effective degrees of freedom (98 vs the Welch denominator). When the true variances are close, this pays off; when they aren't, pooled is biased and Welch is the safe default.

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
## [1] 0.2578
```

**Take-away.** When sample variances are similar, Welch and pooled estimators give nearly identical SEs; pooled is more efficient if the equal-variance assumption holds, Welch is safer otherwise.

---

**Reference answer.**

![Ex 5.7b answer](statistics/images/ex5/answers/ex5_7b_answer.png)
""", "images": [
    "statistics/images/ex5/questions/ex5_7b_question.png",
    "statistics/images/ex5/answers/ex5_7b_answer.png",
]}

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
