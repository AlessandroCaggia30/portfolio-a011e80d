"""Ex 6 — Inferential statistics: confidence intervals & hypothesis tests (CI focus)."""

ex6 = {}

ex6["6_1a"] = {"title": "Ex 6.1a — 95% CI for mean NrSkills (Italian developers)",
"content": """**Question.** What is the estimate of the average number of computer skills (`NrSkills`) of Italian developers? Build the 95% confidence interval for the considered mean, clarifying whether assumptions are needed and what is the distribution used to build the interval. Also provide an interpretation of the interval, with a specific focus on the meaning of the confidence level. (`Developers_ITA`)

---

**Answer.** Point estimate = sample mean $\\bar x \\approx 19.15$. The 95% CI for the mean with **unknown variance** can be built without special assumptions on the population distribution because $n$ is large enough to apply the **CLT**. The interval is $[18.37, 19.93]$ and it is the same irrespective of whether the normal or Student's $t$ approximation is used, because the number of degrees of freedom is so high that the percentiles of the two distributions are almost the same. With confidence 95% we conclude that the average number of skills of Italian developers is between 18.37 and 19.93. The 95% refers to the *procedure*: 95% of samples drawn this way produce an interval that contains the parameter; we cannot know whether the specific sample at hand led to one that does.

```r
distr.summary.x(NrSkills, data=Developers_ITA)     # mean ~ 19.15
CI.mean(NrSkills, conf.level=0.95, data=Developers_ITA)
```
""", "images": [
    "statistics/images/ex6/ex6_1_question.png",
    "statistics/images/ex6/ex6_1a_ai.png",
    "statistics/images/ex6/ex6_1a_answer.png",
]}

ex6["6_1b"] = {"title": "Ex 6.1b — Point estimate & ME from interval (German devs)",
"content": """**Question.**

![Ex 6.1b question](statistics/images/ex6/questions/ex6_1b_question.png)

A survey on German developers gives a 95% CI for the mean number of computer skills equal to $[16.91,\\,18.29]$.
**b1)** What is the point estimate of the average number of skills (for German developers)? What considerations about developers in the two countries?
**b2)** What is the margin of error of the interval? What considerations on the possible variation of the margin of error if a 99% confidence interval is built?

---

**Setup.** A symmetric CLT-based CI for a mean has the form $\\bar x \\pm \\text{ME}$ with $\\text{ME} = c\\cdot s/\\sqrt n$, where $c$ is the reliability factor ($z_{1-\\alpha/2}$ or $t_{1-\\alpha/2,n-1}$). Two algebraic identities follow immediately from the CI's symmetry around $\\bar x$:
$$\\bar x \\;=\\; \\tfrac{L+U}{2}, \\qquad \\text{ME} \\;=\\; \\tfrac{U-L}{2},$$
with $L,U$ the lower/upper bounds. So even *without* access to the raw sample we can recover the point estimate and the margin of error directly from a printed CI.

---

**AI walkthrough.**

**b1) Point estimate.** Midpoint of $[16.91,\\,18.29]$:
$$\\bar x_{GER} \\;=\\; \\tfrac{16.91+18.29}{2} \\;=\\; \\tfrac{35.20}{2} \\;=\\; 17.60.$$
*Cross-country comparison.* The German interval $[16.91,\\,18.29]$ lies entirely **below** the Italian interval $[18.37,\\,19.93]$ from 6.1a — they do **not overlap**. Non-overlap of two CIs at the same level is a *sufficient* (though conservative) condition for the underlying means to differ significantly: at 95% confidence the average number of computer skills of German developers is strictly less than that of Italian developers (by between $18.37-18.29 = 0.08$ and $19.93-16.91 = 3.02$ skills, depending on the closest/farthest alignment of the two CIs).

**b2) Margin of error.** Half-width of the German CI:
$$\\text{ME}_{95} \\;=\\; \\tfrac{18.29-16.91}{2} \\;=\\; \\tfrac{1.38}{2} \\;=\\; 0.69.$$
*Effect of raising confidence to 99%.* The ME factors into $c \\cdot \\text{SE}$. The standard error $\\text{SE} = s/\\sqrt n$ depends **only on the sample**, not on the confidence level, so it does not change. The reliability factor does: $z_{0.975}=1.960 \\to z_{0.995}=2.576$, an inflation of
$$\\frac{2.576}{1.960} \\;\\approx\\; 1.314.$$
Hence $\\text{ME}_{99} \\approx 1.314 \\cdot 0.69 \\approx 0.907$, and the implied 99% CI is $17.60 \\pm 0.91 = [16.69,\\,18.51]$ — about 31% wider than the 95% one. **General rule:** *higher confidence ⇒ wider interval*, because we demand the procedure to cover the parameter in a larger fraction of repeated samples and must therefore admit more values as plausible.

*Take-aways.* (i) From any symmetric CI you can always read off $\\bar x$ (midpoint) and $\\text{ME}$ (half-width) — no raw data needed. (ii) Two non-overlapping same-level CIs imply a significant difference in the means at that level; overlap is *not* necessarily evidence of no difference. (iii) Raising the confidence level scales the ME by $z_{\\text{new}}/z_{\\text{old}}$ while leaving the SE untouched.

---

**Answer.**
- **b1)** $\\bar x_{GER} = (16.91+18.29)/2 = 17.60$. The German CI lies entirely below the Italian CI, so German developers have on average **fewer** computer skills than Italians (significantly so at 95% confidence).
- **b2)** $\\text{ME}_{95} = (18.29-16.91)/2 = 0.69$. Increasing the confidence level from 95% to 99% leaves the SE unchanged but raises the reliability factor from $1.96$ to $2.576$; the ME — and therefore the interval width — grows by a factor $\\approx 1.31$, giving $\\text{ME}_{99} \\approx 0.91$.

```r
# b1) Point estimate = midpoint of the CI
L <- 16.91; U <- 18.29
xbar_GER <- (L + U)/2;  xbar_GER           # 17.60

# Cross-country check: GER interval lies below ITA interval [18.37, 19.93]
xbar_ITA <- (18.37 + 19.93)/2;  xbar_ITA   # 19.15
xbar_ITA - xbar_GER                        # ~ 1.55 fewer skills on average

# b2) Margin of error = half-width
ME_95 <- (U - L)/2;  ME_95                 # 0.69

# Reliability factors and the 95% -> 99% inflation
z_95 <- qnorm(0.975);  z_99 <- qnorm(0.995)
z_95;  z_99                                # 1.960 ; 2.576
infl <- z_99 / z_95;  infl                 # ~ 1.314

# Implied 99% CI (same SE, larger multiplier)
ME_99 <- ME_95 * infl;  ME_99              # ~ 0.907
c(xbar_GER - ME_99, xbar_GER + ME_99)      # ~ [16.69, 18.51]
```

---

**Reference answer.**

![Ex 6.1b answer](statistics/images/ex6/answers/ex6_1b_answer.png)
""", "images": [
    "statistics/images/ex6/questions/ex6_1b_question.png",
    "statistics/images/ex6/answers/ex6_1b_answer.png",
]}

ex6["6_1c"] = {"title": "Ex 6.1c — 95% CI for the proportion of Hybrid workers (ITA)",
"content": """**Question.** Estimate the proportion of (Italian) developers working in a *Hybrid* mode (`WorkingMode == "Hybrid"`). Obtain the 95% CI for the parameter of interest, and specify how it was determined (round results to 3 decimals).

---

**Setup.** The parameter of interest is the population proportion $p = \\Pr(\\text{WorkingMode} = \\text{Hybrid})$. Code each observation as a Bernoulli indicator $Y_i = \\mathbf 1\\{\\text{Hybrid}\\}$; the sample proportion $\\hat p = \\bar Y = X/n$ (with $X = \\sum Y_i \\sim \\mathrm{Bin}(n, p)$) is the natural estimator. Its standard error and the CLT-based CI are
$$SE(\\hat p) = \\sqrt{\\tfrac{\\hat p(1-\\hat p)}{n}}, \\qquad \\hat p \\pm z_{1-\\alpha/2}\\,SE(\\hat p).$$
Two ingredients: (i) a **random sample** from the target population; (ii) $n$ large enough for the normal approximation — the textbook rule of thumb is $n\\hat p \\ge 5$ **and** $n(1-\\hat p) \\ge 5$.

**AI walkthrough.** Step by step:
1. **Point estimate.** Count Hybrid workers and divide by $n = 802$: $\\hat p = X/n = 0.47$.
2. **CLT check.** $n\\hat p = 802 \\cdot 0.47 \\approx 377$ and $n(1-\\hat p) \\approx 425$ — both far above 5, so the normal approximation is essentially exact (the binomial PMF and the normal density on the $\\hat p$ scale overlap, see left panel of the AI plot).
3. **Standard error.** $SE(\\hat p) = \\sqrt{0.47 \\cdot 0.53 / 802} \\approx 0.0176 \\approx 0.018$.
4. **Reliability factor.** With $\\alpha = 0.05$, $z_{0.975} = \\Phi^{-1}(0.975) \\approx 1.960$ (`qnorm(0.975)`).
5. **Margin of error.** $ME = z_{0.975}\\,SE(\\hat p) = 1.960 \\cdot 0.0176 \\approx 0.0345$.
6. **CI.** $\\hat p \\pm ME = 0.47 \\pm 0.0345 \\;\\Rightarrow\\; [0.436,\\;0.505]$ (rounded to 3 decimals).

**Answer.** The point estimate is the **sample proportion** $\\hat p = 0.47$. The sample size is large enough to apply the CLT, so the 95% CI is built by adding/subtracting from $\\hat p$ the product of $SE(\\hat p) = \\sqrt{\\hat p(1-\\hat p)/n} \\approx 0.018$ and the $0.975$ percentile of the standard normal:
$$\\hat p \\pm 1.96 \\cdot SE(\\hat p) = 0.47 \\pm 0.035 = [0.436,\\;0.505].$$
With 95% confidence the true share of Hybrid workers among Italian developers lies between 43.6% and 50.5%; the interval **straddles 0.5**, so the data are compatible with a roughly even split between Hybrid and non-Hybrid arrangements.

```r
CI.prop(WorkingMode, success="Hybrid", conf.level=0.95,
        digits=3, data=Developers_ITA)
# Manual check:
phat <- 0.47
n    <- 802
n*phat; n*(1-phat)                       # ~377 and ~425 -- both >> 5, CLT OK
se_p <- sqrt(phat*(1-phat)/n)            # ~ 0.0176
phat + c(-1,1) * qnorm(0.975) * se_p     # [0.436, 0.505]
```
""", "images": [
    "statistics/images/ex6/questions/ex6_1c_question.png",
    "statistics/images/ex6/ex6_1c_ai.png",
    "statistics/images/ex6/answers/ex6_1c_answer.png",
]}

ex6["6_1d"] = {"title": "Ex 6.1d — Validity of a CI from a self-selected social-channel survey",
"content": """**Question.** A young developer believes the share of developers working in hybrid mode is actually **lower** than the estimated one. He runs a survey on one social channel (dedicated to developers) and gets 25 answers, 8 of which are in hybrid mode. Discuss whether it is possible/reasonable to provide a point or interval (at 95%) estimate for the proportion based on this survey; if it is, specify the analytic expressions of the estimates.

---

**Setup.** The previous CI in 6.1c was based on a *random* sample of 802 Italian developers and used the CLT-based formula
$$\\hat p \\pm z_{1-\\alpha/2}\\,\\sqrt{\\tfrac{\\hat p(1-\\hat p)}{n}}.$$
Two conditions are required: (i) the sample is a **random sample** from the target population; (ii) $n$ is **large enough** for the normal approximation — the usual rule of thumb is $n\\hat p \\ge 5$ and $n(1-\\hat p) \\ge 5$.

**AI walkthrough.** Step by step:
1. **Point estimate** is mechanical: $\\hat p = x/n = 8/25 = 0.32$. It can *always* be computed from any sample.
2. **Target population** = all developers. **Sampling frame** = followers of one developer-focused social channel — a strict sub-population. The frame is **misaligned** with the target $\\Rightarrow$ *coverage bias*.
3. **Self-selection.** Respondents choose to reply $\\Rightarrow$ those with strong opinions (e.g., dissatisfied with current work mode) over-represent themselves $\\Rightarrow$ $\\hat p$ is biased, $E[\\hat p] \\ne p$.
4. **Sample size.** $n\\hat p = 25 \\cdot 0.32 = 8 \\ge 5$ and $n(1-\\hat p) = 17 \\ge 5$ — the CLT rule of thumb is *just* met, but with $n = 25$ the normal approximation is fragile.
5. **Verdict.** The CI formula can be written down, but its 95% coverage guarantee does **not** hold — points (2) and (3) violate the random-sample assumption.

**Answer.** A **point estimate** can always be computed: $\\hat p = 8/25 = 0.32$.
For a **95% CI** there are critical issues:
1. The sample is **not drawn from the entire population** but only from a sub-population of developers following that specific social channel — results are not representative of the general developer population.
2. Respondents **self-select** (they choose to answer), so the sample is not a random sample — those who reply may be those most interested in the topic and the estimate $\\hat p = 0.32$ may be biased.
3. The sample size $n = 25$ is borderline for the CLT-based normal approximation for proportions.

So a reliable CI cannot really be built from this survey. Formally one would still write
$$\\hat p \\pm z_{0.975}\\,\\sqrt{\\tfrac{\\hat p(1-\\hat p)}{n}} = 0.32 \\pm 1.96\\sqrt{\\tfrac{0.32 \\cdot 0.68}{25}} \\approx [0.137, 0.503],$$
but this interval inherits the bias of the sampling design and should not be trusted.

```r
# Point estimate is fine
phat <- 8/25; phat                       # 0.32
# CLT rule-of-thumb check
n <- 25; n*phat; n*(1-phat)              # 8 and 17 -- both >= 5
# Formal CI formula (with caveats above):
se_p <- sqrt(phat*(1-phat)/n)            # ~ 0.0933
phat + c(-1,1) * qnorm(0.975) * se_p     # ~ [0.137, 0.503]
```
""", "images": [
    "statistics/images/ex6/ex6_1_question.png",
]}

ex6["6_2a"] = {"title": "Ex 6.2a — Mean difference NA vs EU sales (vgsales, Action)",
"content": """**Question.** It is of interest to estimate the average difference in the copies of videogames of genre *Action* in the North American and EU markets. Estimate the mean difference. What assumption on the relationship between the two variables in the population — or on the relationship between the two samples — would you propose?

---

**Setup.** Two columns (`NA_Sales`, `EU_Sales`) are recorded for the *same* game, so the observations come in **matched pairs**. Define the within-game difference $D_i = X_{NA,i} - X_{EU,i}$; its sample mean estimates the population mean difference
$$\\bar D = \\bar X_{NA} - \\bar X_{EU}, \\qquad E[\\bar D] = \\mu_{NA} - \\mu_{EU}.$$
With independent samples we would use $\\operatorname{Var}(\\bar X_{NA} - \\bar X_{EU}) = \\sigma_{NA}^2/n + \\sigma_{EU}^2/n$, but here that is wrong: a popular title inflates *both* NA and EU sales, so $\\operatorname{Cov}(X_{NA}, X_{EU}) > 0$ and the correct variance is
$$\\operatorname{Var}(\\bar D) = \\tfrac{1}{n}\\bigl(\\sigma_{NA}^2 + \\sigma_{EU}^2 - 2\\,\\operatorname{Cov}(X_{NA}, X_{EU})\\bigr) < \\tfrac{\\sigma_{NA}^2 + \\sigma_{EU}^2}{n}.$$
Pairing therefore **shrinks** the SE relative to an (incorrect) independent-samples treatment.

**AI walkthrough.** Step by step:
1. **Filter** the dataframe to `Genre == "Action"`. Each row is one Action title and carries both `NA_Sales` and `EU_Sales`.
2. **Compute the two sample means** $\\bar x_{NA}$ and $\\bar x_{EU}$.
3. **Take the difference** $\\bar x_{NA} - \\bar x_{EU}$; equivalently compute $D_i$ first and take its mean — the two are numerically identical because the mean is linear: $\\overline{X-Y} = \\bar X - \\bar Y$.
4. **Assumption.** Same titles in both markets $\\Rightarrow$ NA and EU sales are *positively correlated* $\\Rightarrow$ samples are **paired**, not independent. The CI/HT in 6.2b must use `type="paired"`.

**Answer.** The point estimate is $\\bar D = \\bar X_{NA} - \\bar X_{EU}$. The appropriate assumption is that the two samples are **paired** (matched on game): independence between NA and EU sales is implausible because a single title drives both markets.

```r
xbar_NA <- mean(vgsales$NA_Sales[vgsales$Genre=="Action"])
xbar_EU <- mean(vgsales$EU_Sales[vgsales$Genre=="Action"])
xbar_NA - xbar_EU                                            # point estimate of mu_NA - mu_EU
# Equivalent matched-pair form (one D per game):
D <- vgsales$NA_Sales[vgsales$Genre=="Action"] -
     vgsales$EU_Sales[vgsales$Genre=="Action"]
mean(D)                                                      # same number
cor(vgsales$NA_Sales[vgsales$Genre=="Action"],
    vgsales$EU_Sales[vgsales$Genre=="Action"])               # strongly positive => paired
```
""", "images": [
    "statistics/images/ex6/questions/ex6_2a_question.png",
    "statistics/images/ex6/ex6_2a_ai.png",
    "statistics/images/ex6/answers/ex6_2a_answer.png",
]}

ex6["6_2b"] = {"title": "Ex 6.2b — 98% paired CI for NA-EU mean difference",
"content": """**Question.** Build a 98% confidence interval for the mean difference. *(c)* Comment on the claim: "Since NA and EU markets are different, the two samples can be assumed independent."

---

**Answer.** Use the **paired** CI (`type="paired"`). The two samples are *not* independent: the same titles appear in both markets, so a popular game inflates both NA and EU sales — the correlation will be strongly positive.

```r
CI.diffmean(x=vgsales$NA_Sales[vgsales$Genre=="Action"],
            y=vgsales$EU_Sales[vgsales$Genre=="Action"],
            type="paired", conf.level=0.98, digits=4)
# Correlation between NA and EU sales (Action)
cor(vgsales$NA_Sales[vgsales$Genre=="Action"],
    vgsales$EU_Sales[vgsales$Genre=="Action"])
```
""", "images": []}

ex6["6_3a"] = {"title": "Ex 6.3a — 95% CI for `Global_Sales` mean (vgsales, 1990 vs 2010)",
"content": """**Question.**

![Ex 6.3a question](statistics/images/ex6/questions/ex6_3a_question.png)

An analyst is interested in evaluating the mean number of copies sold globally (variable `Global_Sales`) in two selected reference years, **1990** and **2010**. Calculate a confidence interval (using 3 decimal places) with confidence level $0.95$ for the mean number of global copies for each of the two years, specifying any assumptions that may be necessary in each case. Explain the observed differences in the margins of error of the proposed intervals.

---

**Setup.** Two sub-samples, indexed by year. We need a CI for $\\mu = E[\\textit{Global\\_Sales}]$ in each year. Behaviour depends on $n_t$:
- **1990** has $n_{1990} = 16$ — *small*. CLT does *not* kick in: a CI for the mean requires the population to be (approximately) **normal**, and uses the **Student's $t$** percentile with $n-1 = 15$ degrees of freedom.
- **2010** has $n_{2010} = 1259$ — *very large*. CLT applies regardless of the population shape, so the normal-based CI is valid; the $t_{n-1}$ and $z$ percentiles are numerically indistinguishable here ($t_{0.975,1258} \\approx 1.962$ vs $z_{0.975} = 1.960$).

The 95% CI is in both cases $\\bar x \\pm c \\cdot s/\\sqrt n$, with $c = t_{0.975,\\,n-1}$ (small $n$) or $c \\approx z_{0.975}=1.96$ (large $n$).

---

**Answer.**
- **1990** ($n = 16$, assuming `Global_Sales` $\\sim \\mathcal N$): $t_{0.975,15} \\approx 2.131$. The interval is $(0.516,\\;5.658)$ — **wide** because $n$ is tiny *and* sales of 1990 titles are very heterogeneous ($s_{1990}^2 \\approx 4.825$).
- **2010** ($n = 1259$, CLT): the interval is $(0.405,\\;0.548)$ — **narrow** and almost identical whether built with $t$ or normal percentiles.

**Why the ME differs.** Margin of error is $\\text{ME} = c \\cdot s/\\sqrt n$. Three forces all push 1990's ME up and 2010's ME down:
1. **Reliability factor $c$:** $t_{0.975,15} \\approx 2.131$ (1990) vs $\\approx 1.96$ (2010) — Student's $t$ has fatter tails for small df.
2. **Population variance:** the 1990 sample has $s^2_{1990} \\approx 4.825$ (a few blockbusters drive heterogeneity), while $s^2_{2010} \\approx 1.295$ — much more concentrated. Less heterogeneity in 2010 $\\Rightarrow$ smaller $s$.
3. **Sample size:** $\\sqrt n$ is $4$ for 1990 but $\\approx 35.5$ for 2010 — the SE $s/\\sqrt n$ collapses for 2010.

All three forces operate **in the same direction**, producing a much sharper interval estimate (smaller width) for 2010 than for 1990.

```r
# CI for each year (vgsales)
CI.mean(Global_Sales, conf.level=0.95, digits=3, data=vgsales[vgsales$Year==1990,])
CI.mean(Global_Sales, conf.level=0.95, digits=3, data=vgsales[vgsales$Year==2010,])

# Sanity check: sample size, mean, variance per year
n_1990  <- sum(vgsales$Year==1990); n_2010 <- sum(vgsales$Year==2010)
x_1990  <- vgsales$Global_Sales[vgsales$Year==1990]
x_2010  <- vgsales$Global_Sales[vgsales$Year==2010]
c(n_1990, mean(x_1990), var(x_1990))      # ~ 16, 3.087, 4.825
c(n_2010, mean(x_2010), var(x_2010))      # ~ 1259, 0.477, 1.295

# Reliability factors compared
qt(0.975, df=15)                          # 2.1314  (Student's t, 1990)
qt(0.975, df=1258)                        # 1.9619  (Student's t, 2010)
qnorm(0.975)                              # 1.9600  (normal, large-n approx)
```
""", "images": [
    "statistics/images/ex6/questions/ex6_3a_question.png",
    "statistics/images/ex6/answers/ex6_3a_answer.png",
]}

ex6["6_3b1"] = {"title": "Ex 6.3b1 — Count and sample proportion of female customers (DS)",
"content": """**Question.** How many female customers are in the dataset? Compute the sample proportion $\\hat p$ of females.

---

**Answer.** Count females via `sum(DS$Gender=="Female")` and divide by $n=750$.

```r
n.females <- sum(DS$Gender=="Female"); n.females
table(DS$Gender)         # alternative
nrow(DS)                 # number of cases
n.females / 750          # sample proportion
```
""", "images": []}

ex6["6_3b2"] = {"title": "Ex 6.3b2 — 99% CI for female proportion + sample-size planning",
"content": """**Question.**

![Ex 6.3b2 question](statistics/images/ex6/questions/ex6_3b2_question.png)

Build a 99% CI for the proportion of female customers. Then determine the minimum sample size to achieve a target margin of error at 95% confidence.

---

**Answer.** Use `CI.prop`. For the sample-size invert
$$\\text{ME} = z_{1-\\alpha/2}\\sqrt{\\hat p(1-\\hat p)/n} \\le \\text{ME}^\\star \\Rightarrow n \\ge (z_{1-\\alpha/2}\\hat p_{\\max}/\\text{ME}^\\star)^2.$$
The worst-case variance is at $\\hat p = 0.5$ (conservative formula).

```r
CI.prop(Gender, success="Female", data=DS)
qnorm(0.995)
z_025 <- qnorm(0.975)            # 95% two-sided multiplier
(z_025 * 0.5 / 0.11)^2           # minimum sample size (conservative)
```

---

**AI walkthrough.** With $n = 750$ customers, $\\hat p = 389/750 \\approx 0.5187$ (sample share of females from b1). The large-$n$ Wald CI for a proportion is
$$\\hat p \\;\\pm\\; z_{1-\\alpha/2}\\,\\sqrt{\\tfrac{\\hat p(1-\\hat p)}{n}}.$$

*99% CI for the female proportion.* $z_{0.005} \\approx 2.576$ and the estimated SE is $\\sqrt{0.5187 \\cdot 0.4813 / 750} \\approx 0.01824$. The interval is $0.5187 \\pm 2.576 \\times 0.01824 = 0.5187 \\pm 0.0470 \\approx [0.472,\\,0.566]$ — it sits just above $0.5$ but **does include** values $\\le 0.5$ once we go to 99% confidence; the sample is therefore consistent with a population female share between roughly 47% and 57%.

*Sample-size planning at 95% confidence.* Invert $\\text{ME} \\le \\text{ME}^\\star$: $n \\ge (z_{1-\\alpha/2}\\sqrt{\\hat p(1-\\hat p)}/\\text{ME}^\\star)^2$. The maximum of $\\hat p(1-\\hat p)$ is $0.25$ at $\\hat p = 0.5$, giving the **conservative** lower bound $n \\ge (z_{1-\\alpha/2}\\cdot 0.5/\\text{ME}^\\star)^2$. With $z_{0.025}=1.96$ and $\\text{ME}^\\star = 0.11$: $n \\ge (1.96 \\cdot 0.5 / 0.11)^2 \\approx 79.34 \\Rightarrow n = 80$. Plugging in the sample $\\hat p \\approx 0.5187$ in place of $0.5$ gives essentially the same number, because $\\hat p$ is very close to the worst case.

*Take-aways.* (i) Raising confidence from 95% to 99% widens the CI by the factor $2.576/1.96 \\approx 1.31$ — the SE itself does not move. (ii) The minimum-$n$ formula scales as $n \\propto 1/\\text{ME}^{\\star\\,2}$: halving the target ME quadruples the required sample size. (iii) Use $\\hat p = 0.5$ as the safe default whenever no prior estimate is available — it gives the largest $n$ and hence the most cautious plan.

```r
# Point estimate from b1
phat <- 389/750            # ~ 0.5187
n    <- 750

# 99% CI for the proportion (manual)
se     <- sqrt(phat*(1-phat)/n)          # ~ 0.01824
z_099  <- qnorm(0.995)                   # 2.576
ME_99  <- z_099 * se                     # ~ 0.0470
c(phat - ME_99, phat + ME_99)            # ~ [0.472, 0.566]

# Same via CI.prop
CI.prop(Gender, success="Female", conf.level=0.99, data=DS)

# Minimum n for ME <= 0.11 at 95% confidence (conservative, p=0.5)
z_095 <- qnorm(0.975)                    # 1.96
(z_095 * 0.5 / 0.11)^2                   # ~ 79.34  -> n = 80

# Plug-in version using phat from the sample
(z_095 * sqrt(phat*(1-phat)) / 0.11)^2   # ~ 79.28  -> n = 80
```

---

**Reference answer.**

![Ex 6.3b2 answer](statistics/images/ex6/answers/ex6_3b2_answer.png)
""", "images": [
    "statistics/images/ex6/questions/ex6_3b2_question.png",
    "statistics/images/ex6/answers/ex6_3b2_answer.png",
]}

ex6["6_3c"] = {"title": "Ex 6.3c — 99% CI for diff in Employment proportions (GER vs ITA)",
"content": """**Question.** Compare the proportions of `Employment == "Employed, full-time"` between German and Italian developers and build a 99% CI for the difference (`Developers_ITA` vs `Developers_GER`).

---

**Setup.** Two **independent** samples (different individuals across countries) with $n_{ITA}=802$ and $n_{GER}=820$. The large-sample (Wald) CI for the difference of proportions is
$$(\\hat p_{GER}-\\hat p_{ITA}) \\;\\pm\\; z_{1-\\alpha/2}\\,\\sqrt{\\tfrac{\\hat p_{GER}(1-\\hat p_{GER})}{n_{GER}} + \\tfrac{\\hat p_{ITA}(1-\\hat p_{ITA})}{n_{ITA}}}.$$
Independence allows summing the two variances (no covariance term), unlike the paired case of 6.2.

**AI walkthrough.**
1. **Read the marginals.** `distr.table.x(Employment)` on each country gives the share of full-timers. Suppose the GER table reports a full-time share of $\\hat p_{GER}\\approx 0.097$ and the ITA table $\\hat p_{ITA}\\approx 0.150$ (re-read the printed table to assign the correct cell — beware that "Employed, full-time" may not be the first row).
2. **Point estimate.** $\\hat p_{GER}-\\hat p_{ITA} \\approx 0.097-0.150 = -0.053$. Negative sign $\\Rightarrow$ German full-time share is **lower** than the Italian one.
3. **Standard error.** Independence gives $\\operatorname{SE} = \\sqrt{\\hat p_{GER}(1-\\hat p_{GER})/820 + \\hat p_{ITA}(1-\\hat p_{ITA})/802} \\approx \\sqrt{0.097\\cdot 0.903/820 + 0.150\\cdot 0.850/802} \\approx \\sqrt{1.07\\cdot 10^{-4} + 1.59\\cdot 10^{-4}} \\approx 0.0163$.
4. **99% multiplier.** $z_{0.995}\\approx 2.576$, so $\\text{ME} \\approx 2.576\\times 0.0163 \\approx 0.0420$.
5. **Interval.** $-0.053\\pm 0.042 \\approx [-0.095,\\,-0.011]$. The CI sits **entirely below 0** $\\Rightarrow$ at 99% confidence GER's full-time share is between 1.1 and 9.5 percentage points lower than ITA's.
6. **Validity check.** Wald requires $n_i\\hat p_i$ and $n_i(1-\\hat p_i)$ all $\\ge 5$; here the smallest count is $820\\cdot 0.097\\approx 80$, well above the threshold.

*Take-aways.* (i) Compare **two-sample proportion** CIs by checking whether 0 is inside: outside $\\Rightarrow$ significant gap at the chosen level. (ii) Independence means **no covariance term** in the SE — opposite to the paired case 6.2 where the SE shrank via positive correlation. (iii) Sample sizes are roughly balanced, so SE is dominated by the variance terms, not by $1/n$ asymmetry.

**Answer.** Two independent samples give a 99% CI for $p_{GER}-p_{ITA}$ that excludes 0 — strong evidence of a country gap in full-time employment.

```r
# Marginal shares of "Employed, full-time" in each country
distr.table.x(Developers_ITA$Employment, f.digits=3)
distr.table.x(Developers_GER$Employment, f.digits=3)

phat_GER  <- 0.097                       # full-time share (GER)
phat_ITA  <- 0.150                       # full-time share (ITA)
n_GER     <- 820;  n_ITA <- 802
diff.prop <- phat_GER - phat_ITA         # ~ -0.053

SE.diff   <- sqrt( phat_GER*(1-phat_GER)/n_GER +
                   phat_ITA*(1-phat_ITA)/n_ITA )   # ~ 0.0163
ME <- qnorm(0.995) * SE.diff              # ~ 0.042
c(diff.prop - ME, diff.prop + ME)         # ~ [-0.095, -0.011]

# Same via CI.diffprop (course package), if available
CI.diffprop(x=Developers_GER$Employment, y=Developers_ITA$Employment,
            success="Employed, full-time", conf.level=0.99, digits=3)
```
""", "images": []}

ex6["6_3d"] = {"title": "Ex 6.3d — 99% CI for AmountSpent: Married vs Single (Close, 0 children)",
"content": """**Question.** Build a 99% CI for the difference in mean `AmountSpent` between Married and Single customers, restricted to the sub-population with `Location == "Close"` and `Children == 0`.

---

**Setup.** Two **independent** sub-samples — different individuals belong to the Married and Single groups, so $\\operatorname{Cov}(\\bar X_M,\\bar X_S)=0$. With unknown (possibly unequal) variances and reasonably sized sub-samples, the Welch CI is
$$(\\bar X_M - \\bar X_S) \\;\\pm\\; t^{*}_{1-\\alpha/2,\\,\\nu}\\,\\sqrt{\\tfrac{s_M^{2}}{n_M} + \\tfrac{s_S^{2}}{n_S}},$$
with Satterthwaite df $\\nu = \\dfrac{(s_M^{2}/n_M + s_S^{2}/n_S)^2}{(s_M^{2}/n_M)^2/(n_M-1) + (s_S^{2}/n_S)^2/(n_S-1)}$. `CI.diffmean` defaults to this Welch form — the safe choice unless equal variances are explicitly assumed.

---

**AI walkthrough.** Step by step:
1. **Define the sub-population.** Filter `DS` to `Location=="Close" & Children==0`. This isolates one segment, then we split it by `Married`.
2. **Two groups, two samples.** Within the slice each customer is Married *or* Single — never both — so the two `AmountSpent` vectors are independent.
3. **Why a CI for the *difference*?** We want $\\Delta = \\mu_M - \\mu_S$, not the two means in isolation. A CI for $\\Delta$ directly answers "is there a marital-status effect on spending in this segment?" If it excludes 0 there is evidence of a difference at the chosen level.
4. **Why $t$ and not $z$?** The Welch CI uses $t^{*}_{\\nu}$ because $\\sigma_M,\\sigma_S$ are estimated from data. With $\\alpha=0.01$ the multiplier is `qt(0.995, df=nu)`; for large $\\nu$ it is essentially `qnorm(0.995) = 2.576`.
5. **Read the output.** R prints $\\bar x_M - \\bar x_S$, the SE, the df, the multiplier, and the bounds. Both bounds positive $\\Rightarrow$ Married spend more on average; both negative $\\Rightarrow$ Single spend more; straddling 0 $\\Rightarrow$ no evidence of a difference at the 1% level.
6. **Equivalence of the two approaches.** Explicit subsets and `by=Married` return the **same** numbers — `by=` just splits internally. Use it to avoid copy-paste bugs in the filter; use the explicit form when you need the vectors for downstream checks (`length`, `mean`, `sd`).

---

**Answer.** Use `CI.diffmean` after sub-setting, or via `by=`. With confidence 99% the bounds tell us whether the typical Married-vs-Single spending gap (inside the Close / no-children segment) is statistically distinguishable from zero.

```r
# Approach 1: explicit subsets
sel.M <- DS$Location=="Close" & DS$Children==0 & DS$Married=="Married"
sel.S <- DS$Location=="Close" & DS$Children==0 & DS$Married=="Single"
length(DS$AmountSpent[sel.M]); length(DS$AmountSpent[sel.S])    # group sizes
CI.diffmean(x=DS$AmountSpent[sel.M], y=DS$AmountSpent[sel.S],
            conf.level=0.99, digits=3)

# Approach 2: by= argument (cleaner — same result)
sel.sub <- DS$Location=="Close" & DS$Children==0
CI.diffmean(x=DS$AmountSpent[sel.sub], by=DS$Married[sel.sub],
            conf.level=0.99, digits=3)

# Manual check (Welch SE + t multiplier)
xM <- DS$AmountSpent[sel.M]; xS <- DS$AmountSpent[sel.S]
nM <- length(xM); nS <- length(xS)
se <- sqrt(var(xM)/nM + var(xS)/nS)
nu <- (var(xM)/nM + var(xS)/nS)^2 /
      ((var(xM)/nM)^2/(nM-1) + (var(xS)/nS)^2/(nS-1))
ME <- qt(0.995, df=nu) * se
c(mean(xM) - mean(xS) - ME, mean(xM) - mean(xS) + ME)
```
""", "images": []}

ex6["6_4a"] = {"title": "Ex 6.4a — Pooled-variance CI for vgsales mean (NF vs F)",
"content": """**Question.** Compute the 95% pooled-variance CI for the difference of two-group means, with $\\bar x_{NF}=90.7,\\ \\bar x_F=87.2,\\ s_{NF}=5.4,\\ s_F=4.8,\\ n_{NF}=n_F=10$. Then redo the interval in the **known-variance** version ($\\sigma_{NF}=5.2,\\ \\sigma_F=5$) and compare.

---

**Setup.** Two **independent** samples assumed drawn from populations with the *same* (unknown) variance. The natural variance estimator is the pooled one:
$$S_p^{\\,2} \\;=\\; \\frac{(n_{NF}-1)s_{NF}^{\\,2} + (n_F-1)s_F^{\\,2}}{n_{NF}+n_F-2}, \\qquad \\text{SE}(\\bar X_{NF}-\\bar X_F) \\;=\\; S_p\\sqrt{\\tfrac{1}{n_{NF}}+\\tfrac{1}{n_F}}.$$
Under the (small-sample) normality assumption the pivot follows a Student's $t$ with $\\nu = n_{NF}+n_F-2 = 18$ df, so the CI is
$$(\\bar X_{NF}-\\bar X_F) \\;\\pm\\; t_{1-\\alpha/2,\\,\\nu}\\,\\text{SE}.$$
If the population variances are **known**, the SE is built directly from $\\sigma_{NF},\\sigma_F$ and the multiplier becomes $z_{1-\\alpha/2}$.

**AI walkthrough.** Step by step:
1. **Point estimate.** $\\bar D = \\bar x_{NF} - \\bar x_F = 90.7 - 87.2 = 3.5$.
2. **Pooled variance.** With $n_{NF}=n_F=10$, $S_p^{\\,2}$ is the **simple average** of the two sample variances: $S_p^{\\,2} = \\tfrac{9\\cdot 5.4^{2} + 9\\cdot 4.8^{2}}{18} = \\tfrac{5.4^{2}+4.8^{2}}{2} = \\tfrac{29.16+23.04}{2} = 26.10$, so $S_p \\approx 5.109$.
3. **Pooled SE.** $\\text{SE} = S_p\\sqrt{1/10+1/10} = 5.109\\cdot\\sqrt{0.2} \\approx 2.285$.
4. **Critical value.** $t_{0.975,\\,18} \\approx 2.1009$ (heavier-tailed than $z_{0.975}=1.96$ because of small $n$).
5. **Margin & CI (pooled-$t$).** $\\text{ME}_t = 2.1009\\times 2.285 \\approx 4.80$, giving $[\\,-1.30,\\,8.30\\,]$. The interval **contains 0** at the 95% level — the data are consistent with no NF-vs-F gap.
6. **Known-$\\sigma$ pathway.** $\\text{SE}_z = \\sqrt{\\sigma_{NF}^{2}/10 + \\sigma_F^{2}/10} = \\sqrt{2.704 + 2.5} \\approx 2.294$ — essentially the same SE as the pooled one because $s\\approx\\sigma$. The multiplier shrinks to $z_{0.975}=1.96$, so $\\text{ME}_z \\approx 4.50$, giving $[\\,-1.00,\\,8.00\\,]$.
7. **Comparison.** The known-variance interval is **narrower** because we skip the penalty for estimating $\\sigma$: the width ratio is $t_{0.975,18}/z_{0.975} \\approx 1.072$ — a 7% inflation for $n=10$. Push $n$ up and the gap vanishes ($t \\to z$). Both intervals straddle 0, so the qualitative conclusion (no significant NF-vs-F gap at 95%) is identical.

**Take-aways.** (i) Pooling is justified only when the two populations have (approximately) **the same variance**; otherwise use Welch's separate-variance SE. (ii) The $t$-vs-$z$ correction matters most at small $n$ — for $n_1=n_2=10$, $t_{0.975,18}$ is $\\approx 7\\%$ larger than $1.96$. (iii) With equal sample sizes the pooled variance is the *mean* of the two sample variances, a handy sanity check.

```r
x_bar_NF <- 90.7; x_bar_F <- 87.2
diff.bar <- x_bar_NF - x_bar_F                 # 3.5
sd_NF <- 5.4; sd_F <- 4.8

# (a) Pooled-variance t CI, df = n_NF + n_F - 2 = 18
s2_pool <- (9*sd_NF^2 + 9*sd_F^2)/18           # 26.10
sqrt(s2_pool)                                   # S_p ~ 5.109
se.diff <- sqrt(s2_pool/10 + s2_pool/10)        # ~ 2.285
qt(0.975, df=18)                                # 2.1009
ME <- qt(0.975, df=18) * se.diff                # ~ 4.80
c(diff.bar - ME, diff.bar + ME)                 # ~ [-1.30,  8.30]

# (b) Known-variance form, sigma_NF=5.2, sigma_F=5
sigma_NF <- 5.2; sigma_F <- 5
SE.diff  <- sqrt(sigma_NF^2/10 + sigma_F^2/10)  # ~ 2.294
qnorm(0.975)                                    # 1.96
ME.k <- qnorm(0.975) * SE.diff                  # ~ 4.50
c(diff.bar - ME.k, diff.bar + ME.k)             # ~ [-1.00,  8.00]

# Inflation factor for unknown sigma at n=10:
qt(0.975, df=18) / qnorm(0.975)                 # ~ 1.072  (about 7% wider)
```
""", "images": [
    "statistics/images/ex6/ex6_4a_ai.png",
]}

ex6["6_6a"] = {"title": "Ex 6.6a — 95% CI for a single proportion ($n=100$, 40 successes)",
"content": """**Question.** Build a 95% CI for the proportion of successes from a sample of $n=100$ customers, $40$ of whom answered favourably.

---

**AI walkthrough.** Let $X = \\#\\{\\text{favourable}\\} \\sim \\text{Bin}(n,p)$ and $\\hat p = X/n$ the sample proportion. For $n$ large enough that $n\\hat p \\ge 5$ and $n(1-\\hat p) \\ge 5$ (here $40$ and $60$, both $\\gg 5$), the **CLT** gives
$$\\hat p \\;\\overset{a}{\\sim}\\; \\mathcal{N}\\!\\left(p,\\; \\tfrac{p(1-p)}{n}\\right),$$
and the **Wald CI** (plug-in for the unknown $p$ in the SE) is
$$\\hat p \\;\\pm\\; z_{1-\\alpha/2}\\,\\sqrt{\\tfrac{\\hat p(1-\\hat p)}{n}}.$$

Step-by-step with $\\hat p = 40/100 = 0.40$, $\\alpha = 0.05$, $z_{0.975} = 1.96$:
- Variance: $\\tfrac{0.4 \\cdot 0.6}{100} = 2.4 \\times 10^{-3}$.
- SE: $\\sqrt{2.4 \\times 10^{-3}} \\approx 0.04899$.
- ME: $1.96 \\times 0.04899 \\approx 0.0960$.
- CI: $0.40 \\pm 0.0960 = [0.304,\\; 0.496]$.

**Answer.** Point estimate $\\hat p = 0.40$; with 95% confidence the population proportion of favourable customers lies in $[0.304,\\,0.496]$ — a fairly wide interval ($\\approx 19$ pp), reflecting the moderate sample size. The 95% refers to the *procedure*: 95% of samples drawn this way produce an interval covering the true $p$; we cannot tell whether this specific one does.

```r
phat  <- 40/100
var_p <- phat*(1-phat)/100
se    <- sqrt(var_p)
var_p; se                       # 0.0024 ; ~ 0.04899
qnorm(0.975)                    # 1.959964
ME    <- qnorm(0.975) * se      # ~ 0.0960
c(phat - ME, phat + ME)         # [0.304, 0.496]
```
""", "images": []}

ex6["6_6b"] = {"title": "Ex 6.6b — Same 95% CI with $n = 1000$",
"content": """**Question.** Build the 95% CI for the proportion again with $n=1000$ (same $\\hat p = 0.4$). Comment on how precision changes relative to the $n=100$ case.

---

**AI walkthrough.** With a large $n$ the CLT guarantees $\\hat p \\overset{a}{\\sim} \\mathcal{N}\\!\\left(p,\\;\\tfrac{p(1-p)}{n}\\right)$, so the Wald CI is
$$\\hat p \\;\\pm\\; z_{1-\\alpha/2}\\,\\sqrt{\\tfrac{\\hat p(1-\\hat p)}{n}}.$$

Plug in $\\hat p = 0.4$, $n=1000$, $z_{0.975}=1.96$:
- Variance: $\\tfrac{0.4\\cdot 0.6}{1000} = 2.4\\times 10^{-4}$.
- SE: $\\sqrt{2.4\\times 10^{-4}} \\approx 0.01549$.
- ME: $1.96 \\times 0.01549 \\approx 0.0304$.
- CI: $0.4 \\pm 0.0304 = [0.370,\\;0.430]$.

**Comparison with $n=100$.** Same $\\hat p$, same confidence level, so only the SE changes. Because $\\text{SE} \\propto 1/\\sqrt{n}$, multiplying $n$ by 10 divides the SE — and the half-width — by $\\sqrt{10}\\approx 3.16$. The interval shrinks from $[0.304,\\,0.496]$ (width $\\approx 0.192$) to $[0.370,\\,0.430]$ (width $\\approx 0.061$): a tenfold sample buys a $\\sim 3.16\\times$ precision gain (diminishing returns of $1/\\sqrt{n}$).

```r
phat <- 0.4
se_p1000  <- sqrt(phat*(1-phat)/1000)
se_p1000                              # ~ 0.01549
ME_p1000  <- qnorm(0.975) * se_p1000
ME_p1000                              # ~ 0.0304
c(phat - ME_p1000, phat + ME_p1000)   # [0.370, 0.430]
# Precision-gain factor relative to n=100:
sqrt(1000/100)                        # ~ 3.162
```
""", "images": [
    "statistics/images/ex6/questions/ex6_6b_question.png",
    "statistics/images/ex6/answers/ex6_6b_answer.png",
]}

ex6["6_6c"] = {"title": "Ex 6.6c — Lower confidence level (90% two-sided, $z_{0.05}$)",
"content": """**Question.** Re-do the proportion CI (with $\\hat p = 0.40$, $n = 100$) but using $z = q_{0.95} = 1.645$ instead of $z_{0.975} = 1.96$ — i.e. a 90% two-sided CI (or equivalently, the multiplier of a one-sided 95% bound). Compare the width to the 95% CI of part (a).

---

**AI walkthrough.** The Wald CI structure is unchanged,
$$\\hat p \\;\\pm\\; z_{1-\\alpha/2}\\,\\sqrt{\\tfrac{\\hat p(1-\\hat p)}{n}},$$
only the **confidence level** moves from $1-\\alpha = 0.95$ down to $0.90$, so the critical value drops from $z_{0.975}=1.96$ to $z_{0.95}=1.645$. The SE is identical to part (a) because $\\hat p$ and $n$ are unchanged.

Step-by-step with $\\hat p = 0.40$, $n = 100$:
- SE: $\\sqrt{\\tfrac{0.4\\cdot 0.6}{100}} = \\sqrt{2.4\\times 10^{-3}} \\approx 0.04899$.
- ME$_{90\\%}$: $1.645 \\times 0.04899 \\approx 0.0806$.
- CI: $0.40 \\pm 0.0806 = [0.319,\\,0.481]$ (width $\\approx 0.161$).

**Comparison with part (a).** Same point estimate, same SE — only the multiplier shrinks. The width ratio is
$$\\tfrac{\\text{ME}_{90\\%}}{\\text{ME}_{95\\%}} \\;=\\; \\tfrac{1.645}{1.96} \\;\\approx\\; 0.839,$$
so the 90% CI is about $16\\%$ narrower than the 95% CI ($0.192 \\to 0.161$). The price is a higher *miss rate*: only 90% of such intervals cover the true $p$ in repeated sampling, vs 95% before. **Equivalence with a one-sided 95% bound:** because $z_{0.95}$ is the $5\\%$-tail quantile, $[\\,0.40 - 1.645\\cdot\\text{SE},\\,+\\infty)$ is a one-sided 95% lower confidence bound for $p$ — the lower endpoint of the two-sided 90% CI doubles as the floor of a 95% one-sided statement.

**Answer.** 90% CI for $p$: $[0.319,\\,0.481]$. Shorter than the 95% CI of (a) by the factor $1.645/1.96$ ($\\approx 16\\%$ narrower), at the cost of 5 pp less confidence.

```r
phat <- 0.4
se   <- sqrt(phat*(1-phat)/100)
se                                # ~ 0.04899
qnorm(0.95)                       # 1.6449
ME_90 <- qnorm(0.95) * se
ME_90                             # ~ 0.0806
c(phat - ME_90, phat + ME_90)     # [0.319, 0.481]
# Width-shrink factor vs 95% CI (part a):
qnorm(0.95) / qnorm(0.975)        # ~ 0.839
```
""", "images": []}

ex6["6_6d"] = {"title": "Ex 6.6d — Sample size for ME $\\le 0.04$ at 95% confidence",
"content": """**Question.** What sample size guarantees a margin of error $\\le 0.04$ at 95% confidence, with **no prior information** on $p$?

---

**AI walkthrough.** Invert the Wald margin-of-error formula for a proportion,
$$\\text{ME} \\;=\\; z_{1-\\alpha/2}\\,\\sqrt{\\tfrac{p(1-p)}{n}} \\;\\le\\; m \\quad\\Longleftrightarrow\\quad n \\;\\ge\\; \\left(\\tfrac{z_{1-\\alpha/2}}{m}\\right)^{\\!2}\\, p(1-p).$$

Since $p$ is unknown, we need an upper bound for $p(1-p)$. The function $g(p)=p(1-p)$ is a downward parabola on $[0,1]$ that attains its **maximum** at $p=\\tfrac12$ with value $\\tfrac14$ (set $g'(p)=1-2p=0$). Plugging the **worst case** $p(1-p)=0.25$ gives the *conservative* sample size — guaranteed to meet the ME target for any true $p$.

Step-by-step with $m=0.04$, $\\alpha=0.05$, $z_{0.975}=1.96$:
- Worst-case variance term: $p(1-p)=0.25$.
- Required $n$: $n \\ge \\left(\\tfrac{1.96}{0.04}\\right)^{2}\\cdot 0.25 = 49^{2}\\cdot 0.25 = 2401 \\cdot 0.25 = 600.25$.
- Round **up** (a fractional unit is not enough to meet the bound): $n = 601$.

**Answer.** $n = 601$ subjects suffice for a 95% Wald CI with half-width $\\le 0.04$ regardless of the true $p$. If a pilot study suggested e.g. $\\hat p \\approx 0.2$, the requirement would drop to $n \\ge (49)^{2}\\cdot 0.16 = 384.16 \\Rightarrow 385$ — a $\\sim 36\\%$ saving — but absent any prior info the safe choice is the worst-case $p=0.5$.

```r
# Worst-case (no prior info on p): p(1-p) = 0.25
n_wc <- (qnorm(0.975) * 0.5 / 0.04)^2
n_wc                              # ~ 600.25
ceiling(n_wc)                     # 601

# If a pilot study suggested p ~ 0.2:
n_pilot <- (qnorm(0.975) / 0.04)^2 * 0.2 * 0.8
ceiling(n_pilot)                  # 385

# Sanity check: ME at n = 601, p = 0.5
qnorm(0.975) * sqrt(0.25/601)     # ~ 0.03997  (< 0.04 OK)
```
""", "images": []}

ex6["6_6e"] = {"title": "Ex 6.6e — 99% CI for difference of two proportions",
"content": """**Question.**

Build a 99% CI for $\\hat p_A - \\hat p_B$ with $\\hat p_A=0.4, \\hat p_B=0.36, n_A=100, n_B=120$.

---

**AI walkthrough.** Two independent samples $\\Rightarrow$ the Wald CI for the difference of proportions is
$$(\\hat p_A - \\hat p_B) \\;\\pm\\; z_{1-\\alpha/2}\\,\\sqrt{\\tfrac{\\hat p_A(1-\\hat p_A)}{n_A} + \\tfrac{\\hat p_B(1-\\hat p_B)}{n_B}}.$$
For a CI we **never pool** the variances (pooling is only for the $H_0:p_A=p_B$ test). With $\\alpha=0.01$, $z_{0.005} = 2.576$.

Step-by-step:
- Point estimate: $\\hat p_A - \\hat p_B = 0.40 - 0.36 = 0.04$.
- Unpooled SE: $\\sqrt{\\tfrac{0.4\\cdot 0.6}{100} + \\tfrac{0.36\\cdot 0.64}{120}} = \\sqrt{0.0024 + 0.00192} = \\sqrt{0.00432} \\approx 0.0657$.
- Margin: $2.576 \\times 0.0657 \\approx 0.1693$.
- CI: $0.04 \\pm 0.1693 = [-0.129,\\,0.209]$.

The interval **contains 0**, so at the 1% level there is no evidence that $p_A$ and $p_B$ differ — the 4-pp gap is well within sampling noise for these sample sizes.

```r
p_A <- 0.4; p_B <- 0.36
p_A - p_B                                # point estimate = 0.04
se.diff <- sqrt(p_A*(1-p_A)/100 + p_B*(1-p_B)/120)
se.diff                                  # ~ 0.0657
ME <- qnorm(0.995) * se.diff             # ~ 0.1693
c(p_A - p_B - ME, p_A - p_B + ME)        # ~ [-0.129, 0.209]
```

""", "images": [
    "statistics/images/ex6/questions/ex6_6e_question.png",
    "statistics/images/ex6/answers/ex6_6e_answer.png",
]}

ex6["6_7a"] = {"title": "Ex 6.7a — CI for proportion of DS platform games (vgsales)",
"content": """**Question.** Using the `vgsales` dataset, build a $90\\%$ confidence interval for the proportion $p_{DS}$ of video games released on the `DS` platform. Then find the **largest** confidence level $c$ at which the lower bound of the (two-sided) CI still stays above $16\\%$.

---

**Setup.** Let $\\hat p_n$ be the sample share of DS games. With $n$ large the CLT gives the **Wald** CI
$$\\hat p_n \\;\\pm\\; z_{1-\\alpha/2}\\,SE(\\hat p_n),\\qquad SE(\\hat p_n)=\\sqrt{\\tfrac{\\hat p_n(1-\\hat p_n)}{n}}.$$
The lower bound is $\\hat p_n - z_{1-\\alpha/2}\\,SE$. As $c=1-\\alpha$ **increases**, the multiplier $z_{1-\\alpha/2}$ grows and the lower bound **drops**, so we look for the *largest* $c$ that keeps it above $0.16$.

---

**Answer — part 1 ($90\\%$ CI).**
```r
CI.prop(Platform, success="DS", conf.level=0.9, digits=4, data=vgsales)
## p_hat = 0.1628,   90% CI = [0.1462, 0.1794]
phat <- 0.1628
SE   <- (0.1794 - 0.1462)/2 / qnorm(0.95)   # back out SE from printed CI
SE                                          # ~ 0.01009
qnorm(0.95) * SE                            # ME ~ 0.0166
c(phat - qnorm(0.95)*SE, phat + qnorm(0.95)*SE)
## [0.1462, 0.1794]
```

The point estimate is $\\hat p \\approx 0.1628$ ($\\approx 16.3\\%$); the $90\\%$ CI is $[0.146,\\,0.179]$, i.e. ME $\\approx 0.0166$ and $SE \\approx 0.0101$.

---

**Answer — part 2 (largest $c$ with lower bound $> 0.16$).** Set the lower bound equal to $0.16$ and solve for the critical $z^{*}$:
$$\\hat p - z^{*}\\,SE \\;=\\; 0.16 \\;\\Longrightarrow\\; z^{*} = \\frac{\\hat p - 0.16}{SE} = \\frac{0.1628 - 0.16}{0.01009} \\approx 0.278.$$
Then $1-\\alpha/2 = \\Phi(0.278) \\approx 0.6094$, so $\\alpha/2 \\approx 0.3906$ and
$$c \\;=\\; 1 - \\alpha \\;=\\; 2\\Phi(z^{*}) - 1 \\;\\approx\\; 0.219 \\;\\;(\\approx 22\\%).$$

```r
z.star <- (phat - 0.16)/SE             # ~ 0.278
c.max  <- 2*pnorm(z.star) - 1          # ~ 0.219 — largest two-sided level
c.max
# Sanity check: lower bound at c = c.max should equal 0.16
phat - qnorm(1 - (1 - c.max)/2) * SE   # ~ 0.16
```

**Interpretation.** Even at $90\\%$ the lower bound $0.146$ already sits *below* $0.16$, so the binding level is much smaller than $90\\%$. The lower bound just touches $0.16$ at $c \\approx 22\\%$: any higher confidence widens the interval and pushes the lower bound under $16\\%$.
""", "images": []}

ex6["6_8a1"] = {"title": "Ex 6.8 a1 — 99% CI for mean Skills.Idx (Developers_ITA)",
"content": """**Question.** Using the `Developers_ITA` dataset, build a 99% confidence interval for the mean of `Skills.Idx` and for the mean of `NrSkills`. Comment on the assumptions needed.

---

**Setup.** Population variance $\\sigma^2$ is unknown $\\Rightarrow$ standardise with the sample SD $S_n$:
$$T_n \\;=\\; \\tfrac{\\bar X_n - \\mu}{S_n/\\sqrt n} \\;\\sim\\; t_{n-1}.$$
The two-sided $(1-\\alpha)$ CI is
$$\\bar X_n \\;\\pm\\; t_{1-\\alpha/2,\\,n-1}\\,\\tfrac{S_n}{\\sqrt n} \\;\\approx\\; \\bar X_n \\;\\pm\\; z_{1-\\alpha/2}\\,\\tfrac{S_n}{\\sqrt n}\\quad (n \\text{ large}).$$
With $n \\approx 820$ the percentiles $t_{0.995,\\,819} = 2.5818$ and $z_{0.995} = 2.5758$ agree to two decimals; CLT removes any need for normality of the population.

**AI walkthrough — Skills.Idx.** Given $\\bar x = 78.21$, $s = 14.16$, $n = 820$:
1. **SE** $= s/\\sqrt n = 14.16/\\sqrt{820} \\approx 0.494$.
2. **ME** $= t_{0.995,\\,819}\\cdot \\text{SE} \\approx 2.5818 \\cdot 0.494 \\approx 1.275$.
3. **CI** $= 78.21 \\pm 1.275 = [76.94,\\,79.49]$.

**AI walkthrough — NrSkills.** Given $\\bar x = 19.22$, $s = 11.33$, $n = 820$:
1. **SE** $= 11.33/\\sqrt{820} \\approx 0.395$.
2. **ME** $\\approx 2.5818 \\cdot 0.395 \\approx 1.021$.
3. **CI** $= 19.22 \\pm 1.021 = [18.20,\\,20.24]$.

**Answer.** With 99% confidence the average `Skills.Idx` of Italian developers lies in $[76.94,\\,79.49]$ and the average `NrSkills` in $[18.20,\\,20.24]$. **Assumptions:** $(i)$ random sample from the target population; $(ii)$ $n$ large enough for the CLT — no normality required. The 99% refers to the *procedure*: 99% of intervals built this way contain $\\mu$ in repeated sampling.

```r
CI.mean(Skills.Idx, conf.level=0.99, data=Developers_ITA)
# CI = [76.94, 79.49]
CI.mean(NrSkills,   conf.level=0.99, data=Developers_ITA)
# CI = [18.20, 20.24]

# Manual check (Skills.Idx)
xbar <- 78.21; s <- 14.16; n <- 820
se   <- s/sqrt(n)                    # ~ 0.494
ME   <- qt(0.995, df=n-1) * se       # ~ 1.275
c(xbar - ME, xbar + ME)              # [76.94, 79.49]

# t vs z critical values almost identical at n=820
qt(0.995, df=819)    # 2.5818
qnorm(0.995)         # 2.5758
```
""", "images": [
    "statistics/images/ex6/questions/ex6_8a1_question.png",
    "statistics/images/ex6/answers/ex6_8a1_answer.png",
]}

ex6["6_8a2"] = {"title": "Ex 6.8 a2 — Pooled-variance CI: NrSkills GER vs ITA",
"content": """**Question.** A previous study on German developers reports $\\bar x_{GER} = 17.6$, $s_{GER} = 10.12$ from a sample of $n_{GER} = 802$ observations. Combine this with the Italian sample ($\\bar x_{ITA} = 19.22$, $s_{ITA} = 11.33$, $n_{ITA} = 820$ from `Developers_ITA`) to build a 99% confidence interval for the mean difference $\\mu_{ITA} - \\mu_{GER}$ of `NrSkills`, **assuming equal population variances**.

---

**Setup.** With **independent** samples and the working assumption $\\sigma_{GER}^2 = \\sigma_{ITA}^2 = \\sigma^2$, the textbook estimator is the **pooled variance**
$$S_p^{2} = \\frac{(n_1-1)s_1^{2} + (n_2-1)s_2^{2}}{n_1 + n_2 - 2}, \\qquad \\text{SE}_{pool} = S_p\\sqrt{\\tfrac{1}{n_1} + \\tfrac{1}{n_2}}.$$
The Wald CI is
$$(\\bar X_{ITA} - \\bar X_{GER}) \\;\\pm\\; z_{1-\\alpha/2}\\,\\text{SE}_{pool}.$$
Both samples are large ($n_1+n_2 = 1622 \\gg 30$), so we use $z$ instead of $t_{1620}$ (they agree to four decimals). With $\\alpha = 0.01$, $z_{0.995} = 2.576$.

**AI walkthrough.** Step-by-step numerical breakdown:
- **Point estimate.** $\\bar X_{ITA} - \\bar X_{GER} = 19.22 - 17.60 = 1.62$.
- **Weighted SS.** $(n_{GER}-1)s_{GER}^{2} = 801 \\cdot 10.12^{2} \\approx 801 \\cdot 102.41 \\approx 82{,}033$ and $(n_{ITA}-1)s_{ITA}^{2} = 819 \\cdot 11.33^{2} \\approx 819 \\cdot 128.37 \\approx 105{,}131$.
- **Pooled variance.** $S_p^{2} = \\dfrac{82{,}033 + 105{,}131}{1620} = \\dfrac{187{,}164}{1620} \\approx 115.53$, so $S_p \\approx 10.75$.
- **Pooled SE.** $\\text{SE}_{pool} = 10.75\\sqrt{\\tfrac{1}{802} + \\tfrac{1}{820}} = 10.75\\sqrt{0.001247 + 0.001220} \\approx 10.75 \\cdot 0.04967 \\approx 0.534$.
- **Margin.** $\\text{ME} = 2.576 \\cdot 0.534 \\approx 1.376$.
- **CI.** $1.62 \\pm 1.376 = [0.244,\\,2.996]$.

The interval **excludes $0$**, so at the 1% level Italian developers know on average **more** programming skills than German developers — between $\\approx 0.25$ and $\\approx 3.00$ more, with 99% confidence.

**Answer.**
```r
distr.summary.x(NrSkills, data=Developers_ITA)
mean.GER <- 17.6;  s.GER <- 10.12; n.GER <- 802
mean.ITA <- 19.22; s.ITA <- 11.33; n.ITA <- 820
diff.bar <- mean.ITA - mean.GER                          # 1.62

# Equal variances: pool the two sample variances (pair (n_i-1) with s_i^2)
s2_pool <- ((n.GER-1)*s.GER^2 + (n.ITA-1)*s.ITA^2) / (n.GER + n.ITA - 2)
s2_pool                                                  # ~ 115.53
sp <- sqrt(s2_pool)                                      # ~ 10.75
se.diff <- sp * sqrt(1/n.GER + 1/n.ITA)                  # ~ 0.534
ME <- qnorm(0.995) * se.diff                             # ~ 1.376
c(diff.bar - ME, diff.bar + ME)                          # ~ [0.244, 2.996]
```

**Remarks.** (i) Pooling is justified here only by the *assumption* of equal variances — exercise 6.8b drops it (Welch) and obtains a virtually identical CI because $n_{GER} \\approx n_{ITA}$. (ii) The same pooled $S_p^{2}$ enters both terms of $\\text{SE}_{pool}$; do **not** mix $s_{GER}^{2}/n_{GER}$ with $s_{ITA}^{2}/n_{ITA}$ — that would be the Welch SE, not the pooled SE.
""", "images": []}

ex6["6_8b"] = {"title": "Ex 6.8b — Welch's SE (unequal variances)",
"content": """**Question.** Repeat the previous comparison of mean `NrSkills` between Italian and German developers **without assuming equal variances**. Does the conclusion change?

---

**Setup.** Welch's standard error keeps the two sample variances **separate** (no pooling):
$$\\text{SE}_W \\;=\\; \\sqrt{\\frac{s_{GER}^{2}}{n_{GER}} + \\frac{s_{ITA}^{2}}{n_{ITA}}}, \\qquad \\text{CI} \\;=\\; (\\bar X_{ITA}-\\bar X_{GER}) \\;\\pm\\; z_{1-\\alpha/2}\\,\\text{SE}_W.$$
For large $n$ Welch's $t$-quantile (Satterthwaite df) is numerically indistinguishable from $z_{1-\\alpha/2}$, so we use $z_{0.995} \\approx 2.576$.

**Why it matters.** When $s_{GER} \\neq s_{ITA}$, pooling injects a *bias* in the SE: it under-states the variance of the group with the larger $s$ and over-states the smaller one. Welch is the safer default; pooling is only justified when the variances are truly equal.

**Answer.**
```r
# Same numbers as Ex 6.8 a2 — but now keep variances separate (Welch)
mean.GER <- 17.60;  s.GER <- 10.12;  n.GER <- 802
mean.ITA <- 19.22;  s.ITA <- 11.33;  n.ITA <- 820
diff.bar <- mean.ITA - mean.GER                            # 1.62

se.welch <- sqrt(s.GER^2/n.GER + s.ITA^2/n.ITA)            # 0.5326
ME       <- qnorm(0.995) * se.welch                        # 1.372
c(diff.bar - ME, diff.bar + ME)                            # [0.248, 2.992]

# Sanity check vs pooled SE from Ex 6.8 a2
s2_pool  <- ((n.GER-1)*s.GER^2 + (n.ITA-1)*s.ITA^2) / (n.GER + n.ITA - 2)
se.pool  <- sqrt(s2_pool/n.GER + s2_pool/n.ITA)            # 0.5331
se.welch / se.pool                                         # 0.9991
```

**Numerical comparison.**
- Welch SE $\\approx 0.5326$ vs pooled SE $\\approx 0.5331$ — relative gap $\\approx 0.1\\%$.
- 99% CI (Welch): $[0.25,\\,2.99]$; 99% CI (pooled): $[0.25,\\,2.99]$ — identical to two decimals.

**Conclusion.** The interval is essentially the **same** as the pooled-variance version because (i) the sample sizes are nearly balanced ($n_{GER} \\approx n_{ITA}$), which makes the pooled and unpooled SEs algebraically very close, and (ii) the variance ratio $s_{ITA}^{2}/s_{GER}^{2} \\approx 1.25$ is mild. Both intervals lie entirely **above $0$**, so the qualitative conclusion (Italian developers have on average *more* skills than German developers, by between $0.25$ and $2.99$ skills with $99\\%$ confidence) is **robust** to the equal-variance assumption.
""", "images": []}

ex6["6_8c1"] = {"title": "Ex 6.8 c1 — Paired CI for FinSkills vs Skills",
"content": """**Question.**

![Ex 6.8c1 question](statistics/images/ex6/questions/ex6_8c1_question.png)

In `Developers_ITA` each developer reports both a financial-skills index (`FinSkills.Idx`) and a general skills index (`Skills.Idx`). Build a **90% confidence interval** for the mean within-developer difference $\\mu_D = \\mu_{Fin} - \\mu_{Skills}$. Are the two indices on average different?

---

**Setup.** Because the two measurements are taken on the *same* unit (the same developer), the two samples are **paired**. Define $D_i = \\text{FinSkills}_i - \\text{Skills}_i$ and apply a one-sample $t$ CI to $D$:
$$\\bar D \\;\\pm\\; t_{1-\\alpha/2,\\,n-1}\\,\\tfrac{S_D}{\\sqrt n}.$$
Pairing typically shrinks the SE because individual-level skill differences cancel out within each pair.

**Answer.**
```r
CI.diffmean(x=FinSkills.Idx, y=Skills.Idx, type="paired",
            conf.level=0.9, data=Developers_ITA)
# Mean diff ~ -1.66, 90% CI ~ [-2.20, -1.12]
```

---

**AI walkthrough.** With $n = 820$ Italian developers, the sample mean difference is $\\bar D = \\bar X_F - \\bar X_S \\approx -1.66$ and the sample SD of the differences is $S_D \\approx 9.41$. The standard error of $\\bar D$ is
$$\\text{SE}(\\bar D) = \\tfrac{S_D}{\\sqrt n} = \\tfrac{9.41}{\\sqrt{820}} \\approx 0.3287.$$
At 90% confidence the critical value is $t_{0.95,\\,819} \\approx 1.6464$ (essentially $z_{0.95}=1.645$ because $n$ is large), giving margin of error
$$\\text{ME} = 1.6464 \\times 0.3287 \\approx 0.541.$$
Therefore the 90% CI is $-1.66 \\pm 0.541 \\approx [-2.20,\\,-1.12]$. Because the interval lies **entirely below 0**, financial skills are on average lower than general skills, by between $1.12$ and $2.20$ points (with 90% confidence). The **independent-samples** version (which ignores the pairing) would inflate the SE — pairing matters here because a developer with high general skills also tends to have higher financial skills, so the within-pair correlation reduces the variance of $D$.

```r
# Manual paired CI
D       <- Developers_ITA$FinSkills.Idx - Developers_ITA$Skills.Idx
n       <- length(D)                     # 820
Dbar    <- mean(D)                       # ~ -1.66
S_D     <- sd(D)                         # ~ 9.41
se      <- S_D / sqrt(n)                 # ~ 0.3287
tstar   <- qt(0.95, df = n-1)            # ~ 1.6464
ME      <- tstar * se                    # ~ 0.541
c(Dbar - ME, Dbar + ME)                  # ~ [-2.20, -1.12]

# Same via CI.diffmean (paired)
CI.diffmean(x=FinSkills.Idx, y=Skills.Idx, type="paired",
            conf.level=0.9, data=Developers_ITA)
```

---

**Reference answer.**

![Ex 6.8c1 answer](statistics/images/ex6/answers/ex6_8c1_answer.png)
""", "images": [
    "statistics/images/ex6/questions/ex6_8c1_question.png",
    "statistics/images/ex6/answers/ex6_8c1_answer.png",
]}

ex6["6_8d"] = {"title": "Ex 6.8d — CI for Skills mean: Full-time vs Freelance",
"content": """**Question.** Within `Developers_ITA`, compare the mean number of computer skills (`NrSkills`) between **full-time employees** (`Employment == "Employed, full-time"`) and **freelancers** (`Employment == "Contractor/Freelance"`). Build the default CI and comment on whether the two groups differ on average.

---

**Setup.** Two independent sub-samples from the same survey, *not* paired (a freelancer is a different person from a full-timer). Use the *independent-samples* CI for $\\mu_{Full}-\\mu_{Free}$ with **Welch's SE** (default in `CI.diffmean`, since the two variances and sample sizes need not match):
$$(\\bar X_{Full} - \\bar X_{Free}) \\;\\pm\\; t^{*}_{\\nu}\\,\\sqrt{\\tfrac{s_{Full}^{2}}{n_{Full}} + \\tfrac{s_{Free}^{2}}{n_{Free}}}, \\qquad \\nu = \\text{Welch d.f.}$$
The default confidence level is $95\\%$; with $n_{Full}, n_{Free}$ both in the hundreds, $t^{*}_{\\nu} \\approx 1.96$.

**Answer.**
```r
Skills.Full      <- Developers_ITA$NrSkills[
                       Developers_ITA$Employment == "Employed, full-time"]
Skills.Freelance <- Developers_ITA$NrSkills[
                       Developers_ITA$Employment == "Contractor/Freelance"]
CI.diffmean(x=Skills.Full, y=Skills.Freelance)   # default conf.level = 0.95
## diff.mean ~  0.42
## 95% CI    ~  [0.05, 0.79]
```

The 95% CI lies **entirely above 0** (but barely), so at the 5% level full-timers report on average *slightly more* computer skills than freelancers — about $0.4$ extra skills, with a margin of error between roughly $0.05$ and $0.79$. The effect is statistically detectable but **practically small** relative to the scale of `NrSkills`, so it should not drive any career-level conclusion.
""", "images": []}

ex6["6_9a"] = {"title": "Ex 6.9a — CI for proportion difference (vgsales action genre)",
"content": """**Question.** Two independent samples of *Action*-genre video games from the `vgsales` dataset give $23$ "hits" out of $n_X = 140$ in market $X$ and $37$ "hits" out of $n_Y = 159$ in market $Y$. Build a **99% confidence interval** for the difference $p_Y - p_X$ of population success-proportions and judge whether one market has a higher hit rate.

---

**Setup.** With *independent* Bernoulli samples, the natural estimator is the difference of sample proportions
$$\\widehat{\\Delta} \\;=\\; \\hat p_Y - \\hat p_X, \\qquad \\hat p_X = \\tfrac{x}{n_X},\\;\\; \\hat p_Y = \\tfrac{y}{n_Y}.$$
Independence makes the variance additive:
$$\\text{Var}(\\widehat{\\Delta}) = \\tfrac{p_X(1-p_X)}{n_X} + \\tfrac{p_Y(1-p_Y)}{n_Y}, \\qquad \\text{SE}(\\widehat{\\Delta}) = \\sqrt{\\tfrac{\\hat p_X(1-\\hat p_X)}{n_X} + \\tfrac{\\hat p_Y(1-\\hat p_Y)}{n_Y}}.$$
Both samples are large with $n\\hat p$ and $n(1-\\hat p)$ comfortably $\\geq 5$, so the CLT-based **Wald** CI is valid:
$$\\widehat{\\Delta} \\;\\pm\\; z_{1-\\alpha/2}\\,\\text{SE}(\\widehat{\\Delta}).$$
For $c = 0.99$ the reliability factor is $z_{0.995} \\approx 2.5758$.

---

**Worked example (numbers).** Plug in:

| quantity | formula | value |
|----------|---------|-------|
| $\\hat p_X$ | $23/140$ | $0.1643$ |
| $\\hat p_Y$ | $37/159$ | $0.2327$ |
| $\\widehat\\Delta$ | $\\hat p_Y - \\hat p_X$ | $0.0684$ |
| $s^2_X/n_X$ | $\\hat p_X(1-\\hat p_X)/140$ | $0.000981$ |
| $s^2_Y/n_Y$ | $\\hat p_Y(1-\\hat p_Y)/159$ | $0.001124$ |
| $\\text{SE}(\\widehat\\Delta)$ | $\\sqrt{0.000981 + 0.001124}$ | $0.04501$ |
| $z_{0.995}$ | `qnorm(.995)` | $2.5758$ |
| ME | $z \\cdot \\text{SE}$ | $0.1159$ |
| 99% CI | $[0.0684 - 0.1159,\\; 0.0684 + 0.1159]$ | $[-0.0476,\\; 0.1843]$ |

**Answer.**
```r
x <- 23;  n_X <- 140                 # Market X: hits / sample size
y <- 37;  n_Y <- 159                 # Market Y
p_X_hat <- x/n_X                     # 0.1643
p_Y_hat <- y/n_Y                     # 0.2327
Delta_hat <- p_Y_hat - p_X_hat       # 0.0684
SE <- sqrt(p_X_hat*(1-p_X_hat)/n_X + p_Y_hat*(1-p_Y_hat)/n_Y)   # 0.04501
z <- qnorm(.995)                     # 2.5758
ME <- z * SE                         # 0.1159
c(Delta_hat - ME, Delta_hat + ME)    # [-0.0476, 0.1843]
# Sanity check via prop.test (large-sample CI)
prop.test(c(y, x), c(n_Y, n_X), conf.level = 0.99, correct = FALSE)$conf.int
```

![Ex 6.9a — two 99% one-sample CIs and the sampling distribution of $\\widehat\\Delta$](statistics/images/ex6/ex6_9a_ai.png)

**Interpretation.** The 99% CI for $p_Y - p_X$ is $[-0.048,\\; 0.184]$ and **contains 0**. At the $\\alpha = 1\\%$ level we cannot conclude that one market has a higher Action-game hit rate than the other: an observed gap of $\\approx 6.8$ percentage points is well within sampling noise given $n_X = 140$, $n_Y = 159$. The left panel of the figure shows why — the two one-sample 99% CIs $[0.084,\\,0.245]$ and $[0.146,\\,0.319]$ overlap substantially. To detect a gap of this size at 99% confidence, samples roughly $(2.5758/1.96)^2 \\approx 1.73\\times$ larger would be needed to push the same ME below $|\\widehat\\Delta|$.
""", "images": [
    "statistics/images/ex6/ex6_9a_ai.png",
]}

ex6["6_10a"] = {"title": "Ex 6.10a — CI for diff in JP sales: Strategy vs Role-Playing",
"content": """**Question.**

![Ex 6.10a question](statistics/images/ex6/questions/ex6_10a_question.png)

Referring to sales in the Japanese market (`JP_Sales`), the interest lies in comparing video games of the genres **Strategy** and **Role-Playing**. Assess the average difference in the number of copies sold in the Japanese market between *Strategy* and *Role-Playing* videogames using a **90% confidence interval**. Would it be sensible for a company producing Strategy and Role-Playing games to reposition itself in the market by increasing investment in the second genre of games?

---

**Setup — independent or paired?** A video game belongs to *one and only one* genre, so the two sub-samples (`Genre == "Strategy"` and `Genre == "Role-Playing"`) share **no observations**: they are **independent**. The parameter of interest is $\\mu_{JP,S} - \\mu_{JP,R}$ and both sub-samples are large enough to invoke the CLT, so we use the independent two-sample interval
$$(\\bar X_S - \\bar X_R) \\;\\pm\\; t_{1-\\alpha/2,\\,\\nu}\\,\\sqrt{\\frac{s_S^{\\,2}}{n_S} + \\frac{s_R^{\\,2}}{n_R}},$$
with **unequal variances** (Welch). For a 90% CI, $\\alpha = 0.10$ and the reliability factor is $t_{0.95,\\nu}$ (the textbook output uses $z_{0.95}\\approx 1.645$ because $\\nu$ is large).

**AI walkthrough.**
1. **Slice the data**: `vgsales$JP_Sales[vgsales$Genre=="Strategy"]` and `...=="Role-Playing"`.
2. **Independence**: a game has a single genre $\\Rightarrow$ the samples are disjoint $\\Rightarrow$ `type="independent"` (not paired).
3. **Point estimate**: $\\bar X_S - \\bar X_R \\approx -0.1642$ (Strategy sells, on average, $\\sim 0.16$M fewer copies in Japan than Role-Playing).
4. **RStudio output**: $ci_{0.90}(\\mu_{JP,S} - \\mu_{JP,R}) = (-0.1938,\\,-0.1345)$.
5. **Inference**: the interval lies **entirely below 0**, so at the 10% significance level Strategy sells *significantly less* than Role-Playing in the JP market.
6. **Business reading**: repositioning toward Role-Playing is supported — with 90% confidence, every Role-Playing title brings on average between **134\\,500 and 193\\,800 extra copies** in Japan vs. a Strategy title (`JP_Sales` is in millions).

---

**Answer.** Point estimate $\\bar X_S - \\bar X_R \\approx -0.1642$. The 90% independent-samples CI is
$$ci_{0.90}(\\mu_{JP,S} - \\mu_{JP,R}) = \\bigl[-0.1938,\\,-0.1345\\bigr].$$
Both endpoints are negative ⇒ Strategy games sell, on average, fewer copies in Japan than Role-Playing games. **Yes**, repositioning toward Role-Playing is sensible from a sales-volume perspective — though the conclusion is restricted to the JP market and to expected sales, so before reallocating budget the firm should also weigh production costs, competition density in the Role-Playing segment, and strategic fit (see 6.10b).

```r
# Independent two-sample CI for the mean difference
CI.diffmean(vgsales$JP_Sales[vgsales$Genre=="Strategy"],
            vgsales$JP_Sales[vgsales$Genre=="Role-Playing"],
            type="independent", conf.level=0.90, digits=4)
## ci(mu_S - mu_R) = (-0.1938, -0.1345)

# Manual check of the point estimate
mean(vgsales$JP_Sales[vgsales$Genre=="Strategy"])      # ~ 0.078
mean(vgsales$JP_Sales[vgsales$Genre=="Role-Playing"])  # ~ 0.242
diff <- mean(vgsales$JP_Sales[vgsales$Genre=="Strategy"]) -
        mean(vgsales$JP_Sales[vgsales$Genre=="Role-Playing"])
diff                                                   # ~ -0.1642
```

![Ex 6.10a answer — interval (start)](statistics/images/ex6/answers/ex6_10a_answer1.png)

![Ex 6.10a answer — interval (cont.)](statistics/images/ex6/answers/ex6_10a_answer2.png)
""", "images": [
    "statistics/images/ex6/questions/ex6_10a_question.png",
    "statistics/images/ex6/answers/ex6_10a_answer1.png",
    "statistics/images/ex6/answers/ex6_10a_answer2.png",
]}

ex6["6_11a"] = {"title": "Ex 6.11a — Paired 90% CI: blood indicator before vs after test",
"content": """**Question.**

![Ex 6.11a question](statistics/images/ex6/questions/ex6_11a_question.png)

A researcher believes that a physical test alters the values of a certain blood indicator. To evaluate the average level of the indicator of interest, 25 patients are randomly selected to undergo the physical test, and the level of the blood indicator is measured **before** and **after** the test. The results are:

- **Before:** $\\bar x_B = 85.0$, $s_B = 4.8$
- **After:** $\\bar x_A = 81.4$, $s_A = 5.9$
- Sample correlation $r_{BA} = 0.6$

Discussing any required assumptions, build a **90% confidence interval** for the mean difference Before − After and state your conclusion.

---

**Setup — why paired, not independent.** The same 25 individuals are measured twice (Before/After), so the two samples are **dependent**: treating them as independent would inflate the standard error by ignoring the within-subject correlation $r_{BA}=0.6>0$. Work with the within-patient difference $D_i = B_i - A_i$, whose population mean is $\\mu_D = \\mu_B - \\mu_A$.

**Variance of $D$.** From the variance of a linear combination,
$$\\sigma_D^2 = \\sigma_B^2 + \\sigma_A^2 - 2\\,\\rho_{BA}\\,\\sigma_B\\sigma_A,$$
estimated by
$$s_D^2 = s_B^2 + s_A^2 - 2\\,r_{BA}\\,s_B s_A = 4.8^2 + 5.9^2 - 2(0.6)(4.8)(5.9) = 23.04 + 34.81 - 33.984 = 23.866,$$
so $s_D \\approx 4.885$.

**Assumption.** With $n=25$ small, the CLT does **not** apply: we must assume the differences $D_i$ are approximately **normal**. The pivot $(\\bar D - \\mu_D)/(s_D/\\sqrt n)$ then follows a **Student's $t_{24}$**. The two-sided 90% reliability factor is $t_{0.95,24} \\approx 1.7109$ (i.e. `qt(0.95, 24)`).

---

**Answer.** Point estimate $\\bar d = 85.0 - 81.4 = 3.6$. Standard error of the mean difference
$$\\text{SE}(\\bar D) = s_D/\\sqrt{25} = 4.885/5 \\approx 0.977,$$
and margin of error
$$\\text{ME} = t_{0.95,24}\\cdot \\text{SE}(\\bar D) \\approx 1.7109 \\times 0.977 \\approx 1.672.$$

The 90% CI for $\\mu_B - \\mu_A$ is
$$\\bar d \\pm \\text{ME} = 3.6 \\pm 1.672 = (1.928,\\; 5.272).$$

**Conclusion.** The interval lies **entirely above 0**, so at the 90% confidence level the data support the researcher's claim: the physical test **lowers** the blood indicator on average by between roughly **1.9 and 5.3 units**. The within-subject correlation matters: had we wrongly assumed independence, $s_D^2$ would have been $4.8^2+5.9^2 = 57.85$ instead of $23.87$ — an SE roughly $\\sqrt{57.85/23.87}\\approx 1.56\\times$ larger and a noticeably wider, less informative CI.

```r
# Summary statistics
xbar_B <- 85;     xbar_A <- 81.4
s_B    <- 4.8;    s_A    <- 5.9
r_BA   <- 0.6
n      <- 25

# Paired mean difference and its standard error
diff.BA <- xbar_B - xbar_A                    # 3.6
s2_D    <- s_B^2 + s_A^2 - 2*r_BA*s_B*s_A     # 23.866
se_D    <- sqrt(s2_D/n)                       # 0.9772
qt(0.95, df=n-1)                              # 1.7109
ME      <- qt(0.95, df=n-1)*se_D              # 1.6722
c(diff.BA - ME, diff.BA + ME)                 # 1.928, 5.272
```

![Ex 6.11a answer](statistics/images/ex6/answers/ex6_11a_answer.png)
""", "images": [
    "statistics/images/ex6/questions/ex6_11a_question.png",
    "statistics/images/ex6/answers/ex6_11a_answer.png",
]}

ex6["6_12a"] = {"title": "Ex 6.12a — 99% CI for Adventure high-sales proportion",
"content": """**Question.** We are interested in evaluating which are the most popular video games, those that have sold more than 1 million copies globally (`Global_Sales`). Estimate the proportion of videogames of genre **Adventure** that exceed one million copies sold globally (`Global_Sales`). Assess if it is possible to obtain a 99% confidence interval (using 4 decimal places) for this proportion and, if so, determine and interpret the obtained result.

---

**Answer.** Define the Bernoulli indicator $X_i = \\mathbb{1}\\{\\text{Global\\_Sales}_i > 1\\}$ on the **Adventure** sub-sample. The point estimate is

$$\\hat p = \\frac{1}{n}\\sum_{i=1}^{n} X_i = \\frac{\\#\\{\\text{Adventure games with } > 1\\text{M copies}\\}}{n_{\\text{Adv}}}.$$

**Feasibility check (CLT condition).** The normal approximation $\\hat p \\sim \\mathcal{N}\\!\\left(p,\\ \\tfrac{p(1-p)}{n}\\right)$ holds provided $n\\hat p \\geq 5$ **and** $n(1-\\hat p) \\geq 5$. In the vgsales data the Adventure genre contains roughly $n_{\\text{Adv}} \\approx 1286$ titles, of which $\\approx 40$ exceed 1 M copies, so $\\hat p \\approx 0.0311$, $n\\hat p \\approx 40 \\geq 5$ and $n(1-\\hat p) \\approx 1246 \\geq 5$ — **both conditions are met**, the CI is feasible.

**Interval formula.** With $z_{1-\\alpha/2} = z_{0.995} = $ `qnorm(0.995)` $\\approx 2.5758$,

$$\\hat p \\pm z_{0.995}\\sqrt{\\frac{\\hat p(1-\\hat p)}{n}} \\;\\approx\\; 0.0311 \\pm 2.5758 \\cdot \\sqrt{\\frac{0.0311 \\cdot 0.9689}{1286}} \\;\\approx\\; (0.0186,\\ 0.0436).$$

**Interpretation.** With 99% confidence the *true* proportion of Adventure videogames that have sold more than 1 million copies globally lies between **~1.9 % and ~4.4 %** — a small share, consistent with Adventure being a niche genre.

```r
# Logical vector: TRUE if an Adventure game sold > 1M copies globally
highsales.adv <- vgsales$Global_Sales[vgsales$Genre=="Adventure"] > 1

# Sanity checks
length(highsales.adv)         # n   (Adventure sub-sample size)
sum(highsales.adv)            # number of successes (TRUEs)
mean(highsales.adv)           # p_hat (point estimate)

# 99% CI for the proportion (4 decimal places)
CI.prop(highsales.adv, conf.level=0.99, digits=4)

# CLT condition check
n     <- length(highsales.adv); p_hat <- mean(highsales.adv)
n*p_hat;  n*(1-p_hat)         # both must be >= 5

# Manual verification of the interval
ME <- qnorm(0.995)*sqrt(p_hat*(1-p_hat)/n)
c(p_hat - ME, p_hat + ME)
```
""", "images": []}

ex6["6_12b"] = {"title": "Ex 6.12b — Expression of the CI and margin of error",
"content": """**Question.** Provide the expression of the confidence interval determined in the previous point, specifying the quantities on which it depends and their numerical values (also referring to the output obtained using RStudio). What is the margin of error?

---

**Setup.** Re-using the Adventure sub-sample of Ex 6.12a: $n_{\\text{Adv}} \\approx 1286$, successes $X = \\#\\{\\text{Global\\_Sales} > 1\\} \\approx 40$, point estimate $\\hat p = X/n \\approx 0.0311$. Confidence level $1-\\alpha = 0.99 \\Rightarrow \\alpha/2 = 0.005$.

**General expression.** Because $\\hat p$ is a sample proportion and the CLT regularity conditions $n\\hat p \\ge 5$, $n(1-\\hat p) \\ge 5$ are satisfied (cf. Ex 6.12a), the **large-sample Wald CI** for $p$ is

$$\\hat p \\;\\pm\\; \\underbrace{z_{1-\\alpha/2}}_{\\text{reliability factor}}\\;\\underbrace{\\sqrt{\\tfrac{\\hat p(1-\\hat p)}{n}}}_{SE(\\hat p)}$$

so the interval depends on exactly **three** quantities: the point estimate $\\hat p$, the sample size $n$, and the chosen confidence level (which fixes $z_{1-\\alpha/2}$).

**Numerical decomposition.**

| Quantity | Symbol | Value | RStudio command |
|---|---|---|---|
| Sample size (Adventure) | $n$ | $1286$ | `length(highsales.adv)` |
| Point estimate | $\\hat p$ | $0.0311$ | `mean(highsales.adv)` |
| Standard error | $SE(\\hat p)=\\sqrt{\\hat p(1-\\hat p)/n}$ | $\\approx 0.00485$ | derived from CI half-width |
| Reliability factor | $z_{0.995}$ | $2.5758$ | `qnorm(0.995)` |
| Margin of error | $ME = z_{0.995}\\cdot SE$ | $\\approx 0.0125$ | half-width of CI |
| 99% CI | $\\hat p \\pm ME$ | $(0.0186,\\,0.0436)$ | `CI.prop(..., conf.level=0.99)` |

**Worked computation.**
1. $SE(\\hat p) = \\sqrt{0.0311 \\cdot 0.9689 / 1286} = \\sqrt{0.0000234} \\approx 0.00485$.
2. $ME = z_{0.995}\\cdot SE = 2.5758 \\cdot 0.00485 \\approx 0.01249 \\approx 0.0125$.
3. **Cross-check from the RStudio output of Ex 6.12a:** the CI half-width is $(0.0436 - 0.0186)/2 = 0.0125$ — matches exactly.

**Answer.** $ME \\approx 0.0125$, i.e. about **1.25 percentage points**. Equivalently, the 99% CI can be written as $\\hat p \\pm ME = 0.0311 \\pm 0.0125$.

**Take-aways.** (i) The Wald CI half-width *is* the margin of error — read the CI off the RStudio output and divide by 2 to recover $ME$ without recomputing $SE$. (ii) Because $SE \\propto 1/\\sqrt{n}$, halving the margin of error at the same confidence requires **quadrupling** $n$. (iii) Tightening confidence from 95% to 99% inflates $ME$ by the factor $z_{0.995}/z_{0.975} = 2.5758/1.96 \\approx 1.31$, i.e. roughly $+31\\%$ wider.

```r
# Re-use objects from Ex 6.12a
n     <- length(highsales.adv)           # 1286
p_hat <- mean(highsales.adv)             # 0.0311
se_p  <- sqrt(p_hat*(1-p_hat)/n)         # ~ 0.00485
z     <- qnorm(0.995)                    # 2.5758
ME    <- z * se_p                        # ~ 0.0125
ME
c(p_hat - ME, p_hat + ME)                # (0.0186, 0.0436) -- matches Ex 6.12a

# Cross-check: read CI off the RStudio output and halve it
(0.0436 - 0.0186)/2                      # 0.0125 -- OK
```
""", "images": []}

ex6["6_13a"] = {"title": "Ex 6.13a — 99% CI for cafeteria-visit proportion (≥1 visit/month)",
"content": """**Question.** A bookstore was recently renovated, and a cafeteria area was added. It is of interest to assess the customers' frequency of visits to the cafeteria. Interviews to 140 randomly chosen customers led to the following counts for the **number of visits in the last month**:

| nr. Visits  | 0  | 1  | 2  | 3  | 4  | 5  | 6 | 7 | 8 |
|---|---|---|---|---|---|---|---|---|---|
| nr. Clients | 32 | 43 | 14 | 10 | 18 | 13 | 5 | 0 | 5 |

Build a **99% confidence interval for the proportion of customers** who will visit the cafeteria at least once a month. Is it necessary to make assumptions to determine the interval?

---

**Setup.** Define the binary variable $Y_i = \\mathbf 1\\{\\text{customer } i \\text{ visited at least once}\\}$, so $Y_i \\sim \\text{Bernoulli}(p)$ with $p$ = unknown population proportion of customers visiting the cafeteria at least once a month. The estimator is the sample proportion $\\hat p = X/n$ with $X = \\sum_i Y_i$. By the CLT, provided $n\\hat p \\ge 5$ and $n(1-\\hat p) \\ge 5$ — the rule-of-thumb for the normal approximation — we have
$$\\hat p \\;\\overset{a}{\\sim}\\; \\mathcal{N}\\!\\left(p,\\;\\tfrac{p(1-p)}{n}\\right) \\;\\;\\Longrightarrow\\;\\; \\hat p \\;\\pm\\; z_{1-\\alpha/2}\\,\\sqrt{\\tfrac{\\hat p(1-\\hat p)}{n}}.$$
No assumption on the population distribution is needed beyond the random-sample requirement — Bernoulli is fully specified by $p$ once the random-sampling assumption holds.

**AI walkthrough.** Step by step, with $n=140$ and $\\alpha = 0.01$:
1. **Recode the frequency table.** "At least one visit" = $\\{nr.Visits \\ge 1\\}$, so the success count is $X = 140 - 32 = 108$ (everyone except the 32 zeros). $\\hat p = 108/140 = 0.77\\overline{142} \\approx 0.7714$.
2. **CLT check.** $n\\hat p = 108 \\ge 5$ and $n(1-\\hat p) = 32 \\ge 5$ — both comfortably above 5, so the normal approximation is reliable. **No parametric assumption** on the population is required; only that the 140 customers form a random sample.
3. **Standard error.** $SE(\\hat p) = \\sqrt{\\hat p(1-\\hat p)/n} = \\sqrt{0.7714 \\cdot 0.2286 / 140} = \\sqrt{0.0012597} \\approx 0.03549$.
4. **Reliability factor.** Confidence $1-\\alpha = 0.99 \\Rightarrow z_{0.995} = $ `qnorm(0.995)` $\\approx 2.5758$ (heavier than the usual 1.96 because we squeeze tail mass from 2.5% to 0.5% per tail).
5. **Margin of error.** $ME = z_{0.995} \\cdot SE \\approx 2.5758 \\cdot 0.03549 \\approx 0.0914$.
6. **Interval.** $\\hat p \\pm ME \\approx 0.7714 \\pm 0.0914 = [0.680,\\; 0.863]$.

**Answer.** Point estimate $\\hat p \\approx 0.7714$; **99% CI for $p$** $\\approx [0.680,\\,0.863]$. No assumption on the distribution of `nr.Visits` is needed — only that the 140 customers are a random sample of the target population — because the CLT regularity conditions ($n\\hat p, n(1-\\hat p) \\ge 5$) are met. With 99% confidence, between 68.0% and 86.3% of the bookstore's customers visit the cafeteria at least once a month; the 99% refers to the *procedure*, not to this specific interval.

**Take-aways.** (i) For "at least one" proportions, recoding count data to a Bernoulli indicator is mechanical: success $=$ total $-$ zeros. (ii) The CLT-based CI for $\\hat p$ needs only the random-sample assumption plus the $5/5$ rule of thumb — no distributional model. (iii) Going from 95% to 99% inflates the multiplier by $\\approx 2.5758/1.96 \\approx 31\\%$, widening the interval roughly by the same factor (here from $\\approx \\pm 0.070$ to $\\pm 0.091$).

```r
n     <- 140
x     <- n - 32                              # 108 customers with >= 1 visit
p_hat <- x/n                                 # 0.7714
# CLT rule-of-thumb check
n*p_hat;        n*(1-p_hat)                  # 108 ; 32 -- both >> 5
se_p  <- sqrt(p_hat*(1-p_hat)/n)             # ~ 0.03549
qnorm(0.995)                                 # 2.5758
ME    <- qnorm(0.995) * se_p                 # ~ 0.0914
c(p_hat - ME, p_hat + ME)                    # ~ [0.680, 0.863]

# One-shot equivalent if the raw 0/1 vector is available:
visits <- rep(0:8, c(32,43,14,10,18,13,5,0,5))
at_least_one <- visits >= 1
CI.prop(at_least_one, success=TRUE, conf.level=0.99, digits=4)
```
""", "images": []}

ex6["6_13d"] = {"title": "Ex 6.13d — 90% CI for average monthly visits (cafeteria)",
"content": """**Question.** Building on the cafeteria data of Ex 6.13 (`nr.Visits` = 0..8 with frequencies 32, 43, 14, 10, 18, 13, 5, 0, 5; $n=140$), determine — if feasible on the available data — a **90% confidence interval for the average monthly number of visits**.

![Question](images/ex6/ex6_13_question.png)

---

**Setup.** Let $X$ = number of monthly cafeteria visits per customer, with unknown population mean $\\mu = E[X]$ and variance $\\sigma^2$. The natural estimator is the sample mean $\\bar X = \\tfrac{1}{n}\\sum_{i=1}^{n} X_i$. The catch is that $X$ is **discrete** (a count, $0,1,\\dots,8$) and clearly **non-normal** — the frequency table is heavy at 0 and 1 with a right tail. However, the *parameter* $\\mu$ is real-valued, and with $n = 140 \\gg 30$ the CLT gives
$$\\bar X \\;\\overset{a}{\\sim}\\; \\mathcal{N}\\!\\left(\\mu,\\;\\tfrac{\\sigma^2}{n}\\right) \\;\\;\\Longrightarrow\\;\\; \\bar x \\;\\pm\\; z_{1-\\alpha/2}\\,\\sqrt{\\tfrac{s^2}{n}},$$
with $\\sigma^2$ replaced by the sample variance $s^2$ (the large $n$ also makes the $z$/$t$ distinction immaterial — $t_{139}$ and $z$ agree to four decimals). So the CI **is** feasible with only the random-sample assumption.

**AI walkthrough.** With $n=140$ and $\\alpha = 0.10$:
1. **Sample mean from the frequency table.** $\\bar x = \\tfrac{1}{140}\\sum_k k\\,f_k = \\tfrac{0\\cdot 32 + 1\\cdot 43 + 2\\cdot 14 + 3\\cdot 10 + 4\\cdot 18 + 5\\cdot 13 + 6\\cdot 5 + 7\\cdot 0 + 8\\cdot 5}{140} = \\tfrac{308}{140} = 2.2$ visits/customer/month.
2. **Second moment.** $\\overline{x^2} = \\tfrac{1}{140}\\sum_k k^2 f_k = \\tfrac{0 + 43 + 56 + 90 + 288 + 325 + 180 + 0 + 320}{140} = \\tfrac{1302}{140} = 9.3$.
3. **Sample variance via the shortcut.** $s^2 = \\tfrac{n}{n-1}\\bigl(\\overline{x^2} - \\bar x^2\\bigr) = \\tfrac{140}{139}\\bigl(9.3 - 4.84\\bigr) = \\tfrac{140}{139}\\cdot 4.46 \\approx 4.4921$. The Bessel factor $n/(n-1) \\approx 1.0072$ barely moves the number.
4. **Standard error.** $SE(\\bar X) = \\sqrt{s^2/n} = \\sqrt{4.4921/140} = \\sqrt{0.03209} \\approx 0.1791$.
5. **Reliability factor.** $1-\\alpha = 0.90 \\Rightarrow z_{0.95} = $ `qnorm(0.95)` $\\approx 1.6449$ (lighter than 1.96 because each tail carries 5% mass).
6. **Margin of error.** $ME = 1.6449 \\cdot 0.1791 \\approx 0.2946$.
7. **Interval.** $\\bar x \\pm ME = 2.2 \\pm 0.2946 = [1.905,\\; 2.495]$.

**Answer.** Point estimate $\\bar x = 2.2$ visits/customer/month; **90% CI for $\\mu$** $\\approx [1.905,\\,2.495]$. The CI is feasible: even though `nr.Visits` is a discrete, clearly non-normal variable, the CLT delivers approximate normality of $\\bar X$ thanks to $n = 140$ being well above the rule-of-thumb of 30, and only the random-sampling assumption is required. With 90% confidence, the average monthly cafeteria visits per customer lies between **1.91 and 2.50**.

**Take-aways.** (i) Discreteness of $X$ is irrelevant for a CI on the *mean* — what matters is the CLT for $\\bar X$, which only needs $n$ large enough (here $140 \\gg 30$). (ii) When you have a frequency table, never expand the vector unnecessarily: $\\bar x = \\sum k f_k / n$ and $\\overline{x^2} = \\sum k^2 f_k / n$ are mechanical. (iii) Going from 99% (panel **a**) to 90% (panel **d**) shrinks the multiplier from 2.576 to 1.645 — a $\\approx 36\\%$ tighter interval per unit SE, which is why panel **d** feels far more informative.

```r
# Build the 0/1/.../8 vector with the right multiplicities
visits  <- rep(0:8, c(32,43,14,10,18,13,5,0,5))
n       <- length(visits)                                    # 140

# Frequency-table shortcuts (matches the by-hand work above)
xbar    <- (43 + 2*14 + 3*10 + 4*18 + 5*13 + 6*5 + 8*5)/n    # 308/140 = 2.2
mean.x2 <- (43 + 4*14 + 9*10 + 16*18 + 25*13 + 36*5 + 64*5)/n  # 1302/140 = 9.3
s2_x    <- (n/(n-1)) * (mean.x2 - xbar^2)                    # ~ 4.4921
se_Xbar <- sqrt(s2_x/n)                                      # ~ 0.1791

qnorm(0.95)                                                  # 1.6449
ME      <- qnorm(0.95) * se_Xbar                             # ~ 0.2946
c(xbar - ME, xbar + ME)                                      # ~ [1.905, 2.495]

# One-shot equivalent on the raw vector:
mean(visits); var(visits)                                    # 2.2 ; 4.4921
CI.mean(visits, conf.level=0.90, digits=4)
```
""", "images": ["images/ex6/ex6_13_question.png", "images/ex6/ex6_13d_answer.png"]}

ex6["6_14a"] = {"title": "Ex 6.14a — 90% CI for diff in proportions: EA vs Activision best-sellers",
"content": """**Question.** Using the `vgsales` dataset, a game is labeled **best-seller** when North-American sales exceed 1 million units (`NA_Sales > 1`). Build a **90% confidence interval for the difference in the population proportions of best-sellers** between **Electronic Arts** (group 1) and **Activision** (group 2).

---

**Setup.** For each publisher define the Bernoulli indicator $Y_{i}^{(j)} = \\mathbf 1\\{NA\\_Sales_i > 1\\}$, so $Y_{i}^{(j)} \\sim \\text{Bernoulli}(p_j)$, $j \\in \\{EA, ACT\\}$. The two samples are **independent** (different publishers), so the sample proportions $\\hat p_j = X_j/n_j$ are independent and, by the CLT (with $n_j \\hat p_j \\ge 5$ and $n_j(1-\\hat p_j)\\ge 5$),
$$\\hat p_1 - \\hat p_2 \\;\\overset{a}{\\sim}\\; \\mathcal{N}\\!\\left(p_1 - p_2,\\;\\tfrac{p_1(1-p_1)}{n_1} + \\tfrac{p_2(1-p_2)}{n_2}\\right).$$
The **two-sample CI** therefore is
$$(\\hat p_1 - \\hat p_2) \\;\\pm\\; z_{1-\\alpha/2}\\,\\sqrt{\\tfrac{\\hat p_1(1-\\hat p_1)}{n_1} + \\tfrac{\\hat p_2(1-\\hat p_2)}{n_2}}.$$
Note: this is the **plain (unpooled)** SE — pooling is reserved for the hypothesis test under $H_0: p_1 = p_2$, not for the CI.

**AI walkthrough.** With $\\alpha = 0.10$:
1. **Recode each publisher to a 0/1 best-seller vector** by subsetting `vgsales$NA_Sales` on `Publisher` and comparing to 1. Independence holds because no game is published by both EA and Activision.
2. **Count successes and sample sizes.** `sum()` over each logical vector gives $X_{EA}, X_{ACT}$; `length()` gives $n_{EA}, n_{ACT}$. Compute $\\hat p_{EA} = X_{EA}/n_{EA}$ and $\\hat p_{ACT} = X_{ACT}/n_{ACT}$.
3. **CLT regularity.** EA and Activision are both very large publishers (hundreds of games each), and best-sellers are common enough that $n_j \\hat p_j$ and $n_j(1-\\hat p_j)$ are well above 5 — normal approximation is reliable.
4. **Standard error (unpooled).** $SE = \\sqrt{\\hat p_{EA}(1-\\hat p_{EA})/n_{EA} + \\hat p_{ACT}(1-\\hat p_{ACT})/n_{ACT}}$.
5. **Reliability factor.** $z_{0.95} = $ `qnorm(0.95)` $\\approx 1.6449$.
6. **Interval.** $(\\hat p_{EA} - \\hat p_{ACT}) \\pm 1.6449 \\cdot SE$. If the CI **contains 0**, the data are compatible with $p_{EA} = p_{ACT}$ at the 10% level; if it lies entirely above (below) 0, EA has the higher (lower) best-seller rate.

**Answer.** Read $\\hat p_{EA}$, $\\hat p_{ACT}$, and the CI directly off the `CI.diffprop()` output. The sign and position of the interval relative to 0 carry the conclusion: an interval entirely on one side of 0 signals a statistically meaningful difference in best-seller rates between the two publishers at the 90% confidence level.

**Take-aways.** (i) For a difference-of-proportions **CI**, always use the **unpooled** SE — pooling under $H_0$ is for the *test*, not for the CI. (ii) Independence of the two samples is what makes the variances add; check that the two groups have no overlap. (iii) `CI.diffprop()` from the course's `tidystats`-style helper packages the recoding/normal-quantile/SE arithmetic into one call — but every step is reproducible by hand from $\\hat p_j$, $n_j$ and `qnorm()`.

```r
# Two independent samples — best-seller indicator per publisher
EA_best_seller  <- vgsales$NA_Sales[vgsales$Publisher=="Electronic Arts"] > 1
ACT_best_seller <- vgsales$NA_Sales[vgsales$Publisher=="Activision"]      > 1

# Successes per group
sum(EA_best_seller);  sum(ACT_best_seller)

# Per-group point estimates and CLT check (mean = p_hat, n shown in output)
distr.summary.x(EA_best_seller,  digits=4)
distr.summary.x(ACT_best_seller, digits=4)

# 90% CI for p_EA - p_ACT (unpooled SE, normal reliability factor)
CI.diffprop(EA_best_seller, ACT_best_seller, conf.level=0.9, digits=4)

# Manual cross-check
p1 <- mean(EA_best_seller);  n1 <- length(EA_best_seller)
p2 <- mean(ACT_best_seller); n2 <- length(ACT_best_seller)
se <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
z  <- qnorm(0.95)
c((p1-p2) - z*se, (p1-p2) + z*se)
```
""", "images": []}

ex6["6_15a"] = {"title": "Ex 6.15a — Mean difference from supplied summary stats",
"content": """**Question.** Two independent samples give $\\bar x = -39.34$ and $\\bar y = -49.71$. Compute the **sample mean difference** $\\bar d = \\bar x - \\bar y$, the natural point estimate of $\\mu_X - \\mu_Y$.

---

**Setup.** For two **independent** samples drawn from populations with means $\\mu_X,\\,\\mu_Y$, the parameter of interest is the **difference of means** $\\mu_X - \\mu_Y$. Its unbiased point estimator is the **difference of sample means**:
$$\\widehat{\\mu_X - \\mu_Y} \\;=\\; \\bar X - \\bar Y \\;\\equiv\\; \\bar d.$$
By linearity of expectation $\\mathbb E[\\bar X - \\bar Y] = \\mu_X - \\mu_Y$ (unbiased), and by independence $\\mathrm{Var}(\\bar X - \\bar Y) = \\sigma_X^2/n_x + \\sigma_Y^2/n_y$ — the SE used in part **b** for the CI. Sign convention: $\\bar d > 0$ means group $X$ scores higher on average than group $Y$; $\\bar d < 0$ the opposite. The sign is **only** about the labelling — swapping $X \\leftrightarrow Y$ flips it.

---

**AI walkthrough.** Plug in the two summary means; mind the **double-minus**:
$$\\bar d \\;=\\; \\bar x - \\bar y \\;=\\; (-39.34) - (-49.71) \\;=\\; -39.34 + 49.71 \\;=\\; 10.37.$$
*Interpretation.* Both group means are negative (so both populations sit below zero on whatever scale this is — log-returns, deviations from a reference, etc.), but $X$ is **less negative** than $Y$: on average $X$ exceeds $Y$ by $10.37$ units. Equivalently, $\\bar y$ is $10.37$ units *below* $\\bar x$.

*Why this matters for parts b–c.* The 95% CI for $\\mu_X - \\mu_Y$ is centred at exactly this $\\bar d$, with half-width $t_{1-\\alpha/2}\\cdot \\text{SE}$ depending on the variance assumption (pooled vs Welch — see 6.15b). If that CI excludes zero, we reject $H_0\\!: \\mu_X = \\mu_Y$ at the same level; if it straddles zero, we cannot.

---

**Answer.** $\\bar d = \\bar x - \\bar y = -39.34 - (-49.71) = \\mathbf{10.37}$.

```r
xbar <- -39.34
ybar <- -49.71
diff.bar <- xbar - ybar
diff.bar                # 10.37
```
""", "images": []}

ex6["6_15b"] = {"title": "Ex 6.15b — Pooled vs separate t-CI",
"content": """**Question.** Build the 95% CI for the mean difference $\\mu_X - \\mu_Y$ using sample variances $s^2_x = 118.93,\\ n_x = 20,\\ s^2_y = 129.55,\\ n_y = 28$ (with $\\bar d = \\bar x - \\bar y = 10.37$ from part a). Compare the **separate-variances (Welch)** and **pooled** standard errors.

---

**Setup.** Two independent samples, unknown variances. Two flavours of the two-sample $t$-CI:
- **Pooled** (assume $\\sigma_X^2 = \\sigma_Y^2$): $s_p^2 = \\dfrac{(n_x-1)s_x^2 + (n_y-1)s_y^2}{n_x+n_y-2}$, $\\text{SE} = s_p\\sqrt{1/n_x + 1/n_y}$, df $= n_x + n_y - 2 = 46$.
- **Welch** (separate variances): $\\text{SE} = \\sqrt{s_x^2/n_x + s_y^2/n_y}$, df via Satterthwaite.

The two SEs are close when $s_x^2 \\approx s_y^2$ — true here (118.93 vs 129.55), so both CIs nearly coincide.

---

**Answer.**
```r
diff.bar <- 10.37
s2_x <- 118.93; n_x <- 20
s2_y <- 129.55; n_y <- 28

# Separate variances (Welch)
se_unequal <- sqrt(s2_x/n_x + s2_y/n_y); se_unequal
df_welch   <- (s2_x/n_x + s2_y/n_y)^2 /
              ((s2_x/n_x)^2/(n_x-1) + (s2_y/n_y)^2/(n_y-1)); df_welch
c(diff.bar - qt(0.975, df_welch)*se_unequal,
  diff.bar + qt(0.975, df_welch)*se_unequal)

# Pooled variance
s2_pool  <- ((n_x-1)*s2_x + (n_y-1)*s2_y) / (n_x + n_y - 2); s2_pool
se_equal <- sqrt(s2_pool*(1/n_x + 1/n_y)); se_equal
qt(0.975, 46)
c(diff.bar - qt(0.975, 46)*se_equal,
  diff.bar + qt(0.975, 46)*se_equal)
```
""", "images": []}

ex6["6_17a"] = {"title": "Ex 6.17a — Paired 99% CI for dwell-time difference (23 stores, two weeks)",
"content": """**Question.**

![Ex 6.17a question](statistics/images/ex6/questions/ex6_17a_question.png)

A sample of $n=23$ stores is randomly drawn from a city. For each store the average dwell time of customers is measured in **two different weeks** (same store, two visits), with summary statistics

- Week 1: $\\bar x = 56.8$, $s_X = 6.1$
- Week 2: $\\bar y = 46.7$, $s_Y = 6.9$
- Sample covariance between the two weekly means: $s_{XY} = 34.6$

Build a **99% confidence interval** for the mean difference $\\mu_X - \\mu_Y$, discussing why the paired scheme must be used.

---

**Setup — paired, not independent.** Each store contributes **two** measurements taken in two different weeks. The two samples are matched by store, hence **dependent**: store-level heterogeneity (location, footfall, opening hours) inflates both weekly means in the same direction, and the positive sample covariance $s_{XY}=34.6>0$ confirms it. Treating the samples as independent would **overstate the standard error** of the difference and produce an unnecessarily wide CI.

Work with the within-store difference $D_i = X_i - Y_i$, whose population mean is $\\mu_D = \\mu_X - \\mu_Y$ and whose sample mean equals
$$\\bar D = \\bar X - \\bar Y = 56.8 - 46.7 = 10.1.$$

**Variance of $\\bar D$.** From $\\Var(X-Y) = \\Var(X) + \\Var(Y) - 2\\Cov(X,Y)$,
$$s_D^2 = s_X^2 + s_Y^2 - 2\\,s_{XY} = 6.1^2 + 6.9^2 - 2(34.6) = 37.21 + 47.61 - 69.2 = 15.62,$$
so the standard error of the paired mean difference is
$$\\widehat{\\mathrm{SE}}(\\bar D) = \\sqrt{\\dfrac{s_D^2}{n}} = \\sqrt{\\dfrac{15.62}{23}} \\approx \\sqrt{0.6791} \\approx 0.824.$$

**Distribution & critical value.** Assuming $D_i$ approximately normal (small $n=23$), $\\bar D$ follows a Student-$t$ with $n-1=22$ degrees of freedom. For a $1-\\alpha=0.99$ CI we need $t_{0.995,\\,22}\\approx 2.819$.

**99% CI.**
$$\\bar D \\pm t_{0.995,22}\\,\\widehat{\\mathrm{SE}}(\\bar D) = 10.1 \\pm 2.819\\times 0.824 \\approx 10.1 \\pm 2.32 = [\\,7.78,\\ 12.42\\,].$$

**Interpretation.** With confidence 99% the average dwell time in Week 1 exceeds that in Week 2 by between roughly **7.8 and 12.4 minutes**. The interval lies entirely **above zero**, so a null difference $\\mu_X=\\mu_Y$ is incompatible with the data at the 1% level. Note that ignoring the positive covariance would have given $\\widehat{\\mathrm{SE}}_\\text{indep} = \\sqrt{(s_X^2+s_Y^2)/n} = \\sqrt{84.82/23}\\approx 1.92$, more than double the paired SE — the matched design buys a much sharper estimate.

![Ex 6.17a — paired vs independent: shrinking the SE by exploiting within-store covariance](statistics/images/ex6/ex6_17a_ai.png)

```r
xbar <- 56.8; ybar <- 46.7
sx <- 6.1;   sy <- 6.9;   sxy <- 34.6;   n <- 23
dbar <- xbar - ybar                                    # 10.1
sD2  <- sx^2 + sy^2 - 2*sxy                            # 15.62
seD  <- sqrt(sD2 / n)                                  # 0.824
t_crit <- qt(0.995, df = n - 1)                        # 2.819
c(dbar - t_crit*seD, dbar + t_crit*seD)                # 99% CI -> ~ [7.78, 12.42]
```

![Ex 6.17a answer](statistics/images/ex6/answers/ex6_17a_answer.png)
""", "images": [
    "statistics/images/ex6/questions/ex6_17a_question.png",
    "statistics/images/ex6/ex6_17a_ai.png",
    "statistics/images/ex6/answers/ex6_17a_answer.png",
]}

ex6["6_18b"] = {"title": "Ex 6.18b — Paired 98% CI: NA vs EU sales (Action)",
"content": """**Question.**

![Ex 6.18b question](statistics/images/ex6/questions/ex6_18b_question.png)

For the `vgsales` dataset restricted to genre **Action**, build a **98% confidence interval** for the mean difference in copies sold in North America vs the European market. Each title contributes a NA_Sales and an EU_Sales value, so the two samples are **paired by game** (same row of the dataset).

---

**Setup — why paired.** The two columns `NA_Sales` and `EU_Sales` are recorded on the **same game**: a popular title boosts both regions together, an obscure one drags both down. The within-game correlation is therefore strongly positive, and treating the samples as independent would dramatically overstate $\\Var(\\bar D)$. With $n$ in the hundreds of Action titles the CLT covers normality of $\\bar D$, so a paired $t$-CI (essentially $z$-CI) on the differences $D_i = \\text{NA}_i - \\text{EU}_i$ is appropriate.

**Pivot and CI.** With $\\bar d$ the sample mean of the differences and $s_D$ their sample SD,
$$\\bar d \\pm t_{0.99,\\,n-1}\\,\\frac{s_D}{\\sqrt n}.$$
The 98% two-sided level gives $\\alpha/2 = 0.01$, so the reliability factor is $t_{0.99,n-1}$ (close to $z_{0.99}\\approx 2.326$ for large $n$).

---

**Answer.** Use `CI.diffmean` with `type="paired"` and `conf.level=0.98`. The point estimate is $\\bar d = \\overline{\\text{NA}} - \\overline{\\text{EU}}$ (positive in this dataset — Action games sell more in NA than in EU on average), and the CI is centred on it. Because NA and EU sales are highly correlated across games (large positive `cor(NA,EU)`), the paired SE is much smaller than the independent-samples SE would have been, yielding a tight interval.

```r
# Paired 98% CI for mean(NA_Sales - EU_Sales) on Action titles
CI.diffmean(x = vgsales$NA_Sales[vgsales$Genre=="Action"],
            y = vgsales$EU_Sales[vgsales$Genre=="Action"],
            type = "paired", conf.level = 0.98, digits = 4)

# Sanity check: within-game correlation (close to 1 = strong pairing)
cor(vgsales$NA_Sales[vgsales$Genre=="Action"],
    vgsales$EU_Sales[vgsales$Genre=="Action"])
```

**Conclusion.** If the resulting CI lies entirely above 0, Action games sell significantly more in NA than in EU on average at the 98% level; if it straddles 0, the paired data do not support a regional difference.

![Ex 6.18b answer](statistics/images/ex6/answers/ex6_18b_answer.png)
""", "images": [
    "statistics/images/ex6/questions/ex6_18b_question.png",
    "statistics/images/ex6/answers/ex6_18b_answer.png",
]}
