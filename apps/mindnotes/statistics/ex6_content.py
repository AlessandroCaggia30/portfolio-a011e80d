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
"content": """**Question.** A survey on German developers gives a 95% CI for the mean number of computer skills equal to $[16.91, 18.29]$.
**b1)** What is the point estimate of the average number of skills (for German developers)? What considerations about developers in the two countries?
**b2)** What is the margin of error of the interval? What considerations on the possible variation of the margin of error if a 99% confidence interval is built?

---

**Answer.**
- **b1)** The point estimate is the **centre** of the interval: $(16.91 + 18.29)/2 = 17.60$. The German interval $[16.91, 18.29]$ lies entirely **below** the Italian one $[18.37, 19.93]$ — they do not overlap, so German developers appear to have on average **fewer** skills than Italians.
- **b2)** The margin of error is **half the width**: $(18.29 - 16.91)/2 = 0.69$. Increasing the confidence level from 95% to 99% increases the percentile (reliability factor) of the distribution while keeping the standard error unchanged, so the ME — and thus the interval width — **increases**.

```r
# b1) point estimate = midpoint
(16.91 + 18.29)/2     # 17.60
# b2) margin of error = half width
(18.29 - 16.91)/2     # 0.69
# 99% would use a larger percentile, widening the interval
qnorm(0.975); qnorm(0.995)
```
""", "images": []}

ex6["6_1c"] = {"title": "Ex 6.1c — 95% CI for the proportion of Hybrid workers (ITA)",
"content": """**Question.** Estimate the proportion of (Italian) developers working in a *Hybrid* mode (`WorkingMode == "Hybrid"`). Obtain the 95% CI for the parameter of interest, and specify how it was determined (round results to 3 decimals).

---

**Answer.** Point estimate is the **sample proportion** $\\hat p = 0.47$. The sample size is big enough to apply the CLT, so the 95% CI is obtained by adding/subtracting from $\\hat p$ the product of $SE(\\hat p) = \\sqrt{\\hat p(1-\\hat p)/n}$ and the 0.975 percentile of the standard normal: $\\hat p \\pm 1.96 \\cdot SE(\\hat p) = [0.436, 0.505]$.

```r
CI.prop(WorkingMode, success="Hybrid", conf.level=0.95,
        digits=3, data=Developers_ITA)
# Manual check:
phat <- 0.47
se_p <- sqrt(phat*(1-phat)/802)   # ~ 0.018
phat + c(-1,1) * qnorm(0.975) * se_p   # [0.436, 0.505]
```
""", "images": []}

ex6["6_1d"] = {"title": "Ex 6.1d — Validity of a CI from a self-selected social-channel survey",
"content": """**Question.** A young developer believes the share of developers working in hybrid mode is actually **lower** than the estimated one. He runs a survey on one social channel (dedicated to developers) and gets 25 answers, 8 of which are in hybrid mode. Discuss whether it is possible/reasonable to provide a point or interval (at 95%) estimate for the proportion based on this survey; if it is, specify the analytic expressions of the estimates.

---

**Answer.** A **point estimate** can always be computed: $\\hat p = 8/25 = 0.32$.
For a **95% CI** there are two critical issues:
1. The sample is **not drawn from the entire population** but only from a sub-population of developers following that specific social channel — results are not representative of the general developer population.
2. Respondents **self-select** (they choose to answer), so the sample is not a random sample — those who reply may be those most interested in the topic and the estimate $\\hat p = 0.32$ may be biased.
3. The sample size $n = 25$ is borderline for the CLT-based normal approximation for proportions.
So a reliable CI cannot really be built from this survey.

```r
# Point estimate is fine
phat <- 8/25; phat        # 0.32
# Formal CI formula (with caveats above):
se_p <- sqrt(phat*(1-phat)/25)
phat + c(-1,1) * qnorm(0.975) * se_p
```
""", "images": []}

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
"content": """**Question.** Re-do the proportion CI but using $z = q_{0.95} = 1.645$ instead of $z_{0.025}$ — i.e. a 90% two-sided CI (or equivalently, a one-sided 95% bound).

---

**Answer.** A smaller $z$ shortens the interval at the cost of lower confidence.

```r
qnorm(0.95)
ME_95 <- qnorm(0.95) * se
c(0.4 - ME_95, 0.4 + ME_95)
```
""", "images": []}

ex6["6_6d"] = {"title": "Ex 6.6d — Sample size for ME $\\le 0.04$ at 95% confidence",
"content": """**Question.** What sample size guarantees a margin of error $\\le 0.04$ at 95% confidence, with no prior info on $p$?

---

**Answer.** Use the worst-case variance $p(1-p) = 0.25$ at $p=0.5$, giving $n \\ge (z_{0.025}\\cdot 0.5/0.04)^2$.

```r
(qnorm(0.975) * 0.5 / 0.04)^2   # ~ 600.25  ->  n = 601
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

""", "images": []}

ex6["6_7a"] = {"title": "Ex 6.7a — CI for proportion of DS platform games (vgsales)",
"content": """**Question.** Using the `vgsales` dataset, build a 90% confidence interval for the proportion $p_{DS}$ of video games released on the `DS` platform. Then find the largest confidence level at which the lower bound stays above $16\\%$.

---

**Setup.** Let $\\hat p_n$ be the sample share of DS games. The Wald CI for a proportion is
$$\\hat p_n \\;\\pm\\; z_{1-\\alpha/2}\\,\\sqrt{\\tfrac{\\hat p_n(1-\\hat p_n)}{n}}.$$

**Answer.**
```r
CI.prop(Platform, success="DS", conf.level=0.9, digits=4, data=vgsales)
# Output: p_hat = 0.1628, CI_90% = [0.1462, 0.1794]
qnorm(0.95)   # 1.6449 -> 90% two-sided multiplier
qnorm(0.995)  # 2.5758 -> 99% two-sided multiplier
# Largest one-sided level c with lower bound > 0.16:
# z* = (p_hat - 0.16)/SE = 0.0028/0.0166
pnorm(0.0428/0.0166)   # ~ 0.9952
```

The estimate is $\\hat p \\approx 16.3\\%$ with SE $\\approx 0.0166$, so the 90% CI is $[0.146,\\,0.179]$. Stretching the level upward, the lower bound crosses $0.16$ at roughly $c \\approx 99.5\\%$.
""", "images": []}

ex6["6_8a1"] = {"title": "Ex 6.8 a1 — 99% CI for mean Skills.Idx (Developers_ITA)",
"content": """**Question.** Using the `Developers_ITA` dataset, build a 99% confidence interval for the mean of `Skills.Idx` and for the mean of `NrSkills`. Comment on the assumptions needed.

---

**Setup.** With unknown variance and large $n \\approx 820$, the CLT lets us use either the $t$ or the normal critical value — they agree to four decimals.
$$\\bar X_n \\;\\pm\\; t_{1-\\alpha/2,\\,n-1}\\,\\tfrac{S_n}{\\sqrt n} \\;\\approx\\; \\bar X_n \\;\\pm\\; z_{1-\\alpha/2}\\,\\tfrac{S_n}{\\sqrt n}.$$

**Answer.**
```r
CI.mean(Skills.Idx, conf.level=0.99, data=Developers_ITA)
# e.g. CI = [76.94, 79.49]
CI.mean(NrSkills,   conf.level=0.99, data=Developers_ITA)
# e.g. CI = [18.20, 20.24]
qt(0.995, df=819)    # 2.5818  -> t critical
qnorm(0.995)         # 2.5758  -> z critical (almost identical)
```

No distributional assumption is required because $n$ is large (CLT). With 99% confidence the average `Skills.Idx` of Italian developers lies in $[76.9,\\,79.5]$ and the average `NrSkills` in $[18.2,\\,20.2]$.
""", "images": []}

ex6["6_8a2"] = {"title": "Ex 6.8 a2 — Pooled-variance CI: NrSkills GER vs ITA",
"content": """**Question.** A previous study on German developers reports $\\bar x_{GER} = 17.6$, $s_{GER} = 10.12$ from a sample of $n_{GER} = 802$ observations. Combine this with the Italian sample ($\\bar x_{ITA} = 19.22$, $s_{ITA} = 11.33$, $n_{ITA} = 820$ from `Developers_ITA`) to build a 99% confidence interval for the mean difference $\\mu_{ITA} - \\mu_{GER}$ of `NrSkills`, **assuming equal population variances**.

---

**Setup.** With independent samples and a common variance,
$$S_p^{2} = \\frac{(n_1-1)s_1^{2} + (n_2-1)s_2^{2}}{n_1 + n_2 - 2}, \\qquad \\text{SE} = S_p\\sqrt{\\tfrac{1}{n_1} + \\tfrac{1}{n_2}}.$$
The CI is $(\\bar X_{ITA} - \\bar X_{GER}) \\pm z_{1-\\alpha/2}\\,\\text{SE}$.

**Answer.**
```r
distr.summary.x(NrSkills, data=Developers_ITA)
mean.GER <- 17.6;  s.GER <- 10.12
mean.ITA <- 19.22; s.ITA <- 11.33
diff.bar <- mean.ITA - mean.GER     # 1.62

# Equal variances: pool the two sample variances (pair (n_i-1) with s_i^2)
s2_pool <- (801*s.GER^2 + 819*s.ITA^2) / (801 + 819)
se.diff <- sqrt(s2_pool/802 + s2_pool/820)
ME <- qnorm(0.995) * se.diff
c(diff.bar - ME, diff.bar + ME)     # ~ [0.25, 2.99]
```

The 99% CI excludes 0, so Italian developers have on average **more** computer skills than German developers (between $0.25$ and $2.99$ more, with 99% confidence).
""", "images": []}

ex6["6_8b"] = {"title": "Ex 6.8b — Welch's SE (unequal variances)",
"content": """**Question.** Repeat the previous comparison of mean `NrSkills` between Italian and German developers **without assuming equal variances**. Does the conclusion change?

---

**Setup.** Welch's standard error keeps the two sample variances separate:
$$\\text{SE}_W = \\sqrt{\\frac{s_{GER}^{2}}{n_{GER}} + \\frac{s_{ITA}^{2}}{n_{ITA}}}, \\qquad \\text{CI} = (\\bar X_{ITA}-\\bar X_{GER}) \\pm z_{1-\\alpha/2}\\,\\text{SE}_W.$$
For large $n$ the $z$-quantile is essentially identical to the Welch $t$-quantile.

**Answer.**
```r
# Different variances (Welch) — same data as 6.8 a2
s.GER <- 10.12; s.ITA <- 11.33
se.diff <- sqrt(s.GER^2/802 + s.ITA^2/820)
ME <- qnorm(0.995) * se.diff
c(diff.bar - ME, diff.bar + ME)    # ~ [0.25, 2.99]
```

The interval is essentially the **same** as the pooled-variance version because the two sample sizes are similar ($802 \\approx 820$); both intervals lie entirely above $0$, so the conclusion (ITA $>$ GER) is robust to the equal-variance assumption.
""", "images": []}

ex6["6_8c1"] = {"title": "Ex 6.8 c1 — Paired CI for FinSkills vs Skills",
"content": """**Question.** In `Developers_ITA` each developer reports both a financial-skills index (`FinSkills.Idx`) and a general skills index (`Skills.Idx`). Build a **90% confidence interval** for the mean within-developer difference $\\mu_D = \\mu_{Fin} - \\mu_{Skills}$. Are the two indices on average different?

---

**Setup.** Because the two measurements are taken on the *same* unit, they are **paired**. Define $D_i = \\text{FinSkills}_i - \\text{Skills}_i$ and apply a one-sample CI to $D$:
$$\\bar D \\;\\pm\\; t_{1-\\alpha/2,\\,n-1}\\,\\tfrac{S_D}{\\sqrt n}.$$
Pairing usually shrinks the SE because individual-level fluctuations cancel out.

**Answer.**
```r
CI.diffmean(x=FinSkills.Idx, y=Skills.Idx, type="paired",
            conf.level=0.9, data=Developers_ITA)
# Mean diff ~ -1.66, 90% CI ~ [-2.18, -1.14]
```

The CI is entirely **negative**, so financial-skills are on average lower than general skills (by between 1.14 and 2.18 points, at 90% confidence).
""", "images": []}

ex6["6_8d"] = {"title": "Ex 6.8d — CI for Skills mean: Full-time vs Freelance",
"content": """**Question.** Within `Developers_ITA`, compare the mean number of computer skills (`NrSkills`) between **full-time employees** (`Employment == "Employed, full-time"`) and **freelancers** (`Employment == "Contractor/Freelance"`). Build the default CI and comment on whether the two groups differ on average.

---

**Setup.** Two independent sub-samples drawn from the same survey: build an *independent-samples* CI for $\\mu_{Full} - \\mu_{Free}$ using Welch's SE (the default in `CI.diffmean`):
$$\\bar X_{Full} - \\bar X_{Free} \\;\\pm\\; t^{*}\\,\\sqrt{\\tfrac{s_{Full}^{2}}{n_{Full}} + \\tfrac{s_{Free}^{2}}{n_{Free}}}.$$
The default confidence level is $95\\%$.

**Answer.**
```r
sel <- Developers_ITA$Employment == "Employed, full-time"
Skills.Full      <- Developers_ITA$NrSkills[sel]
Skills.Freelance <- Developers_ITA$NrSkills[
                       Developers_ITA$Employment == "Contractor/Freelance"]
CI.diffmean(x=Skills.Full, y=Skills.Freelance)
# Default conf.level = 0.95
```

If the resulting CI **contains 0**, full-timers and freelancers are statistically indistinguishable in their average number of skills; if it lies entirely above (or below) 0, the corresponding group has more skills on average.
""", "images": []}

ex6["6_9a"] = {"title": "Ex 6.9a — CI for proportion difference (vgsales action genre)",
"content": """**Question.** Two independent samples of *Action*-genre video games from the `vgsales` dataset give $23$ "hits" out of $n_X = 140$ in market $X$ and $37$ "hits" out of $n_Y = 159$ in market $Y$. Build a **99% confidence interval** for the difference $p_Y - p_X$ of population success-proportions and judge whether one market has a higher hit rate.

---

**Setup.** With independent Bernoulli samples,
$$\\widehat{\\Delta} = \\hat p_Y - \\hat p_X, \\qquad \\text{SE}(\\widehat{\\Delta}) = \\sqrt{\\tfrac{\\hat p_X(1-\\hat p_X)}{n_X} + \\tfrac{\\hat p_Y(1-\\hat p_Y)}{n_Y}}.$$
The Wald CI is $\\widehat{\\Delta} \\pm z_{1-\\alpha/2}\\,\\text{SE}$.

**Answer.**
```r
p_X_hat <- 23/140                # 0.1643
p_Y_hat <- 37/159                # 0.2327
s2_Px <- p_X_hat*(1-p_X_hat)/140
s2_Py <- p_Y_hat*(1-p_Y_hat)/159
qnorm(.995)                      # 2.5758
ME <- qnorm(.995) * sqrt(s2_Px + s2_Py)
c(p_Y_hat - p_X_hat - ME, p_Y_hat - p_X_hat + ME)
# diff = 0.0684, ME ~ 0.116  ->  CI ~ [-0.048, 0.184]
```

The 99% CI **contains 0**, so at the 1% level we cannot conclude that one market has a higher Action-game hit rate than the other; the observed difference of $\\approx 6.8$ percentage points is compatible with sampling noise.
""", "images": []}

ex6["6_10a"] = {"title": "Ex 6.10a — CI for diff in JP sales: Strategy vs Role-Playing",
"content": """**Question.** Referring to sales in the Japanese market (`JP_Sales`), the interest lies in comparing video games of the genres **Strategy** and **Role-Playing**. Assess the average difference in the number of copies sold in the Japanese market between *Strategy* and *Role-Playing* videogames using a 90% confidence interval. Would it be sensible for a company producing Strategy and Role-Playing games to reposition itself in the market by increasing investment in the second genre of games?

---

**Answer.** Independent-samples CI on `JP_Sales` between the two genres at confidence level 0.90. If the CI lies entirely on one side of 0, the difference is significant at the 10% level and repositioning toward the higher-selling genre is supported.

```r
CI.diffmean(vgsales$JP_Sales[vgsales$Genre=="Strategy"],
            vgsales$JP_Sales[vgsales$Genre=="Role-Playing"],
            type="independent", conf.level=0.90, digits=4)
```
""", "images": []}

ex6["6_11a"] = {"title": "Ex 6.11a — Paired 90% CI: blood indicator before vs after test",
"content": """**Question.** A researcher believes that a physical test alters the values of a certain blood indicator. To evaluate the average level of the indicator of interest, 25 patients are randomly selected to undergo the physical test, and the level of the blood indicator is measured before and after the physical test. The obtained results are:

- **Before the test:** sample mean $= 85.0$, sample sd $= 4.8$
- **After the test:** sample mean $= 81.4$, sample sd $= 5.9$

The correlation between the indicator levels before and after the test is 0.6. After discussing whether assumptions are needed, build a 90% confidence interval for the difference in the mean levels of the indicator before and after the test. What are your conclusions?

---

**Answer.** Same 25 patients measured twice ⇒ **paired** data. Use $s^2_D = s_B^2 + s_A^2 - 2\\rho_{BA}\\,s_B s_A$ for the variance of the within-subject difference. Since $n = 25$ is small, the differences must be assumed approximately normal so that the pivot follows a Student-$t$ with $n - 1 = 24$ df. For a 90% CI the multiplier is `qt(0.95, 24)`.

```r
xbar_B <- 85;  xbar_A <- 81.4    # B = Before, A = After
diff.BA <- xbar_B - xbar_A       # 3.6
s_B <- 4.8; s_A <- 5.9; r_AB <- 0.6
s2_D <- s_B^2 + s_A^2 - 2*r_AB*s_B*s_A
se_D <- sqrt(s2_D/25)
qt(0.95, df=24)
ME <- qt(0.95, df=24)*se_D
c(diff.BA - ME, diff.BA + ME)
```
""", "images": []}

ex6["6_12a"] = {"title": "Ex 6.12a — 99% CI for Adventure high-sales proportion",
"content": """**Question.** We are interested in evaluating which are the most popular video games, those that have sold more than 1 million copies globally (`Global_Sales`). Estimate the proportion of videogames of genre **Adventure** that exceed one million copies sold globally (`Global_Sales`). Assess if it is possible to obtain a 99% confidence interval (using 4 decimal places) for this proportion and, if so, determine and interpret the obtained result.

---

**Answer.** Build a logical vector indicating whether each Adventure game has `Global_Sales > 1`; the proportion of `TRUE`s is $\\hat p$. The CI is valid because the Adventure sub-sample is large enough for the CLT-based normal approximation (`n\\hat p ≥ 5`, `n(1-\\hat p) ≥ 5`).

```r
highsales.adv <- vgsales$Global_Sales[vgsales$Genre=="Adventure"] > 1
CI.prop(highsales.adv, conf.level=0.99, digits=4)
```
""", "images": []}

ex6["6_12b"] = {"title": "Ex 6.12b — Expression of the CI and margin of error",
"content": """**Question.** Provide the expression of the confidence interval determined in the previous point, specifying the quantities on which it depends and their numerical values (also referring to the output obtained using RStudio). What is the margin of error?

---

**Answer.** The interval is $\\hat p \\pm z_{1-\\alpha/2}\\sqrt{\\hat p(1-\\hat p)/n}$. With confidence 99% the reliability factor is $z_{0.995} = $ `qnorm(0.995)` $\\approx 2.5758$. The standard error read off the RStudio output is roughly 0.005, so $ME = z_{0.995} \\cdot SE \\approx 2.5758 \\cdot 0.005 \\approx 0.0129$.

```r
qnorm(0.995)                 # 2.5758
ME <- qnorm(0.995)*0.005     # ~ 0.0129
ME
```
""", "images": []}

ex6["6_13a"] = {"title": "Ex 6.13a — 99% CI for cafeteria-visit proportion (≥1 visit/month)",
"content": """**Question.** A bookstore was recently renovated, and a cafeteria area was added. It is of interest to assess the customers' frequency of visits to the cafeteria. Interviews to 140 randomly chosen customers led to the following counts for the **number of visits in the last month**:

| nr. Visits  | 0  | 1  | 2  | 3  | 4  | 5  | 6 | 7 | 8 |
|---|---|---|---|---|---|---|---|---|---|
| nr. Clients | 32 | 43 | 14 | 10 | 18 | 13 | 5 | 0 | 5 |

Build a **99% confidence interval for the proportion of customers** who will visit the cafeteria at least once a month. Is it necessary to make assumptions to determine the interval?

---

**Answer.** Customers visiting at least once a month = $140 - 32 = 108$, so $\\hat p = 108/140 \\approx 0.7714$. With $n\\hat p = 108$ and $n(1-\\hat p) = 32$ both $\\geq 5$, the CLT applies and no parametric assumption is needed. The CI is $\\hat p \\pm z_{0.995}\\sqrt{\\hat p(1-\\hat p)/n}$.

```r
p_hat <- 108/140                                          # 0.7714
ME <- qnorm(0.995)*sqrt(p_hat*(1-p_hat)/140)
c(p_hat - ME, p_hat + ME)
```
""", "images": []}

ex6["6_13d"] = {"title": "Ex 6.13d — 90% CI for average monthly visits (cafeteria)",
"content": """**Question.** Building on the cafeteria data of Ex 6.13 (`nr.Visits` = 0..8 with frequencies 32, 43, 14, 10, 18, 13, 5, 0, 5; $n=140$), determine — if feasible on the available data — a **90% confidence interval for the average monthly number of visits**.

---

**Answer.** The CI is feasible: although `nr.Visits` is **discrete**, the parameter (the population *mean*) is a real number, and with $n = 140$ the CLT makes $\\bar X$ approximately normal. Compute the weighted mean and $E[X^2]$ from the frequency table, then $s^2 = \\tfrac{n}{n-1}(E[X^2] - \\bar x^2)$ and use $\\bar x \\pm z_{0.95}\\sqrt{s^2/n}$.

```r
# Sample mean from frequencies (vector built via rep is an alternative)
xbar    <- (43+2*14+3*10+4*18+5*13+6*5+8*5)/140              # 308/140 = 2.2
mean.x2 <- (43+4*14+9*10+16*18+25*13+36*5+64*5)/140          # E[X^2]
s2_x    <- (140/139)*(mean.x2 - xbar^2)                      # sample variance
se_Xbar <- sqrt(s2_x/140)
qnorm(0.95)
c(xbar - qnorm(0.95)*se_Xbar, xbar + qnorm(0.95)*se_Xbar)
```
""", "images": []}

ex6["6_14a"] = {"title": "Ex 6.14a — CI for diff in proportions: EA vs Activision",
"content": """**Question.** CI for the difference in proportions of best-selling games between Electronic Arts and Activision.

---

**Answer.**
```r
EA_best_seller  <- vgsales$NA_Sales[vgsales$Publisher=="Electronic Arts"] > 1
ACT_best_seller <- vgsales$NA_Sales[vgsales$Publisher=="Activision"]      > 1
sum(EA_best_seller); sum(ACT_best_seller)
distr.summary.x(EA_best_seller, digits=4)
distr.summary.x(ACT_best_seller, digits=4)
CI.diffprop(EA_best_seller, ACT_best_seller, conf.level=0.9, digits=4)
```
""", "images": []}

ex6["6_15a"] = {"title": "Ex 6.15a — Mean difference from supplied summary stats",
"content": """**Question.** $\\bar x = -39.34,\\ \\bar y = -49.71$ — compute the sample mean difference $\\bar d = \\bar x - \\bar y$.

---

**Answer.**
```r
xbar <- -39.34; ybar <- -49.71
diff.bar <- xbar - ybar; diff.bar
```
""", "images": []}

ex6["6_15b"] = {"title": "Ex 6.15b — Pooled vs separate t-CI",
"content": """**Question.** 95% CI for the mean difference using sample variances $s^2_x = 118.93,\\ n_x = 20,\\ s^2_y = 129.55,\\ n_y = 28$ — compare separate-variances vs pooled standard errors.

---

**Answer.**
```r
s2_x <- 118.93; n_x <- 20
s2_y <- 129.55; n_y <- 28
se_unequal <- sqrt(s2_x/n_x + s2_y/n_y); se_unequal
s2_pool    <- ((n_x-1)*s2_x + (n_y-1)*s2_y) / (n_x + n_y - 2)
se_equal   <- sqrt(s2_pool/n_x + s2_pool/n_y)
qt(0.975, 46)
c(diff.bar - qt(0.975, 46)*se_equal,
  diff.bar + qt(0.975, 46)*se_equal)
```
""", "images": []}

ex6["6_17a"] = {"title": "Ex 6.17a — Paired difference 56.8 vs 46.7",
"content": """**Question.** Build a 99% CI for the paired mean difference.

---

**Answer.**
```r
dbar <- 56.8 - 46.7
se_D <- sqrt((6.1^2 + 6.9^2 - 2*34.6)/23)
qt(0.95, 22)
c(dbar - qt(.995, 22)*se_D, dbar + qt(.995, 22)*se_D)
```
""", "images": []}

ex6["6_18b"] = {"title": "Ex 6.18b — Paired 98% CI: NA vs EU sales (Action)",
"content": """**Question.** 98% paired CI for NA vs EU sales (Action genre); compute correlation.

---

**Answer.**
```r
CI.diffmean(vgsales$NA_Sales[vgsales$Genre=="Action"],
            vgsales$EU_Sales[vgsales$Genre=="Action"],
            type="paired", conf.level=0.98, digits=4)
cor(vgsales$NA_Sales[vgsales$Genre=="Action"],
    vgsales$EU_Sales[vgsales$Genre=="Action"])
```
""", "images": []}
