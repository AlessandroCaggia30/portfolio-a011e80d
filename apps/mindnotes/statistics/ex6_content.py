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
""", "images": []}

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

**Answer.** Each game is observed in **both** markets, so the two samples are *paired* (matched on game). The natural estimator is the matched-pair mean $\\bar D = \\bar X_{NA} - \\bar X_{EU}$. Independence between NA and EU sales is implausible because the **same** game drives both.

```r
xbar_NA <- mean(vgsales$NA_Sales[vgsales$Genre=="Action"])
xbar_EU <- mean(vgsales$EU_Sales[vgsales$Genre=="Action"])
xbar_NA - xbar_EU
```
""", "images": []}

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

ex6["6_3a"] = {"title": "Ex 6.3a — 90% CI for mean Salary (DS, known $\\sigma$)",
"content": """**Question.** Build a 90% confidence interval for the mean `Salary` in `DS`, assuming the population standard deviation $\\sigma = 38\\,000$ is known.

---

**Answer.** With $\\sigma$ known and $n = 750$ (large), $\\bar X \\sim \\mathcal N(\\mu, \\sigma/\\sqrt n)$. The CI is $\\bar x \\pm z_{0.95}\\cdot\\sigma/\\sqrt n$, with $z_{0.95}=1.6449$.

```r
distr.summary.x(Salary, stats="mean", data=DS)
CI.mean(Salary, sigma=38000, conf.level=0.90, data=DS)
```
""", "images": []}

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
"content": """**Question.** Build a 99% CI for the proportion of female customers. Then determine the minimum sample size to achieve a target margin of error at 95% confidence.

---

**Answer.** Use `CI.prop`. For the sample-size invert
$$\\text{ME} = z_{1-\\alpha/2}\\sqrt{\\hat p(1-\\hat p)/n} \\le \\text{ME}^\\star \\Rightarrow n \\ge (z_{1-\\alpha/2}\\hat p_{\\max}/\\text{ME}^\\star)^2.$$
The worst-case variance is at $\\hat p = 0.5$ (conservative formula).

```r
CI.prop(Gender, success="Female", data=DS)
qnorm(0.995)
z_05  <- qnorm(0.95)
(z_05 * 0.5 / 0.11)^2   # minimum sample size (conservative)
```
""", "images": []}

ex6["6_3c"] = {"title": "Ex 6.3c — 99% CI for diff in Employment proportions (GER vs ITA)",
"content": """**Question.** Compare the proportions of `Employment == "Employed, full-time"` between German and Italian developers and build a 99% CI for the difference (`Developers_ITA`).

---

**Answer.** Two **independent** samples ($n_{ITA}=802$, $n_{GER}=820$). Use
$$(\\hat p_{GER}-\\hat p_{ITA}) \\pm z_{1-\\alpha/2}\\sqrt{\\tfrac{\\hat p_{GER}(1-\\hat p_{GER})}{n_{GER}} + \\tfrac{\\hat p_{ITA}(1-\\hat p_{ITA})}{n_{ITA}}}.$$

```r
distr.table.x(Developers_ITA$Employment, f.digits=3)
phat_GER  <- 1 - 0.903                  # full-time share (GER)
phat_ITA  <- 0.15                       # full-time share (ITA)
diff.prop <- phat_GER - phat_ITA
SE.diff   <- sqrt( phat_GER*(1-phat_GER)/820 +
                   phat_ITA*(1-phat_ITA)/802 )
ME <- qnorm(0.995) * SE.diff
c(diff.prop - ME, diff.prop + ME)
```
""", "images": []}

ex6["6_3d"] = {"title": "Ex 6.3d — 99% CI for AmountSpent: Married vs Single (Close, 0 children)",
"content": """**Question.** Build a 99% CI for the difference in mean `AmountSpent` between Married and Single customers, restricted to the sub-population with `Location == "Close"` and `Children == 0`.

---

**Answer.** Two **independent** sub-samples (different individuals). Use `CI.diffmean` after sub-setting, or via `by=`.

```r
# Approach 1: explicit subsets
sel.M <- DS$Location=="Close" & DS$Children==0 & DS$Married=="Married"
sel.S <- DS$Location=="Close" & DS$Children==0 & DS$Married=="Single"
CI.diffmean(x=DS$AmountSpent[sel.M], y=DS$AmountSpent[sel.S],
            conf.level=0.99, digits=3)

# Approach 2: by= argument
sel.sub <- DS$Location=="Close" & DS$Children==0
CI.diffmean(x=DS$AmountSpent[sel.sub], by=DS$Married[sel.sub],
            conf.level=0.99, digits=3)
```
""", "images": []}

ex6["6_4a"] = {"title": "Ex 6.4a — Pooled-variance CI for vgsales mean (NF vs F)",
"content": """**Question.** Compute the pooled-variance CI for the difference in two-group sample means, with $\\bar x_{NF}=90.7, \\bar x_F=87.2, s_{NF}=5.4, s_F=4.8, n_{NF}=n_F=10$. Also provide the known-variance version ($\\sigma_{NF}=5.2, \\sigma_F=5$).

---

**Answer.**
```r
x_bar_NF <- 90.7; x_bar_F <- 87.2
diff.bar <- x_bar_NF - x_bar_F
sd_NF <- 5.4; sd_F <- 4.8

# Pooled variance, df = n_NF + n_F - 2 = 18 here
s2_pool <- (9*sd_NF^2 + 9*sd_F^2)/18
se.diff <- sqrt(s2_pool/10 + s2_pool/10)
qt(0.975, df=18)
ME <- qt(0.975, df=18) * se.diff
c(diff.bar - ME, diff.bar + ME)

# (b) Known-variance form, with sigma_NF=5.2, sigma_F=5
sigma_NF <- 5.2; sigma_F <- 5
SE.diff  <- sqrt(sigma_NF^2/10 + sigma_F^2/10)
qnorm(0.975)
ME.k <- qnorm(0.975) * SE.diff
c(diff.bar - ME.k, diff.bar + ME.k)
```
""", "images": []}

ex6["6_6a"] = {"title": "Ex 6.6a — 95% CI for a single proportion ($n=100$, 40 successes)",
"content": """**Question.** Build a 95% CI for the proportion of successes from a sample of $n=100$ customers, $40$ of whom answered favourably.

---

**Answer.** Point estimate $\\hat p = 0.4$; CI uses $\\sqrt{\\hat p(1-\\hat p)/n}$ and $z_{0.025}$.

```r
phat  <- 40/100
var_p <- phat*(1-phat)/100
se    <- sqrt(var_p)
var_p; se
qnorm(0.975)            # percentile
ME    <- qnorm(0.975) * se
c(phat - ME, phat + ME) # interval
```
""", "images": []}

ex6["6_6b"] = {"title": "Ex 6.6b — Same 95% CI with $n = 1000$",
"content": """**Question.** Build the 95% CI again with $n=1000$ (same $\\hat p = 0.4$). Comment on the gain in precision.

---

**Answer.** SE shrinks as $1/\\sqrt{n}$, so the interval is roughly $\\sqrt{10}\\approx 3.16$ times narrower.

```r
se_p1000  <- sqrt(0.4*0.6/1000)
se_p1000
ME_p1000  <- qnorm(0.975) * se_p1000
c(0.4 - ME_p1000, 0.4 + ME_p1000)
```
""", "images": []}

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
"content": """**Question.** Build a 99% CI for $\\hat p_A - \\hat p_B$ with $\\hat p_A=0.4, \\hat p_B=0.36, n_A=100, n_B=120$.

---

**Answer.** Unpooled SE; $z_{0.005} = $ `qnorm(0.995)` $\\approx 2.576$.

```r
p_A <- 0.4; p_B <- 0.36
p_A - p_B                                # point estimate = 0.04
se.diff <- sqrt(p_A*(1-p_A)/100 + p_B*(1-p_B)/120)
se.diff
ME <- qnorm(0.995) * se.diff
c(p_A - p_B - ME, p_A - p_B + ME)
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
