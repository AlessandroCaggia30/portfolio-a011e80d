"""Ex 7 — Hypothesis tests (one and two-sample), chi-squared, intro to regression."""

ex7 = {}

ex7["7_1a"] = {"title": "Ex 7.1a — One-sample t-test on Weeks (H1: μ<45), NewHired",
"content": """**Question.** With the `NewHired` sample (n=47), assess whether on average people who relied on the job agency found a new job in **less than 45 weeks**. a1) Assumptions needed. a2) Test statistic, realisation, p-value, conclusion at α=0.05. a3) Would the conclusion change at α=0.01? a4) Meaning of the significance level.

---

**Answer.**

**a1)** The 47 workers must be representative of the population of interest. The sample size is not particularly large but is enough to apply the **CLT**, so $\\bar X$ is approximately normal without needing a normality assumption on `Weeks`.

**a2)** Hypotheses (the most serious error is concluding the average is < 45 when it is in fact higher, so set the claim as the alternative):
$$H_0: \\mu \\geq 45 \\quad \\text{vs} \\quad H_1: \\mu < 45$$

Since $\\sigma^2$ is unknown, use the **t-statistic** $T = (\\bar X - \\mu_0)/(S/\\sqrt n) \\sim t_{n-1}$ under $H_0$. With $\\bar X = 40.1915$, $S = 17.2206$, $S/\\sqrt{47} = 2.5119$:
$$t_\\text{obs} = \\frac{40.1915 - 45}{2.5119} = -1.9143$$

p-value $= P(t_{46} \\leq -1.9143) = 0.0309$ (Student t), or 0.0278 with the normal approximation. Both < 0.05 → **reject $H_0$**: enough evidence that the average time to find a new job is < 45 weeks.

```r
TEST.mean(Weeks, mu0=45, alternative="less", data=NewHired)
# Manual:
xbar <- mean(NewHired$Weeks); s <- sd(NewHired$Weeks); n <- 47
tstat <- (xbar - 45) / (s/sqrt(n)); tstat
pt(tstat, df=n-1)              # Student t p-value
pnorm(tstat)                   # normal approximation
```

**a3)** At α=0.01 the p-value (0.0309) > α, so we would **not reject** $H_0$.

**a4)** α is the maximum probability of a **Type I error** (rejecting $H_0$ when true). Since $H_0$ specifies a set of values, the max is attained at the value closest to $H_1$, i.e. μ=45.
""", "images": []}

ex7["7_1b"] = {"title": "Ex 7.1b — Known σ²=16: power and Type II error probabilities",
"content": """**Question.** Now assume the variance of `Weeks` is known and equal to 16. b1) At level α=0.1, what is the probability of concluding the average is < 45 weeks when it is actually **50** weeks? b2) Same probability when it is actually **43** weeks?

---

**Answer.** With σ²=16 known, use the **z-test**. Rejection region (one-sided, lower):
$$R: \\bar X < \\mu_0 - z_{0.1}\\,\\sigma/\\sqrt{n} = 45 - 1.28 \\cdot 4/\\sqrt{47} = 44.25$$

**b1)** True mean = 50 (μ=50 belongs to $H_0$). Concluding < 45 = rejecting $H_0$ → this is a **Type I error**:
$$P(\\bar X < 44.25 \\mid \\mu=50) = P\\!\\left(Z < \\frac{44.25-50}{4/\\sqrt{47}}\\right) \\approx 0$$

**b2)** True mean = 43 (μ=43 belongs to $H_1$). The complementary event (NOT rejecting $H_0$) is a **Type II error**:
$$P(\\bar X \\geq 44.25 \\mid \\mu=43) = P\\!\\left(Z > \\frac{44.25-43}{4/\\sqrt{47}}\\right) \\approx 0.016$$

```r
qnorm(0.9)                                          # 1.28
crit <- 45 - 1.28 * 4/sqrt(47); crit                # 44.25
pnorm(crit, mean=50, sd=4/sqrt(47))                 # b1) ≈ 0
1 - pnorm(crit, mean=43, sd=4/sqrt(47))             # b2) ≈ 0.016
```
""", "images": []}

ex7["7_1c"] = {"title": "Ex 7.1c — One-proportion test: struggle >52 weeks vs p₀=0.10",
"content": """**Question.** A worker claims the **proportion** of agency-relying workers who struggle more than one year (>52 weeks) to find a job is higher than 10%. Test at α=0.05.

---

**Answer.** Most serious error = concluding p>0.10 when it is not. Put the claim in $H_1$:
$$H_0: p \\leq 0.10 \\quad \\text{vs} \\quad H_1: p > 0.10$$

Sample proportion: $\\hat p = 7/47 \\approx 0.1498$. Test statistic under $H_0$ (using $p_0$ for the SE):
$$Z = \\frac{\\hat p - p_0}{\\sqrt{p_0(1-p_0)/n}} = \\frac{0.1498 - 0.10}{\\sqrt{0.10 \\cdot 0.90 / 47}} \\approx 1.12$$

p-value = $P(Z > 1.12) \\approx 0.13$ > 0.05 → **do not reject $H_0$**. Not enough empirical support for the worker's claim.

```r
phat <- mean(NewHired$Weeks > 52); phat             # ≈ 0.1498
n <- 47; p0 <- 0.10
z <- (phat - p0) / sqrt(p0*(1-p0)/n); z              # ≈ 1.12
1 - pnorm(z)                                         # p-value ≈ 0.13
TEST.prop(Weeks > 52, p0=0.10, alternative="greater", data=NewHired)
```
""", "images": []}

ex7["7_3a"] = {"title": "Ex 7.3a — Two-proportion z-test: cafeteria visit pre/post promotion",
"content": """**Question.** Pre-promotion (n=140) and post-promotion (n=159) samples of monthly cafeteria visits. Test whether the promotion **increases the proportion** of customers visiting at least once a month.

---

**Answer.** Customers with 0 stops are non-visitors; ≥1 stop = visitor. From the tables, non-visitors are 32 in both pre and post. So:
$$\\hat p_\\text{PRE} = (140-32)/140 = 0.7714, \\quad \\hat p_\\text{POST} = (159-32)/159 = 0.7987$$

Hypotheses (most serious error: extending an ineffective promotion):
$$H_0: p_\\text{POST} = p_\\text{PRE} \\quad \\text{vs} \\quad H_1: p_\\text{POST} > p_\\text{PRE}$$

Pooled proportion under $H_0$:
$$\\hat p = \\frac{n_\\text{PRE}\\hat p_\\text{PRE} + n_\\text{POST}\\hat p_\\text{POST}}{n_\\text{PRE}+n_\\text{POST}} = 0.7859$$

Standard error under $H_0$ and observed difference:
$$se = \\sqrt{\\hat p(1-\\hat p)\\!\\left(\\tfrac{1}{n_\\text{PRE}}+\\tfrac{1}{n_\\text{POST}}\\right)} = 0.0475, \\quad \\hat p_\\text{POST}-\\hat p_\\text{PRE} = 0.0273$$

z-statistic and p-value:
$$z = \\frac{0.0273}{0.0475} \\approx 0.575, \\quad \\text{p-value} = P(Z > 0.575) = 0.2827$$

**Do not reject** $H_0$ at any standard $\\alpha$ (≤ 0.10): no empirical evidence that the promotion increases visit propensity, so **do not extend** it to other bookstores.

```r
nPRE <- 140; nPOST <- 159
pPRE <- (140-32)/140; pPOST <- (159-32)/159
phat <- (nPRE*pPRE + nPOST*pPOST)/(nPRE+nPOST); phat
se   <- sqrt(phat*(1-phat)*(1/nPRE + 1/nPOST)); se
1 - pnorm(pPOST - pPRE, mean=0, sd=se)              # p-value ≈ 0.2827
TEST.diffprop(x=Stops_POST>=1, y=Stops_PRE>=1,
              pdiff=0, alternative="greater")
```
""", "images": []}

ex7["7_3b"] = {"title": "Ex 7.3b — Two-proportion test: visiting >4 times/month pre vs post",
"content": """**Question.** Assess whether the promotion has a significant impact on the proportion of customers who visit the cafeteria **more than 4 times** a month. Report all the measures you refer to.

---

**Answer.** Let $p_\\text{PRE}, p_\\text{POST}$ now denote the proportions of customers who visit the cafeteria **more than 4 times** per month. Count customers with >4 stops directly from the frequency table — pre: $13+5+0+5=23$ out of 140; post: $17+6+5+9=37$ out of 159.
$$\\hat p_\\text{PRE} = 23/140 = 0.1643, \\quad \\hat p_\\text{POST} = 37/159 = 0.2327$$

Same one-sided framing as part (a) — the most serious error is extending an ineffective promotion to other branches:
$$H_0: p_\\text{POST} = p_\\text{PRE} \\quad \\text{vs} \\quad H_1: p_\\text{POST} > p_\\text{PRE}$$

Sample sizes are large enough for the **normal approximation**. Pooled proportion and standard error under $H_0$:
$$\\hat p = \\frac{n_\\text{PRE}\\hat p_\\text{PRE} + n_\\text{POST}\\hat p_\\text{POST}}{n_\\text{PRE}+n_\\text{POST}} = 0.2007, \\quad se_0 = \\sqrt{\\hat p(1-\\hat p)\\!\\left(\\tfrac{1}{n_\\text{PRE}}+\\tfrac{1}{n_\\text{POST}}\\right)} = 0.0464$$

Observed difference $\\hat p_\\text{POST}-\\hat p_\\text{PRE}=0.0684$, so

$$\\text{p-value} = P(Z > 0.0684/0.0464) = P(Z > 1.474) \\approx 0.0703$$

**Decision.** At $\\alpha=0.05$ we **do not reject** $H_0$; at $\\alpha=0.10$ we would. Heavy-user growth from 16.4% to 23.3% is borderline — using $\\alpha=0.10$ is the *less conservative* choice toward the null and risks rolling out a promotion that does not actually expand the heavy-user base.

```r
nPRE <- 140; nPOST <- 159
pPRE <- 23/140; pPOST <- 37/159
phat <- (nPRE*pPRE + nPOST*pPOST)/(nPRE+nPOST); phat   # 0.2007
se   <- sqrt(phat*(1-phat)*(1/nPRE + 1/nPOST)); se     # 0.0464
1 - pnorm(pPOST - pPRE, mean=0, sd=se)                 # ≈ 0.0703
1 - pnorm((pPOST - pPRE)/se)                           # equivalent
TEST.diffprop(x=Stops_POST>4, y=Stops_PRE>4,
              pdiff=0, alternative="greater")
```
""", "images": []}

ex7["7_4a"] = {"title": "Ex 7.4a — Chi-squared goodness of fit: DS$History uniform (0.25 each)",
"content": """**Question (DS).** Are the frequencies of the levels of the variable `History` equal to each other? What conclusion do you reach using a significance level of 0.05? Report the R functions used.

---

**Answer.** Goodness-of-fit test on the four levels (`Low`, `Medium`, `High`, `None`) with
$$H_0: p_{Low}=p_{Med}=p_{High}=p_{None}=0.25 \\quad \\text{vs} \\quad H_1: \\text{at least one } p \\neq 0.25.$$
Observed counts (n=750): High 205 (0.27), Low 181 (0.24), Medium 150 (0.20), None 214 (0.29). Expected under $H_0$: $0.25\\cdot 750 = 187.5$ in each cell.

```r
chisq.test(x=table(DS$History), p=c(0.25, 0.25, 0.25, 0.25))
```

The test statistic is $\\chi^2 = \\sum_k (O_k - E_k)^2/E_k = 13.104$, with **p-value $= 0.0044$**. Reject $H_0$ at any level $> 0.0044$ (in particular at $\\alpha=0.05$): the variable `History` is **not** uniformly distributed.
""", "images": []}

ex7["7_4b"] = {"title": "Ex 7.4b — Goodness of fit on History within the two Location sub-populations",
"content": """**Question.** Would your answer to point (a) change if you referred separately to the distribution of `History` in the two sub-populations of customers who live near (`Close`) or far from a competing physical store (`Location`)?

---

**Answer.** Restrict the goodness-of-fit test of (a) to each `Location` stratum. Same null in each stratum:
$$H_0: p_{Low}=p_{Med}=p_{High}=p_{None}=0.25 \\quad \\text{vs} \\quad H_1: \\text{at least one } p \\neq 0.25,$$
with test statistic $\\chi^2=\\sum_k (O_k-E_k)^2/E_k \\sim \\chi^2_{4-1}=\\chi^2_3$ under $H_0$. Critical value at $\\alpha=0.05$: $\\chi^2_{3,\\,0.95}=7.815$.

```r
chisq.test(x=table(DS$History[DS$Location=="Close"]),
           p=c(0.25, 0.25, 0.25, 0.25))
chisq.test(x=table(DS$History[DS$Location=="Far"]),
           p=c(0.25, 0.25, 0.25, 0.25))
qchisq(0.95, df=3)                                       # 7.815
```

**Location = Close** ($n=360$, $E_k=90$ each): observed counts give $\\chi^2 = 25.556$, p-value $= 0.0000118$. Since $25.556 > 7.815$ → **reject $H_0$**.

**Location = Far** ($n=390$, $E_k=97.5$ each): $\\chi^2 \\approx 63.766$, p-value $\\approx 9.3\\cdot 10^{-14} \\approx 0$. Since $63.766 \\gg 7.815$ → **reject $H_0$** at any level.

**Conclusion.** The answer to (a) is **confirmed and reinforced** in both strata: in each `Location` sub-population the four `History` levels are not equally frequent. The non-uniformity of (a) is therefore not an artifact of mixing two homogeneous sub-populations — it is genuinely present at *both* distances from the competitor.
""", "images": []}

ex7["7_5a"] = {"title": "Ex 7.5a — Fish-diet vs cholesterol: setup, pooled variance, rejection region (normal)",
"content": """**Question.** Two independent samples of 100 males each follow a Standard diet vs a Seafood diet for 6 months. Cholesterol summaries: $\\bar X_\\text{Std}=210.1$, $s^2_\\text{Std}=37.4$; $\\bar X_\\text{Sea}=196.8$, $s^2_\\text{Sea}=33.5$. The researchers claim the difference $\\mu_\\text{Std}-\\mu_\\text{Sea}$ is significantly greater than 10 at $\\alpha=0.05$. Determine the **rejection region** and conclude.

---

**Answer.** Hypotheses:
$$H_0:\\mu_\\text{Std}-\\mu_\\text{Sea} \\le 10 \\quad \\text{vs} \\quad H_1:\\mu_\\text{Std}-\\mu_\\text{Sea} > 10.$$
Pooled variance (assuming equal variances): $s^2_\\text{pool} = \\frac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1+n_2-2} = \\frac{99\\cdot 37.4 + 99\\cdot 33.5}{198} = 35.45$.

Using the normal approximation (central limit theorem, large samples), reject $H_0$ at level 0.05 if
$$(\\bar X_\\text{Std}-\\bar X_\\text{Sea}) > 10 + z_{0.05}\\sqrt{2\\cdot 35.45/100} = 10 + 1.645\\cdot \\sqrt{0.7090} = 11.385.$$

The observed difference is $210.1 - 196.8 = 13.3 > 11.385$ → **reject $H_0$**. The researchers' claim is backed up by the data.

```r
xbar.s <- 210.1; var.s <- 37.4; n.s <- 100
xbar.f <- 196.8; var.f <- 33.5; n.f <- 100
s2.pool <- ((n.s-1)*var.s + (n.f-1)*var.f)/(n.s + n.f - 2)   # 35.45
thresh  <- 10 + qnorm(0.95)*sqrt(2*s2.pool/n.s)              # 11.385
(xbar.s - xbar.f); thresh                                    # 13.3  vs 11.385
```
""", "images": []}

ex7["7_5b"] = {"title": "Ex 7.5b — Student's t version of the rejection-region test",
"content": """**Question.** Repeat the test in (a) using Student's t distribution rather than the normal approximation.

---

**Answer.** Same hypotheses as (a): $H_0:\\mu_\\text{Std}-\\mu_\\text{Sea}\\le 10$ vs $H_1:\\mu_\\text{Std}-\\mu_\\text{Sea}>10$.

**Assumptions for the t-version.** (i) **Normality** of cholesterol in both populations (the t-statistic is *exactly* $t_{n_1+n_2-2}$ only when the populations are normal; with $n_1=n_2=100$ the CLT already protects against mild departures); (ii) **equal variances** (justifies the pooled $s^2_\\text{pool}=35.45$, with $s^2_\\text{Std}/s^2_\\text{Sea}=37.4/33.5=1.12$ — easily compatible with equality).

**Test statistic.** Under $H_0$ (taking equality at the boundary $\\mu_\\text{Std}-\\mu_\\text{Sea}=10$),
$$T = \\frac{(\\bar X_\\text{Std}-\\bar X_\\text{Sea}) - 10}{\\sqrt{s^2_\\text{pool}(1/n_1 + 1/n_2)}} = \\frac{(\\bar X_\\text{Std}-\\bar X_\\text{Sea}) - 10}{\\sqrt{2\\,s^2_\\text{pool}/n}} \\sim t_{n_1+n_2-2}=t_{198}.$$

**Rejection region.** Reject $H_0$ when $T > t_{198,\\,0.05}=1.6526$, i.e. when
$$\\bar X_\\text{Std}-\\bar X_\\text{Sea} > 10 + t_{198,\\,0.05}\\cdot\\sqrt{2\\cdot 35.45/100} = 10 + 1.6526\\cdot 0.8420 = 11.392.$$

**Decision.** Observed difference $13.3 > 11.392$ → **reject $H_0$** at $\\alpha=0.05$. Realised t-statistic: $t_\\text{obs}=(13.3-10)/0.8420=3.92$, far in the right tail.

**Comparison with the normal version of (a).**

| | Normal (a) | Student's t (b) |
|---|---|---|
| Critical value | $z_{0.05}=1.6449$ | $t_{198,\\,0.05}=1.6526$ |
| Rejection threshold for $\\bar X_\\text{Std}-\\bar X_\\text{Sea}$ | $11.385$ | $11.392$ |
| Observed difference | $13.3$ | $13.3$ |
| Decision | Reject $H_0$ | Reject $H_0$ |

The two thresholds differ by less than 0.01 cholesterol units. With df $=198$, Student's t is virtually indistinguishable from the standard normal (tails differ at the 4th decimal); the gap matters only for small $n$ — e.g. with $n_1=n_2=5$ (df=8) the critical value would jump to $t_{8,\\,0.05}=1.860$, materially shifting the threshold.

```r
xbar.s <- 210.1; var.s <- 37.4; n.s <- 100
xbar.f <- 196.8; var.f <- 33.5; n.f <- 100
s2.pool <- ((n.s-1)*var.s + (n.f-1)*var.f)/(n.s + n.f - 2)   # 35.45
se      <- sqrt(2*s2.pool/n.s)                              # 0.8420
qt(0.95, df=198)                                            # 1.6526
thresh.t <- 10 + qt(0.95, df=198)*se;  thresh.t             # 11.392
t.obs    <- ((xbar.s - xbar.f) - 10)/se; t.obs              # 3.92
1 - pt(t.obs, df=198)                                       # p-value ~ 6e-5
# Built-in equivalent (provides the same t, df, threshold, p-value):
# TEST.diffmean(... alternative="greater", mu0=10, var.test=TRUE)
```

**Conclusion.** The Student's t test reproduces (a) almost exactly — same decision, virtually the same threshold — confirming that with $n=100$ per group the normal approximation of (a) is fully adequate.
""", "images": []}

ex7["7_5c"] = {"title": "Ex 7.5c — p-value of the sample realisation",
"content": """**Question.** Compute the **p-value** for the observed difference 13.3 and confirm the conclusion. Specify the R functions used.

---

**Answer.** The p-value is the probability, under $H_0$ (i.e. with $\\mu_\\text{Std}-\\mu_\\text{Sea}=10$), of drawing a difference at least as large as $13.3$:
$$\\text{p-value} = \\Pr(\\bar X_\\text{Std}-\\bar X_\\text{Sea} > 13.3 \\mid \\mu_\\text{Std}-\\mu_\\text{Sea}=10).$$

Using the normal approximation with $se = \\sqrt{2\\cdot 35.45/100} = 0.8420$:

```r
# Normal approximation
1 - pnorm(13.3, mean=10, sd=sqrt(2*35.45/100))         # ≈ 0.000044
1 - pnorm((13.3 - 10)/sqrt(2*35.45/100))               # equivalent
# Student's t (df = n1 + n2 - 2 = 198)
1 - pt((13.3 - 10)/sqrt(2*35.45/100), df=198)          # ≈ 0.000061
```

p-value $\\approx 4.4\\cdot 10^{-5}$ (normal) or $\\approx 6.1\\cdot 10^{-5}$ (Student's t). Both extremely small — $H_0$ would be rejected at any conventional significance level, confirming the conclusion from the rejection-region approach.
""", "images": []}

ex7["7_6a"] = {"title": "Ex 7.6a — Arcade wi-fi: paired test on average daily revenue (n=7, before vs after)",
"content": """**Question.** The manager of an arcade chain recently activated free wi-fi at one of its premises. Before extending the service to the other premises, he wants to check — accounting for the costs to be sustained — whether the service will promote an **increase in daily revenues**. He compares daily revenues (hundreds of €) recorded in a typical week **before** the installation ($n=7$) with those recorded in a week **three months after** ($n=7$). Sample summaries:

| | Mean | Variance |
|---|---|---|
| PRE  | $\\bar X_\\text{PRE}=13$  | $s^2_\\text{PRE}=12$ |
| POST | $\\bar X_\\text{POST}=16$ | $s^2_\\text{POST}=21$ |

A covariance of $s_\\text{PRE,POST}=11$ was also observed between the daily revenues recorded in the two weeks.

State the hypotheses and the assumptions required; then decide at $\\alpha=0.05$ whether it is reasonable to extend the wi-fi service to the other premises.

---

**Answer.** The two samples are **paired** (same days of the week, observed before vs after on the *same* premises), so we work on the differences $D_i = X^\\text{POST}_i - X^\\text{PRE}_i$. Because $n=7$ is small we cannot invoke the CLT — we must **assume joint normality** of the two populations, which makes the differences $D_i$ also normal.

Hypotheses (one-sided — wi-fi extension is justified only if revenues *increase*, so "no increase" goes in $H_0$):
$$H_0: \\mu_\\text{POST}=\\mu_\\text{PRE} \\quad \\text{vs} \\quad H_1: \\mu_\\text{POST} > \\mu_\\text{PRE}.$$

Sample mean of the differences: $\\bar D = 16-13 = 3$. Sample variance of the differences (using the formula for paired data, which **exploits the covariance**):
$$s_D^2 = s^2_\\text{POST}+s^2_\\text{PRE} - 2\\,s_\\text{PRE,POST} = 21+12-2\\cdot 11 = 11.$$
Standard error of $\\bar D$: $se(\\bar D)=\\sqrt{s_D^2/n}=\\sqrt{11/7}=1.2536$.

**Rejection-region approach** (test statistic under $H_0$ is Student's $t_{n-1}=t_6$):
$$\\bar D > t_{6,\\,0.05}\\cdot se(\\bar D) = 1.94318\\cdot 1.2536 = 2.4359.$$
Observed $\\bar D = 3 > 2.4359$ → **reject $H_0$**.

**p-value approach**:
$$\\text{p-value}=\\Pr(t_6 > \\bar D/se(\\bar D)) = \\Pr(t_6 > 3/1.2536)=\\Pr(t_6 > 2.3931)\\approx 0.027.$$

```r
dbar <- 16 - 13                       # 3
sD2  <- 21 + 12 - 2*11                # 11   (paired-data formula)
seD  <- sqrt(sD2/7)                   # 1.2536
qt(0.95, df=6)                        # 1.94318  -> t-critical
qt(0.95, df=6)*seD                    # 2.4359   -> RR threshold for Dbar
dbar/seD                              # 2.3931   -> observed t
1 - pt(dbar/seD, df=6)                # p-value ≈ 0.027
```

**Decision.** p-value $\\approx 0.027 < 0.05$ → **reject $H_0$**: at the 5% level the data provide evidence that the wi-fi service has increased average daily revenues, so it is reasonable to **extend the service** to the other premises. (Note: at the more conservative $\\alpha=0.01$ the conclusion would flip — $H_0$ would *not* be rejected, since $0.027 > 0.01$.)
""", "images": []}

ex7["7_6b"] = {"title": "Ex 7.6b — Effect on the decision if statistics referred to 2 weeks (n=14)",
"content": """**Question.** Without making calculations but carefully justifying the answer, say whether the decision in (a) would change if the sample statistics referred to **2 weeks** (n=14) instead of 1.

---

**Answer.** If the data referred to 2 weeks, i.e. $n=14$ days, the **standard error** $se(\\bar D)=\\sqrt{s_D^2/n}$ would **decrease** and the **degrees of freedom** $n-1$ of Student's t would **increase**, since the sample size increases.

*Rejection-region threshold.* The threshold $t_{n-1,\\alpha}\\cdot se(\\bar D)$ would be **lower**: an increase in df reduces the percentile of Student's t, so $t_{13,0.05} < t_{6,0.05}$, and this is multiplied by a smaller standard error since $se(\\bar D_7) > se(\\bar D_{14})$. Hence
$$t_{13,0.05}\\cdot se(\\bar D_{14}) \\;<\\; t_{6,0.05}\\cdot se(\\bar D_7) = 2.4359.$$
The observed $\\bar D = 3$ still exceeds this lower threshold → **still reject $H_0$**.

*p-value.* The standardised statistic $\\bar D/se(\\bar D)$ would **increase** (smaller denominator) and would be evaluated on a Student's t with **more degrees of freedom**, whose tails are *lighter* (the right-tail probability decreases as df grows). Both effects shrink the right-tail probability:
$$\\Pr\\!\\left(t_{13} > \\tfrac{\\bar D}{se(\\bar D_{14})}\\right) \\;<\\; \\Pr\\!\\left(t_{6} > \\tfrac{\\bar D}{se(\\bar D_{14})}\\right) \\;<\\; \\Pr\\!\\left(t_{6} > \\tfrac{\\bar D}{se(\\bar D_7)}\\right) = 0.027.$$

In both approaches the conclusion is **reinforced**: $H_0$ is rejected at $\\alpha=0.05$, and very likely also at $\\alpha=0.01$ (contrary to part (a), where $H_0$ was not rejected at 0.01).

```r
# Illustration: assume the same sample statistics carry over to n=14
seD7  <- sqrt(11/7)                      # 1.2536  (original)
seD14 <- sqrt(11/14)                     # 0.8864  (smaller)
qt(0.95, df=6) *seD7                     # 2.4359  threshold n=7
qt(0.95, df=13)*seD14                    # 1.5673  threshold n=14 (lower)
1 - pt(3/seD7 , df=6)                    # 0.0270  p-value  n=7
1 - pt(3/seD14, df=13)                   # 0.0035  p-value  n=14 (< 0.01)
```
""", "images": []}

ex7["7_7a"] = {"title": "Ex 7.7a — Two-proportion z-test: ChatGPT use by Younger vs Senior (Developers_ITA)",
"content": """**Question.** Test $H_0: p_{\\text{Young}} = p_{\\text{Senior}}$ vs $H_1: p_{\\text{Young}} > p_{\\text{Senior}}$, where $p$ is the proportion of developers using AI tools (e.g. ChatGPT) in their work, in the two independent subpopulations of younger (`Younger=TRUE`) and more senior (`Younger=FALSE`) developers.

---

**Answer.** Sample sizes are large enough to approximate the distribution of the difference between the two sample proportions with a normal distribution. The two sample proportions are 0.57 and 0.40, and the realisation of the test statistic equals **4.77** with p-value < 0.0001, leading to rejection of $H_0$ whatever the chosen significance level.

```r
TEST.diffprop(x=Developers_ITA$ChatGPT[Developers_ITA$Younger==TRUE],
              y=Developers_ITA$ChatGPT[Developers_ITA$Younger==FALSE],
              success.x="Yes", pdiff=0, alternative="greater", digits=4)
# Manual computation:
phat_y <- mean(Developers_ITA$ChatGPT[Developers_ITA$Younger==TRUE]=="Yes")
phat_s <- mean(Developers_ITA$ChatGPT[Developers_ITA$Younger==FALSE]=="Yes")
n_y    <- sum(Developers_ITA$Younger==TRUE)
n_s    <- sum(Developers_ITA$Younger==FALSE)
phat0  <- (n_y*phat_y + n_s*phat_s)/(n_y + n_s)
se0    <- sqrt(phat0*(1-phat0)*(1/n_y + 1/n_s))
z      <- (phat_y - phat_s)/se0; z
1 - pnorm(z)
```
""", "images": []}

ex7["7_7b"] = {"title": "Ex 7.7b — Chi-squared independence: Age_Class × LearnTool (Developers_ITA)",
"content": """**Question.** Test $H_0$: `Age_Class` and `LearnTool` are independent vs $H_1$: they are associated. Use $\\alpha=0.1$, reporting the realisation of the test statistic, its p-value and indicating the threshold of the rejection region.

---

**Answer.** The chi-squared statistic equals **115.69** with p-value $\\approx 0$, so $H_0$ is rejected. The rejection-region threshold at level $\\alpha=0.1$ is the order-0.9 quantile of a $\\chi^2$ with $\\text{df}=(5-1)\\cdot(5-1)=16$, equal to **23.54**.

```r
chisq.test(Developers_ITA$Age_Class, Developers_ITA$LearnTool)
qchisq(0.9, df=16)   # rejection-region threshold = 23.54
```
""", "images": []}

ex7["7_8a"] = {"title": "Ex 7.8a — Subgroup t-test: AmountSpent by Location (Female), one-sided less",
"content": """**Question.** A geo-localized marketing strategy aimed at increasing the average amount spent by women who live close to a competing store is worth implementing **only if** these women currently spend on average **less** than those who live far from a competing store. Test $H_0:\\mu_{\\text{F,Close}} = \\mu_{\\text{F,Far}}$ vs $H_1:\\mu_{\\text{F,Close}} < \\mu_{\\text{F,Far}}$ at $\\alpha=0.05$.

---

**Answer.** With $\\bar x_{\\text{F,Close}}=1051.91$ and $\\bar x_{\\text{F,Far}}=1418.66$, the standardized test statistic is **-5.25** and the p-value is below any conventional level regardless of the equality-of-variances assumption (under unequal variances the statistic is -5.1, still highly significant). Reject $H_0$: there is evidence that women living near a competing store spend less, so the geo-localized strategy is appropriate.

```r
sel.Close <- DS$Sex=="Female" & DS$Location=="Close"
sel.Far   <- DS$Sex=="Female" & DS$Location=="Far"
TEST.diffmean(x=DS$AmountSpent[sel.Close],
              y=DS$AmountSpent[sel.Far],
              alternative="less", var.test=TRUE)
```
""", "images": []}

ex7["7_9a"] = {"title": "Ex 7.9a — Chi-squared goodness of fit: DS$Children vs Italian distribution",
"content": """**Question.** Test whether the distribution of the number of children of `DS` customers mirrors that of the Italian population: 76% with no children, 13% with 1 child, 9% with 2 children, 2% with 3+ children.

---

**Answer.** Sample distribution: 0→360 (0.48), 1→184 (0.25), 2→111 (0.15), 3→95 (0.13). The chi-squared goodness-of-fit statistic equals **608.81** on $\\text{df}=3$, with an extremely small p-value $\\approx 0$. **Reject $H_0$**: the sample distribution does not match the Italian one.

```r
distr.table.x(DS$Children)
chisq.test(c(360, 184, 111, 95), p=c(0.76, 0.13, 0.09, 0.02))
```
""", "images": []}

ex7["7_9b"] = {"title": "Ex 7.9b — Chi-squared goodness of fit: DS$Age vs Italian age bands",
"content": """**Question.** Test whether the distribution of customers across age groups in `DS` matches the Italian population on the defined bands: 30% Young, 50% Middle, 20% Senior.

---

**Answer.** Sample composition: Young→216 (0.29), Middle→390 (0.52), Senior→144 (0.19). The chi-squared statistic equals **0.5488** on $\\text{df}=2$, giving a very high p-value. **Do not reject $H_0$**: the distribution of customers by age group reflects that of the Italian population.

```r
distr.table.x(DS$Age)
chisq.test(c(216, 390, 144), p=c(0.3, 0.5, 0.2))
```
""", "images": []}

ex7["7_10a"] = {"title": "Ex 7.10a — Two-sample test on pooled summary stats: considered vs competing company",
"content": """**Question.** A survey on 800 customers of a competing company gives mean expenditure $\\bar y=1300$ and $s_y=960$. The considered company (the 750 women in `DS`) has $\\bar x=1228.44$ and $s_x^2=940900.9$. Test at $\\alpha=10\\%$ whether the considered-company average expenditure is significantly **higher** than the competitor's: $H_0:\\mu_x = \\mu_y$ vs $H_1:\\mu_x > \\mu_y$.

---

**Answer.** Pooled variance $s_p^2 = \\dfrac{(n_x-1)s_x^2 + (n_y-1)s_y^2}{n_x+n_y-2}=930938.7$. The standardized statistic equals $|\\bar x-\\bar y|/\\sqrt{s_p^2/n_x + s_p^2/n_y} = 1.4592$. Using the normal approximation, the p-value is $\\Pr(Z\\ge 1.4592)\\approx 0.072$. At $\\alpha=10\\%$ **do not reject $H_0$**: the average expenditure of the considered company is **not** significantly higher than that of the competing company.

```r
xbar <- 1228.44; s2.x <- 940900.9; n.x <- 750
ybar <- 1300;    s2.y <- 960^2;    n.y <- 800
s2.pool <- ((n.x-1)*s2.x + (n.y-1)*s2.y) / (n.x + n.y - 2)   # 930938.7
t.stat  <- abs(xbar - ybar) / sqrt(s2.pool/n.x + s2.pool/n.y); t.stat  # 1.4592
1 - pnorm(t.stat)                                            # one-sided p-value ~ 0.072
```
""", "images": []}
