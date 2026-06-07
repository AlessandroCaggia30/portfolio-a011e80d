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

ex7["7_5a"] = {"title": "Ex 7.5a — Fish-diet vs cholesterol: rejection region (normal & t) and p-value",
"content": """**Question.** Two independent samples of 100 males each, with similar baseline cholesterol, are assigned for 6 months to a **Standard** diet vs a **Seafood** diet (fish-rich). End-of-study cholesterol summaries:

| Diet | Mean | Variance |
|---|---|---|
| Standard | $\\bar X_\\text{Std}=210.1$ | $s^2_\\text{Std}=37.4$ |
| Seafood  | $\\bar X_\\text{Sea}=196.8$ | $s^2_\\text{Sea}=33.5$ |

Assuming the variance of cholesterol is the same regardless of diet, the researchers claim: "a fish-rich diet promotes a reduction in cholesterol; in particular, the mean difference $\\mu_\\text{Std}-\\mu_\\text{Sea}$ is significantly **greater than 10**" at $\\alpha=0.05$. Evaluate the claim by determining **both the rejection region and the p-value** of the sample realisation (report the R functions used).

---

**Answer.** Hypotheses (most serious error: declaring the gap > 10 when it is not, so put the strict inequality in $H_1$):
$$H_0:\\mu_\\text{Std}-\\mu_\\text{Sea} \\le 10 \\quad \\text{vs} \\quad H_1:\\mu_\\text{Std}-\\mu_\\text{Sea} > 10.$$

**Pooled variance** (equal-variances assumption, $n_1=n_2=100$):
$$s^2_\\text{pool} = \\frac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1+n_2-2} = \\frac{99\\cdot 37.4 + 99\\cdot 33.5}{198} = 35.45.$$
Standard error of the difference under $H_0$: $se = \\sqrt{2\\,s^2_\\text{pool}/n} = \\sqrt{0.7090} = 0.8420$.

**Rejection region — normal approximation.** The sample size is large, so by the CLT the standardised difference is approximately $\\mathcal N(0,1)$ under $H_0$. Reject $H_0$ at $\\alpha=0.05$ if
$$\\bar X_\\text{Std}-\\bar X_\\text{Sea} > 10 + z_{0.05}\\cdot se = 10 + 1.6449\\cdot 0.8420 = 11.385.$$

**Rejection region — Student's t.** Under equal variances and (approximately) normal populations, $T = \\big((\\bar X_\\text{Std}-\\bar X_\\text{Sea}) - 10\\big)/se \\sim t_{n_1+n_2-2}=t_{198}$. The 0.05-level critical value $t_{198,\\,0.05}\\approx 1.6526$ gives threshold $10 + 1.6526\\cdot 0.8420 \\approx 11.39$ — **almost identical to the normal case** because with df $=198$ Student's t and the standard normal coincide to four decimals.

**Decision.** Observed difference $= 210.1 - 196.8 = 13.3 > 11.385$ (and $> 11.39$) → **reject $H_0$**. The realised $t$-statistic is $t_\\text{obs} = (13.3-10)/0.8420 = 3.92$, deep in the right tail.

**p-value of the sample realisation.** The p-value is the $H_0$-probability of drawing a difference at least as extreme as $13.3$:
$$\\text{p-value} = \\Pr(\\bar X_\\text{Std}-\\bar X_\\text{Sea} > 13.3 \\mid \\mu_\\text{Std}-\\mu_\\text{Sea}=10).$$

- Normal approximation: $1-\\Phi(3.92) \\approx 4.4\\cdot 10^{-5}$ — R: `1-pnorm(13.3, mean=10, sd=sqrt(2*35.45/100))` or equivalently `1-pnorm((13.3-10)/sqrt(2*35.45/100))`.
- Student's t (df=198): $\\approx 6.1\\cdot 10^{-5}$ — R: `1-pt((13.3-10)/sqrt(2*35.45/100), df=198)`.

Both are far below any conventional $\\alpha$ (0.10, 0.05, 0.01, 0.001), **confirming the rejection** reached via the rejection-region approach: the data strongly support the researchers' claim that a Seafood diet lowers mean cholesterol by **more than 10** units.

```r
xbar.s <- 210.1; var.s <- 37.4; n.s <- 100
xbar.f <- 196.8; var.f <- 33.5; n.f <- 100
s2.pool <- ((n.s-1)*var.s + (n.f-1)*var.f)/(n.s + n.f - 2)    # 35.45
se      <- sqrt(2*s2.pool/n.s)                                # 0.8420
# Critical values and rejection thresholds
qnorm(0.95)                                                   # 1.6449
qt(0.95, df=198)                                              # 1.6526
10 + qnorm(0.95)*se                                           # 11.385  (normal)
10 + qt(0.95, df=198)*se                                      # 11.392  (t)
(xbar.s - xbar.f)                                             # 13.3
# p-values
1 - pnorm(13.3, mean=10, sd=se)                               # 4.4e-5  (normal)
1 - pnorm((13.3 - 10)/se)                                     # same
1 - pt((13.3 - 10)/se, df=198)                                # 6.1e-5  (t)
# Built-in equivalent (same t, df, threshold, p-value):
# TEST.diffmean(..., alternative="greater", mu0=10, var.test=TRUE)
```
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

ex7["7_7a"] = {"title": "Ex 7.7a — Two-proportion z-test: AI-tool use by Younger vs Senior (Developers_ITA)",
"content": """**Question.** We focus on whether the developers use AI tools, such as ChatGPT or others, in their work. Is it reasonable to conclude that the proportion of **younger** developers (`Younger=TRUE`) using such tools is **higher** than the proportion of more **senior** developers (`Younger=FALSE`)? State the hypotheses, discuss the assumptions needed, and report your conclusions.

---

**Answer.**

**Hypotheses.** Let $p_\\text{Young}$, $p_\\text{Senior}$ be the population proportions of younger / senior developers who use AI tools at work. The claim "younger use them *more*" is the research hypothesis we want empirical support for, so it goes in $H_1$ (one-sided):
$$H_0:\\, p_\\text{Young} = p_\\text{Senior} \\quad \\text{vs} \\quad H_1:\\, p_\\text{Young} > p_\\text{Senior}.$$

**Assumptions.** (i) The two sub-samples are **independent** (Younger=TRUE vs FALSE are disjoint sub-populations of `Developers_ITA`). (ii) Each sub-sample is large enough that, by the **CLT**, the sampling distribution of $\\hat p_\\text{Young} - \\hat p_\\text{Senior}$ is approximately normal; no distributional assumption on `ChatGPT` is needed. (iii) Under $H_0$ the two proportions share a common value $p$, estimated by the **pooled** proportion below.

**Test statistic & realisation.** Sample proportions $\\hat p_\\text{Young} \\approx 0.57$, $\\hat p_\\text{Senior} \\approx 0.40$, observed difference $\\hat p_\\text{Young} - \\hat p_\\text{Senior} \\approx 0.17$. Pooled proportion and standard error under $H_0$:
$$\\hat p = \\frac{n_\\text{Y}\\hat p_\\text{Y} + n_\\text{S}\\hat p_\\text{S}}{n_\\text{Y}+n_\\text{S}}, \\qquad se_0 = \\sqrt{\\hat p(1-\\hat p)\\!\\left(\\tfrac{1}{n_\\text{Y}}+\\tfrac{1}{n_\\text{S}}\\right)}.$$
Standardised z-statistic under $H_0$:
$$z_\\text{obs} = \\frac{\\hat p_\\text{Young} - \\hat p_\\text{Senior}}{se_0} \\;\\approx\\; \\mathbf{4.77}, \\qquad \\text{p-value} = P(Z > 4.77) < 0.0001.$$

**Decision.** Reject $H_0$ at any conventional significance level (0.10, 0.05, 0.01, 0.001 — and well beyond). There is overwhelming evidence that the proportion of younger developers using AI tools such as ChatGPT in their work is **significantly higher** than that of more senior developers.

```r
# Built-in wrapper (one-sided, success="Yes"):
TEST.diffprop(x=Developers_ITA$ChatGPT[Developers_ITA$Younger==TRUE],
              y=Developers_ITA$ChatGPT[Developers_ITA$Younger==FALSE],
              success.x="Yes", pdiff=0, alternative="greater", digits=4)

# Manual computation (matches the wrapper):
phat_y <- mean(Developers_ITA$ChatGPT[Developers_ITA$Younger==TRUE]  == "Yes")   # ~ 0.57
phat_s <- mean(Developers_ITA$ChatGPT[Developers_ITA$Younger==FALSE] == "Yes")   # ~ 0.40
n_y    <- sum(Developers_ITA$Younger==TRUE)
n_s    <- sum(Developers_ITA$Younger==FALSE)
phat0  <- (n_y*phat_y + n_s*phat_s)/(n_y + n_s)                                  # pooled p
se0    <- sqrt(phat0*(1-phat0)*(1/n_y + 1/n_s))                                  # SE under H0
z      <- (phat_y - phat_s)/se0; z                                               # ~ 4.77
1 - pnorm(z)                                                                     # p-value < 1e-4
```
""", "images": [
    "statistics/images/ex7/ex7_7_question.png",
    "statistics/images/ex7/ex7_7a_answer.png",
]}

ex7["7_7b"] = {"title": "Ex 7.7b — Chi-squared independence: Age_Class × LearnTool (Developers_ITA)",
"content": """**Question.** Can we conclude that there is **association** between developers' age (`Age_Class`, 5 levels) and the tools used to improve coding skills (`LearnTool`, 5 levels)? If yes, answer using a test at the **0.1 level**, reporting the realisation of the test statistic, its p-value, and the threshold of the rejection region (report the R functions used).

---

**Answer.**

**Assumptions.** This is a **chi-squared test of independence** on the $5\\times 5$ contingency table $\\texttt{Age\\_Class}\\times\\texttt{LearnTool}$. The approximation to $\\chi^2$ requires the **expected counts** $\\widehat{E}_{ij} = n_{i\\cdot}\\,n_{\\cdot j}/n$ to be **$\\geq 5$** in (almost) every cell — a condition that holds here (unlike point c, where `AISearch=Other` has expected count $\\approx 2.77$ and the test cannot be used).

**Hypotheses.**
$$H_0: \\text{Age\\_Class}\\perp\\!\\!\\!\\perp \\text{LearnTool}\\quad\\text{vs}\\quad H_1: \\text{they are associated.}$$

**Test statistic.** With $r=c=5$ levels, under $H_0$
$$X^2 = \\sum_{i=1}^{r}\\sum_{j=1}^{c}\\frac{(O_{ij}-\\widehat{E}_{ij})^2}{\\widehat{E}_{ij}} \\;\\;\\dot\\sim\\;\\; \\chi^2_{(r-1)(c-1)} = \\chi^2_{16}.$$
Its **realisation** is $X^2_\\text{obs} = \\mathbf{115.69}$.

**p-value & rejection region.**
$$p = \\Pr\\!\\big(\\chi^2_{16} \\geq 115.69\\big) \\;\\approx\\; 0.$$
At $\\alpha=0.1$ the rejection region is $X^2 > \\chi^2_{16,\\,0.9}$. The threshold is the **0.9-quantile** of a $\\chi^2_{16}$:
$$\\chi^2_{16,\\,0.9} \\;=\\; \\texttt{qchisq(0.9, df=16)} \\;=\\; \\mathbf{23.54}.$$

**Decision.** Since $X^2_\\text{obs} = 115.69 \\gg 23.54$ (equivalently $p\\approx 0 < 0.1$), **reject $H_0$**: there is overwhelming evidence of **association** between `Age_Class` and `LearnTool` — the tools developers use to upskill depend on their age group, and this conclusion holds at **any conventional significance level**.

```r
# Built-in chi-square test of independence (Pearson)
chisq.test(Developers_ITA$Age_Class, Developers_ITA$LearnTool)
# X-squared = 115.69, df = 16, p-value < 2.2e-16

# Rejection-region threshold at alpha = 0.1
qchisq(0.9, df=16)   # 23.54

# Sanity check: same p-value computed by hand from the statistic
1 - pchisq(115.69, df=16)   # ~ 0
```
""", "images": [
    "statistics/images/ex7/ex7_7_question.png",
    "statistics/images/ex7/ex7_7a_answer.png",
]}

ex7["7_8a"] = {"title": "Ex 7.8a — One-sample t-test on Children (H1: μ<1.5): give up baby products?",
"content": """**Question.** The sales manager considers **giving up the sale of baby products** if the average number of children per customer (`DS$Children`) does **not exceed 1.5**. Specify the assumptions, indicate the statistical test, report the realisation of the test statistic and its p-value, and define / interpret the p-value on the available data.

---

**Answer.**

**Assumptions.** With $n=750$ the **CLT** applies, so $\\bar X$ is approximately normal and no distributional assumption on `Children` is required. $\\sigma^2$ is unknown and is replaced by $S^2$.

**Hypotheses.** The most serious error is **giving up baby products when in fact the average is still ≥ 1.5** (Type I error). Put the "stop selling" claim in $H_1$:
$$H_0:\\mu \\geq 1.5 \\quad \\text{vs} \\quad H_1:\\mu < 1.5$$

**Test statistic.** Sample mean $\\bar x = 0.92$ (well below 1.5). The standardized one-sample **t-statistic** is
$$t_\\text{obs} = \\frac{\\bar x - 1.5}{S/\\sqrt{n}} = -14.91 \\;\\;\\sim\\; t_{749} \\text{ under } H_0$$

**p-value.** $p = P(t_{749} \\leq -14.91) < 0.0001$ (Student t, or equivalently the normal approximation given the large $n$). The p-value is the probability — **under $H_0$** — of observing a test statistic at least as extreme (in the direction of $H_1$) as the one realised. Here it is essentially zero.

**Decision & interpretation.** Reject $H_0$ at any conventional $\\alpha$ (0.10, 0.05, 0.01, 0.001). There is overwhelming evidence that the **average number of children per customer is significantly less than 1.5**, so it is **convenient to give up the sale of baby products**.

---

**AI walkthrough.** The empirical distribution behind $\\bar x \\approx 0.92$ is the one given in Ex 7.9a: $360$ customers with 0 children, $184$ with 1, $111$ with 2, $95$ with 3+. Hence
$$\\bar x = \\frac{0\\cdot 360 + 1\\cdot 184 + 2\\cdot 111 + 3\\cdot 95}{750} = \\frac{691}{750} \\approx 0.9213.$$
The sample variance (treating 3+ as 3) is
$$s^2 = \\frac{1}{n-1}\\sum_i (x_i-\\bar x)^2 \\approx 1.132 \\Rightarrow s \\approx 1.0640,\\quad s/\\sqrt{n} \\approx 0.03885.$$
This gives the standardised realisation $t_\\text{obs} = (0.9213 - 1.5)/0.03885 \\approx -14.90$, matching the value reported above.

*Rejection-region cross-check.* At $\\alpha=0.05$, $t_{749,\\,0.05} \\approx -1.6473$, so the lower-tail rejection region for $\\bar X$ is
$$\\bar x < 1.5 + (-1.6473)\\cdot 0.03885 \\approx 1.436.$$
The observed $\\bar x = 0.921$ is *deep* below 1.436 — equivalent to saying $t_\\text{obs}$ sits in the extreme left tail, far past the critical value.

*Tie to the CI.* A two-sided 95% CI for $\\mu$ is $\\bar x \\pm 1.96\\cdot s/\\sqrt n \\approx [0.845,\\,0.997]$, which **does not contain** $1.5$ — the same conclusion the one-sided test reaches: $\\mu$ is, with overwhelming evidence, well under 1.5.

*Type I vs Type II framing.* Because the directional claim ("stop selling") sits in $H_1$, rejecting $H_0$ commits to giving up baby products. $\\alpha = 0.05$ caps at 5% the chance of dropping a still-profitable product line; the realised p-value $\\approx 0$ means we make this commitment with essentially no doubt.

```r
# Sample statistics
xbar <- mean(DS$Children); s <- sd(DS$Children); n <- length(DS$Children)
xbar                                                # ≈ 0.92
tstat <- (xbar - 1.5) / (s/sqrt(n)); tstat          # ≈ -14.91
pt(tstat, df=n-1)                                   # Student-t p-value (~ 0)
pnorm(tstat)                                        # normal approximation (~ 0)

# Rejection-region cross-check at alpha = 0.05
qt(0.05, df=n-1)                                    # ≈ -1.6473
1.5 + qt(0.05, df=n-1)*(s/sqrt(n))                  # RR upper edge for xbar ≈ 1.436

# Two-sided 95% CI for mu (must miss 1.5)
CI.mean(Children, conf.level=0.95, data=DS)         # ≈ [0.845, 0.997]

# Built-in wrapper
TEST.mean(Children, mu0=1.5, alternative="less", data=DS)
```
""", "images": []}

ex7["7_9a"] = {"title": "Ex 7.9a — Chi-squared goodness of fit: DS$Children vs Italian distribution",
"content": """**Question (Ex 7.17a, `DS`).** What is the distribution of the number of children observed at the sample level? Is it possible to conclude that, at the population level, the distribution of customers by number of children mirrors that observed for the Italian population (**72% households without children**, 13% with 1 child, 9% with 2 children, 2% with 3+ children)?

> *Textbook typo*: 0.72+0.13+0.09+0.02 = 0.96 ≠ 1. The intended value (used in the official solution and the R call below) is **0.76** for "no children", so the population vector is $p_0 = (0.76, 0.13, 0.09, 0.02)$.

---

**Answer.** Categorical comparison of one observed multinomial against a fully-specified reference $\\Rightarrow$ **chi-squared goodness-of-fit test**.

$$H_0: p = p_0 \\quad \\text{vs} \\quad H_1: p \\neq p_0, \\qquad p_0 = (0.76,\\,0.13,\\,0.09,\\,0.02)$$

*Sample distribution* ($n=750$):

| Children | 0 | 1 | 2 | 3+ |
|---|---|---|---|---|
| Observed $O_k$ | 360 | 184 | 111 | 95 |
| Relative freq | 0.48 | 0.25 | 0.15 | 0.13 |
| Expected $E_k = n p_{0k}$ | 570 | 97.5 | 67.5 | 15 |

All $E_k \\geq 5$, so the asymptotic $\\chi^2$ approximation is valid.

*Test statistic:*
$$X^2 = \\sum_{k} \\frac{(O_k - E_k)^2}{E_k} = \\frac{(360-570)^2}{570} + \\frac{(184-97.5)^2}{97.5} + \\frac{(111-67.5)^2}{67.5} + \\frac{(95-15)^2}{15}$$
$$\\phantom{X^2} \\approx 77.37 + 76.74 + 28.03 + 426.67 = 608.81$$

Under $H_0$, $X^2 \\sim \\chi^2_{K-1} = \\chi^2_3$. p-value $= P(\\chi^2_3 > 608.81) \\approx 0$ (the 0.05 critical value is only 7.81). **Reject $H_0$** at any reasonable $\\alpha$: the distribution of children among `DS` customers does **not** mirror the Italian one.

*Where does the discrepancy come from?* The category "3+ children" contributes 426.67 of the 608.81 total — `DS` has 95 such households versus only 15 expected. Customers with many children are massively over-represented; childless households are under-represented (360 vs 570).

```r
# Sample distribution
distr.table.x(DS$Children)
# DS$Children Count Prop
#   0           360  0.48
#   1           184  0.25
#   2           111  0.15
#   3            95  0.13
# TOTAL         750  1.00

# Goodness-of-fit test
chisq.test(c(360, 184, 111, 95), p=c(0.76, 0.13, 0.09, 0.02))
## X-squared = 608.81, df = 3, p-value < 2.2e-16

# Manual cross-check
O  <- c(360, 184, 111, 95); p0 <- c(0.76, 0.13, 0.09, 0.02); n <- sum(O)
E  <- n * p0; E                                     # 570.0 97.5 67.5 15.0
X2 <- sum((O - E)^2 / E); X2                        # 608.81
1 - pchisq(X2, df=length(O)-1)                      # p-value ~ 0
qchisq(0.95, df=3)                                  # critical value 7.815
```
""", "images": []}

ex7["7_9b"] = {"title": "Ex 7.9b — Chi-squared goodness-of-fit in R: are the 4 supermarket entrances used equally?",
"content": """**Question.** Continuing Ex 7.9: a manager surveyed the $n=130$ customers entering a supermarket on a working day and recorded which of the **four entrances** they used:

| Entrance | I | II | III | IV |
|---|---|---|---|---|
| Observed $O_k$ | 24 | 30 | 36 | 40 |

Draw a conclusion about the hypotheses specified in part a) at the **5% significance level**, using an appropriate R function and reporting the syntax used.

---

**Answer.** The hypotheses set in a) test whether the four entrances are used with the **same frequency**:
$$H_0:\\ p_1=p_2=p_3=p_4=\\tfrac14 \\quad \\text{vs} \\quad H_1:\\ \\exists\\,k:\\ p_k\\neq\\tfrac14$$

This is a one-sample multinomial vs a **fully specified** reference $\\Rightarrow$ chi-squared **goodness-of-fit** test. Under $H_0$ the expected counts are $E_k = n\\,p_{0k} = 130/4 = 32.5$ for every cell; all $E_k\\geq 5$, so the asymptotic $\\chi^2_{K-1}=\\chi^2_3$ approximation is valid.

*Realisation of the test statistic.*
$$X^2 = \\sum_{k=1}^{4}\\frac{(O_k-E_k)^2}{E_k} = \\frac{(24-32.5)^2 + (30-32.5)^2 + (36-32.5)^2 + (40-32.5)^2}{32.5} = \\frac{147}{32.5} \\approx 4.5231$$

*Decision rule (α = 0.05).* Critical value $\\chi^2_{0.95,\\,3} = 7.815$. Since $X^2 = 4.5231 < 7.815$, equivalently the p-value
$$\\text{p-value} = P(\\chi^2_3 > 4.5231) \\approx 0.2104 \\;>\\; 0.05,$$
we **do not reject $H_0$** at the 5% level: the data are compatible with the four entrances being used with equal frequency.

*Sanity check.* The biggest residual contribution comes from entrance IV ($(40-32.5)^2/32.5 \\approx 1.73$); spread across 3 d.f. this is well within ordinary sampling variation when each cell expects ~32.5 visits.

```r
# Goodness-of-fit test, fully specified p0 = (1/4, 1/4, 1/4, 1/4)
chisq.test(x = c(24, 30, 36, 40), p = c(0.25, 0.25, 0.25, 0.25))
## X-squared = 4.5231, df = 3, p-value = 0.2104

# Manual cross-check
O  <- c(24, 30, 36, 40); p0 <- rep(1/4, 4); n <- sum(O)   # n = 130
E  <- n * p0; E                                            # 32.5 32.5 32.5 32.5
X2 <- sum((O - E)^2 / E); X2                               # 4.5231
1 - pchisq(X2, df = length(O) - 1)                         # p-value ≈ 0.2104
qchisq(0.95, df = 3)                                       # critical value 7.8147
```
""", "images": []}

ex7["7_10a"] = {"title": "Ex 7.10a — Two-sample test on pooled summary stats: considered vs competing company",
"content": """**Question.** A survey on $n_y = 800$ customers of a competing company gives mean expenditure $\\bar y=1300$ and standard deviation $s_y=960$. The considered company (the $n_x = 750$ women in `DS`) has $\\bar x=1228.44$ and $s_x^2=940{,}900.9$. Test at $\\alpha=10\\%$ whether the considered-company average expenditure is significantly **higher** than the competitor's: $H_0:\\mu_x = \\mu_y$ vs $H_1:\\mu_x > \\mu_y$.

---

**Answer.**

*Setup.* Two **independent** samples, both very large ($n_x, n_y \\geq 30$), so by the CLT the sample means are approximately normal without needing normality of the underlying expenditure variable. Population variances are unknown; assuming they are reasonably close, use the **pooled-variance two-sample t-test** (with such large samples the t and Z critical values coincide).

*Pooled variance.* Note $s_y^2 = 960^2 = 921{,}600$.
$$s_p^2 = \\frac{(n_x-1)\\,s_x^2 + (n_y-1)\\,s_y^2}{n_x+n_y-2} = \\frac{749 \\cdot 940{,}900.9 + 799 \\cdot 921{,}600}{1548} = 930{,}938.7$$

*Standard error of the difference.*
$$\\text{SE}(\\bar x - \\bar y) = \\sqrt{s_p^2\\left(\\tfrac{1}{n_x}+\\tfrac{1}{n_y}\\right)} = \\sqrt{930{,}938.7\\,(1/750 + 1/800)} = \\sqrt{2402.92} \\approx 49.02$$

*Test statistic (one-sided, upper tail).*
$$T_\\text{obs} = \\frac{\\bar x - \\bar y}{\\text{SE}} = \\frac{1228.44 - 1300}{49.02} = \\frac{-71.56}{49.02} \\approx -1.4592$$

*Decision (α = 0.10).* Critical value $z_{0.90} = 1.2816$ (or equivalently $t_{0.90,\\,1548}\\approx 1.2824$). Since $T_\\text{obs} = -1.4592 < 1.2816$, we **do not reject $H_0$**. Equivalently, the one-sided p-value is
$$\\text{p-value} = \\Pr(Z \\geq -1.4592) = 1 - \\Phi(-1.4592) \\approx 0.9277 \\gg 0.10.$$

*Interpretation.* The considered company's mean ($1228.44$) is actually **lower** than the competitor's ($1300$), so the data move in the opposite direction of $H_1$. There is no evidence that the considered company spends more per customer; if anything, the reverse appears mildly suggestive (the two-sided p-value would be $\\approx 0.145$, still not significant at 10%).

```r
# Summary-stats two-sample test (pooled variance), H1: mu_x > mu_y
xbar <- 1228.44; s2.x <- 940900.9; n.x <- 750
ybar <- 1300;    s2.y <- 960^2;    n.y <- 800              # s2.y = 921600

s2.pool <- ((n.x-1)*s2.x + (n.y-1)*s2.y) / (n.x + n.y - 2) # 930938.7
se.diff <- sqrt(s2.pool * (1/n.x + 1/n.y));      se.diff   # 49.02
t.stat  <- (xbar - ybar) / se.diff;              t.stat    # -1.4592

# One-sided (upper) p-value: Pr(T >= t.stat)
1 - pnorm(t.stat)                                          # ~ 0.9277  -> do NOT reject
1 - pt(t.stat, df = n.x + n.y - 2)                         # ~ 0.9277

qnorm(0.90)                                                # 1.2816 (critical value)
```
""", "images": []}
