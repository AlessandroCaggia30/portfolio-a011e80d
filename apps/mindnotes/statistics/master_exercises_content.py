# =====================================================================
# Master Exam Ready exercises — ONE consolidated exercise per subtopic.
# Each master exercise uses a single dataset and covers all unique
# subparts asked across the linked snippets, eliminating redundancy.
# Populated by agents (one per subtopic).
# =====================================================================

master_exercises = {}


master_exercises["g13a_ci_one_mean"] = {
    "title": "Master Exam — CI for one mean (consolidated)",
    "content": r"""**Setup.** A market-research firm collected the monthly turnover `Sales` (in €) for a random sample of $n=100$ pizzerias in Milan. The sample summaries are
$$\bar x \;=\; 23\,947, \qquad s \;=\; 8\,200, \qquad n \;=\; 100.$$
For most of this exercise the population variance $\sigma^2$ is **unknown** and is estimated from the data; in part **(c)** we contrast this with the textbook case in which $\sigma = 8\,000$ is assumed **known**. Let $\mu$ denote the population mean monthly turnover.

---

**(a) Point estimate of $\mu$ and unbiasedness.** Propose an unbiased estimator of $\mu$, justify why it is unbiased, and compute the point estimate from the sample.

The sample mean
$$\bar X \;=\; \frac{1}{n}\sum_{i=1}^{n} X_i$$
is unbiased for $\mu$ because, by linearity of expectation, $\mathbb E[\bar X] = \tfrac{1}{n}\sum_i \mathbb E[X_i] = \mu$. This requires only i.i.d. sampling — **no distributional assumption** on $X$ is needed for unbiasedness. The point estimate is $\hat\mu = \bar x = 23\,947$ €.

```r
n    <- 100
xbar <- 23947
s    <- 8200
xbar                                # 23947  -> point estimate of mu
```

*Interpretation.* $\bar x = 23\,947$ is a single realised draw from the sampling distribution of $\bar X$, which is *centred* on the unknown $\mu$. Unbiasedness says nothing about how close *this* particular $\bar x$ is to $\mu$ — that needs an SE and a CI (parts (b)–(d)).

---

**(b) Estimated standard error with unknown $\sigma$.** Since $\sigma$ is unknown we cannot use $SE(\bar X) = \sigma/\sqrt n$ exactly; we plug in the sample SD:
$$\widehat{SE}(\bar X) \;=\; \frac{s}{\sqrt n} \;=\; \frac{8\,200}{\sqrt{100}} \;=\; 820.$$

```r
se_hat <- s / sqrt(n);  se_hat        # 820
```

*Recovering $s$ from raw sums.* When the dataset is summarised by $\sum x_i$ and $\sum x_i^2$ only (no $s$ printed), use the computational form
$$s^2 \;=\; \frac{1}{n-1}\!\left(\sum_{i=1}^n x_i^2 \;-\; n\,\bar x^{\,2}\right),$$
then $\widehat{SE} = s/\sqrt n$.

```r
# If only raw sums are given (illustrative numbers, n=15):
# sum_x <- 2755; sum_x2 <- 585203
# xbar  <- sum_x/15;  s2 <- (sum_x2 - 15*xbar^2)/14
```

*What the SE does and does not tell us.* $\widehat{SE}$ measures the **typical** sampling variability of $\bar X$ across hypothetical resamples of size $n=100$ — a property of the *estimator*. It does **not** tell us how far the realised $\bar x = 23\,947$ is from $\mu$ for *this* sample; that distance $|\bar x - \mu|$ depends on the unknown $\mu$. To bound it probabilistically we need a CI.

---

**(c) Exact SE with known $\sigma$.** If instead the population SD were *known*, $\sigma = 8\,000$, then
$$SE(\bar X) \;=\; \frac{\sigma}{\sqrt n} \;=\; \frac{8\,000}{\sqrt{100}} \;=\; 800$$
— **exact**, no plug-in needed; inference uses $z$-quantiles. With $\sigma$ unknown (the realistic case) we use $t_{n-1}$-quantiles to compensate for the extra noise in $s^2$.

```r
sigma <- 8000
SE_exact <- sigma / sqrt(n);  SE_exact   # 800  (only if sigma is KNOWN)
```

---

**(d) 95% confidence interval for $\mu$.** Because $n=100$ is large, the CLT gives $\bar X \overset{a}{\sim} \mathcal N(\mu,\sigma^2/n)$ regardless of the shape of $X$; with $\sigma$ unknown, $(\bar X - \mu)/(s/\sqrt n) \sim t_{n-1}$. The two-sided CI is
$$\bar x \;\pm\; t_{1-\alpha/2,\,n-1}\,\frac{s}{\sqrt n}.$$
With $\alpha=0.05$ and $n-1=99$, $t_{0.975,\,99} \approx 1.984$ (and $z_{0.975} = 1.960$ — essentially the same at this $n$). Then
$$ME_{95} \;=\; 1.984 \cdot 820 \;\approx\; 1\,627, \qquad CI_{95} \;=\; 23\,947 \pm 1\,627 \;=\; [22\,320,\;25\,574].$$

```r
alpha <- 0.05
tcrit <- qt(1 - alpha/2, df = n-1);  tcrit          # 1.984
ME_95 <- tcrit * se_hat;             ME_95          # 1627
c(xbar - ME_95, xbar + ME_95)                        # [22320, 25574]
```

*Interpretation of the 95%.* The 95% is a property of the **procedure**, not of this particular interval: if we drew many independent samples and built a CI each time, about 95% of those intervals would cover $\mu$. For our one realised interval we cannot say "there is a 95% probability that $\mu$ lies inside" — $\mu$ is fixed; either the interval covers it or it does not.

![Master illustration](statistics/images/master/master_g13a_ai.png)

---

**(e) Effect of a higher confidence level (99% CI).** The SE depends only on the sample, *not* on the confidence level. Raising the level inflates only the reliability factor: with $n-1=99$, $t_{0.995,\,99} \approx 2.626$ (and $z_{0.995}=2.576$ — again essentially equal). Then
$$ME_{99} \;=\; 2.626 \cdot 820 \;\approx\; 2\,153, \qquad CI_{99} \;\approx\; [21\,794,\;26\,100],$$
about **32% wider** than the 95% interval (ratio $t_{0.995,\,99}/t_{0.975,\,99} \approx 2.626/1.984 \approx 1.32$). *Rule:* higher confidence ⇒ wider interval.

```r
ME_99 <- qt(0.995, df = n-1) * se_hat;  ME_99      # ~2153
c(xbar - ME_99, xbar + ME_99)                       # ~[21794, 26100]
qt(0.995, df=n-1) / qt(0.975, df=n-1)               # ~1.323 inflation
```

---

**(f) Margin-of-error decomposition.** The ME factors cleanly into
$$ME \;=\; \underbrace{c}_{\text{reliability}} \;\cdot\; \underbrace{s/\sqrt n}_{\text{SE}}.$$
From a printed CI one can always read off
$$\bar x \;=\; \tfrac{L+U}{2}, \qquad ME \;=\; \tfrac{U-L}{2}.$$
For our 95% CI $[22\,320,\,25\,574]$: midpoint $=23\,947$ ✓, half-width $=1\,627$ ✓ — no raw data needed.

```r
L <- 22320; U <- 25574
(L + U)/2                       # 23947 = xbar
(U - L)/2                       # 1627  = ME_95
```

---

**(g) Effect of the sample size on the SE.** $SE \propto 1/\sqrt n$. Compare two sample sizes with the same $s$:
$$SE(n=15) \;=\; \frac{8\,200}{\sqrt{15}} \;\approx\; 2\,117, \qquad SE(n=50) \;=\; \frac{8\,200}{\sqrt{50}} \;\approx\; 1\,160.$$
Going from $n=15$ to $n=50$ shrinks the SE by $\sqrt{15/50} \approx 0.548$ — almost in half. With $n=100$ the SE drops to $820$, sharper still.

```r
8200/sqrt(15)                    # 2117  -- SE at n=15
8200/sqrt(50)                    # 1160  -- SE at n=50
sqrt(15/50)                      # 0.548 -- shrinkage factor 15 -> 50
```

---

**(h) "Closeness" claim — true or false?** *Claim:* "The estimate $\bar x_{100} = 23\,947$ from $n=100$ is closer to $\mu$ than the estimate $\bar x_{15}$ would be."

**FALSE.** A larger $n$ gives a smaller $SE(\bar X) = \sigma/\sqrt n$, so the *sampling distribution* of $\bar X$ is more **concentrated** around $\mu$. But a *specific realisation* from that tighter distribution is **not guaranteed** to land closer to $\mu$ than a realisation from the smaller-$n$ distribution — you can always be unlucky. The correct rephrasing is probabilistic: for any fixed $c>0$,
$$\Pr(|\bar X_{100} - \mu| \le c) \;\ge\; \Pr(|\bar X_{15} - \mu| \le c),$$
so on average across hypothetical resamples, the large-$n$ estimator is more concentrated.

---

**(i) Validity of the CI — selection bias, sample size, discrete data.** Two conditions must hold for the CI to deliver its nominal 95% coverage:
1. **Random sample** from the target population (no selection bias). If, e.g., the firm only surveyed pizzerias that *replied to a voluntary online questionnaire*, respondents would self-select — $\bar x$ would be biased ($\mathbb E[\bar X] \ne \mu$) and the CI formula would still produce an interval, but its 95% guarantee **would not hold**.
2. **Normality or large-$n$ CLT.** With $n=100$ the CLT applies to $\bar X$ regardless of the shape of `Sales` — **right-skewed, heavy-tailed, even discrete count data are all fine**, because the variable being inferred is the *mean* $\mu$ (real-valued); the CLT smooths the sampling distribution of $\bar X$. For small $n$ ($n\le 30$), `Sales` should be approximately normal for the $t_{n-1}$ CI to be exact; otherwise coverage degrades.

*Small-$n$ price tag.* The reliability factor inflates for small $n$: $t_{0.975,\,15} \approx 2.131$ vs $t_{0.975,\,99} \approx 1.984$ — an extra $\approx 7\%$ width purely from the fatter $t$ tails, **on top of** the $1/\sqrt n$ SE inflation. A CI from $n=16$ is therefore wider on *two* counts (larger $c$ **and** larger $s/\sqrt n$).

The CI procedure cannot fix a biased sampling design — it can only correct for sampling noise under valid random sampling.

---

**(j) Closeness probability $\Pr(|\bar X - \mu| \le \varepsilon)$.** Under the CLT, $\bar X \approx \mathcal N(\mu,\,(s/\sqrt n)^2)$, so
$$\Pr(|\bar X - \mu| \le \varepsilon) \;\approx\; 2\Phi\!\left(\frac{\varepsilon}{s/\sqrt n}\right) - 1.$$
For $\varepsilon = 1\,000$ €:
$$\Pr(|\bar X - \mu| \le 1000) \;\approx\; 2\Phi(1000/820) - 1 \;=\; 2\Phi(1.220) - 1 \;\approx\; 2(0.8888) - 1 \;=\; 0.7776.$$

```r
eps <- 1000
2*pnorm(eps / se_hat) - 1            # ~0.778
```

So roughly 78% of resamples produce a $\bar X$ within 1 000 € of $\mu$.

---

**(k) One-sided 95% CI (lower bound only).** When only an *upper* bound on the risk of "too low a mean" matters (e.g., a lender wants to be 95% confident that average turnover is at least some level), use a one-sided CI:
$$\bar x \;-\; t_{1-\alpha,\,n-1}\,\tfrac{s}{\sqrt n} \;\le\; \mu.$$
With $\alpha=0.05$, $t_{0.95,\,99} \approx 1.660$, so
$$\mu \;\ge\; 23\,947 - 1.660 \cdot 820 \;\approx\; 22\,586.$$

```r
tcrit_one <- qt(0.95, df = n-1);  tcrit_one        # 1.660
xbar - tcrit_one * se_hat                            # ~22586
```

With 95% confidence the average monthly turnover is at least 22 586 €. (One-sided CIs at level $1-\alpha$ use $z_{1-\alpha}$, **not** $z_{1-\alpha/2}$.)

---

**(l) Required sample size for a target margin of error.** From $ME = z_{1-\alpha/2}\,\sigma/\sqrt n$, solve for $n$:
$$n \;=\; \left(\frac{z_{1-\alpha/2}\,\sigma}{ME}\right)^{2}.$$
Suppose we want a 95% CI of half-width $ME = 500$ €, using the plug-in $\sigma \approx s = 8\,200$:
$$n \;\ge\; \left(\frac{1.96 \cdot 8\,200}{500}\right)^{2} \;\approx\; (32.14)^{2} \;\approx\; 1\,034.$$
Round **up** to $n = 1\,034$.

```r
ME_target <- 500
sigma_pilot <- 8200
n_req <- ceiling((qnorm(0.975) * sigma_pilot / ME_target)^2)
n_req                                # 1034
```

*Take-away.* Halving the ME quadruples the required $n$ — the $\sqrt n$ rate is the binding cost of precision.

---

**Summary.** Across the 12 subparts we have built — from one sample $(\bar x = 23\,947,\,s = 8\,200,\,n = 100)$ — the full toolkit for inference on a single population mean: point estimate, SE (known vs unknown $\sigma$), two-sided CIs at 95% and 99%, ME decomposition, sample-size effects, closeness probabilities, one-sided bound, and the required-$n$ formula. The recurring lesson: the CI's coverage guarantee rests on **random sampling** and on a valid Normal approximation (CLT for $n\gtrsim 30$, or normality of $X$ for small $n$); plugging numbers into the formula without checking these assumptions yields an interval, not a 95% interval.
""",
    "images": ["statistics/images/master/master_g13a_ai.png"]
}


master_exercises["g14a_one_sample"] = {
    "title": "Master — One-sample tests (mean & proportion): NewHired + DS",
    "content": r"""**Master exercise — One-sample tests for a mean ($\sigma$ unknown) and a proportion.**

Consolidates the unique sub-tasks asked in **Ex 7.1a** (mean, $H_1:\mu<45$, NewHired), **Ex 7.1c** (proportion, $H_1:p>0.10$, NewHired) and **Ex 7.8a** (mean, $H_1:\mu<1.5$, large $n$ on DS$Children — p-value definition + CI cross-check). Unified workflow: assumptions $\to$ hypotheses $\to$ statistic $\to$ rejection region $\to$ p-value $\to$ decision.

---

### Datasets (two scenarios, one workflow)

**Scenario A — `NewHired`** ($n=47$). Variable `Weeks` = time (weeks) to find a new job.
- Summary: $\bar x = 40.1915$, $s = 17.2206$, $\widehat{\text{SE}} = s/\sqrt n = 2.5119$.
- Count: **7 of 47** workers took **more than 52 weeks** $\Rightarrow$ $\hat p = 7/47 = 0.1489$.
- Targets: $\mu_0 = 45$ weeks; $p_0 = 0.10$.

**Scenario B — `DS$Children`** ($n=750$). Variable `Children` = number of children per customer.
- Summary: $\bar x = 0.9213$, $s = 1.0640$, $\widehat{\text{SE}} = s/\sqrt n = 0.03885$.
- Target: $\mu_0 = 1.5$ (cutoff under which the manager drops baby products).

---

### (a) One-sample $t$-test for the mean — small $n$, $H_1:\mu<45$ (Ex 7.1a)

**Assumptions.** Representative sample; $n=47$ is large enough for the **CLT** $\Rightarrow$ $\bar X$ approximately normal without a normality assumption on `Weeks`. $\sigma^2$ unknown $\Rightarrow$ Student-$t$.

**Hypotheses.** Most serious error = declaring the average $<45$ when it is not. Put the directional claim in $H_1$:
$$H_0:\mu \geq 45 \quad \text{vs} \quad H_1:\mu < 45.$$

**Statistic & realisation.**
$$T = \frac{\bar X - \mu_0}{S/\sqrt n} \;\overset{H_0}{\sim}\; t_{46}, \qquad t_\text{obs} = \frac{40.1915 - 45}{2.5119} = -1.9143.$$

**Rejection regions (lower tail).**
- $\alpha = 0.05$: $R = \{t < -t_{0.95,\,46}\} = \{t < -1.679\}$. Since $-1.9143 < -1.679$ $\Rightarrow$ **reject $H_0$**.
- $\alpha = 0.01$: $R = \{t < -t_{0.99,\,46}\} = \{t < -2.410\}$. Since $-1.9143 > -2.410$ $\Rightarrow$ **do not reject $H_0$**.

**One-sided p-value.** $p = P(t_{46} \leq -1.9143) \approx 0.0309$ (normal approx.: $\Phi(-1.9143)\approx 0.0278$, since df $=46$ is large). Since $0.01 < p < 0.05$: reject at 5%, retain at 1%.

**Type I error.** $\alpha$ caps the probability of rejecting a true $H_0$; on $H_0:\mu\geq 45$ this max is attained at the boundary $\mu = 45$.

```r
# (a) NewHired: lower-tail t-test, mu0 = 45
xbar <- 40.1915; s <- 17.2206; n <- 47; mu0 <- 45
tstat <- (xbar - mu0)/(s/sqrt(n)); tstat               # -1.9143
qt(0.05, df = n-1); qt(0.01, df = n-1)                 # -1.679, -2.410
pt(tstat, df = n-1)                                    # 0.0309
TEST.mean(Weeks, mu0 = 45, alternative = "less", data = NewHired)
```

![Master illustration](statistics/images/master/master_g14a_ai.png)

---

### (b) Two-sided variant for context ($H_1:\mu \neq 45$)

Same $t_\text{obs} = -1.9143$; the rejection region splits in two tails:
- $\alpha=0.05$: $R = \{|t| > t_{0.975,\,46}\} = \{|t| > 2.013\}$. Since $1.9143 < 2.013$ $\Rightarrow$ **do not reject**.
- Two-sided p-value: $p_{2} = 2\cdot 0.0309 = 0.0618 > 0.05$.

Doubling the p-value flips the 5%-level decision — the same data are **not** significant two-sided. The direction of $H_1$ must be pre-specified from subject-matter knowledge, never picked after seeing the data.

```r
qt(0.975, df = n-1)                                    # 2.013
2 * pt(-abs(tstat), df = n-1)                          # 0.0618
```

---

### (c) One-proportion $z$-test, $H_1:p>0.10$ (Ex 7.1c)

**Setting.** A worker claims that the proportion of agency-relying workers who **struggle more than one year** ($>52$ weeks) exceeds 10%.

**Hypotheses.** Most serious error = declaring $p>0.10$ when it is not. Put the claim in $H_1$:
$$H_0:p \leq 0.10 \quad \text{vs} \quad H_1:p > 0.10.$$

**Statistic.** Under $H_0$ the SE is built with $p_0$ (textbook one-prop convention):
$$Z = \frac{\hat p - p_0}{\sqrt{p_0(1-p_0)/n}} \;\dot\sim\; N(0,1).$$

**Realisation.** With $\hat p = 7/47 = 0.1489$, $p_0 = 0.10$, $n = 47$:
$$\widehat{\text{SE}}_0 = \sqrt{\frac{0.10\cdot 0.90}{47}} = 0.04376, \qquad z_\text{obs} = \frac{0.1489 - 0.10}{0.04376} \approx 1.12.$$

**Rejection region (upper tail).**
- $\alpha=0.05$: $R = \{z > z_{0.95}\} = \{z > 1.6449\}$. $1.12 < 1.6449$ $\Rightarrow$ **do not reject**.
- $\alpha=0.01$: $R = \{z > z_{0.99}\} = \{z > 2.326\}$ — also not rejected.

**One-sided p-value.** $p = 1 - \Phi(1.12) \approx 0.13 > 0.05$ — confirms the RR decision.

**Conclusion.** Insufficient evidence that more than 10% of agency-relying workers struggle over a year. The worker's claim is not statistically supported.

```r
# (c) NewHired: one-proportion z-test, upper tail, p0 = 0.10
phat <- 7/47; p0 <- 0.10; n <- 47
se0   <- sqrt(p0*(1-p0)/n);              se0           # 0.04376
zstat <- (phat - p0)/se0;                zstat          # 1.12
1 - pnorm(zstat)                                        # 0.13
qnorm(0.95); qnorm(0.99)                                # 1.6449, 2.326
TEST.prop(Weeks > 52, p0 = 0.10, alternative = "greater", data = NewHired)
```

---

### (d) One-sample $t$-test, large $n$, $H_1:\mu<1.5$ (Ex 7.8a — `DS$Children`)

**Setting.** The sales manager will **drop baby products** if the average number of children per customer does not exceed 1.5. Same template as (a), but $n = 750$: CLT applies trivially and $t_{749}$ is indistinguishable from $N(0,1)$.

**Hypotheses.** Most serious error = dropping a still-profitable product line. Put the "stop selling" claim in $H_1$:
$$H_0:\mu \geq 1.5 \quad \text{vs} \quad H_1:\mu < 1.5.$$

**Statistic & realisation.**
$$t_\text{obs} = \frac{0.9213 - 1.5}{0.03885} \approx -14.91 \;\overset{H_0}{\sim}\; t_{749}.$$

**p-value.** $p = P(t_{749} \leq -14.91) < 10^{-4}$ — numerically zero. Reject $H_0$ at **any** conventional $\alpha$.

**p-value definition.** Probability, **under $H_0$**, of a statistic at least as extreme (in the direction of $H_1$) as the realised one. Here it is the area to the left of $-14.91$ under $t_{749}$.

**Cross-checks** (all equivalent forms of the same decision):
- RR on $\bar X$ at $\alpha=0.05$: $\bar x < 1.5 + (-1.6473)\cdot 0.03885 \approx 1.436$. Observed $0.9213 \ll 1.436$ $\Rightarrow$ reject.
- 95% two-sided CI for $\mu$: $0.9213 \pm 1.96\cdot 0.03885 = [0.845,\;0.997]$. **Does not contain** $1.5$ $\Rightarrow$ two-sided null rejected at 5%; a fortiori the one-sided $H_0:\mu\geq 1.5$ is rejected.

**Conclusion.** Overwhelming evidence that the average number of children is below 1.5 — drop the baby-products line.

```r
# (d) DS: large-n lower-tail t-test, mu0 = 1.5
xbar <- 0.9213; s <- 1.0640; n <- 750; mu0 <- 1.5
tstat <- (xbar - mu0)/(s/sqrt(n)); tstat              # -14.91
pt(tstat, df = n-1)                                   # < 1e-4
qt(0.05, df = n-1)                                    # -1.6473
mu0 + qt(0.05, df = n-1)*(s/sqrt(n))                  # 1.436 (RR upper edge on xbar)
CI.mean(Children, conf.level = 0.95, data = DS)       # [0.845, 0.997]
TEST.mean(Children, mu0 = 1.5, alternative = "less", data = DS)
```

---

### (e) Unified decision rule (mean & proportion, any tail)

Let $T$ be the test statistic ($t_{n-1}$ for the mean with $\sigma$ unknown; $Z\sim N(0,1)$ for the proportion), $\nu$ the appropriate df, $\theta\in\{\mu,p\}$:

| $H_1$ | Rejection region (level $\alpha$) | p-value |
|---|---|---|
| $\theta < \theta_0$ | $T < -t_{1-\alpha,\,\nu}$ (or $Z<-z_{1-\alpha}$) | $P(T \leq t_\text{obs})$ |
| $\theta > \theta_0$ | $T > t_{1-\alpha,\,\nu}$ (or $Z>z_{1-\alpha}$) | $P(T \geq t_\text{obs})$ |
| $\theta \neq \theta_0$ | $|T| > t_{1-\alpha/2,\,\nu}$ (or $|Z|>z_{1-\alpha/2}$) | $2\,P(T \geq |t_\text{obs}|)$ |

$$\boxed{\;\text{Reject } H_0 \iff t_\text{obs}\in R_\alpha \iff p \leq \alpha.\;}$$

The two formulations always agree. A large p-value $\neq$ "$H_0$ true"; it only means the data lack the strength to reject at the chosen level.

---

### (f) Side-by-side summary of the three applications

| Application | $H_1$ | Statistic | $t_\text{obs}/z_\text{obs}$ | p-value | @ 5% | @ 1% |
|---|---|---|---|---|---|---|
| (a) `Weeks` mean, $\mu_0=45$ | $\mu<45$ | $t_{46}$ | $-1.9143$ | $0.0309$ | reject | retain |
| (c) `Weeks>52` proportion, $p_0=0.10$ | $p>0.10$ | $Z$ | $1.12$ | $0.13$ | retain | retain |
| (d) `Children` mean, $\mu_0=1.5$ | $\mu<1.5$ | $t_{749}$ | $-14.91$ | $<10^{-4}$ | reject | reject |

---

**Linked snippets:** Ex 7.1a (mean, $H_1:\mu<45$, NewHired) $\to$ part (a); Ex 7.1c (proportion, $H_1:p>0.10$, NewHired) $\to$ part (c); Ex 7.8a (mean, $H_1:\mu<1.5$, DS$Children — large-$n$, p-value definition, CI cross-check) $\to$ part (d). All three are instances of the unified template (e).
""",
    "images": ["statistics/images/master/master_g14a_ai.png"],
}


# =====================================================================
# g15a_simple_reg — Simple regression: estimation, R^2, slope test
# Consolidates: ex8.1a, ex8.1b, ex8.2a, ex8.3a, ex8.5a, ex8.8a
# Dataset: NewHired (Weeks ~ Age), n = 47
# =====================================================================
master_exercises["g15a_simple_reg"] = {
    "title": "Master Exam — Simple regression on NewHired (Weeks ~ Age): OLS, $R^2$, SE, t-test, CI, F-test",
    "content": r"""**Master exercise — Simple linear regression (consolidated).**

A single dataset, eight sub-points covering every unique concept asked across the linked snippets **Ex 8.1a**, **Ex 8.1b**, **Ex 8.2a**, **Ex 8.3a**, **Ex 8.5a** and **Ex 8.8a**: OLS criterion, point estimates, slope interpretation, $R^2$ decomposition, standard errors of $\hat\beta_0$ and $\hat\beta_1$, $t$-test for slope significance, confidence interval for $\beta_1$, $F$-test for overall significance and the $F=t^2$ identity for simple regression.

---

### Dataset (single, shared by all parts)

A job agency tracks $n=47$ workers who managed to find a new job. For each worker the agency records two variables:

- $X = \text{Age}$ (years)
- $Y = \text{Weeks}$ (weeks needed to find a new job)

Sample summary statistics:
$$\bar x = 38.617,\qquad \bar y = 45.745,\qquad s^2_x = 88.246,\qquad s^2_y = 631.589,\qquad s_{xy} = 149.110,\qquad r_{xy} = 0.6315.$$

(Consistency check: $r_{xy}=s_{xy}/\sqrt{s^2_x\,s^2_y}=149.110/\sqrt{88.246\cdot 631.589}=149.110/236.16=0.6315$ ✓.)

```r
n      <- 47
xbar   <- 38.617;  ybar  <- 45.745
s2_x   <- 88.246;  s2_y  <- 631.589
s_xy   <- 149.110; r_xy  <- 0.6315
```

Postulated model:
$$Y_i \;=\; \beta_0 \;+\; \beta_1 X_i \;+\; \epsilon_i,\qquad \epsilon_i \stackrel{iid}{\sim} N(0,\sigma^2_\epsilon),\qquad i=1,\dots,n=47.$$

Round to 4 decimals throughout.

---

### (a) OLS criterion

The OLS estimators are defined as the minimisers of the residual sum of squares:
$$(\hat\beta_0,\hat\beta_1) \;=\; \arg\min_{\beta_0,\beta_1}\sum_{i=1}^{n}\bigl(y_i-\beta_0-\beta_1 x_i\bigr)^2.$$

The first-order conditions (the *normal equations*) yield the closed-form solutions
$$\boxed{\;\hat\beta_1 \;=\; \frac{s_{xy}}{s^2_x},\qquad \hat\beta_0 \;=\; \bar y - \hat\beta_1\bar x.\;}$$

The intercept formula encodes the fact that the OLS line **always passes through the centre of mass** $(\bar x,\bar y)$. No distributional assumption on $\epsilon$ is needed for these estimators to be defined or unbiased — Gauss–Markov requires only zero-mean, homoscedastic, uncorrelated errors.

---

### (b) Point estimates of $\beta_0$ and $\beta_1$

Plug the sample moments into the formulas:
$$\hat\beta_1 \;=\; \frac{149.110}{88.246} \;=\; 1.6898,$$
$$\hat\beta_0 \;=\; 45.745 \;-\; 1.6898\cdot 38.617 \;=\; -19.5262.$$

**Estimated regression line:**
$$\boxed{\;\widehat{\text{Weeks}} \;=\; -19.5262 \;+\; 1.6898\cdot\text{Age}.\;}$$

```r
b1 <- s_xy/s2_x;          b1                # 1.6898
b0 <- ybar - b1*xbar;     b0                # -19.5262
```

---

### (c) Interpretation of the slope

$\hat\beta_1 = 1.6898$ means that a $+1$-year increase in Age is associated, **on average**, with $\approx 1.69$ *additional* weeks needed to find a new job. The sign is **positive**, so older agency-relying workers tend to need *longer* job searches — consistent with labour-market intuition (skill obsolescence, age-based hiring frictions, narrower set of suitable openings).

The intercept $\hat\beta_0 = -19.53$ is **not** economically meaningful on its own: it corresponds to $\text{Age}=0$, which is far outside the observed range, and is mathematically allowed to be negative even though Weeks must be $\geq 0$ in reality.

---

### (d) Goodness of fit — $R^2$ via the variance decomposition

For every observation,
$$y_i - \bar y \;=\; \underbrace{(\hat y_i - \bar y)}_{\text{explained}} \;+\; \underbrace{(y_i - \hat y_i)}_{\text{residual}}.$$
Squaring and summing over $i$, the OLS cross-product $\sum_i(\hat y_i-\bar y)(y_i-\hat y_i)$ vanishes by the orthogonality of residuals to the fitted values, leaving
$$\underbrace{\sum_i(y_i-\bar y)^2}_{SST} \;=\; \underbrace{\sum_i(\hat y_i-\bar y)^2}_{SSR} \;+\; \underbrace{\sum_i(y_i-\hat y_i)^2}_{SSE}.$$

The **coefficient of determination** is the explained share:
$$R^2 \;=\; \frac{SSR}{SST} \;=\; 1 \;-\; \frac{SSE}{SST} \;\in\; [0,1].$$

In simple linear regression $R^2 = r_{xy}^2$ (squared sample correlation):
$$R^2 \;=\; 0.6315^2 \;=\; 0.3988.$$

**Interpretation.** Age alone explains $\approx \mathbf{40\%}$ of the variability of Weeks; the remaining $\approx 60\%$ is residual noise coming from omitted drivers (qualification, sector, regional labour market, network, $\ldots$). Moderate fit.

```r
R2  <- r_xy^2;            R2                # 0.3988
SST <- (n-1)*s2_y;        SST               # 29053.1
SSR <- R2*SST;            SSE <- SST - SSR  # 11586.4 ; 17466.7
```

---

### (e) Standard errors of $\hat\beta_1$ and $\hat\beta_0$

**Step 1 — total and residual SS.**
$$SST \;=\; (n-1)\,s^2_y \;=\; 46\cdot 631.589 \;=\; 29{,}053.1,$$
$$SSE \;=\; (1-R^2)\,SST \;=\; 0.6012\cdot 29{,}053.1 \;=\; 17{,}466.7,$$
$$SSR \;=\; R^2\cdot SST \;=\; 0.3988\cdot 29{,}053.1 \;=\; 11{,}586.4.$$

**Step 2 — residual variance.**
$$s^2_\epsilon \;=\; \frac{SSE}{n-2} \;=\; \frac{17{,}466.7}{45} \;=\; 388.15,\qquad s_\epsilon \;=\; 19.70.$$

**Step 3 — slope SE.**
$$\widehat{\text{se}}(\hat\beta_1) \;=\; \frac{s_\epsilon}{\sqrt{(n-1)\,s^2_x}} \;=\; \frac{19.70}{\sqrt{46\cdot 88.246}} \;=\; \frac{19.70}{63.7245} \;=\; 0.3092.$$

**Step 4 — intercept SE.**
$$\widehat{\text{se}}(\hat\beta_0) \;=\; s_\epsilon\sqrt{\frac{1}{n} + \frac{\bar x^2}{(n-1)\,s^2_x}} \;=\; 19.70\sqrt{0.02128 + 0.36728} \;=\; 19.70\cdot 0.6234 \;=\; 12.281.$$

Intuition: $\widehat{\text{se}}(\hat\beta_1)$ shrinks with sample size $n$, with the spread of $X$ ($s^2_x$), and with low noise $s_\epsilon$ — same three drivers that show up in every regression SE.

```r
s2_eps <- SSE/(n-2);      s_eps <- sqrt(s2_eps)            # 21.94 ; 4.6843
se_b1  <- s_eps/sqrt((n-1)*s2_x);          se_b1
se_b0  <- s_eps*sqrt(1/n + xbar^2/((n-1)*s2_x));  se_b0
```

---

### (f) Slope significance — two-sided $t$-test

**Hypotheses.** $H_0:\beta_1 = 0$ (no linear relation between Age and Weeks) vs $H_1:\beta_1\neq 0$.

**Test statistic** (under $H_0$, with $\sigma^2_\epsilon$ unknown):
$$T \;=\; \frac{\hat\beta_1 - 0}{\widehat{\text{se}}(\hat\beta_1)} \;\overset{H_0}{\sim}\; t_{n-2} \;=\; t_{45}.$$

**Realisation.** Using the SE from (e):
$$t_\text{obs} \;=\; \frac{1.6898}{0.3092} \;\approx\; 5.464.$$

(Cross-check via the algebraic identity $t^2 = (n-2)\,R^2/(1-R^2) = 45\cdot 0.3988/0.6012 = 29.85 \Rightarrow |t| = 5.464$ ✓ — the two routes agree exactly because $t^2 = F = \text{SSR}/\text{MSE}$ holds in any simple OLS regression.)

**Rejection region** at $\alpha=0.05$ (two-sided): $R = \{|t| > t_{0.975,\,45}\} = \{|t| > 2.014\}$.

Since $|t_\text{obs}|$ vastly exceeds 2.014, **reject $H_0$**. The two-sided $p$-value is
$$p \;=\; 2\cdot P(T_{45} > |t_\text{obs}|) \;\approx\; 0 \quad(<10^{-5}).$$

**Conclusion.** Age is a **strongly significant** predictor of Weeks at every conventional level ($\alpha = 1\%, 5\%, 10\%$). On the basis of the estimated model, a one-year increase in age corresponds, on average, to a $\approx 1.69$-week increase in job-search duration.

```r
t_obs <- sqrt((n-2)*R2/(1-R2));            t_obs           # 5.464
2*(1 - pt(abs(t_obs), df=n-2))                              # p ~ 1.8e-6
qt(0.975, df=n-2)                                           # 2.014
```

---

### (g) 95% confidence interval for $\beta_1$

$$\hat\beta_1 \;\pm\; t_{0.975,\,n-2}\,\widehat{\text{se}}(\hat\beta_1) \;=\; 1.6898 \;\pm\; 2.014\cdot 0.3092.$$

Numerically:
$$CI_{95}(\beta_1) \;=\; 1.6898 \;\pm\; 0.6227 \;=\; [1.0671,\;2.3125].$$

**Interpretation.** With 95% confidence, the population slope lies between $\approx 1.07$ and $\approx 2.31$ extra weeks per additional year of age. **Zero is far outside the interval**, which is consistent — by duality — with rejecting $H_0:\beta_1=0$ at the 5% level in part (f). The CI procedure and the two-sided $t$-test are equivalent: $0 \in CI_{1-\alpha} \iff$ retain $H_0$ at level $\alpha$.

```r
se_b1 <- 0.3092
ME    <- qt(0.975, df=n-2) * se_b1
c(b1 - ME, b1 + ME)                                         # [1.067, 2.313]
```

---

### (h) $F$-test (overall model significance) and the $F = t^2$ identity

For simple regression with $k=1$ predictor, the **ANOVA $F$-test** of $H_0:\beta_1=0$ vs $H_1:\beta_1\neq 0$ uses:

| Source | SS | df | MS |
|---|---|---|---|
| Regression | $SSR = 11{,}586.4$ | $1$ | $MSR = 11{,}586.4$ |
| Residual   | $SSE = 17{,}466.7$ | $n-2 = 45$ | $MSE = s^2_\epsilon = 388.15$ |
| Total      | $SST = 29{,}053.1$ | $n-1 = 46$ | — |

$$F_\text{obs} \;=\; \frac{MSR}{MSE} \;=\; \frac{11{,}586.4}{388.15} \;=\; 29.85 \;\overset{H_0}{\sim}\; F_{1,\,45}.$$

Critical value at $\alpha=0.05$: $F_{0.95;\,1,\,45} \approx 4.06$. Since $29.85 \gg 4.06$, **reject $H_0:\beta_1 = 0$**. The $p$-value is $1 - P(F_{1,45} \leq 29.85) \approx 1.8\times 10^{-6}$.

**The $F = t^2$ identity.** In simple regression with one predictor,
$$F \;=\; t^2 \quad\text{exactly.}$$
Indeed, $t^2 = (n-2)\,R^2/(1-R^2) = 45\cdot 0.3988/0.6012 = 29.85 = F$ (up to rounding). This is **not a coincidence**: the $F$-statistic on $(1,n-2)$ degrees of freedom is the *square* of the two-sided $t$-statistic on $n-2$ df, because the rejection regions coincide:
$$|T_{n-2}| > t_{1-\alpha/2,\,n-2} \;\iff\; T_{n-2}^2 > F_{1-\alpha;\,1,\,n-2}.$$
Same null, same alternative, same rejection set $\Rightarrow$ algebraically the same test.

**When does $F$ become genuinely different?** Only in *multiple* regression ($k \geq 2$). There the $F$-test checks the **joint** significance of all slopes simultaneously ($H_0:\beta_1=\cdots=\beta_k=0$), while individual $t$-tests check them one at a time. The two can disagree (e.g. $F$ significant but no single $t$ significant in the presence of multicollinearity).

```r
MSR   <- SSR/1;  MSE <- SSE/(n-2)
F_obs <- MSR/MSE;          F_obs                            # 29.86
F_obs - t_obs^2                                             # ~ 0
1 - pf(F_obs, df1=1, df2=n-2)                               # p ~ 1.8e-6
qf(0.95, df1=1, df2=n-2)                                    # 4.06

# Cross-check on the raw data
mod <- lm(Weeks ~ Age, data=NewHired)
summary(mod)        # b0, b1, t-stats, R^2, F
confint(mod, level=0.95)
anova(mod)          # ANOVA decomposition
```

---

### (i) Summary box

| Quantity | Value | Source |
|---|---|---|
| $\hat\beta_0$ | $-19.5262$ | $\bar y - \hat\beta_1\bar x$ |
| $\hat\beta_1$ | $1.6898$ | $s_{xy}/s^2_x$ |
| $R^2$ | $0.3988$ | $r_{xy}^2 = SSR/SST$ |
| $SST,\,SSR,\,SSE$ | $29053.1,\,11586.4,\,17466.7$ | variance decomposition |
| $s_\epsilon$ | $19.70$ | $\sqrt{SSE/(n-2)}$ |
| $\widehat{\text{se}}(\hat\beta_1)$ | $0.3092$ | $s_\epsilon/\sqrt{(n-1)s^2_x}$ |
| $\widehat{\text{se}}(\hat\beta_0)$ | $12.28$ | $s_\epsilon\sqrt{1/n+\bar x^2/((n-1)s^2_x)}$ |
| $t_\text{obs}$ ($H_0:\beta_1=0$) | $5.464$, $p\approx 1.8\times 10^{-6}$ | $\hat\beta_1/\widehat{\text{se}}(\hat\beta_1)$ |
| 95% CI for $\beta_1$ | $[1.067,\,2.313]$ | $\hat\beta_1 \pm t_{0.975,45}\widehat{\text{se}}$ |
| $F_\text{obs}$ | $29.85 = t_\text{obs}^2$, $p\approx 1.8\times 10^{-6}$ | $MSR/MSE$ |

**Conclusion across (a)–(h).** Age is a **strongly significant** linear predictor of the number of Weeks needed to find a new job. The slope is positive ($+1.69$ weeks per extra year of age), the relation explains about 40% of the variability of Weeks, and the $t$-test on $\beta_1$, the 95% CI for $\beta_1$ and the $F$-test on the full model all reach the *same* conclusion — algebraically inevitable in simple regression because $F = t^2$.

---

**Linked snippets:** Ex 8.1a (OLS + slope significance), Ex 8.1b ($R^2$ via variance decomposition), Ex 8.2a (OLS criterion), Ex 8.3a (Weeks ~ Age fit + 90% CI for slope), Ex 8.5a (closed-form estimation from summary statistics + $R^2$ + $t$-test), Ex 8.8a (estimation from summary statistics + slope $t$-test). All six reduce to the unified workflow above.

![Master G15a — scatter+OLS+residuals, SS decomposition, t-test, CI for slope](statistics/images/master/master_g15a_ai.png)
""",
    "images": ["statistics/images/master/master_g15a_ai.png"],
}


# ---------------------------------------------------------------------
# G15c — Multiple regression  (master exercise)
# Dataset: `superstore` (n = 2200).  Response: MntMeatProducts (€/yr).
# Regressors: IncomeK (k€), Age (yrs), KidsAtHome (number of kids: 0..2).
# Linked snippets distilled: ex9 9_1, 9_2, 9_3, 9_4, 9_8, 9_10, 9_11,
# 9_12, 9_13.
# ---------------------------------------------------------------------
master_exercises["g15c_multi_reg"] = {
    "title": "Master Ex — Multiple regression (superstore: MntMeat ~ IncomeK + Age + KidsAtHome, n=2200)",
    "content": r"""**One consolidated exam-style exercise on multiple regression.** Distilled from Ex 9.1, 9.2, 9.3, 9.4, 9.8, 9.10, 9.11, 9.12, 9.13: a single dataset, all unique subparts asked at the exam.

**Setting.** Dataframe `superstore` ($n = 2200$ retail customers). Response: `MntMeatProducts` = € spent on meat products in the last 2 years. Regressors: `IncomeK` = annual household income in k€, `Age` = age in years, `KidsAtHome` $\in\{0,1,2\}$ = number of children at home (treated as numeric).

Fit by OLS the multiple linear model
$$\text{MntMeat}_i \;=\; \beta_0 + \beta_1\,\text{IncomeK}_i + \beta_2\,\text{Age}_i + \beta_3\,\text{KidsAtHome}_i + \varepsilon_i,\qquad \varepsilon_i\stackrel{\text{iid}}{\sim}\mathcal N(0,\sigma^2).$$

**`R` summary (rounded):**

\begin{tabular}{p{8cm}|p{8cm}|p{8cm}|p{7cm}|p{7cm}}
\textbf{Coefficient} & \textbf{Estimate} & \textbf{Std. Error} & \textbf{$t$ value} & \textbf{$p$-value} \\
(Intercept) $\hat\beta_0$ & $-74.10$ & $13.05$ & $-5.68$ & $\approx 0$ \\
IncomeK $\hat\beta_1$ & $+6.142$ & $0.168$ & $36.56$ & $\approx 0$ \\
Age $\hat\beta_2$ & $-2.805$ & $0.286$ & $-9.81$ & $\approx 0$ \\
KidsAtHome $\hat\beta_3$ & $-78.40$ & $7.62$ & $-10.29$ & $\approx 0$ \\
\end{tabular}

Residual std error $\hat\sigma_\varepsilon = 134.50$ on $df = n-K-1 = 2196$. Multiple $R^2 = 0.6361$, adjusted $R^2 = 0.6356$. Global $F(3,2196) = 1279.7$, $p\approx 0$.

**Reference quantiles:** $t_{2196,\,0.975}\approx 1.961$, $t_{2196,\,0.995}\approx 2.578$, $F_{3,2196,\,0.95}\approx 2.61$.

---

**(a) Read the estimated equation; interpret each slope (ceteris paribus).**

The fitted multiple-regression hyperplane is
$$\widehat{\text{MntMeat}} \;=\; -74.10 \;+\; 6.142\,\text{IncomeK} \;-\; 2.805\,\text{Age} \;-\; 78.40\,\text{KidsAtHome}.$$

Partial-slope reading — *each $\hat\beta_j$ is the expected change in `MntMeat` for a 1-unit increase in $x_j$ holding all other regressors fixed*:

- $\hat\beta_1 = +6.142$ €/k€ — keeping `Age` and `KidsAtHome` fixed, **+1 k€ of household income raises expected yearly meat spend by $\approx 6.14$ €**.
- $\hat\beta_2 = -2.805$ €/yr — at fixed income and number of kids, **+1 year of age lowers expected meat spend by $\approx 2.81$ €** (older customers buy less meat once income is controlled for).
- $\hat\beta_3 = -78.40$ €/kid — at fixed income and age, **+1 child at home is associated with $-78.4$ € on expected meat spend**.
- $\hat\beta_0 = -74.10$ has no useful interpretation (a 0-k€-income, 0-year-old customer with 0 kids — outside the data).

---

**(b) Goodness of fit: multiple $R^2$, adjusted $R^2$, and why the penalty.**

$$R^2 \;=\; 1 - \frac{\text{SSE}}{\text{SST}} \;=\; \frac{\text{SSR}}{\text{SST}} \;=\; 0.6361,\qquad R^2_{\text{adj}} \;=\; 1 - \frac{\text{SSE}/(n-K-1)}{\text{SST}/(n-1)} \;=\; 0.6356.$$

About **63.6%** of the variance of `MntMeat` is jointly explained by income, age and number of kids. Adjusted $R^2$ is barely smaller because it penalises the model by $K=3$ added regressors: with $n=2200$ the penalty is tiny, but with small $n$ it matters — adding a useless regressor *always* raises $R^2$ but can lower $R^2_{\text{adj}}$. Use $R^2_{\text{adj}}$ to compare models with a different number of predictors.

```r
# Fit the master multiple regression
mod <- lm(MntMeatProducts ~ IncomeK + Age + KidsAtHome, data=superstore)
summary(mod)$r.squared           # 0.6361
summary(mod)$adj.r.squared       # 0.6356
```

---

**(c) Global $F$-test (all slopes = 0).**

$H_0: \beta_1=\beta_2=\beta_3=0$ vs $H_1:$ at least one $\beta_j\ne 0$, with statistic
$$F \;=\; \frac{\text{SSR}/K}{\text{SSE}/(n-K-1)} \;=\; \frac{R^2/K}{(1-R^2)/(n-K-1)} \;=\; \frac{0.6361/3}{0.3639/2196} \;=\; 1279.7,$$
on $(K,\,n-K-1)=(3,\,2196)$ df. Since $F = 1279.7 \gg F_{3,2196,\,0.95} = 2.61$ (equivalently $p\approx 0$), **reject $H_0$**: at least one slope is non-zero — the model is globally significant.

```r
qf(0.95, df1=3, df2=2196)        # ~ 2.61
# Realised F = 1279.7 >> 2.61  =>  reject H0 at any usual alpha.
```

---

**(d) Individual $t$-tests on each coefficient.**

For $H_0:\beta_j=0$ vs $H_1:\beta_j\ne 0$, $t_j = \hat\beta_j / \widehat{\text{se}}(\hat\beta_j) \sim t_{n-K-1}=t_{2196}$ under $H_0$. With $t_{2196,\,0.975}\approx 1.961$:

\begin{tabular}{p{8cm}|p{8cm}|p{8cm}|p{8cm}|p{6cm}}
\textbf{Coefficient} & \textbf{$\hat\beta_j$} & \textbf{$\widehat{\text{se}}$} & \textbf{$t$} & \textbf{Conclusion} \\
IncomeK & $+6.142$ & $0.168$ & $36.56$ & reject $H_0$ ($p\!\approx\! 0$) \\
Age & $-2.805$ & $0.286$ & $-9.81$ & reject $H_0$ ($p\!\approx\! 0$) \\
KidsAtHome & $-78.40$ & $7.62$ & $-10.29$ & reject $H_0$ ($p\!\approx\! 0$) \\
\end{tabular}

All three slopes are highly significant at any usual $\alpha$.

```r
summary(mod)                     # t-values & p-values in the coefficients table
qt(0.975, df=2196)               # 1.961  (df = n-K-1)
```

---

**(e) 95% confidence interval for each $\beta_j$.**

$\hat\beta_j \pm t_{2196,\,0.975}\cdot\widehat{\text{se}}(\hat\beta_j) = \hat\beta_j \pm 1.961\,\widehat{\text{se}}(\hat\beta_j)$:

\begin{tabular}{p{10cm}|p{18cm}|p{10cm}}
\textbf{Parameter} & \textbf{95% CI} & \textbf{Conclusion} \\
$\beta_{\text{IncomeK}}$ & $(6.142 - 1.961\cdot 0.168,\;6.142 + 1.961\cdot 0.168)=(5.812,\;6.472)$ & 0 excluded \\
$\beta_{\text{Age}}$ & $(-2.805 - 1.961\cdot 0.286,\;-2.805 + 1.961\cdot 0.286)=(-3.366,\;-2.244)$ & 0 excluded \\
$\beta_{\text{KidsAtHome}}$ & $(-78.40 - 1.961\cdot 7.62,\;-78.40 + 1.961\cdot 7.62)=(-93.34,\;-63.46)$ & 0 excluded \\
\end{tabular}

Each CI excludes 0 — consistent with the $t$-tests at (d). Note the duality: *the CI at level $1-\alpha$ excludes 0 iff the two-sided $t$-test rejects at $\alpha$*.

```r
confint(mod, level=0.95)
##                 2.5 %    97.5 %
## (Intercept)   -99.69    -48.51
## IncomeK         5.812     6.472
## Age            -3.366    -2.244
## KidsAtHome    -93.34    -63.46
```

---

**(f) Marginal effect over a non-unit change: $\beta\cdot\Delta x$ and its 95% CI.**

Question: how much extra meat spend is expected, on average, for a **+10 k€ pay rise** (other things equal)?

Point estimate: $\Delta\hat y = 10\cdot\hat\beta_1 = 10\cdot 6.142 = 61.42$ €. The 95% CI rescales the CI for $\beta_1$:
$$10\cdot(5.812,\,6.472) = (58.12,\;64.72)\;\text{€}.$$
Strictly positive, narrow: a typical 10 k€ raise is associated with $\approx 58$–$65$ € extra meat spend per year.

Same logic for the **+5-year-of-age** scenario: $5\hat\beta_2=-14.03$ €, 95% CI $5\cdot(-3.366,\,-2.244)=(-16.83,\,-11.22)$ €.

```r
10 * coef(mod)["IncomeK"]                  # 61.42 EUR
10 * confint(mod, "IncomeK", level=0.95)   # (58.12 ; 64.72)
5  * confint(mod, "Age",     level=0.95)   # (-16.83 ; -11.22)
```

---

**(g) Confounding / omitted-variable bias — *why* including all 3 regressors matters.**

Run instead the *simple* regression `MntMeat ~ IncomeK`:
$$\widehat{\text{MntMeat}}_{\text{simple}} \;=\; -101.5 \;+\; 5.380\,\text{IncomeK},\qquad R^2_{\text{simple}} \;=\; 0.521.$$

The simple slope on `IncomeK` is $+5.380$ €/k€, *smaller* than the partial slope $+6.142$ from the multiple model. The two are linked by the OVB formula (for a two-regressor block, derived in Ex 9.3):
$$E[\hat\beta_1^{\text{simple}}] \;=\; \beta_1 \;+\; \beta_3\,\frac{\mathrm{Cov}(\text{IncomeK},\text{KidsAtHome})}{\mathrm{Var}(\text{IncomeK})}.$$

Empirically, $\mathrm{cor}(\text{IncomeK},\text{KidsAtHome}) \approx -0.34$ — *richer customers tend to have fewer kids at home* (children grow up and move out by the time income peaks). Since $\hat\beta_3<0$ and the regressors are negatively correlated, the product $\hat\beta_3\cdot(\mathrm{Cov}/\mathrm{Var})>0$ shrinks the simple slope downward toward $5.38$. Once `KidsAtHome` (and `Age`) are controlled for, the *true* partial income effect ($+6.14$) re-emerges. The simple model **mis-attributes** part of the income effect to the kids channel — a textbook confounding pattern (parallel to Ex 9.3's Competition vs Quality sign-flip).

```r
mod.simple <- lm(MntMeatProducts ~ IncomeK, data=superstore); summary(mod.simple)
## (Intercept)  -101.50
## IncomeK         5.380   R^2 = 0.521
cor(superstore$IncomeK, superstore$KidsAtHome)   # ~ -0.34
# OVB shrinks the simple slope toward 5.38; controlling for KidsAtHome restores 6.14.
```

---

**(h) Prediction at a target customer profile (`IncomeK = 70, Age = 45, KidsAtHome = 1`).**

Point prediction:
$$\hat y_0 \;=\; -74.10 + 6.142\cdot 70 - 2.805\cdot 45 - 78.40\cdot 1 \;=\; -74.10 + 429.94 - 126.23 - 78.40 \;=\; 151.21\;\text{€}.$$

Two intervals built on this same $\hat y_0$:

- **95% confidence interval** for $E[\text{MntMeat}\mid x_0]$ — narrow, captures *only* the sampling error of $\hat\beta$: $\widehat{\text{se}}(\hat y_0)\approx 3.5\Rightarrow (144.3,\;158.1)$ €. Width $\approx 14$ €.
- **95% prediction interval** for a *single* customer's spend — wider, adds the irreducible residual $\hat\sigma_\varepsilon^2 = 134.50^2$: $\widehat{\text{se}}_{\text{pred}}(y_0)\approx\sqrt{3.5^2+134.5^2}\approx 134.55\Rightarrow (-112.6,\;414.9)$ €. Width $\approx 528$ € — about **38× wider**.

This is the structural pattern noted in Ex 9.9 / 9.10 / 9.11: low residual share of variance ($1-R^2\approx 0.36$) lets the CI on the mean be informative, but individual predictions remain very uncertain because $\hat\sigma_\varepsilon$ — not $\widehat{\text{se}}(\hat\beta)$ — dominates the PI.

```r
newdata <- data.frame(IncomeK=70, Age=45, KidsAtHome=1)
predict(mod, newdata, interval="confidence", level=0.95)
##      fit      lwr      upr
##  151.21   144.3    158.1
predict(mod, newdata, interval="prediction", level=0.95)
##      fit      lwr      upr
##  151.21  -112.6    414.9
```

---

**(i) Mini-diagnostic checklist (linear-model assumptions).**

OLS inference rests on: $E[\varepsilon\mid X]=0$ (linearity), $\mathrm{Var}(\varepsilon\mid X)=\sigma^2$ (homoscedasticity), $\mathrm{Cor}(\varepsilon_i,\varepsilon_j)=0$, approximate normality of $\varepsilon$ (CLT covers it at $n=2200$). Standard checks: `plot(mod, which=1)` (residuals vs fitted — funnel = heteroscedasticity; curvature = non-linearity), `hist(rstandard(mod))` (right-skew / heavy tails). In `superstore`, residuals vs fitted typically show a funnel (variance grows with $\hat y$) and right-skewed residuals — so the CIs/PI above should be taken with caution; sandwich SEs (`lmtest::coeftest(mod, vcov=sandwich::vcovHC)`) or a log-transform of `MntMeat` are the usual fixes.

```r
plot(mod, which=1)               # residuals vs fitted: funnel?
hist(rstandard(mod), breaks=30)  # right-skew?
# Robust-SE fallback if assumptions fail:
# library(sandwich); library(lmtest); coeftest(mod, vcov=vcovHC(mod, type="HC3"))
```

---

**Concept map covered by this single exercise.**

\begin{tabular}{p{12cm}|p{26cm}}
\textbf{Concept} & \textbf{Where in this exercise} \\
Estimate $\hat\beta_0,\hat\beta_1,\hat\beta_2,\hat\beta_3$ & part (a) \\
Partial-slope (ceteris paribus) interpretation & part (a) \\
Multiple $R^2$, adjusted $R^2$ (and penalty) & part (b) \\
Global $F$-test (all slopes $=0$) & part (c) \\
Individual $t$-tests on each $\beta_j$ & part (d) \\
95% CI for each $\beta_j$ (CI ↔ test duality) & part (e) \\
Marginal effect $\beta\cdot\Delta x$ and its CI & part (f) \\
Confounding / omitted-variable bias (simple vs multiple slope) & part (g) \\
Point prediction at a given $x_0$; CI vs PI gap & part (h) \\
Residual diagnostics (assumption check) & part (i) \\
\end{tabular}

![Master G15c — coefficient t-stats, added-variable plot, R² vs adj-R², CI vs PI](statistics/images/master/master_g15c_ai.png)
""",
    "images": ["statistics/images/master/master_g15c_ai.png"],
}


master_exercises["g13b_ci_one_prop"] = {
    "title": "Master Exam — CI for one proportion (consolidated)",
    "content": r"""**Master dataset.** A retailer interviews $n = 500$ random customers about whether they returned to the store within the last month. The result is $X = 200$ returning customers, i.e. a sample proportion $\hat p = X/n = 200/500 = 0.40$. We reuse this single dataset across every part below — only $n$, the confidence level $1-\alpha$, or the target margin of error change.

This master exercise consolidates the unique sub-questions of the 13 linked snippets into **eight building blocks** of `g13b_ci_one_prop` (CI for one proportion + sample-size planning):
1. Sample proportion + plug-in standard error (incl. **how to *build* the Bernoulli indicator**: direct $\{0,1\}$ coding, paired-data sign reduction, count-data recoding).
2. Two-sided 95% Wald CI.
3. Effect of $n$ on precision (compare $n=100$ vs $n=1000$, same $\hat p$).
4. Effect of confidence level (90% vs 95% vs 99%, same $n=500$).
5. Sample-size planning: worst-case ($p=0.5$) vs pilot ($\hat p$).
6. Margin-of-error decomposition $ME = z\cdot SE$.
7. One-sided / lower-bound version of the CI + largest level keeping $L > p_0$.
8. Validity check (CLT condition $n\hat p \ge 5$ AND $n(1-\hat p) \ge 5$).

---

### Part (1) — Point estimate and standard error

Model each customer as $Y_i = \mathbb{1}\{\text{returns}\} \sim \text{Bernoulli}(p)$ i.i.d. The natural unbiased estimator of $p$ is the sample proportion
$$\hat p \;=\; \frac{1}{n}\sum_{i=1}^{n} Y_i \;=\; \frac{X}{n} \;=\; \frac{200}{500} \;=\; 0.40.$$
Unbiasedness: $\mathbb{E}[\hat p] = p$ by linearity of expectation. Since $\Var(Y_i) = p(1-p)$ and the draws are i.i.d.,
$$\Var(\hat p) \;=\; \frac{p(1-p)}{n}, \qquad SE(\hat p) \;=\; \sqrt{\frac{p(1-p)}{n}}.$$
Plug $\hat p$ in for the unknown $p$:
$$\widehat{SE}(\hat p) \;=\; \sqrt{\frac{\hat p(1-\hat p)}{n}} \;=\; \sqrt{\frac{0.40 \cdot 0.60}{500}} \;=\; \sqrt{4.8\times 10^{-4}} \;\approx\; 0.02191.$$

```r
n     <- 500
x     <- 200
phat  <- x/n                          # 0.40
se    <- sqrt(phat*(1-phat)/n)        # ~ 0.02191
phat; se
```

**Building the Bernoulli indicator $Y_i$ — three common patterns.** Exams disguise the same one-proportion problem by giving the data in different formats. In every case, *first* reduce to a $\{0,1\}$ vector, *then* apply $\hat p = \bar Y$.

| Data format | Construction of $Y_i$ | Example |
|---|---|---|
| **Direct categorical** ("Yes"/"No", "Female"/"Male", "Hybrid"/other) | $Y_i = \mathbb 1\{\text{cat}_i = \text{success}\}$ | `Y <- (Gender == "Female")` |
| **Paired data** (Pre/Post, NA/EU, before/after): "fraction who *improved*" | Take the **sign** of the within-unit difference: $Y_i = \mathbb 1\{D_i > 0\}$ with $D_i = \text{Post}_i - \text{Pre}_i$. Magnitude is discarded. | `Y <- (Post - Pre > 0)` |
| **Threshold on a numeric variable**: "share of titles selling $> 1$ M" | $Y_i = \mathbb 1\{X_i > c\}$ for the threshold $c$ | `Y <- (Global_Sales > 1)` |
| **Count / frequency table**: "share of customers visiting $\ge k$ times" | $Y_i = \mathbb 1\{N_i \ge k\}$; from a frequency table, $X = n - \sum_{j < k} f_j$ | `x <- n - 32; phat <- x/n` |

The reduction is mechanical and **does not require any extra assumption** beyond i.i.d. sampling — the Bernoulli model is fully specified by $p$ once each $Y_i \in \{0,1\}$. Once $\hat p$ and $n$ are in hand, the same Wald machinery of Parts (2)–(8) applies *verbatim*.

---

### Part (2) — 95% Wald confidence interval

By the CLT, when $n$ is large enough that $n\hat p \ge 5$ and $n(1-\hat p) \ge 5$,
$$\hat p \;\overset{a}{\sim}\; \mathcal{N}\!\left(p,\; \tfrac{p(1-p)}{n}\right) \quad\Longrightarrow\quad \hat p \;\pm\; z_{1-\alpha/2}\,\widehat{SE}(\hat p).$$
With $\alpha = 0.05$ and $z_{0.975} = 1.96$:
- $ME_{95\%} = 1.96 \cdot 0.02191 \approx 0.04294$.
- $\text{CI}_{95\%} = 0.40 \pm 0.04294 = [0.357,\, 0.443]$ (width $\approx 0.086$).

**Interpretation.** With 95% confidence the true proportion $p$ of returning customers lies in $[0.357,\,0.443]$. The 95% refers to the *procedure*: 95% of intervals built this way cover the unknown $p$ in repeated sampling — we cannot say whether *this* specific interval does.

```r
ME95 <- qnorm(0.975) * se             # ~ 0.04294
c(phat - ME95, phat + ME95)           # [0.357, 0.443]
CI.prop(returns, success = "yes", conf.level = 0.95, data = customers)
```

![Master illustration](statistics/images/master/master_g13b_ai.png)

---

### Part (3) — Effect of sample size: $n=100$ vs $n=1000$ (same $\hat p$)

Holding $\hat p$ and confidence level fixed, only the SE changes. Because $SE \propto 1/\sqrt n$, multiplying $n$ by 10 divides the half-width by $\sqrt{10}\approx 3.16$ — **diminishing returns**.

| Case | $n$ | $SE = \sqrt{\hat p(1-\hat p)/n}$ | $ME_{95\%} = 1.96\cdot SE$ | 95% CI |
|---|---|---|---|---|
| Small | $100$ | $\sqrt{0.24/100} \approx 0.04899$ | $\approx 0.0960$ | $[0.304,\, 0.496]$ (w. $\approx 0.192$) |
| Master | $500$ | $\approx 0.02191$ | $\approx 0.0429$ | $[0.357,\, 0.443]$ (w. $\approx 0.086$) |
| Large | $1000$ | $\sqrt{0.24/1000} \approx 0.01549$ | $\approx 0.0304$ | $[0.370,\, 0.430]$ (w. $\approx 0.061$) |

**Take-away.** Going $n: 100 \to 1000$ shrinks the width by exactly $\sqrt{10}\approx 3.16\times$, not $10\times$. **Quadrupling** $n$ halves $ME$.

```r
for (n_i in c(100, 500, 1000)) {
  se_i <- sqrt(0.4*0.6/n_i)
  ME_i <- qnorm(0.975) * se_i
  cat("n =", n_i, " SE =", round(se_i,5), " ME =", round(ME_i,4),
      " CI = [", round(0.4-ME_i,3), ",", round(0.4+ME_i,3), "]\n")
}
```

---

### Part (4) — Effect of confidence level: 90% vs 95% vs 99% (same $n=500$, same $\hat p$)

Only the multiplier $z_{1-\alpha/2}$ changes; SE is fixed at $0.02191$.

| Level $1-\alpha$ | $z_{1-\alpha/2}$ | $ME = z\cdot SE$ | CI | Width |
|---|---|---|---|---|
| 90% | $z_{0.95} = 1.6449$ | $\approx 0.03604$ | $[0.364,\, 0.436]$ | $0.072$ |
| 95% | $z_{0.975} = 1.9600$ | $\approx 0.04294$ | $[0.357,\, 0.443]$ | $0.086$ |
| 99% | $z_{0.995} = 2.5758$ | $\approx 0.05643$ | $[0.344,\, 0.456]$ | $0.113$ |

**Take-aways.** Pushing $90\% \to 95\%$ widens the interval by $1.96/1.6449 \approx 1.19$ ($+19\%$). Pushing $95\% \to 99\%$ widens by $2.5758/1.96 \approx 1.31$ ($+31\%$). Higher confidence = wider interval at fixed $n$ — there is no free precision.

```r
for (lvl in c(0.90, 0.95, 0.99)) {
  z   <- qnorm(1 - (1-lvl)/2)
  ME  <- z * se
  cat("level =", lvl, " z =", round(z,4), " ME =", round(ME,5),
      " CI = [", round(phat-ME,3), ",", round(phat+ME,3), "]\n")
}
```

---

### Part (5) — Sample-size planning: $ME \le m$ at confidence $1-\alpha$

Invert the margin-of-error formula:
$$ME \;=\; z_{1-\alpha/2}\,\sqrt{\tfrac{p(1-p)}{n}} \;\le\; m \quad\Longleftrightarrow\quad n \;\ge\; \left(\tfrac{z_{1-\alpha/2}}{m}\right)^{\!2}\, p(1-p).$$

**Two regimes for $p(1-p)$.**

**Regime A — worst case (no prior info).** Maximise $g(p)=p(1-p)$ on $[0,1]$: $g'(p) = 1 - 2p = 0 \Rightarrow p^{\star} = 0.5$, giving $\max g = 0.25$. Plugging $p(1-p) = 0.25$ yields the *conservative* $n$ — guaranteed to meet the ME target whatever the true $p$.

**Regime B — pilot info ($\hat p = 0.40$).** Use $\hat p(1-\hat p) = 0.40\cdot 0.60 = 0.24$.

**Target: $m = 0.04$ at 95% ($z_{0.975}=1.96$).**

- Worst case: $n \ge (1.96/0.04)^{2} \cdot 0.25 = 49^{2} \cdot 0.25 = 600.25 \Rightarrow n = 601$.
- Pilot $\hat p=0.4$: $n \ge (1.96/0.04)^{2} \cdot 0.24 = 2401 \cdot 0.24 = 576.24 \Rightarrow n = 577$.

**Always round up** ($\lceil \cdot \rceil$) — a fractional unit does not meet the bound. Saving from prior info: $\sim 4\%$ when $\hat p$ is close to $0.5$, but can be huge for skewed $\hat p$ (e.g. $\hat p = 0.1$: $0.09/0.25 = 36\%$ of the worst-case sample).

```r
m  <- 0.04
z  <- qnorm(0.975)
n_wc    <- (z/m)^2 * 0.25
ceiling(n_wc)                      # 601
n_pilot <- (z/m)^2 * phat*(1-phat) # 0.24
ceiling(n_pilot)                   # 577
# Sanity at n=601, p=0.5:
qnorm(0.975) * sqrt(0.25/601)      # ~ 0.03997 < 0.04 OK
```

---

### Part (6) — Margin-of-error decomposition

The Wald CI can be written compactly as
$$\hat p \;\pm\; \underbrace{z_{1-\alpha/2}}_{\text{reliability factor}}\;\underbrace{\sqrt{\tfrac{\hat p(1-\hat p)}{n}}}_{SE(\hat p)} \;=\; \hat p \pm ME.$$

So the interval depends on **exactly three quantities**: point estimate $\hat p$, sample size $n$, confidence level $1-\alpha$ (which fixes $z$).

| Quantity | Symbol | Value (master, 95%) | R command |
|---|---|---|---|
| Sample size | $n$ | $500$ | `length(y)` |
| Point estimate | $\hat p$ | $0.40$ | `mean(y)` |
| Standard error | $SE = \sqrt{\hat p(1-\hat p)/n}$ | $0.02191$ | `sqrt(phat*(1-phat)/n)` |
| Reliability factor | $z_{0.975}$ | $1.96$ | `qnorm(0.975)` |
| Margin of error | $ME = z \cdot SE$ | $0.04294$ | half-width of CI |
| 95% CI | $\hat p \pm ME$ | $[0.357,\,0.443]$ | `CI.prop(..., conf.level=0.95)` |

**Quick recovery trick.** Read the CI off the RStudio output and **halve the width** to recover $ME$ without recomputing $SE$: $(0.443 - 0.357)/2 = 0.043$. Then $SE = ME/z$.

---

### Part (7) — One-sided lower confidence bound

For decisions of the form *"is $p$ at least $p_0$?"* a **one-sided** CI is more efficient than a two-sided one. The $(1-\alpha)$ lower confidence bound uses $z_{1-\alpha}$ (one-tail) instead of $z_{1-\alpha/2}$ (two-tail):
$$\big[\,\hat p - z_{1-\alpha}\,SE(\hat p),\;+\infty\big).$$

At 95% one-sided: $z_{0.95} = 1.6449$, so
$$L_{95\%}^{\text{one-sided}} \;=\; 0.40 - 1.6449 \cdot 0.02191 \;\approx\; 0.40 - 0.03604 \;=\; 0.364.$$
**Equivalence.** This $L$ is *exactly* the lower endpoint of the **two-sided 90% CI** of Part (4): the 90% two-sided level splits its $10\%$ miss-mass into $5\%$ per tail, and a one-sided 95% lower bound puts all $5\%$ in the upper tail — the lower endpoint is identical.

**Largest confidence at which $L > p_0$?** Suppose the retailer wants the lower bound above $p_0 = 0.36$. Solve $\hat p - z^{\star}\,SE = p_0$:
$$z^{\star} \;=\; \tfrac{\hat p - p_0}{SE} \;=\; \tfrac{0.40 - 0.36}{0.02191} \;\approx\; 1.826,$$
so $c^{\star} = 2\Phi(z^{\star}) - 1 \approx 2\cdot 0.9661 - 1 \approx 0.932$. The retailer can claim "$p > 0.36$" with two-sided confidence up to $\sim 93.2\%$; beyond that the interval widens past $0.36$.

```r
# One-sided 95% lower bound
L_one <- phat - qnorm(0.95) * se          # ~ 0.364
# Largest two-sided level with lower bound > 0.36
z_star <- (phat - 0.36)/se                 # ~ 1.826
c_star <- 2*pnorm(z_star) - 1              # ~ 0.932
```

---

### Part (8) — Validity check (CLT condition)

The Wald CI relies on the CLT approximation $\hat p \overset{a}{\sim} \mathcal{N}(p, p(1-p)/n)$. Rule of thumb:
$$n\hat p \;\ge\; 5 \quad\text{AND}\quad n(1-\hat p) \;\ge\; 5.$$

Master dataset: $n\hat p = 500\cdot 0.40 = 200 \ge 5$ and $n(1-\hat p) = 500\cdot 0.60 = 300 \ge 5$ — both **massively** satisfied, the CLT approximation is excellent.

**When does it fail?** Small $n$ combined with an extreme $\hat p$. Example: $n=30$ with $\hat p = 0.05$ gives $n\hat p = 1.5 < 5$ — Wald becomes unreliable (it can even produce endpoints outside $[0,1]$). In that case use **exact binomial** (`binom.test`) or the **Wilson** CI (`prop.test`), both of which work without the large-$n$ approximation.

```r
# Master check
n*phat;  n*(1-phat)                # 200 ; 300 -- both >> 5

# Counterexample (small n, extreme phat)
n_s <- 30; phat_s <- 0.05
n_s*phat_s;  n_s*(1-phat_s)        # 1.5 ; 28.5 -- FAILS the 5/5 rule
# Use exact instead:
binom.test(x = 1, n = 30)$conf.int
```

---

### Summary table (master dataset $n=500$, $\hat p = 0.40$)

| Quantity | Value | Where |
|---|---|---|
| $\hat p$ | $0.40$ | Part 1 |
| $\widehat{SE}$ | $0.02191$ | Part 1 |
| $ME_{95\%}$ | $0.04294$ | Part 2 |
| 95% CI | $[0.357,\,0.443]$ | Part 2 |
| 90% CI | $[0.364,\,0.436]$ | Part 4 |
| 99% CI | $[0.344,\,0.456]$ | Part 4 |
| $n$ for $ME \le 0.04$, worst case | $601$ | Part 5 |
| $n$ for $ME \le 0.04$, pilot $\hat p=0.4$ | $577$ | Part 5 |
| One-sided 95% lower bound | $0.364$ | Part 7 |
| CLT check $(n\hat p,\,n(1-\hat p))$ | $(200,\,300)$ — OK | Part 8 |

**Master take-aways.**
1. CI half-width = $ME = z_{1-\alpha/2}\cdot SE$; only **three** levers control it: $\hat p$, $n$, confidence.
2. Precision improves at rate $1/\sqrt n$ — **quadrupling** $n$ halves $ME$.
3. Higher confidence $\Rightarrow$ wider CI (linear in $z$).
4. Sample-size planning: $n \ge (z/m)^2\,p(1-p)$, with $p = 0.5$ as the safe upper bound; pilot $\hat p$ can save substantially when far from $0.5$.
5. One-sided $95\%$ lower bound coincides with the lower endpoint of the two-sided $90\%$ CI.
6. Always check $n\hat p \ge 5$ and $n(1-\hat p) \ge 5$ before trusting the Wald CI.

---

**Linked snippets:** Ex 5.6b, Ex 5.13 a3 (sample proportion + SE as unbiased estimator of $p$); Ex 6.1c (95% CI for proportion); Ex 6.3b1, Ex 6.3b2 (sample proportion + 99% CI with sample-size planning); Ex 6.6a–d (full sequence: 95% CI at $n=100$, $n=1000$, 90% level, sample size for $ME\le 0.04$); Ex 6.7a (CI + largest $c$ keeping lower bound above a threshold); Ex 6.12a, 6.12b (99% CI + ME decomposition with CLT check); Ex 6.13a (99% CI with CLT validity).
""",
    "images": ["statistics/images/master/master_g13b_ai.png"],
}


master_exercises["g13c_ci_diff_means"] = {
    "title": "Master Exam — CI for the difference of two independent means (consolidated)",
    "content": r"""**Setup.** A market-research firm collected `AmountSpent` (€/year) for a random sample of customers in dataset `DS`, split by marital status. The summary statistics are

| Group | $n$ | $\bar x$ (€) | $s$ (€) |
|---|---:|---:|---:|
| Married ($M$) | 205 | 1\,300 | 900 |
| Single ($S$)  | 295 | 1\,100 | 800 |

The two samples are **independent** (different customers). Let $\mu_M,\mu_S$ be the population mean spends and $\sigma_M^2,\sigma_S^2$ their variances. The parameter of interest is the **difference** $\delta := \mu_M - \mu_S$.

---

### Part (1) — Point estimate and interpretation

The natural unbiased estimator of $\delta = \mu_M - \mu_S$ is the **difference of sample means**
$$\hat\delta \;=\; \bar X_M - \bar X_S, \qquad \mathbb E[\hat\delta] \;=\; \mu_M - \mu_S \;=\; \delta$$
by linearity of expectation — no distributional assumption needed beyond i.i.d. sampling within each group. From the data,
$$\hat\delta \;=\; \bar x_M - \bar x_S \;=\; 1300 - 1100 \;=\; \mathbf{200\ \text{€}}.$$

*Interpretation.* On average, married customers in the sample spend **200 € more per year** than single ones. Whether this point estimate reflects a real population difference, or just sampling noise, is exactly what the CI will tell us.

```r
xbar_M <- 1300;  xbar_S <- 1100
n_M    <- 205;   n_S    <- 295
s_M    <- 900;   s_S    <- 800
delta_hat <- xbar_M - xbar_S;  delta_hat        # 200
```

---

### Part (2) — SE under **known** variances $\sigma_M,\sigma_S$

Because the samples are independent, variance **adds**:
$$\Var(\bar X_M - \bar X_S) \;=\; \Var(\bar X_M) + \Var(\bar X_S) \;=\; \frac{\sigma_M^2}{n_M} + \frac{\sigma_S^2}{n_S}.$$
If $\sigma_M = 900$ and $\sigma_S = 800$ were **known** (textbook case), the *exact* SE is
$$SE_{\text{known}} \;=\; \sqrt{\frac{900^2}{205} + \frac{800^2}{295}} \;=\; \sqrt{3951.22 + 2169.49} \;=\; \sqrt{6120.71} \;\approx\; \mathbf{78.235\ \text{€}}.$$
With known variances, inference uses **$z$-quantiles** (no $t$-table, no degrees of freedom).

```r
SE_known <- sqrt(900^2/n_M + 800^2/n_S);  SE_known    # 78.235
```

*Why no covariance term?* The two samples are **independent**, so $\Cov(\bar X_M,\bar X_S) = 0$. Pairing (Sec. g13d) is the case when this fails — then SE involves the variance of paired differences, not the sum.

---

### Part (3) — SE under **unknown** but **assumed equal** variances (pooled)

If the variances are unknown but we are willing to assume $\sigma_M^2 = \sigma_S^2 = \sigma^2$, we **pool** the two sample variances by their degrees of freedom:
$$s_p^2 \;=\; \frac{(n_M-1)\,s_M^2 + (n_S-1)\,s_S^2}{n_M + n_S - 2} \;=\; \frac{204\cdot 810000 + 294\cdot 640000}{498} \;=\; \frac{353\,400\,000}{498} \;\approx\; \mathbf{709\,638.55}.$$
Then
$$\widehat{SE}_{\text{pool}} \;=\; \sqrt{s_p^2 \left(\frac{1}{n_M} + \frac{1}{n_S}\right)} \;=\; \sqrt{709638.55 \cdot (0.00488 + 0.00339)} \;=\; \sqrt{5867.3} \;\approx\; \mathbf{76.598\ \text{€}}.$$
Inference uses $t$ with **$df_{\text{pool}} = n_M + n_S - 2 = 498$**.

```r
s2_pool <- ((n_M-1)*s_M^2 + (n_S-1)*s_S^2) / (n_M + n_S - 2);  s2_pool   # 709638.55
SE_pool <- sqrt(s2_pool * (1/n_M + 1/n_S));                    SE_pool   # 76.598
df_pool <- n_M + n_S - 2;                                       df_pool  # 498
```

The pooled **variance** $s_p^2$ is a weighted average of the two within-group sample variances (weights $= n_i - 1$); the pooled SE is then $\sqrt{s_p^2(1/n_M+1/n_S)}$. Pooled is **more efficient** than Welch *when* the equal-variance assumption is correct (smaller SE, narrower CI), at the cost of being **biased** if the assumption fails.

---

### Part (4) — SE under **unknown unequal** variances (Welch)

If we are *not* willing to assume equal variances, plug in each sample variance separately:
$$\widehat{SE}_{W} \;=\; \sqrt{\frac{s_M^2}{n_M} + \frac{s_S^2}{n_S}} \;=\; \sqrt{\frac{810000}{205} + \frac{640000}{295}} \;=\; \sqrt{6120.71} \;\approx\; \mathbf{78.235\ \text{€}}.$$

This is **numerically identical** to the known-$\sigma$ SE of Part (2), because the *formula* is the same — only the interpretation differs (plug-in vs exact). The price for not assuming equality is paid in the **degrees of freedom** via the **Satterthwaite** approximation:
$$df_W \;=\; \frac{\left(\dfrac{s_M^2}{n_M} + \dfrac{s_S^2}{n_S}\right)^{2}}{\dfrac{(s_M^2/n_M)^2}{n_M-1} + \dfrac{(s_S^2/n_S)^2}{n_S-1}} \;=\; \frac{6120.71^2}{\dfrac{3951.22^2}{204} + \dfrac{2169.49^2}{294}} \;\approx\; \frac{37\,463\,103}{92\,539} \;\approx\; \mathbf{404.8}.$$

Welch's $df$ is **fractional** and always satisfies $\min(n_M-1,n_S-1) \le df_W \le n_M+n_S-2$. Here $df_W \approx 405 < df_{\text{pool}} = 498$: Welch "spends" some degrees of freedom for the right to drop the equal-variance assumption. With $df_W \approx 405$, the $t$-quantile is essentially the $z$-quantile (the cost is invisible at large $n$).

```r
SE_W <- sqrt(s_M^2/n_M + s_S^2/n_S);  SE_W                                # 78.235
num   <- (s_M^2/n_M + s_S^2/n_S)^2
den   <- (s_M^2/n_M)^2/(n_M-1) + (s_S^2/n_S)^2/(n_S-1)
df_W  <- num / den;                    df_W                               # 404.8
```

---

### Part (5) — Pooled df vs Welch df at a glance

| Quantity | Pooled (equal var.) | Welch (unequal var.) |
|---|---:|---:|
| $\widehat{SE}$ | $\sqrt{s_p^2(1/n_M+1/n_S)}$ = **76.598** | $\sqrt{s_M^2/n_M + s_S^2/n_S}$ = **78.235** |
| $df$ | $n_M + n_S - 2 = $ **498** (integer) | Satterthwaite $\approx$ **404.8** (fractional) |
| $t$-quantile (95%) | $t_{0.975,498} \approx 1.965$ | $t_{0.975,405} \approx 1.966$ |
| Required assumption | $\sigma_M^2 = \sigma_S^2$ | none on variance ratio |
| If equality holds | more efficient (narrower CI) | slightly conservative |
| If equality fails | biased SE, wrong coverage | unaffected |

Difference here: pooled SE is **2% smaller** than Welch's. With nearly balanced sample sizes the two are very close; the gap widens when both $n$'s and variances are unbalanced.

---

### Part (6) — 95% confidence interval for $\delta$

The two-sided CI for $\delta$ at level $1-\alpha$ is
$$(\bar x_M - \bar x_S) \;\pm\; t_{1-\alpha/2,\,df}\;\widehat{SE}.$$

**Welch (default).** $t_{0.975,\,405} \approx 1.966$:
$$ME_{95}^{W} \;=\; 1.966 \cdot 78.235 \;\approx\; 153.81, \qquad CI_{95}^{W} \;=\; 200 \pm 153.81 \;=\; \mathbf{[46.19,\; 353.81]}.$$

**Pooled.** $t_{0.975,\,498} \approx 1.965$:
$$ME_{95}^{\text{pool}} \;=\; 1.965 \cdot 76.598 \;\approx\; 150.51, \qquad CI_{95}^{\text{pool}} \;=\; 200 \pm 150.51 \;=\; \mathbf{[49.49,\; 350.51]}.$$

```r
alpha <- 0.05
# Welch
tW   <- qt(1 - alpha/2, df = df_W);   tW                  # 1.966
MEW  <- tW * SE_W;                    MEW                 # 153.81
c(delta_hat - MEW, delta_hat + MEW)                       # [46.19, 353.81]

# Pooled
tP   <- qt(1 - alpha/2, df = df_pool); tP                 # 1.965
MEP  <- tP * SE_pool;                  MEP                # 150.51
c(delta_hat - MEP, delta_hat + MEP)                       # [49.49, 350.51]

# Same numbers from raw data:
t.test(AmountSpent ~ MaritalStatus, data = DS, var.equal = FALSE)$conf.int  # Welch
t.test(AmountSpent ~ MaritalStatus, data = DS, var.equal = TRUE )$conf.int  # Pooled
```

*Interpretation of the 95%.* Under repeated sampling, about 95% of intervals built this way would contain the true mean difference $\mu_M - \mu_S$. For *this* one realised interval we cannot speak of probability — $\delta$ is fixed, either inside or outside.

![Master illustration](statistics/images/master/master_g13c_ai.png)

---

### Part (7) — 99% confidence interval for $\delta$

Only the reliability factor changes; SE is identical.

**Welch.** $t_{0.995,\,405} \approx 2.587$:
$$ME_{99}^{W} \;=\; 2.587 \cdot 78.235 \;\approx\; 202.40, \qquad CI_{99}^{W} \;\approx\; 200 \pm 202.40 \;=\; \mathbf{[-2.40,\; 402.40]}.$$

**Pooled.** $t_{0.995,\,498} \approx 2.586$:
$$ME_{99}^{\text{pool}} \;=\; 2.586 \cdot 76.598 \;\approx\; 198.08, \qquad CI_{99}^{\text{pool}} \;\approx\; 200 \pm 198.08 \;=\; \mathbf{[1.92,\; 398.08]}.$$

The 99% Welch interval is **about 32% wider** than the 95% one ($2.587/1.966 \approx 1.316$) — pure consequence of the higher confidence demand, not of the data.

```r
qt(0.995, df = df_W)   * SE_W       # 202.40   -> Welch ME at 99%
qt(0.995, df = df_pool)* SE_pool    # 198.08   -> Pooled ME at 99%
```

---

### Part (8) — Does the CI contain $0$?

The CI is the set of values of $\delta = \mu_M - \mu_S$ that are *compatible* with the data at the stated confidence. Checking whether **$0 \in CI$** is the CI's most-used decision tool:

| CI | Bounds | Contains $0$? | Conclusion at level $\alpha$ |
|---|---|:---:|---|
| 95% Welch | $[46.19,\,353.81]$ | **No** | reject $H_0: \mu_M = \mu_S$ at 5% — evidence of a difference |
| 95% Pooled | $[49.49,\,350.51]$ | **No** | same conclusion, slightly stronger |
| 99% Welch | $[-2.40,\,402.40]$ | **Yes** (barely) | cannot reject at 1% — evidence not strong enough |
| 99% Pooled | $[1.92,\,398.08]$ | **No** (just) | reject at 1%, by a hair |

This is the **CI–test duality**: $0 \notin CI_{1-\alpha}$ ⇔ $H_0: \delta = 0$ rejected at level $\alpha$. The borderline behaviour at 99% (Welch contains 0, pooled does not) is a small reminder that pooled and Welch can give different qualitative answers when the test is close to the threshold — another reason Welch is the safer default.

---

### Part (9) — Pooled vs Welch: choosing in practice

| Situation | Use |
|---|---|
| Variances assumed/known equal, balanced samples | Pooled (slight efficiency gain) |
| Variances unknown, $\max(s_1^2,s_2^2)/\min(s_1^2,s_2^2) \le 2$, balanced $n$ | Either — virtually identical |
| Variances clearly different ratio $> 3$ | **Welch** (mandatory) |
| Sample sizes very unbalanced ($n_1 \gg n_2$) | **Welch** — pooled is dangerously biased |
| Default in R (`t.test`) | **Welch** (`var.equal=FALSE`) |

**Rule of thumb (Casella–Berger).** When the **larger** variance is in the **larger** sample, pooled **over-states** the true SE (conservative — intervals too wide, but coverage $\ge$ nominal). When the larger variance is in the **smaller** sample, pooled **under-states** the SE (anti-conservative — intervals too narrow, coverage $<$ nominal). Since you usually do not know which regime you are in, *use Welch unless you have a strong reason*. Here $s_M^2/s_S^2 = 810000/640000 = 1.266 < 2$ — within the safe band, both procedures agree.

```r
ratio <- max(s_M^2, s_S^2) / min(s_M^2, s_S^2);  ratio    # 1.266 -- safe
# Formal test (Levene/F) only if needed:
var.test(AmountSpent ~ MaritalStatus, data = DS)$p.value
```

---

### Summary table (master dataset, $\hat\delta = 200$ €)

| Quantity | Pooled (eq. var.) | Welch (uneq. var.) | Known $\sigma$ ($z$) |
|---|---:|---:|---:|
| $\widehat{SE}$ | $76.598$ | $78.235$ | $78.235$ |
| $df$ | $498$ | $\approx 404.8$ | — ($z$) |
| $t_{0.975}$ or $z_{0.975}$ | $1.965$ | $1.966$ | $1.960$ |
| $ME_{95}$ | $150.51$ | $153.81$ | $153.34$ |
| **95% CI** | $[49.49,\,350.51]$ | $[46.19,\,353.81]$ | $[46.66,\,353.34]$ |
| $t_{0.995}$ or $z_{0.995}$ | $2.586$ | $2.587$ | $2.576$ |
| $ME_{99}$ | $198.08$ | $202.40$ | $201.54$ |
| **99% CI** | $[1.92,\,398.08]$ | $[-2.40,\,402.40]$ | $[-1.54,\,401.54]$ |

**Master take-aways.**
1. The point estimate of $\mu_1 - \mu_2$ is $\bar x_1 - \bar x_2$ (unbiased, independent samples).
2. **Variance adds**: $\Var(\bar X_1 - \bar X_2) = \sigma_1^2/n_1 + \sigma_2^2/n_2$ because samples are independent (covariance = 0).
3. Three SE variants: **known $\sigma$** (exact, $z$), **pooled** ($t$, $df = n_1+n_2-2$, assumes equal var.), **Welch** ($t$, Satterthwaite $df$, no equal-var. assumption).
4. Pooled $s_p^2$ = weighted average of the two within-group sample variances (weights $= n_i - 1$); Welch SE keeps the per-group variances separate ($\sqrt{s_1^2/n_1 + s_2^2/n_2}$); numerically the Welch SE equals the known-$\sigma$ SE when one plugs $s$ in for $\sigma$.
5. $0 \in CI$ ⇔ "no significant difference at level $\alpha$" — the CI–test duality.
6. **Default = Welch.** Use pooled only when the equal-variance assumption is justified; it is anti-conservative when the larger variance sits in the smaller sample.

---

**Linked snippets:** Ex 5.1f (SE of difference: known / unknown equal / unknown unequal variances); Ex 5.6a (pooled vs Welch SE for AmountSpent by Sex); Ex 5.7a, Ex 5.7b (pizzeria price difference under known equal, known unequal, and unknown variances); Ex 6.3d (99% CI for AmountSpent, Married vs Single, Close × 0 children); Ex 6.4a (pooled-variance CI for vgsales mean NF vs F); Ex 6.8 a2 (pooled CI: NrSkills GER vs ITA); Ex 6.8b (Welch SE for unequal variances); Ex 6.8d (CI for Skills mean Full-time vs Freelance); Ex 6.10a (95% CI for difference of means); Ex 6.15a, Ex 6.15b (CI for difference of two means and reading whether it contains zero).
""",
    "images": ["statistics/images/master/master_g13c_ai.png"],
}



master_exercises["g13d_ci_diff_prop"] = {
    "title": "Master Exam — CI for difference of two proportions (consolidated)",
    "content": r"""**Master dataset.** A bookstore chain interviews two **independent** random samples of customers and asks whether each one bought $\ge 2$ books in the last year ("heavy reader"). The samples are stratified by sex:

| Group | Sample size | Heavy readers | Sample proportion |
|---|---|---|---|
| Male ($M$) | $n_M = 850$ | $x_M = 650$ | $\hat p_M = 650/850 \approx 0.7647$ |
| Female ($F$) | $n_F = 650$ | $x_F = 391$ | $\hat p_F = 391/650 \approx 0.6015$ |

The two samples are drawn **independently** (different individuals), so $\operatorname{Cov}(\hat p_M,\hat p_F)=0$. Let $p_M, p_F$ denote the population proportions of heavy readers among male / female customers respectively. We will build CIs for the difference $\delta \equiv p_M - p_F$, then interpret what the interval tells us about whether the two proportions can plausibly be equal.

This master exercise consolidates the seven sub-topics of `g13d_ci_diff_prop` (CI for difference of two proportions):
1. Sample proportions and point estimate of the difference.
2. Standard error of $\hat p_M - \hat p_F$ (variances add under independence).
3. Two-sided 95% Wald CI for $p_M - p_F$.
4. Two-sided 99% CI and comparison with part (3).
5. Does the CI contain $0$? Interpretation in terms of a plausible-null gap.
6. One-sided lower confidence bound for $p_M - p_F$.
7. Validity check (CLT condition $n_i\hat p_i \ge 5$ AND $n_i(1-\hat p_i) \ge 5$).

---

### Part (1) — Sample proportions and point estimate of the gap

Each sample is a sequence of i.i.d. Bernoulli draws. The unbiased estimator of $p_i$ is the sample proportion $\hat p_i = X_i/n_i$, and the natural unbiased estimator of $\delta = p_M - p_F$ is the **difference of sample proportions**
$$\hat\delta \;=\; \hat p_M - \hat p_F, \qquad \mathbb E[\hat\delta] = p_M - p_F = \delta.$$
Plug in the counts:
$$\hat p_M = \tfrac{650}{850} \approx 0.7647, \qquad \hat p_F = \tfrac{391}{650} \approx 0.6015, \qquad \hat\delta \approx 0.7647 - 0.6015 = 0.1632.$$

So in the sample, the male heavy-reader rate exceeds the female rate by about **16.3 percentage points**. Whether this gap reflects a real population difference (rather than sampling noise) is exactly what the CI in parts (3)–(5) decides.

```r
# Sample sizes and success counts
n_M <- 850;  x_M <- 650
n_F <- 650;  x_F <- 391

phat_M <- x_M / n_M;  phat_M       # 0.7647
phat_F <- x_F / n_F;  phat_F       # 0.6015
dhat   <- phat_M - phat_F;  dhat   # 0.1632
```

---

### Part (2) — Standard error of $\hat p_M - \hat p_F$

Under independence, the variance of a difference equals the sum of the variances:
$$\operatorname{Var}(\hat p_M - \hat p_F) \;=\; \operatorname{Var}(\hat p_M) + \operatorname{Var}(\hat p_F) \;=\; \frac{p_M(1-p_M)}{n_M} + \frac{p_F(1-p_F)}{n_F}.$$
There is **no covariance term** because the two samples are drawn from disjoint groups — this is the structural simplification that the paired case (g13e) does not enjoy. Since the population $p_i$ are unknown, plug in the sample proportions to get the *estimated* SE:
$$\widehat{SE}(\hat\delta) \;=\; \sqrt{\frac{\hat p_M(1-\hat p_M)}{n_M} + \frac{\hat p_F(1-\hat p_F)}{n_F}}.$$

Numerically:
$$\frac{0.7647\cdot 0.2353}{850} \approx 2.117\cdot 10^{-4}, \qquad \frac{0.6015\cdot 0.3985}{650} \approx 3.688\cdot 10^{-4},$$
$$\widehat{SE}(\hat\delta) \;=\; \sqrt{2.117\cdot 10^{-4} + 3.688\cdot 10^{-4}} \;=\; \sqrt{5.805\cdot 10^{-4}} \;\approx\; 0.02409.$$

**Why the female group contributes a larger variance term.** $\hat p_F \approx 0.60$ is closer to $0.5$ than $\hat p_M \approx 0.76$, so $\hat p_F(1-\hat p_F) \approx 0.240$ versus $\hat p_M(1-\hat p_M) \approx 0.180$ — and at the same time $n_F = 650 < n_M = 850$. Both effects push the female contribution up.

```r
SE_diff <- sqrt( phat_M*(1-phat_M)/n_M + phat_F*(1-phat_F)/n_F )
SE_diff                            # ~ 0.02409
```

---

### Part (3) — Two-sided 95% Wald CI for $p_M - p_F$

By the CLT, for large $n_M, n_F$,
$$\frac{\hat\delta - \delta}{\widehat{SE}(\hat\delta)} \;\overset{a}{\sim}\; \mathcal{N}(0,1) \quad\Longrightarrow\quad \delta \in \hat\delta \pm z_{1-\alpha/2}\,\widehat{SE}(\hat\delta) \text{ with probability } 1-\alpha.$$
At 95% confidence, $z_{0.975} \approx 1.96$, so
$$ME_{95\%} \;=\; 1.96\cdot 0.02409 \;\approx\; 0.04722,$$
$$CI_{95\%}(\delta) \;=\; 0.1632 \pm 0.04722 \;=\; [0.1159,\,0.2104].$$

**Interpretation.** With 95% confidence, the true gap $p_M - p_F$ lies between **11.6 and 21.0 percentage points** in favour of males. The interval sits **entirely above 0** — equality of the two population proportions is rejected at the 5% level.

```r
z95 <- qnorm(0.975);  z95          # 1.95996
ME95 <- z95 * SE_diff;  ME95       # ~ 0.04722
CI95 <- c(dhat - ME95, dhat + ME95)
CI95                               # ~ [0.1159, 0.2104]

# Same via course helper if available
# CI.diffprop(x = sample_M, y = sample_F, success = "heavy",
#             conf.level = 0.95)
```

![Master illustration](statistics/images/master/master_g13d_ai.png)

---

### Part (4) — Two-sided 99% CI and comparison with 95%

Raising the confidence level only changes the reliability factor $z_{1-\alpha/2}$ (point estimate and SE are unchanged):
$$z_{0.995} \approx 2.5758, \qquad ME_{99\%} \;=\; 2.5758\cdot 0.02409 \;\approx\; 0.06205,$$
$$CI_{99\%}(\delta) \;=\; 0.1632 \pm 0.06205 \;=\; [0.1011,\,0.2252].$$

Width comparison:

| Level | $z$ | ME | CI | Width |
|---|---|---|---|---|
| 95% | $1.960$ | $0.0472$ | $[0.1159,\,0.2104]$ | $0.0944$ |
| 99% | $2.576$ | $0.0621$ | $[0.1011,\,0.2252]$ | $0.1241$ |

The 99% CI is wider by a factor $2.576/1.960 \approx 1.314$ — **higher confidence costs precision linearly in $z$**. Both intervals still exclude 0, so the conclusion of part (3) is robust to a stricter confidence level: even at 99%, the gap cannot plausibly be 0 or favour females.

```r
z99  <- qnorm(0.995);  z99         # 2.5758
ME99 <- z99 * SE_diff
CI99 <- c(dhat - ME99, dhat + ME99)
CI99                               # ~ [0.1011, 0.2252]
```

---

### Part (5) — Does the CI contain 0? Plausible-null interpretation

The reasoning is the duality between CIs and two-sided tests of $H_0: \delta = 0$:
$$0 \in CI_{1-\alpha}(\delta) \;\iff\; H_0\!: p_M = p_F \text{ is NOT rejected at level } \alpha.$$

In our master dataset $0 \notin [0.1159, 0.2104]$, so the hypothesis "males and females buy heavy at the same rate" is **rejected at the 5% level**, and (since $0 \notin [0.1011, 0.2252]$) also **at the 1% level**. The data give strong evidence of a structural difference in reading behaviour by sex.

**A counter-example.** If the female-sample success count had instead been $x_F = 480$ out of $n_F = 650$, we would get $\hat p_F \approx 0.7385$, $\hat\delta \approx 0.7647 - 0.7385 = 0.0262$, $\hat p_F(1-\hat p_F)/n_F \approx 0.7385\cdot 0.2615/650 \approx 2.972\cdot 10^{-4}$, hence $\widehat{SE} = \sqrt{2.117\cdot 10^{-4} + 2.972\cdot 10^{-4}} \approx 0.02256$, and the 95% CI $0.0262\pm 1.96\cdot 0.02256 = 0.0262 \pm 0.0442 = [-0.018,\,0.070]$ — now **containing 0**. The same procedure would conclude: "the data are compatible with $p_M = p_F$ at 5%". Whether 0 is inside the CI is the single most-cited summary of a two-sample proportion comparison.

```r
# Plausible-null check (master dataset)
0 >= CI95[1] & 0 <= CI95[2]          # FALSE -> reject H0 at 5%
0 >= CI99[1] & 0 <= CI99[2]          # FALSE -> reject H0 at 1%
```

---

### Part (6) — One-sided lower confidence bound

If the bookstore only cares whether **male** heavy-reader share exceeds the female one (decisions like "should we tilt marketing budget toward male customers?"), a **one-sided** lower bound is more informative than a two-sided CI: it puts the entire miss-mass $\alpha$ into the upper tail of $\hat\delta$, so the lower endpoint is **higher** than the two-sided one at the same nominal level.

At 95% one-sided confidence, $z_{0.95} = 1.6449$, so
$$L_{95\%}^{\text{one-sided}} \;=\; \hat\delta - z_{0.95}\,\widehat{SE}(\hat\delta) \;=\; 0.1632 - 1.6449\cdot 0.02409 \;\approx\; 0.1632 - 0.03963 \;=\; 0.1236.$$
**Statement.** "With 95% confidence, $p_M - p_F \ge 0.124$" — i.e., we are at least 95% confident the male share exceeds the female share by **at least 12.4 percentage points**.

**Equivalence with the two-sided 90% CI.** The two-sided 90% CI puts $5\%$ in each tail, so its lower endpoint is identical to the one-sided 95% lower bound:
$$L_{90\%}^{\text{two-sided}} \;=\; \hat\delta - z_{0.95}\,\widehat{SE}(\hat\delta) \;=\; 0.1236 \quad\checkmark$$
This identity is a useful sanity check on R output: read the lower endpoint off a 90% two-sided CI to recover the 95% one-sided lower bound for free.

```r
L_one <- dhat - qnorm(0.95) * SE_diff;  L_one      # ~ 0.1236
# Cross-check via 90% two-sided
ME90 <- qnorm(0.95) * SE_diff
c(dhat - ME90, dhat + ME90)                        # lower endpoint matches L_one
```

---

### Part (7) — Validity check (CLT condition)

The Wald CI relies on the joint normal approximation of $(\hat p_M, \hat p_F)$. Rule of thumb: for **each** sample,
$$n_i \hat p_i \ge 5 \quad\text{AND}\quad n_i (1-\hat p_i) \ge 5.$$

Master dataset:

| Group | $n_i$ | $\hat p_i$ | $n_i\hat p_i$ | $n_i(1-\hat p_i)$ | OK? |
|---|---|---|---|---|---|
| Male | $850$ | $0.7647$ | $650$ | $200$ | yes (both $\gg 5$) |
| Female | $650$ | $0.6015$ | $391$ | $259$ | yes (both $\gg 5$) |

All four counts are in the hundreds — the normal approximation is **excellent**, and the Wald CI is fully trustworthy here.

**When does it fail?** Very small $n_i$ combined with an extreme $\hat p_i$ (close to 0 or 1). In that regime, use exact methods such as the Wilson or Agresti–Coull CI for each proportion, or `prop.test` for the difference, both of which behave better in the tails. In our master dataset there is no need.

```r
# Master check (all four >= 5 ?)
n_M * phat_M;  n_M * (1 - phat_M)      # 650 ; 200
n_F * phat_F;  n_F * (1 - phat_F)      # 391 ; 259  -- all >> 5
```

---

### Summary table (master dataset)

| Quantity | Value | Where |
|---|---|---|
| $\hat p_M$ | $0.7647$ | Part 1 |
| $\hat p_F$ | $0.6015$ | Part 1 |
| $\hat\delta = \hat p_M - \hat p_F$ | $0.1632$ | Part 1 |
| $\widehat{SE}(\hat\delta)$ | $0.02409$ | Part 2 |
| 95% CI for $\delta$ | $[0.1159,\,0.2104]$ | Part 3 |
| 99% CI for $\delta$ | $[0.1011,\,0.2252]$ | Part 4 |
| Is $0 \in$ CI? | No (at 95% or 99%) | Part 5 |
| One-sided 95% lower bound | $0.1236$ | Part 6 |
| CLT counts $(n_M\hat p_M,n_M(1{-}\hat p_M),n_F\hat p_F,n_F(1{-}\hat p_F))$ | $(650,\,200,\,391,\,259)$ — OK | Part 7 |

**Master take-aways.**
1. With independent samples, $\widehat{SE}(\hat p_1 - \hat p_2) = \sqrt{\hat p_1(1-\hat p_1)/n_1 + \hat p_2(1-\hat p_2)/n_2}$ — variances add, no covariance.
2. Wald CI: $(\hat p_1 - \hat p_2) \pm z_{1-\alpha/2}\,\widehat{SE}$ — same skeleton as the one-proportion CI, only the SE changes.
3. Raising confidence widens the CI linearly in $z$; point estimate and SE do not move.
4. CI contains $0$ $\iff$ two-sided test of $p_1 = p_2$ is not rejected at level $\alpha$. In our data the CI excludes 0 at both 5% and 1% — strong evidence of a sex gap.
5. One-sided 95% lower bound = lower endpoint of the two-sided 90% CI — use this trick to read it off any 90% CI.
6. Validity needs **four** counts $\ge 5$: $n_i\hat p_i$ and $n_i(1-\hat p_i)$ for $i=1,2$. Easily satisfied here.

---

**Linked snippets:** Ex 5.5a (proportions + SE for $\hat p_F - \hat p_M$ in the bookstore survey); Ex 5.5b (change in heavy-reader share 2015 vs 2022, same SE-of-difference logic across two independent cohorts); Ex 5.13b (point estimate + SE for the Milano vs Pavia proportion gap); Ex 6.3c (99% CI for diff in employment proportions GER vs ITA with CLT validity check); Ex 6.6e (99% CI for difference of two proportions, full Wald skeleton); Ex 6.9a (Wald CI for proportion difference, vgsales action genre); Ex 6.14a (90% CI for diff in best-seller proportions EA vs Activision).
""",
    "images": ["statistics/images/master/master_g13d_ai.png"],
}


master_exercises["g13e_ci_paired"] = {
    "title": "Master Exam — CI for paired mean (consolidated)",
    "content": r"""**Setup.** A retail chain runs a targeted **TV-and-online advertising campaign** and wants to quantify its effect on store-level sales. A random sample of $n=23$ stores is selected; for each store the manager records the **weekly sales** (in €1000s) over two periods of equal length:

- $X_i$ = sales in the week **before** the campaign,
- $Y_i$ = sales in the week **after** the campaign.

Because each store contributes *both* a "before" and an "after" value, the two columns are **matched by store** — i.e. they are *paired*, not independent. Define the within-store difference
$$D_i \;=\; Y_i - X_i \;=\; \text{Sales}_{\text{after},i} - \text{Sales}_{\text{before},i}.$$

The chain's data analyst summarises the $n=23$ differences as
$$\bar d \;=\; 10.1, \qquad s_d \;=\; 4.2, \qquad n \;=\; 23.$$
*(Individual standard deviations $s_X, s_Y$ around 8.0 with strong within-store correlation $\rho \approx 0.86$ — see Part (e).)*

For most of this exercise the population variances are **unknown** and estimated from $s_d$. The goal is to estimate the population mean change $\mu_D = \mu_Y - \mu_X$ and decide whether the campaign **moved the needle**.

---

### Part (a) — Why paired? Same unit, twice.

Each store has its own intrinsic level (location, footfall, mix of products), and **that** store-level heterogeneity is the dominant source of variation in raw weekly sales. If we treated the 23 "before" values and the 23 "after" values as two **independent** samples we would inherit *all* of that between-store noise in both $\bar X$ and $\bar Y$, and the SE of $\bar Y - \bar X$ would balloon.

The paired design avoids it. By **subtracting within store**, $D_i = Y_i - X_i$, the store-specific level cancels and only the *within-store change* survives. Formally, if we model $X_i = \mu_X + u_i + \varepsilon_i^X$ and $Y_i = \mu_Y + u_i + \varepsilon_i^Y$ with $u_i$ the store effect, then
$$D_i \;=\; (\mu_Y - \mu_X) + (\varepsilon_i^Y - \varepsilon_i^X),$$
and the noisy $u_i$ has been **subtracted out**. The estimator
$$\bar D \;=\; \frac{1}{n}\sum_{i=1}^n D_i \;=\; \bar Y - \bar X$$
is unbiased for $\mu_D = \mu_Y - \mu_X$ and inherits only the residual within-store variance.

**Assumptions for inference.** The pairs $(X_i, Y_i)$ are i.i.d. across stores (random sample). For small $n=23$ we additionally need $D_i$ approximately **normal** so that $\bar D$ has a $t_{n-1}$ pivot; with larger $n$ the CLT would cover this automatically.

---

### Part (b) — Compute $\bar d$ and $s_d$ from the differences.

Given the summary statistics:
$$\bar d \;=\; \bar y - \bar x \;=\; 10.1 \quad (\text{€1000s per week}), \qquad s_d \;=\; 4.2.$$

A few sanity remarks:
1. **Sign.** $\bar d > 0$ means post-campaign weekly sales are on average **higher** than pre-campaign — the direction matches the chain's hope.
2. **$s_d$ is the spread of the *within-store* differences**, *not* of raw sales. A store with weekly sales fluctuating by $\pm 8$ around its mean can still have a very *stable* week-to-week change of, say, $+10 \pm 4$. That's exactly what pairing exploits.
3. **No covariance term in $s_d$ once differences are formed.** If instead we had $s_X$ and $s_Y$ separately, we'd recover $s_d^2$ via $s_d^2 = s_X^2 + s_Y^2 - 2\,s_{XY}$ (see Part (e)).

```r
n     <- 23
dbar  <- 10.1          # mean of the within-store differences
sd_d  <- 4.2           # sample SD of the differences
```

---

### Part (c) — Standard error of the paired mean.

Treat $\bar D$ as a one-sample mean on the *differences*. The estimated standard error is
$$\widehat{SE}_{\text{paired}}(\bar D) \;=\; \frac{s_d}{\sqrt n} \;=\; \frac{4.2}{\sqrt{23}} \;=\; \frac{4.2}{4.7958} \;\approx\; \boxed{0.876}.$$

That's the **only** SE we need: once $D_i$ is formed, the problem is *literally* a one-sample CI for $\mu_D$.

```r
se_paired <- sd_d / sqrt(n);   se_paired         # ~ 0.876
```

---

### Part (d) — Why the paired SE is so small: $\Var(X-Y) = \Var(X)+\Var(Y) - 2\Cov(X,Y)$.

To see the *mechanism* of the SE shrinkage, expand the variance of the difference of two random variables:
$$\Var(X - Y) \;=\; \Var(X) + \Var(Y) - 2\,\Cov(X, Y) \;=\; \sigma_X^2 + \sigma_Y^2 - 2\rho\,\sigma_X\sigma_Y.$$

Three regimes for the same $\sigma_X, \sigma_Y$:

| $\rho$ | Variance of $X-Y$ | Comment |
|---|---|---|
| $\;\;\rho = 0$ (independent) | $\sigma_X^2 + \sigma_Y^2$ | textbook independent-samples baseline |
| $\;\;\rho > 0$ (paired, positive) | $\sigma_X^2 + \sigma_Y^2 - 2\rho\sigma_X\sigma_Y$ | **shrinks** the variance |
| $\;\;\rho = 1$ (perfectly aligned) | $(\sigma_X - \sigma_Y)^2$ | shrinks to (almost) zero |

In our data, $\rho \approx 0.86$ between pre- and post-campaign weekly sales: high-traffic stores stay high-traffic, low-traffic stays low — so most of $\sigma_X^2$ is **cancelled** by $2\rho\sigma_X\sigma_Y$ in the variance of the difference. That cancellation **is** the paired advantage.

Equivalently, on the *mean* of differences,
$$\Var(\bar D) \;=\; \frac{\sigma_X^2 + \sigma_Y^2 - 2\rho\sigma_X\sigma_Y}{n}.$$

---

### Part (e) — Paired vs wrong-independent SE on this dataset.

Suppose (for the comparison) $s_X \approx s_Y \approx 8.0$ (typical store-level week-to-week SD) with sample covariance $s_{XY}$ implied by $s_d^2 = s_X^2 + s_Y^2 - 2 s_{XY}$:
$$s_d^2 = 4.2^2 = 17.64, \qquad s_X^2 + s_Y^2 = 64 + 64 = 128 \;\;\Rightarrow\;\; s_{XY} = \tfrac{128 - 17.64}{2} \approx 55.18,$$
giving sample correlation $\hat\rho = s_{XY}/(s_X s_Y) = 55.18/64 \approx 0.862$. Now compare:

| SE flavour | Formula | Value | Penalty |
|---|---|---|---|
| **Paired** (correct) | $s_d/\sqrt n = 4.2/\sqrt{23}$ | $0.876$ | $\times 1$ baseline |
| **Independent** (wrong) | $\sqrt{(s_X^2+s_Y^2)/n} = \sqrt{128/23}$ | $2.359$ | $\times 2.69$ |

The wrong analysis would have inflated the SE by **a factor of 2.7** — and the CIs by the same factor (next part). That's the cost of forgetting the design.

```r
sx <- 8.0;  sy <- 8.0
# Recover the implied covariance and correlation
sxy <- ( (sx^2 + sy^2) - sd_d^2 ) / 2;   sxy        # ~ 55.18
rho <- sxy / (sx * sy);                  rho        # ~ 0.862
# Wrong independent SE for the same data
se_indep <- sqrt( (sx^2 + sy^2) / n );   se_indep   # ~ 2.359
se_indep / se_paired                                 # ~ 2.69
```

---

### Part (f) — $t$-CI: 90%, 95%, 99%.

For small $n=23$ and unknown variance, the pivot is
$$T \;=\; \frac{\bar D - \mu_D}{s_d/\sqrt n} \;\sim\; t_{n-1} \;=\; t_{22}.$$
The two-sided $(1-\alpha)$ CI is
$$\bar d \;\pm\; t_{1-\alpha/2,\,22}\,\frac{s_d}{\sqrt n}, \qquad SE_{\text{paired}} = 0.876.$$

| Level | $\alpha/2$ | $t_{1-\alpha/2,\,22}$ | $ME = t\cdot SE$ | CI for $\mu_D$ |
|---|---|---|---|---|
| 90% | 0.05  | 1.7171 | $1.7171\cdot 0.876 \approx 1.504$ | $[\,8.596,\;11.604\,]$ |
| 95% | 0.025 | 2.0739 | $2.0739\cdot 0.876 \approx 1.816$ | $[\,8.284,\;11.916\,]$ |
| 99% | 0.005 | 2.8188 | $2.8188\cdot 0.876 \approx 2.469$ | $[\,7.631,\;12.569\,]$ |

Higher confidence ⇒ larger $t$ ⇒ wider CI, but **all three intervals lie entirely above zero** (a clean rejection of "no effect" at all three levels).

```r
dbar    <- 10.1;   sd_d <- 4.2;   n <- 23
se      <- sd_d / sqrt(n);   se                     # ~ 0.876
t90 <- qt(0.95, df = n-1);   t90                    # 1.7171
t95 <- qt(0.975, df = n-1);  t95                    # 2.0739
t99 <- qt(0.995, df = n-1);  t99                    # 2.8188

c(dbar - t90*se, dbar + t90*se)                     # 90% CI
c(dbar - t95*se, dbar + t95*se)                     # 95% CI
c(dbar - t99*se, dbar + t99*se)                     # 99% CI

# One-shot equivalent (if raw paired data are available):
# CI.diffmean(x = after, y = before, type = "paired", conf.level = 0.95)
# t.test(after, before, paired = TRUE, conf.level = 0.95)
```

![Master illustration](statistics/images/master/master_g13e_ai.png)

---

### Part (g) — Side-by-side: paired (correct) vs independent (wrong) CIs.

Using the same point estimate $\bar d = 10.1$ but the **wrong** SE $= 2.359$:

| Level | $t_{1-\alpha/2,\,22}$ | Paired ME / CI | Wrong-indep ME / CI |
|---|---|---|---|
| 90% | 1.7171 | $1.50$ / $[8.60,\,11.60]$ | $4.05$ / $[6.05,\,14.15]$ |
| 95% | 2.0739 | $1.82$ / $[8.28,\,11.92]$ | $4.89$ / $[5.21,\,14.99]$ |
| 99% | 2.8188 | $2.47$ / $[7.63,\,12.57]$ | $6.65$ / $[3.45,\,16.75]$ |

The wrong intervals are about $2.7\times$ wider. **Crucially**, even the wrong 99% CI still excludes $0$ here because the effect is large — but in a marginal case the wrong analysis can flip the conclusion from "significant" to "inconclusive". The lesson is procedural, not numerical: *paired design $\Rightarrow$ paired SE*.

---

### Part (h) — Decision: does the campaign work?

All three CIs lie strictly **above $0$**:
$$[8.60,\,11.60]_{90\%} \;\subset\; [8.28,\,11.92]_{95\%} \;\subset\; [7.63,\,12.57]_{99\%}.$$
So $\mu_D = 0$ is **incompatible** with the data at every conventional level. We conclude:

> *With 99% confidence, the targeted advertising campaign raised weekly store sales by between **€7.6k and €12.6k per store**.*

If, hypothetically, the 95% CI had been $[-1.2,\,3.0]$ — i.e. **containing $0$** — the conclusion would have been the opposite: the data would be consistent with no change, and the campaign's effect could not be distinguished from sampling noise (at the 5% level).

**Operational meaning.** Multiply $\bar d$ by the number of stores and the duration of effect to get an aggregate revenue gain; compare with the campaign cost to assess ROI. The CI quantifies the precision of the per-store estimate, not the cost-benefit.

---

### Summary table (master dataset $n=23$, $\bar d = 10.1$, $s_d = 4.2$)

| Quantity | Value | Where |
|---|---|---|
| Point estimate $\bar d$ | $10.1$ | Part (b) |
| Paired SE $= s_d/\sqrt n$ | $0.876$ | Part (c) |
| Wrong independent SE | $2.359$ | Part (e) |
| Penalty ratio | $\times 2.69$ | Part (e) |
| $t_{0.95,22}$ / $t_{0.975,22}$ / $t_{0.995,22}$ | $1.717\,/\,2.074\,/\,2.819$ | Part (f) |
| 90% CI | $[8.60,\,11.60]$ | Part (f) |
| 95% CI | $[8.28,\,11.92]$ | Part (f) |
| 99% CI | $[7.63,\,12.57]$ | Part (f) |
| Contains $0$? | **No** (all 3 levels) | Part (h) |

**Master take-aways.**
1. *Paired* means the **same unit** is measured **twice** — the design *creates* dependence on purpose, to **subtract out** unit-level variance.
2. The mechanism is the covariance term: $\Var(X-Y) = \Var(X) + \Var(Y) - 2\,\Cov(X,Y)$; with $\rho > 0$ this **shrinks** the variance of the difference (and the SE).
3. Once differences $D_i$ are formed the problem collapses to a one-sample CI: $\bar d \pm t_{1-\alpha/2,\,n-1}\,s_d/\sqrt n$.
4. Always use $t_{n-1}$ (small $n$) or $z$ (large $n$ via CLT) — *never* mix $s_d$ with a $z$ quantile when $n$ is small.
5. Ignoring pairing and running an independent-samples CI **inflates the SE** by $1/\sqrt{1-\rho}$ at equal variances (here $\approx 2.7\times$): correct *design* in, correct *SE* out.
6. CI excludes $0$ ⇒ evidence of an effect at the chosen level; CI contains $0$ ⇒ data consistent with no change.

---

**Linked snippets.**
- **Ex 5.4** — paired estimator + paired SE for a pre/post customer-spending survey ($n=315$, $\hat\rho=0.65$); illustrates the SE inflation when pairing is ignored.
- **Ex 5.6d** — derivation of the equal-variance collapse $\Var(\bar D) = 2\sigma^2(1-\rho)/n$ and the pooled-variance plug-in $s^2_{\text{pool}} = (s_X^2+s_Y^2)/2$; explains why pairing only helps when $\rho>0$.
- **Ex 6.2a, 6.2b** — NA vs EU sales for Action titles: assumption of paired sampling (6.2a) + matching 98% paired CI (6.2b).
- **Ex 6.8 c1** — paired 90% CI for FinSkills − Skills on the same developer ($n=820$); side-by-side comparison with the wider independent-samples CI.
- **Ex 6.11a** — paired 90% CI for a blood indicator before/after a physical test ($n=25$, $\rho=0.6$, $t_{24}$).
- **Ex 6.17a** — paired 99% CI for dwell time across two weeks at $n=23$ stores; this master is the consolidated/rounded version of that exercise (master uses rounded $s_d=4.2$, SE $\approx 0.876$; 6.17a derives $s_d \approx 3.95$, SE $\approx 0.824$ from the original $s_{XY}=34.6$).
- **Ex 6.18b** — paired 98% CI for NA vs EU sales on Action titles (large $n$, CLT covers normality of $\bar D$).
""",
    "images": ["statistics/images/master/master_g13e_ai.png"],
}


master_exercises["g14e_power"] = {
    "title": "Master Exam — Power, Type II error, and sample-size effects (consolidated)",
    "content": r"""**Setup.** Consider the `NewHired` dataset, where `Weeks` measures how long each newly hired worker took to find a job. We assume the population SD is **known**, $\sigma = 4$ weeks (variance $\sigma^2 = 16$), and the sample size is $n = 47$. A career-service manager tests the lower-tailed claim that the average search time is **below 45 weeks**:

$$H_0:\ \mu \;\ge\; \mu_0 = 45 \qquad \text{vs}\qquad H_1:\ \mu \;<\; 45.$$
Throughout the master, the **alternative truth** considered is $\mu = \mu_1 = 43$ weeks (i.e. the population mean is actually $2$ weeks below the null boundary). All tests use the $z$-statistic
$$Z \;=\; \frac{\bar X - \mu_0}{\sigma/\sqrt n} \;\sim\; \mathcal N(0,1)\ \text{under}\ H_0.$$
The standard error is $SE = \sigma/\sqrt n = 4/\sqrt{47} \approx 0.5835$ weeks.

---

**(a) Two error types — definitions and decision matrix.** A hypothesis test takes a yes/no decision on $H_0$ from random data, so two distinct mistakes are possible:

| | $H_0$ true | $H_0$ false |
|---|---|---|
| **Reject $H_0$** | **Type I error** (prob. $\alpha$) | Correct (prob. $1-\beta$ = **power**) |
| **Fail to reject $H_0$** | Correct (prob. $1-\alpha$) | **Type II error** (prob. $\beta$) |

* **Type I** = rejecting a *true* null (false alarm). Its probability is *chosen* by the analyst via $\alpha$ (e.g. 5%, 10%).
* **Type II** = failing to reject a *false* null (missed detection). Its probability $\beta$ is *induced* by $\alpha$, $n$, $\sigma$ and the true alternative $\mu_1$.
* **Power** $= 1 - \beta = P(\text{reject } H_0 \mid \mu = \mu_1)$ is the test's ability to *detect* the specified alternative.

The two errors are in tension: shrinking $\alpha$ shifts the critical value *toward* the null, which inflates $\beta$. The only way to reduce both simultaneously is to add information — i.e. increase $n$.

---

**(b) Critical value on the $\bar X$ scale.** For a lower-tailed test at level $\alpha$, $H_0$ is rejected when $Z < -z_{1-\alpha} = -z_\alpha^{(\text{tail})}$ — equivalently when $\bar X$ falls below
$$\boxed{\;c \;=\; \mu_0 \;-\; z_{1-\alpha}\,\frac{\sigma}{\sqrt n}\;}$$
With $\alpha = 0.10$, $z_{0.90} = 1.2816$:
$$c \;=\; 45 \;-\; 1.2816 \cdot \frac{4}{\sqrt{47}} \;=\; 45 - 0.748 \;\approx\; 44.252\ \text{weeks}.$$
**Decision rule.** Reject $H_0$ iff $\bar x < 44.252$.

```r
mu0 <- 45;  sigma <- 4;  n <- 47;  alpha <- 0.10
se   <- sigma/sqrt(n);                   se        # ~ 0.5835
zalp <- qnorm(1 - alpha);                zalp      # 1.2816
c    <- mu0 - zalp*se;                   c         # ~ 44.252
```

---

**(c) Two complementary truths — $\mu \in H_0$ (Type I face) vs $\mu \in H_1$ (Type II face).** The same critical value $c=44.252$ has *two* error roles depending on where the *true* $\mu$ lives — and Ex 7.1b asks us to compute both faces explicitly.

*(c1) True $\mu = 50$ (well inside $H_0$, since $50\ge 45$).* Concluding "$\bar X < 45$" — i.e. **rejecting** $H_0$ — would be a **Type I error** because $\mu=50$ belongs to $H_0$. Its probability is
$$P(\bar X < c \mid \mu = 50) \;=\; P\!\left(Z < \frac{44.252 - 50}{0.5835}\right) \;=\; P(Z < -9.85) \;\approx\; 0.$$
At $\mu=50$ the rejection probability collapses to **essentially zero** — far below the nominal $\alpha=0.10$, because $\mu=50$ sits $5$ weeks **above** the boundary of $H_0$ and the test virtually never reaches the lower critical region. This is the *flip side* of the formal guarantee $\sup_{\mu\in H_0} P(\text{reject})=\alpha$: at the *boundary* $\mu=\mu_0=45$ the rejection rate is exactly $\alpha=0.10$; **deeper inside $H_0$** it drops toward zero. Plotting this rejection probability as a function of $\mu$ traces the **power curve** — flat-low across $H_0$ (Type I face) and ramping up across $H_1$ (power face).

*(c2) True $\mu_1 = 43$ (inside $H_1$, since $43 < 45$).* Under $H_1$, $\bar X \sim \mathcal N(\mu_1,\,\sigma^2/n)$. A **Type II error** happens whenever the data *fails* to reject, i.e. $\bar X \ge c$:
$$\beta \;=\; P(\bar X \ge c \mid \mu = \mu_1)
\;=\; P\!\left(Z \ge \frac{c - \mu_1}{\sigma/\sqrt n}\right)
\;=\; P\!\left(Z \ge \frac{44.252 - 43}{0.5835}\right)
\;=\; P(Z \ge 2.146)$$
$$\beta \;\approx\; 1 - \Phi(2.146) \;\approx\; 0.0159 \quad(\text{about }1.6\%).$$
Geometrically, $\beta$ is the area under the $H_1$ density $\mathcal N(43,\,SE^2)$ that lies **above** the cutoff $c = 44.252$.

```r
mu1   <- 43
# (c1) Type I face at mu = 50 (deep inside H0)
pnorm(c, mean = 50, sd = se)                  # ~ 0   (P(reject | mu=50))
# (c2) Type II face at mu1 = 43 (inside H1)
beta  <- 1 - pnorm(c, mean = mu1, sd = se);   beta   # ~ 0.0159
power <- 1 - beta;                            power  # ~ 0.9841
```

---

**(d) Power $= 1 - \beta$.** Power is the area under the $H_1$ density *below* the cutoff $c$:
$$\text{Power} \;=\; 1 - \beta \;=\; P(\bar X < c \mid \mu = \mu_1) \;\approx\; 0.9841.$$
With $n = 47$, $\sigma = 4$, $\alpha = 0.10$, and a true mean $2$ weeks below the null boundary, the test rejects roughly **98.4%** of the time — a very powerful design.

![Master illustration](statistics/images/master/master_g14e_ai.png)

---

**(e) Effect of sample size $n$ — sketch + table.** The SE shrinks as $\sigma/\sqrt n$, so both densities (under $H_0$ and $H_1$) get **narrower** as $n$ grows. The critical value
$$c(n) \;=\; \mu_0 - z_{1-\alpha}\,\sigma/\sqrt n$$
moves **closer** to $\mu_0 = 45$. Since $\mu_1 = 43$ is fixed and below $c$, the $H_1$-tail above $c$ shrinks, so $\beta \downarrow$ and power $\uparrow$.

| $n$ | $SE = 4/\sqrt n$ | $c = 45 - 1.2816\,SE$ | $z^* = (c - 43)/SE$ | $\beta = P(Z \ge z^*)$ | Power $= 1-\beta$ |
|---|---|---|---|---|---|
| 10  | 1.2649 | 43.379 | 0.300 | 0.3821 | 0.6179 |
| 20  | 0.8944 | 43.854 | 0.955 | 0.1697 | 0.8303 |
| 47  | 0.5835 | 44.252 | 2.146 | 0.0159 | 0.9841 |
| 100 | 0.4000 | 44.487 | 3.717 | 0.0001 | 0.9999 |
| 200 | 0.2828 | 44.638 | 5.794 | $\approx 0$ | $\approx 1$ |

The growth of power is **monotonic** in $n$ — and quite steep because the effect size in SE units, $(\mu_0 - \mu_1)/SE = 2/SE$, grows like $\sqrt n$.

```r
ns   <- c(10, 20, 47, 100, 200)
sapply(ns, function(n){
  se <- sigma/sqrt(n);  c <- mu0 - qnorm(1-alpha)*se
  1 - pnorm(c, mu1, se)                       # beta
})
```

---

**(f) Effect of the effect size $|\mu_1 - \mu_0|$.** Fix $n = 47$, $\alpha = 0.10$. The further $\mu_1$ is *into* the alternative region, the smaller the $H_1$-tail above $c = 44.252$:

| $\mu_1$ | $z^* = (c - \mu_1)/SE$ | $\beta$ | Power |
|---|---|---|---|
| 44.5 | $-0.425$ | 0.6645 | 0.3355 |
| 44.0 | $0.432$  | 0.3328 | 0.6672 |
| 43.5 | $1.289$  | 0.0987 | 0.9013 |
| 43.0 | $2.146$  | 0.0159 | 0.9841 |
| 42.0 | $3.860$  | $\approx 6\times 10^{-5}$ | $\approx 1$ |

**Edge case.** As $\mu_1 \uparrow \mu_0 = 45$, $\beta \to 1 - \alpha$ and power $\to \alpha$ — i.e. when there is *no* real effect, the test rejects only at its nominal Type I rate.

---

**(g) Effect of $\alpha$.** Increasing $\alpha$ moves $c$ *further from* $\mu_0$ (toward the alternative region), enlarging the rejection set and therefore boosting power — at the cost of more false alarms.

| $\alpha$ | $z_{1-\alpha}$ | $c$ | $\beta$ at $\mu_1 = 43$ | Power |
|---|---|---|---|---|
| 0.01 | 2.3263 | 43.643 | 0.1361 | 0.8639 |
| 0.05 | 1.6449 | 44.040 | 0.0418 | 0.9582 |
| 0.10 | 1.2816 | 44.252 | 0.0159 | 0.9841 |
| 0.20 | 0.8416 | 44.509 | 0.0040 | 0.9960 |

The trade-off is mechanical: $\alpha + \beta$ is **not** constrained to 1, but every move that lowers one tends to raise the other — only changes to $n$ (or $\sigma$) can shrink both at once.

```r
alphas <- c(0.01, 0.05, 0.10, 0.20)
sapply(alphas, function(a){
  c <- mu0 - qnorm(1-a)*se;  1 - pnorm(c, mu1, se)
})                                              # beta column
```

---

**Summary — three levers acting on $\beta$ / power**

| Lever | Mechanism | Effect on power | Cost |
|---|---|---|---|
| $n \uparrow$           | $SE \downarrow$, $c$ closer to $\mu_0$, $H_1$-tail shrinks | $\uparrow$ | data collection |
| $|\mu_1 - \mu_0| \uparrow$ | $H_1$ density centred further from $c$            | $\uparrow$ | not a design choice — set by reality |
| $\alpha \uparrow$       | $c$ moves further from $\mu_0$ into the alternative side | $\uparrow$ | more Type I errors |

**Master take-aways.**
1. $\alpha$ is *chosen*; $\beta$ is *induced* — but both depend on the same critical value $c$ on the $\bar X$ scale.
2. $c = \mu_0 - z_{1-\alpha}\,\sigma/\sqrt n$ for a lower-tailed test; flip the sign for upper-tailed; use $z_{1-\alpha/2}$ for two-sided.
3. $\beta = P(\bar X \ge c \mid \mu = \mu_1)$ is just a tail of the $\mathcal N(\mu_1,\,\sigma^2/n)$ density — no new machinery.
4. Power grows monotonically in $n$, in the effect size $|\mu_1-\mu_0|$, and in $\alpha$.
5. The Type I / Type II trade-off can only be *escaped* — not balanced — by raising $n$.
6. In our worked case ($n=47$, $\sigma=4$, $\mu_0=45$, $\mu_1=43$, $\alpha=0.10$): $c = 44.252$, $\beta \approx 0.016$, power $\approx 0.984$.

---

**Linked snippets:** Ex 7.1b (NewHired Weeks: σ²=16 known, α=0.10 lower-tailed test; computes $\beta$ at $\mu = 50$ — Type I error case — and at $\mu = 43$ — Type II error case ≈ 0.016 — the dataset that anchors this master).
""",
    "images": ["statistics/images/master/master_g14e_ai.png"],
}


master_exercises["g14b_two_sample"] = {
    "title": "Master Exam — Two-sample independent tests (means & proportions)",
    "content": r"""**Master exercise — Independent two-sample tests for means and proportions.**

Consolidates the unique sub-tasks asked in **Ex 7.3a** (two-prop $z$, cafeteria pre/post visit), **Ex 7.3b** (two-prop $z$, heavy-user cutoff $>4$), **Ex 7.5a** (pooled two-sample $t$ vs $\mu_0=10$, fish-diet cholesterol), **Ex 7.7a** (two-prop $z$, AI-tool use Younger vs Senior), **Ex 7.10a** (pooled two-sample $t$ on summary stats, competing-company comparison at $\alpha=0.10$).

Unified workflow (independent samples): assumptions $\to$ hypotheses $\to$ pick SE (Welch / pooled $t$ / pooled-$\hat p$ for proportions) $\to$ statistic $\to$ rejection region $\to$ p-value $\to$ decision.

---

### Master template — three independent-samples building blocks

| Setting | Statistic | SE under $H_0$ | Reference distr. |
|---|---|---|---|
| **(M)** Mean comparison, $\sigma$'s unknown, equal-var assumption | $T = \dfrac{\bar x_1-\bar x_2-\Delta_0}{\widehat{\text{SE}}}$ | $s_p\sqrt{\tfrac{1}{n_1}+\tfrac{1}{n_2}}$, $s_p^2 = \tfrac{(n_1-1)s_1^2+(n_2-1)s_2^2}{n_1+n_2-2}$ | $t_{n_1+n_2-2}$ |
| **(W)** Welch variant, unequal variances | same $T$ | $\sqrt{s_1^2/n_1 + s_2^2/n_2}$ | $t$ on Welch–Satterthwaite df |
| **(P)** Proportion comparison | $Z = \dfrac{\hat p_1-\hat p_2}{\widehat{\text{SE}}_0}$ | $\sqrt{\hat p(1-\hat p)(\tfrac{1}{n_1}+\tfrac{1}{n_2})}$ with $\hat p = \tfrac{x_1+x_2}{n_1+n_2}$ | $N(0,1)$ |

Decision rule (any tail): $\text{Reject } H_0 \iff t_\text{obs}\in R_\alpha \iff p\leq\alpha$.

---

### (a) Two-proportion $z$-test, cafeteria visit pre/post (Ex 7.3a)

**Data.** Pre-promotion sample $n_\text{PRE}=140$ (108 visitors $\Rightarrow$ $\hat p_\text{PRE} = 108/140 = 0.7714$); post-promotion $n_\text{POST}=159$ (127 visitors $\Rightarrow$ $\hat p_\text{POST} = 127/159 = 0.7987$). A "visitor" is a customer with $\geq 1$ stop in the month.

**Hypotheses (one-sided upper).** Most serious error = rolling out an ineffective promotion to other branches:
$$H_0:\,p_\text{POST}=p_\text{PRE} \quad\text{vs}\quad H_1:\,p_\text{POST}>p_\text{PRE}.$$

**Assumptions.** Two independent samples; both $n\hat p$, $n(1-\hat p) \gg 5$ $\Rightarrow$ CLT holds; under $H_0$ the two proportions share a common $p$.

**Pooled-$\hat p$ SE (template P).**
$$\hat p = \frac{108+127}{140+159} = \frac{235}{299} = 0.7860, \qquad \widehat{\text{SE}}_0 = \sqrt{0.7860\cdot 0.2140\cdot\bigl(\tfrac{1}{140}+\tfrac{1}{159}\bigr)} \approx 0.0475.$$

**Statistic & p-value.**
$$z_\text{obs} = \frac{0.7987 - 0.7714}{0.0475} = \frac{0.0273}{0.0475} \approx 0.575, \qquad p = 1 - \Phi(0.575) \approx 0.2827.$$

**Decision.** RR$_{0.05} = \{z > 1.6449\}$. $0.575 < 1.6449$ (equivalently $0.28 \gg 0.05$) $\Rightarrow$ **do not reject $H_0$**. No evidence the promotion increases the visit rate — **do not extend** it.

```r
# (a) Ex 7.3a: cafeteria visit pre/post, two-prop z, upper tail
n1 <- 140; x1 <- 108                       # PRE
n2 <- 159; x2 <- 127                       # POST
ph1 <- x1/n1; ph2 <- x2/n2; c(ph1, ph2)    # 0.7714, 0.7987
phat <- (x1+x2)/(n1+n2);  phat             # 0.7860 pooled
se0  <- sqrt(phat*(1-phat)*(1/n1 + 1/n2)); se0   # 0.0475
z    <- (ph2 - ph1)/se0;  z                # 0.575
1 - pnorm(z)                               # 0.2827
qnorm(0.95)                                # 1.6449
TEST.diffprop(x = Stops_POST >= 1, y = Stops_PRE >= 1, pdiff = 0, alternative = "greater")
```

![Master illustration](statistics/images/master/master_g14b_ai.png)

---

### (b) Two-proportion $z$-test, heavy users (Ex 7.3b)

**Cutoff change.** Now a customer is a **heavy user** if they visited **more than 4 times** per month. Counts: pre 23/140, post 37/159.
$$\hat p_\text{PRE} = 23/140 = 0.1643, \qquad \hat p_\text{POST} = 37/159 = 0.2327.$$

**Hypotheses.** Same one-sided framing as (a): $H_0:p_\text{POST}=p_\text{PRE}$ vs $H_1:p_\text{POST}>p_\text{PRE}$.

**Pooled-$\hat p$ SE.**
$$\hat p = \frac{23+37}{140+159} = \frac{60}{299} = 0.2007, \qquad \widehat{\text{SE}}_0 = \sqrt{0.2007\cdot 0.7993\cdot\bigl(\tfrac{1}{140}+\tfrac{1}{159}\bigr)} \approx 0.0464.$$

**Statistic & p-value.**
$$z_\text{obs} = \frac{0.0684}{0.0464} \approx 1.474, \qquad p = 1 - \Phi(1.474) \approx 0.0703.$$

**Decision.** RR$_{0.05} = \{z > 1.6449\}$. $1.474 < 1.6449$ $\Rightarrow$ **do not reject** at 5%. But **at $\alpha=0.10$**, $1.474 < z_{0.90}=1.2816$ is FALSE — $1.474 > 1.2816$ $\Rightarrow$ **reject** at 10%. Borderline; growth from 16.4% to 23.3% in heavy-user share is suggestive but not conclusive at conventional 5%.

```r
# (b) Ex 7.3b: heavy users (>4 stops), two-prop z, upper tail
n1 <- 140; x1 <- 23                        # PRE heavy
n2 <- 159; x2 <- 37                        # POST heavy
phat <- (x1+x2)/(n1+n2);  phat             # 0.2007
se0  <- sqrt(phat*(1-phat)*(1/n1 + 1/n2)); se0   # 0.0464
z    <- (x2/n2 - x1/n1)/se0;  z            # 1.474
1 - pnorm(z)                               # 0.0703
qnorm(0.95); qnorm(0.90)                   # 1.6449, 1.2816
```

---

### (c) Pooled two-sample $t$ with $\Delta_0 \neq 0$ — fish-diet cholesterol (Ex 7.5a)

**Data.** Two independent groups of $n_1 = n_2 = 100$ males, assigned for 6 months to **Standard** vs **Seafood** diet:

| Diet | $\bar x$ | $s^2$ |
|---|---|---|
| Standard | $210.1$ | $37.4$ |
| Seafood | $196.8$ | $33.5$ |

Researchers claim the mean difference $\mu_\text{Std} - \mu_\text{Sea}$ is **strictly greater than 10**.

**Hypotheses (one-sided upper, $\Delta_0 = 10$).**
$$H_0:\,\mu_\text{Std} - \mu_\text{Sea} \leq 10 \quad\text{vs}\quad H_1:\,\mu_\text{Std} - \mu_\text{Sea} > 10.$$

**Assumptions.** Independence; equal variances assumed $\Rightarrow$ pooled-variance template (M). $n=100$ each $\Rightarrow$ CLT, so normal & $t_{198}$ critical values match to four decimals.

**Pooled variance & SE.**
$$s_p^2 = \frac{99\cdot 37.4 + 99\cdot 33.5}{198} = 35.45, \qquad \widehat{\text{SE}} = \sqrt{2 s_p^2/n} = \sqrt{0.7090} = 0.8420.$$

**Statistic & RR.** Observed $\bar x_\text{Std}-\bar x_\text{Sea} = 13.3$.
$$t_\text{obs} = \frac{13.3 - 10}{0.8420} = 3.92, \qquad R_{0.05} = \{T > t_{0.95,\,198}\} \approx \{T > 1.6526\}.$$

Equivalently, the RR on $\bar x_\text{Std}-\bar x_\text{Sea}$ is $> 10 + 1.6526\cdot 0.8420 = 11.39$. Since $13.3 > 11.39$ $\Rightarrow$ **reject $H_0$**.

**p-value.** $p = 1 - F_{t_{198}}(3.92) \approx 6.1\cdot 10^{-5}$ (normal approx.: $4.4\cdot 10^{-5}$).

**Conclusion.** Overwhelming evidence the Seafood diet lowers mean cholesterol by **more than 10** units.

```r
# (c) Ex 7.5a: pooled two-sample t with Delta0 = 10, one-sided upper
xs <- 210.1; vs <- 37.4; ns <- 100         # Standard diet
xf <- 196.8; vf <- 33.5; nf <- 100         # Seafood diet
sp2 <- ((ns-1)*vs + (nf-1)*vf)/(ns+nf-2);  sp2     # 35.45
se  <- sqrt(2*sp2/ns);                     se      # 0.8420
t   <- (xs - xf - 10)/se;                  t       # 3.92
qt(0.95, df = ns+nf-2);  qnorm(0.95)               # 1.6526, 1.6449
10 + qt(0.95, df = 198)*se                          # 11.39 (RR on diff)
1 - pt(t, df = ns+nf-2)                             # 6.1e-5
1 - pnorm(t)                                         # 4.4e-5
TEST.diffmean(..., alternative = "greater", mu0 = 10, var.test = TRUE)
```

---

### (d) Two-proportion $z$-test, AI-tool use Younger vs Senior (Ex 7.7a)

**Data (`Developers_ITA`).** Sample proportions of developers using AI tools (e.g. ChatGPT) at work: $\hat p_\text{Young} \approx 0.57$, $\hat p_\text{Senior} \approx 0.40$ (sub-samples large; the textbook reports $z_\text{obs} \approx 4.77$ from the raw data).

**Hypotheses (one-sided upper).** The "younger use them more" claim is the research hypothesis:
$$H_0:\,p_\text{Young} = p_\text{Senior} \quad\text{vs}\quad H_1:\,p_\text{Young} > p_\text{Senior}.$$

**Assumptions.** Independent sub-samples (Younger=TRUE vs FALSE are disjoint); each sub-sample large enough for CLT; under $H_0$ a common $p$, estimated by the pooled $\hat p$.

**Statistic.** With template (P) (pooled $\hat p$, pooled SE) the realised $z \approx 4.77$, giving $p = 1 - \Phi(4.77) < 10^{-4}$.

**Decision.** $4.77 \gg z_{0.95}=1.6449$ at any conventional $\alpha$ (5%, 1%, 0.1%, …) $\Rightarrow$ **reject $H_0$**. Overwhelming evidence that younger developers adopt AI tools more.

```r
# (d) Ex 7.7a: AI-tool use Younger vs Senior, two-prop z (built-in)
TEST.diffprop(x = Developers_ITA$ChatGPT[Developers_ITA$Younger == TRUE],
              y = Developers_ITA$ChatGPT[Developers_ITA$Younger == FALSE],
              success.x = "Yes", pdiff = 0, alternative = "greater", digits = 4)

# Manual cross-check:
phY <- mean(Developers_ITA$ChatGPT[Developers_ITA$Younger == TRUE]  == "Yes")  # ~0.57
phS <- mean(Developers_ITA$ChatGPT[Developers_ITA$Younger == FALSE] == "Yes")  # ~0.40
nY  <- sum(Developers_ITA$Younger == TRUE)
nS  <- sum(Developers_ITA$Younger == FALSE)
phat <- (nY*phY + nS*phS)/(nY + nS)
se0  <- sqrt(phat*(1-phat)*(1/nY + 1/nS))
z    <- (phY - phS)/se0;  z                # ~4.77
1 - pnorm(z)                                # < 1e-4
```

---

### (e) Pooled two-sample $t$ from summary stats, $\alpha=0.10$ (Ex 7.10a)

**Data.** Competing company: $n_y=800$, $\bar y = 1300$, $s_y = 960$. Considered company (the $n_x=750$ women in `DS`): $\bar x = 1228.44$, $s_x^2 = 940{,}900.9$.

**Hypotheses (one-sided upper).** Claim: considered-company mean expenditure is higher than competitor's:
$$H_0:\,\mu_x = \mu_y \quad\text{vs}\quad H_1:\,\mu_x > \mu_y.$$

**Assumptions.** Independent samples; large $n$'s $\Rightarrow$ CLT (no normality of expenditure needed); pooled-variance template (M) with $s_y^2 = 960^2 = 921{,}600$.

**Pooled variance & SE.**
$$s_p^2 = \frac{749\cdot 940{,}900.9 + 799\cdot 921{,}600}{1548} \approx 930{,}938.7, \qquad \widehat{\text{SE}} = \sqrt{s_p^2(\tfrac{1}{750}+\tfrac{1}{800})} = \sqrt{2402.92} \approx 49.02.$$

**Statistic.**
$$t_\text{obs} = \frac{1228.44 - 1300}{49.02} = \frac{-71.56}{49.02} \approx -1.459.$$

**Decision at $\alpha=0.10$.** RR $= \{T > z_{0.90}\} = \{T > 1.2816\}$ (with df $=1548$, $t$ matches $z$). $-1.459 < 1.2816$ $\Rightarrow$ **do not reject**. Equivalently $p = 1 - \Phi(-1.459) \approx 0.9277 \gg 0.10$.

**Interpretation.** Observed difference is in the **wrong direction** ($\bar x < \bar y$): not only is there no evidence for "considered $>$ competitor", the data point mildly the opposite way. (The two-sided p-value would be $\approx 0.145$, still not significant at 10%.)

```r
# (e) Ex 7.10a: pooled two-sample t from summary stats, one-sided upper, alpha = 0.10
xbar <- 1228.44; s2x <- 940900.9; nx <- 750
ybar <- 1300;    s2y <- 960^2;    ny <- 800           # 921600

sp2 <- ((nx-1)*s2x + (ny-1)*s2y)/(nx+ny-2); sp2       # 930938.7
se  <- sqrt(sp2*(1/nx + 1/ny));             se        # 49.02
t   <- (xbar - ybar)/se;                    t         # -1.459
1 - pnorm(t)                                          # 0.9277  (one-sided upper)
1 - pt(t, df = nx+ny-2)                                # 0.9277
qnorm(0.90)                                           # 1.2816 (one-sided crit at 10%)
```

---

### (f) Side-by-side summary

| # | Application | Template | $H_1$ | Diff. | SE | Stat | p-value | @ chosen $\alpha$ |
|---|---|---|---|---|---|---|---|---|
| (a) | Visit $\geq 1$ pre/post (Ex 7.3a) | P (pooled $\hat p$) | $p_\text{POST}>p_\text{PRE}$ | $0.0273$ | $0.0475$ | $z = 0.575$ | $0.2827$ | retain @ 0.05 |
| (b) | Heavy users $>4$ pre/post (Ex 7.3b) | P | $p_\text{POST}>p_\text{PRE}$ | $0.0684$ | $0.0464$ | $z = 1.474$ | $0.0703$ | retain @ 0.05; reject @ 0.10 |
| (c) | Cholesterol Std vs Sea, $\Delta_0=10$ (Ex 7.5a) | M (pooled $t$) | $\mu_\text{Std}-\mu_\text{Sea}>10$ | $13.3$ | $0.8420$ | $t_{198} = 3.92$ | $6\cdot 10^{-5}$ | reject @ 0.05 |
| (d) | AI tools Younger vs Senior (Ex 7.7a) | P | $p_\text{Young}>p_\text{Senior}$ | $0.17$ | (built-in) | $z \approx 4.77$ | $<10^{-4}$ | reject @ 0.05 |
| (e) | Considered vs competitor mean (Ex 7.10a) | M | $\mu_x>\mu_y$ | $-71.56$ | $49.02$ | $t \approx -1.459$ | $0.9277$ | retain @ 0.10 |

---

### (g) Master take-aways

1. **One shape, three SEs.** $T = (\text{diff} - \Delta_0)/\widehat{\text{SE}}$ is the same across (M), (W), (P); only $\widehat{\text{SE}}$ and the reference distribution change.
2. **Means with $\sigma$'s unknown.** Pooled-$t$ (template M) assumes $\sigma_1 = \sigma_2$. Welch (template W) drops that assumption — safe default. When $n_1=n_2$ the two SEs **numerically coincide**.
3. **Proportions: pool under $H_0$.** Use $\hat p = (x_1+x_2)/(n_1+n_2)$ in the SE because, under $H_0$, the two proportions share a common $p$. Separate $\hat p_1,\hat p_2$ are for CIs, not for the test SE.
4. **Direction matters.** Place the directional claim (the one whose false rejection is the most damaging error) in $H_1$. Never pick the side after seeing the data; the two-sided p-value is twice the one-sided **only** when $T$ points in the $H_1$ direction (else $\approx 1-\text{one-sided}$).
5. **Non-zero $\Delta_0$ goes into the numerator.** Ex 7.5a tests $\Delta_0=10$, not $\Delta_0=0$: subtract 10 from the observed mean gap before dividing by SE.
6. **Equivalent decisions.** $t_\text{obs}\in R_\alpha \iff p\leq\alpha$. Large $p$ $\neq$ "$H_0$ true"; only "insufficient evidence at $\alpha$".

---

**Linked snippets:**
Ex 7.3a (cafeteria visit pre/post, pooled-$\hat p$ $z$) $\to$ part (a);
Ex 7.3b (heavy users $>4$, same template, borderline at 10%) $\to$ part (b);
Ex 7.5a (pooled $t$ with $\Delta_0=10$, fish-diet cholesterol, RR & p-value) $\to$ part (c);
Ex 7.7a (Younger vs Senior AI-tool use, pooled-$\hat p$ $z$, large-$z$ rejection) $\to$ part (d);
Ex 7.10a (pooled $t$ on summary stats, considered vs competitor at $\alpha=0.10$) $\to$ part (e).
All five are instances of the unified independent-samples template above.
""",
    "images": ["statistics/images/master/master_g14b_ai.png"],
}


# =====================================================================
# g14c_paired — Paired hypothesis test (one-sided)
# Consolidates: ex7.6a (n=7, before/after wi-fi), ex7.6b (n=14 effect)
# Dataset: Arcade revenue, n=7 stores, d_bar = 120 EUR, s_d = 110 EUR
# =====================================================================
master_exercises["g14c_paired"] = {
    "title": "Master Exam — Paired hypothesis test (Arcade wi-fi, before vs after)",
    "content": r"""**Master exercise — Paired hypothesis test for a mean difference (one-sided).**

This single exercise consolidates the unique sub-tasks asked across **Ex 7.6a** (paired $t$-test with $n=7$) and **Ex 7.6b** (effect of doubling the sample to $n=14$): why pairing is the right design, construction of the differences $D_i$, the test statistic $T = \bar D/(s_d/\sqrt n)\sim t_{n-1}$, rejection regions at $\alpha=0.05$ and $\alpha=0.01$, the one-sided $p$-value, and the *scaling laws* that explain how SE, $t_\text{obs}$ and the $p$-value move when $n$ doubles.

---

### Dataset (single, shared by all parts)

An arcade chain installed **free wi-fi** in its stores. For each of $n=7$ stores the manager recorded the **weekly revenue** before installation ($X^\text{B}_i$, €) and again three months **after** ($X^\text{A}_i$, €). Because the same store is measured twice, the natural unit of analysis is the **within-store difference**
$$D_i \;=\; X^\text{A}_i \;-\; X^\text{B}_i,\qquad i=1,\dots,7.$$

Sample summaries of the differences:
$$n = 7,\qquad \bar d \;=\; 120\;\text{€},\qquad s_d \;=\; 110\;\text{€}.$$

(The before/after means and the cross-covariance have already been folded into $s_d$; with only the differences in hand the problem becomes a one-sample $t$-problem on $D$.)

---

### (a) Why **paired** design? — removing between-store variability

The two columns $X^\text{B}$ and $X^\text{A}$ are **not** two independent samples: they share the same set of stores. A small arcade in a sleepy suburb has *both* a low $X^\text{B}$ and a low $X^\text{A}$; a busy city-centre arcade has both high values. The store-level shock is **common to the two measurements**, so taking the difference $D_i$ literally **subtracts it out**:
$$\Var(D) \;=\; \Var(X^\text{A}) + \Var(X^\text{B}) \;-\; 2\,\Cov(X^\text{A},X^\text{B}).$$
With a positive within-store correlation ($\rho>0$) — which is exactly what we expect across two snapshots of the same store — $\Var(D)$ is **smaller** than the independent-samples variance $\Var(X^\text{A}) + \Var(X^\text{B})$. Pairing therefore **increases statistical power** at fixed $n$: same data, smaller SE, larger $|t|$, smaller $p$.

> *Intuition.* Comparing "store $i$ to itself, three months later" is much sharper than comparing "the average of 7 stores to the average of 7 other stores". Pairing exploits the design to **cancel** the between-unit variance.

**From per-group summaries to $s_d$ via the covariance.** If — as in Ex 7.6a's original wording — the data come as separate PRE/POST means and *variances* plus the cross-**covariance** $s_{\text{PRE,POST}}$, you reconstruct $s_d$ without raw data:
$$s_d^2 \;=\; s_\text{POST}^2 + s_\text{PRE}^2 \;-\; 2\,s_\text{PRE,POST}.$$
For instance, with $s_\text{POST}^2=21,\,s_\text{PRE}^2=12,\,s_\text{PRE,POST}=11$ one gets $s_d^2 = 21+12-22 = 11$ — the variance is dramatically smaller than the independent-samples sum $21+12=33$ precisely *because* of the positive covariance. The master proceeds with the seed values $\bar d=120,\,s_d=110$; the covariance route is the bridge from textbook input data to the differences' SD.

Because $n=7$ is small the CLT does not apply; we assume the **differences are normal**, $D_i \stackrel{iid}{\sim} N(\mu_D,\sigma_D^2)$ with $\sigma_D^2$ unknown.

---

### (b) Hypotheses and test statistic

Wi-fi extension is justified **only** if revenues **increase**. The most serious error is to roll out wi-fi chain-wide when there is no real gain, so the directional claim sits in $H_1$:
$$H_0:\mu_D \;\leq\; 0 \quad\text{vs}\quad H_1:\mu_D \;>\; 0.$$

With $\sigma_D$ unknown and $D$ assumed normal, the standardised pivot is Student's $t$:
$$T \;=\; \frac{\bar D - 0}{s_d/\sqrt n} \;\overset{H_0}{\sim}\; t_{n-1} \;=\; t_{6}.$$

The standard error and the observed statistic are
$$\widehat{\text{SE}}(\bar D) \;=\; \frac{s_d}{\sqrt n} \;=\; \frac{110}{\sqrt 7} \;=\; 41.576\;\text{€},$$
$$t_\text{obs} \;=\; \frac{120}{41.576} \;=\; 2.8863.$$

```r
# --- (b) Test statistic at n = 7 -----------------------------------------
dbar <- 120; sd_d <- 110; n7 <- 7
se7   <- sd_d / sqrt(n7);     se7              # 41.576
tobs7 <- dbar / se7;          tobs7            # 2.8863
```

---

### (c) Rejection regions at $\alpha=0.05$ and $\alpha=0.01$ (one-sided upper)

Reject $H_0$ when $T$ is **too large**. Critical values from $t_6$:
$$t_{0.95,\,6} \;=\; 1.9432, \qquad t_{0.99,\,6} \;=\; 3.1427.$$

| Level $\alpha$ | RR on $T$ | RR on $\bar D$ (multiply by SE $=41.576$) | Decision |
|---|---|---|---|
| $0.05$ | $T > 1.9432$ | $\bar D > 80.79$ € | $\bar d = 120 > 80.79$ $\Rightarrow$ **reject $H_0$** |
| $0.01$ | $T > 3.1427$ | $\bar D > 130.66$ € | $\bar d = 120 < 130.66$ $\Rightarrow$ **do not reject $H_0$** |

So at the 5% level the data support extending the wi-fi service; at the more conservative 1% level the evidence is *not quite* enough.

```r
# --- (c) Critical values and RRs on Dbar at n = 7 ------------------------
qt(0.95, df = n7 - 1)                          # 1.9432  (5%, upper)
qt(0.99, df = n7 - 1)                          # 3.1427  (1%, upper)
qt(0.95, df = n7 - 1) * se7                    # 80.79   (RR on Dbar, 5%)
qt(0.99, df = n7 - 1) * se7                    # 130.66  (RR on Dbar, 1%)
```

---

### (d) One-sided $p$-value

$$p \;=\; P\!\left(t_6 > t_\text{obs}\right) \;=\; P(t_6 > 2.8863) \;\approx\; 0.0139.$$

```r
1 - pt(tobs7, df = n7 - 1)                     # 0.01391 -> reject at 5%, retain at 1%
```

Sanity check vs the RR table: $\alpha = 0.05$: $p \approx 0.014 < 0.05 \Rightarrow$ reject; $\alpha = 0.01$: $p \approx 0.014 > 0.01 \Rightarrow$ retain. Both formulations agree, as they must.

**Interpretation.** Under $H_0$ (no average revenue change), observing a sample mean increase of 120 € (or more) across 7 stores has probability $\approx 1.4\%$ — small enough to reject at 5%, but not at 1%.

![Master illustration](statistics/images/master/master_g14c_ai.png)

---

### (e) Worked numerical summary ($n=7$)

| Quantity | Formula | Value |
|---|---|---|
| Point estimate $\bar d$ | given | $120$ € |
| Sample SD $s_d$ | given | $110$ € |
| Standard error $\widehat{\text{SE}}$ | $s_d/\sqrt n$ | $41.576$ € |
| Observed $t_\text{obs}$ | $\bar d/\widehat{\text{SE}}$ | $2.8863$ |
| Critical $t_{0.95,\,6}$ | quantile | $1.9432$ |
| Critical $t_{0.99,\,6}$ | quantile | $3.1427$ |
| One-sided $p$-value | $P(t_6 > t_\text{obs})$ | $0.0139$ |
| Decision at $5\%$ | $p < 0.05$ | **reject $H_0$** |
| Decision at $1\%$ | $p > 0.01$ | retain $H_0$ |

---

### (f) Effect of doubling the sample to $n=14$ (Ex 7.6b)

Suppose the same per-store summaries $\bar d = 120$, $s_d = 110$ are obtained from a sample of **$n=14$** stores (or, equivalently, two weeks of paired observations on the same 7 stores producing the same $\bar d$ and $s_d$). Three things change *systematically*, **without** recomputing from raw data — the scaling laws are exact.

**1. SE shrinks by $\sqrt 2$.**
$$\widehat{\text{SE}}_{14} \;=\; \frac{s_d}{\sqrt{14}} \;=\; \frac{110}{3.7417} \;=\; 29.399 \;=\; \frac{\widehat{\text{SE}}_7}{\sqrt 2}.$$

**2. $t_\text{obs}$ grows by $\sqrt 2$** (same numerator, smaller denominator):
$$t_\text{obs}^{(14)} \;=\; \frac{120}{29.399} \;=\; 4.0818 \;=\; t_\text{obs}^{(7)}\cdot \sqrt 2.$$

**3. df grows from 6 to 13 $\Rightarrow$ critical values shrink.**
$$t_{0.95,\,13} \;=\; 1.7709 \;<\; 1.9432 \;=\; t_{0.95,\,6}, \qquad t_{0.99,\,13} \;=\; 2.6503 \;<\; 3.1427 \;=\; t_{0.99,\,6}.$$

**4. $p$-value collapses.** Both effects push the tail probability down — larger statistic *and* lighter tail on more df:
$$p_{14} \;=\; P(t_{13} > 4.0818) \;\approx\; 0.000648,$$
i.e. **roughly 20$\times$ smaller** than $p_7 \approx 0.0139$.

| Quantity | $n=7$ | $n=14$ | Multiplier |
|---|---|---|---|
| $\widehat{\text{SE}} = s_d/\sqrt n$ | $41.576$ | $29.399$ | $1/\sqrt 2$ |
| $t_\text{obs} = \bar d/\widehat{\text{SE}}$ | $2.8863$ | $4.0818$ | $\sqrt 2$ |
| df $=n-1$ | $6$ | $13$ | — |
| $t_{0.95,\,\text{df}}$ | $1.9432$ | $1.7709$ | $\downarrow$ |
| RR threshold on $\bar D$ at $5\%$ | $80.79$ | $52.06$ | $\downarrow$ |
| $t_{0.99,\,\text{df}}$ | $3.1427$ | $2.6503$ | $\downarrow$ |
| RR threshold on $\bar D$ at $1\%$ | $130.66$ | $77.92$ | $\downarrow$ |
| One-sided $p$-value | $0.0139$ | $0.000648$ | $\div\;21$ |
| Decision at $5\%$ | reject | reject | **reinforced** |
| Decision at $1\%$ | retain | **reject** | **flips to reject** |

> *Take-away.* Doubling $n$ keeps the point estimate fixed but **(i)** shrinks the SE by $\sqrt 2$, **(ii)** scales $t_\text{obs}$ by $\sqrt 2$, **(iii)** moves the critical value *down* (heavier-to-lighter $t$-tail as df grows), and **(iv)** collapses the $p$-value by an order of magnitude. The 1%-level decision **flips** from retain to reject — the very conclusion of Ex 7.6b.

```r
# --- (f) Effect of doubling to n = 14 -------------------------------------
n14    <- 14
se14   <- sd_d / sqrt(n14);   se14             # 29.399  = se7 / sqrt(2)
tobs14 <- dbar / se14;        tobs14           # 4.0818  = tobs7 * sqrt(2)
qt(0.95, df = n14 - 1)                         # 1.7709  (5%, df=13)
qt(0.99, df = n14 - 1)                         # 2.6503  (1%, df=13)
qt(0.95, df = n14 - 1) * se14                  # 52.06   (RR on Dbar, 5%)
qt(0.99, df = n14 - 1) * se14                  # 77.92   (RR on Dbar, 1%)
1 - pt(tobs14, df = n14 - 1)                   # 0.000648 -> reject at both 5% AND 1%

# --- One-shot built-in wrapper, if raw paired data are available ----------
# TEST.mean(After - Before, mu0 = 0, alternative = "greater")
# t.test(After, Before, paired = TRUE, alternative = "greater")
```

---

### (g) Final decision

- **At $n=7$:** $p \approx 0.014$ — reject $H_0$ at $5\%$, retain at $1\%$. Wi-fi extension is *moderately* supported.
- **At $n=14$:** $p \approx 0.00065$ — reject $H_0$ at *both* $5\%$ and $1\%$. Strong evidence of a revenue increase.

In both cases the **direction** is the same (revenues went up by 120 € on average per store-week). What changes with $n$ is the **strength of evidence** that this is more than sampling noise.

---

**Linked snippets:** Ex 7.6a (paired $t$-test with $n=7$, before vs after wi-fi, RR + $p$-value at $\alpha=0.05$); Ex 7.6b (qualitative effect of doubling the sample to $n=14$ on SE, RR threshold and $p$-value — same conclusion *reinforced*, with the $1\%$ decision flipping to reject).
""",
    "images": ["statistics/images/master/master_g14c_ai.png"],
}


master_exercises["g15b_prediction"] = {
    "title": "Master Exam — Prediction intervals & CI for the mean response (consolidated)",
    "content": r"""**Setup.** A sociologist suspects that families exposed to many TV commercials end up spending more — and therefore borrowing more. A random sample of $n=430$ households is drawn from the `TeleDebt` dataframe; for each household two variables are recorded:

- $X = $ `Television` = hours per week the TV is turned on,
- $Y = $ `Debt` = total outstanding family debt in dollars.

After fitting `lm(Debt ~ Television, data=TeleDebt)` the regression summary delivers
$$\hat\beta_0 \;=\; 1479.262, \qquad \hat\beta_1 \;=\; 99.7471, \qquad s_\epsilon \;\approx\; 670, \qquad R^2 \;=\; 0.7784, \qquad n \;=\; 430.$$
The `Television` column ranges over roughly $[5,\,40]$ hours/week with $\bar x \approx 32$; this is the *modelled support* — anything outside it is extrapolation.

The questions in this master:

> For a household that watches TV $x_0$ hours/week, what is our best **point prediction** of its debt? With what **margin**? And how does the answer change if we want the **average** debt of *all* such families instead of the debt of *one* particular family?

---

### Part (a) — Point prediction $\hat y_0 = \hat\beta_0 + \hat\beta_1 x_0$ (single number, two interpretations).

Once the line is fitted, both the *best guess* for one new family and the *best guess* for the population mean at $X=x_0$ collapse to the **same number** — plug $x_0$ into the equation:
$$\hat y_0 \;=\; \hat\beta_0 + \hat\beta_1 x_0.$$

For TeleDebt at $x_0 = 33$:
$$\hat y_0 \;=\; 1479.262 + 99.7471 \cdot 33 \;=\; 1479.262 + 3291.654 \;=\; \boxed{4770.92 \;\text{\$}}.$$

That single number is simultaneously the
- **estimator** of $E[Y\mid X=33]$ (the *mean* debt across all families with $\text{TV}=33$), and
- **forecast** of $Y_0$ for *one specific* new family with $\text{TV}=33$.

What distinguishes the two questions is **how uncertain the answer is** — that is, the **standard error**, not the point estimate.

```r
b0 <- 1479.262;  b1 <- 99.7471
x0 <- 33
yhat <- b0 + b1*x0;   yhat        # 4770.92
```

---

### Part (b) — Two error sources: line uncertainty vs irreducible noise.

The data-generating model is $Y_i = \beta_0 + \beta_1 X_i + \varepsilon_i$ with $\varepsilon_i \sim \mathcal{N}(0, \sigma_\epsilon^2)$ i.i.d. and independent of $X$. Now compare the **two questions** about a target value $x_0$:

| Quantity | Target | Sources of uncertainty |
|---|---|---|
| **CI for the mean** $E[Y\mid X=x_0]$ | a *population parameter* | only **estimation noise** in $(\hat\beta_0, \hat\beta_1)$ — i.e. *line position* at $x_0$ |
| **PI for an individual** $Y_0$ | a *future random outcome* | line-position noise **plus** the irreducible scatter $\varepsilon_0$ of a brand-new draw |

Algebraically, write $\hat Y_0 = \hat\beta_0 + \hat\beta_1 x_0$ and $Y_0 = \beta_0 + \beta_1 x_0 + \varepsilon_0$:

$$\underbrace{\Var(\hat Y_0)}_{\text{line uncertainty at }x_0} \;=\; \sigma_\epsilon^2 \left[\frac{1}{n} + \frac{(x_0 - \bar x)^2}{(n-1)s_x^2}\right] \;\equiv\; \sigma_\epsilon^2 \cdot h(x_0),$$

$$\underbrace{\Var(Y_0 - \hat Y_0)}_{\text{forecast error}} \;=\; \underbrace{\sigma_\epsilon^2}_{\varepsilon_0} \;+\; \underbrace{\sigma_\epsilon^2\, h(x_0)}_{\text{line noise}} \;=\; \sigma_\epsilon^2\,[\,1 + h(x_0)\,].$$

The "$1+$" inside the bracket is **the** difference between a PI and a CI — it adds the variance of the *single* future $\varepsilon_0$ that the CI does not need to forecast (the CI targets a non-random parameter).

---

### Part (c) — The two standard errors.

Plugging in the residual-standard-error estimate $s_\epsilon$:
$$\text{SE}_{\text{mean}}(x_0) \;=\; s_\epsilon\,\sqrt{\frac{1}{n} + \frac{(x_0-\bar x)^2}{(n-1)s_x^2}}, \qquad \text{SE}_{\text{pred}}(x_0) \;=\; s_\epsilon\,\sqrt{1 + \frac{1}{n} + \frac{(x_0-\bar x)^2}{(n-1)s_x^2}}.$$

Equivalently $\text{SE}_{\text{pred}}^2 = \text{SE}_{\text{mean}}^2 + s_\epsilon^2$ — the **residual variance adds** to the line-position variance. Two extremes make this crystal clear:

- **Sample-mean tower.** At $x_0 = \bar x$ the leverage $(x_0-\bar x)^2$ term vanishes, so $\text{SE}_{\text{mean}} = s_\epsilon/\sqrt n$ (familiar one-sample SE on $\bar Y$). $\text{SE}_{\text{pred}}$ still carries $s_\epsilon\sqrt{1+1/n} \to s_\epsilon$ — it never shrinks to zero, no matter how large $n$ is.
- **Limit $n \to \infty$.** $\text{SE}_{\text{mean}} \to 0$ (we eventually pin the line *exactly*), but $\text{SE}_{\text{pred}} \to s_\epsilon$ — there is **always** the noise of the next draw, even when $\beta_0, \beta_1$ are known. **No amount of data can shrink a PI below $\sim s_\epsilon$.**

---

### Part (d) — 99% PI at $x_0 = 33$ on TeleDebt: the wide interval.

For TeleDebt, $x_0=33$ is essentially at the centre of the sample (close to $\bar x$), so the leverage term is negligible and
$$\text{SE}_{\text{pred}}(33) \;\approx\; s_\epsilon\sqrt{1 + 1/n} \;\approx\; 670\sqrt{1 + 1/430} \;\approx\; 670.78.$$
With $\alpha=0.01$ and df $=n-2=428$, $t_{0.995,\,428}\approx 2.587$. The half-width is
$$\text{ME}_{\text{PI}} \;=\; 2.587 \cdot 670.78 \;\approx\; 1733.78,$$
and the 99% PI is
$$4770.92 \;\pm\; 1733.78 \;=\; \boxed{[\,3037.14,\; 6504.69\,]\;\text{\$}.}$$

```r
n     <- 430;  s_e   <- 670
x0    <- 33;   xbar  <- 32                     # x0 near sample mean
lev   <- 1/n + (x0 - xbar)^2 / ((n-1)*var.x)   # tiny here
se_pred <- s_e * sqrt(1 + lev);   se_pred       # ~ 670.78
tcrit   <- qt(0.995, df = n-2);   tcrit         # ~ 2.587
ME_pi   <- tcrit * se_pred;       ME_pi         # ~ 1733.78
yhat    <- 4770.92
c(yhat - ME_pi, yhat + ME_pi)                    # [3037.14, 6504.69]

# One-shot equivalent via predict():
mod <- lm(Debt ~ Television, data = TeleDebt)
predict(mod, newdata = data.frame(Television = 33),
        interval = "prediction", level = 0.99)
##        fit       lwr       upr
## 1   4770.92   3037.14   6504.69
```

**Interpretation.** With 99% confidence, a *single* family that watches $33$ hours of TV per week has debt between **\$3 037 and \$6 505**. The interval reflects (i) line-position noise (tiny at the centre) and (ii) the irreducible scatter $\varepsilon_0$ around the line (dominant).

---

### Part (e) — 99% CI for the mean response at $x_0 = 33$: the narrow interval.

Drop the "$1+$" inside the radical:
$$\text{SE}_{\text{mean}}(33) \;\approx\; s_\epsilon\sqrt{\,1/n + (x_0-\bar x)^2/((n-1)s_x^2)\,} \;\approx\; 670\sqrt{1/430 + \text{small}} \;\approx\; 32.32 \;\;(\text{leverage at }x_0\approx\bar x\text{ is dominated by the }1/n\text{ term}),$$
$$\text{ME}_{\text{CI}} \;=\; 2.587 \cdot 32.32 \;\approx\; 83.61, \qquad \text{CI}_{99\%} \;=\; 4770.92 \pm 83.61 \;=\; \boxed{[\,4687.31,\;4854.53\,]\;\text{\$}.}$$

```r
se_mean <- s_e * sqrt(lev);       se_mean        # ~ 32.32
ME_ci   <- tcrit * se_mean;       ME_ci          # ~ 83.61
c(yhat - ME_ci, yhat + ME_ci)                    # [4687.31, 4854.53]

# One-shot equivalent via predict():
predict(mod, newdata = data.frame(Television = 33),
        interval = "confidence", level = 0.99)
##        fit       lwr       upr
## 1   4770.92   4687.31   4854.53
```

**Interpretation.** With 99% confidence, the **average** debt across **all** families with $\text{TV}=33$ hours/week lies in $[\$4687,\,\$4855]$ — a much narrower window because we no longer have to absorb the noise of one specific household.

---

### Part (f) — Side-by-side PI vs CI: PI is **always** wider.

| Quantity | $\sqrt{\cdot}$ factor | SE at $x_0=33$ | ME (99%) | Interval |
|---|---|---|---|---|
| CI for mean $E[Y\mid X=33]$ | $\sqrt{1/n + (x_0-\bar x)^2/((n-1)s_x^2)}$ | $\approx 32.32$ | $83.61$ | $[4687,\,4855]$ |
| **PI** for a single $Y_0(33)$ | $\sqrt{1 + 1/n + (x_0-\bar x)^2/((n-1)s_x^2)}$ | $\approx 670.78$ | $1733.78$ | $[3037,\,6505]$ |

Ratio of half-widths: $1733.78 / 83.61 \approx 20.7$ — the PI is roughly **21× wider** than the CI on this dataset. The exact algebraic identity is
$$\text{SE}_{\text{pred}}^2 - \text{SE}_{\text{mean}}^2 \;=\; s_\epsilon^2 \;\;\Rightarrow\;\; \frac{\text{SE}_{\text{pred}}}{\text{SE}_{\text{mean}}} \;=\; \sqrt{1 + \frac{1}{h(x_0)}}.$$
With large $n$ and $x_0$ near $\bar x$, $h(x_0) \approx 1/n$ is tiny, so the ratio explodes — exactly what we see. **The PI is the conservative tool when "one specific future observation" is what we care about.**

---

### Part (g) — Why both intervals widen as $x_0$ moves away from $\bar x$ (leverage curve).

Inside both square roots sits the **leverage term**
$$\frac{(x_0 - \bar x)^2}{(n-1) s_x^2}.$$
This is *quadratic* in the distance $|x_0 - \bar x|$, so:

1. **At the centre $x_0 = \bar x$** the leverage is zero — both intervals are at their **narrowest**.
2. **Move out** in either direction and the leverage grows; both SEs grow, both intervals fan out — the so-called *prediction-band hourglass*.
3. The fan is *symmetric in $x_0$* around $\bar x$ (it depends only on $(x_0-\bar x)^2$).

Geometric reason: the fitted line is anchored at $(\bar x, \bar y)$ and *pivots* around that point under sampling noise of $\hat\beta_1$. The further out you go, the more a small wobble in slope translates into a vertical wobble of $\hat y_0$. **The PI inherits this fan and adds a constant $s_\epsilon$ "floor" on top.**

```r
# Numerical check: leverage at a near-centre point and at a far one
lev_at  <- function(x0, xbar, n, var.x) 1/n + (x0 - xbar)^2 / ((n-1)*var.x)
xbar    <- 32;  n <- 430;  var.x <- 50          # toy var.x for illustration
lev_at(33, xbar, n, var.x)                      # ~ 0.00237  (near-centre)
lev_at(45, xbar, n, var.x)                      # ~ 0.00929  (far)  -> wider PI

# Visual check: leverage curve fans out as x0 leaves xbar
newdf <- data.frame(Television = seq(5, 40, length = 100))
pi    <- predict(mod, newdata = newdf, interval = "prediction", level = 0.99)
ci    <- predict(mod, newdata = newdf, interval = "confidence", level = 0.99)
matplot(newdf$Television, cbind(pi, ci[,2:3]), type="l",
        lty = c(1,2,2,3,3), col = c("black","red","red","blue","blue"))
```

---

### Part (h) — Sales-on-discount cross-check (Ex 8.10): PI vs CI at $x_0 = 12$.

Different dataset, same machinery. With $n=50$, $\bar x = 10$, $\sum(x_i-\bar x)^2 = 25\,000$, $s_\epsilon^2 = 83.33$, $\hat\beta_0=48$, $\hat\beta_1=1.2$:
$$\hat y(12) \;=\; 48 + 1.2 \cdot 12 \;=\; 62.4.$$

Leverage at $x_0=12$:
$$h(12) \;=\; \tfrac{1}{50} + \tfrac{(12-10)^2}{25\,000} \;=\; 0.02 + 0.00016 \;=\; 0.02016.$$

With $t_{0.975,\,48}\approx 2.011$ and $s_\epsilon=\sqrt{83.33}=9.129$:
$$\text{ME}_{\text{PI}} \;=\; 2.011 \cdot 9.129 \cdot \sqrt{1 + 0.02016} \;\approx\; 18.54 \;\Rightarrow\; \text{PI}_{95\%} \;=\; (43.86,\,80.94),$$
$$\text{ME}_{\text{CI}} \;=\; 2.011 \cdot 9.129 \cdot \sqrt{\;\;\;\;\;\,0.02016} \;\approx\; \;\,2.61 \;\Rightarrow\; \text{CI}_{95\%} \;=\; (59.79,\,65.01).$$

Ratio $18.54/2.61 \approx 7.1$ — the PI is again much wider. (The ratio is smaller than on TeleDebt because $n$ is smaller, so the line-position SE is not negligible relative to $s_\epsilon$.)

---

### Part (i) — Extrapolation at $x_0 = 30$: **risky** even with the leverage term.

The leverage factor in the PI/CI formula already inflates the half-width as $x_0$ leaves $\bar x$ — so isn't that "honest enough"? **No.** The formula's inflation is *inside the linear-mean model*: it tells you how uncertain $\hat y_0$ is **assuming** $E[Y\mid X]=\beta_0+\beta_1 X$ still holds at $x_0$. Outside the modelled range, *the assumption itself* may break.

For Ex 8.10 the discount data runs $X \in [\,\bar x - 2 s_x,\,\bar x + 2 s_x\,] \approx [5,\,25]$ but the question asks for $x_0 = 30$ — **outside** the support. Two things go wrong:

1. **Mean function might bend.** Beyond observed data the relationship could plateau (saturation: $30\%$ discount may not stimulate more purchases) or even reverse (signal that the product is defective). The leverage formula has **no way** to detect this; it just keeps drawing a straight line.
2. **Variance might also bend.** Heteroscedasticity often grows at the extremes — a single $s_\epsilon$ becomes a bad summary of true scatter.

Numerically, the formula still produces an interval:
$$h(30) = \tfrac{1}{50} + \tfrac{(30-10)^2}{25\,000} = 0.02 + 0.016 = 0.036, \quad \text{ME}_{\text{PI}} = 2.011\cdot 9.129\cdot\sqrt{1.036} \;\approx\; 18.69,$$
$$\hat y(30) = 48 + 1.2\cdot 30 = 84 \;\Rightarrow\; \text{PI}_{95\%} \approx (65.32,\,102.68).$$
The interval is **wider** ($\pm 18.69$ vs $\pm 18.54$ at $x_0=12$) — but the **honest** uncertainty is much larger because the model itself may not apply. **Rule of thumb:** flag any prediction with $x_0$ outside $[\,\min(x_i),\,\max(x_i)\,]$ as *extrapolation* and refuse to publish a single number without strong domain-knowledge justification.

```r
# At an extrapolation point, the formula still gives a number — DO NOT trust it
x0   <- 30
yhat <- 48 + 1.2*x0                                   # 84
lev  <- 1/50 + (x0 - 10)^2 / 25000                    # 0.036
ME   <- qt(0.975, df=48) * sqrt(83.33) * sqrt(1+lev)  # ~ 18.69
c(yhat - ME, yhat + ME)                                # (65.32, 102.68) -- extrapolation!

# Compare with the AmountSpent-on-Salary case (Ex 8.2b):
# At Salary=0, observed Salary ranges [10000, 170000] -> 0 is FAR outside support.
# Naive PI even ends up centred on a negative point prediction yhat(0) = -15.7$.
```

---

### Summary table (TeleDebt; $\hat\beta_0=1479.262$, $\hat\beta_1=99.7471$, $s_\epsilon\approx 670$, $n=430$)

| Quantity | Value | Where |
|---|---|---|
| Point prediction $\hat y_0$ at $x_0=33$ | $4770.92$ \$ | Part (a) |
| $\text{SE}_{\text{mean}}(33)$ | $\approx 32.32$ \$ | Part (e) |
| $\text{SE}_{\text{pred}}(33)$ | $\approx 670.78$ \$ | Part (d) |
| Identity $\text{SE}_{\text{pred}}^2 - \text{SE}_{\text{mean}}^2 = s_\epsilon^2$ | $670.78^2 - 32.32^2 \approx 670^2$ | Part (c) |
| $t_{0.995,\,428}$ | $\approx 2.587$ | Part (d) |
| 99% CI for $E[Y\mid X=33]$ | $[4687.31,\,4854.53]$ \$ | Part (e) |
| 99% PI for $Y_0$ at $X=33$ | $[3037.14,\,6504.69]$ \$ | Part (d) |
| PI / CI half-width ratio | $\approx 20.7\times$ | Part (f) |
| 95% PI at $x_0=12$ (Ex 8.10) | $(43.86,\,80.94)$ | Part (h) |
| 95% CI at $x_0=12$ (Ex 8.10) | $(59.79,\,65.01)$ | Part (h) |
| 95% PI at $x_0=30$ (extrapolation, Ex 8.10) | $(65.32,\,102.68)$ — **risky** | Part (i) |

**Master take-aways.**
1. **One point estimate, two questions.** $\hat y_0 = \hat\beta_0 + \hat\beta_1 x_0$ is *both* the estimator of $E[Y\mid X=x_0]$ and the forecast of a single $Y_0$ — the questions differ only in *uncertainty*, not in the point.
2. **PI = CI plus irreducible noise.** $\text{SE}_{\text{pred}}^2 = \text{SE}_{\text{mean}}^2 + s_\epsilon^2$. The "$1+$" inside the radical encodes the variance of the next $\varepsilon_0$ — a CI doesn't carry it because it targets a parameter, not a future draw.
3. **PI is always wider than CI** at the same $x_0$ and level — by a factor that can be enormous when $n$ is large and $x_0$ is near $\bar x$ ($\approx 22\times$ on TeleDebt).
4. **PI has a floor.** As $n\to\infty$, CI shrinks to $0$ but PI tends to $s_\epsilon \cdot t_{\alpha/2}$ — more data **cannot** save you from $\varepsilon_0$.
5. **Both intervals widen as $x_0$ moves away from $\bar x$** via the leverage term $(x_0-\bar x)^2/((n-1)s_x^2)$ — the "fan" or "hourglass" prediction band. Minimum width at $x_0 = \bar x$, growing quadratically with distance.
6. **Extrapolation $\Rightarrow$ risky.** The leverage term inflates intervals *inside* the linear-mean model only. Outside the observed support of $X$, the mean function or the variance may break — and the formula has no way to tell. Refuse to publish PI/CI at $x_0$ outside $[\min(x_i),\,\max(x_i)]$ without strong domain backing.
7. **R one-liners.** `predict(mod, newdata=..., interval="prediction"|"confidence", level=...)`. Both are reported as `fit / lwr / upr`; only the `interval=` argument changes.
8. **Causality cap (carried over from Ex 8.1c).** Even a tight CI for $E[Y\mid X=x_0]$ documents *association*, not causation; observational data and confounders (income, household size, age, reverse causality) forbid claims like "lowering $X$ would lower mean $Y$".

---

**Linked snippets:** Ex 8.1c (99% PI and CI at $\text{Television}=33$ on TeleDebt — *the* source dataset for this master, both intervals computed, $22\times$ width ratio, causality cap); Ex 8.2b (PI at $\text{Salary}=0$ on AmountSpent — extrapolation diagnosis far below the support of $X$, **plus** heteroscedasticity invalidating $s_\epsilon$); Ex 8.10a (sales-on-discount cross-check: 95% PI vs 95% CI at $x_0=12$ in-sample, and 95% PI at $x_0=30$ as an extrapolation warning — same machinery, smaller $n$, smaller PI/CI ratio).

![Master G15b — fitted line with CI and PI bands, leverage curve, CI vs PI, extrapolation](statistics/images/master/master_g15b_ai.png)
""",
    "images": ["statistics/images/master/master_g15b_ai.png"],
}


master_exercises["g14d_chi_squared"] = {
    "title": "Master Exam — Chi-squared (GoF + independence) (consolidated)",
    "content": r"""**Setup.** A retail chain monitors customer behaviour via the categorical variable `History`, a four-level summary of past purchase activity with levels **None**, **Low**, **Medium**, **High**. The data analyst also records each customer's `Location` (proximity to a competing physical store: **Close** vs **Far**). A random sample of $n=750$ customers gives the following two views of the same data:

**View 1 — marginal counts of `History`** (for the goodness-of-fit test):

| History | None | Low | Medium | High | total |
|---|---|---|---|---|---|
| Observed $O_k$ | 181 | 150 | 205 | 214 | $n=750$ |

**View 2 — `History` $\times$ `Location` contingency table** (for the independence test):

| | Close | Far | row total |
|---|---|---|---|
| **None**   | 100 | 81  | 181 |
| **Low**    |  60 | 90  | 150 |
| **Medium** | 110 | 95  | 205 |
| **High**   |  90 | 124 | 214 |
| **col total** | **360** | **390** | **750** |

Both views are useful, but they answer **different** questions:

- *GoF:* does **one** categorical variable follow a **fully specified** distribution? (Here: is `History` **uniform** across the four levels?)
- *Independence:* are **two** categorical variables (`History` and `Location`) **statistically independent**?

The two tests share the same machinery — a Pearson statistic $X^2 = \sum (O-E)^2/E$ that under $H_0$ is approximately $\chi^2$ — but **differ in (i) how $E$ is built and (ii) the degrees of freedom**.

---

### Part (a) — Goodness-of-fit: is `History` uniform?

Take a single categorical variable with $K$ levels and **hypothesised** probabilities $(p_1^0,\dots,p_K^0)$ summing to 1. Here the manager wants to test whether the four `History` levels are equally likely, i.e. $p_k^0 = 0.25$ for every $k$.

**Hypotheses.**
$$H_0:\; p_{\text{None}} = p_{\text{Low}} = p_{\text{Medium}} = p_{\text{High}} = 0.25
\quad \text{vs} \quad H_1:\; \text{at least one } p_k \ne 0.25.$$

**Expected counts under $H_0$.** With a fully specified null and total sample size $n$,
$$E_k \;=\; n\,p_k^0 \;=\; 750 \times 0.25 \;=\; 187.5 \quad \text{for every } k.$$
The expected counts do **not** depend on the data — they are dictated entirely by $H_0$.

**Pearson statistic & null distribution.**
$$X^2 \;=\; \sum_{k=1}^{K} \frac{(O_k - E_k)^2}{E_k} \;\overset{H_0}{\dot\sim}\; \chi^2_{K-1-p},$$
where $K$ is the number of levels and $p$ is the **number of parameters estimated from the data** to build $E_k$. Here $H_0$ is **fully specified** (we plug in $p_k^0=0.25$ — nothing is estimated), so $p=0$ and
$$\text{df} \;=\; K - 1 - p \;=\; 4 - 1 - 0 \;=\; 3.$$
If we had instead tested a *parametric family* (e.g. "Poisson with some rate" estimating $\hat\lambda$ from the data, or "Normal with $\hat\mu,\hat\sigma$"), we would subtract one df per estimated parameter — see Part (e).

**Per-cell contributions and the realised statistic.**

| level | $O_k$ | $E_k$ | $O_k-E_k$ | $(O_k-E_k)^2/E_k$ |
|---|---|---|---|---|
| None   | 181 | 187.5 |  $-6.5$  | $0.2253$ |
| Low    | 150 | 187.5 | $-37.5$  | $7.5000$ |
| Medium | 205 | 187.5 | $+17.5$  | $1.6333$ |
| High   | 214 | 187.5 | $+26.5$  | $3.7453$ |
| **sum** | $n=750$ | $n=750$ | $0$ | $X^2_\text{obs} = \mathbf{13.104}$ |

The **Low** cell dominates the statistic: its deficit of 37.5 customers vs the uniform expectation contributes 7.5 — more than half of $X^2_\text{obs}$.

**Critical value & p-value.** At $\alpha=0.05$ on $\chi^2_3$,
$$\chi^2_{3,\,0.95} \;=\; \texttt{qchisq(0.95, df=3)} \;=\; 7.815,
\qquad p\text{-value} \;=\; \Pr(\chi^2_3 \geq 13.104) \;=\; 0.00442.$$

**Decision.** $X^2_\text{obs}=13.104 > 7.815$ (equivalently $p=0.0044<0.05$) → **reject $H_0$**: the four `History` levels are **not** equally probable. Note the rejection holds even at $\alpha=0.01$ (since $13.104 > \chi^2_{3,\,0.99}=11.345$), so the imbalance is *not* a borderline call.

```r
# Goodness-of-fit: uniform null over the 4 levels of History
O   <- c(None=181, Low=150, Medium=205, High=214)
p0  <- rep(0.25, 4)
E   <- sum(O) * p0;   E                              # 187.5 each
X2  <- sum((O - E)^2 / E);  X2                       # 13.104

qchisq(0.95, df = 3)                                  # 7.815
1 - pchisq(X2,  df = 3)                               # 0.00442  (p-value)

# Built-in equivalent:
chisq.test(x = O, p = p0)                             # X-sq=13.104, df=3, p=0.0044
```

![Master illustration](statistics/images/master/master_g14d_ai.png)

---

### Part (b) — Independence: is `History` related to `Location`?

Now we have a $4\times 2$ contingency table $O_{ij}$ and want to test whether `History` (row variable, $r=4$ levels) and `Location` (column variable, $c=2$ levels) are statistically **independent**.

**Hypotheses.**
$$H_0:\; \text{History} \perp\!\!\!\perp \text{Location}
\quad \text{vs} \quad H_1:\; \text{they are associated.}$$

**Expected counts under $H_0$.** Independence means $P(\text{row}=i,\,\text{col}=j) = p_{i\cdot}\,p_{\cdot j}$. The marginal probabilities are *unknown*, so we estimate them from the row/column totals: $\hat p_{i\cdot} = n_{i\cdot}/n$ and $\hat p_{\cdot j} = n_{\cdot j}/n$. Then
$$\widehat E_{ij} \;=\; n\,\hat p_{i\cdot}\,\hat p_{\cdot j} \;=\; \frac{n_{i\cdot}\,n_{\cdot j}}{n}.$$
Unlike the GoF case, the expected counts here are **estimated from the data** — specifically from the row and column marginals.

Computing $\widehat E_{ij} = (\text{row}_i \cdot \text{col}_j)/750$ for every cell:

| | Close | Far |
|---|---|---|
| **None**   | $181\cdot 360/750 = 86.88$  | $181\cdot 390/750 = 94.12$  |
| **Low**    | $150\cdot 360/750 = 72.00$  | $150\cdot 390/750 = 78.00$  |
| **Medium** | $205\cdot 360/750 = 98.40$  | $205\cdot 390/750 = 106.60$ |
| **High**   | $214\cdot 360/750 = 102.72$ | $214\cdot 390/750 = 111.28$ |

**Validity check (Cochran's rule):** all $\widehat E_{ij} \ge 5$ — in fact $\widehat E_{ij} \ge 72$ everywhere — so the $\chi^2$ approximation is valid (see Part (d)).

**Pearson statistic & null distribution.**
$$X^2 \;=\; \sum_{i=1}^{r}\sum_{j=1}^{c} \frac{(O_{ij}-\widehat E_{ij})^2}{\widehat E_{ij}}
\;\overset{H_0}{\dot\sim}\; \chi^2_{(r-1)(c-1)}.$$
The df arise from a parameter count: the $r\times c$ table has $rc-1$ free probabilities under $H_1$; under independence we only need $(r-1)+(c-1)$ marginal probabilities, so $\text{df}=(rc-1)-[(r-1)+(c-1)] = (r-1)(c-1)$. Here:
$$\text{df} \;=\; (4-1)(2-1) \;=\; 3.$$

**Per-cell contributions and the realised statistic.**

| cell | $O_{ij}$ | $\widehat E_{ij}$ | $(O-\widehat E)^2/\widehat E$ |
|---|---|---|---|
| None, Close   | 100 | 86.88  | $1.981$ |
| None, Far     |  81 | 94.12  | $1.828$ |
| Low, Close    |  60 | 72.00  | $2.000$ |
| Low, Far      |  90 | 78.00  | $1.846$ |
| Medium, Close | 110 | 98.40  | $1.368$ |
| Medium, Far   |  95 | 106.60 | $1.262$ |
| High, Close   |  90 | 102.72 | $1.575$ |
| High, Far     | 124 | 111.28 | $1.455$ |
| **sum** | $750$ | $750$ | $X^2_\text{obs} = \mathbf{13.315}$ |

**Critical value & p-value.** Same df=3 as in part (a), so the threshold is the same:
$$\chi^2_{3,\,0.95} \;=\; 7.815, \qquad p\text{-value} \;=\; \Pr(\chi^2_3 \geq 13.315) \;=\; 0.00400.$$

**Decision.** $X^2_\text{obs}=13.315 > 7.815$ (equivalently $p=0.004<0.05$) → **reject $H_0$**: `History` and `Location` are **associated**. Inspecting the residuals, **Low** is over-represented Far and under-represented Close, while **None** shows the reverse — proximity to a competitor changes the customer mix.

```r
# Independence: 4x2 contingency table  History x Location
O <- matrix(c(100, 81,
               60, 90,
              110, 95,
               90, 124),
            nrow = 4, byrow = TRUE,
            dimnames = list(History  = c("None","Low","Medium","High"),
                            Location = c("Close","Far")))

rsum <- rowSums(O);   csum <- colSums(O);   n <- sum(O)
E    <- outer(rsum, csum) / n;   E                   # all >= 72  -> OK
X2   <- sum((O - E)^2 / E);      X2                  # 13.315

df   <- (nrow(O) - 1) * (ncol(O) - 1);  df           # 3
qchisq(0.95, df = df)                                # 7.815
1 - pchisq(X2,  df = df)                             # 0.00400  (p-value)

# Built-in equivalent (Pearson, no continuity correction)
chisq.test(O, correct = FALSE)                       # X-sq=13.315, df=3, p=0.004
# Inspect *where* the dependence lives:
chisq.test(O, correct = FALSE)$stdres                # |z| > ~2 == driver cell
```

---

### Part (c) — Side-by-side: GoF vs Independence.

The two tests share a common skeleton but differ in what is "known" vs "estimated":

| Aspect | Goodness of fit (Part a) | Independence (Part b) |
|---|---|---|
| Variables | one categorical, $K$ levels | two categorical, $r\times c$ levels |
| Null specification | fully specified $p_k^0$ | unspecified — built from marginals |
| Expected count $E$ | $n\,p_k^0$ (no estimation) | $n_{i\cdot}\,n_{\cdot j}/n$ (marginals estimated) |
| Parameters estimated, $p$ | 0 (or $\ge 1$ if family is parametric — see Part e) | $(r-1)+(c-1)$ marginal probs |
| Degrees of freedom | $K - 1 - p$ | $(r-1)(c-1)$ |
| df **here** | $4-1-0=3$ | $(4-1)(2-1)=3$ |
| $X^2_\text{obs}$ | $13.104$ | $13.315$ |
| Critical $\chi^2_{3,\,0.95}$ | $7.815$ | $7.815$ |
| p-value | $0.0044$ | $0.0040$ |
| Decision at $\alpha=0.05$ | reject $H_0$ — non-uniform | reject $H_0$ — associated |

Both tests reject at $\alpha=0.05$ — but they reject *different* nulls. The GoF result alone could have come from a perfectly *independent* but *non-uniform* `History`; the independence result alone could have come from a *uniform* `History` with a stratum effect. The two are **complementary**, not redundant.

---

### Part (d) — Validity: why all $\widehat E_{ij} \geq 5$ matters.

The $\chi^2$ approximation rests on a **normal approximation** to each cell count: $(O_{ij}-E_{ij})/\sqrt{E_{ij}} \approx \mathcal{N}(0,1)$ under $H_0$. This breaks down when $E_{ij}$ is small because (i) the count is approximately Poisson with a fat right tail and (ii) the squared ratio $(O-E)^2/E$ is dominated by tiny denominators, producing spuriously large $X^2$. The standard **Cochran rule** (and the warning R prints) is

> **All $\widehat E_{ij} \ge 5$** (or, more leniently, *at most* 20% of cells with $\widehat E_{ij} < 5$ and **no** cell with $\widehat E_{ij}<1$).

Here the **smallest** $\widehat E_{ij}$ is $72.00$, so we are nowhere near the boundary — the $\chi^2_3$ approximation is reliable. **If** a cell had $\widehat E_{ij} < 5$ (a real risk in fine-grained 5×5 tables — exactly what happens in the linked Ex 7.7b's part (c), where `AISearch=Other` has $\widehat E \approx 2.77$), the remedies are: (i) **merge** sparse categories with a substantive neighbour; (ii) switch to **Fisher's exact test** (`fisher.test`); or (iii) **simulate** the p-value (`chisq.test(..., simulate.p.value=TRUE, B=1e4)`).

```r
# Diagnostic: report expected counts and Cochran's rule
E <- chisq.test(O, correct=FALSE)$expected
min(E);                                              # 72.00   -> safely >= 5
mean(E < 5)                                          # 0       -> 0% of cells
# If the rule failed, prefer:
fisher.test(O)                                       # exact, no asymptotics
chisq.test(O, correct=FALSE, simulate.p.value=TRUE, B=10000)
```

---

### Part (d.5) — Two cross-check GoF cases: non-uniform null *rejected*, uniform null *retained*.

The master so far showcased a single GoF where uniform $H_0$ is **rejected**. Two linked snippets sharpen the lesson by varying both the null shape and the conclusion.

**(i) Non-uniform fully-specified null — Ex 7.9a (`DS$Children` vs Italian distribution).** Here $H_0$ is *not* uniform: $p_0 = (0.76,\,0.13,\,0.09,\,0.02)$ for $K=4$ ordered categories $0,1,2,3+$ children. Observed $O=(360,184,111,95)$ on $n=750$ give expected $E = n\,p_0 = (570,\,97.5,\,67.5,\,15)$ — all $\ge 5$, valid. The Pearson statistic is
$$X^2 = \tfrac{(360-570)^2}{570} + \tfrac{(184-97.5)^2}{97.5} + \tfrac{(111-67.5)^2}{67.5} + \tfrac{(95-15)^2}{15} = 77.37+76.74+28.03+426.67 \;=\; 608.81,$$
with df $=K-1=3$ (fully specified $p_0$, no parameter estimation). Critical $\chi^2_{3,0.95}=7.815$, p-value $\approx 0$ — **reject decisively**. The "3+" cell alone contributes $426.67$ out of $608.81$: the `DS` customer base hugely **over-represents** large families relative to the national distribution. *Take-away:* the GoF machinery handles any fully-specified $p_0$, not just the uniform — only $E_k$ changes.

**(ii) Uniform null *retained* — Ex 7.9b (4 supermarket entrances).** Same uniform null as Part (a) — $H_0: p_k=1/4$ — but on much smaller, more even data: $O=(24,30,36,40)$ on $n=130$, so $E_k = 32.5$ everywhere. The statistic is
$$X^2 = \tfrac{(24-32.5)^2+(30-32.5)^2+(36-32.5)^2+(40-32.5)^2}{32.5} = \tfrac{72.25+6.25+12.25+56.25}{32.5} = \tfrac{147}{32.5} \;\approx\; 4.523,$$
df $=3$, p-value $=\Pr(\chi^2_3>4.523)\approx 0.2104 > 0.05$ — **do not reject**. Entrance IV is mildly busier than the others, but the four counts are well within sampling noise around equal usage. *Take-away:* a non-significant GoF documents *compatibility* with the null (never "proof"), and the same template (df=$K-1$, upper-tail $\chi^2$) decides both cases.

| Case | Null shape | $n$ | $X^2_\text{obs}$ | df | p-value | Decision @ $\alpha=0.05$ |
|---|---|---|---|---|---|---|
| Master (a) — `History` uniform | $(0.25,0.25,0.25,0.25)$ | 750 | 13.104 | 3 | 0.0044 | **reject** |
| Ex 7.9a — `Children` vs IT | $(0.76,0.13,0.09,0.02)$ | 750 | 608.81 | 3 | $\approx 0$ | **reject** |
| Ex 7.9b — 4 entrances | $(0.25,0.25,0.25,0.25)$ | 130 | 4.523 | 3 | 0.2104 | retain |

```r
# (i) Non-uniform p0 — Ex 7.9a
chisq.test(c(360,184,111,95), p = c(0.76, 0.13, 0.09, 0.02))
# X-sq = 608.81, df = 3, p < 2.2e-16

# (ii) Uniform p0, small n — Ex 7.9b
chisq.test(c(24, 30, 36, 40), p = rep(1/4, 4))
# X-sq = 4.523, df = 3, p = 0.2104
```

---

### Part (e) — Df when parameters are estimated (composite GoF nulls).

The general rule is
$$\text{df}_\text{GoF} \;=\; K \;-\; 1 \;-\; p,$$
where $p$ is the number of free parameters of the null **family** that are estimated from the data. Examples:

| Null model | Free parameters $p$ | df (with $K$ cells) |
|---|---|---|
| Uniform / fully specified $(p_1^0,\dots,p_K^0)$ | $0$ | $K-1$ |
| Poisson$(\lambda)$ — $\hat\lambda$ from data | $1$ | $K-2$ |
| Normal$(\mu,\sigma^2)$ — $\hat\mu,\hat\sigma$ from data | $2$ | $K-3$ |
| Binomial$(m,p)$ with $m$ known, $\hat p$ from data | $1$ | $K-2$ |

The "$-1$" reflects the constraint $\sum p_k = 1$ (one probability is determined by the others); the "$-p$" reflects each extra parameter you let the data choose. **Independence** is a special case: $p=(r-1)+(c-1)$ marginal probabilities, giving $(r-1)(c-1)$ df. Estimating parameters *can change the conclusion*: e.g. with $K=4$ and a Poisson null we would have df=2, $\chi^2_{2,\,0.95}=5.991<7.815$ — easier to reject for the same observed $X^2$.

```r
# How df shrinks as you estimate more parameters
K <- 4
qchisq(0.95, df = K - 1 - 0)        # 7.815   (uniform / fully specified)
qchisq(0.95, df = K - 1 - 1)        # 5.991   (Poisson, 1 par estimated)
qchisq(0.95, df = K - 1 - 2)        # 3.841   (Normal, 2 par estimated)
```

---

### Summary table (master dataset, $n=750$).

| Quantity | GoF (uniform `History`) | Independence (`History` $\times$ `Location`) |
|---|---|---|
| Null | $p_k^0=0.25$ (fully specified) | rows $\perp$ cols (marginals estimated) |
| $E$ formula | $E_k = n\,p_k^0 = 187.5$ | $\widehat E_{ij} = n_{i\cdot}n_{\cdot j}/n$ |
| Cells | 4 | 8 |
| Smallest $E$ | $187.5$ | $72.00$ |
| df | $K-1-p = 3$ | $(r-1)(c-1) = 3$ |
| $X^2_\text{obs}$ | $13.104$ | $13.315$ |
| $\chi^2_{3,\,0.95}$ | $7.815$ | $7.815$ |
| p-value | $0.00442$ | $0.00400$ |
| Decision @ $\alpha=0.05$ | reject — non-uniform | reject — associated |

**Master take-aways.**
1. **One template, two tests:** $X^2 = \sum (O-E)^2/E$ approximately $\chi^2_\text{df}$ under $H_0$; only **how you build $E$** and **how you count df** change.
2. **GoF $E$ comes from the null** $(E_k=n\,p_k^0)$; **independence $E$ comes from the data** $(\widehat E_{ij}=n_{i\cdot}n_{\cdot j}/n)$.
3. **Df = (cells) $-1-$ (parameters estimated to build $E$).** Fully-specified GoF: df=$K-1$. Composite GoF with $p$ estimated parameters: df=$K-1-p$. Independence: df=$(r-1)(c-1)$.
4. **Always check $\widehat E_{ij}\ge 5$** (Cochran's rule); otherwise merge categories, switch to Fisher's exact test, or simulate the p-value.
5. **Rejection region** is the **upper tail** of $\chi^2_\text{df}$: $X^2 > \chi^2_{\text{df},\,1-\alpha}$ (equivalently $p<\alpha$). The $\chi^2$ test is **inherently one-sided** because large $X^2$ is the only direction of incompatibility with $H_0$.
6. **Where does the rejection live?** Inspect **standardised Pearson residuals** $(O-\widehat E)/\sqrt{\widehat E}$ — cells with $|z|\gtrsim 2$ drive the statistic and tell the substantive story (here: Low $\uparrow$ Far, None $\uparrow$ Close).

---

**Linked snippets:** Ex 7.4a (GoF uniform on `History`, $n=750$, $\chi^2_\text{obs}=13.104$ — the marginal counts that seed this master); Ex 7.4b (same GoF stratified by `Location`, showing the imbalance is present in *both* sub-populations); Ex 7.7b (chi-squared independence on a $5\times 5$ table `Age_Class`$\times$`LearnTool` with $X^2_\text{obs}=115.69$ on $\chi^2_{16}$, and a worked example of the $\widehat E_{ij}<5$ validity failure in its part (c)); Ex 7.9a (GoF against a **non-uniform** Italian-population $p_0=(0.76,0.13,0.09,0.02)$ on `DS$Children`, $X^2=608.81$ — null *rejected* spectacularly, mostly via the "3+" cell); Ex 7.9b (GoF uniform on 4 supermarket entrances, $n=130$, $X^2=4.523$, $p=0.2104$ — same template, null *retained* — the contrast case to Part (a)).
""",
    "images": ["statistics/images/master/master_g14d_ai.png"],
}


master_exercises["g1a_pie"] = {
    "title": "Master Exam — Pie chart (consolidated)",
    "content": r"""**Setup.** From the customer-survey master dataset ($n=100$), we focus on the nominal binary variable `SmokingArea` (Yes / No) introduced in **Ex 1, Q1.1a**. The observed frequency table is

$$n_\text{Yes}=49,\qquad n_\text{No}=51,\qquad n=100.$$

The companion variables in the linked snippets are `History` with four ordered levels None < Low < Medium < High (**Ex 1, Q1.2d**) and `Sex` with two nominal levels F / M (**Ex 1, Q1.3c**). They serve as contrast cases below — `History` to illustrate when the pie chart is **the wrong tool**, and `Sex` to illustrate the *trivial* binary case.

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (a) When is a pie chart appropriate?</summary>

A pie chart visualises a **share-of-the-whole** decomposition of a single categorical variable. Three conditions must all hold:

1. **Nominal (unordered) scale.** Slices are arranged around a circle; the circle has no canonical "first" or "last" position, so any visual ordering imposed on the categories is arbitrary. Use a pie chart only when category order is genuinely **irrelevant** — i.e. for *nominal* data such as `SmokingArea` or `Sex`.
2. **Few categories** (rule of thumb: $K \le 5$, ideally $K=2$–$4$). Beyond that, slice angles become too similar to compare and the legend takes over the chart.
3. **Exhaustive and mutually exclusive** categories — the proportions must sum to $1$ (i.e. $100\%$). A pie chart of a *subset* of a variable's levels is misleading because the white area in the disc has no probabilistic meaning.

`SmokingArea` satisfies all three: it is nominal, has $K=2$ exhaustive levels, and the two proportions sum to $1$.

</details>

<details class="master-subpart">
<summary>(b) When is the pie chart the wrong tool?</summary>

* **Ordinal variables — order is lost.** For `History` (None < Low < Medium < High) the *order* of the categories is meaningful (it encodes increasing customer-history intensity), but a pie chart destroys it: the slices are placed around a circle, so "next to" no longer means "next on the scale". A reader cannot answer "is the distribution skewed towards High?" from a pie chart, but can read it instantly off a vertical bar plot with categories in their natural order. **Use a bar plot for ordinal data.**
* **Many categories — slices unreadable.** With $K \ge 6$ slices of similar size, the human eye cannot rank angles reliably; sorted bars (Pareto chart) are uniformly better.
* **Comparing two distributions.** Side-by-side pie charts are notoriously hard to compare because the eye compares *angles*, not *lengths*. Use **stacked/grouped bar charts** instead.

A useful contrast: `Sex` (F/M, nominal, $K=2$) — a pie chart is *technically appropriate* but conveys exactly the same information as the single number $\hat p_F$. With only two nominal categories, a one-line text statement ("F: 47%, M: 53%") is often clearer than any chart.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (c) Building the pie chart — proportions and angles</summary>

Step 1 — **relative frequencies.** Divide each absolute frequency by the sample size:
$$\hat p_\text{Yes} \;=\; \frac{n_\text{Yes}}{n} \;=\; \frac{49}{100} \;=\; 0.49,\qquad \hat p_\text{No} \;=\; \frac{n_\text{No}}{n} \;=\; \frac{51}{100} \;=\; 0.51.$$
By construction $\hat p_\text{Yes}+\hat p_\text{No}=1$ (exhaustive and mutually exclusive — condition 3 of part (a)).

Step 2 — **convert proportions to slice angles** by multiplying by the full circle ($360°$):
$$\theta_i \;=\; \hat p_i \times 360°.$$

| Category | $n_i$ | $\hat p_i$ | $\theta_i$ (degrees) |
|---|---|---|---|
| Yes | $49$ | $0.49$ | $0.49\times 360° = 176.4°$ |
| No  | $51$ | $0.51$ | $0.51\times 360° = 183.6°$ |
| **Total** | $\mathbf{100}$ | $\mathbf{1.00}$ | $\mathbf{360.0°}$ |

The two angles sum to $360°$ — a built-in consistency check.

Step 3 — **draw**: each slice is a circular sector with central angle $\theta_i$, all sectors sharing the centre of the disc. Label each slice with its category name and (optionally) its percentage.

```r
# Build the data, compute proportions and slice angles
SmokingArea <- factor(c(rep("Yes", 49), rep("No", 51)),
                      levels = c("Yes", "No"))      # nominal -> level order is cosmetic
tab <- table(SmokingArea);  tab                     # Yes 49   No 51
prop.table(tab)                                     # Yes 0.49 No 0.51
prop.table(tab) * 360                               # 176.4   183.6   (slice angles)

pie(table(SmokingArea))                             # the canonical one-liner
# Polished version with percentages on the slices:
pct <- round(100 * prop.table(tab), 1)
pie(tab, labels = paste0(names(tab), " (", pct, "%)"),
    main = "SmokingArea (n = 100)")
```

![Master illustration](statistics/images/master/master_g1a_pie_ai.png)

</details>

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (d) Reading a pie chart — rank by slice size</summary>

The reader's task is to *rank* and *roughly quantify* the categories by **slice area** (equivalently, central angle). For `SmokingArea`:

* The two slices are *almost equal* ($176.4°$ vs $183.6°$ — a $7.2°$ difference, barely $2\%$ of the circle). Visually, the chart says "the two groups are essentially balanced".
* `No` is the **modal** category (largest slice, $51\%$); `Yes` is a close second ($49\%$).
* Equivalent verbal report: "About half the customers prefer a smoking area".

A pie chart should **not** be used to read off precise numbers — that is the job of the underlying frequency table. It communicates the *qualitative shape* of the distribution: "balanced", "one dominant category", "long tail of small categories", etc.

</details>

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (e) Alternative: vertical bar plot</summary>

For the same data, a bar plot with category names on the $x$-axis and counts (or relative frequencies) on the $y$-axis encodes information in **bar height** — a length, which the human visual system reads more accurately than angles or areas (Cleveland & McGill 1984). Concretely:

| Display | Visual channel | Strength | Weakness |
|---|---|---|---|
| Pie chart | angle / area | intuitive "share of the whole" | hard to compare similar slices, useless for ordinal data |
| Bar plot  | length         | precise, scales to large $K$, preserves order | the "100% whole" is implicit, not visual |

**Default advice.** Use a pie chart **only** for nominal variables with $K\le 4$ when the share-of-the-whole reading is the primary message. Use a bar plot in *every other case* — including ordinal `History` and large-$K$ tables.

```r
# Bar-plot alternative (preferred for ordinal data such as History):
barplot(table(SmokingArea), ylab = "Frequency",
        main = "SmokingArea — bar plot")
# For ordinal History the bar plot KEEPS the natural order:
History <- factor(History, levels = c("None","Low","Medium","High"))
barplot(table(History))                             # do NOT use pie() here
```

</details>

---

**Master take-aways.**

1. A pie chart is appropriate for a **nominal** variable with **few exhaustive** categories — never for ordinal data, never for many categories, never for two-group comparisons.
2. Build it in three steps: **relative frequencies $\hat p_i = n_i/n$ $\to$ slice angles $\theta_i = \hat p_i\times 360°$ $\to$ draw**; check $\sum_i\hat p_i = 1$ and $\sum_i\theta_i = 360°$.
3. Read it by **ranking slice sizes** and identifying the modal category; do not try to extract precise numbers from the angles.
4. The default safe alternative is a **vertical bar plot** — it generalises to ordinal data and to large $K$, and uses length (a more accurate visual channel than angle).
5. For `SmokingArea` here, the two slices are nearly identical ($49\%$ vs $51\%$, $176.4°$ vs $183.6°$): the chart's qualitative message is **"balanced sample"**.

---

**Linked snippets:** Ex 1, Q1.1a (`SmokingArea`, Yes/No — the dataset used here); Ex 1, Q1.2d (`History`, ordinal None/Low/Medium/High — the *counter-example* where a pie chart loses the meaningful order); Ex 1, Q1.3c (`Sex`, nominal F/M — a second valid use case, with the same construction recipe).
""",
    "images": ["statistics/images/master/master_g1a_pie_ai.png"],
}


master_exercises["g1c_hist"] = {
    "title": "Master Exam — Histogram (consolidated)",
    "content": r"""**Setup.** From the pizzerie dataset of $n=100$ shops we examine **Sales** (monthly turnover, in thousands of €). Sales is a **continuous numerical** variable, so a *histogram* is the natural graphical summary. The data have been pre-binned into the **unequal-width** classes below:

| Class $[a_i, b_i)$ | Width $w_i$ | Frequency $f_i$ | Rel. freq. $f_i/n$ | **Density** $h_i=\dfrac{f_i/n}{w_i}$ |
|---|---:|---:|---:|---:|
| $[0,10)$   | 10 | 18 | 0.18 | 0.0180 |
| $[10,20)$  | 10 | 28 | 0.28 | 0.0280 |
| $[20,30)$  | 10 | 30 | 0.30 | 0.0300 |
| $[30,40)$  | 10 | 14 | 0.14 | 0.0140 |
| $[40,80)$  | 40 | 10 | 0.10 | 0.0025 |
| **Total**  | — | **100** | **1.00** | — |

The last class is **four times wider** than the others — this is what makes the unequal-width subtlety bite.

---

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) When is a histogram appropriate?</summary>

A histogram is the right tool when the variable is **continuous numerical** (or at least *quantitative* with many distinct values, like a discretised count with a wide range). The horizontal axis carries a **metric meaning** — distances between values matter — so we draw *bars touching each other* over real intervals.

- **Use a histogram for:** Sales (€), height (cm), waiting time (min), exam score (0–100).
- **Do NOT use a histogram for:** unordered categorical (`Province`, `Brand`) — use a **bar chart** with gaps; ordinal with few levels (`Likert` 1–5) — bar chart is usually clearer; discrete counts with $\le 10$ values — a **stem-and-leaf** or bar chart is more honest.

The contrast with a bar chart is structural, not cosmetic: histogram bars *touch* because the $x$-axis is continuous; bar-chart bars have *gaps* because the $x$-axis is a set of labels with no metric.

```r
sales <- pizzerie$Sales
is.numeric(sales)                              # TRUE -> histogram OK
hist(sales, breaks = c(0,10,20,30,40,80))      # base R, see Parts (c)-(f)
```

</details>

<details class="master-subpart">
<summary><span class="tag tag-4plus">≥4 ex</span> (b) Equal vs unequal classes.</summary>

Two competing goals shape the binning:

1. **Resolution where the data live.** Narrow bins in dense regions reveal mode and shape.
2. **Stability where the data are sparse.** Wide bins in the tails avoid spiky, noise-driven bars.

**Equal-width bins** (the default `hist()` behaviour) are simpler to read and to compare across datasets — every bar covers the same $x$-range, so *heights are directly comparable*. **Unequal-width bins** are used when the variable spans a long tail (income, sales, durations): a single bin like $[40,80)$ keeps the tail visible without 4 nearly-empty narrow bins.

Here, classes $[0,10),\dots,[30,40)$ have width 10, but $[40,80)$ has width 40 — the analyst pooled the sparse tail into one wide class. **This forces us to plot density, not frequency** (Part c).

</details>

<details class="master-subpart">
<summary><span class="tag tag-4plus">≥4 ex</span> (c) Frequency vs density: heights with equal vs unequal widths.</summary>

For a histogram, the *visual quantity* the eye reads is **area**, not height. We want
$$\text{area of bar } i \;=\; \text{proportion of observations in class } i \;=\; \frac{f_i}{n}.$$
Since area $=$ height $\times$ width $= h_i \cdot w_i$, the height must be
$$\boxed{\,h_i \;=\; \dfrac{f_i / n}{w_i}\,} \qquad \text{(``density'' scale).}$$

- **Equal widths.** Every $w_i \equiv w$, so $h_i \propto f_i$: plotting raw counts is equivalent to plotting density up to a global rescaling — *both give the same shape*. This is why intro courses get away with "histogram = bar chart of counts".
- **Unequal widths.** $w_i$ varies across bars, so $h_i \propto f_i$ is **no longer** proportional to $f_i/w_i$ — the *shape changes*. Only the **density scale** preserves the rule "area = proportion".

For our pizzerie data, the densities in the table above are correct: the tall bar is $[20,30)$ with $h = 0.0300$; the **shortest** bar is the wide tail $[40,80)$ with $h = 0.0025$ — even though *its frequency* $f_5 = 10$ is **not** the smallest.

```r
# Correct histogram with UNEQUAL widths: density scale (default since R 4.x for unequal)
br <- c(0,10,20,30,40,80)
hist(sales, breaks = br, freq = FALSE,
     main = "Sales — density (area = proportion)",
     xlab = "Sales (k EUR)", ylab = "Density")
# Heights printed by hist() are exactly f_i/(n*w_i):
h <- hist(sales, breaks = br, plot = FALSE)$density;  round(h, 4)
# -> 0.0180 0.0280 0.0300 0.0140 0.0025
```

The total area of all bars equals 1: $0.18+0.28+0.30+0.14+0.10 = 1$.

![Master illustration](statistics/images/master/master_g1c_hist_ai.png)

---

### Part (d) — How many classes? Sturges and $\sqrt n$.

When *we* design the bins (rather than receiving them pre-binned), two classical rules of thumb give a starting number of classes $K$:

- **Sturges:** $K \approx 1 + \log_2 n$. For $n=100$: $K \approx 1 + \log_2 100 = 1 + 6.64 \approx 7.6 \;\Rightarrow\; K \approx 8$.
- **Square root:** $K \approx \sqrt n$. For $n=100$: $K \approx \sqrt{100} = 10$.

Both rules are *guidelines*: tweak by $\pm 2$ classes based on what the picture reveals. Other named rules (Scott, Freedman–Diaconis) target bin **width** instead and behave better with skewed data — `hist()` in R uses Sturges by default, but accepts `breaks = "FD"` or `breaks = "Scott"`.

The general trade-off:

- **Too few bins ($K$ small)** — over-smoothing: hides modes, gaps, asymmetry.
- **Too many bins ($K$ large)** — under-smoothing: every bar is noise; no visible shape.

```r
length(hist(sales, breaks = "Sturges", plot = FALSE)$breaks) - 1   # ~ 8
length(hist(sales, breaks = "FD",      plot = FALSE)$breaks) - 1   # often more, robust to skew
# Manual: K = ceiling(sqrt(n)) = 10, width = diff(range(sales))/K
```

The pre-given $K=5$ unequal-width design here is *coarser* than Sturges (8) — a deliberate choice because the tail $[40,80)$ is sparse.

---

### Part (e) — Reading the picture: skew, mode, gaps.

Once drawn on the density scale, the histogram tells a story:

- **Modal class.** The tallest bar (in density) is $[20,30)$ with $h=0.030$. The **mode** is reported as the class, not a single value: "modal Sales class is $[20,30)$ k€".
- **Skewness.** Compare the left and right of the mode. Frequencies left of the mode: $18 + 28 = 46$; right of the mode: $14 + 10 = 24$. The right tail is longer and lighter — **right-skewed** (positive skew). This is the canonical pattern for income/sales variables; we expect $\bar x > \text{median} > \text{mode-class midpoint}$.
- **Tail / outliers.** The wide $[40,80)$ class contains $10\%$ of shops spread over a 40-unit range. No empty class, so no obvious *gap*, but the very low density there flags the tail as light.
- **Comparing bars by eye = comparing AREAS**, not heights. The $[40,80)$ bar is *short* but *wide* (area $= 0.0025 \times 40 = 0.10$); the $[20,30)$ bar is *tall* but narrow (area $= 0.030 \times 10 = 0.30$). The eye correctly reads "three times more shops" because area scales with proportion.

```r
# Quantitative shape diagnostics aligned with the picture
mean(sales);  median(sales);  e1071::skewness(sales)        # mean > median, skewness > 0
# Modal class via tabulation
table(cut(sales, breaks = br, right = FALSE))
```

---

### Part (f) — The common error: plotting counts $f_i$ with unequal widths.

If we mistakenly use the **frequency scale** with the *unequal* bins, R will (silently in older versions, with a warning in current versions) draw bars with heights $f_i$ and widths $w_i$ — so the **areas** become $f_i \cdot w_i$, which is **no longer proportional to the share of data**. Concretely:

| Class | $f_i$ | $w_i$ | Area on freq scale = $f_i \cdot w_i$ | True share $f_i/n$ |
|---|---:|---:|---:|---:|
| $[0,10)$  | 18 | 10 | 180 | 0.18 |
| $[10,20)$ | 28 | 10 | 280 | 0.28 |
| $[20,30)$ | 30 | 10 | 300 | 0.30 |
| $[30,40)$ | 14 | 10 | 140 | 0.14 |
| $[40,80)$ | 10 | 40 | **400** | 0.10 |

On the **frequency** scale the tail bar $[40,80)$ has the *largest area* of all five — even though it contains the *fewest* shops! The picture suggests that high-Sales shops dominate the sample, which is the opposite of the truth. This is why R will refuse:

```r
# WRONG — frequency scale with unequal widths (R will issue a warning)
hist(sales, breaks = c(0,10,20,30,40,80), freq = TRUE)
# Warning message:
#  In plot.histogram(...) :
#   the AREAS in the plot are wrong -- rather use 'freq = FALSE'
```

The rule is simple and exam-friendly:

- **Equal widths** $\Rightarrow$ either scale is fine (`freq = TRUE` or `FALSE` give the same shape).
- **Unequal widths** $\Rightarrow$ **density only** (`freq = FALSE`); plotting counts is a textbook misleading-graph error.

---

### Summary table (pizzerie Sales, $n=100$, unequal bins).

| Quantity | Value / Rule |
|---|---|
| Variable type | continuous numerical $\Rightarrow$ histogram (not bar chart) |
| $K$ for $n=100$ (Sturges) | $1+\log_2 100 \approx 8$ |
| $K$ for $n=100$ ($\sqrt n$) | $10$ |
| Equal-width heights | $h_i = f_i$ (or $f_i/n$) — shape invariant |
| Unequal-width heights | $h_i = (f_i/n)/w_i$ — density, **area = proportion** |
| Tallest density bar (here) | $[20,30)$, $h=0.030$ |
| Skew (here) | right (positive): tail toward high Sales |
| Common error | `freq = TRUE` with unequal widths $\Rightarrow$ areas wrong |

**Master take-aways.**
1. **Histogram $=$ density picture for a continuous variable.** Bars touch because the $x$-axis is metric.
2. **Area, not height, encodes proportion.** $\text{area}_i = h_i \, w_i = f_i/n$ by construction.
3. **Equal widths are forgiving:** counts and densities give the same shape. **Unequal widths are not:** counts on the $y$-axis produce *misleading areas* — always use density (`freq = FALSE`).
4. **Pick $K$ with Sturges ($1+\log_2 n$) or $\sqrt n$**; then adjust by eye. Too few bins hide structure; too many turn the picture into noise.
5. **Read shape from the density:** modal class (tallest density), skew (asymmetry around the mode), and gaps/light tails (low or zero density bars).

---

**Bimodality — when the picture shows *two* peaks.** A *unimodal* histogram has one tall central class; a *bimodal* histogram has **two locally-tall classes separated by a valley**. Bimodality is a diagnostic for **two mixed sub-populations** (e.g. customers with short vs long visits, two brands sharing the variable). In **Ex 1.5g** the variable `Time` ($n=1800$) has two local density highs at $[10,20)$ (the modal class) and $[60,90)$, separated by the much lower-density class $[30,60)$ — the visual signature of *two typical customer behaviours* (a quick visit and a long visit). With bimodal data, **a single mean and median both fall in the valley** and mis-represent both sub-populations: report the *bimodality* as the headline finding and consider splitting the sample.

---

**Linked snippets:** Ex 0.1a/c/e/i (histogram of pizzerie `Sales` — design choices, equal vs unequal widths, density scale); Ex 1.1c (variable type → choose histogram); Ex 1.1d (equal vs unequal-width bins — the density-vs-frequency lesson); Ex 1.3a (variable inventory, identifying numerical/continuous); Ex 1.3g (10 equal-width breaks on `Revenue`, right-skew reading); Ex 1.3h (alternative bin widths on `Revenue` — when more bins do *not* add information); Ex 1.5g (densities for `Time` with unequal widths and bimodality); Ex 2.3c, 2.5g, 2.6a3 (histogram + grouped-data shape diagnostics in second-block exams).
""",
    "images": ["statistics/images/master/master_g1c_hist_ai.png"],
}


master_exercises["g13f_estimation"] = {
    "title": "Master Exam — Unbiased estimators and sampling SE (consolidated)",
    "content": r"""**Setup.** A market-research firm collected the monthly turnover `Sales` (in €) for a random sample of $n=100$ pizzerias in Milan. The sample summaries are
$$\bar x \;=\; 24\,000, \qquad s \;=\; 8\,000, \qquad n \;=\; 100.$$
Let $\mu$ and $\sigma^2$ denote the (unknown) population mean and variance of `Sales`. The focus of this master is the *estimator* layer that sits **underneath** confidence intervals and tests: what makes an estimator unbiased, what its standard error measures, and how the SE shrinks with $n$.

---

**(a) Unbiased estimators: definition, and why $\bar X$ and $S^2$ qualify.**

**Definition.** An estimator $T = T(X_1,\dots,X_n)$ of a parameter $\theta$ is **unbiased** if
$$\mathbb E[T] \;=\; \theta \quad \text{for every value of } \theta.$$
Equivalently, its **bias** $\operatorname{bias}(T) = \mathbb E[T] - \theta$ is identically zero. Geometrically: across the (hypothetical) population of all samples of size $n$, the sampling distribution of $T$ is *centred on the truth*. Unbiasedness says nothing about the *spread* of $T$ — that is the job of the SE (part b) — and is a **frequentist** property: it does not require any prior on $\theta$.

**1. Sample mean is unbiased for $\mu$.** For i.i.d. $X_1,\dots,X_n$ with $\mathbb E[X_i]=\mu$, linearity of expectation gives
$$\mathbb E[\bar X] \;=\; \mathbb E\!\Big[\tfrac{1}{n}\sum_{i=1}^n X_i\Big] \;=\; \tfrac{1}{n}\sum_{i=1}^n \mathbb E[X_i] \;=\; \tfrac{1}{n}\cdot n\mu \;=\; \mu.$$
**No distributional assumption** on the $X_i$ is needed — Normality, skewness, heavy tails are all fine. Plugging in the data, the point estimate is $\hat\mu = \bar x = 24\,000$ €.

**2. Why $S^2$ uses $n-1$ (the famous Bessel correction).** Consider the "naive" variance estimator $\tilde S^2 = \tfrac{1}{n}\sum_i (X_i - \bar X)^2$. Using the algebraic identity $\sum_i (X_i-\bar X)^2 = \sum_i (X_i-\mu)^2 - n(\bar X-\mu)^2$ and taking expectations,
$$\mathbb E\!\Big[\sum_i (X_i-\bar X)^2\Big] \;=\; n\sigma^2 \;-\; n\cdot \tfrac{\sigma^2}{n} \;=\; (n-1)\sigma^2,$$
so $\mathbb E[\tilde S^2] = \tfrac{n-1}{n}\sigma^2 < \sigma^2$ — **biased downward**. Dividing instead by $n-1$ exactly cancels the missing factor:
$$S^2 \;=\; \frac{1}{n-1}\sum_{i=1}^n (X_i-\bar X)^2 \quad\Longrightarrow\quad \mathbb E[S^2] \;=\; \sigma^2.$$
Intuition for the "$-1$": one degree of freedom has been spent estimating $\mu$ via $\bar X$, so only $n-1$ truly free squared deviations remain — averaging by $n-1$ restores unbiasedness. Note: $S$ itself is **not** unbiased for $\sigma$ (Jensen's inequality on the concave $\sqrt{\cdot}$), but the bias vanishes rapidly with $n$.

Numerically: $s^2 = 8\,000^2 = 6.4\times 10^7$, $s=8\,000$ €.

```r
n    <- 100
xbar <- 24000
s    <- 8000
s2   <- s^2;  s2                         # 6.4e7  -> point estimate of sigma^2

# Quick simulation: bias of S^2 (n-1) vs naive Stilde^2 (n)  under N(mu, sigma^2)
set.seed(1)
B <- 20000;  mu <- 0;  sig <- 1;  nn <- 5    # small n exaggerates the bias
samp     <- matrix(rnorm(B*nn, mu, sig), nrow = B)
S2_un    <- apply(samp, 1, var)              # divisor n-1   -> unbiased
S2_naive <- rowMeans((samp - rowMeans(samp))^2)  # divisor n -> biased
mean(S2_un);     mean(S2_naive)              # ~1.000   ~0.800  ((n-1)/n=0.8)
```

---

**(b) What does the SE measure? — $P(|\bar X-\mu|>SE)$ under Normality.**

The **standard error** of $\bar X$ is the SD of its sampling distribution:
$$SE(\bar X) \;=\; \sqrt{\operatorname{Var}(\bar X)} \;=\; \frac{\sigma}{\sqrt n}.$$
For our pizzerias, **plugging in** $s$ for the unknown $\sigma$ gives the *estimated* SE
$$\widehat{SE}(\bar X) \;=\; \frac{s}{\sqrt n} \;=\; \frac{8\,000}{\sqrt{100}} \;=\; 800 \text{ €}.$$
The SE is **not** the distance between $\bar x$ and $\mu$ for *this* sample (we cannot know that — $\mu$ is unknown); it is the **typical** distance across hypothetical resamples of size $n=100$.

**Probability that the realised $\bar X$ falls farther than one SE from $\mu$ (Normal case).** If $X_i \sim \mathcal N(\mu,\sigma^2)$ then $\bar X \sim \mathcal N(\mu,\sigma^2/n)$ **exactly** (no CLT needed), so $Z = (\bar X - \mu)/(\sigma/\sqrt n) \sim \mathcal N(0,1)$. Therefore
$$P\!\big(|\bar X - \mu| > SE\big) \;=\; P\!\left(\frac{|\bar X - \mu|}{\sigma/\sqrt n} > 1\right) \;=\; P(|Z|>1) \;=\; 2\,[1-\Phi(1)] \;\approx\; 2(1-0.8413) \;=\; 0.3173.$$
**Roughly one sample in three** lands more than one SE from $\mu$ — the "1-SE band" is *not* a high-confidence band. Pushing to $\pm 2\,SE$ drops the miss-rate to $P(|Z|>2)\approx 4.6\%$, and $\pm 1.96\,SE$ hits the canonical 5%. This is exactly the calibration that fuels the 95% CI $\bar X \pm 1.96\,SE$ when $\sigma$ is known.

```r
SE_hat <- s / sqrt(n);  SE_hat            # 800   estimated SE
2 * (1 - pnorm(1))                        # 0.3173    P(|Z|>1)
2 * (1 - pnorm(2))                        # 0.04550   P(|Z|>2)
2 * (1 - pnorm(1.96))                     # 0.05000   the 95% calibration

# Visual check by simulation (Normal sampling distribution of Xbar)
set.seed(2);  B <- 1e5;  mu <- 24000;  sig <- 8000
xb <- rowMeans(matrix(rnorm(B*n, mu, sig), nrow = B))
mean(abs(xb - mu) > sig/sqrt(n))          # ~0.317
```

*Caveat.* The 0.317 figure assumed (i) Normal data and (ii) **known** $\sigma$. Under unknown $\sigma$ the exact statement uses $T=(\bar X-\mu)/(S/\sqrt n)\sim t_{n-1}$ and gives $P(|T|>1)=2[1-F_{t_{n-1}}(1)]\approx 0.3197$ at $n=100$ — practically the same; the gap matters only for very small $n$.

---

**(c) Effect of sample size on the SE — the $1/\sqrt n$ law.**

Because $SE(\bar X) = \sigma/\sqrt n$, the SE **shrinks as the square root** of $n$, not linearly. Concretely, multiplying $n$ by $k$ divides the SE by $\sqrt k$:

| $n$ | $\widehat{SE} = s/\sqrt n$ (€) | factor vs $n=100$ |
|---|---|---|
| 25  | $1\,600$ | $2.00\times$ |
| 100 | $800$    | $1.00\times$ (baseline) |
| 400 | $400$    | $0.50\times$ |
| 900 | $267$    | $0.33\times$ |
| 10\,000 | $80$ | $0.10\times$ |

**Three consequences.**
1. **Diminishing returns.** Halving the SE requires **quadrupling** $n$ (and the data-collection cost). Going from $n=100$ to $n=200$ shrinks the SE by only $\sqrt 2\approx 1.41\times$, i.e. about 29%.
2. **CI half-width follows the same law.** With $\sigma$ known, $ME_{95}=1.96\cdot\sigma/\sqrt n$ inherits the $1/\sqrt n$ rate — so to halve a CI we again need $n\to 4n$. Confidence level $1-\alpha$ does **not** interact with $n$ in this rate; it only scales the leading constant ($z_{1-\alpha/2}$).
3. **The "1-SE miss-rate" 0.317 does not depend on $n$.** The probability $P(|\bar X-\mu|>SE)$ is invariant under the standardisation $Z=(\bar X-\mu)/(\sigma/\sqrt n)$ — for every $n$, "more than one SE off" happens about a third of the time under Normality. What changes with $n$ is the *physical scale* of one SE (€800 at $n=100$, €80 at $n=10\,000$), not the probability mass beyond it.

```r
# 1/sqrt(n) law: tabulate SE across sample sizes
ns <- c(25, 100, 400, 900, 10000)
data.frame(n = ns, SE = round(s / sqrt(ns), 1),
           factor_vs_100 = round(sqrt(100/ns), 3))
#       n     SE factor_vs_100
# 1    25 1600.0         2.000
# 2   100  800.0         1.000
# 3   400  400.0         0.500
# 4   900  266.7         0.333
# 5 10000   80.0         0.100

# Sample size to achieve a target ME at 95% (sigma known)
target_ME <- 200                            # want +/- 200 EUR
ceiling( (qnorm(0.975) * s / target_ME)^2 ) # 6147 pizzerias needed
```

![Master illustration](statistics/images/master/master_g13f_ai.png)

---

### Summary table (master dataset, $n=100$, $\bar x=24\,000$, $s=8\,000$).

| Quantity | Formula | Value | Why it matters |
|---|---|---|---|
| Point estimate of $\mu$ | $\bar x$ | $24\,000$ € | Unbiased ($\mathbb E[\bar X]=\mu$), no distributional assumption |
| Point estimate of $\sigma^2$ | $s^2 = \tfrac{1}{n-1}\sum(x_i-\bar x)^2$ | $6.4\times 10^7$ €² | Bessel correction $\Rightarrow \mathbb E[S^2]=\sigma^2$ |
| Estimated SE of $\bar X$ | $s/\sqrt n$ | $800$ € | Typical sampling spread of $\bar X$ |
| $P(|\bar X-\mu|>SE)$ under Normality | $2[1-\Phi(1)]$ | $0.3173$ | A 1-SE band is **not** high-confidence |
| $P(|\bar X-\mu|>2\,SE)$ | $2[1-\Phi(2)]$ | $0.0455$ | The "2-SE rule of thumb" $\approx 95\%$ |
| $P(|\bar X-\mu|>1.96\,SE)$ | $2[1-\Phi(1.96)]$ | $0.0500$ | Exact 95% calibration |
| SE at $n=400$ | $s/\sqrt{400}$ | $400$ € | Quadruple $n$ to halve the SE |

**Master take-aways.**
1. **Unbiased $\ne$ accurate for this sample.** $\mathbb E[T]=\theta$ is a property of the *sampling distribution*, not of the realised $\hat\theta$. Two unbiased estimators can still differ by their variance — the one with **smaller variance** (and hence smaller SE) is preferred.
2. **The $n-1$ in $S^2$ is the price for using $\bar X$ in place of $\mu$.** One degree of freedom is consumed; dividing by $n-1$ restores $\mathbb E[S^2]=\sigma^2$. **Beware:** $S$ is still slightly biased for $\sigma$, but the bias is $O(1/n)$ and irrelevant for $n=100$.
3. **SE measures variability of the *estimator*, not of the data.** $SE(\bar X) = \sigma/\sqrt n$ is far smaller than $\sigma$ (the data's spread); confusing the two is one of the most common inference mistakes.
4. **The "1-SE band" misses about a third of the time** under Normality ($P(|Z|>1)\approx 0.317$). Use $\pm 1.96\,SE$ for 95% (with known $\sigma$, large $n$) or $\pm t_{0.975,n-1}\,\widehat{SE}$ otherwise.
5. **$1/\sqrt n$ scaling is brutal.** Halving the SE costs $4\times$ the sample; cutting it to a tenth costs $100\times$. Plan the sample size from the *target ME*, not from intuition.
6. **Why this matters downstream.** The CI of master g13a, the test statistics of g14a–g14c, and the regression SEs of g15a all start from the SE computed here. Get the SE wrong (e.g. forget Bessel, use $\sigma$ instead of $\sigma/\sqrt n$, mis-scale with $n$) and every interval and p-value built on top of it is wrong by the same factor.

---

**Linked snippets:** Ex 5.13a1 (definition $\mathbb E[T]=\theta$ and unbiasedness of $\bar X$ for $\mu$ via linearity of expectation, with $\sigma$ **known** so $SE=\sigma/\sqrt n$ is exact); Ex 5.13a2 (standardising the event $|\bar X-\mu|>SE$ to $|Z|>1$, the resulting $P\approx 0.3173$ under Normality or CLT, and the Chebyshev fallback when neither assumption holds). The Bessel correction ($S^2$ unbiased for $\sigma^2$, $n-1$ vs $n$) and the $1/\sqrt n$ sample-size law in part (c) live **only** in this master — they consolidate beyond the two snippets.
""",
    "images": ["statistics/images/master/master_g13f_ai.png"],
}


master_exercises["g15d_categorical"] = {
    "title": "Master Exam — Categorical predictors, dummies & interactions (consolidated)",
    "content": r"""**Setup.** A consultancy collected `Salary` (€/month, net), `grade` (annual performance score, $0$–$100$), `sex` ($F$/$M$) and `course` (training track attended, three levels $a$, $b$, $c$) for a random sample of $n=100$ junior employees (the **GS** dataset). The OLS fit of

$$\text{Salary}_i \;=\; \beta_0 \;+\; \beta_g\,\text{grade}_i \;+\; \beta_M\,D^{sex}_{M,i} \;+\; \beta_b\,D^{course}_{b,i} \;+\; \beta_c\,D^{course}_{c,i} \;+\; \varepsilon_i$$

returns the point estimates

$$\hat\beta_0 = 1\,400,\quad \hat\beta_g = 35,\quad \hat\beta_M = 2\,000,\quad \hat\beta_b = 450,\quad \hat\beta_c = -150,$$

with $R^2 = 0.612$, residual SD $\hat\sigma = 620$, and residual df $n-p-1 = 100-4-1 = 95$. Reference categories are $sex=F$ and $course=a$. The "full" model above (call it $\mathcal M_1$) is compared in part **(d)** with the reduced model $\mathcal M_0$ that drops both course dummies, and in parts **(e)**–**(f)** with an extended model $\mathcal M_2$ that adds a $sex\times grade$ interaction.

---

**(a) Why $k$ categories become $k-1$ dummies — the dummy-variable trap.** For a nominal variable with $k$ levels $\{L_1,\dots,L_k\}$ we **cannot** put $k$ indicator columns into a regression that also has an intercept: the $k$ dummies sum to the all-ones vector, which is the intercept column, so the design matrix $\mathbf X$ has rank $\le p$ and $(\mathbf X^\top\mathbf X)^{-1}$ does not exist. The standard fix — **treatment / reference coding** — drops one level (the **baseline**) and keeps $k-1$ dummies:

$$D^{course}_{b,i} \;=\; \begin{cases}1 & course_i = b\\ 0 & \text{otherwise}\end{cases},\qquad D^{course}_{c,i} \;=\; \begin{cases}1 & course_i = c\\ 0 & \text{otherwise}\end{cases}.$$

Course $a$ is identified by $D^{course}_b = D^{course}_c = 0$. Same logic for `sex`: with $k=2$ levels we add **one** dummy $D^{sex}_M = \mathbb 1\{sex=M\}$ and absorb $sex=F$ into the intercept. With `grade` (continuous) no dummy is needed.

```r
# Treatment coding is R's default for factors
GS$sex    <- factor(GS$sex,    levels = c("F","M"))     # F = baseline
GS$course <- factor(GS$course, levels = c("a","b","c")) # a = baseline
contrasts(GS$course)                                     # 2 columns: courseb, coursec
fit1 <- lm(Salary ~ grade + sex + course, data = GS)
summary(fit1)                                            # -> beta0=1400, beta_g=35, beta_M=2000, ...
```

*Three coding alternatives* (same fit, different parametrisation): **treatment** (baseline absorbed; coefficients = level vs baseline — the default and what we use), **sum-to-zero** (deviations from the grand mean, `contr.sum`), **no-intercept + all $k$ dummies** (cell means, `lm(y ~ 0 + course)`). All produce identical fitted values; they differ only in *what each coefficient means*.

---

**(b) Reference-category interpretation.** With $sex=F$ and $course=a$ as the reference levels, the intercept gives the expected `Salary` for a *reference* employee with all dummies zero **and** all continuous predictors zero:

$$\mathbb E[\text{Salary}\mid grade=0,\; sex=F,\; course=a] \;=\; \hat\beta_0 \;=\; 1\,400 \text{ €}.$$

The reference is **arbitrary** — any level can play that role and the choice does not change the *fit* (predictions, residuals, $R^2$, $F$-statistics), only the *coefficient labels*. Re-levelling to $course=c$ baseline simply relabels: the new intercept becomes $\hat\beta_0 + \hat\beta_c = 1\,400 + (-150) = 1\,250$, the new `coursea` coefficient becomes $-\hat\beta_c = +150$, the new `courseb` coefficient becomes $\hat\beta_b - \hat\beta_c = 450-(-150) = 600$.

```r
GS$course <- relevel(GS$course, ref = "c")               # change baseline
fit1c <- lm(Salary ~ grade + sex + course, data = GS)
# Same R^2, same fitted values; only the dummies' interpretation changes:
all.equal(fitted(fit1), fitted(fit1c))                   # TRUE
```

**Practical rule.** Choose the baseline that makes the *comparisons of interest* read off most naturally — e.g. control vs treatment, or the largest/most-stable cell.

---

**(c) Reading a dummy coefficient (level vs baseline, ceteris paribus).** Each dummy coefficient is the **mean shift** in $y$ between that level and the baseline, **holding all other regressors fixed**:

- $\hat\beta_M = +2\,000$: at the same `grade` and the same `course`, **men** earn on average **2 000 € more per month** than **women**. *That is the conditional gender gap implied by this model.* It is **not** a causal effect — it could reflect any omitted variable correlated with `sex` and `Salary` (seniority, hours, role). Significance: $t = \hat\beta_M / \widehat{SE}(\hat\beta_M)$; with $\widehat{SE}\approx 130$ this is $t\approx 15.4$ on $95$ df, $p<10^{-26}$ — clearly significant.
- $\hat\beta_b = +450$: at the same `grade` and same `sex`, attending **course $b$** is associated with **+450 €/month** vs the baseline **course $a$**.
- $\hat\beta_c = -150$: at the same `grade` and same `sex`, **course $c$** is associated with **−150 €/month** vs **course $a$**.
- The **course $b$ vs course $c$ difference** is *not* a coefficient — it is the **linear combination** $\hat\beta_b - \hat\beta_c = 450-(-150) = 600$ €/month, with SE obtained from the covariance matrix or by re-levelling and re-fitting.

```r
# Three concrete predictions (ceteris paribus comparisons)
beta0  <- 1400; bg <- 35; bM <- 2000; bb <- 450; bc <- -150
# Woman, grade 70, course a:
beta0 + bg*70                                            # 3850
# Man,   grade 70, course a:
beta0 + bg*70 + bM                                       # 5850   (gap = 2000)
# Man,   grade 70, course b:
beta0 + bg*70 + bM + bb                                  # 6300
# Course b vs c, holding grade & sex fixed:
bb - bc                                                  # 600
# SE of (bb - bc) from the covariance matrix:
V <- vcov(fit1);  L <- c(0,0,0,1,-1)
sqrt(t(L) %*% V %*% L)                                   # e.g. ~ 165 -> t ~ 3.6
```

---

**(d) Joint significance of `course` — partial $F$ test.** Whether *the categorical variable as a whole* matters is **not** answered by looking at the two individual dummies separately (one can be non-significant while the variable jointly is) — we need a joint test of

$$H_0:\; \beta_b = \beta_c = 0 \qquad\text{vs.}\qquad H_1:\; \beta_b \neq 0 \text{ or } \beta_c \neq 0.$$

The **partial (incremental) $F$** compares the **full** model $\mathcal M_1$ (with the two course dummies, $p_1=4$ regressors) to the **reduced** model $\mathcal M_0$ (without them, $p_0=2$):

$$F \;=\; \frac{(RSS_0 - RSS_1)/(p_1-p_0)}{RSS_1/(n - p_1 - 1)} \;\sim\; F_{p_1-p_0,\; n-p_1-1}\quad\text{under }H_0.$$

Plugging in $RSS_1 = 36.5\times 10^6$, $RSS_0 = 41.8\times 10^6$, $p_1-p_0 = 2$, $n-p_1-1=95$:

$$F \;=\; \frac{(41.8-36.5)\times 10^6/2}{36.5\times 10^6/95} \;=\; \frac{2.65\times 10^6}{384\,210} \;\approx\; 6.90.$$

Critical value $F_{2,95;\,0.95} \approx 3.09$ ($p\approx 0.0016$) — **reject** $H_0$: `course` is jointly significant at $\alpha=0.05$, even though one of its two dummies ($\hat\beta_c=-150$) would, in isolation, look insignificant.

```r
fit1 <- lm(Salary ~ grade + sex + course, data = GS)     # full   M1
fit0 <- lm(Salary ~ grade + sex,          data = GS)     # reduced M0
anova(fit0, fit1)                                        # partial F, df1=2, df2=95
# Manually:
n  <- 100;  p1 <- 4;  p0 <- 2
RSS1 <- 36.5e6;  RSS0 <- 41.8e6
F_stat <- ((RSS0 - RSS1)/(p1 - p0)) / (RSS1/(n - p1 - 1));  F_stat   # ~ 6.90
qf(0.95, df1 = p1 - p0, df2 = n - p1 - 1)                            # 3.094
1 - pf(F_stat, df1 = p1 - p0, df2 = n - p1 - 1)                      # ~ 0.0016
```

**Why not just look at the two $t$-stats?** Because the $t$ tests are **marginal** (each given the *other* predictors including the other course dummy). The joint $F$ is the right global test — it is what a "drop the variable?" decision should rest on, and it generalises immediately to any group of regressors (interactions, polynomials, splines).

---

**(e) Interaction $sex\times grade$ — letting the slope differ by group.** The model so far assumes the marginal effect of `grade` on `Salary` is the **same** for women and men: $\partial \mathbb E[Salary]/\partial grade = \beta_g$ regardless of $sex$. To let the slope **differ by sex**, add the interaction:

$$\mathcal M_2:\; \text{Salary} \;=\; \beta_0 + \beta_g\,grade + \beta_M\,D^{sex}_M + \beta_b\,D^{course}_b + \beta_c\,D^{course}_c + \gamma\,(D^{sex}_M\cdot grade) + \varepsilon.$$

Suppose the fit gives $\hat\beta_g = 30$ and $\hat\gamma = 12$ (so $\hat\beta_M$ also shifts to e.g. $1\,150$). Now the per-point return to `grade`, holding `course` fixed, is

$$\text{women:}\quad \frac{\partial\,\widehat{Salary}}{\partial grade} = \hat\beta_g = 30,\qquad \text{men:}\quad \frac{\partial\,\widehat{Salary}}{\partial grade} = \hat\beta_g + \hat\gamma = 42.$$

So $\hat\gamma=12$ means **each extra grade point is worth 12 € more for men than for women** — the gender gap is no longer a constant 2 000 €, it **widens with grade**. The interaction coefficient is tested by its own $t$ (or, equivalently for one regressor, by `anova(fit1, fit2)`). If $\hat\gamma$ is not significant we go back to $\mathcal M_1$.

```r
fit2 <- lm(Salary ~ grade + sex + course + sex:grade, data = GS)  # M2
# (Shortcut: Salary ~ (grade + sex)^2 + course expands to grade + sex + grade:sex + course)
summary(fit2)                                                      # t-test on gamma
anova(fit1, fit2)                                                  # equivalent partial F (df1 = 1)

# Per-sex slopes, holding course fixed:
b_g  <- coef(fit2)["grade"];          b_g                          # 30  -> women
gam  <- coef(fit2)["grade:sexM"];     b_g + gam                    # 42  -> men
```

*Centering tip.* If $\hat\beta_M$ is hard to interpret because $grade=0$ is far outside the data, **centre** `grade` (subtract its mean): the main effect of `sex` then reads off as the gender gap at the **average grade**, not at $grade=0$.

---

**(f) Parallel-lines vs separate-lines models — geometry & test.** Holding `course` at the baseline $a$ for clarity, $\mathcal M_1$ and $\mathcal M_2$ trace, in the $(grade, Salary)$ plane, two **lines** — one for women, one for men:

| Model | Women: intercept | Women: slope | Men: intercept | Men: slope | Geometry |
|---|---|---|---|---|---|
| **$\mathcal M_1$** (no interaction) | $\beta_0 = 1\,400$ | $\beta_g = 35$ | $\beta_0+\beta_M = 3\,400$ | $\beta_g = 35$ | **parallel lines** — same slope, vertical shift $\beta_M$ |
| **$\mathcal M_2$** (with interaction) | $\beta_0 = 1\,400$ | $\beta_g = 30$ | $\beta_0+\beta_M = 2\,550$ | $\beta_g+\gamma = 42$ | **non-parallel lines** — both intercept and slope shift; lines may cross |

So **the interaction is exactly the *lack of parallelism*** between the two lines (or, in general, between the $k$ lines indexed by a categorical variable). Testing $H_0:\gamma = 0$ is testing *"are the two lines parallel?"*. Geometrically:

- $\beta_M$ alone $=$ **shift** of one line above the other (constant gap at every `grade`).
- $\gamma$ alone (with $\beta_M = 0$) $=$ **rotation** of one line relative to the other through the origin.
- $\beta_M$ **and** $\gamma$ together $=$ a fully separate line per group — equivalent to fitting `lm(Salary ~ grade, data = subset(sex=='F'))` and `lm(Salary ~ grade, data = subset(sex=='M'))` separately (when *no other regressors* are shared); with shared regressors (here, `course`) the joint fit is **more efficient** because it pools the residual variance.

```r
# Visualise parallel vs non-parallel
g_grid <- 0:100
# M1 -- parallel lines (course = a)
yF1 <- 1400 + 35*g_grid;          yM1 <- (1400 + 2000) + 35*g_grid
# M2 -- non-parallel (course = a, beta_M shifted to 1150, gamma = 12)
yF2 <- 1400 + 30*g_grid;          yM2 <- (1400 + 1150) + (30 + 12)*g_grid

plot (g_grid, yM1, type="l", lwd=2, col="navy", ylim=c(0, 8000),
      xlab="grade", ylab="Salary", main="Parallel (M1) vs separate (M2) lines")
lines(g_grid, yF1, lwd=2, lty=2, col="navy")
lines(g_grid, yM2, lwd=2, col="darkorange")
lines(g_grid, yF2, lwd=2, lty=2, col="darkorange")
legend("topleft", lty=c(1,2,1,2), col=c("navy","navy","darkorange","darkorange"),
       legend=c("M1 men","M1 women","M2 men","M2 women"), bty="n")
```

**Decision rule.** Test $H_0:\gamma=0$ (interaction zero) via the $t$ on $\hat\gamma$ or `anova(fit1, fit2)`. If **not** rejected, prefer $\mathcal M_1$ (parallel lines — simpler, more df for the residual, identical slope for all groups). If **rejected**, keep $\mathcal M_2$ (separate slopes — the effect of the continuous predictor genuinely differs across groups).

---

**Summary table — the GS model family.**

| Quantity | $\mathcal M_0$ (no course) | $\mathcal M_1$ (full, no interaction) | $\mathcal M_2$ (with $sex\times grade$) |
|---|---|---|---|
| Regressors (excl. intercept) | 2 | 4 | 5 |
| Residual df | $97$ | $95$ | $94$ |
| Course dummies | — | 2 ($b$, $c$) — joint $F_{2,95}=6.90$, $p\approx 0.0016$ | 2 ($b$, $c$) |
| Sex effect | constant shift $\beta_M$ | constant shift $\beta_M=2\,000$ | shift $\beta_M$ **+** slope shift $\gamma$ |
| Lines in $(grade,Salary)$ by sex (at $course=a$) | one line per sex, **parallel** | one line per sex, **parallel** | one line per sex, **non-parallel** |
| Test for "course matters" | n/a | partial $F_{2,95}$ on dropping $\{b,c\}$ | partial $F_{2,94}$ on dropping $\{b,c\}$ |
| Test for "slope differs by sex" | n/a | n/a | $t$ on $\hat\gamma$ $\equiv$ partial $F_{1,94}$ |

**Master take-aways.**
1. **A categorical variable with $k$ levels enters as $k-1$ dummies** (when the model has an intercept). The omitted level is the **reference / baseline** and lives inside $\hat\beta_0$.
2. **The choice of baseline is arbitrary for the fit but matters for interpretation** — coefficients are always *level vs baseline*. Re-levelling relabels coefficients but leaves predictions, $R^2$, and $F$-tests unchanged.
3. **Each dummy coefficient = mean shift in $y$ vs baseline, ceteris paribus.** Differences between *two non-baseline* levels are linear combinations — get their SE from $\mathbf{L}^\top \widehat{\text{Var}}(\hat\beta) \mathbf{L}$ or by re-levelling.
4. **Joint significance of a $k$-level factor uses the partial $F$** on all $k-1$ dummies simultaneously (`anova(fit0, fit1)`) — *never* read it off a single $t$, which is marginal.
5. **An interaction $D\cdot x$ lets the slope on a continuous $x$ differ across the levels of a categorical $D$.** $\gamma=0$ $\Leftrightarrow$ **parallel-lines** model; $\gamma\neq 0$ $\Leftrightarrow$ **separate-slope** model.
6. **Always interpret main effects in the presence of interactions carefully**: with $sex\times grade$ in the model, $\hat\beta_M$ is the gender gap at $grade=0$ — centre `grade` (subtract $\bar{grade}$) to read it as the gap at the *average* grade instead.

---

**Linked snippets:** Ex 9.5 (Restaurants: `evening_only` as a single dummy — special case $k=2$ of part (a)–(c)); Ex 9.6 (MBA `TypeDegree` with multiple levels — the $k-1$ coding rule and joint test of part (a),(d)); Ex 9.7 (Performance vs `Sector` — reference-category interpretation and partial $F$, parts (b)–(d)); Ex 9.9 (GS `sex + course` on Salary — the dataset that seeds this master, including the interaction discussion of parts (e)–(f)).

![Master G15d — boxplots by group, parallel vs separate lines, coefficient heatmap](statistics/images/master/master_g15d_ai.png)
""",
    "images": ["statistics/images/master/master_g15d_ai.png"],
}



# =====================================================================
# g15e_diagnostics --- Residual diagnostics & multicollinearity
# Consolidates: ex8.4a (Restaurants surface + diagnostics)
# Dataset: Restaurants (revenues ~ surface), n = 50
# =====================================================================
master_exercises["g15e_diagnostics"] = {
    "title": "Master Exam --- Residual diagnostics & multicollinearity on Restaurants ($n=50$)",
    "content": r"""**Master exercise --- Residual diagnostics & multicollinearity (consolidated).**

A single dataset, seven sub-points covering every unique diagnostic concept asked across **Ex 8.4a** (Restaurants: revenues ~ surface): raw and standardised residuals, residuals-vs-fitted heteroscedasticity check, residuals-vs-predictor structure check, Normality of residuals (histogram + Q-Q), Cook's distance / leverage / influence, VIF for multicollinearity, and remedies (log / Box-Cox / drop predictor / ridge).

---

### Dataset (single, shared by all parts)

The `restaurants` dataset records $n=50$ restaurants with

- $X = \text{surface}$ (square metres of dining area)
- $Y = \text{revenues}$ (weekly revenues, thousands of EUR)

We fit the simple OLS model
$$Y_i \;=\; \beta_0 \;+\; \beta_1 X_i \;+\; \epsilon_i,\qquad \epsilon_i \stackrel{iid}{\sim}\mathcal N(0,\sigma^2_\epsilon),\qquad i=1,\dots,50.$$

OLS gives $\hat\beta_0 = 246.812$, $\hat\beta_1 = 0.4049$ (kEUR per $m^2$), $\widehat\sigma_\epsilon \approx 41.7$, $R^2 \approx 0.61$. For parts (f)--(g) we also entertain the *multiple* regression $Y \sim \text{surface} + \text{seats} + \text{evening\_only}$, with `seats` strongly correlated with `surface` ($r \approx 0.93$).

```r
mod  <- lm(revenues ~ surface, data = restaurants)
summary(mod)                                     # coefs, R^2, sigma_hat
e    <- residuals(mod)                           # raw residuals e_i
yhat <- fitted(mod)                              # fitted values
n    <- nobs(mod);  p <- length(coef(mod))       # 50 obs, 2 parameters
```

---

### (a) Raw residuals $e_i$ and standardised residuals $r_i^{\text{std}}$

The **raw residual** is the empirical counterpart of the unobservable error $\epsilon_i$:
$$e_i \;=\; y_i \;-\; \hat y_i \;=\; y_i \;-\; (\hat\beta_0 + \hat\beta_1 x_i).$$

By construction OLS forces $\sum_i e_i = 0$ and $\sum_i x_i e_i = 0$, so the residual *vector* lives in an $(n-p)$-dimensional subspace --- this is why we divide SSE by $n-p$ when estimating $\sigma_\epsilon^2$:
$$\widehat\sigma_\epsilon^2 \;=\; \frac{1}{n-p}\sum_{i=1}^{n}e_i^2.$$

Raw residuals are **not** identically distributed: $\mathrm{Var}(e_i) = \sigma_\epsilon^2(1-h_{ii})$ where $h_{ii}$ is the *leverage* (the $i$-th diagonal of the hat matrix $H=X(X^\top X)^{-1}X^\top$). High-leverage points have *smaller* raw-residual variance --- they pull the line toward themselves and so produce mechanically tiny $e_i$. To make residuals comparable across observations we rescale:

$$\boxed{\;r_i^{\text{std}} \;=\; \frac{e_i}{\widehat\sigma_\epsilon\sqrt{1-h_{ii}}}\;\approx\;\mathcal N(0,1)\text{ under model assumptions.}\;}$$

A *Studentised* (a.k.a. externally Studentised) residual replaces $\widehat\sigma_\epsilon$ by $\widehat\sigma_{\epsilon,(i)}$ --- the SD estimated *without* observation $i$ --- and is exactly $t_{n-p-1}$ under Normality.

Rule of thumb: $|r_i^{\text{std}}|>2$ is *worth inspecting*; $|r_i^{\text{std}}|>3$ is a *likely outlier*.

```r
e        <- residuals(mod)
r_std    <- rstandard(mod)                       # internally Studentised
r_stud   <- rstudent(mod)                        # externally Studentised (t_{n-p-1})
sigma_hat <- summary(mod)$sigma                  # sqrt( SSE / (n-p) )
sum(e)                                           # ~ 0  (OLS normal eq.)
sum(restaurants$surface * e)                     # ~ 0
mean(abs(r_std) > 2)                             # share of "worth-inspecting" points
```

---

### (b) Plot $e$ vs $\hat y$ --- funnel $\Rightarrow$ heteroscedasticity

The **residuals-vs-fitted** plot is the single most informative diagnostic. Under the Gauss--Markov assumptions, $e_i$ should look like noise around zero with **constant vertical spread** for every value of $\hat y$. Failure patterns:

| Pattern in $e$ vs $\hat y$ | Diagnosis | Remedy (sketch) |
|---|---|---|
| Random cloud, constant spread | OK --- homoscedastic, well-specified | none |
| **Funnel** (spread $\uparrow$ as $\hat y\uparrow$) | **Heteroscedasticity** --- $\mathrm{Var}(\epsilon_i)\propto \mu_i^\alpha$ | log/Box--Cox $Y$; WLS; HC robust SE |
| Curvature (U or inverted-U) | Wrong functional form --- missing $X^2$ or interaction | add polynomial term; transform $X$ |
| Trend in mean (line not at 0) | Should not happen for OLS with intercept | check code; refit |

For Restaurants, plotting `e` vs `yhat` shows a clear **funnel opening to the right**: small restaurants ($\hat y \approx 280$) have residuals tightly clustered within $\pm 30$, while large restaurants ($\hat y \approx 540$) have residuals scattered across $\pm 100$. This is **textbook multiplicative heteroscedasticity**: bigger restaurants have proportionally bigger *absolute* shocks, consistent with revenues being a *count-like* quantity whose variance grows with its mean.

Formal tests confirm visual reading: Breusch--Pagan and White:
$$H_0:\;\mathrm{Var}(\epsilon_i)=\sigma^2_\epsilon \quad\text{vs}\quad H_1:\;\mathrm{Var}(\epsilon_i)=h(\mathbf z_i^\top \boldsymbol\gamma).$$

```r
plot(yhat, e, pch = 19, col = "steelblue",
     xlab = "Fitted values", ylab = "Residuals e_i",
     main = "Residuals vs Fitted --- funnel = heteroscedasticity")
abline(h = 0, lty = 2)
lines(lowess(yhat, e), col = "firebrick", lwd = 2)

library(lmtest)
bptest(mod)                                     # Breusch-Pagan; p < 0.05 -> reject homoscedasticity
bptest(mod, ~ surface + I(surface^2))           # White-style (squares included)
```

---

### (c) Plot $e$ vs each $x$ --- functional-form check

Whereas $e$ vs $\hat y$ pools information across *all* predictors, plotting **residuals against each predictor separately** isolates which variable is misspecified. Expected pattern under correct specification: a **flat horizontal band**, with the LOWESS smoother hugging zero. Telltale failures:

- **Quadratic shape** in $e$ vs $x_j$: model is missing $x_j^2$ --- add it.
- **Monotone trend**: model is missing $x_j$ entirely (only an issue if $x_j$ was omitted).
- **Funnel widening with $x_j$**: heteroscedasticity is driven by $x_j$ --- use $\log Y$ or WLS with weights $1/x_j^2$.

For Restaurants the $e$-vs-`surface` plot also shows a funnel (because `surface` is monotone in $\hat y$), confirming that `surface` is the *driver* of the heteroscedasticity, not a hidden third variable.

```r
plot(restaurants$surface, e, pch = 19, col = "steelblue",
     xlab = "surface (m^2)", ylab = "Residuals e_i",
     main = "Residuals vs surface")
abline(h = 0, lty = 2)
lines(lowess(restaurants$surface, e), col = "firebrick", lwd = 2)
```

---

### (d) Histogram + Q-Q of residuals --- Normality

Normality of $\epsilon$ is **not** needed for OLS unbiasedness or consistency --- it is needed for **exact** small-sample $t$- and $F$-inference and for prediction *intervals*. Two visual tools:

1. **Histogram** of $r_i^{\text{std}}$ --- should be roughly bell-shaped, centred at 0, with $\sim 95\%$ within $[-2,2]$.
2. **Normal Q-Q plot** --- sorted $r_i^{\text{std}}$ against $\Phi^{-1}((i-0.5)/n)$. Points should lie on the 45-degree line. Deviations diagnose:

| Q-Q pattern | Diagnosis |
|---|---|
| Straight line | Normal --- OK |
| **S-shape** (heavy tails) | Leptokurtic --- outliers; consider robust regression |
| **Inverted-S** | Light tails --- usually benign |
| **Concave up** (curves up at both ends) | Right-skewed residuals --- try $\log Y$ |
| Right tail bends above line | Right-skew, again pointing to $\log Y$ |

Formal supplement: **Shapiro--Wilk** ($H_0$: Normal). For Restaurants the Q-Q plot bends concavely upward at the right (a handful of large positive residuals from big restaurants on busy nights) --- a *second* signal pointing to the $\log Y$ remedy.

```r
par(mfrow = c(1, 2))
hist(r_std, breaks = 15, col = "steelblue", border = "white",
     main = "Histogram of standardised residuals", xlab = "r_std")
qqnorm(r_std, pch = 19, col = "steelblue", main = "Normal Q-Q")
qqline(r_std, col = "firebrick", lwd = 2)
par(mfrow = c(1, 1))

shapiro.test(r_std)                              # H0: Normal; reject if p < 0.05
```

---

### (e) Cook's distance, leverage, influence

A point can be unusual in three distinct ways --- and we need three distinct diagnostics:

| Concept | Measures | Statistic | Flag threshold |
|---|---|---|---|
| **Outlier** | Large *vertical* residual ($y_i$ far from $\hat y_i$) | $\lvert r_i^{\text{std}}\rvert$ | $>2$ inspect, $>3$ outlier |
| **Leverage** | Far in *predictor* space ($x_i$ far from $\bar x$) | $h_{ii}=[H]_{ii}$, $\sum h_{ii}=p$ | $h_{ii}>2p/n$ (here $2\cdot 2/50=0.08$) |
| **Influence** | Removing it *changes* the fit | Cook's $D_i$ | $D_i>4/n$ (here $0.08$); $D_i>1$ severe |

**Cook's distance** combines both ingredients into a single scalar --- it asks: *"how much does $\hat{\boldsymbol\beta}$ move if we delete observation $i$?"*
$$\boxed{\;D_i \;=\; \frac{(r_i^{\text{std}})^2}{p}\cdot\frac{h_{ii}}{1-h_{ii}}.\;}$$

The decomposition is the key insight: $D_i$ is *large* only when **both** factors are big --- a point must be **both** an outlier **and** high-leverage to actually move the fit. A high-leverage point with a tiny residual sits on the regression line and contributes nothing to $D_i$; a big residual at average $x$ has $h_{ii}\approx 1/n$ and again gives small $D_i$.

For Restaurants, the two biggest establishments (`surface` $\approx 280$ $m^2$) sit at $h_{ii}\approx 0.15$ --- above the $0.08$ leverage threshold --- but their residuals are moderate, so $D_i \approx 0.06 < 0.08$: they are **leverage points but not influential**. No deletion needed; OLS coefficients are stable.

```r
h    <- hatvalues(mod)
D    <- cooks.distance(mod)
flag <- data.frame(
  i = 1:n, surface = restaurants$surface, e = e, r_std = r_std,
  h = h, D = D,
  lev_flag = h > 2 * p / n,
  inf_flag = D > 4 / n
)
flag[flag$lev_flag | flag$inf_flag | abs(flag$r_std) > 2, ]

par(mfrow = c(2, 2))
plot(mod)                                        # 4 canonical diagnostic plots
par(mfrow = c(1, 1))
```

---

### (f) VIF for multicollinearity (multiple regression)

In a *multiple* regression $Y \sim X_1+\dots+X_k$, the variance of $\hat\beta_j$ inflates whenever $X_j$ can be linearly predicted from the *other* regressors:

$$\boxed{\;\mathrm{Var}(\hat\beta_j) \;=\; \frac{\sigma^2_\epsilon}{(n-1)\,s^2_{X_j}}\cdot\underbrace{\frac{1}{1-R_j^2}}_{\text{VIF}_j},\quad R_j^2 = R^2 \text{ of } X_j \sim X_{-j}.\;}$$

The **variance-inflation factor** $\mathrm{VIF}_j = 1/(1-R_j^2)$ tells us by what factor multicollinearity has inflated $\mathrm{Var}(\hat\beta_j)$ compared with the idealised orthogonal case $R_j^2=0$. Standard rules of thumb:

| VIF | Diagnosis | Action |
|---|---|---|
| $\le 1$ | No collinearity | none |
| $1$--$5$ | Mild --- usually harmless | none |
| $> 5$ | **Concerning** | inspect correlation matrix |
| $> 10$ | **Severe multicollinearity** | act: drop / combine / ridge |

For the Restaurants *multiple* model $Y \sim \text{surface}+\text{seats}+\text{evening\_only}$, `seats` and `surface` are nearly collinear ($r=0.93$), so
$$R_{\text{surface}}^2 \approx R_{\text{seats}}^2 \approx 0.87,\qquad \mathrm{VIF}\approx \frac{1}{1-0.87}\approx 7.7,$$
flagged as concerning. Symptoms in the regression output: huge SEs for $\hat\beta_{\text{surface}}$ and $\hat\beta_{\text{seats}}$, individual $t$-tests insignificant, yet the overall $F$-test highly significant and $R^2$ large --- the *classic* multicollinearity signature.

```r
modM <- lm(revenues ~ surface + seats + evening_only, data = restaurants)
library(car)
vif(modM)                                        # one number per regressor
1 / (1 - summary(lm(surface ~ seats + evening_only, restaurants))$r.squared)
cor(restaurants[, c("surface", "seats", "evening_only")])
```

---

### (g) Remedies

Pair each diagnostic failure with its standard fix:

| Symptom (which part flagged it) | Fix | When to prefer |
|---|---|---|
| Heteroscedasticity, funnel in (b)/(c); right-skew in (d) | **$\log Y$** (or $\sqrt Y$) | $Y>0$, multiplicative noise, want easy interpretation ($\hat\beta$ as % change) |
| Heteroscedasticity but $Y$ has zeros or negatives | **Box--Cox** $Y^{(\lambda)} = (Y^\lambda-1)/\lambda$; pick $\hat\lambda$ by ML | Need a data-driven transformation; `MASS::boxcox` |
| Heteroscedasticity, transformation undesirable | **WLS** with weights $1/\hat\sigma_i^2$, or **HC robust SE** (`sandwich::vcovHC`) | Coefficients unchanged, only SEs corrected |
| Multicollinearity (VIF $> 10$) in (f) | **Drop** the most redundant regressor (the one with lowest substantive priority) | Simple, transparent; coefficient interpretation stays clean |
| Multicollinearity, all regressors substantively required | **Ridge** ($\hat{\boldsymbol\beta}_\text{ridge}=(X^\top X+\lambda I)^{-1}X^\top y$) | Trades a small bias for big SE reduction; pick $\lambda$ by CV |
| Multicollinearity, want sparsity | **Lasso** ($\ell_1$ penalty) | Some coefficients shrink exactly to 0 --- automatic variable selection |
| Influential outlier in (e) | Refit *without* it; report both; or use **robust regression** (`MASS::rlm`) | Do not delete data silently --- always report sensitivity |
| Non-linearity (curvature in (c)) | Add **polynomial / spline** terms in $X$ | Preserves interpretation if curvature is mild |

For Restaurants, the recommended workflow is:
1. **Refit on $\log(\text{revenues})$** --- funnel disappears, right-tail Q-Q deviation shrinks, slope becomes a semi-elasticity (each extra $m^2$ raises revenues by $\hat\beta_1\times 100\%$).
2. In the multiple model, **either drop `seats`** (highly redundant with `surface`) **or** fit **ridge regression** if both are wanted for substantive interpretation.

```r
# Remedy 1: log-transform Y -> attacks heteroscedasticity + Normality together
modL <- lm(log(revenues) ~ surface, data = restaurants)
plot(fitted(modL), residuals(modL))              # funnel should be gone
bptest(modL)                                     # p should rise above 0.05

# Remedy 2: Box-Cox to pick lambda automatically
library(MASS)
bc <- boxcox(mod, plotit = FALSE)
lam <- bc$x[which.max(bc$y)];  lam               # ~ 0 -> confirms log

# Remedy 3: HC robust SE (keeps OLS coefs, fixes inference)
library(sandwich); library(lmtest)
coeftest(mod, vcov = vcovHC(mod, type = "HC3"))

# Remedy 4: Drop redundant regressor
modM2 <- lm(revenues ~ surface + evening_only, data = restaurants)
vif(modM2)                                       # all VIFs ~ 1 now

# Remedy 5: Ridge regression with CV-tuned lambda
library(glmnet)
X <- model.matrix(revenues ~ surface + seats + evening_only, restaurants)[, -1]
y <- restaurants$revenues
cv <- cv.glmnet(X, y, alpha = 0)                 # alpha=0 -> ridge
coef(cv, s = "lambda.min")
```

---

### Summary diagnostic checklist

| Step | Plot / Statistic | What you are looking for | Failure $\Rightarrow$ |
|---|---|---|---|
| 1 | $e$ vs $\hat y$ | Random cloud, constant spread | Funnel $\Rightarrow$ heteroscedasticity ; curvature $\Rightarrow$ misspecification |
| 2 | $e$ vs each $x_j$ | Flat band around 0 | Curvature $\Rightarrow$ add $x_j^2$ ; funnel $\Rightarrow$ $x_j$-driven heterosc. |
| 3 | Hist + Q-Q of $r^{\text{std}}$ | Bell shape, 45 deg line | Heavy tails $\Rightarrow$ outliers ; skew $\Rightarrow$ transform $Y$ |
| 4 | $\lvert r_i^{\text{std}}\rvert$ | All $<2$ ideally | $>3$ $\Rightarrow$ outlier ; investigate |
| 5 | $h_{ii}$ | All $< 2p/n$ | High leverage $\Rightarrow$ inspect (not necessarily delete) |
| 6 | Cook's $D_i$ | $<4/n$ | Large $D_i$ $\Rightarrow$ influential ; refit without and report |
| 7 | VIF$_j$ | $<5$ | $>10$ $\Rightarrow$ drop, combine, or ridge |

**Master take-aways.**
1. **Diagnostics are sequential, not parallel** --- you always look at $e$ vs $\hat y$ first; everything else is conditional on what that plot reveals.
2. **Standardise before judging residuals** --- raw $e_i$ are heteroscedastic by construction (variance $\propto 1-h_{ii}$); use $r_i^{\text{std}}$ or $r_i^{\text{stud}}$ for cross-observation comparisons.
3. **Outlier $\ne$ leverage $\ne$ influence** --- Cook's $D_i$ multiplies the two ingredients; only points that are *both* outlying *and* high-leverage actually move the fit.
4. **VIF measures redundancy, not importance** --- a regressor can have huge VIF and still be the right scientific variable; the remedy is ridge or substantive simplification, not blind deletion.
5. **One transformation often kills two birds** --- $\log Y$ usually fixes both right-skew (Normality) and funnel (heteroscedasticity) simultaneously, because both arise from multiplicative noise.

---

**Linked snippet:** Ex 8.4a (Restaurants: revenues ~ surface, $n=50$, $\hat\beta_0=246.81$, $\hat\beta_1=0.4049$ kEUR/$m^2$, $R^2\approx 0.61$ --- the dataset whose residual plot exhibits the funnel that motivates this entire master).

![Master G15e — residuals vs fitted (cone), Q-Q, Cook's distance, VIF](statistics/images/master/master_g15e_ai.png)
""",
    "images": ["statistics/images/master/master_g15e_ai.png"],
}


master_exercises["g1b_bar"] = {
    "title": "Master Exam — Bar plot (consolidated)",
    "content": r"""**Setup.** A market-research firm classifies $n=100$ pizzerias by their **District** of operation. The variable is **nominal categorical** with $K=3$ levels:

$$n_\text{Lodi}=35,\qquad n_\text{Milano}=33,\qquad n_\text{Pavia}=32,\qquad n=100.$$

Companion variables in the linked snippets are the **ordinal** `History` (None < Low < Medium < High) — used here to illustrate axis-ordering rules — and the *continuous* `Sales` (k€) — used to contrast bar plot vs histogram.

---

<details class="master-subpart" open>
<summary>(a) When is a bar plot appropriate?</summary>

A bar plot is the default graphical summary for a **categorical** variable — both *nominal* (`District`, `Sex`, `Brand`) and *ordinal* (`History`, Likert 1–5). It encodes the frequency (or relative frequency) of each category as the **length of a rectangle**, separated by **gaps** because the $x$-axis is a set of labels — not a metric scale.

Three conditions for appropriateness:

1. **The variable is categorical** (nominal or ordinal). For continuous numerical data (`Sales`), use a **histogram** (bars touch). For discrete numerical data with few values (`Children = 0,1,2,...`), use a **spike plot** (see master `g1d_spike`).
2. **The categories are exhaustive and mutually exclusive** — each observation contributes to exactly one bar.
3. **$K$ is moderate** — a bar plot scales to dozens of categories (unlike a pie chart, which fails beyond $K\approx 5$); for very large $K$, sort the bars (Pareto chart) and consider rotating to horizontal.

`District` satisfies all three with $K=3$ nominal levels and $n=100$ exhaustively assigned.

</details>

---

<details class="master-subpart">
<summary>(b) Axis ordering: nominal vs ordinal.</summary>

The rule is structural — it depends on the **measurement scale**, not on cosmetic preference:

* **Nominal variable** (no intrinsic order). The category labels can be placed on the axis in **any order** — the picture is informationally equivalent under any permutation. By convention one sorts either **alphabetically** (Lodi, Milano, Pavia — easy to find a category) or by **frequency** (Lodi 35, Milano 33, Pavia 32 — modal category leftmost; this is the **Pareto** ordering, useful for "what dominates?" questions).
* **Ordinal variable** (categories have a natural order). The axis order **must respect the scale**: `History` must be drawn as None, Low, Medium, High — *never* sorted by frequency. Reordering an ordinal axis by count destroys the very feature that makes ordinal data informative (you can no longer see whether the distribution is concentrated at "low" or "high").

For `District`, all three valid orderings (alphabetical, Pareto, original) tell the same story: **the three districts are essentially balanced** (35 / 33 / 32, a $3\%$ spread). Lodi is the modal district by a hair.

</details>

---

<details class="master-subpart">
<summary>(c) Building the bar plot — frequency vs relative frequency.</summary>

Step 1 — **decide the $y$-axis scale.** For a single sample the choice is *cosmetic*:

| Scale | Height of bar $i$ | When to prefer |
|---|---|---|
| Frequency (count) | $f_i$ | Single sample; reader wants absolute numbers |
| Relative frequency | $\hat p_i = f_i/n$ | Comparing two samples of different sizes |
| Percentage | $100\,\hat p_i$ | Reports / non-technical audience |

Step 2 — **shape invariance.** Because *every* bar is rescaled by the **same constant** ($1/n$ or $100/n$), the **shape** of the bar plot is **identical** under all three scales — only the axis tick labels change. Concretely:

| District | $f_i$ | $\hat p_i$ | $\%$ |
|---|---:|---:|---:|
| Lodi    | 35 | 0.35 | 35% |
| Milano  | 33 | 0.33 | 33% |
| Pavia   | 32 | 0.32 | 32% |
| **Total** | **100** | **1.00** | **100%** |

The *ranking* (Lodi $>$ Milano $>$ Pavia) and the *ratios* between bars are preserved. This invariance fails only when comparing **two samples of different sizes** — then frequencies make group 1 look "bigger" everywhere even when the *shapes* are equal, so always use relative frequencies in that case.

```r
# Build the bar plot for District (n = 100; Lodi 35, Milano 33, Pavia 32)
District <- factor(c(rep("Lodi", 35), rep("Milano", 33), rep("Pavia", 32)),
                   levels = c("Lodi", "Milano", "Pavia"))   # nominal -> order is cosmetic
tab <- table(District);  tab                                # Lodi 35  Milano 33  Pavia 32

# Canonical one-liner (vertical, frequency scale, alphabetical-by-construction order)
barplot(table(District), ylab = "Frequency",
        main = "District (n = 100)")

# Relative-frequency scale -> SAME SHAPE (invariance)
barplot(prop.table(table(District)), ylab = "Relative frequency",
        ylim = c(0, 0.4))

# Pareto order (nominal only): sort by frequency, modal category first
barplot(sort(table(District), decreasing = TRUE))

# Ordinal variable: KEEP the natural order, do NOT sort by frequency
History <- factor(History, levels = c("None","Low","Medium","High"))
barplot(table(History))                       # natural order preserved
```

![Master illustration](statistics/images/master/master_g1b_bar_ai.png)

</details>

---

<details class="master-subpart">
<summary>(d) Horizontal vs vertical bars.</summary>

Two display orientations exist; the choice is driven by **label length** and **number of categories**:

* **Vertical bars** (`barplot(table(x))`, default) — best when category names are short ($\le 8$ characters) and $K \le 8$. Heights read as a *count line* up the $y$-axis.
* **Horizontal bars** (`barplot(..., horiz = TRUE)`) — best when category names are long (city names, product descriptions) or $K$ is large ($\ge 10$): the names sit on the $y$-axis with room to breathe, no need to rotate text or abbreviate.

For our three districts ("Lodi", "Milano", "Pavia") either works; the vertical default is fine. If we added 12 more provinces, horizontal would become preferable.

```r
# Horizontal version (preferred for long category names or large K)
barplot(table(District), horiz = TRUE, xlab = "Frequency")
```

</details>

---

<details class="master-subpart">
<summary>(e) The zero-axis caveat.</summary>

A bar's **length** encodes the count — so the **$y$-axis must start at zero**, otherwise the visual ratio between bars is no longer proportional to the ratio of frequencies. Example: with bars 35, 33, 32, an axis cut at 30 would render visible bar lengths 5, 3, 2 — making Lodi look **2.5×** taller than Pavia (true ratio: $35/32 \approx 1.09$). This is the textbook *truncated-axis* deception; bar plots and (especially) histograms must always include zero on the value axis.

Line plots and scatter plots are different — they encode value by **position**, not length, so truncating the axis is acceptable when the data live far from zero.

</details>

---

<details class="master-subpart">
<summary>(f) Reading the picture.</summary>

For `District`:

* **Modal district:** Lodi (35), by a small margin.
* **Distribution shape:** essentially uniform across the three districts — the largest difference (Lodi vs Pavia, 3 units) is small relative to the SE of a multinomial count, $\sqrt{n\hat p_i(1-\hat p_i)} \approx \sqrt{100\cdot 0.33\cdot 0.67}\approx 4.7$. Any pairwise difference of $\le 5$ is well within sampling noise.
* **Verbal report:** "The three districts contribute roughly equally to the sample (about 1/3 each)."

A bar plot does **not** carry any cumulative or share-of-whole semantics by itself (those would be stacked bars or pie charts). It says exactly: "category $i$ occurs $f_i$ times".

```r
# CONTRAST: continuous Sales -> use hist(), not barplot()
hist(pizzerie$Sales, freq = FALSE)            # bars touch; metric x-axis
```

</details>

---

**Master take-aways.**

1. A bar plot is the **default** display for categorical data (nominal *and* ordinal). Bars have **gaps** because the $x$-axis carries labels, not distances.
2. **Axis ordering** depends on the scale: nominal $\Rightarrow$ any order (alphabetical or Pareto); ordinal $\Rightarrow$ natural order, **never** sorted by frequency.
3. **Shape is invariant** under frequency $\leftrightarrow$ relative frequency $\leftrightarrow$ percentage — the rescaling is the same constant for every bar. Pick the scale by audience and by whether two samples are being compared.
4. **Orientation** (vertical vs horizontal) is a readability choice — go horizontal when names are long or $K$ is large.
5. **Always include zero on the value axis** — bars encode counts by *length*, so a truncated axis breaks the visual proportionality and misleads the reader.
6. For `District` here, the three counts (35 / 33 / 32) lie inside the multinomial sampling-noise band, so the chart's qualitative message is **"districts are balanced; no dominant one"**.

---

**Linked snippets:** Ex 1, Q1.1b (`District`, nominal Lodi/Milano/Pavia — the dataset used here); Ex 1, Q1.2d (`History`, ordinal None/Low/Medium/High — the *counter-example* for axis ordering, where Pareto sorting would be wrong); Ex 0.1a/c (`Sales`, continuous — the contrast case where one must use a histogram, not a bar plot).
""",
    "images": ["statistics/images/master/master_g1b_bar_ai.png"],
}


master_exercises["g1d_spike"] = {
    "title": "Master Exam — Spike plot (consolidated)",
    "content": r"""**Setup.** From the customer-survey dataset of $n=100$ pizzerias we examine **Children** = number of children seated at a table during the visit. The variable is **discrete numerical** (counts $\{0,1,2,3,4,5\}$) with the frequency table

| Children $x$ | 0 | 1 | 2 | 3 | 4 | 5 | Total |
|---|---:|---:|---:|---:|---:|---:|---:|
| Frequency $f_x$ | 24 | 27 | 29 | 15 | 3 | 2 | **100** |
| Rel. freq. $\hat p_x$ | 0.24 | 0.27 | 0.29 | 0.15 | 0.03 | 0.02 | **1.00** |

Companion variables in the linked snippets are the *continuous* `Sales` (k€, requires a **histogram**) and the *categorical* `District` (nominal, requires a **bar plot**). They serve here as structural contrast cases.

---

<details class="master-subpart" open>
<summary><span class="tag tag-4plus">≥4 ex</span> (a) When is a spike plot appropriate?</summary>

A spike plot (also called a *needle plot* or *line plot* in some textbooks) is the right tool for a **discrete numerical** variable — i.e. a quantitative variable whose support is a *countable* set of values, typically integers (counts: `Children`, `Goals`, `# defects`; or coarsely quantised measurements with few distinct values).

Three structural conditions:

1. **Quantitative $x$-axis with metric meaning.** Unlike a bar plot, the gap between $x=0$ and $x=1$ encodes the *true distance* on the number line — so the spikes are placed at the actual integer positions, with empty horizontal space between them being a meaningful "no observations at non-integer values".
2. **Discrete support** (few distinct values, typically $\le 15$). For a discrete variable with hundreds of distinct integer values (e.g. age in years, $n=10\,000$), a histogram with width-1 bins is essentially equivalent and usually more legible.
3. **Frequencies (or relative frequencies) on the $y$-axis.** Each spike is a vertical line segment from $y=0$ up to $y=f_x$ (or $\hat p_x$); the line has **zero width**, so the picture honestly says "all probability mass sits exactly at this integer".

`Children` satisfies all three: support $\{0,\dots,5\}$, six distinct integers, $n=100$.

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-4plus">≥4 ex</span> (b) Construction — spikes at integers.</summary>

The spike plot has the form
$$\{(x, f_x) : x \in \{0,1,2,3,4,5\}\},$$
drawn as a **vertical segment** from $(x,0)$ to $(x, f_x)$ for each integer in the support. No connection between consecutive spikes (no "polyline"), because the variable does not take non-integer values.

Heights (frequency scale): $24, 27, 29, 15, 3, 2$. The picture immediately shows:

* **Modal value:** $x=2$ (tallest spike, $f_2 = 29$). The mode for discrete data is a **single value**, not a class.
* **Concentration:** the three spikes $x \in \{0,1,2\}$ carry $24+27+29 = 80 = 80\%$ of the mass.
* **Right-skew:** spikes at $x=4,5$ are tiny ($3+2 = 5$), the tail thins out quickly.

As with bar plots, the **$y$-axis must start at zero** — the spike *length* encodes the count.

```r
# Build the data as a frequency table
Children <- c(rep(0,24), rep(1,27), rep(2,29), rep(3,15), rep(4,3), rep(5,2))
tab <- table(Children);  tab                       # 0:24  1:27  2:29  3:15  4:3  5:2

# Canonical spike-plot one-liner
plot(table(Children), type = "h",
     xlab = "Children", ylab = "Frequency",
     main = "Children per table (n = 100)")

# Equivalent explicit form: spikes at integer positions
x  <- 0:5;  fx <- c(24, 27, 29, 15, 3, 2)
plot(x, fx, type = "h", lwd = 3,
     xlab = "Children", ylab = "Frequency")
points(x, fx, pch = 16)                            # optional: dot on top

# Relative-frequency scale (shape invariant; rescale by 1/n)
plot(prop.table(table(Children)), type = "h",
     ylab = "Relative frequency")
```

![Master illustration](statistics/images/master/master_g1d_spike_ai.png)

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (c) Visible gaps and the "no fractional children" property.</summary>

The horizontal **gaps** between spikes are *informative* — they represent the fact that the variable cannot take values like $x=2.5$. This is the structural difference from a histogram: a histogram of integer counts would draw width-1 bars **touching** each other, falsely suggesting a continuum where none exists.

The same logic explains why we cannot use a *bar plot* either: a bar plot's $x$-axis has **no metric**, so the visual distance between bars is arbitrary; a spike plot's $x$-axis is the **integer number line**, with distance $|x-y|$ meaningful.

</details>

---

<details class="master-subpart">
<summary>(d) Spike plot vs histogram — why the histogram misleads for discrete data.</summary>

Suppose, instead of spikes, we drew a histogram with class breaks $[-0.5, 0.5), [0.5, 1.5), \dots, [4.5, 5.5)$ — width-1 bars containing one integer each. Three problems arise:

1. **False sense of continuity.** The touching bars suggest that fractional values are possible; a reader unfamiliar with the variable might interpolate "2.5 children" as a meaningful quantity.
2. **Arbitrary class boundaries.** Why $[-0.5, 0.5)$ and not $[0, 1)$? Any choice introduces visual ambiguity at the boundaries that does not exist in the data.
3. **Width-vs-height confusion.** With unequal classes (e.g. pooling the rare $\{4,5\}$ into a single $[3.5, 5.5)$ bar of width 2), one must switch to *density* and the picture becomes hard to compare to the integer frequencies.

The spike plot avoids all three issues by being **honest about the discreteness**: zero-width spikes, no interpolation, no boundary choices.

```r
# WRONG for discrete data: a histogram falsely suggests continuity
hist(Children, breaks = seq(-0.5, 5.5, by = 1))    # touching bars -> misleading
```

A useful summary:

| Variable type | Display | Bars |
|---|---|---|
| Continuous numerical (`Sales`) | Histogram | touch, metric $x$-axis |
| Discrete numerical with few values (`Children`) | **Spike plot** | zero-width, metric $x$-axis with gaps |
| Categorical, nominal or ordinal (`District`, `History`) | Bar plot | gaps, label $x$-axis |

```r
# CONTRAST: continuous variable -> histogram is correct
hist(pizzerie$Sales, freq = FALSE)                 # bars touch; metric x-axis
# CONTRAST: categorical variable -> bar plot with gaps
barplot(table(District))                           # x-axis is labels, not metric
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (e) Reading the picture — mode, skew, mean.</summary>

Quantitative summaries that are *consistent* with the spike plot for `Children`:

* **Mode** = $2$ (tallest spike, $f_2=29$).
* **Sample mean** = $\bar x = \tfrac{1}{n}\sum_x x f_x = \tfrac{0\cdot24 + 1\cdot27 + 2\cdot29 + 3\cdot15 + 4\cdot3 + 5\cdot2}{100} = \tfrac{0+27+58+45+12+10}{100} = \tfrac{152}{100} = 1.52$.
* **Median** = $1$. Recipe: "smallest $x$ with cumulative count $\ge n/2 = 50$". Cumulative counts: $24$ at $x=0$; $24+27=51$ at $x=1$ — already $\ge 50$, so $\widetilde x = 1$. *(No interpolation: for a discrete variable the median is always one of the observed values.)*
* **Skewness:** with $\text{mean}=1.52$, $\text{median}=1$, $\text{mode}=2$, the relationship $\text{mean} > \text{median}$ confirms **right (positive) skew** — visible in the long thin right tail of the spike plot ($x=4,5$).

```r
# Recompute the sample summaries from the frequency table
x  <- 0:5
fx <- c(24, 27, 29, 15, 3, 2)
sum(x * fx) / sum(fx)                              # 1.52  (mean)
# median: smallest x with cumsum(fx) >= n/2 = 50
x[which(cumsum(fx) >= 50)[1]]                      # 1    (median)
x[which.max(fx)]                                   # 2    (mode)
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (f) Gaps in the support — when an observed value is *missing*.</summary>

A subtlety arises when the support is **not** a contiguous integer range. In **Ex 1.4a1** the variable `Quantity_New` takes values $\{1,2,3,4,6\}$ — the value $5$ is **never observed** in the sample ($n=24\,240$). A correct spike plot must leave a **visible blank** at $x=5$:

```r
# Quantity_New: value 5 is NEVER observed
x  <- c(1, 2, 3, 4, 6);  fx <- c(5401, 7340, 8238, 2561, 700)
plot(x, fx, type = "h", lwd = 3, xlim = c(0, 7),
     xlab = "Quantity_New", ylab = "Frequency")
points(x, fx, pch = 16)
# Note the visible gap at x = 5: "no one bought exactly 5 items".
```

Why a **bar plot is wrong here.** A bar plot's $x$-axis is a *label* axis, so placing the labels `"1" "2" "3" "4" "6"` next to one another would draw five bars side by side and **close the visual gap between 4 and 6**, falsely suggesting a smooth transition. Only the spike plot, with its **metric** $x$-axis, honestly displays the missing $x=5$.

This is the canonical exam pitfall: *whenever the discrete support has gaps (unobserved integers inside the range), only a spike plot represents the data without distortion.*

</details>

---

**Master take-aways.**

1. **Spike plot $=$ honest display of a discrete numerical variable.** Each spike is a vertical line of zero width at an integer $x$-value, so the picture never suggests fractional outcomes.
2. **Metric $x$-axis.** The visible *gaps* between spikes encode the impossibility of non-integer values — this is the structural difference from a bar plot (label axis) and from a histogram (touching bars on a continuous axis).
3. **Histograms mislead for discrete data:** their touching bars imply a continuum, their boundary choices are arbitrary, and unequal classes force a density rescaling that breaks intuition.
4. **Shape invariance** under frequency $\leftrightarrow$ relative frequency $\leftrightarrow$ percentage holds exactly as for bar plots — the same global rescaling, the same picture.
5. **Gaps in the support** (unobserved integers inside the range, e.g. `Quantity_New` skipping $5$) make the spike plot *mandatory*: a bar plot would collapse the gap and mislead.
6. For `Children` here, the spike plot reveals a **right-skewed discrete distribution** with mode at $2$, mean $\bar x = 1.52$, median $=1$, and a thin tail at $x=4,5$.

---

**Linked snippets:** Ex 0.2b2 (spike plot — discrete count contrast case); Ex 1.2c (`Children`, discrete counts $\{0,1,2,3\}$ with $n=750$ — companion dataset); Ex 1.3e (`Age`, discrete with many values — spike preferred over histogram when frequencies are similar); Ex 1.4a1 (`Quantity_New`, gap at $x=5$ — the structural case in part (f), where a bar plot would close the gap).
""",
    "images": ["statistics/images/master/master_g1d_spike_ai.png"],
}


master_exercises["g2b_approx"] = {
    "title": "Master Exam — Approximating cumulative proportions inside a grouped class (Uniform-on-interval)",
    "content": r"""**Setup.** A telecom operator recorded `Duration` (seconds) for $n=400$ customer-service calls, then released the data only in **grouped form**:

| Class $[L_j,\,R_j)$ | Width $w_j=R_j-L_j$ | Frequency $n_j$ | Rel. freq. $f_j=n_j/n$ | Cum. rel. freq. $F_j$ |
|---|---:|---:|---:|---:|
| $[0,\,10)$    | 10 | 40  | 0.10 | 0.10 |
| $[10,\,30)$   | 20 | 160 | 0.40 | 0.50 |
| $[30,\,60)$   | 30 | 120 | 0.30 | 0.80 |
| $[60,\,120)$  | 60 | 80  | 0.20 | 1.00 |
| **Total**     |    | 400 | 1.00 |      |

Only the **counts in each class are known** — the raw $x_i$ values inside each class were discarded. We want to estimate cumulative proportions $\widehat F(x)=\widehat{\mathbb P}(X\le x)$ at points $x$ that fall **inside** a class (so $F$ is not given to us directly).

---

### (a) The Uniform-on-interval assumption

The only statement the grouped table makes is: **a fraction $f_j$ of the data lies in $[L_j,R_j)$.** To turn this into a function $\widehat F(x)$ at every $x$, we need an assumption about **how** those $n_j$ points are spread *inside* the class. The standard, minimal-information choice is:

$$\boxed{\;X\mid X\in[L_j,R_j)\;\sim\;\mathrm{Uniform}[L_j,R_j)\;}$$

i.e. inside each class the points are evenly spread. Equivalently, the **density** on class $j$ is constant:
$$\widehat f(x)\;=\;\frac{f_j}{w_j}\qquad\text{for }x\in[L_j,R_j).$$

This is the **histogram density** — the height of the histogram bar drawn so that *area = relative frequency*. Two consequences:
1. The grouped CDF $\widehat F$ is **piecewise linear**: flat-slope ramps inside each class, joined at the class boundaries.
2. At a boundary $R_j$, $\widehat F(R_j)=F_j$ (the cumulative entry from the table); inside the class we **linearly interpolate**.

---

### (b) Linear-interpolation formula for $\widehat F(x)$, $x\in[L_j,R_j)$

The fraction of class $j$'s width that lies to the left of $x$ is $(x-L_j)/w_j$. Under Uniform-on-interval, the same fraction of class $j$'s mass $f_j$ lies to the left of $x$:
$$\widehat F(x)\;=\;F_{j-1}\;+\;\frac{x-L_j}{w_j}\,f_j\qquad\text{(linear ramp inside class }j\text{)}.$$
Equivalently in **density-times-overlap** form,
$$\widehat F(x)\;=\;F_{j-1}\;+\;\widehat f(x)\cdot(x-L_j)\;=\;F_{j-1}\;+\;\frac{f_j}{w_j}\,(x-L_j),$$
which is the **same number** — just two different bookkeeping views of one straight-line interpolation.

---

### (c) Worked example: $\widehat{\mathbb P}(X\le 15)$

$x=15$ lies in class $j=2$, namely $[L_2,R_2)=[10,30)$, with $w_2=20$, $f_2=0.40$, $F_1=0.10$.

**View 1 — linear interpolation of $F$.** Fraction of the class to the left of $15$ is $(15-10)/20=5/20=0.25$, so $0.25$ of class 2's mass $0.40$ has accumulated:
$$\widehat F(15)\;=\;F_1\;+\;0.25\cdot f_2\;=\;0.10\;+\;0.25\cdot 0.40\;=\;0.10+0.10\;=\;0.20.$$

**View 2 — density times overlap.** Density on $[10,30)$ is $\widehat f=f_2/w_2=0.40/20=0.02$. Mass between $10$ and $15$ is $\widehat f\cdot(15-10)=0.02\cdot 5=0.10$. Add $F_1=0.10$ to get $0.20$. **Same answer.**

So roughly **20% of calls last $\le 15$ s** under the grouped-data Uniform-on-interval approximation.

```r
# Grouped table
L <- c(0, 10, 30, 60); R <- c(10, 30, 60, 120)
n <- 400
freq  <- c(40, 160, 120, 80)
f     <- freq / n                                  # 0.10 0.40 0.30 0.20
F_cum <- cumsum(f)                                  # 0.10 0.50 0.80 1.00

# Hat-F at any x via linear interpolation inside the bracketing class
F_hat <- function(x) {
  if (x <= L[1]) return(0)
  if (x >= R[length(R)]) return(1)
  j  <- max(which(L <= x))                          # bracketing class index
  Fjm1 <- if (j == 1) 0 else F_cum[j - 1]
  Fjm1 + (x - L[j]) / (R[j] - L[j]) * f[j]
}
F_hat(15)                                           # 0.20

# Same answer via density * overlap
dens <- f / (R - L)                                 # histogram density per class
dens[2] * (15 - 10) + F_cum[1]                      # 0.02 * 5 + 0.10 = 0.20

# R's built-in equivalent on the grid of class boundaries:
approx(x = c(L[1], R), y = c(0, F_cum), xout = 15)$y   # 0.20
```

![Master illustration](statistics/images/master/master_g2b_approx_ai.png)

---

### (d) Two boundary checks (always do these)

1. **At $x=L_j$** (left edge of class $j$): the formula gives $\widehat F(L_j)=F_{j-1}+0\cdot f_j=F_{j-1}$ — matches the cumulative entry just before class $j$.
2. **At $x=R_j$** (right edge): $\widehat F(R_j)=F_{j-1}+1\cdot f_j=F_j$ — matches the cumulative entry at the end of class $j$.

So $\widehat F$ is **continuous and increasing**, made of straight segments whose slopes are the histogram densities $f_j/w_j$. In our table the slopes are $\{0.010,\,0.020,\,0.010,\,0.0033\}$ per second — call density is highest in class 2 (steepest ramp).

---

### (e) Quantile (inverse) direction — same machinery in reverse

To find the median (or any quantile $q$), locate the class containing $q$ and invert the linear ramp. With $q=0.5$, $F_1=0.10<0.5\le F_2=0.50$, so the median lies in class 2, and
$$x_{0.5}\;=\;L_2\;+\;\frac{q-F_{j-1}}{f_j}\,w_j\;=\;10\;+\;\frac{0.5-0.10}{0.40}\cdot 20\;=\;10+20\;=\;30.$$
(Exactly the right edge — a coincidence of this dataset.) For $q=0.25$: $x_{0.25}=10+\tfrac{0.25-0.10}{0.40}\cdot 20=10+7.5=17.5$ s.

```r
q_hat <- function(q) {
  j <- min(which(F_cum >= q))
  Fjm1 <- if (j == 1) 0 else F_cum[j - 1]
  L[j] + (q - Fjm1) / f[j] * (R[j] - L[j])
}
q_hat(0.25); q_hat(0.50); q_hat(0.75)               # 17.5  30.0  52.5
```

---

### (f) When is Uniform-on-interval reasonable?

- **Reasonable** when class widths are small *relative to the variation of the true density* — within $[10,30)$ a true unimodal density is approximately flat over 20 seconds.
- **Suspect** in the *first* and *last* class when widths are large or the distribution is heavy-tailed: e.g. $[60,120)$ is 60 s wide; if calls really follow a decreasing density there, Uniform-on-interval **overestimates** $F(x)$ at $x$ just past 60 and **underestimates** near 120. This is the **grouping bias** of all summary statistics computed from the table (mean-from-midpoints, variance, etc.).
- **Diagnostic.** If you also have raw data on a subset, overlay the piecewise-linear $\widehat F$ on the empirical CDF; large deviations inside wide classes warn you to either keep classes narrow at registration time or use a smoother model (e.g., midpoint-based spline, or a parametric fit such as $\mathrm{Exp}(\lambda)$ matched to the grouped mean).

---

### Master take-aways

1. **One assumption** — Uniform-on-interval — converts a histogram into a fully-specified estimator of $\widehat F$ and $\widehat f$.
2. **Inside a class, $\widehat F$ is a straight line**; two equivalent computations: (i) $F_{j-1}+\frac{x-L_j}{w_j}f_j$, (ii) $F_{j-1}+\frac{f_j}{w_j}(x-L_j)$. Pick whichever is easier to read off the table.
3. **Boundaries match the table**: $\widehat F(L_j)=F_{j-1}$, $\widehat F(R_j)=F_j$ — always sanity-check at the edges.
4. **Quantiles invert the same ramp** — useful for grouped-data median / IQR.
5. **The approximation is exactly as good as the class widths are narrow** relative to the true density's variation; wide tail-classes are the usual culprits for grouping bias.

---

**Linked snippets.** Ex 0.1g (approx P(Frost < 80) via uniform-on-interval on a grouped table); Ex 0.2a2 (approx P(50 $\le$ fare < 100) via the same linear interpolation); Ex 1.5a (P(Time $\le$ 5) from a grouped histogram); Ex 1.5c (P(15 $\le$ Time $\le$ 50) by subtracting two linear-interp values on the ogive); Ex 2.2a / 2.2a1 (cumulative-frequency table $\to$ ogive $\to$ interpolated probabilities); master `g2a_exact` (the exact counterpart, used when raw data are available).
""",
    "images": ["statistics/images/master/master_g2b_approx_ai.png"],
}


master_exercises["g3_main"] = {
    "title": "Master Exam — Derived variables (rates, densities, margins) and the bin-and-compare workflow",
    "content": r"""**Setup.** A criminologist has a state-level dataset `crime` with $n=50$ rows (one per US state) and two raw columns:

| State | Population | ViolentCrimes |
|---|---:|---:|
| Alabama  | 4\,903\,000 |  21\,693 |
| Alaska   |   731\,000 |   6\,302 |
| Arizona  | 7\,279\,000 |  35\,031 |
| ...      | ... | ... |
| Wyoming  |   579\,000 |   1\,317 |

Comparing **counts** of ViolentCrimes across states is misleading — California will dominate Wyoming purely because $\text{Pop}_\text{CA}\gg\text{Pop}_\text{WY}$. To compare *intensity* we need a **derived variable** that scales out population. That is the entire point of this master: building derived variables, choosing the right normaliser, and using them in a **bin-and-compare** workflow.

---

<details class="master-subpart" open>
<summary>(a) Constructing `Rate.Violent` per 100\,000 inhabitants</summary>

The conventional public-health / criminology rate is
$$\boxed{\;\text{Rate.Violent}_i\;=\;\frac{\text{ViolentCrimes}_i}{\text{Population}_i}\times 100\,000\;}\qquad\text{[violent crimes per 100k people per year]}.$$

The factor $10^5$ is **only a unit choice**: it makes the typical state-level rate fall in the readable range 100–800 instead of $0.001$–$0.008$. The *ranking* of states is identical for any positive multiplier.

```r
crime$Rate.Violent <- crime$ViolentCrimes / crime$Population * 1e5
head(crime[order(-crime$Rate.Violent), c("State", "Population",
                                         "ViolentCrimes", "Rate.Violent")], 5)
##           State Population ViolentCrimes Rate.Violent
## Alaska    Alaska     731000          6302       862.10
## NewMexico ...        2096000         16671      795.37
## Tennessee ...        6829000         42704      625.34
## Louisiana ...        4649000         25542      549.41
## Arkansas  ...        3018000         15124      501.13
```

**What changed?** California ($n_\text{crime}\approx 175{,}000$) drops out of the top five entirely once you normalise by population, and Alaska — small absolute numbers but a tiny denominator — jumps to #1. This is the **whole reason** derived variables exist: raw counts confound *intensity* with *size*.

---

### (b) Why per-100k (a "rate") rather than the raw ratio?

The ratio $\text{ViolentCrimes}/\text{Population}$ is **already** a unit-free comparison; multiplying by $10^5$ just rescales for readability. Equivalently you can interpret Rate.Violent as
$$\widehat{\mathbb P}(\text{a randomly drawn inhabitant was a violent-crime victim this year})\times 100\,000.$$
So Alaska's $862$ means roughly $0.86\%$ of Alaskans were victims of a recorded violent crime — a probability statement on the **same scale** for every state regardless of size.

---

### (c) Bin-and-compare workflow

Once Rate.Violent exists, all standard distribution-comparison tools become available — but applied to the *derived* variable, not the raw count.

**Step 1 — Bin the continuous derived variable into 4 classes** (equal-width here; quantile-bins are also common):

| Class (rate per 100k) | $n_j$ | $f_j$ | $F_j$ |
|---|---:|---:|---:|
| [100, 250)  | 14 | 0.28 | 0.28 |
| [250, 400)  | 18 | 0.36 | 0.64 |
| [400, 550)  | 11 | 0.22 | 0.86 |
| [550, 900)  |  7 | 0.14 | 1.00 |
| **Total**   | 50 | 1.00 |      |

**Step 2 — Compare a low-rate group vs. a high-rate group.** Split states into "Low" (Rate.Violent below national median) and "High" (above). Compare mean Population, mean GDP-per-capita, etc., between the two groups. Now any subsequent *comparison* is fair: high-rate vs. low-rate states do not differ trivially because of size.

```r
# Step 1 — bin Rate.Violent into 4 equal-width classes
brks <- c(100, 250, 400, 550, 900)
crime$Rate.Bin <- cut(crime$Rate.Violent, breaks = brks, right = FALSE)
table(crime$Rate.Bin)

# Step 2 — Low / High split at the median
med <- median(crime$Rate.Violent)
crime$Group <- ifelse(crime$Rate.Violent < med, "Low", "High")
aggregate(Population ~ Group, data = crime, FUN = mean)
aggregate(ViolentCrimes ~ Group, data = crime, FUN = mean)

# Step 3 — graphical comparison
hist(crime$Rate.Violent, breaks = brks, freq = FALSE,
     main = "Violent-crime rate per 100k by state", xlab = "rate")
boxplot(Rate.Violent ~ Group, data = crime, col = c("#fbb35a", "#1d2b54"))
```

**Why does this work?** All three steps operate on the **per-capita rate**, never on raw counts. The bins, the median split, the histogram — all answer the same kind of question on a **comparable scale**.

![Master illustration](statistics/images/master/master_g3_main_ai.png)

---

### (d) The same template for other common derived variables

Whenever the raw measurement is a *count or a total* and you want a *comparable* number, divide by the natural "size" variable:

| Derived variable | Formula | Numerator | Denominator | Typical scale factor |
|---|---|---|---|---|
| **Rate per capita** | $\text{Crimes}/\text{Population}$ | count | head-count | $\times 10^5$ ("per 100k") |
| **Population density** | $\text{Population}/\text{Area}$ | head-count | area (km$^2$) | $\div 1$ (per km$^2$) |
| **GDP per capita** | $\text{GDP}/\text{Population}$ | currency total | head-count | currency / person |
| **Profit margin %** | $(\text{Rev}-\text{Cost})/\text{Rev}$ | currency *difference* | currency total | $\times 100$ (%) |
| **Unemployment rate** | $\text{Unemployed}/\text{LabourForce}$ | count | count | $\times 100$ (%) |
| **Birth rate** | $\text{Births}/\text{Population}$ | count | head-count | $\times 10^3$ ("per 1000") |

```r
# Population density (people per km^2)
crime$Density <- crime$Population / crime$Area

# Profit margin (% of revenue)
firms$Margin.Pct <- (firms$Revenue - firms$Cost) / firms$Revenue * 100

# Unemployment rate (%)
labour$Unemp.Rate <- labour$Unemployed / labour$LabourForce * 100
```

The structural pattern is always the same:
1. **Identify the confounder of size** (Population, Area, Revenue, LabourForce, ...).
2. **Divide** the count/total by that confounder.
3. **Rescale** for readability (×100 for %, ×100k for rates, leave raw for densities).
4. **Replace the raw variable** with the derived one throughout the analysis.

---

### (e) Pitfalls and sanity checks

1. **Zero-denominator rows.** If any Population (or Area, Revenue, ...) is 0 or near-0, the rate is undefined or explodes. Drop or winsorise:
   ```r
   crime <- subset(crime, Population > 0)
   ```
2. **Different reference periods.** Crime counts are annual; if Population is a mid-year estimate, ensure both refer to the same year. A 5% timing mismatch becomes a 5% rate error.
3. **Small-denominator instability.** A state of $50\,000$ people with 3 recorded murders has rate $6.0$/100k, but the *uncertainty* is enormous (a Poisson SE of $\sqrt{3}/50000\times 10^5 \approx 3.5$). Always report SE or CI for rates from small denominators:
   $$\widehat{\mathrm{SE}}(\text{Rate})\;=\;\sqrt{\widehat\lambda/N}\times 10^5,\qquad \widehat\lambda=\text{count}/\text{Pop}.$$
4. **Margin% with near-zero revenue.** $(\text{Rev}-\text{Cost})/\text{Rev}$ is unstable when Rev is tiny; consider log-revenue or absolute profit instead.
5. **Do not double-normalise.** Rate.Violent is already per-capita; do not then divide by Population again when comparing across states.

---

### (f) Putting it together — the analysis pipeline

```r
# 1. Load and audit
crime <- read.csv("crime.csv")
stopifnot(all(crime$Population > 0))                 # no zero denominators

# 2. Build derived variable(s)
crime$Rate.Violent <- crime$ViolentCrimes / crime$Population * 1e5

# 3. Describe the derived variable
summary(crime$Rate.Violent)                          # min, Q1, median, mean, Q3, max
sd(crime$Rate.Violent)
hist(crime$Rate.Violent, breaks = 8, freq = FALSE)

# 4. Bin / group / compare
crime$Group <- ifelse(crime$Rate.Violent > median(crime$Rate.Violent),
                      "High", "Low")
aggregate(cbind(Population, ViolentCrimes) ~ Group, data = crime, FUN = mean)

# 5. Visualise the comparison
boxplot(Rate.Violent ~ Group, data = crime)

# 6. (Optional) Quantify the SE for small states
crime$Rate.SE <- sqrt(crime$ViolentCrimes) / crime$Population * 1e5
```

---

### Master take-aways

1. **Raw counts confound size with intensity** — *never* compare counts across units that differ in scale. Always derive a per-unit quantity first.
2. **Per-capita rate, density, and margin %** are three instances of the **same recipe**: numerator divided by the natural size variable, then rescaled for readability.
3. **The scale factor ($\times 100$, $\times 10^3$, $\times 10^5$) is cosmetic** — it changes readability, not rankings or inference.
4. **Bin the *derived* variable**, not the raw one, when running histogram / median-split / two-group comparison workflows.
5. **Watch small denominators**: rates from tiny denominators have huge Poisson SE; report uncertainty or drop them.
6. **Audit before deriving**: zero/missing denominators, mismatched reference periods, and units (per-year vs. per-month) are the most common silent bugs.

---

**Linked snippets.** Ex 0.1b (building the Density variable: Population $\div$ Area --- a derived "per-area" magnitude); Ex 0.1d (building Rate.Violent = ViolentCrimes / Population $\times 10^5$ --- the canonical per-capita rate used throughout this master); Ex 2.8a (Margin = (Revenue $-$ Cost)/Revenue, the *difference-over-total* template applied to firm-level profitability).
""",
    "images": ["statistics/images/master/master_g3_main_ai.png"],
}


master_exercises["g4a_bytype"] = {
    "title": "Master Exam — Mode, median, mean by variable type (consolidated)",
    "content": r"""**Setup.** A *measure of central tendency* answers the question "what is a typical value?". The three classical measures --- **mode** (most frequent value), **median** (middle value when data are ordered), and **mean** $\bar x = \tfrac{1}{n}\sum_{i=1}^n x_i$ --- are **not interchangeable**: each one requires a specific *measurement scale* to be meaningful. Picking the wrong one is not a numerical mistake, it is a **scale-of-measurement mistake**: e.g. the mean of `District = {A,B,C}` is undefined, and the mean of `History = {None,Low,Medium,High}` is *computable but meaningless* because the gaps between adjacent levels are not numerically defined.

This master organises the rule around the four scales used throughout Ex 1 / Ex 2 (`District` / `Country` --- nominal; `Age_recode` --- ordinal; `Quantity_New` --- discrete numerical; `Sales` / `Time` --- continuous numerical) and works one numeric example per scale.

---

<details class="master-subpart" open>
<summary><span class="tag tag-4plus">≥4 ex</span> (a) Decision table --- which measure applies to which scale?</summary>

| Scale | Example variable | Operations allowed | **Mode** | **Median** | **Mean** |
|---|---|---|:---:|:---:|:---:|
| **Nominal** (unordered categories) | `District` (A, B, C, D) | $=, \ne$ only | yes | **no** (no order) | **no** (no arithmetic) |
| **Ordinal** (ordered categories, gaps undefined) | `History` (None < Low < Medium < High) | $=, \ne, <, >$ | yes | yes | **no** (gaps not numeric) |
| **Discrete numerical** (integer counts) | `Children` $\in\{0,1,2,3,\dots\}$ | $=, \ne, <, >, +, -$ | yes | yes | yes |
| **Continuous numerical** (real-valued) | `Sales` ($\in\mathbb{R}^+$, kEUR) | $=, \ne, <, >, +, -, \times, \div$ | yes (binned) | yes | yes |

**Mnemonic --- "the more structure the scale has, the more measures you may use":** nominal $\Rightarrow$ mode only; ordinal $\Rightarrow$ adds the median (because order is defined); numerical $\Rightarrow$ adds the mean (because distances are defined).

A "yes" in the table only means *the measure is well-defined*; whether it is the **best** summary is a separate question (taken up in master **g4b** for skewed numerical data).

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (b) Nominal --- only the **mode** is meaningful</summary>

Take `District` with the $n=20$ observed values

$$\{A,A,A,A,A,A,A,A,\;B,B,B,B,B,\;C,C,C,C,\;D,D,D\}.$$

**Frequency table.**

| District | $A$ | $B$ | $C$ | $D$ | total |
|---|---:|---:|---:|---:|---:|
| $n_i$ | 8 | 5 | 4 | 3 | 20 |

* **Mode:** the category with the largest count, $\widehat{\text{Mo}} = A$ (frequency 8 of 20).
* **Median?** Even if we sort the *labels* alphabetically, "alphabetical" is a label property, not a property of the underlying variable: the order $A < B < C < D$ is **arbitrary** (re-label and "the median" changes). **Not defined.**
* **Mean?** Would require $(A+A+B+\dots)/n$, which is symbolic nonsense. **Not defined.**

The *only* legitimate summary is "the modal district is $A$, with $40\%$ of customers". This is exactly the situation of **Ex 1, Q1.1f** for `District` (pizzerie, mode = `Lodi`) and **Q1.3d** for `Country` (customer_habits, dominant European country).

```r
District <- factor(c(rep("A",8), rep("B",5), rep("C",4), rep("D",3)))
table(District)                                # A 8  B 5  C 4  D 3
names(which.max(table(District)))              # "A"   <- mode
# median(District);  mean(District)            # would ERROR -- as it should
```

</details>

---

<details class="master-subpart">
<summary>(c) Ordinal --- **mode + median**, NOT the mean</summary>

Take `History` with the four ordered levels $\text{None} < \text{Low} < \text{Medium} < \text{High}$ and the $n=11$ observed sequence (already sorted on the underlying order)

$$\{\text{None},\text{None},\text{Low},\text{Low},\text{Low},\;\underbrace{\text{Medium}}_{\text{position }6},\;\text{Medium},\text{Medium},\text{High},\text{High},\text{High}\}.$$

**Frequency table.**

| History | None | Low | Medium | High | total |
|---|---:|---:|---:|---:|---:|
| $n_i$ | 2 | 3 | 3 | 3 | 11 |

* **Mode:** the three categories `Low`, `Medium`, `High` all have $n_i=3$ --- the distribution is **tri-modal**. (If a unique mode is required, report all three.)
* **Median:** with $n=11$ (odd) the median is the value at position $(n+1)/2 = 6$ in the sorted list. Counting along the sorted sequence, position 6 falls in the `Medium` block, so $\widehat{\text{Me}} = \text{Medium}$.
* **Mean?** Computing $\bar x = (\text{None}+\text{Low}+\dots)/11$ is **not defined** without a numeric coding. The temptation is to code $\{\text{None},\text{Low},\text{Medium},\text{High}\}\mapsto\{0,1,2,3\}$ and average to get $\tfrac{2\cdot 0+3\cdot 1+3\cdot 2+3\cdot 3}{11}=\tfrac{18}{11}\approx 1.64$ --- but **the gap "Low--Medium" is not necessarily equal to "Medium--High"**, so this number is an artefact of the coding (use $\{0,1,2,10\}$ and the "mean" changes drastically). Report the **median** instead, which is invariant under any monotone re-coding.

```r
History <- factor(c("None","None","Low","Low","Low",
                    "Medium","Medium","Medium","High","High","High"),
                  levels = c("None","Low","Medium","High"), ordered = TRUE)
table(History)
median(History)                                # Medium      <- ordinal median is OK
# mean(as.numeric(History))                    # 2.64 -- arbitrary, do NOT report
```

This is the situation of **Ex 1, Q1.2b** (`Age_recode` --- ordinal, mode + median = `Middle`); the contrast case for a *continuous* modal-class reading is discussed in **Q1.5e** (`Time`).

</details>

---

<details class="master-subpart">
<summary>(d) Discrete numerical --- **all three** are meaningful</summary>

Take `Children` (number of children per household) with the $n=15$ observed values

$$\{0,0,0,0,0,\;1,1,1,1,\;2,2,2,\;3,3,\;5\}.$$

* **Mode:** $\widehat{\text{Mo}} = 0$ (frequency 5 of 15 --- the largest).
* **Median:** $n=15$ (odd), position $(15+1)/2 = 8$. The sorted sequence at position 8 is the **4th `1`**, so $\widehat{\text{Me}} = 1$.
* **Mean:** $$\bar x = \frac{5\cdot 0 + 4\cdot 1 + 3\cdot 2 + 2\cdot 3 + 1\cdot 5}{15} = \frac{0+4+6+6+5}{15} = \frac{21}{15} = 1.40.$$

All three are well-defined here because `Children` lives on an **interval/ratio** scale: the gap from 2 to 3 children is the same as from 0 to 1 child. Note however that **Mo $<$ Me $<\bar x$** --- a tell-tale sign of right-skew, picked up in detail in master **g4b**.

![Master illustration](statistics/images/master/master_g4a_bytype_ai.png)

```r
Children <- c(0,0,0,0,0, 1,1,1,1, 2,2,2, 3,3, 5)
table(Children)                               # 0:5  1:4  2:3  3:2  5:1
as.numeric(names(sort(table(Children), decreasing = TRUE)))[1]   # 0  <- mode
median(Children)                              # 1
mean(Children)                                # 1.4
```

This is the situation of **Ex 1, Q1.4a3** (`Quantity_New` --- discrete count of products jointly purchased, mode = 3, median = 2, mean $\approx 2.44$).

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (e) Continuous numerical --- **all three**, with the mode read off a histogram</summary>

Take `Sales` (monthly turnover, in kEUR) with the $n=10$ observations

$$\{15,\;18,\;19,\;21,\;22,\;23,\;24,\;27,\;30,\;47\}.$$

* **Mean:** $$\bar x = \frac{15+18+19+21+22+23+24+27+30+47}{10} = \frac{246}{10} = 24.6\;\text{kEUR}.$$
* **Median:** $n=10$ (even), median = average of positions 5 and 6: $\tfrac{22+23}{2} = 22.5\;\text{kEUR}.$
* **Mode:** for a *truly continuous* variable every observation is typically unique, so the **raw** mode is not informative. Instead, bin the data and report the **modal class** (the bin with the highest *density*). Using bins of width 5 starting at 15:

| Bin $[a,b)$ | $[15,20)$ | $[20,25)$ | $[25,30)$ | $[30,35)$ | $[35,50)$ |
|---|:---:|:---:|:---:|:---:|:---:|
| $f_i$ | 3 | 4 | 1 | 1 | 1 |

The **modal class** is $[20,25)$ with 4 of 10 observations; its midpoint $22.5$ is a coarse "mode" estimate. Again **Me $<\bar x$** (22.5 vs 24.6) because of the right-tail outlier 47 --- the link to master **g4b**.

```r
Sales <- c(15,18,19,21,22,23,24,27,30,47)
mean(Sales)                                   # 24.6
median(Sales)                                 # 22.5
# Modal class via a histogram:
h <- hist(Sales, breaks = c(15,20,25,30,35,50), plot = FALSE)
h$mids[which.max(h$counts)]                   # 22.5  <- modal-class midpoint
```

This is the situation of **Ex 1, Q1.5e** (`Time` --- modal class by *density*, $[10,20)$ wins over the larger-count $[60,90)$) and **Ex 2, Q2.2b** (modal class identification for grouped hours of device access, $[25,30)$).

</details>

---

### Master take-aways

1. **Pick the measure from the *scale*, not from the question.** The flowchart is mechanical: nominal $\Rightarrow$ mode; ordinal $\Rightarrow$ mode + median; numerical $\Rightarrow$ mode + median + mean.
2. **The mean is forbidden for ordinal data** even though a numeric coding makes it computable: the result depends on the *arbitrary* spacing between levels and so is not a property of the data.
3. **The median requires an order**, which is why it is undefined for nominal data --- it depends on a meaningful "next" value.
4. **For continuous variables the raw mode is not useful**; report the *modal class* from a histogram (highest density bin), as in Ex 2, Q2.2b.
5. The ordering **Mo $\le$ Me $\le\bar x$** (or its reverse) is itself diagnostic: when the three measures disagree on a numerical variable, the distribution is **skewed** --- the bridge to master **g4b**.

---

**Linked snippets:** Ex 1, Q1.1f (`District` --- nominal, mode only); Ex 1, Q1.2b (`Age_recode` --- ordinal, mode + median); Ex 1, Q1.3d (`Country` --- second nominal example); Ex 1, Q1.4a3 (`Quantity_New` --- discrete numerical, all three); Ex 1, Q1.5e (`Time` --- continuous, modal class by density); Ex 2, Q2.2b (modal class identification for grouped data, unequal widths).
""",
    "images": ["statistics/images/master/master_g4a_bytype_ai.png"],
}


master_exercises["g4b_skew"] = {
    "title": "Master Exam — Mean vs median under skewness (consolidated)",
    "content": r"""**Setup.** Across Ex 1 and Ex 2 we repeatedly compute both the **mean** and the **median** of the variable `Sales` (monthly turnover of $n=100$ pizzerie, in €) and observe a *systematic gap*:

$$\widehat{\text{Me}} \;=\; 22\,350\;\text{€},\qquad \bar x \;=\; 23\,947\;\text{€},\qquad \bar x - \widehat{\text{Me}} \;=\; +1\,597\;\text{€}.$$

The mean is **above** the median by about $7\%$. The histogram of `Sales` (see master **g1c_hist**) shows a **long right tail** --- a handful of high-revenue shops up to $80\,000$€ pull the mean upward while leaving the median essentially unchanged. This master pins down exactly *why* this happens, *when* to prefer the median, and how to read the sign of $\bar x - \widehat{\text{Me}}$ as a quick **skewness diagnostic**.

---

### (a) Rule of thumb --- the sign of $\bar x - \widehat{\text{Me}}$ encodes the skew

| Shape of the distribution | Relation | Visual cue |
|---|---|---|
| **Right-skewed** (long tail on the right) | $\bar x > \widehat{\text{Me}}$ | mean is *pulled* into the tail $\Rightarrow$ above the median |
| **Symmetric** (no tail asymmetry) | $\bar x = \widehat{\text{Me}}$ | mean and median coincide |
| **Left-skewed** (long tail on the left) | $\bar x < \widehat{\text{Me}}$ | mean is pulled into the *left* tail $\Rightarrow$ below the median |

For pizzerie `Sales`: $\bar x - \widehat{\text{Me}} = +1\,597 > 0$ $\;\Rightarrow\;$ **right-skewed**, consistent with the histogram. Equivalently, the dimensionless **Pearson skewness coefficient**

$$\text{Sk}_P \;=\; \frac{3(\bar x - \widehat{\text{Me}})}{s} \;=\; \frac{3 \times 1\,597}{s}$$

is positive ($s$ here is the sample standard deviation, around $11\,500$€, giving $\text{Sk}_P \approx 0.42$ --- a *moderate* positive skew).

![Master illustration](statistics/images/master/master_g4b_skew_ai.png)

---

### (b) Why the mean is pulled by the tail --- **sum** vs **rank**

The mean and median answer different optimisation problems, which is the entire reason they react differently to outliers.

**Mean = minimiser of squared distance.** $\bar x$ solves $\;\min_c \sum_{i=1}^n (x_i - c)^2.\;$ Setting the derivative to zero gives $\sum_i(x_i-\bar x)=0$ $\Leftrightarrow$ $\bar x = \tfrac{1}{n}\sum_i x_i$. Every observation enters via its **value**, weighted by *how far it is squared* from $c$. A single observation **arbitrarily large** can therefore move $\bar x$ **arbitrarily far**.

**Median = minimiser of absolute distance.** $\widehat{\text{Me}}$ solves $\;\min_c \sum_i |x_i - c|.\;$ The optimum is the value with as many points to its left as to its right --- a property of the **ranks**, not the values. Moving the largest observation from $80\,000$€ to $800\,000$€ leaves its **rank** unchanged, so the median *does not move*.

**Concrete demonstration on a 9-point toy version of Sales (kEUR):**

$$\{15,\;18,\;20,\;22,\;\mathbf{23},\;25,\;27,\;30,\;35\}.$$

* sorted, $n=9$ $\Rightarrow$ median = position 5 = **23 kEUR**.
* mean $= (15+18+20+22+23+25+27+30+35)/9 = 215/9 \approx \mathbf{23.89}$ kEUR.

Now contaminate the largest value by replacing $35$ with an outlier $\mathbf{200}$ kEUR:

$$\{15,\;18,\;20,\;22,\;\mathbf{23},\;25,\;27,\;30,\;\mathbf{200}\}.$$

* sorted, $n=9$ $\Rightarrow$ median = position 5 = **23 kEUR** *(unchanged)*.
* mean $= (215-35+200)/9 = 380/9 \approx \mathbf{42.22}$ kEUR *(jumped by +18.33)*.

A *single* observation moved the mean by **77%** while leaving the median **untouched** --- the cleanest possible illustration of "sum vs rank".

```r
x  <- c(15,18,20,22,23,25,27,30, 35)
xo <- c(15,18,20,22,23,25,27,30, 200)        # one outlier, last point
mean(x);    median(x)                         # 23.89   23
mean(xo);   median(xo)                        # 42.22   23   <- median unmoved
```

---

### (c) Robustness --- the **breakdown point**

The *breakdown point* of an estimator is the smallest fraction of observations that one must corrupt (replace by $\pm\infty$) to send the estimator to $\pm\infty$.

| Estimator | Breakdown point | Interpretation |
|---|:---:|---|
| **Mean** $\bar x$ | $0$ (formally $1/n \to 0$) | a *single* contaminated observation suffices |
| **Median** $\widehat{\text{Me}}$ | $50\%$ | must corrupt *half* the sample to break it |

The median is the **most robust** of all sensible location estimators --- you cannot do better than $50\%$ breakdown, and the median attains the bound. This is exactly why the toy demonstration above moves the mean but not the median: corrupting $1$ out of $9$ points is $11\%$ contamination, well within the median's tolerance but already infinite for the mean.

---

### (d) Decision rule --- when to report the **median** and when the **mean**

| Situation | Preferred summary | Why |
|---|---|---|
| **Symmetric, light tails** (e.g. `Age` in Ex 2.5f after outlier-cleaning) | **mean** | uses all information; SE is smaller; CLT machinery applies cleanly |
| **Skewed** (left or right --- e.g. `Sales`, household `income`, `house prices`) | **median** | unaffected by tail; truly "typical" |
| **Outliers known to be data errors** | **mean of the cleaned sample** | restore efficiency once contamination is removed |
| **Outliers are genuine but extreme** (e.g. one $80\,000$€ pizzeria) | **median** *and* report both | report the median as the headline; mention the mean to flag the asymmetry |
| **Nominal / ordinal data** | mode / median (see master **g4a**) | mean is undefined |

For pizzerie `Sales`, the right answer to "what does a typical pizzeria earn?" is therefore $\mathbf{22\,350}$€ --- the median --- *not* the $23\,947$€ mean. Reporting the mean alone would systematically **overstate** the typical revenue by $\approx 1\,600$€/month because of a handful of high-revenue shops.

```r
sales <- pizzerie$Sales
mean(sales);     median(sales)                 # 23947   22350
mean(sales) - median(sales)                    # 1597    -> positive -> right skew
3*(mean(sales) - median(sales)) / sd(sales)    # ~ 0.42  -> Pearson skewness
# Robustness check: 5%-trimmed mean is close to the median, far from the raw mean
mean(sales, trim = 0.05)                       # ~ 22900 -> confirms skew, not noise
```

---

### Master take-aways

1. **Read the skew off the sign:** $\bar x > \widehat{\text{Me}}$ $\Rightarrow$ **right** skew; $\bar x < \widehat{\text{Me}}$ $\Rightarrow$ **left** skew; equality $\Rightarrow$ symmetry. For `Sales` the gap is $+1\,597$€ $\Rightarrow$ right-skewed.
2. **Why it happens:** the mean is the minimiser of *squared* distance and so depends on every observation's **value**; the median is the minimiser of *absolute* distance and depends only on **ranks**. The tail moves the value, not the rank.
3. **Robustness in one number:** breakdown point is $0$ for the mean, $50\%$ for the median --- one corrupted observation is enough to ruin the mean, half the sample is required to ruin the median.
4. **Decision rule:** for *skewed* data report the **median**; for symmetric data with light tails report the **mean** (more efficient); always *report both* when they disagree, because their gap *is* a skewness diagnostic.
5. **Applied to pizzerie Sales:** the typical shop earns **22 350 €/month** (median), not 23 947 €/month (mean); the $+1\,597$€ gap *is the right tail*, not noise.

---

**Linked snippets:** Ex 1, Q1.1h (`Sales` mean vs median, full sample); Ex 1, Q1.3f and Q1.3i (subgroup comparison of mean & median); Ex 1, Q1.6a (overall `Sales` central-tendency summary); Ex 2, Q2.5f (`Age` mean vs median --- the *counter*-case where the two essentially agree after cleaning).
""",
    "images": ["statistics/images/master/master_g4b_skew_ai.png"],
}


master_exercises["g4c_grouped"] = {
    "title": "Master Exam — Approximate mean & median from grouped data (consolidated)",
    "content": r"""**Setup.** A municipal survey of $n=88$ **Brescia pizzerie** has been pre-binned into three **unequal-width** classes of *monthly Sales* (€, in thousands). The published table is (cf. Ex 1, Q1.1i for the raw € version)

| Class $[a_i,b_i)$ | Width $w_i$ | Rel. freq. $f_i$ | Cum. rel. freq. $F_i$ | Midpoint $m_i=(a_i+b_i)/2$ |
|---|---:|---:|---:|---:|
| $[0,15)$    | 15 | 0.21 | 0.21 | 7.5  |
| $[15,30)$   | 15 | 0.63 | 0.84 | 22.5 |
| $[30,90)$   | 60 | 0.16 | 1.00 | 60.0 |
| **Total**   |    | **1.00** | | |

We must reconstruct the **mean** and **median** of turnover without access to the raw $n=100$ observations. Two standard approximations are used, both based on a single *strong* assumption that we make explicit at the end.

---

### (a) Approximate mean — midpoint weighting

When the raw data are unavailable, every observation in class $[a_i,b_i)$ is **collapsed to its midpoint** $m_i=(a_i+b_i)/2$. The grouped-data mean is then the weighted average of those midpoints, with weights equal to the relative frequencies:
$$\bar x_g \;=\; \sum_{i=1}^{K} f_i\, m_i.$$

Plug in:
$$\bar x_g \;=\; 0.21\cdot 7.5 \;+\; 0.63\cdot 22.5 \;+\; 0.16\cdot 60.0
\;=\; 1.575 \;+\; 14.175 \;+\; 9.600 \;=\; \mathbf{25.35}\ \text{(k€/month)}.$$

**Reading.** The "average" Brescia pizzeria takes in $\approx 25\,350$ € per month (matches Ex 1.1i to the euro). Note how the open-ended-feeling third class $[30,90)$ — *only* $16\%$ of shops, but $60$-wide — single-handedly pulls the mean up by $9.6$ units: that is the lever a wide right-tail class exerts on the midpoint-weighted mean.

```r
# Approximate mean from grouped data
a   <- c( 0, 15, 30); b <- c(15, 30, 90)
f   <- c(0.21, 0.63, 0.16)               # relative frequencies
mid <- (a + b)/2                          # 7.5, 22.5, 60
xbar_g <- sum(f * mid);  xbar_g           # 25.35
```

---

### (b) Approximate median — linear interpolation inside the median class

**Step 1 — locate the median class.** The median is the value at cumulative frequency $0.5$. From the $F_i$ column:
$$F_1 = 0.21,\qquad F_2 = 0.84,\qquad F_3 = 1.00.$$
Since $F_1=0.21<0.5\le F_2=0.84$, the median lies in the **second** class $[15,30)$.

**Step 2 — linear interpolation.** Within the median class we **assume the observations are uniformly distributed** (the "uniform-on-interval" assumption — see (c)). Under that assumption the cumulative-frequency curve is a straight line on $[a_M,b_M)$, going from height $F_{M-1}$ at $a_M$ to height $F_M$ at $b_M$. Solving for the abscissa at which the line hits $0.5$:
$$\widetilde{\mathrm{med}} \;=\; a_M \;+\; w_M\cdot\frac{0.5 - F_{M-1}}{f_M}.$$

Here $a_M=15$, $w_M=15$, $F_{M-1}=F_1=0.21$, $f_M=0.63$:
$$\widetilde{\mathrm{med}} \;=\; 15 \;+\; 15\cdot\frac{0.5 - 0.21}{0.63}
\;=\; 15 \;+\; 15\cdot\frac{0.29}{0.63}
\;=\; 15 \;+\; 15\cdot 0.4603
\;=\; 15 \;+\; 6.905 \;\approx\; \mathbf{21.91}\ \text{(k€/month)}.$$

**Reading.** Half the pizzerie take in $\le 21\,905$ € per month; the other half take in more (matches Ex 1.1i: $p_{50} \approx 21\,904.76$ €). Note that $\widetilde{\mathrm{med}}\approx 21.9 < \bar x_g \approx 25.3$ — a textbook **right-skew signature**, driven by the heavy, wide third class.

```r
# Approximate median by linear interpolation inside the median class
F    <- cumsum(f);  F                     # 0.21, 0.84, 1.00
M    <- which(F >= 0.5)[1];  M            # 2 -> median class is [15,30)
med_g <- a[M] + (b[M]-a[M]) * (0.5 - c(0,F)[M]) / f[M]
med_g                                     # ~ 21.905
```

![Master illustration](statistics/images/master/master_g4c_grouped_ai.png)

---

### (c) Why these are *approximations* — the uniform-on-interval assumption

Both formulae above rely on the same hidden hypothesis:
> **Within each class $[a_i,b_i)$, the original observations are uniformly distributed.**

Two immediate consequences of that hypothesis:

1. **Mean.** If $X\mid X\in[a_i,b_i)$ is uniform on $[a_i,b_i)$, then $E[X\mid X\in[a_i,b_i)] = (a_i+b_i)/2 = m_i$. The conditional mean of each class equals the midpoint, which is exactly what $\bar x_g=\sum f_i m_i$ uses. Without uniformity, the true class-conditional mean can sit anywhere in the interval — the midpoint is then **biased**.
2. **Median.** Uniformity makes the within-class CDF a straight line, which is precisely the geometric content of the linear-interpolation formula. Any other within-class shape (e.g. skewed, bimodal) would bend that line and shift the median.

**When the approximation is acceptable.**

* Class widths are *narrow* relative to the spread of the data (so even a non-uniform within-class shape cannot move the midpoint or the interpolated quantile far).
* Class widths are *equal* (or nearly so) — then any systematic within-class bias tends to cancel across classes.
* The variable is roughly *unimodal* and *smooth* on each class.

**When it breaks down.**

* The Brescia data have a **wide last class** $[30,90)$, width $60$ vs $15$ for the others. The midpoint $m_3=60$ is essentially arbitrary — the true mean of that class could be $35$ (most shops just over $30$) or $80$ (most shops near the upper bound). Either alternative would shift $\bar x_g$ by several units. **Wide classes are the main source of grouped-data bias.**
* An **open-ended** top class (e.g. "$30$ or more") has *no* midpoint at all; one must either invent an upper bound or abandon the mean and report the median instead — a standard reason the **median is preferred for grouped data** with open extremes.
* Strong **within-class skew** (e.g. most observations near one end of the class) biases both the midpoint mean and the linear-interpolation median.

**Practical rule.** Use grouped formulae for back-of-the-envelope summaries, but cite *the table* — not the recomputed mean — as the authoritative description. When raw data exist, always recompute on the raw data.

```r
# A back-check: simulate uniform-within-class data and compare
set.seed(1)
sim <- unlist(lapply(seq_along(f), function(i)
        runif(round(100*f[i]), a[i], b[i])))
mean(sim);  median(sim)                   # should be ~ 25.35 and ~ 21.9
```

---

### Summary

| Quantity | Formula | Brescia value | Interpretation |
|---|---|---:|---|
| Approx. mean   | $\bar x_g=\sum f_i m_i$ | $25.35$ | Average monthly turnover (k€) |
| Approx. median | $a_M + w_M(0.5-F_{M-1})/f_M$ | $21.91$ | Half the shops are below |
| Mean $>$ median | gap $\approx 3.4$ | --- | **Right-skew** signature |

**Master take-aways.**

1. **Mean from grouped data** = weighted average of class **midpoints**, weights = relative frequencies — exact only if observations are uniformly distributed within each class.
2. **Median from grouped data** = **linear interpolation** inside the class where the cumulative relative frequency crosses $0.5$ — again exact only under within-class uniformity.
3. **Both formulae are approximations.** Their accuracy degrades with **wide** classes, **open** extremes, and **strong within-class skew**.
4. **Mean vs median gap reads as skewness** even in grouped form: $\bar x_g > \widetilde{\mathrm{med}}$ here $\Rightarrow$ right-skew, driven by the wide $[30,90)$ class.
5. **Default reporting rule.** With open or very wide extreme classes, **report the grouped median**, not the grouped mean — the median is far more robust to the within-class uniformity assumption at the tails.

---

**Linked snippets:** Ex 1, Q1.1i (grouped-data mean from frequency table); Ex 1, Q1.5f (median by interpolation, equal-width classes); Ex 1, Q1.6b (mean & median from a frequency distribution); Theory G4c (approximate mean & median, grouped data).
""",
    "images": ["statistics/images/master/master_g4c_grouped_ai.png"],
}


master_exercises["g4d_compare"] = {
    "title": "Master Exam — Cross-subgroup comparison (consolidated)",
    "content": r"""**Setup.** Two running cross-subgroup cases from Ex 1 anchor this master:

* **Case A --- `Time` ≤30 min vs >30 min** (Ex 1.5h, $n=1800$ customers, grouped data). Splitting customers by visit length yields two near-disjoint behavioural profiles.
* **Case B --- `Quantity` 2015-16 vs 2022-23** (Ex 1.4b, discrete count of jointly purchased products). Splitting by *period* tracks how customer habits have shifted over time.

For Case A the per-subgroup approximate summaries (computed in Ex 1.5h) are:

| Subgroup | $n_g$ | $\bar x_g$ (min) | $\widetilde{\mathrm{med}}_g$ (min) |
|---|---:|---:|---:|
| Short visit (≤30) | $836$ | $17.06$ | $17.05$ |
| Long visit (>30)  | $964$ | $79.65$ | $76.06$ |
| **Pooled (all $n=1800$)** | $1800$ | $50.58$ | $41.01$ |

For Case B (Ex 1.4b, mode / median / mean of `Quantity`):

| Period | Mode | $\widetilde{\mathrm{med}}$ | $\bar x$ |
|---|:---:|:---:|---:|
| 2015-16 | 3 | 3 | 2.64 |
| 2022-23 | 3 | 2 | 2.45 |

The question is the *cross-subgroup* one: do the subgroups differ in central tendency, by how much, and how should we interpret the gap?

---

### (a) Compute mean / median per subgroup

**Case A (Time subgroups).** The per-subgroup mean and median are computed by the grouped-data formulas (midpoints / linear interpolation, see master **g4c**):
$$\bar x_A = 17.06,\quad \widetilde{\mathrm{med}}_A = 17.05,\qquad \bar x_B = 79.65,\quad \widetilde{\mathrm{med}}_B = 76.06.$$

The **pooled** mean (ignoring the split) is the weighted average of the subgroup means with weights $n_g/n$:
$$\bar x = \frac{n_A\bar x_A + n_B\bar x_B}{n_A+n_B} = \frac{836\cdot 17.06 + 964\cdot 79.65}{1800} \approx 50.58\ \text{min},$$
matching the overall mean reported in Ex 1.5f. The pooled median ($\approx 41$) is *not* in general the weighted average of subgroup medians --- it must be recomputed on the merged sample.

```r
# Subgroup means & medians from grouped data (Time, Ex 1.5h)
# Group A: Time <= 30
midA <- c(5, 15, 25);  fA <- c(0.146, 0.502, 0.352)
xbarA <- sum(midA*fA);  xbarA                      # 17.06
# Group B: Time > 30
midB <- c(45, 75, 120); fB <- c(0.183, 0.592, 0.225)
xbarB <- sum(midB*fB);  xbarB                      # 79.65
# Pooled mean from subgroup means (weights = n_g)
(836*xbarA + 964*xbarB)/1800                       # ~ 50.58
```

---

### (b) Compare the gap — absolute and relative

**Case A — absolute gap (means).**
$$\Delta\bar x = \bar x_B - \bar x_A = 79.65 - 17.06 = \mathbf{62.59}\ \text{min}.$$

**Absolute gap (medians).** $\Delta\widetilde{\mathrm{med}} = 76.06 - 17.05 = 59.01$ min — same direction and order of magnitude as the mean gap. The two subgroups occupy *almost disjoint* regions of the support, so the gap is a *huge location shift*, not a tail artefact.

**Relative gap** (against the pooled mean):
$$\frac{\Delta\bar x}{\bar x} = \frac{62.59}{50.58} \approx 124\%.$$
A relative gap above 100% confirms that "average" and "subgroup average" are essentially unrelated --- this is the signature of a **mixture distribution** (the two profiles flagged in Ex 1.5g as "short visit ~10-30 min" vs "long visit ~60-90 min"). Ex 1.5h's punchline is that the *subgroup* mean and median fall in the corresponding *modal class*, whereas the *pooled* mean (50.58) and median (41.01) fall in low-density classes --- a textbook reason why pooling is misleading here.

**Case B — Quantity periods.** The mean fell from $2.64\to 2.45$ (a $0.19$-unit drop, $\approx 7\%$ relative), the median from $3\to 2$. Mode is $3$ in both periods (the source's reported "Mode 3 / 2" is flagged in Ex 1.4b as a likely typo --- recomputation gives $3$ in 2022-23 too, since $34.0\% > 30.3\%$). The shift toward bundles of $1$-$2$ items (combined share $52.6\%$ in 2022-23 vs $38.6\%$ in 2015-16) is a genuine, *non-tail* re-weighting.

```r
# Case A: Time-subgroup gap (Ex 1.5h)
xbarA <- 17.06;  xbarB <- 79.65
xbarA - xbarB                                  # -62.59  (B is 62.6 min higher)
(xbarB - xbarA) / ((836*xbarA + 964*xbarB)/1800)   # ~ 1.24  -> 124% rel. gap

# Case B: Quantity period gap (Ex 1.4b)
xbar_1516 <- 2.64;  xbar_2223 <- 2.45
xbar_1516 - xbar_2223                          # 0.19  -> small shift, but mode 3 in both
```

![Master illustration](statistics/images/master/master_g4d_compare_ai.png)

---

### (c) Interpret — which subgroup has the higher central tendency?

**Case A.** Both summaries point the same way:
$$\bar x_B = 79.65 > 17.06 = \bar x_A,\qquad \widetilde{\mathrm{med}}_B = 76.06 > 17.05 = \widetilde{\mathrm{med}}_A.$$
"Long-visit" customers stay $\approx 60$ min longer than short-visit customers, and the coincidence of mean and median gaps in each subgroup ($\bar x_g \approx \widetilde{\mathrm{med}}_g$) tells us each subgroup is **locally roughly symmetric** --- the right-skewness of the *pooled* distribution disappears once we condition on visit type. This is the master-level reason why **subgroup analysis recovers symmetry that pooled analysis hides**.

**Case B.** From 2015-16 to 2022-23, *all three* central-tendency measures move down (mean $2.64\to 2.45$, median $3\to 2$, modal share of value 1+2 grows from $38.6\%$ to $52.6\%$). The shift is small in mean but large in *distribution shape*: 2022-23 has a bimodal-looking pattern with mass at $1,2,3$, while 2015-16 was concentrated at $3$. A 0.19-unit mean gap masks the much larger compositional change visible in the frequency table.

**Standardised gap (Cohen's $d$).** When subgroup SDs are reported, the gap can be normalised:
$$d \;=\; \frac{\bar x_g - \bar x_{g'}}{s_p},\qquad s_p = \sqrt{\tfrac{(n_g-1)s_g^2+(n_{g'}-1)s_{g'}^2}{n_g+n_{g'}-2}}.$$
Benchmarks (Cohen): $|d|\approx 0.2$ small, $0.5$ medium, $0.8$ large. For Case A the SDs are not in the Ex 1.5h summary, but the *raw* gap of $62.59$ min against any plausible within-class SD ($\lesssim 30$ min) yields $|d|\gtrsim 2$ --- an enormous standardised effect, consistent with the disjoint-supports picture.

**Caveats before any causal reading.**

* The gap is *descriptive*; the survey is not randomised, so unobserved confounders (e.g. day of week, customer demographics) may drive the visit-length split.
* For the period comparison (Case B), 2015-16 and 2022-23 differ on *many* dimensions beyond `Quantity` (price levels, promotional regime, COVID aftermath) --- the change cannot be attributed to any single cause.

---

### (d) Simpson's paradox warning — when pooling can lie

Whenever we move from *subgroup* summaries to a *pooled* summary, two things must be checked.

**(i) The pooled mean is a weighted average of subgroup means.**
$$\bar x = \sum_g \tfrac{n_g}{n}\,\bar x_g.$$
If the subgroup sizes $n_g$ are very unequal, the pooled mean is dragged toward the larger group's mean. In Case A, $n_A=836$, $n_B=964$ are *slightly imbalanced* but close to equal; the pooled mean $50.58$ lies between $17.06$ and $79.65$ but closer to the *long-visit* group, reflecting the larger $n_B$.

**(ii) Simpson's paradox.** A direction of association at the *pooled* level can be **reversed** at the *subgroup* level, if a confounder is unevenly distributed across the third variable. Skeleton: suppose `Time` is also broken down by `Day` (weekday vs weekend), and
$$\bar x_{A,\text{weekday}} < \bar x_{B,\text{weekday}},\qquad \bar x_{A,\text{weekend}} < \bar x_{B,\text{weekend}},$$
yet a careless period comparison (Case B style) finds
$$\bar x_{2015\text{-}16,\text{pooled}} > \bar x_{2022\text{-}23,\text{pooled}}.$$
Pooled mean comparisons can hide a confounder (e.g. the weekend share of the sample differs between periods). The paradox arises because the third variable (`Day`) has different baseline `Time` levels *and* different shares across periods.

**Practical guard.**

* Whenever you report a pooled gap $\bar x_g-\bar x_{g'}$, also check the **stratified** gaps within plausible confounders (day, age, region).
* If stratified gaps go the *opposite* way, **do not aggregate** --- report the stratified picture.
* If stratified gaps point the same way, the pooled gap is a fair summary.

In the present cases, no further stratification is available in the printed tables, so the gaps stand as *marginal* comparisons. Treat them as descriptions of the dataset, not as causal statements.

```r
# Simpson check (skeleton)
# tapply(Time, list(VisitType, Day), mean)   # stratified gap
# tapply(Time, VisitType, mean)              # pooled gap
# If within-day gaps reverse the pooled gap, suspect Simpson.
```

---

### Summary

**Case A --- Time subgroups (Ex 1.5h).**

| Quantity | Value | Reading |
|---|---:|---|
| $\bar x_A,\ \bar x_B$            | $17.06,\ 79.65$   | Long-visit group $\approx 63$ min higher |
| $\widetilde{\mathrm{med}}_A,\ \widetilde{\mathrm{med}}_B$ | $17.05,\ 76.06$ | Same direction (location shift, not tail) |
| Relative gap                     | $\approx 124\%$   | Huge --- mixture distribution signature |
| Pooled mean / median             | $50.58,\ 41.01$   | Falls in low-density classes (Ex 1.5g) |

**Case B --- Quantity periods (Ex 1.4b).**

| Quantity | 2015-16 | 2022-23 | Reading |
|---|---:|---:|---|
| Mode   | $3$    | $3$    | Same (source typo flagged) |
| Median | $3$    | $2$    | Down by one unit |
| Mean   | $2.64$ | $2.45$ | Down $\approx 7\%$ |

**Master take-aways.**

1. **Cross-subgroup comparison = same descriptive recipe applied twice**, then compare $\bar x_g$, $\widetilde{\mathrm{med}}_g$, and dispersion side by side; the *gap* is the headline number.
2. **Report both absolute and standardised gaps.** Raw gap is interpretable in original units; Cohen's $d$ makes it comparable across studies and contexts.
3. **Mean gap $\approx$ median gap** signals a *location shift* of the whole distribution; a divergence between the two signals a *tail-driven* effect.
4. **Pooled mean** = $\sum_g (n_g/n)\bar x_g$ — biased toward the larger subgroup when group sizes are unequal; equal $n_g$ removes that distortion.
5. **Simpson's paradox.** A pooled gap can *reverse* when an unmeasured stratifier is introduced. Always cross-tabulate against plausible confounders before claiming the marginal gap reflects a real subgroup difference.

---

**Linked snippets:** Ex 1, Q1.4b (`Quantity` across periods 2015-16 vs 2022-23 --- Case B above); Ex 1, Q1.5h (`Time` ≤30 vs >30 subgroups --- Case A above, grouped-data summaries per subgroup); Theory G4d (cross-subgroup / period comparison).
""",
    "images": ["statistics/images/master/master_g4d_compare_ai.png"],
}


master_exercises["g1e_cum"] = {
    "title": "Master Exam — Cumulative plots: ECDF + ogive (consolidated)",
    "content": r"""**Setup.** Two complementary cumulative plots arise depending on the **type of variable**:

* **Discrete numerical** $\Rightarrow$ **ECDF** (empirical cumulative distribution function): a *right-continuous step* function that jumps by $f_i/n$ at every observed value.
* **Continuous numerical, pre-binned into classes** $\Rightarrow$ **ogive**: a piecewise-linear curve connecting the cumulative relative frequencies at the **upper endpoints** of each class.

We use two running examples from the course datasets:

1. **Discrete — `DS$Children`** (number of children per customer, $n=750$, values $0,1,2,3$):

| $x_i$ | $f_i$ | $\hat p_i = f_i/n$ | $F(x_i)=\sum_{j\le i} \hat p_j$ |
|---:|---:|---:|---:|
| $0$ | $360$ | $0.480$ | $0.480$ |
| $1$ | $184$ | $0.245$ | $0.725$ |
| $2$ | $111$ | $0.148$ | $0.873$ |
| $3$ | $\;\,95$ | $0.127$ | $1.000$ |

2. **Continuous (grouped) — Insurance "Nr contracts"** ($n=2000$, the dataset from Ex 2.4):

| Class $[a_i, b_i)$ | $f_i$ | $\hat p_i$ | Cumul. $F(b_i)$ |
|---|---:|---:|---:|
| $[0,10)$    | $200$ | $0.10$ | $0.10$ |
| $[10,20)$   | $200$ | $0.10$ | $0.20$ |
| $[20,30)$   | $400$ | $0.20$ | $0.40$ |
| $[30,60)$   | $500$ | $0.25$ | $0.65$ |
| $[60,90)$   | $500$ | $0.25$ | $0.90$ |
| $[90,150]$  | $200$ | $0.10$ | $1.00$ |

---

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) The common idea: $F(x) = \sum_{x_i \le x} \hat p_i$</summary>

In **both** cases the cumulative function is the *running sum* of the relative frequencies:
$$F(x) \;=\; \mathbb{P}(X \le x) \;\approx\; \frac{1}{n}\sum_{i=1}^{n} \mathbf{1}\{X_i \le x\}.$$
It is **monotone non-decreasing**, starts at $0$ (left of the support) and ends at $1$ (right of the support). The *shape* — step vs. continuous-piecewise-linear — depends on what we *assume about the data between observed values*.

* For **discrete** data: between two consecutive observed integers there is, by assumption, *no probability mass at all*. So $F$ stays flat until the next jump $\Rightarrow$ **step function**.
* For **continuous (grouped)** data: probability mass is *spread* across the class width. The simplest assumption is **uniform-within-class** $\Rightarrow$ $F$ grows *linearly* across each class $\Rightarrow$ **piecewise-linear ogive**.
</details>


---

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (b) Discrete case — ECDF (right-continuous step)</summary>

For `Children` define
$$F(x) \;=\; \frac{1}{n}\sum_{i=1}^{n}\mathbf{1}\{X_i \le x\}, \qquad x \in \mathbb{R}.$$
This is **right-continuous**: at each observed integer $x_i$ the function *jumps up* to the cumulative proportion $F(x_i)$ and then *stays at that level* until the next observed value. Concretely,

| $x$ | value of $F(x)$ |
|---:|---:|
| $x < 0$       | $0$ |
| $0 \le x < 1$ | $0.480$ |
| $1 \le x < 2$ | $0.725$ |
| $2 \le x < 3$ | $0.873$ |
| $x \ge 3$     | $1.000$ |

**Reading the ECDF.**
* The **height of each jump** equals the relative frequency $\hat p_i$ of the corresponding value (here jumps of $0.480, 0.245, 0.148, 0.127$ at $x=0,1,2,3$).
* The **modal value** is the one with the *largest jump* ($x=0$, jump $=0.48$).
* The **median** is the smallest $x$ such that $F(x) \ge 0.5$: here $F(0)=0.48<0.5$ and $F(1)=0.725\ge 0.5$ $\Rightarrow$ $\widetilde{x}=1$.
* **Any quantile** $q_\alpha$ is found by drawing the horizontal line $y=\alpha$ and reading off the *first* $x$ where the step reaches that height.

```r
# Discrete case: ECDF for Children
F.hat <- ecdf(DS$Children)                 # the ECDF object
F.hat(0); F.hat(1); F.hat(2); F.hat(3)     # 0.480 0.725 0.873 1.000
plot(F.hat, verticals = FALSE, do.points = TRUE,
     xlab = "Children", ylab = "F(x)",
     main = "ECDF of Children")            # right-continuous step plot
abline(h = 0.5, lty = 2)                   # median guide line -> hits at x = 1
quantile(DS$Children, 0.5, type = 1)       # 1  (discrete quantile)
distr.plot.x(Children, plot.type = "cumulative", data = DS)   # course-package equiv.
```
</details>


---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (c) Continuous (grouped) case — ogive (linear interpolation within class)</summary>

For "Nr contracts" we **do not** know individual values; we know only the class counts. Under the **uniform-on-interval** assumption, mass is spread uniformly across each class width, so $F$ grows *linearly* from the cumulative proportion at the lower endpoint $a_i$ to the cumulative proportion at the upper endpoint $b_i$.

**Construction (4 steps):**

1. Compute cumulative relative frequencies $F(b_i)$ at each upper endpoint (last column of the setup table).
2. Anchor the curve: $F(a_1) = 0$ (here $F(0) = 0$).
3. Plot the points $(b_i, F(b_i))$ for every class.
4. **Connect consecutive points with straight lines** $\Rightarrow$ the ogive.

The slope on class $[a_i, b_i)$ is
$$\text{slope}_i \;=\; \frac{F(b_i) - F(a_i)}{b_i - a_i} \;=\; \frac{\hat p_i}{w_i} \;=\; h_i,$$
i.e. the **histogram density** of that class. Wide classes have *shallow* slopes; narrow classes with the same mass have *steep* slopes.

**Reading off a quantile (linear interpolation).** To find the value $x_\alpha$ such that $F(x_\alpha)=\alpha$:
* Locate the class $[a_i, b_i)$ where the line $y=\alpha$ crosses the ogive (i.e. $F(a_i) < \alpha \le F(b_i)$).
* Solve the linear interpolation formula:
$$x_\alpha \;=\; a_i \;+\; \frac{\alpha - F(a_i)}{F(b_i)-F(a_i)} \,\cdot\, (b_i - a_i).$$

**Example — median ($\alpha=0.5$) of Nr contracts.** The crossing class is $[30,60)$ (since $F(30)=0.40 < 0.5 \le 0.65 = F(60)$):
$$\widetilde{x} \;=\; 30 \;+\; \frac{0.50 - 0.40}{0.65 - 0.40}\,(60-30) \;=\; 30 + \tfrac{0.10}{0.25}\cdot 30 \;=\; 30 + 12 \;=\; 42.$$
So **half** of consultants stipulate **at most 42** contracts.

**Example — 90th percentile.** $F(90)=0.90$ *exactly* at the endpoint $\Rightarrow$ no interpolation needed: $q_{0.90}=90$ (this is the "minimum number of contracts for the top consultants" in Ex 2.4c).

```r
# Continuous (grouped): ogive for Nr contracts
endpoints <- c(0, 10, 20, 30, 60, 90, 150)
cumprop   <- c(0, 0.10, 0.20, 0.40, 0.65, 0.90, 1.00)
plot(endpoints, cumprop, type = "b", pch = 19,
     xlab = "Nr contracts", ylab = "F(x)",
     main = "Ogive of Nr contracts")
abline(h = 0.5, lty = 2)

# Read off ANY quantile by linear interpolation on the ogive:
approx(cumprop, endpoints, xout = 0.50)$y  # 42   (median)
approx(cumprop, endpoints, xout = 0.25)$y  # 22.5 (Q1)
approx(cumprop, endpoints, xout = 0.75)$y  # 72   (Q3) -- matches Ex 2.4b
approx(cumprop, endpoints, xout = 0.90)$y  # 90   (P90) -- matches Ex 2.4c
```

![Master illustration](statistics/images/master/master_g1e_cum_ai.png)
</details>


---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (d) Median read-off at $F=0.5$ — the unified recipe</summary>

| Variable type | What you draw | How you read the median |
|---|---|---|
| Discrete (ECDF) | Step function with jumps $\hat p_i$ at observed values | **Smallest observed value** $x$ with $F(x)\ge 0.5$ — *no interpolation* (median is one of the observed values) |
| Continuous (ogive) | Piecewise-linear curve through $(b_i, F(b_i))$ | **Linear interpolation** within the class where the curve crosses $0.5$ |

The *visual* recipe is identical: trace the horizontal line $y=0.5$ until it hits the curve, drop a vertical line to the $x$-axis, read off the value. The *arithmetic* differs only in what "the curve" is — a step or a slanted line.
</details>


---

<details class="master-subpart">
<summary>(e) Ogive vs ECDF — when each is appropriate</summary>

| Feature | ECDF (discrete) | Ogive (continuous, grouped) |
|---|---|---|
| Shape | Right-continuous *step* | Piecewise-linear, *continuous* |
| Jumps at | Each observed value | None (continuous) |
| Slope = | $0$ between values, $\infty$ at jumps | $h_i = \hat p_i / w_i$ on each class (= histogram density) |
| Quantile reading | Pick the value where $F$ first reaches $\alpha$ | Linear interpolation within crossing class |
| Underlying assumption | Mass concentrated at observed points | Mass *uniformly spread* across each class |
| Right tool when | Few distinct values (counts, scores, "Children", "Nr_visits") | Many distinct values, pre-binned (income, time, weight) |

A continuous variable with *raw* (un-binned) data of size $n$ admits a *true* ECDF with $n$ tiny jumps of size $1/n$ each — that ECDF is what `ecdf()` produces in R, and it converges (Glivenko--Cantelli) to the underlying $F$ as $n\to\infty$. The **ogive is the natural smoothing** of that staircase when only grouped counts are available.

```r
# If raw continuous data are available (no pre-binning), prefer the true ECDF:
distr.plot.x(Time, plot.type = "cumulative", data = mydata)
```

---

</details>

**Master take-aways.**

1. **Same idea, two shapes.** $F(x)=\sum_{x_i\le x}\hat p_i$ is *monotone non-decreasing* from $0$ to $1$; it is a **step** for discrete data and a **piecewise-linear ogive** for continuous-grouped data.
2. **Jumps vs slopes.** ECDF jumps equal relative frequencies $\hat p_i$; ogive slopes equal histogram densities $h_i=\hat p_i/w_i$ --- so an ogive *is* the integral of the histogram.
3. **Right-continuity matters.** The ECDF jumps *up* at $x_i$ and *stays at* the higher level (so $F(x_i)=\hat p_1+\dots+\hat p_i$, not $\hat p_1+\dots+\hat p_{i-1}$).
4. **Quantile reading is the universal use case.** Median at $F=0.5$; quartiles at $0.25, 0.75$; percentiles at any $\alpha\in(0,1)$ --- *step lookup* for discrete, *linear interpolation* for grouped.
5. **Choose the right tool.** Few distinct values $\Rightarrow$ ECDF; pre-binned continuous data $\Rightarrow$ ogive; raw continuous data $\Rightarrow$ true ECDF via `ecdf()` (asymptotically equivalent to the ogive).

---

**Linked snippets:** Ex 0.2a3 (ogive of `fare` from class densities --- the construction recipe with cumulated proportions); Ex 0.2b3 (step diagram for `size.family` --- discrete case, no interpolation); Ex 1.2c (`Children` spike plot --- the same discrete variable whose ECDF we build here); Ex 1.4a2 (cumulative percentage table and step plot for discrete `Quantity_New` with a *gap* at value $5$); Ex 1.5d (ogive of `Time`, $n=1800$, median estimation via linear interpolation inside the median class); Ex 2.4a (identify the ogive); Ex 2.7a (ogive of `Nr_visits`, $n=2200$ --- reading increments as relative frequencies).
""",
    "images": ["statistics/images/master/master_g1e_cum_ai.png"],
}


master_exercises["g2a_exact"] = {
    "title": "Master Exam — Exact proportions from raw data (consolidated)",
    "content": r"""**Setup.** From the `customer_habits` dataset ($n=34\,866$ rows, each row a transaction by an identified customer; see Ex 1.3a), focus on the **nominal binary** variable `Sex` with two levels F (female) and M (male). The frequency table is

| Sex | Count $n_i$ | $\hat p_i = n_i/n$ |
|---|---:|---:|
| F | $15\,235$ | $0.4370$ |
| M | $19\,631$ | $0.5630$ |
| **Total** | $\mathbf{34\,866}$ | $\mathbf{1.0000}$ |

Because the **raw data vector** `customer_habits$Sex` is available (not only the grouped table), every proportion we want can be computed **exactly** --- no uniform-on-interval approximation is needed.

---

<details class="master-subpart" open>
<summary><span class="tag tag-4plus">≥4 ex</span> (a) Exact proportion from a raw vector: `mean(condition)`</summary>

The fundamental identity is
$$\hat p \;=\; \frac{\#\{i : \text{condition is true}\}}{n} \;=\; \frac{1}{n}\sum_{i=1}^{n}\mathbf{1}\{\text{condition}(X_i)\}.$$
In R, a logical comparison applied to a vector produces a **logical vector** of `TRUE`/`FALSE` values; coerced to numeric, `TRUE = 1` and `FALSE = 0`. Therefore
$$\boxed{\;\hat p \;=\; \texttt{mean(condition)}\;}$$
because `mean()` divides the sum of the 1's (the number of TRUEs) by the length $n$.

For `Sex == "F"`:
$$\hat p_F \;=\; \frac{15\,235}{34\,866} \;=\; 0.43696\ldots \;\approx\; 0.4370.$$
And symmetrically $\hat p_M = 19\,631/34\,866 = 0.5630$.

**Why "exact"?** Because we are *counting* the rows of the raw dataset, not interpolating across class endpoints. The only source of error is sampling (random variation of $\hat p$ around the true population proportion $p$), *not* binning.

```r
# Exact proportion from raw vector
mean(customer_habits$Sex == "F")        # 0.4369586  -> p_hat_F = 0.4370
mean(customer_habits$Sex == "M")        # 0.5630414  -> p_hat_M = 0.5630
```

</details>

<details class="master-subpart">
<summary><span class="tag tag-4plus">≥4 ex</span> (b) From a logical vector: `sum()` vs `mean()` vs `length()`</summary>

Let `cond <- customer_habits$Sex == "F"`. Then:

| Quantity | R expression | Value | Interpretation |
|---|---|---:|---|
| Total sample size | `length(cond)` | $34\,866$ | Denominator $n$ |
| Number of TRUEs (count of F) | `sum(cond)` | $15\,235$ | Numerator $\#$ |
| Proportion of F | `mean(cond)` | $0.4370$ | $\hat p_F = \#/n$ |
| Number of FALSEs (count of M) | `sum(!cond)` | $19\,631$ | $n - \#$ |

The three operations `sum`, `mean`, `length` on a logical vector therefore give **count**, **proportion**, **denominator** --- the three numbers that fully specify a Bernoulli summary.

```r
# Sum / count / length on a logical vector
cond <- customer_habits$Sex == "F"
length(cond)                            # 34866   (denominator n)
sum(cond)                               # 15235   (count of F)
mean(cond)                              # 0.4370  (proportion of F)
sum(!cond)                              # 19631   (count of M)

# Frequency table (same numbers, packaged):
table(customer_habits$Sex)              # F 15235  M 19631
prop.table(table(customer_habits$Sex))  # F 0.4370 M 0.5630
distr.table.x(x = customer_habits$Sex)  # course package equivalent
```

**Composite conditions.** Logical operators `&` (and), `|` (or), `!` (not) combine logicals **element-wise**. For example, the exact proportion of *young female* customers is
```r
mean(customer_habits$Sex == "F" & customer_habits$Age <= 30)   # exact P(F and young)
mean(customer_habits$Sex == "F" | customer_habits$Age >  60)   # exact P(F or old)
```
which counts rows satisfying *both* conditions and divides by $n$. No further formula is needed --- the data and Boolean algebra do all the work.

![Master illustration](statistics/images/master/master_g2a_exact_ai.png)

</details>

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (c) Exact (raw data) vs approximate (grouped data) --- when each applies</summary>

This is the **central methodological point** for the entire G2 family of subtopics:

| Situation | Method | Why |
|---|---|---|
| **Raw data vector available** (continuous *or* discrete, *or* categorical) | **Exact** --- `mean(condition)` | Each row contributes a $0$ or $1$ to the average; no assumption about the distribution between observed values. |
| **Only a grouped frequency table** (continuous binned into classes; *no* raw values) | **Approximate** --- uniform-on-interval | We do not know where, within each class, the mass actually sits; the simplest assumption is uniform spread $\Rightarrow$ linear interpolation on the ogive (see master `g2b_approx`). |

**Concretely:**

* `customer_habits$Age` (raw vector, $34\,866$ values) $\Rightarrow$ `mean(Age >= 20 & Age <= 40)` is **exact** (Ex 2.5c).
* The Insurance "Nr contracts" dataset of Ex 2.4 is given only as the cumulative-frequency *table* (no individual consultant rows) $\Rightarrow$ $P(20 \le X < 40)$ must be obtained by linear interpolation on the ogive --- *approximate*, because we are assuming uniform spread within each class.

For the **binary** variable `Sex` the question of approximation does not even arise: the variable is intrinsically discrete with $K=2$ levels, so `mean(Sex == "F")` is *literally* the empirical probability --- no other choice possible.

**Standard error of the exact proportion.** Once $\hat p$ is computed exactly from $n$ raw observations, its sampling uncertainty is
$$\widehat{\text{SE}}(\hat p) \;=\; \sqrt{\frac{\hat p (1-\hat p)}{n}}.$$
For $\hat p_F = 0.4370$ at $n=34\,866$: $\widehat{\text{SE}} = \sqrt{0.4370\cdot 0.5630/34866}\approx 0.00266$ $\Rightarrow$ a 95% Wald CI of roughly $0.4370 \pm 1.96\cdot 0.00266 = [0.4318,\, 0.4422]$. *Exactness* refers to the **point estimate**; the CI accounts for sampling error, which is unavoidable.

```r
# Exact (raw data):
mean(customer_habits$Age >= 20 & customer_habits$Age <= 40)    # Ex 2.5c style

# Approximate (only grouped table available -- see master g2b_approx):
# uniform-on-interval -> linear interpolation on the ogive
endpoints <- c(0, 10, 20, 30, 60, 90, 150)
cumprop   <- c(0, 0.10, 0.20, 0.40, 0.65, 0.90, 1.00)
diff(approx(endpoints, cumprop, xout = c(20, 40))$y)           # P(20 <= X < 40)

# Standard error and Wald CI for the exact proportion:
n     <- nrow(customer_habits)
phat  <- mean(customer_habits$Sex == "F")
se    <- sqrt(phat * (1 - phat) / n)
phat + c(-1, 1) * qnorm(0.975) * se                            # 95% Wald CI
prop.test(sum(customer_habits$Sex == "F"), n)                  # CI + test, one line
```

</details>

<details class="master-subpart">
<summary>(d) Worked numbers for `Sex` in `customer_habits`</summary>

| Question | Logical expression | Numerator | Denominator | Exact $\hat p$ |
|---|---|---:|---:|---:|
| Proportion of females | `Sex == "F"` | $15\,235$ | $34\,866$ | $0.4370$ |
| Proportion of males | `Sex == "M"` | $19\,631$ | $34\,866$ | $0.5630$ |
| Sex ratio F:M | $n_F / n_M$ | --- | --- | $15\,235/19\,631 = 0.7761$ |
| Female majority? (check) | $\hat p_F > 0.5$? | --- | --- | **No** ($0.437<0.5$) |
| % difference vs balance | $\hat p_F - 0.5$ | --- | --- | $-0.063$ (i.e. $6.3$ pp below parity) |

The sample is **slightly male-skewed** --- a $56.3\%$ vs $43.7\%$ split --- but well within the range usually treated as "approximately balanced" for downstream analyses.

</details>

---

**Master take-aways.**

1. **`mean(condition)` is the universal exact-proportion formula** in R: it works for *any* condition on *any* raw vector (categorical, discrete, continuous) and equals $\#TRUE / n$ by construction.
2. **Three operations on a logical vector cover everything**: `length` = $n$, `sum` = count of TRUEs, `mean` = proportion of TRUEs. Memorise this triple --- it replaces every textbook proportion formula.
3. **Exact vs approximate is decided by the data, not the variable type.** If raw observations are available $\Rightarrow$ exact via `mean()`; if only grouped frequencies $\Rightarrow$ approximate via uniform-on-interval (linear interpolation on the ogive).
4. **Composite events become Boolean expressions** with `&`, `|`, `!`. No new formula is needed --- set theory and R's element-wise logic do the bookkeeping.
5. **For `Sex` in `customer_habits`:** $\hat p_F = 15\,235/34\,866 = 0.4370$ and $\hat p_M = 0.5630$ --- *exact* point estimates with $\widehat{\text{SE}}\approx 0.0027$, so the 95% CI for $p_F$ is roughly $[0.432, 0.442]$, comfortably below $0.5$.

---

**Linked snippets:** Ex 1.3b (`Sex` frequency table for `customer_habits` --- the source of the numbers used throughout this master); Ex 2.5c (exact proportion of customers with $20\le \text{Age}\le 40$ via `mean()`); Ex 2.6b (exact proportion of products sold below cost via a Boolean condition on raw `Unit_Price`/`Unit_Cost`); master `g2b_approx` (the *approximate* counterpart, used when only grouped data are available).
""",
    "images": ["statistics/images/master/master_g2a_exact_ai.png"],
}


master_exercises["g7_twoway"] = {
    "title": "Master Exam — Two-way tables (consolidated)",
    "content": r"""**Setup.** From the dataset `DS` we cross-tabulate `Sex` (rows: $K_R = 4$ categories — `F`, `M`, `NB`, `NA`) against `History` (columns: $K_C = 2$ categories — `Yes` = has prior purchase history, `No` = first-time customer) for $n = 500$ customers. The **joint absolute-frequency table** $n_{ij}$ is

\begin{tabular}{p{7cm}|p{7cm}|p{7cm}|p{7cm}|p{10cm}}
\textbf{Sex \textbackslash{} History} & \textbf{Yes} & \textbf{No} & \textbf{Row total $n_{i\cdot}$} & \\
F   & 140 & 70  & 210 & \\
M   & 130 & 100 & 230 & \\
NB  & 25  & 15  & 40  & \\
NA  & 10  & 10  & 20  & \\
\textbf{Col total $n_{\cdot j}$} & \textbf{305} & \textbf{195} & \textbf{500} & \\
\end{tabular}

So $n_{\cdot\cdot} = n = 500$, with $\sum_i n_{i\cdot} = 210 + 230 + 40 + 20 = 500$ and $\sum_j n_{\cdot j} = 305 + 195 = 500$ (consistency check passes).

```r
tbl <- matrix(c(140, 70, 130, 100, 25, 15, 10, 10), nrow = 4, byrow = TRUE,
              dimnames = list(Sex = c("F","M","NB","NA"), History = c("Yes","No")))
tbl                                       # joint absolute frequencies n_ij
addmargins(tbl)                           # row + column totals appended
sum(tbl)                                  # n = 500
```

---

**(a) Joint absolute frequencies $n_{ij}$.** The entries above *are* the joint absolute frequencies: $n_{ij}$ counts customers simultaneously in row category $i$ and column category $j$. They satisfy $n_{ij} \ge 0$ and $\sum_{i,j} n_{ij} = n$. Each cell answers the question "how many customers are *both* of sex $i$ *and* history $j$?" — e.g.\ $n_{F,\text{Yes}} = 140$ means $140$ customers are female *and* repeat purchasers.

---

**(b) Marginal distributions.** Sum across one index to collapse the joint table to a univariate distribution.

* **Row marginal** $n_{i\cdot} = \sum_j n_{ij}$: this is the **distribution of `Sex` ignoring `History`** — $(210, 230, 40, 20)$ for $(F, M, NB, NA)$. Proportions: $(0.420, 0.460, 0.080, 0.040)$.
* **Column marginal** $n_{\cdot j} = \sum_i n_{ij}$: distribution of `History` ignoring `Sex` — $(305, 195)$ for (Yes, No). Proportions: $(0.610, 0.390)$.

So $61\%$ of all customers are repeat buyers, and the modal sex is `M` ($46\%$).

```r
rowSums(tbl)                              # n_{i.} = 210 230 40 20
colSums(tbl)                              # n_{.j} = 305 195
```

---

**(c) Joint proportions $p_{ij} = n_{ij}/n$.** Divide every joint cell by $n = 500$:

\begin{tabular}{p{7cm}|p{7cm}|p{7cm}|p{7cm}|p{10cm}}
\textbf{Sex \textbackslash{} History} & \textbf{Yes} & \textbf{No} & \textbf{Row $p_{i\cdot}$} & \\
F   & 0.280 & 0.140 & 0.420 & \\
M   & 0.260 & 0.200 & 0.460 & \\
NB  & 0.050 & 0.030 & 0.080 & \\
NA  & 0.020 & 0.020 & 0.040 & \\
\textbf{Col $p_{\cdot j}$} & \textbf{0.610} & \textbf{0.390} & \textbf{1.000} & \\
\end{tabular}

These are *empirical joint probabilities* — $p_{F,\text{Yes}} = 0.280$ means $28\%$ of *all* customers are female repeat buyers. They sum to $1$ over the full grid.

```r
prop.table(tbl)                           # divides by n -> p_ij
```

---

**(d) Row-conditional distributions $p(j \mid i) = n_{ij}/n_{i\cdot}$.** "Given a customer is of sex $i$, what is the chance of history $j$?" Divide each cell by *its own row total*:

\begin{tabular}{p{7cm}|p{7cm}|p{7cm}|p{7cm}|p{10cm}}
\textbf{Sex \textbackslash{} History} & \textbf{Yes} & \textbf{No} & \textbf{Sum} & \\
F   & $140/210 = 0.667$ & $70/210 = 0.333$ & 1 & \\
M   & $130/230 = 0.565$ & $100/230 = 0.435$ & 1 & \\
NB  & $25/40 = 0.625$ & $15/40 = 0.375$ & 1 & \\
NA  & $10/20 = 0.500$ & $10/20 = 0.500$ & 1 & \\
\end{tabular}

Each row now sums to $1$. Females are the **most history-rich** group ($66.7\%$ repeat) and `NA`-sex is at chance ($50/50$).

```r
prop.table(tbl, margin = 1)               # each row sums to 1 -> p(j|i)
```

---

**(e) Column-conditional distributions $p(i \mid j) = n_{ij}/n_{\cdot j}$.** "Given a customer's history is $j$, what is the chance of sex $i$?" Divide by *column totals* $(305, 195)$:

\begin{tabular}{p{7cm}|p{7cm}|p{7cm}|p{10cm}}
\textbf{Sex \textbackslash{} History} & \textbf{Yes ($/305$)} & \textbf{No ($/195$)} & \\
F   & $140/305 = 0.459$ & $70/195 = 0.359$ & \\
M   & $130/305 = 0.426$ & $100/195 = 0.513$ & \\
NB  & $25/305 = 0.082$ & $15/195 = 0.077$ & \\
NA  & $10/305 = 0.033$ & $10/195 = 0.051$ & \\
\textbf{Sum} & \textbf{1.000} & \textbf{1.000} & \\
\end{tabular}

Among **repeat** buyers, females are the largest group ($45.9\%$); among **first-time** buyers, males dominate ($51.3\%$). Note the columns sum to $1$, not the rows — this is the **opposite direction** of conditioning vs (d).

```r
prop.table(tbl, margin = 2)               # each column sums to 1 -> p(i|j)
```

---

**(f) Independence check.** Under statistical independence of `Sex` and `History`,
$$p(j \mid i) \;=\; p_{\cdot j} \quad \text{for every } i, j,$$
i.e.\ the row-conditional distributions should all coincide with the marginal column distribution $(0.610, 0.390)$. Compare row by row:

| Sex $i$ | $p(\text{Yes}\mid i)$ | Marginal $p_{\cdot,\text{Yes}}$ | $|\Delta|$ |
|---|---:|---:|---:|
| F  | 0.667 | 0.610 | 0.057 |
| M  | 0.565 | 0.610 | 0.045 |
| NB | 0.625 | 0.610 | 0.015 |
| NA | 0.500 | 0.610 | 0.110 |

The conditional proportions **deviate** from the marginal $0.610$ — most clearly for `F` (above) and `M`/`NA` (below). A formal $\chi^2$ test (expected $e_{ij} = n_{i\cdot} n_{\cdot j}/n$, e.g.\ $e_{F,\text{Yes}} = 210\cdot 305/500 = 128.1$; observed $140$) gives $\chi^2 \approx 5.4$ on $df = (4-1)(2-1) = 3$, $p \approx 0.15$ — borderline non-significant at $\alpha = 0.05$ but suggestive of mild association.

```r
chisq.test(tbl)                           # X^2, df, p-value
chisq.test(tbl)$expected                  # expected counts e_ij under H0 = indep
matrix(colSums(tbl)/sum(tbl), nrow = 4, ncol = 2, byrow = TRUE,
       dimnames = dimnames(tbl))          # H0 conditional (= marginal)
```

---

**(g) Qualitative reading of association.** Putting the conditional comparisons together:

* **Sex moves the history rate** by up to $\approx 17$ pp (from $50\%$ for `NA` to $66.7\%$ for `F`).
* The **direction** is: females are over-represented among repeat buyers, males among first-time buyers — *a modest positive association between `F` and `Yes`*.
* The magnitudes are small enough that, with only $n = 500$, the association is **not formally significant** at conventional levels, but it is *practically* visible.
* `NB` and `NA` sub-groups are too small ($n_{i\cdot} \le 40$) to draw firm conclusions — wide sampling error.

```r
# Visual diagnostics for association
mosaicplot(tbl, shade = TRUE, main = "Sex x History (Pearson residuals)")
barplot(prop.table(tbl, 1), beside = TRUE, legend.text = TRUE,
        main = "Row-conditional p(History | Sex)")
```

---

**Master take-aways.**

1. **Joint vs marginal vs conditional are three different normalisations of the same table** — divide by $n$, by row sums, or by column sums. Always state which.
2. **Row-conditional $\ne$ column-conditional** — Bayes' rule connects them: $p(i\mid j) = p(j\mid i)\,p_{i\cdot}/p_{\cdot j}$. Direction of conditioning matters.
3. **Independence is a *uniformity* statement**: every row-conditional row equals the marginal column distribution (equivalently every column-conditional equals the marginal row). Visually, all rows of `prop.table(tbl,1)` look identical.
4. **Quantify deviations** via the $\chi^2$ statistic (sum of $(o-e)^2/e$) and translate to a $p$-value with $df = (K_R-1)(K_C-1)$. Use `mosaicplot(... , shade = TRUE)` for a visual residual map.
5. **Always do a consistency check**: row totals and column totals must both sum to $n$, and joint proportions must sum to $1$.

---

**Linked snippets:** Ex 3.1b (joint table SmokingArea × District); Ex 3.2c/2e/2g/2h (DS conditional distributions); Ex 3.6a–g (Country × Sex full family — joint, marginal, conditional, χ² independence); Ex 3.7a1/a3 (Product × Sex two-way splits); Ex 3.9a1 (LoL tier × class); Ex 3.10a1/a2 (Company Prod × Channel); Ex 3.12a/b/c (Effectiveness × Channel two-way table).
""",
    "images": ["images/master/master_g7_twoway_ai.png"],
}


master_exercises["g8_condsumm"] = {
    "title": "Master Exam — Conditional summary measures (consolidated)",
    "content": r"""**Setup.** From the `pizzerie` dataset we have monthly `Sales` (€) for $n = 100$ pizzerias, classified by `District` into three groups:

\begin{tabular}{p{7cm}|p{7cm}|p{7cm}|p{7cm}|p{10cm}}
\textbf{District} & $n_g$ & $\bar x_g$ (€) & $s_g$ (€) & \\
L (Loreto)   & 35 & 21\,500 & 7\,800  & \\
M (Missori)  & 33 & 24\,200 & 8\,400  & \\
P (Porta R.) & 32 & 26\,400 & 8\,100  & \\
\textbf{Pooled} & \textbf{100} & \textbf{23\,947} & \textbf{8\,200} & \\
\end{tabular}

The conditional medians (computed from the raw vectors) are $\tilde x_L = 20\,900$, $\tilde x_M = 23\,800$, $\tilde x_P = 25\,900$. We will use these throughout.

---

**(a) Conditional mean, median, and standard deviation per group.**

For a quantitative variable $X$ stratified by a qualitative variable $G$ taking levels $g \in \{L, M, P\}$, the **conditional summaries given $G = g$** are computed *only* on the rows with $G_i = g$:
$$\bar x_g = \frac{1}{n_g}\sum_{i:\,G_i = g} x_i, \qquad
s_g^2 = \frac{1}{n_g - 1}\sum_{i:\,G_i = g}(x_i - \bar x_g)^2, \qquad
\tilde x_g = \text{median}\{x_i : G_i = g\}.$$

Applied to our data:

| District $g$ | $n_g$ | $\bar x_g$ | $\tilde x_g$ | $s_g$ | CV $= s_g/\bar x_g$ |
|---|---:|---:|---:|---:|---:|
| L | 35 | 21\,500 | 20\,900 | 7\,800 | 0.363 |
| M | 33 | 24\,200 | 23\,800 | 8\,400 | 0.347 |
| P | 32 | 26\,400 | 25\,900 | 8\,100 | 0.307 |

Each row is a **complete univariate summary of `Sales` restricted to one District**. The closeness of $\bar x_g$ and $\tilde x_g$ within each group ($|\bar x_g - \tilde x_g|/s_g \approx 0.06$–$0.08$, well below $0.5$) suggests each group's distribution is **roughly symmetric** — no severe skew.

```r
# Conditional summaries by District: mean, median, SD, CV
aggregate(Sales ~ District, data = pizzerie, FUN = mean)
aggregate(Sales ~ District, data = pizzerie, FUN = median)
aggregate(Sales ~ District, data = pizzerie, FUN = sd)
aggregate(Sales ~ District, data = pizzerie,
          FUN = function(x) c(n = length(x), mean = mean(x),
                              median = median(x), sd = sd(x), cv = sd(x)/mean(x)))
# tapply alternative:
tapply(pizzerie$Sales, pizzerie$District, mean)
tapply(pizzerie$Sales, pizzerie$District, sd)
```

---

**(b) Side-by-side boxplots.** A boxplot per District places the five-number summary (min, Q1, median, Q3, max) on a common $y$-axis so the eye can compare **levels** (medians) and **spreads** (IQRs) at once. Schematically:

```
Sales (€)
  45000 |                            o   (outlier in P)
        |          o
  35000 |    +---+                  +---+
        |    |   |     +---+        |   |
  28000 |    | M |     |   |        |   |
  26000 |--- median P ----------(med P) -|
  24000 |---|---|--- median M -----|---|-|
  21000 |---|   |--- median L      |   | |
        |    +-+-+     +-+-+        +-+-+
  10000 |    |                          |
        |____L_________M_________P______
```

(Real plot built via `boxplot(Sales ~ District, data = pizzerie)`.) The plot shows:

* **Medians shift upward** from L to M to P (about $+2\,500$ € per step).
* **Box widths (IQRs) are similar** across districts — within-group variability is roughly homogeneous, in line with the close $s_g$ values.

```r
# Side-by-side boxplots with grand-mean reference line
boxplot(Sales ~ District, data = pizzerie,
        main = "Sales by District", ylab = "Sales (EUR)",
        col = c("#f3c969","#5b8cb7","#c97b63"))
abline(h = mean(pizzerie$Sales), lty = 2)        # grand mean reference
```

---

**(c) Between-group gap vs within-group spread.** The substantive question is whether the District *level* effect is **large relative to within-District noise**. Define:

$$\text{Between-gap (max)} = \bar x_P - \bar x_L = 26\,400 - 21\,500 = 4\,900 \text{ €}.$$
$$\text{Within-group SD (pooled)} = s_{\text{pool}} = \sqrt{\frac{\sum_g (n_g - 1)s_g^2}{n - G}} = \sqrt{\frac{34\cdot 7800^2 + 32\cdot 8400^2 + 31\cdot 8100^2}{97}} \approx 8\,100.$$

The pooled within-group SD ($\approx 8\,100$ €) is **almost double the max between-group gap** ($4\,900$ €). Individual pizzerias scatter much more than group means. Visually, the boxes in (b) **overlap heavily** — a randomly chosen Loreto pizzeria can easily out-earn a randomly chosen Porta Romana one.

---

**(d) Effect size.** Several standardised measures formalise (c):

* **Cohen's $d$ (L vs P)**: $d = (\bar x_P - \bar x_L)/s_{\text{pool}} = 4\,900/8\,100 \approx 0.60$ — a *medium* effect.
* **$\eta^2$ (between/total SS)**: with $SS_{\text{between}} = \sum_g n_g (\bar x_g - \bar x)^2 \approx 4.0\times 10^8$ and $SS_{\text{total}} \approx (n-1)s^2 = 99\cdot 8200^2 \approx 6.66\times 10^9$, $\eta^2 \approx 0.060$ — **District explains $\approx 6\%$ of total variance in Sales**. A small-to-medium effect.
* **One-way ANOVA $F$**: $F = MSB/MSW = (SS_B/2)/(SS_W/97) \approx 3.10$, $df = (2, 97)$, $p \approx 0.05$ — borderline significant.

Interpretation: there is a **real but modest** District effect; predicting an individual pizzeria's sales from its district alone leaves $\approx 94\%$ of variance unexplained.

```r
# One-way ANOVA: F, df, p
fit <- aov(Sales ~ District, data = pizzerie)
summary(fit)
# eta^2 = SS_between / SS_total
ss <- summary(fit)[[1]][["Sum Sq"]]; eta2 <- ss[1]/sum(ss); eta2
# Cohen's d (L vs P), pooled SD:
library(effsize)
cohen.d(Sales ~ District, data = subset(pizzerie, District %in% c("L","P")))
```

---

**(e) Simpson's paradox warning.** Conditional summaries can **reverse the marginal story** when a lurking variable correlates with both group membership and outcome.

*Concrete scenario.* Suppose `Type` $\in \{$takeaway, sit-down$\}$ also matters, and the District–Type mix is unbalanced: Porta Romana is $80\%$ sit-down while Loreto is $30\%$ sit-down. If sit-down pizzerias earn much more on average, the *unconditional* District comparison conflates **location** with **format**. The naive conclusion "Porta R. is a better location" may collapse — or even reverse — once you condition on `Type`:

$$\mathbb E[\text{Sales}\mid \text{District} = P] > \mathbb E[\text{Sales}\mid \text{District} = L]$$
$$\not\Rightarrow\quad \mathbb E[\text{Sales}\mid \text{District} = P,\, \text{Type} = t] > \mathbb E[\text{Sales}\mid \text{District} = L,\, \text{Type} = t]\quad \forall t.$$

**Rule of thumb.** Before claiming a District effect, always stratify by other plausible drivers (Type, Size, Year) and check that the ordering persists *within* every stratum. If it flips in some stratum, the marginal comparison is **misleading** — that is Simpson's paradox.

```r
# Simpson's paradox check: stratify by a second factor
aggregate(Sales ~ District + Type, data = pizzerie, FUN = mean)
interaction.plot(pizzerie$District, pizzerie$Type, pizzerie$Sales,
                 ylab = "Mean Sales", xlab = "District",
                 trace.label = "Type")
```

---

**Master take-aways.**

1. **Conditional summaries = univariate stats restricted to one level of $G$.** Apply mean, median, SD, CV exactly as in the unconditional case — *only the subset changes*.
2. **Side-by-side boxplots beat tables** for comparing $G \ge 3$ groups: they show levels (medians), spreads (IQRs), skew (whisker asymmetry), and outliers simultaneously on a common scale.
3. **The substantive question is gap-vs-noise.** A $4\,900$ € between-mean gap looks impressive *until* you see the within-group SD is $\approx 8\,100$ € — most variation lives *inside* groups, not between them. Effect sizes ($d$, $\eta^2$) formalise this ratio.
4. **Always quantify with $\eta^2$ or Cohen's $d$**, not just with the $p$-value: a significant test on $n = 100$ can still mean tiny practical effect.
5. **Simpson's paradox is the killer of naive group comparisons.** Whenever a third variable plausibly correlates with both group membership and outcome, **stratify and re-check** before drawing causal-sounding conclusions about the grouping variable.

---

**Linked snippets:** Ex 3.1a/1c/1d (conditional mean/median/SD of Sales by SmokingArea); Ex 3.2a/2b/2d/2f/2i/2m (DS AmountSpent conditional summaries by demographic groups); Ex 3.4a1/a2/b (Services Expenses conditional spread); Ex 3.5a1/a2 (TotUsers × Weather conditional summaries); Ex 3.7b1 (conditional summary by Sex); Ex 3.8a (Quantity | Product); Ex 3.9b/9c (LoL conditional distributions); Ex 3.11a/b (Campaign Loyalty conditional summaries).
""",
    "images": ["images/master/master_g8_condsumm_ai.png"],
}

master_exercises["g5_disp"] = {
    "title": "Master Exam — Dispersion measures: range, IQR, variance, SD, CV (consolidated)",
    "content": r"""**Setup.** Customer spending dataset `DS` with variable `AmountSpent` (in euros): $n=500$, sample mean $\bar x = 1200$, sample standard deviation $s = 850$, and five-number summary
$$\min = 200,\quad Q_1 = 600,\quad Q_2 = \text{median}\;(\text{not given}),\quad Q_3 = 1700,\quad \max = 4500.$$

**Why dispersion?** Centre alone ($\bar x$, median) does *not* describe a distribution. Two samples can share the same mean yet differ wildly in spread. Dispersion measures quantify **how far observations lie from the centre**, and (for the CV) make spreads from different variables/units **directly comparable**.

---

### (a) Range = max − min

The crudest spread measure: distance between the two extremes.
$$\boxed{\;\text{Range} \;=\; \max - \min \;=\; 4500 - 200 \;=\; 4300\;\text{euros}.\;}$$

**Pros:** trivially fast, intuitive.
**Cons:** uses only **two observations**, completely ignores the bulk; *extremely sensitive to outliers* (a single very large purchase inflates it). It also grows mechanically with $n$ (more data $\Rightarrow$ more chances to see extremes).

```r
range(DS$AmountSpent)            # min and max
diff(range(DS$AmountSpent))      # range = max - min   -> 4300
```

---

### (b) Interquartile range IQR = $Q_3 - Q_1$

Spread of the **central 50%** of the data --- robust by construction.
$$\boxed{\;\text{IQR} \;=\; Q_3 - Q_1 \;=\; 1700 - 600 \;=\; 1100\;\text{euros}.\;}$$

The IQR ignores the bottom 25% and the top 25%, so it is **robust** to outliers and skew. It is the spread used by the boxplot (box width) and by Tukey's fence rule for outlier flagging: an observation is "outlying" if it lies outside $[Q_1 - 1.5\,\text{IQR},\;Q_3 + 1.5\,\text{IQR}] = [600-1650,\;1700+1650] = [-1050,\;3350]$. The recorded $\max = 4500 > 3350$ already signals at least one Tukey-outlier on the upper tail.

```r
quantile(DS$AmountSpent, c(0.25, 0.75))
IQR(DS$AmountSpent)              # Q3 - Q1             -> 1100
```

---

### (c) Sample variance $s^2 = \tfrac{1}{n-1}\sum(x_i - \bar x)^2$

The **mean squared deviation from the centre**, the workhorse spread measure:
$$\boxed{\;s^2 \;=\; \frac{1}{n-1}\sum_{i=1}^{n}(x_i - \bar x)^2.\;}$$

* **Squaring** makes deviations non-negative *and* heavily penalises large gaps (extreme observations dominate $s^2$).
* The **$n-1$ denominator** (Bessel's correction) makes $s^2$ an *unbiased* estimator of the population variance $\sigma^2$: $\mathbb E[s^2] = \sigma^2$. One degree of freedom is "spent" estimating $\bar x$.

For our DS: $s = 850 \Rightarrow s^2 = 850^2 = 722\,500\;\text{euros}^2$. Note the awkward unit (euros squared) --- variance is **not** in the same units as the data, which motivates the SD.

**Algebraic identity (useful for hand computation):**
$$\sum (x_i-\bar x)^2 \;=\; \sum x_i^2 \;-\; n\,\bar x^2,
\qquad
s^2 \;=\; \frac{\sum x_i^2 - n\bar x^2}{n-1}.$$

```r
var(DS$AmountSpent)              # uses (n-1)          -> 722500
```

---

### (d) Standard deviation $s = \sqrt{s^2}$

Variance brought back to the **original units** of the data:
$$\boxed{\;s \;=\; \sqrt{s^2} \;=\; \sqrt{722\,500} \;=\; 850\;\text{euros}.\;}$$

**Interpretation.** Roughly, $s$ measures the *typical* distance of an observation from $\bar x$. For symmetric, mound-shaped distributions, the empirical "68--95--99.7" rule gives
* about $68\%$ of customers within $\bar x \pm 1s = 1200 \pm 850 = [350,\;2050]$;
* about $95\%$ within $\bar x \pm 2s = [-500,\;2900]$;
* about $99.7\%$ within $\bar x \pm 3s = [-1350,\;3750]$.

The lower band already goes negative ($-500$, $-1350$), which is **impossible** for a spending variable --- a clear signal that `AmountSpent` is **right-skewed**, not symmetric.

**Chebyshev's inequality** (no shape assumption) gives the universal bound
$$P(|X - \bar x| \ge k\,s) \;\le\; \frac{1}{k^2},$$
so e.g. *at least* $75\%$ of the data lie within $\bar x \pm 2s$, *at least* $89\%$ within $\bar x \pm 3s$ --- holds for any distribution.

```r
sd(DS$AmountSpent)               # sqrt(var)           -> 850
# Chebyshev/empirical-rule check
mean(abs(DS$AmountSpent - mean(DS$AmountSpent)) <= 1 * sd(DS$AmountSpent))  # ~0.68 if symmetric
mean(abs(DS$AmountSpent - mean(DS$AmountSpent)) <= 2 * sd(DS$AmountSpent))  # ~0.95 if symmetric
```

![Master illustration](statistics/images/master/master_g5_disp_ai.png)

---

### (e) Coefficient of variation CV = $s / \bar x$ (unit-free)

A spread measure **divided by the centre**, so units cancel:
$$\boxed{\;\text{CV} \;=\; \frac{s}{\bar x} \;=\; \frac{850}{1200} \;=\; 0.7083\ldots \;\approx\; 70.8\%.\;}$$

**Why divide by the mean?**
* **Unit-free** $\Rightarrow$ allows comparison across **different scales**, units, or currencies.
* Captures **relative variability**: a SD of $850$ euros means very different things if $\bar x = 1200$ vs $\bar x = 100\,000$.
* Rule of thumb: $\text{CV} < 0.10$ (10%) = low variability; $0.10$--$0.30$ = moderate; $> 0.30$ = high; $\text{CV} > 1$ = SD exceeds the mean (very dispersed, typical of heavy-tailed/right-skewed data).

Our value $\text{CV} \approx 0.71$ falls in the **high-variability** range --- spending varies by roughly 71% of its average level across customers. Consistent with the right-skew diagnosed from $s$.

**Caveats.** CV requires $\bar x > 0$ and a true **ratio scale** (a meaningful zero). It is **not** appropriate for interval-scale variables like Celsius temperature (zero is arbitrary), nor for variables that can take negative values.

**When to use CV vs SD.**

| Question | Use |
|---|---|
| "How spread is *this* variable in its own units?" | SD ($s$) |
| "Is spending **more dispersed** than income (different scales)?" | CV |
| "Is fund A *relatively* more volatile than fund B?" (returns) | CV |
| "How tight are repeated measurements of the same quantity?" | SD or CV (both fine if $\bar x$ is fixed) |

```r
sd(DS$AmountSpent) / mean(DS$AmountSpent)   # 0.7083  (i.e. 70.8%)
```

---

### (f) Putting it all together --- interpretation for `AmountSpent`

| Measure | Value | What it tells us |
|---|---:|---|
| Range | $4300$ | Extremes span 4300 euros --- a single big spender drives this. |
| IQR | $1100$ | Central half of customers spend within a 1100-euro window. **Robust**. |
| Variance $s^2$ | $722\,500$ euros² | Mean squared deviation; awkward unit but the building block of inference. |
| SD $s$ | $850$ euros | Typical deviation from $\bar x = 1200$. Large relative to $\bar x$. |
| CV | $0.708$ ($70.8\%$) | **High relative variability**; comparable across scales. |

**Shape diagnosis from these numbers alone:**

* $\bar x - 1s = 350 > \min = 200$ but $\bar x + 1s = 2050 < \max = 4500$ $\Rightarrow$ right tail much longer than left.
* $Q_3 - Q_2 > Q_2 - Q_1$ would confirm right skew (median not given, but $Q_3 - \bar x = 500$ vs $\bar x - Q_1 = 600$ already suggests an upper tail stretching well beyond $Q_3$).
* $\max - Q_3 = 2800$ vs $Q_1 - \min = 400$ --- the upper tail is *7$\times$* longer than the lower tail.
* Empirical 68% band $[350,\,2050]$ excludes about $25\%$ on each side --- but the lower side is bounded by $\min = 200$, so the excluded mass is mostly on the **right**.

**Conclusion.** `AmountSpent` is **right-skewed with a heavy upper tail**: the typical customer spends around 1200 euros with a wide spread (SD = 850), and a minority of high-spenders stretches the maximum to 4500. For *comparative* statements across variables (e.g. amount vs frequency of purchases), report the **CV**; for outlier-robust spread, the **IQR**.

```r
# All-in-one numerical summary
summary(DS$AmountSpent)          # Min, Q1, Median, Mean, Q3, Max
fivenum(DS$AmountSpent)          # Tukey's five-number summary

# Boxplot (visualises IQR + outliers)
boxplot(DS$AmountSpent, horizontal = TRUE,
        main = "AmountSpent (euros)", col = "#f4c95d")
```

---

**Master take-aways.**

1. **Range, IQR, variance, SD, CV** form a hierarchy of dispersion measures from coarsest (range, two points) to most refined (variance/SD, all points) to relative (CV, unit-free).
2. **Variance has the wrong unit (squared)**; SD restores the original unit and is the everyday spread measure.
3. **Bessel's $n-1$ correction** makes $s^2$ unbiased --- one df spent on $\bar x$.
4. **IQR is robust**, range is fragile, SD is in between (squaring amplifies outliers).
5. **CV is the *only* dispersion measure that allows comparison across different scales/units** --- but it requires $\bar x > 0$ and a ratio scale.
6. For `AmountSpent`: large CV ($0.71$) + asymmetric extremes ($\max - Q_3 \gg Q_1 - \min$) $\Rightarrow$ **right-skewed, high-variability** spending distribution.

---

**Linked snippets:** master `g4a_bytype` (which measures of *centre* go with which dispersion measure by variable type); master `g4b_skew` (using mean/median/SD to diagnose skew); master `g6a_quant` (Q1, Q3, percentiles --- the building blocks of the IQR); Ex 2.5d (computing SD/CV on `Age` and comparing variables); Ex 2.7d (boxplot reading from five-number summary).
""",
    "images": ["statistics/images/master/master_g5_disp_ai.png"],
}


master_exercises["g6a_quant"] = {
    "title": "Master Exam — Quantiles, percentiles, deciles: definition and computation (consolidated)",
    "content": r"""**Setup.** A `Time` variable (commuting time, in minutes) is reported only as a **grouped frequency table** with cumulative relative frequencies $F(x)$ at the class endpoints:

| Class $[L_j,\,U_j)$ | $n_j$ | $f_j = n_j/n$ | $F(U_j)$ |
|---|---:|---:|---:|
| $[0,\,10)$ | 20 | 0.10 | 0.10 |
| $[10,\,20)$ | 30 | 0.15 | 0.25 |
| $[20,\,30)$ | 50 | 0.25 | 0.50 |
| $[30,\,45)$ | 60 | 0.30 | 0.80 |
| $[45,\,60)$ | 30 | 0.15 | 0.95 |
| $[60,\,90)$ | 10 | 0.05 | 1.00 |
| **Total** | $n=200$ | $1.00$ | --- |

Quantiles answer the **inverse** question to the ECDF: *"below which value does a given fraction $q$ of the mass lie?"*

---

### (a) Definition of the $q$-th quantile

For $q \in (0,1)$, the **$q$-th quantile** $x_q$ of a distribution with CDF $F$ is any value such that
$$\boxed{\;F(x_q) \;=\; q \;\;\Longleftrightarrow\;\; x_q \;=\; F^{-1}(q).\;}$$

In words: $x_q$ is the threshold below which a proportion $q$ of the population (or sample) lies and above which the remaining $1-q$ lies. Synonymous terminology:

| Name | $q$ |
|---|---|
| Median | $0.50$ |
| Quartiles $Q_1, Q_2, Q_3$ | $0.25,\,0.50,\,0.75$ |
| Deciles $D_1, \ldots, D_9$ | $0.1,\,0.2,\,\ldots,\,0.9$ |
| Percentiles $P_1, \ldots, P_{99}$ | $0.01,\,0.02,\,\ldots,\,0.99$ |
| Quintiles | $0.20,\,0.40,\,0.60,\,0.80$ |

For a **continuous** CDF, $F^{-1}(q)$ is uniquely defined. For a **discrete** or **empirical** CDF (a step function), there is a small ambiguity (a whole interval of $x$ values can satisfy $F(x)=q$); the conventions below resolve it.

---

### (b) Sorted-data (empirical) percentile rule --- "smallest $k$"

Given raw sorted observations $x_{(1)} \le x_{(2)} \le \cdots \le x_{(n)}$, the empirical CDF is
$$F_n(x) \;=\; \frac{\#\{i : x_i \le x\}}{n}.$$

The **smallest-$k$ rule** (R type 1, the textbook definition) picks
$$\boxed{\;x_q \;=\; x_{(k)} \quad\text{where}\quad k \;=\; \lceil n\,q\rceil,\;}$$
i.e. the **smallest order statistic** whose empirical CDF reaches $q$:
$$F_n(x_{(k)}) \;=\; k/n \;\ge\; q,
\qquad
F_n(x_{(k-1)}) \;=\; (k-1)/n \;<\; q.$$

**Example.** For $n=200$ and $q = 0.25$: $nq = 50$ $\Rightarrow$ $k = 50$ $\Rightarrow$ $Q_1$ is the $50$-th order statistic. For $q=0.5$: $k = 100$ $\Rightarrow$ $Q_2 = x_{(100)}$. The textbook R default (`type 7`) instead **linearly interpolates** between order statistics, $x_q = x_{(\lfloor h \rfloor)} + (h - \lfloor h \rfloor)(x_{(\lceil h \rceil)} - x_{(\lfloor h \rfloor)})$ with $h = (n-1)q+1$; for large $n$ the two conventions coincide.

When raw data are **not** available --- only the grouped table --- we cannot apply this rule and must use ogive interpolation instead (next section).

```r
# If RAW data are available (sorted-data rule):
# Smallest-k convention (R type 1):
quantile(DS$Time, probs = c(0.25, 0.5, 0.75, 0.9), type = 1)
# Default (R type 7, linear interpolation between order stats):
quantile(DS$Time, probs = c(0.25, 0.5, 0.75, 0.9))
```

---

### (c) Ogive linear interpolation for grouped data

The **ogive** is the polygon obtained by joining the points $(U_j,\,F(U_j))$ at consecutive class endpoints with straight lines. It is the **piecewise-linear interpolant** of the cumulative relative frequency, equivalent to assuming a **uniform distribution within each class**.

To find $x_q$ inside the class $[L_j,\,U_j)$ containing $q$ (i.e. $F(L_j) < q \le F(U_j)$), solve the linear equation
$$\frac{x_q - L_j}{U_j - L_j} \;=\; \frac{q - F(L_j)}{F(U_j) - F(L_j)}$$
which gives
$$\boxed{\;x_q \;=\; L_j \;+\; (U_j - L_j)\,\frac{q - F(L_j)}{F(U_j) - F(L_j)}.\;}$$

This is exactly the formula `approx(x = endpoints, y = F, xout = ...)` implements in R.

```r
# Set up the grouped table (endpoints and cumulative relative frequencies):
endpoints <- c(0, 10, 20, 30, 45, 60, 90)
cumprop   <- c(0, 0.10, 0.25, 0.50, 0.80, 0.95, 1.00)

# Ogive linear interpolation -> quantiles
# x_q = approx(F, x, xout = q)$y    (note the swapped role: we invert F)
q_vec <- c(0.25, 0.50, 0.75, 0.90)
xq    <- approx(x = cumprop, y = endpoints, xout = q_vec)$y
names(xq) <- c("Q1", "Q2 (median)", "Q3", "P90")
xq                                       # 20.00, 30.00, 42.50, 55.00
```

![Master illustration](statistics/images/master/master_g6a_quant_ai.png)

---

### (d) Computing $Q_1$, $Q_2$ (median), $Q_3$, $P_{90}$, deciles for `Time`

**(i) $Q_1$ ($q = 0.25$).** Locate $q=0.25$ in the cumulative column: $F(10)=0.10 < 0.25 \le F(20)=0.25$ $\Rightarrow$ $Q_1$ lies in $[10,\,20)$ (and in fact right at the upper endpoint). Apply the formula:
$$Q_1 \;=\; 10 + (20-10)\cdot\frac{0.25 - 0.10}{0.25 - 0.10} \;=\; 10 + 10\cdot 1 \;=\; 20.$$

**(ii) Median $Q_2$ ($q = 0.50$).** $F(20)=0.25 < 0.50 \le F(30)=0.50$ $\Rightarrow$ median is at the upper endpoint of $[20,\,30)$:
$$Q_2 \;=\; 20 + (30-20)\cdot\frac{0.50 - 0.25}{0.50 - 0.25} \;=\; 30\;\text{minutes}.$$

**(iii) $Q_3$ ($q = 0.75$).** $F(30)=0.50 < 0.75 \le F(45)=0.80$ $\Rightarrow$ $Q_3 \in [30,\,45)$:
$$Q_3 \;=\; 30 + (45-30)\cdot\frac{0.75 - 0.50}{0.80 - 0.50}
   \;=\; 30 + 15\cdot\frac{0.25}{0.30} \;=\; 30 + 12.5 \;=\; 42.5\;\text{minutes}.$$

**(iv) $P_{90}$ ($q = 0.90$).** $F(45)=0.80 < 0.90 \le F(60)=0.95$ $\Rightarrow$ $P_{90} \in [45,\,60)$:
$$P_{90} \;=\; 45 + (60-45)\cdot\frac{0.90 - 0.80}{0.95 - 0.80}
   \;=\; 45 + 15\cdot\frac{0.10}{0.15} \;=\; 45 + 10 \;=\; 55\;\text{minutes}.$$

**(v) Deciles $D_1, \ldots, D_9$.** Same formula with $q = 0.1, 0.2, \ldots, 0.9$:

| Decile | $q$ | Class | $L_j$ | $F(L_j)$ | $F(U_j)$ | $D_k$ |
|---|---:|---|---:|---:|---:|---:|
| $D_1$ | 0.10 | $[0,10)$ | 0 | 0.00 | 0.10 | $0 + 10\cdot(0.10/0.10) = \mathbf{10.0}$ |
| $D_2$ | 0.20 | $[10,20)$ | 10 | 0.10 | 0.25 | $10 + 10\cdot(0.10/0.15) = \mathbf{16.67}$ |
| $D_3$ | 0.30 | $[20,30)$ | 20 | 0.25 | 0.50 | $20 + 10\cdot(0.05/0.25) = \mathbf{22.0}$ |
| $D_4$ | 0.40 | $[20,30)$ | 20 | 0.25 | 0.50 | $20 + 10\cdot(0.15/0.25) = \mathbf{26.0}$ |
| $D_5 = Q_2$ | 0.50 | $[20,30)$ | 20 | 0.25 | 0.50 | $\mathbf{30.0}$ |
| $D_6$ | 0.60 | $[30,45)$ | 30 | 0.50 | 0.80 | $30 + 15\cdot(0.10/0.30) = \mathbf{35.0}$ |
| $D_7$ | 0.70 | $[30,45)$ | 30 | 0.50 | 0.80 | $30 + 15\cdot(0.20/0.30) = \mathbf{40.0}$ |
| $D_8$ | 0.80 | $[30,45)$ | 30 | 0.50 | 0.80 | $\mathbf{45.0}$ |
| $D_9$ | 0.90 | $[45,60)$ | 45 | 0.80 | 0.95 | $\mathbf{55.0}$ |

Within each class, deciles are **equally spaced in proportion-space** but **unequally spaced in $x$-space** --- precisely because the density is *not* uniform across classes.

```r
# All deciles in one shot:
deciles <- approx(x = cumprop, y = endpoints,
                  xout = seq(0.1, 0.9, by = 0.1))$y
names(deciles) <- paste0("D", 1:9)
deciles                                  # D1=10, D2=16.67, ..., D9=55
```

---

### (e) Reading quantiles off the ECDF / ogive

Geometrically, the quantile $x_q$ is found by:
1. Drawing a **horizontal line** at height $q$ across the ogive.
2. Following it to the **intersection** with the polygon.
3. Dropping a **vertical line** from that intersection to the $x$-axis.

The $x$-coordinate of the foot is $x_q$. This is the **graphical read-off** procedure used in every textbook and in past-exam questions (see Ex 2.4b/c, Ex 2.7d).

**Step-by-step for the median $Q_2$ in our table:**
* horizontal at $y=0.5$ crosses the ogive at $(30,\,0.5)$ (exactly at a node);
* drop vertical $\Rightarrow$ $Q_2 = 30$ min.

**For $Q_3$ at $y=0.75$:** the horizontal cuts the segment from $(30,\,0.50)$ to $(45,\,0.80)$ at proportion $(0.75-0.50)/(0.80-0.50) = 5/6$ along it; the $x$-coordinate is $30 + (5/6)\cdot 15 = 42.5$ min --- matching part (d).

**Coherence check.** $Q_1 = 20 \le Q_2 = 30 \le Q_3 = 42.5$ and $\text{IQR} = Q_3 - Q_1 = 22.5$ min --- the central half of commute times lies in a 22.5-min window. $P_{90} = 55$ min: 10% of commuters take longer than 55 min.

**Inverse direction (CDF, not quantile).** The *same* ogive can be read in the opposite direction: pick an $x$, project up to the polygon, read off $F(x)$ on the $y$-axis. E.g. $F(40) = 0.50 + (40-30)/(45-30)\cdot(0.80-0.50) = 0.50 + 0.20 = 0.70$ $\Rightarrow$ 70% of commutes are $<40$ min.

```r
# Inverse direction: ogive value F(x) for a given x
F_at <- approx(x = endpoints, y = cumprop, xout = c(15, 25, 40, 50))$y
F_at                                     # F(15)=0.175, F(25)=0.375, F(40)=0.70, F(50)=0.85

# Visualise the ogive with quantile reads:
plot(endpoints, cumprop, type = "b", pch = 19, col = "#1f3a5f",
     xlab = "Time (min)", ylab = "F(x)", main = "Ogive of Time")
abline(h = c(0.25, 0.5, 0.75, 0.9), lty = 3, col = "gray60")
abline(v = xq, lty = 3, col = "#f4c95d")
```

---

**Master take-aways.**

1. **A quantile inverts the CDF**: $x_q = F^{-1}(q)$. Median, quartiles, deciles, percentiles are just special $q$'s.
2. **Two computation rules** depending on data:
   * Raw data $\Rightarrow$ **sorted-data rule** ("smallest $k$" with $k = \lceil nq\rceil$, or linear interpolation between order statistics).
   * Grouped data $\Rightarrow$ **ogive linear interpolation**, assuming uniform spread within each class.
3. **Grouped quantile formula:** $x_q = L_j + (U_j - L_j)\dfrac{q - F(L_j)}{F(U_j) - F(L_j)}$ inside the class containing $q$.
4. **Geometric read-off:** horizontal at $y=q$ $\to$ ogive intersection $\to$ vertical to $x$-axis.
5. **Coherence:** quantiles are monotone non-decreasing in $q$; $\text{IQR} = Q_3 - Q_1$, range covered by deciles widens where density is low.
6. For `Time`: $Q_1=20$, $Q_2=30$, $Q_3=42.5$, $P_{90}=55$, $\text{IQR}=22.5$ min. The class $[30,\,45)$ contains $D_6, D_7, D_8$ --- the densest portion of the distribution.

---

**Linked snippets:** master `g1e_cum` (ECDF, ogive --- the object we invert here); master `g2b_approx` (uniform-on-interval assumption underlying the linear interpolation); master `g5_disp` (IQR as a dispersion measure built from $Q_1, Q_3$); Ex 2.4b/c (Q1, Q3, P90 read off the Nr-contracts ogive); Ex 2.7a/d (ogive and quantiles of `Nr_visits`).
""",
    "images": ["statistics/images/master/master_g6a_quant_ai.png"],
}


master_exercises["g9_corr"] = {
    "title": "Master Exam — Covariance & correlation (pizzerie Price vs Sales)",
    "content": r"""**Setup.** A survey of $n=100$ pizzerie records two quantitative variables: $X$ = average **Price** of a Margherita ($\in$), $Y$ = monthly **Sales** (thousands of $\in$). The marginal summaries are

| Variable | Mean | Std. dev. |
|---|---:|---:|
| $X$ = Price | $\bar x_P = 7.50$ | $s_P = 1.20$ |
| $Y$ = Sales | $\bar x_S = 18.40$ | $s_S = 4.80$ |

The joint behaviour is captured by **covariance** and **correlation**, the two summary numbers that quantify *linear association* between $X$ and $Y$.

---

### (a) Covariance — the formula and what it measures

The **sample covariance** is
$$\boxed{\;\operatorname{cov}(X,Y) \;=\; \frac{1}{n-1}\sum_{i=1}^{n}(x_i-\bar x)(y_i-\bar y)\;}$$

Each term $(x_i-\bar x)(y_i-\bar y)$ is **positive** when both deviations have the *same* sign (point in the upper-right or lower-left of the scatter relative to the means) and **negative** when they have *opposite* signs (upper-left or lower-right). Summing across the $n$ points and dividing by $n-1$ (Bessel's correction, same as for $s^2$) gives a single number whose **sign** says everything about *direction*:

* $\operatorname{cov}>0$: $X$ and $Y$ tend to move **together** (high $X$ with high $Y$).
* $\operatorname{cov}<0$: they move in **opposite** directions.
* $\operatorname{cov}\approx 0$: no *linear* tendency (could still be non-linearly associated).

**The unit problem.** Covariance carries the **product of the units** of $X$ and $Y$. For pizzerie, $\operatorname{cov}(P,S)$ is measured in $\in\cdot(\text{thousand }\in)$ --- not directly interpretable. Worse, rescaling $X$ (say, expressing Price in cents instead of euros) multiplies the covariance by $100$ without changing the underlying relationship. We therefore **standardise** $\Rightarrow$ correlation.

---

### (b) Pearson correlation coefficient $r$

Divide covariance by the product of the two standard deviations:
$$\boxed{\;r_{XY} \;=\; \frac{\operatorname{cov}(X,Y)}{s_X \, s_Y} \;=\; \frac{\sum_i (x_i-\bar x)(y_i-\bar y)}{\sqrt{\sum_i (x_i-\bar x)^2}\,\sqrt{\sum_i (y_i-\bar y)^2}}\;}$$

This is the **Pearson product-moment correlation**. Key properties:

| Property | Statement |
|---|---|
| Dimensionless | $r$ is a pure number; units cancel. |
| Bounded | $-1 \le r \le +1$ (Cauchy–Schwarz inequality). |
| $r=+1$ | All points lie on an *increasing* straight line. |
| $r=-1$ | All points lie on a *decreasing* straight line. |
| $r=0$ | No **linear** association (does not rule out non-linear). |
| Symmetric | $r_{XY} = r_{YX}$. |
| Scale/location invariant | $r$ is unchanged by any linear rescaling $X' = a + bX$ (with $b>0$). |

**Worked numbers (pizzerie).** Suppose the raw sums give $\sum (x_i-\bar x)(y_i-\bar y) = -85.0$ (negative --- pricier pizzerie tend to sell less). Then
$$\operatorname{cov}(P,S) \;=\; \frac{-85.0}{99} \;=\; -0.859 \;\;[\,\in\cdot \text{k}\in\,],$$
$$r \;=\; \frac{-0.859}{1.20 \times 4.80} \;=\; \frac{-0.859}{5.76} \;=\; -0.149.$$
A **weak negative** linear association: higher Price is mildly associated with lower Sales, but the relationship explains only a small fraction of the spread (see part d).

```r
price <- pizzerie$Price; sales <- pizzerie$Sales
mean(price); sd(price)          # 7.50 ; 1.20
mean(sales); sd(sales)          # 18.40 ; 4.80
cov(price, sales)               # -0.859 (units: euro * thousand euro)
cor(price, sales)               # -0.149  (Pearson, default)
# Manual formula check:
n <- length(price)
sum((price - mean(price)) * (sales - mean(sales))) / (n - 1)   # cov
sum((price - mean(price)) * (sales - mean(sales))) /
  sqrt(sum((price - mean(price))^2) * sum((sales - mean(sales))^2))   # r
```

---

### (c) Reading a scatterplot — direction, form, strength

Before computing $r$ you should always **look** at the cloud. Three descriptors:

| Aspect | What to assess | Implication for $r$ |
|---|---|---|
| **Direction** | Does $Y$ tend to rise or fall as $X$ rises? | Sign of $r$. |
| **Form** | Linear / curved / clustered / fan-shaped? | $r$ only summarises the *linear* part. |
| **Strength** | How tightly do points hug the trend line? | Magnitude $|r|$. |

**Strength bands (rough convention).**

| $|r|$ | Verbal label |
|---|---|
| $0.00$–$0.10$ | Negligible |
| $0.10$–$0.30$ | Weak |
| $0.30$–$0.50$ | Moderate |
| $0.50$–$0.70$ | Strong |
| $0.70$–$1.00$ | Very strong |

For pizzerie, $|r|=0.149$ falls in **weak**: visible negative tilt, but lots of unexplained scatter.

```r
# Visual diagnosis: scatter + LS line
plot(price, sales,
     xlab = "Price (euro)", ylab = "Sales (thousand euro)",
     main = "Pizzerie: Price vs Sales")
abline(lm(sales ~ price), col = "navy", lwd = 2)
```

---

### (d) Coefficient of determination $r^2$ — variance explained

Squaring the correlation gives a percentage interpretation:
$$\boxed{\;r^2 \;=\; \text{fraction of }\operatorname{Var}(Y)\text{ linearly explained by }X.\;}$$

This is the same $R^2$ that appears in simple linear regression of $Y$ on $X$ (or $X$ on $Y$ --- it is symmetric in the bivariate case). The decomposition is
$$\operatorname{Var}(Y) \;=\; \underbrace{r^2\operatorname{Var}(Y)}_{\text{explained}} \;+\; \underbrace{(1-r^2)\operatorname{Var}(Y)}_{\text{residual}}.$$

**Pizzerie.** $r=-0.149 \Rightarrow r^2 = 0.0222 \Rightarrow$ Price linearly accounts for only **2.2%** of the variation in Sales. The remaining $97.8\%$ is driven by location, quality, marketing, customer base, etc. --- correlation alone tells us most of the story is *elsewhere*.

```r
cor(price, sales)^2             # 0.0222 -> r^2 = 2.2% variance explained
```

---

### (e) Correlation $\neq$ causation

A strong $|r|$ proves only that two variables **co-vary**, not that one **causes** the other. Three competing explanations always coexist:

1. **$X$ causes $Y$** (the intuitive read).
2. **$Y$ causes $X$** (reverse causation --- e.g. high Sales let owners raise Price).
3. **A third variable $Z$ causes both** (confounding --- e.g. tourist-area location drives both Price *and* Sales upward, *masking* the negative direct effect of Price).

For pizzerie, $Z$ = neighbourhood income is an obvious confounder. To make causal claims one needs **randomised experiments**, **controlled regression** (multivariate adjustment), **instrumental variables**, or **natural experiments** --- never $r$ alone.

---

### (f) Sensitivity to outliers

Pearson's $r$ is built from sums of *products of deviations*, so a single extreme point can **dominate** the calculation. A high-leverage outlier in the upper-right corner can drag $r$ from near $0$ up to $+0.8$, or *vice versa*. Diagnostic habits:

* Plot the data **first**; never trust $r$ without a scatter.
* Recompute $r$ after removing the most extreme 1–2 points; if $r$ changes a lot, the conclusion was outlier-driven.
* Consider a **robust** alternative (Spearman, below; or Kendall's $\tau$).

```r
# Outlier sensitivity check: drop the most extreme joint point
idx_out <- which.max(abs(scale(price)) + abs(scale(sales)))
cor(price[-idx_out], sales[-idx_out])    # r without the most extreme point
```

---

### (g) Spearman rank correlation $r_S$ — for monotonic but non-linear association

Replace each $x_i$ by its **rank** $R(x_i)$ within the $X$ sample, and each $y_i$ by its rank $R(y_i)$ within the $Y$ sample. Then compute the *Pearson correlation of the ranks*:
$$\boxed{\;r_S \;=\; \operatorname{corr}\bigl(R(X), R(Y)\bigr).\;}$$

**When to prefer Spearman:**

| Situation | Why Spearman wins |
|---|---|
| Relationship is **monotonic** but **curved** (e.g. $Y = e^X$) | Pearson under-reports; Spearman captures the full monotone strength (gives $r_S = +1$ for any strictly increasing transform). |
| **Outliers** present | Ranks bound each observation's influence to its position $\Rightarrow$ robust. |
| **Ordinal** variables (e.g. Likert) | Ranks are the natural scale. |

Properties: $-1\le r_S \le +1$; equals $\pm 1$ iff the ranks match perfectly $\Leftrightarrow$ $Y$ is a strictly monotone function of $X$.

```r
# Spearman (monotonic, robust) and Kendall (rank-based)
cor(price, sales, method = "spearman")   # rank correlation
cor(price, sales, method = "kendall")    # Kendall's tau
# Inferential tests (H0: rho = 0)
cor.test(price, sales)                   # Pearson, t-test on r
cor.test(price, sales, method = "spearman")
```

---

**Master take-aways.**

1. **Covariance gives direction; correlation gives direction + strength on a universal scale $[-1,+1]$.** Divide by $s_X s_Y$ to standardise.
2. **$r^2$ is the percentage of variance linearly explained.** For pizzerie, $r=-0.149 \Rightarrow r^2 = 2.2\%$ --- Price is a weak predictor of Sales.
3. **Always plot first.** Direction, form, strength are visual; $r$ only summarises *linear* association.
4. **Correlation is not causation.** Confounders, reverse causation, and chance all produce non-zero $r$.
5. **Pearson is outlier-sensitive.** Use Spearman or Kendall for monotonic-but-curved or outlier-prone data; both work on ranks.

---

**Linked snippets:** Ex 3.1e (Price vs Sales scatter + cov + Pearson r ≈ 0.67); Ex 3.2l (DS continuous-continuous correlation diagnostic); Ex 3.3a (Satisfaction scatter — direction/form/strength assessment); Ex 3.11c (Campaign Loyalty: Sales vs Revenues vs Costs — weak linear vs non-linear contrast).
""",
    "images": ["images/master/master_g9_corr_ai.png"],
}


master_exercises["g10_normal"] = {
    "title": "Master Exam — Normal distribution N(100, 100)",
    "content": r"""**Setup.** Let $X \sim \mathcal{N}(\mu = 100,\; \sigma^2 = 100)$, so that $\sigma = \sqrt{100} = 10$. The normal (Gaussian) distribution is the **single most important continuous distribution in statistics**: it is the limit of standardised sums (Central Limit Theorem), the natural noise model in regression, and the basis of nearly every classical inference procedure.

---

### (a) Density formula

The probability density function of $X \sim \mathcal{N}(\mu,\sigma^2)$ is
$$\boxed{\;f(x) \;=\; \frac{1}{\sigma\sqrt{2\pi}}\;\exp\!\left(-\,\frac{(x-\mu)^2}{2\sigma^2}\right),\qquad x\in\mathbb{R}.\;}$$

For $X \sim \mathcal{N}(100, 100)$:
$$f(x) \;=\; \frac{1}{10\sqrt{2\pi}}\;\exp\!\left(-\,\frac{(x-100)^2}{200}\right).$$

```r
mu <- 100; sigma <- 10                   # sqrt(100)
dnorm(x = 100, mean = mu, sd = sigma)    # peak height = 0.03989
curve(dnorm(x, mu, sigma), from = 60, to = 140,
      lwd = 2, col = "navy", ylab = "f(x)",
      main = "Density of N(100, 100)")
abline(v = mu, col = "darkorange", lwd = 1.5, lty = 2)
```

**Shape properties.**

| Property | Statement |
|---|---|
| Symmetry | $f(\mu + t) = f(\mu - t)$; symmetric about $\mu$. |
| Mode | Single peak at $x = \mu = 100$, with $f(\mu) = 1/(\sigma\sqrt{2\pi}) \approx 0.0399$. |
| Inflection points | At $x = \mu \pm \sigma = 90$ and $110$. |
| Tails | Exponentially light --- never zero, but vanishingly small far from $\mu$. |
| Total area | $\int_{-\infty}^{+\infty} f(x)\,dx = 1$ (normalising constant $1/(\sigma\sqrt{2\pi})$ ensures this). |
| Moments | $\mathbb{E}[X] = \mu = 100$, $\operatorname{Var}(X) = \sigma^2 = 100$, skewness $=0$, excess kurtosis $=0$. |

---

### (b) Empirical 68–95–99.7 rule

For *any* normal distribution, fixed proportions of probability mass lie within $k$ standard deviations of the mean:
$$\boxed{\;
\mathbb{P}(\mu-\sigma \le X \le \mu+\sigma) \approx 0.6827, \;\;
\mathbb{P}(\mu-2\sigma \le X \le \mu+2\sigma) \approx 0.9545, \;\;
\mathbb{P}(\mu-3\sigma \le X \le \mu+3\sigma) \approx 0.9973.\;}$$

**Applied to $\mathcal{N}(100, 100)$ (with $\sigma=10$):**

| Interval | Range | Probability |
|---|---|---:|
| $\mu \pm 1\sigma$ | $[90,\, 110]$ | $0.6827$ |
| $\mu \pm 2\sigma$ | $[80,\, 120]$ | $0.9545$ |
| $\mu \pm 3\sigma$ | $[70,\, 130]$ | $0.9973$ |

So roughly **2/3** of observations lie within $\pm 10$ of $100$, **95%** within $\pm 20$, and **99.7%** within $\pm 30$. Anything outside $[70,130]$ is a $>3\sigma$ event --- expected to occur about $3$ times per $1\,000$ draws.

```r
# Verify 68 / 95 / 99.7 rule numerically
pnorm(mu +   sigma, mu, sigma) - pnorm(mu -   sigma, mu, sigma)  # 0.6827
pnorm(mu + 2*sigma, mu, sigma) - pnorm(mu - 2*sigma, mu, sigma)  # 0.9545
pnorm(mu + 3*sigma, mu, sigma) - pnorm(mu - 3*sigma, mu, sigma)  # 0.9973
```

---

### (c) Standardisation $Z = (X-\mu)/\sigma$

The **Z-score** transforms any normal $X$ to the **standard normal** $Z \sim \mathcal{N}(0,1)$:
$$\boxed{\;Z \;=\; \frac{X - \mu}{\sigma} \;\sim\; \mathcal{N}(0,1).\;}$$

Why this works: subtracting $\mu$ shifts the distribution to mean $0$; dividing by $\sigma$ rescales it to variance $1$. The standard normal has a **fixed**, tabulated CDF $\Phi(\cdot)$ --- so probability questions about any $\mathcal{N}(\mu,\sigma^2)$ reduce to lookups (or `pnorm` calls) on $\Phi$.

**Examples for $\mathcal{N}(100, 100)$:**

| $x$ | $z = (x-100)/10$ | Interpretation |
|---:|---:|---|
| $90$ | $-1.0$ | "$1$ SD below the mean" |
| $115$ | $+1.5$ | "$1.5$ SD above" |
| $125$ | $+2.5$ | "$2.5$ SD above" --- already in the right tail |
| $80$ | $-2.0$ | bottom $\approx 2.3\%$ |

A z-score is unitless; it expresses *how many standard deviations away* an observation is.

---

### (d) Computing $\mathbb{P}(X \le x)$ via $\Phi$

The CDF of any normal is obtained from the **standard** normal CDF $\Phi$:
$$\boxed{\;F(x) \;=\; \mathbb{P}(X \le x) \;=\; \Phi\!\left(\frac{x-\mu}{\sigma}\right).\;}$$

**Worked: $\mathbb{P}(X \le 115)$ for $\mathcal{N}(100,100)$.**
$$z = \frac{115 - 100}{10} = 1.5,\qquad \mathbb{P}(X\le 115) = \Phi(1.5) = 0.9332.$$

**Worked: $\mathbb{P}(90 \le X \le 115)$.**
$$\mathbb{P}(90 \le X \le 115) = \Phi(1.5) - \Phi(-1.0) = 0.9332 - 0.1587 = 0.7745.$$

```r
pnorm(q = 115, mean = mu, sd = sigma)            # 0.9332
pnorm(115, mu, sigma) - pnorm(90, mu, sigma)     # P(90 <= X <= 115) = 0.7745
pnorm(1.5)                                       # 0.9332 -- standardisation check
```

**Useful $\Phi$ values to memorise.**

| $z$ | $\Phi(z)$ |
|---:|---:|
| $-2.58$ | $0.005$ |
| $-1.96$ | $0.025$ |
| $-1.64$ | $0.050$ |
| $-1.00$ | $0.1587$ |
| $\phantom{-}0.00$ | $0.5000$ |
| $+1.00$ | $0.8413$ |
| $+1.64$ | $0.9500$ |
| $+1.96$ | $0.9750$ |
| $+2.58$ | $0.9950$ |

**Symmetry shortcut:** $\Phi(-z) = 1 - \Phi(z)$.

---

### (e) Quantile inversion $x_q = \mu + z_q \cdot \sigma$

Given a target probability $q\in(0,1)$, the **$q$-quantile** of $X\sim\mathcal{N}(\mu,\sigma^2)$ is
$$\boxed{\;x_q \;=\; \mu + z_q \cdot \sigma,\qquad z_q = \Phi^{-1}(q).\;}$$

This is just the standardisation step solved for $x$.

**Worked examples for $\mathcal{N}(100, 100)$:**

| $q$ | $z_q$ | $x_q = 100 + 10\,z_q$ | Interpretation |
|---:|---:|---:|---|
| $0.025$ | $-1.960$ | $80.40$ | $2.5\%$ of $X$ lie below $80.40$ |
| $0.05$ | $-1.645$ | $83.55$ | $5\%$ below $83.55$ |
| $0.25$ | $-0.674$ | $93.26$ | Lower quartile $Q_1$ |
| $0.50$ | $0.000$ | $100.00$ | Median (= mean for normal) |
| $0.75$ | $+0.674$ | $106.74$ | Upper quartile $Q_3$ |
| $0.95$ | $+1.645$ | $116.45$ | $95\%$ of $X$ lie below $116.45$ |
| $0.975$ | $+1.960$ | $119.60$ | $97.5\%$ below $119.60$ |

The **central 95% interval** is therefore $[80.40,\, 119.60]$ --- the *exact* version of the "$\mu \pm 2\sigma$" rule of thumb $[80,120]$.

```r
qnorm(p = 0.025, mean = mu, sd = sigma)          # 80.40
qnorm(p = 0.975, mean = mu, sd = sigma)          # 119.60
qnorm(c(0.25, 0.50, 0.75), mu, sigma)            # 93.26, 100.00, 106.74
```

---

### (f) Tail probabilities

Right tail:
$$\mathbb{P}(X > x) \;=\; 1 - F(x) \;=\; 1 - \Phi\!\left(\tfrac{x-\mu}{\sigma}\right).$$

Two-sided tail (useful for symmetric tests):
$$\mathbb{P}(|X-\mu| > k\sigma) \;=\; 2\,\bigl[1 - \Phi(k)\bigr].$$

**Worked for $\mathcal{N}(100,100)$.**

| Question | Computation | Answer |
|---|---|---:|
| $\mathbb{P}(X > 125)$ | $1 - \Phi(2.5) = 1 - 0.9938$ | $0.0062$ |
| $\mathbb{P}(X < 75)$ | $\Phi(-2.5) = 0.0062$ | $0.0062$ |
| $\mathbb{P}(\|X-100\| > 20)$ | $2[1 - \Phi(2)] = 2(0.0228)$ | $0.0455$ |
| $\mathbb{P}(X \in [95,105])$ | $\Phi(0.5) - \Phi(-0.5)$ | $0.3829$ |

The right-tail probability $\mathbb{P}(X>125) = 0.0062$ is the same as $\mathbb{P}(Z > 2.5)$ --- the **universality** of the standard normal.

```r
# Right-tail and two-sided probabilities
1 - pnorm(125, mu, sigma)                        # P(X > 125)   = 0.00621
pnorm(125, mu, sigma, lower.tail = FALSE)        # same, one call
2 * (1 - pnorm(120, mu, sigma))                  # P(|X-100|>20) = 0.0455
# Simulation: empirical check
set.seed(1)
x <- rnorm(n = 100000, mean = mu, sd = sigma)
mean(x); sd(x)                                   # ~ 100, ~ 10
mean(x >= 90 & x <= 110)                         # ~ 0.683
quantile(x, c(0.025, 0.975))                     # ~ 80.4, 119.6
```

---

**Master take-aways.**

1. **The Gaussian density is fully determined by $(\mu,\sigma^2)$:** $\mu$ locates the centre, $\sigma$ sets the width, the shape is always the same bell.
2. **68 / 95 / 99.7** is the must-memorise rule of thumb: $\pm 1, \pm 2, \pm 3$ SDs cover those probabilities for *any* normal.
3. **Standardisation $Z=(X-\mu)/\sigma$** converts every normal question to a question about $\mathcal{N}(0,1)$, whose CDF $\Phi$ is tabulated / available via `pnorm`.
4. **CDF $\to$ probabilities, inverse CDF $\to$ quantiles.** $\mathbb{P}(X\le x)=\Phi((x-\mu)/\sigma)$ and $x_q = \mu + z_q\sigma$.
5. **In R:** `dnorm` for density, `pnorm` for CDF and tail probabilities, `qnorm` for quantiles, `rnorm` for simulation. With $\mathcal{N}(100,100)$: central $95\%$ interval is $[80.40, 119.60]$.

---

**Linked snippets:** Ex 4.1a / Ex 4.2a (basic normal probability computations); Ex 4.3a (z-score practice); Ex 4.3b (inverse CDF / quantile); master `g8_var` (variance --- the $\sigma^2$ parameter); master `g11_clt` (Central Limit Theorem --- why $\mathcal{N}$ is ubiquitous as a sampling distribution); master `g13_inf` (z- and t-intervals built on normality).
""",
    "images": ["images/master/master_g10_normal_ai.png"],
}


master_exercises["g11_clt"] = {
    "title": "Master — Sampling distributions & the CLT",
    "content": r"""**Setup.** Let $X_1, X_2, \dots, X_n$ be an i.i.d. sample from a population with mean $\mu = E[X]$ and variance $\sigma^2 = \text{Var}(X) < \infty$. The **sample mean**
$$\bar X \;=\; \frac{1}{n}\sum_{i=1}^n X_i$$
is itself a random variable, with its own distribution called the **sampling distribution of $\bar X$**. The whole point of inferential statistics is to know how $\bar X$ behaves so we can use it to estimate the unknown $\mu$.

---

### (a) Mean, variance, and standard error of $\bar X$

Regardless of the population distribution (continuous, discrete, skewed, anything --- as long as $\mu, \sigma^2$ exist):
$$\boxed{\;E[\bar X] \;=\; \mu, \qquad \text{Var}(\bar X) \;=\; \frac{\sigma^2}{n}, \qquad \text{SE}(\bar X) \;=\; \frac{\sigma}{\sqrt{n}}\;}$$

**Why?** By linearity, $E[\bar X] = \tfrac{1}{n}\sum E[X_i] = \tfrac{1}{n}\cdot n\mu = \mu$ (unbiasedness). By independence + scaling, $\text{Var}(\bar X) = \tfrac{1}{n^2}\sum \text{Var}(X_i) = \tfrac{1}{n^2}\cdot n\sigma^2 = \sigma^2/n$.

**Key implication --- the $\sqrt{n}$ law.** Doubling the sample size does *not* halve the SE; it shrinks it by $1/\sqrt 2 \approx 0.707$. To **halve** the SE you need **four times** the data.

| $n$ | SE multiplier vs $\sigma$ | SE if $\sigma = 10$ |
|---:|---:|---:|
| 1   | $1.000$ | $10.00$ |
| 4   | $0.500$ | $5.00$  |
| 25  | $0.200$ | $2.00$  |
| 100 | $0.100$ | $1.00$  |
| 400 | $0.050$ | $0.50$  |

```r
# Mean, variance, SE of X-bar
mu <- 50; sigma <- 12; n <- 36
se <- sigma / sqrt(n)             # 2
c(E_Xbar = mu, Var_Xbar = sigma^2/n, SE_Xbar = se)
```

---

### (b) Normal population $\Rightarrow$ $\bar X$ is *exactly* Normal

If $X_i \stackrel{\text{iid}}{\sim} N(\mu, \sigma^2)$ then
$$\bar X \;\sim\; N\!\left(\mu,\; \frac{\sigma^2}{n}\right) \quad \text{for every } n \ge 1.$$
**No approximation needed** --- the result is exact because linear combinations of jointly Normal variables are Normal (see master `g12_lincomb`). This is the "best case" --- even $n=2$ gives an exactly Normal $\bar X$.

```r
# Simulate X-bar for X ~ N(mu, sigma^2): exact Normal sampling distribution
xbar_sim <- replicate(10000, mean(rnorm(n, mu, sigma)))
hist(xbar_sim, breaks = 40, freq = FALSE, main = "X-bar from Normal pop")
curve(dnorm(x, mu, se), add = TRUE, col = "red", lwd = 2)
```

---

### (c) Central Limit Theorem (CLT) --- the universal saviour

For **any** population with finite mean $\mu$ and variance $\sigma^2$, as $n \to \infty$:
$$\frac{\bar X - \mu}{\sigma/\sqrt{n}} \;\xrightarrow{d}\; N(0,1) \qquad \Longleftrightarrow \qquad \bar X \;\stackrel{\text{approx}}{\sim}\; N\!\left(\mu, \frac{\sigma^2}{n}\right).$$
This works whether $X$ is Bernoulli, Exponential, Uniform, Poisson, heavy-skewed income data, anything --- *provided* $\sigma^2$ is finite.

**The miracle:** the population shape can be arbitrarily ugly (e.g. Exponential is very right-skewed), yet $\bar X$ becomes bell-shaped as $n$ grows. The sampling distribution converges to Normal.

```r
# CLT in action: skewed (Exponential) parent -> bell-shaped X-bar
xbar_exp <- replicate(10000, mean(rexp(n, rate = 1/mu)))   # E[X]=mu
hist(xbar_exp, breaks = 40, freq = FALSE, main = "X-bar from Exp pop (n=36)")
curve(dnorm(x, mu, mu/sqrt(n)), add = TRUE, col = "red", lwd = 2)
```

---

### (d) Rule of thumb: when is $n$ "large enough"?

The standard practical threshold:
$$\boxed{\;n \;\ge\; 30\;}$$
For most "moderately well-behaved" populations (not extremely skewed, no heavy tails), $n=30$ already gives a Normal approximation accurate to within a couple of percent for typical probability calculations.

**Caveats:**

| Population shape | Recommended $n$ |
|---|---:|
| Already Normal | $n \ge 1$ (exact) |
| Symmetric, light tails (Uniform, etc.) | $n \ge 10\text{--}15$ |
| Moderately skewed (Exponential-ish) | $n \ge 30$ |
| Heavily skewed / outlier-prone (lognormal incomes) | $n \ge 50\text{--}100$ |
| Bernoulli with $p$ near $0$ or $1$ | need $np \ge 5$ *and* $n(1-p) \ge 5$ |

```r
# Rule-of-thumb check: X-bar density across several n from a skewed population
for (n_try in c(2, 5, 10, 30, 100)) {
  sim <- replicate(5000, mean(rexp(n_try, 1)))
  plot(density(sim), main = paste("n =", n_try))
}
```

---

### (e) Sampling distribution of a proportion $\hat p$

Let $X_1, \dots, X_n \stackrel{\text{iid}}{\sim} \text{Bernoulli}(p)$, so each $X_i \in \{0,1\}$ with $P(X_i=1)=p$. The **sample proportion**
$$\hat p \;=\; \frac{1}{n}\sum_{i=1}^n X_i$$
is just a sample mean of Bernoullis, so the general result specialises to:
$$\boxed{\;E[\hat p] \;=\; p, \qquad \text{Var}(\hat p) \;=\; \frac{p(1-p)}{n}, \qquad \text{SE}(\hat p) \;=\; \sqrt{\frac{p(1-p)}{n}}\;}$$
and by the CLT, for $np \ge 5$ and $n(1-p) \ge 5$:
$$\hat p \;\stackrel{\text{approx}}{\sim}\; N\!\left(p,\; \frac{p(1-p)}{n}\right).$$
This is the entire basis for proportion confidence intervals and tests.

```r
# Sample proportion: CLT condition + tail probability
p <- 0.4; n <- 100
se_p <- sqrt(p * (1 - p) / n)         # 0.04899
# CLT condition: np = 40 >= 5, n(1-p) = 60 >= 5  -> OK
pnorm(0.45, mean = p, sd = se_p)      # P(p_hat <= 0.45) ~ 0.8472
```

---

### (f) Worked example --- computing $P(\bar X \le c)$

**Problem.** A population has $\mu = 50$, $\sigma = 12$ (shape unknown but not crazy). Draw $n = 36$ i.i.d. observations. Find $P(\bar X \le 53)$.

**Step 1 — sampling distribution.** $n=36 \ge 30$, so by CLT
$$\bar X \;\stackrel{\text{approx}}{\sim}\; N\!\left(\mu = 50,\; \sigma_{\bar X}^2 = \frac{12^2}{36} = 4\right), \quad \text{SE} = \sigma/\sqrt n = 12/6 = 2.$$

**Step 2 — standardise.**
$$Z \;=\; \frac{\bar X - 50}{2}, \qquad P(\bar X \le 53) \;=\; P\!\left(Z \le \frac{53-50}{2}\right) \;=\; P(Z \le 1.5).$$

**Step 3 — table / R.** $\Phi(1.5) = 0.9332$, so
$$P(\bar X \le 53) \;\approx\; 0.9332.$$

**Contrast with a single observation.** $P(X \le 53)$ would require knowing the population shape (and would be much less peaked --- a single $X$ has SD $12$, not $2$). Averaging $n=36$ values squeezes the distribution by a factor of $6$.

```r
# Direct CLT-based tail probability + standardisation check
pnorm(53, mean = 50, sd = 12/sqrt(36))     # 0.9331928
pnorm((53 - 50) / (12/sqrt(36)))           # 0.9331928 (standardised form)
```

---

**Master take-aways.**

1. **$E[\bar X] = \mu$ and $\text{Var}(\bar X) = \sigma^2/n$** *always* hold (i.i.d. + finite variance). The standard error $\sigma/\sqrt n$ is the single most important number in inferential statistics.
2. **Normal population $\Rightarrow$ exact Normal $\bar X$**; otherwise the CLT gives an *approximate* Normal $\bar X$ once $n$ is large enough.
3. **$n \ge 30$** is the universal rule of thumb --- but stretch it to $50\text{--}100$ for heavily skewed populations and check $np, n(1-p) \ge 5$ for proportions.
4. **A proportion is just a Bernoulli sample mean**: $\hat p \sim N(p, p(1-p)/n)$ approximately.
5. **Probability calculations**: always standardise $Z = (\bar X - \mu)/(\sigma/\sqrt n)$ and look up $\Phi(z)$ (or use `pnorm` in R).

---

**Linked snippets:** master `g12_lincomb` (linear combinations of Normals --- where the "Normal pop $\Rightarrow$ Normal $\bar X$" claim is proved); confidence-intervals and hypothesis-testing snippets (every CI/test uses SE $= \sigma/\sqrt n$ as its scale).
""",
    "images": ["images/master/master_g11_clt_ai.png"],
}


master_exercises["g12_lincomb"] = {
    "title": "Master — Linear combinations of Normal random variables",
    "content": r"""**Setup.** Let $X \sim N(\mu_X, \sigma_X^2)$ and $Y \sim N(\mu_Y, \sigma_Y^2)$ with $(X, Y)$ **jointly (bivariate) Normal**, and let $\rho = \text{Corr}(X,Y)$ so that
$$\text{Cov}(X,Y) \;=\; \rho \, \sigma_X \, \sigma_Y.$$
For constants $a, b, c \in \mathbb{R}$, consider the linear combination $W = aX + bY + c$.

---

### (a) Expectation --- linearity always holds

$$\boxed{\;E[aX + bY + c] \;=\; a\,\mu_X \;+\; b\,\mu_Y \;+\; c\;}$$
This is **pure linearity** of expectation --- no Normality, no independence, no assumptions whatsoever beyond finite means. Constants pull out; expectation of a sum equals the sum of expectations.

```r
# Parameters and expectation of aX + bY + c
mu_X <- 1000; sig_X <- 300
mu_Y <- 1500; sig_Y <- 400
rho  <- 0.5
a <- 1; b <- 1; c0 <- 0
mu_W <- a * mu_X + b * mu_Y + c0     # 2500
```

---

### (b) Variance --- the covariance term matters

$$\boxed{\;\text{Var}(aX + bY) \;=\; a^2\sigma_X^2 \;+\; b^2\sigma_Y^2 \;+\; 2ab\,\text{Cov}(X,Y)\;}$$

Adding the constant $c$ does **not** change the variance: $\text{Var}(W + c) = \text{Var}(W)$.

Using $\text{Cov}(X,Y) = \rho\,\sigma_X\sigma_Y$:
$$\text{Var}(aX + bY) \;=\; a^2\sigma_X^2 + b^2\sigma_Y^2 + 2ab\rho\,\sigma_X\sigma_Y.$$

**Sign of the cross-term:**

| Case | Cross-term $2ab\,\text{Cov}(X,Y)$ | Effect |
|---|---|---|
| Independent: $\text{Cov}=0$ | $0$ | Variances simply add (weighted by $a^2, b^2$) |
| Positively correlated, $a,b$ same sign | $> 0$ | **Increases** Var (co-movement amplifies) |
| Positively correlated, $a,b$ opposite signs | $< 0$ | **Decreases** Var (co-movement cancels) |
| Negatively correlated, same sign $a,b$ | $< 0$ | **Decreases** Var (hedge effect) |

```r
# Variance of aX + bY with cov_XY = rho * sig_X * sig_Y
cov_XY <- rho * sig_X * sig_Y         # 60000
var_W  <- a^2 * sig_X^2 + b^2 * sig_Y^2 + 2 * a * b * cov_XY   # 370000
sd_W   <- sqrt(var_W)                                          # 608.28
```

---

### (c) Normality is preserved under linear combinations

If $(X, Y)$ is **bivariate Normal**, then for *any* constants $a, b, c$:
$$\boxed{\;aX + bY + c \;\sim\; N\!\left(a\mu_X + b\mu_Y + c,\; a^2\sigma_X^2 + b^2\sigma_Y^2 + 2ab\,\text{Cov}(X,Y)\right)\;}$$

This is the **defining feature** of the multivariate Normal family: every linear combination of jointly Normal variables is itself Normal. (Marginally-Normal-but-not-jointly-Normal counterexamples exist, but in this course "Normal + Normal" always means jointly Normal.)

**Generalisation.** For $X_1, \dots, X_k$ jointly Normal,
$$\sum_{i=1}^k a_i X_i + c \;\sim\; N\!\left(\sum a_i\mu_i + c,\;\; \sum_i a_i^2\sigma_i^2 + 2\sum_{i<j} a_i a_j \,\text{Cov}(X_i,X_j)\right).$$
This is exactly the engine behind master `g11_clt` (the sample mean $\bar X = \tfrac{1}{n}\sum X_i$ is a linear combination with $a_i = 1/n$).

---

### (d) Special cases

Set up everyone's most-used formulas (writing $\sigma_{XY} = \text{Cov}(X,Y)$):

| Combination | Mean | Variance |
|---|---|---|
| $X + Y$ | $\mu_X + \mu_Y$ | $\sigma_X^2 + \sigma_Y^2 + 2\sigma_{XY}$ |
| $X - Y$ | $\mu_X - \mu_Y$ | $\sigma_X^2 + \sigma_Y^2 - 2\sigma_{XY}$ |
| $c + X$ | $c + \mu_X$ | $\sigma_X^2$ (unchanged) |
| $aX$ | $a\mu_X$ | $a^2\sigma_X^2$ |
| $\bar X = \tfrac{1}{n}\sum X_i$ (iid) | $\mu$ | $\sigma^2/n$ |

**Watch the sign.** $\text{Var}(X-Y)$ is **not** $\sigma_X^2 - \sigma_Y^2$ --- variance can never be negative. It is $\sigma_X^2 + \sigma_Y^2 - 2\sigma_{XY}$ (the $\sigma_X^2$ and $\sigma_Y^2$ still **add**; only the covariance term flips sign because $b = -1 \Rightarrow 2ab = -2$).

```r
# Special cases: sum vs difference
var_sum <- sig_X^2 + sig_Y^2 + 2 * cov_XY     # 370000
var_dif <- sig_X^2 + sig_Y^2 - 2 * cov_XY     # 130000 (smaller because rho > 0)
```

---

### (e) Independence is the magical simplifier

If $X \perp\!\!\!\perp Y$, then $\text{Cov}(X,Y) = 0$, so:
$$\text{Var}(aX + bY) \;=\; a^2\sigma_X^2 \;+\; b^2\sigma_Y^2 \qquad (\text{cross-term vanishes}).$$
And $X + Y \sim N(\mu_X + \mu_Y, \sigma_X^2 + \sigma_Y^2)$, $X - Y \sim N(\mu_X - \mu_Y, \sigma_X^2 + \sigma_Y^2)$ --- **same variance** for sum and difference under independence (a common exam trap).

**Note.** For jointly Normal variables, $\text{Cov}(X,Y) = 0$ $\Leftrightarrow$ $X \perp\!\!\!\perp Y$ (a special property of the Normal family). In general $\text{Cov}=0$ does **not** imply independence, but inside the Normal world it does.

```r
# Independence: cov_XY = 0  =>  Var(X+Y) = Var(X-Y) = sig_X^2 + sig_Y^2
var_sum_ind <- sig_X^2 + sig_Y^2              # 250000
```

---

### (f) Worked example --- $P(X + Y > k)$

**Problem.** Daily revenue at branch A: $X \sim N(\mu_X = 1000, \sigma_X^2 = 90000)$ (so $\sigma_X = 300$). Daily revenue at branch B: $Y \sim N(\mu_Y = 1500, \sigma_Y^2 = 160000)$ (so $\sigma_Y = 400$). $(X,Y)$ are bivariate Normal with $\rho = 0.5$. Find $P(X + Y > 2800)$.

**Step 1 — distribution of $S = X+Y$.**
* Mean: $\mu_S = 1000 + 1500 = 2500$.
* Covariance: $\sigma_{XY} = \rho\sigma_X\sigma_Y = 0.5 \cdot 300 \cdot 400 = 60000$.
* Variance: $\sigma_S^2 = 90000 + 160000 + 2(60000) = 370000$, so $\sigma_S = \sqrt{370000} \approx 608.28$.
* Distribution: $S \sim N(2500,\, 370000)$ **exactly** (Normality preserved).

**Step 2 — standardise.**
$$Z \;=\; \frac{S - 2500}{608.28}, \qquad P(S > 2800) \;=\; P\!\left(Z > \frac{2800-2500}{608.28}\right) \;=\; P(Z > 0.4933).$$

**Step 3 — table.** $\Phi(0.49) \approx 0.6879$, so
$$P(X + Y > 2800) \;=\; 1 - 0.6879 \;\approx\; 0.3121.$$

**Sensitivity to $\rho$.** Same problem with $\rho = 0$ (independent branches): $\sigma_S^2 = 250000$, $\sigma_S = 500$, $z = 300/500 = 0.6$, $P = 1 - 0.7257 = 0.2743$. **Positive correlation thickens the right tail of $X+Y$** --- branches move together, so a "both above mean" outcome is more likely than under independence.

```r
# P(X + Y > 2800) with rho = 0.5, and rho = 0 comparison + bivariate simulation
1 - pnorm(2800, mean = mu_W, sd = sd_W)       # 0.3110
pnorm(2800, mean = mu_W, sd = sd_W, lower.tail = FALSE)   # same
sd_ind <- sqrt(sig_X^2 + sig_Y^2)             # 500
pnorm(2800, mean = mu_W, sd = sd_ind, lower.tail = FALSE) # 0.2743
# Bivariate Normal simulation check
library(MASS)
Sigma <- matrix(c(sig_X^2, cov_XY, cov_XY, sig_Y^2), 2, 2)
samp  <- mvrnorm(1e6, mu = c(mu_X, mu_Y), Sigma = Sigma)
mean(rowSums(samp) > 2800)                    # ~0.311 -- matches theory
```

---

**Master take-aways.**

1. **Mean is always linear**: $E[aX + bY + c] = a\mu_X + b\mu_Y + c$ regardless of distribution or dependence.
2. **Variance has a cross-term**: $\text{Var}(aX + bY) = a^2\sigma_X^2 + b^2\sigma_Y^2 + 2ab\,\text{Cov}(X,Y)$. The constant $c$ never affects variance.
3. **Normality is preserved**: any linear combination of jointly Normal variables is Normal --- *exactly*, not approximately. This is the backbone of master `g11_clt`'s "Normal pop $\Rightarrow$ Normal $\bar X$" claim.
4. **$X - Y$ trap**: variance is $\sigma_X^2 + \sigma_Y^2 - 2\sigma_{XY}$ (still **plus** $\sigma_Y^2$); only the covariance term flips sign.
5. **Independence kills the cross-term**: $\text{Cov} = 0 \Rightarrow \text{Var}(X \pm Y) = \sigma_X^2 + \sigma_Y^2$ (sum and difference share the same variance).
6. **For probabilities** $P(aX + bY + c \le k)$: compute the new mean and SD, standardise, and use $\Phi$ (or `pnorm`).

---

**Linked snippets:** master `g11_clt` (uses linear-combination Normality to justify "$X \sim$ Normal $\Rightarrow \bar X \sim$ Normal exactly"); covariance/correlation snippets (where $\rho$ and $\text{Cov}$ are defined); bivariate-Normal and conditional-distribution snippets.
""",
    "images": ["images/master/master_g12_lincomb_ai.png"],
}


master_exercises["g6b_box"] = {
    "title": "Master Exam — Boxplots & the 5-number summary",
    "content": r"""**Setup.** From the pizzerie dataset ($n=100$ shops, monthly turnover `Sales` in k€) we compute the **five-number summary** used to draw a boxplot. The numbers (rounded for display) are

| Statistic | Symbol | Value (k€) |
|---|:-:|---:|
| Minimum | $\min$ | $\mathbf{8.0}$ |
| First quartile | $Q_1$ | $\mathbf{17.9}$ |
| Median | $\widetilde{m}$ | $\mathbf{22.4}$ |
| Third quartile | $Q_3$ | $\mathbf{28.6}$ |
| Maximum | $\max$ | $\mathbf{80.0}$ |

The **interquartile range** is $\text{IQR} = Q_3 - Q_1 = 28.6 - 17.9 = \mathbf{10.7}$ k€. These five numbers plus the IQR are everything a boxplot needs --- the rest is geometric bookkeeping.

---

### (a) The 5-number summary --- what it captures, and what it doesn't

The 5-number summary $(\min, Q_1, \widetilde{m}, Q_3, \max)$ is a **distribution-free** snapshot: it is built from ranks, so it requires *no* parametric assumption (no normality, no symmetry). It partitions the sorted sample into **four equal-count groups of $\approx n/4$ observations each**:

- below $Q_1$: bottom $25\%$ ($\approx 25$ shops with Sales $\le 17.9$ k€);
- $[Q_1, \widetilde{m}]$: next $25\%$ (lower-middle quarter);
- $[\widetilde{m}, Q_3]$: next $25\%$ (upper-middle quarter);
- above $Q_3$: top $25\%$ ($\approx 25$ shops with Sales $\ge 28.6$ k€).

**What it shows.** Centre ($\widetilde{m}$), spread of the middle half ($\text{IQR}$), overall range ($\max-\min$), and --- once placed on the plot --- the **shape** (skew) and the existence of **outliers**.

**What it hides.** *Multimodality* (two peaks vs one give the same quartiles) and *fine structure inside the box* (the 50 middle observations are summarised by a single horizontal stripe). For these, look at the histogram (master `g1c_hist`) or a density plot.

```r
# Five-number summary and IQR
sales <- pizzerie$Sales
fivenum(sales)                              # min, Q1, med, Q3, max
summary(sales)                              # adds the mean
quantile(sales, probs = c(0,0.25,0.5,0.75,1))
IQR(sales)                                  # 10.7 (Q3 - Q1)
```

---

### (b) The box: from $Q_1$ to $Q_3$ --- the middle 50%

The rectangular **box** spans from $Q_1$ to $Q_3$. Its *width* on the axis is the IQR:
$$\text{box width} \;=\; Q_3 - Q_1 \;=\; \text{IQR} \;=\; 10.7 \text{ k€}.$$
By construction, the box contains **exactly the central $50\%$** of the data (the **interquartile mass**). It is a **robust measure of spread**: changing any of the bottom $25\%$ or top $25\%$ of points --- even arbitrarily large changes --- leaves $Q_1$, $Q_3$, and hence the box, unmoved.

For pizzerie Sales, the box stretches over $[17.9, 28.6]$ k€: half of all pizzerie earn somewhere between **18 k€ and 29 k€ per month**.

```r
# Draw the boxplot (box = [Q1, Q3])
boxplot(sales,
        horizontal = TRUE,
        col   = "lightsteelblue",
        main  = "Pizzerie Sales (k EUR) -- monthly turnover, n = 100",
        xlab  = "Sales (k EUR)")
```

---

### (c) The median line inside the box

A horizontal (or, in `boxplot(..., horizontal = TRUE)`, vertical) **line at $\widetilde{m}$** is drawn *inside* the box. Three things matter about its position:

1. **Centre of the middle 50%.** The line splits the box into two halves: the *lower middle quarter* $[Q_1, \widetilde{m}]$ and the *upper middle quarter* $[\widetilde{m}, Q_3]$.
2. **Robustness.** Like the box, the median is robust --- it is the 50th percentile, unaffected by tail values. Compare to the **mean**, which would be pulled up to $\bar x = 23.9$ k€ by the right tail (see master `g4b_skew`).
3. **Skewness diagnostic from position.** If the line sits *exactly* in the middle of the box $\Rightarrow$ the inner half is symmetric. If it is *closer to $Q_1$* $\Rightarrow$ the upper middle quarter is wider than the lower one $\Rightarrow$ **right skew within the middle 50%**. For pizzerie Sales:
$$\widetilde{m} - Q_1 = 22.4 - 17.9 = 4.5, \qquad Q_3 - \widetilde{m} = 28.6 - 22.4 = 6.2.$$
The median sits **closer to $Q_1$** (lower-half gap $4.5 <$ upper-half gap $6.2$) $\Rightarrow$ **right-skewed** middle.

```r
# Skew diagnostic from the 5-number summary
med <- median(sales); q1 <- quantile(sales,0.25); q3 <- quantile(sales,0.75)
(med - q1) ; (q3 - med)        # 4.5 vs 6.2 -> right skew in middle 50%
abline(v = median(sales), col = "darkblue", lwd = 2)   # median line on boxplot
```

---

### (d) Whiskers --- to the nearest data within $1.5\cdot\text{IQR}$ of the fences

The **whiskers** are line segments extending from the edges of the box to the *most extreme observations that are still inside the fences*. The fences are
$$L \;=\; Q_1 - 1.5\cdot\text{IQR} \;=\; 17.9 - 1.5(10.7) \;=\; \mathbf{1.85} \text{ k€},$$
$$U \;=\; Q_3 + 1.5\cdot\text{IQR} \;=\; 28.6 + 1.5(10.7) \;=\; \mathbf{44.65} \text{ k€}.$$

**Crucial subtlety.** The whisker does **not** reach the fence itself --- it reaches the *nearest actual datum* that lies within $[L, U]$:
$$w_{\text{lo}} \;=\; \min\{x_i : x_i \ge L\}, \qquad w_{\text{hi}} \;=\; \max\{x_i : x_i \le U\}.$$

For pizzerie Sales, $\min = 8.0 \ge L = 1.85$, so the lower whisker reaches $w_{\text{lo}} = 8.0$. On the upper side, the largest pizzeria below $U=44.65$ is around $w_{\text{hi}} \approx 41.5$ k€ (the next observation, $80$ k€, is past $U$ and becomes an *outlier* dot). Whisker lengths:
$$w_{\text{hi}} - Q_3 \approx 41.5 - 28.6 = 12.9 \quad \text{vs}\quad Q_1 - w_{\text{lo}} = 17.9 - 8.0 = 9.9.$$
The **upper whisker is longer** than the lower one $\Rightarrow$ further evidence of **right skew**.

```r
# Fences (1.5 * IQR) and whisker endpoints
q  <- quantile(sales, c(0.25, 0.75)); iqr <- diff(q)
L  <- q[1] - 1.5*iqr                        # lower fence
U  <- q[2] + 1.5*iqr                        # upper fence
# Whisker endpoints (most extreme NON-outlier observations)
min(sales[sales >= L]); max(sales[sales <= U])
```

![Master illustration](statistics/images/master/master_g6b_box_ai.png)

---

### (e) Outliers --- points beyond the fences

Any datum $x_i < L$ or $x_i > U$ is plotted as an **individual dot** (a "fly", in Tukey's original terminology) outside the whiskers. It is *not* part of the whisker; it is a singled-out point.

For pizzerie Sales, the only outliers are on the **upper** side: the handful of shops with monthly turnover in the $50$--$80$ k€ range (e.g. the $\max=80$ k€ shop). There are **no lower outliers** (none below $1.85$ k€).

Counting from the dataset: roughly $4$ shops are flagged as upper outliers --- exactly the right tail that pulls the mean upward and makes $\bar x > \widetilde{m}$. The full theory of outlier flagging (IQR rule, $3\cdot\text{IQR}$ extreme rule, z-score rule) is the subject of master `g6c_outliers`.

```r
sales[sales < L | sales > U]                # flagged outliers
length(sales[sales > U])                    # count of upper outliers
```

---

### (f) Reading the boxplot --- skew, spread, outliers in one glance

A boxplot is most useful as a **shape diagnostic**. Three checks, in order:

| Check | What to look at | Interpretation for pizzerie Sales |
|---|---|---|
| **Median position in box** | Is $\widetilde{m}$ centred, or closer to $Q_1$ / $Q_3$? | Closer to $Q_1$ ($4.5$ vs $6.2$) $\Rightarrow$ **right skew** in central 50% |
| **Whisker length asymmetry** | Is upper whisker longer than lower, or vice versa? | Upper ($12.9$) $>$ lower ($9.9$) $\Rightarrow$ **right skew** in the tails |
| **Outliers** | Are there dots beyond fences, on which side? | $\approx 4$ dots on **upper** side only $\Rightarrow$ a heavy *right* tail |

All three signals point the same way: **`Sales` is right-skewed**. This matches the histogram (master `g1c_hist`) and the gap $\bar x - \widetilde{m} = +1.5$ k€ (master `g4b_skew`).

**Spread comparison rules of thumb.** When comparing two boxplots (e.g. Sales by `District`):

* **Box widths** $=$ IQRs $\Rightarrow$ compare the spread of the *middle 50%* directly.
* **Whisker spans** $=$ range of the *non-outlier* portion of the data.
* **Median lines** $\Rightarrow$ compare *robust* centres (not influenced by outliers).
* **Notches** (if drawn, `boxplot(..., notch = TRUE)`): non-overlapping notches give an informal $\approx 95\%$ "different medians" signal.

```r
# Boxplot by group (e.g. District): compares 5-number summaries side-by-side
boxplot(Sales ~ District, data = pizzerie, horizontal = TRUE,
        col = c("lightsteelblue","lightyellow","mistyrose"))

# Notched boxplot -- informal CI for the median
boxplot(sales, notch = TRUE, horizontal = TRUE)
```

---

**Master take-aways.**

1. **The boxplot is the 5-number summary, drawn.** Box $=[Q_1, Q_3]$, line $=\widetilde{m}$, whiskers $=$ nearest data within $[Q_1-1.5\text{IQR},\;Q_3+1.5\text{IQR}]$, dots $=$ outliers beyond.
2. **The box covers the middle 50%**; its width is the IQR --- a robust spread measure unaffected by tail values.
3. **The whisker does *not* reach the fence**; it reaches the *most extreme actual datum* still inside the fence.
4. **Read skew from two cues simultaneously:** (i) position of the median line in the box, (ii) asymmetry of whisker lengths. Both pointing the same way confirms the direction of skew.
5. **For pizzerie Sales:** $(\min, Q_1, \widetilde{m}, Q_3, \max) = (8.0, 17.9, 22.4, 28.6, 80.0)$ k€, $\text{IQR}=10.7$; median sits closer to $Q_1$, upper whisker is longer, and roughly $4$ shops appear as upper outliers $\Rightarrow$ **clear right-skew with a heavy right tail**.

---

**Linked snippets:** Ex 2.1b (5-number summary of `Sales` --- source of the numbers above); Ex 2.1c (boxplot of `Sales`); Ex 2.1e (reading skew off the boxplot); Ex 2.5a (boxplot of `Age`, the *symmetric* counter-case); Ex 2.5d (boxplot comparison by group); Ex 2.6a2 (side-by-side boxplots of `Sales` by `District`); Ex 2.7e (boxplot interpretation in a past exam); Ex 2.8b (boxplot + skew diagnostic); masters `g6a_quant` (quartiles --- the inputs to the box), `g6c_outliers` (formal outlier rules --- the dots beyond whiskers), `g4b_skew` (mean vs median for `Sales`), `g1c_hist` (histogram view of the same right-tail).
""",
    "images": ["statistics/images/master/master_g6b_box_ai.png"],
}


master_exercises["g6c_outliers"] = {
    "title": "Master Exam — Outliers & extreme values",
    "content": r"""**Setup.** Continuing with the pizzerie dataset ($n=100$ shops, `Sales` in k€), the five-number summary is $(\min, Q_1, \widetilde{m}, Q_3, \max) = (8.0,\; 17.9,\; 22.4,\; 28.6,\; 80.0)$ and the interquartile range is $\text{IQR} = Q_3 - Q_1 = 10.7$ k€. The sample mean and standard deviation are $\bar x = 23.9$ k€ and $s = 11.6$ k€. The questions for an **outlier** are: *Is this point a typo, an unusual real observation, or a glimpse of a heavy tail?* and *What --- if anything --- should we do about it?* This master is the rule-book.

---

### (a) The IQR rule (Tukey, the default in `boxplot()`)

**Definition.** A datum $x_i$ is flagged as a **mild outlier** if it lies beyond the *fences*
$$x_i \;<\; Q_1 - 1.5\cdot\text{IQR} \qquad \textbf{or} \qquad x_i \;>\; Q_3 + 1.5\cdot\text{IQR}.$$

For pizzerie Sales:
$$L \;=\; Q_1 - 1.5\cdot\text{IQR} \;=\; 17.9 - 1.5(10.7) \;=\; \mathbf{1.85} \text{ k€},$$
$$U \;=\; Q_3 + 1.5\cdot\text{IQR} \;=\; 28.6 + 1.5(10.7) \;=\; \mathbf{44.65} \text{ k€}.$$
Observations $x_i < 1.85$ or $x_i > 44.65$ are **mild outliers**. Since $\min = 8.0 > L$, there are no lower outliers. On the upper side, roughly $4$ shops with monthly turnover above $44.65$ k€ are flagged (including the $\max = 80$ k€).

**Why $1.5$?** Tukey's heuristic. Under a Normal distribution one can show that
$$Q_3 + 1.5\cdot\text{IQR} \;\approx\; \mu + 2.7\,\sigma,$$
so the IQR rule flags about $\mathbf{0.7\%}$ of a Normal sample as "mild outliers" --- a comfortable threshold: not so loose that real data is constantly flagged, not so tight that genuine anomalies slip through.

```r
# IQR mild rule (1.5)
sales <- pizzerie$Sales
q     <- quantile(sales, c(0.25, 0.75)); iqr <- diff(q)
L  <- q[1] - 1.5*iqr; U  <- q[2] + 1.5*iqr           # mild fences
mild <- sales[sales < L | sales > U]                  # mild outliers
length(mild)
boxplot(sales, plot = FALSE)$out                      # same numbers via boxplot
```

![Master illustration](statistics/images/master/master_g6c_outliers_ai.png)

---

### (b) The $3\cdot\text{IQR}$ rule --- extreme outliers

Tukey also defined a second, stricter set of fences for **extreme outliers**:
$$x_i \;<\; Q_1 - 3\cdot\text{IQR} \qquad \textbf{or} \qquad x_i \;>\; Q_3 + 3\cdot\text{IQR}.$$

For pizzerie Sales:
$$L_3 \;=\; Q_1 - 3\cdot\text{IQR} \;=\; 17.9 - 3(10.7) \;=\; \mathbf{-14.2} \text{ k€},$$
$$U_3 \;=\; Q_3 + 3\cdot\text{IQR} \;=\; 28.6 + 3(10.7) \;=\; \mathbf{60.7} \text{ k€}.$$
The $\max = 80 > 60.7$ shop is an **extreme** outlier; the others (between $44.65$ and $60.7$) are *mild* only.

Under Normality, $U_3 \approx \mu + 4.7\,\sigma$ --- flagging only about $\mathbf{0.0003\%}$ of Normal observations. Extreme outliers genuinely deserve a second look.

| Rule | Fence (upper) | Flagged for Sales | Normal $\sigma$-equiv. | Normal tail prob. |
|---|---|---:|:-:|:-:|
| Mild (IQR $\times 1.5$) | $44.65$ k€ | $\approx 4$ shops | $\approx 2.7\,\sigma$ | $\approx 0.7\%$ |
| Extreme (IQR $\times 3$) | $60.7$ k€ | $1$ shop ($\max$) | $\approx 4.7\,\sigma$ | $\approx 0.0003\%$ |

```r
# IQR extreme rule (3)
L3 <- q[1] - 3.0*iqr; U3 <- q[2] + 3.0*iqr           # extreme fences
extreme <- sales[sales < L3 | sales > U3]             # extreme outliers
length(extreme)
```

---

### (c) The z-score rule

An alternative, more **parametric** flagging rule transforms each observation to its **standard score**
$$z_i \;=\; \frac{x_i - \bar x}{s}$$
and flags $x_i$ as an outlier when $|z_i|$ exceeds a threshold $k$:

* $\mathbf{|z| > 2}$ --- **loose** rule (about $5\%$ of a Normal sample flagged --- often *too many*);
* $\mathbf{|z| > 3}$ --- **strict** rule (about $0.27\%$ of a Normal sample flagged --- comparable to the IQR mild rule);
* $\mathbf{|z| > 3.5}$ or robust variants for small samples.

For pizzerie Sales with $\bar x = 23.9$, $s = 11.6$:
$$z_{80} = \frac{80 - 23.9}{11.6} = \mathbf{4.84}, \qquad z_{50} \approx \frac{50 - 23.9}{11.6} = 2.25.$$
Under the $|z|>3$ rule, only the $\max=80$ shop is flagged; under $|z|>2$, several borderline shops in the $46$–$60$ range are also flagged.

**Caveat (a critical one).** The z-score rule is **not robust**: $\bar x$ and $s$ are themselves *pulled toward* the outliers being tested! With a single very extreme point, $s$ inflates, $|z|$ shrinks, and the rule can **fail to flag the very point that broke it** --- the *masking problem*. For this reason, the **IQR rule is usually preferred for unknown distributions**, and the z-score rule is used either (i) for approximately Normal data, or (ii) with **robust replacements** $\widetilde{m}$ for $\bar x$ and $\text{MAD}/0.6745$ for $s$ (the *modified z-score*).

| Rule | Threshold | Robustness | Best when |
|---|:-:|:-:|---|
| IQR mild ($1.5\cdot\text{IQR}$) | $\approx 2.7\sigma$ | **High** (quartiles) | Unknown shape, possible skew |
| IQR extreme ($3\cdot\text{IQR}$) | $\approx 4.7\sigma$ | **High** | Want to flag only the worst |
| z-score $|z|>2$ | $2\sigma$ | **Low** ($\bar x$, $s$) | Normal data, conservative flagging |
| z-score $|z|>3$ | $3\sigma$ | **Low** | Normal data, strict flagging |
| Modified z $|z_M|>3.5$ | $\approx 3.5\sigma$ | **High** (med, MAD) | Normal-ish but want robustness |

```r
# z-score rule (non-robust)
z <- (sales - mean(sales)) / sd(sales)
sales[abs(z) > 2]                                     # loose
sales[abs(z) > 3]                                     # strict
max(abs(z))                                           # 4.84 (the 80 k EUR shop)

# Modified z (robust): uses median + MAD
zM <- 0.6745 * (sales - median(sales)) / mad(sales, constant = 1)
sales[abs(zM) > 3.5]                                  # robust outliers
```

---

### (d) Impact on summary statistics --- non-robust vs robust

Outliers warp some statistics dramatically and leave others almost untouched. The pizzerie Sales numbers make the contrast vivid:

**Non-robust statistics (corrupted).** If we *drop* the upper outliers (the $\approx 4$ shops above $44.65$ k€):

| Statistic | With outliers | Without upper outliers | Change |
|---|:-:|:-:|:-:|
| $\bar x$ | $23.9$ k€ | $22.0$ k€ | $\downarrow$ $1.9$ k€ ($-8\%$) |
| $s$ | $11.6$ k€ | $7.4$ k€ | $\downarrow$ $4.2$ k€ ($-36\%$) |
| $s^2$ | $134.6$ | $54.8$ | $\downarrow$ $60\%$ |
| $\max$ | $80.0$ k€ | $\approx 41.5$ k€ | huge |

The **standard deviation** is by far the most sensitive: a single observation at $5\sigma$ above the mean inflates $s^2$ by roughly $(5\sigma)^2/n \approx 25\sigma^2/100 = 0.25\,\sigma^2$ --- a $25\%$ increase from *one point*. The mean shifts proportionally less but still measurably.

**Robust statistics (essentially unchanged).**

| Statistic | With outliers | Without upper outliers | Change |
|---|:-:|:-:|:-:|
| $\widetilde{m}$ (median) | $22.4$ k€ | $22.3$ k€ | $\downarrow$ $0.1$ k€ ($\approx 0$) |
| $Q_1$, $Q_3$ | $17.9, 28.6$ | $17.9, 28.4$ | virtually identical |
| $\text{IQR}$ | $10.7$ | $10.5$ | $\approx 0$ |
| $\text{MAD}$ | $5.4$ | $5.3$ | $\approx 0$ |

This is the **definition of robustness**: an estimator's value depends on the *middle* of the data and is **unaffected by changes in the extremes**, no matter how large. Replacing any of the outliers by $\infty$ would not move $\widetilde{m}$ or $\text{IQR}$ at all (the *breakdown point* of the median is $50\%$, of the mean is $0\%$).

**Practical rule.** When you suspect outliers, *always* report **both**: the *non-robust* $(\bar x, s)$ pair **and** the *robust* $(\widetilde{m}, \text{IQR})$ pair. Large discrepancies between them are themselves diagnostic --- they tell the reader the tails matter.

```r
# Impact: with vs without outliers
clean <- sales[sales >= L & sales <= U]
c(mean(sales),   mean(clean))                         # 23.9 -> 22.0
c(sd(sales),     sd(clean))                           # 11.6 ->  7.4
c(median(sales), median(clean))                       # 22.4 -> 22.3 (robust)
c(IQR(sales),    IQR(clean))                          # 10.7 -> 10.5 (robust)
```

---

### (e) What to do with outliers --- a decision protocol

The single biggest mistake is to **blindly delete** outliers because they are inconvenient. Tukey's advice, still standard, is *investigate first, decide second*. A useful protocol:

1. **Verify the data.** Is the outlier a *data-entry error* (e.g. $800$ k€ typed instead of $80$)? Check the raw source. A *correctable* error is *corrected*, not removed.
2. **Check the units.** A pizzeria reporting $80\,000$ when others report $20\,000$ might be in **a different unit** (annual vs monthly). Investigate before judging.
3. **Ask whether the point is in scope.** Is the $80$ k€ shop *really* a pizzeria, or a *full restaurant* misclassified as pizzeria? If misclassified, **exclude with explanation**.
4. **Robustly summarise.** Report *both* mean/SD and median/IQR. Let the reader see the discrepancy. If the analysis is robust (median, quantile regression), report *its* results.
5. **Consider a model with heavy tails.** Instead of dropping the point, **fit a model that accommodates it** --- e.g. a $t$-distribution for the data, or robust regression (M-estimators). The outlier may be a *real* phenomenon (a flagship pizzeria) that *belongs* in the analysis.
6. **Sensitivity analysis.** Run the analysis *with* and *without* the suspect points. **Report both**. If conclusions are stable, the outlier is irrelevant to the inference; if they change, *that itself* is the finding.
7. **Delete only with documentation.** If you do drop a point, *write down* exactly why (units error, mis-scope, confirmed typo). **Never** delete silently; **never** delete merely because $|z|>2$.

**Anti-rule:** "Drop everything with $|z|>2$ and re-run the analysis." This is the road to fake findings: it shrinks $s$ artificially, then flags *more* points as outliers, in a cascade that ends with a clean-looking but biased dataset.

```r
# Sensitivity report side-by-side (always pair with the decision protocol)
out_summary <- rbind(
  with_out    = c(mean=mean(sales),   sd=sd(sales),   med=median(sales),   IQR=IQR(sales)),
  without_out = c(mean=mean(clean),   sd=sd(clean),   med=median(clean),   IQR=IQR(clean)),
  pct_change  = round(100*(c(mean(clean),sd(clean),median(clean),IQR(clean)) /
                          c(mean(sales),sd(sales),median(sales),IQR(sales)) - 1), 1))
out_summary
```

---

**Master take-aways.**

1. **Two rules to remember:** **IQR** (mild $1.5$, extreme $3$ --- *robust*) and **z-score** ($|z|>2$ or $3$ --- *non-robust*, parametric). For unknown-shape data, prefer IQR; for Normal data with no extreme contamination, z is fine.
2. **The z-score rule masks its own outliers**: a very extreme point inflates $s$, shrinks $|z|$, and can leave the offender undetected. The robust **modified z-score** ($\widetilde{m}, \text{MAD}$) fixes this.
3. **Mean and SD are extremely sensitive; median and IQR are not.** For pizzerie Sales, dropping the $\approx 4$ upper outliers cuts $s$ by $36\%$ but moves the median by $0.1$ k€. Always report **both** summaries when outliers are suspected.
4. **Don't blindly delete.** *Investigate*: typo? unit error? out of scope? Otherwise consider a heavy-tailed model or a robust estimator. **Delete only with documentation**, and **always run sensitivity analyses**.
5. **For pizzerie Sales:** the $1.5\cdot\text{IQR}$ rule flags $\approx 4$ upper outliers above $44.65$ k€; the $3\cdot\text{IQR}$ extreme rule flags only the $\max=80$ k€; the $|z|>3$ rule agrees with the extreme rule ($z_{\max}=4.84$). The right-tail is real --- a few high-revenue pizzerie --- not a data-entry artefact, so they should be **kept** in the analysis but accompanied by **robust** summary statistics.

---

**Linked snippets:** Ex 2.1d (outlier flagging on `Sales` via IQR rule --- the running example); Ex 2.3b (outlier impact on mean vs median, side-by-side); Ex 2.3c (sensitivity check: re-compute with/without outliers); Ex 2.4b (outlier discussion on grouped data); masters `g6a_quant` (quartiles --- the inputs to fences), `g6b_box` (boxplot --- where outliers appear as dots), `g5_disp` (variance vs IQR vs MAD --- robust vs non-robust spread), `g4b_skew` (mean-median gap, which the right tail creates).
""",
    "images": ["statistics/images/master/master_g6c_outliers_ai.png"],
}
