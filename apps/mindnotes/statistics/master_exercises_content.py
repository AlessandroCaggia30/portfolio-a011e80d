# =====================================================================
# Master Exam Ready exercises — ONE consolidated exercise per subtopic.
# Each master exercise uses a single dataset and covers all unique
# subparts asked across the linked snippets, eliminating redundancy.
# Populated by agents (one per subtopic).
# =====================================================================

master_exercises = {}

master_exercises["g13a_ci_one_mean"] = {
    "title": "Master Exam — CI for one mean (σ known z and σ unknown t)",
    "content": r"""## Setup — running dataset for every numeric example below

A market-research firm collects the monthly turnover `Sales` (in €) for a random sample of $n=100$ pizzerias in Milan. The summary statistics are

$$\bar x \;=\; 23\,947, \qquad s \;=\; 8\,200, \qquad n \;=\; 100.$$

Let $\mu$ denote the unknown population mean monthly turnover. We will treat this *same* sample throughout the entry — first under "$\sigma$ **known**" (using a textbook value $\sigma = 8\,000$ €), then under the realistic "$\sigma$ **unknown**, plug in $s$" regime, then for sample-size planning, then for recovering $s$ from raw sums. No new dataset is introduced anywhere below.

<details class="master-subpart" open>
<summary>(a) <strong>Case 1</strong> — One mean with $\sigma$ <em>known</em> (z-interval)</summary>

**Setting.** $X_1,\ldots,X_n$ i.i.d. from a distribution with mean $\mu$ and **known** variance $\sigma^2$. We want a CI for $\mu$.

**Pivot.** Because $\sigma$ is *known*, the standardised sample mean has an exact reference distribution:
- If $X\sim N(\mu,\sigma^2)$: $\displaystyle Z = \frac{\bar X - \mu}{\sigma/\sqrt n} \sim N(0,1)$ **exactly**, any $n$.
- For non-normal $X$: $Z$ is **approximately** $N(0,1)$ by the CLT for $n\gtrsim 30$.

**Deriving the CI.** From $\Pr\bigl(-z_{1-\alpha/2}\le Z \le z_{1-\alpha/2}\bigr)=1-\alpha$, invert for $\mu$:

$$\Pr\!\left(\bar X - z_{1-\alpha/2}\,\tfrac{\sigma}{\sqrt n} \;\le\; \mu \;\le\; \bar X + z_{1-\alpha/2}\,\tfrac{\sigma}{\sqrt n}\right) = 1-\alpha.$$

$$\boxed{\;\;CI_{1-\alpha}(\mu) \;=\; \bar x \;\pm\; z_{1-\alpha/2}\,\frac{\sigma}{\sqrt n}\;\;}\qquad (\sigma \text{ known})$$

**Worked numbers on the pizzeria sample** with the textbook value $\sigma = 8\,000$, $n=100$, $\bar x = 23\,947$, $\alpha = 0.05$:
- Exact SE: $\sigma/\sqrt n = 8\,000/\sqrt{100} = 800$.
- Critical value: $z_{0.975} = 1.960$.
- $ME_{95} = 1.960 \cdot 800 = 1\,568$.
- $CI_{95} = 23\,947 \pm 1\,568 = [22\,379,\; 25\,515]$.

```r
n     <- 100;   xbar <- 23947;   sigma <- 8000;   alpha <- 0.05
SE    <- sigma / sqrt(n);                 SE         # 800  (exact, no plug-in)
zcrit <- qnorm(1 - alpha/2);              zcrit      # 1.960
ME    <- zcrit * SE;                      ME         # 1568
c(xbar - ME, xbar + ME)                              # [22379, 25515]
# Helper used in the course (BAS package):
# CI.mean(Sales, sigma = 8000, conf.level = 0.95, data = pizzerie)
```

**Why "$\sigma$ known" is the simplest row.** No degrees of freedom, no $t$ inflation, no plug-in noise — $z$-quantiles are *the* reference distribution. The same operation extends to a difference of means with known $\sigma$'s (row 4) — see `g13c`.

</details>

<details class="master-subpart">
<summary>(b) <strong>Case 2</strong> — One mean with $\sigma$ <em>unknown</em> (t-interval) — the realistic case</summary>

**Setting.** Same i.i.d. assumption, but $\sigma^2$ is **unknown** and must be estimated from the data via the sample variance $s^2 = \tfrac{1}{n-1}\sum_i(x_i-\bar x)^2$. We want a CI for $\mu$.

**Pivot.** Replacing $\sigma$ by $s$ in the standardised mean produces a *t*-distributed pivot:
- If $X\sim N(\mu,\sigma^2)$: $\displaystyle T = \frac{\bar X - \mu}{S/\sqrt n} \sim t_{n-1}$ **exactly**.
- For non-normal $X$ and $n\gtrsim 30$: $T$ is approximately $N(0,1)$ by Slutsky's theorem ($S\xrightarrow{P}\sigma$ and CLT for $\bar X$). The $t_{n-1}$ and $N(0,1)$ percentiles become numerically indistinguishable once $n$ is in the hundreds (at $n=100$: $t_{0.975,\,99}\approx 1.984$ vs $z_{0.975}=1.960$ — agree to two decimals).

**Deriving the CI.** Identical inversion as before:

$$\boxed{\;\;CI_{1-\alpha}(\mu) \;=\; \bar x \;\pm\; t_{1-\alpha/2,\,n-1}\,\frac{s}{\sqrt n}\;\;}\qquad (\sigma \text{ unknown})$$

The estimated standard error $\widehat{SE}(\bar X) = s/\sqrt n$ is itself random (it has its own sampling variability); the **$t_{n-1}$ critical value is wider than the $z$** by exactly the right amount to compensate, restoring nominal coverage. At large $n$ the inflation is negligible; at small $n$ (e.g. $n=15$) it is $\approx 9\%$ purely from the reliability factor, *on top* of the larger $s/\sqrt n$ from the small sample.

**Worked numbers on the pizzeria sample** with $\bar x = 23\,947$, $s = 8\,200$, $n = 100$, $\alpha = 0.05$:
- Estimated SE: $\widehat{SE} = s/\sqrt n = 8\,200/\sqrt{100} = 820$.
- Critical value: $t_{0.975,\,99} \approx 1.984$.
- $ME_{95} = 1.984 \cdot 820 \approx 1\,627$.
- $CI_{95} = 23\,947 \pm 1\,627 = [22\,320,\;25\,574]$.

The interval is slightly wider than in part (a) because the $t$-critical value is larger than $z$, *and* the plug-in $s = 8\,200$ is larger than the textbook $\sigma = 8\,000$ used in (a).

```r
n     <- 100;   xbar <- 23947;   s <- 8200;   alpha <- 0.05
se_hat <- s / sqrt(n);                    se_hat     # 820  (estimated, plug-in)
tcrit  <- qt(1 - alpha/2, df = n-1);      tcrit      # 1.984
ME     <- tcrit * se_hat;                 ME         # 1627
c(xbar - ME, xbar + ME)                              # [22320, 25574]
# Course helper (BAS package) — sigma argument omitted -> t-interval used:
# CI.mean(Sales, conf.level = 0.95, data = pizzerie)
# Base R equivalent on raw data:
# t.test(pizzerie$Sales)$conf.int
```

**Reading the margin of error backwards.** Because the CI is symmetric around $\bar x$,
$$\bar x \;=\; \tfrac{L+U}{2}, \qquad ME \;=\; \tfrac{U-L}{2}.$$
From a printed CI we can always recover the point estimate and the half-width without raw data — useful in exam questions that give *only* the interval (e.g. ex `6_1b`: from the German developers' CI $[16.91,\,18.29]$, $\bar x_{GER} = 17.60$ and $ME = 0.69$ pop out immediately).

**Effect of changing the confidence level.** $SE$ does not depend on $\alpha$; only the reliability factor does. Going from 95% to 99% at $n=100$: $t_{0.995,\,99}/t_{0.975,\,99} \approx 2.626/1.984 \approx 1.32$ — the CI grows by $\approx 32\%$. Higher confidence ⇒ wider interval, *always*.

**Effect of changing the sample size.** $\widehat{SE} \propto 1/\sqrt n$. Quadruple $n$ ⇒ halve the SE ⇒ halve the CI width. Halve the desired width ⇒ quadruple $n$ — see subpart (c) for the formal sample-size formula.

![Master illustration — one-mean CI with σ known (z) vs σ unknown (t)](statistics/images/master/master_g13a_ai.png)

</details>

<details class="master-subpart">
<summary>(c) Sample-size planning for a target margin of error on $\mu$</summary>

A CI is useful only if it is short enough to discriminate values of $\mu$ that matter. Pre-data, fix a target margin of error $ME^\star$ at confidence $1-\alpha$ and solve

$$ME \;=\; z_{1-\alpha/2}\,\frac{\sigma}{\sqrt n} \;\le\; ME^\star \quad\Longleftrightarrow\quad \boxed{\;\;n \;\ge\; \left(\frac{z_{1-\alpha/2}\,\sigma}{ME^\star}\right)^{2}\;\;}$$

then round **up** to the next integer. Two practical notes:

- $\sigma$ is unknown at the design stage; use a **pilot estimate** $\sigma\approx s$ from a prior sample (the pizzeria sample gives $s = 8\,200$), or a literature value. The plug-in is what makes the formula a *planning approximation* rather than an exact guarantee.
- Using $z$ instead of $t_{n-1}$ is conservative and standard — the $t$ inflation requires knowing $n$, the very thing we are solving for; the difference vanishes once $n\gtrsim 60$.

**Worked numbers on the pizzeria sample.** Suppose we want a 95% CI with half-width $ME^\star = 500$ €, using $\sigma\approx s = 8\,200$:

$$n \;\ge\; \left(\frac{1.96 \cdot 8\,200}{500}\right)^{2} \;=\; (32.14)^{2} \;\approx\; 1\,033.2 \quad\Longrightarrow\quad n = 1\,034.$$

```r
ME_target   <- 500
sigma_pilot <- 8200
n_req <- ceiling( (qnorm(0.975) * sigma_pilot / ME_target)^2 )
n_req                                # 1034
```

**Take-away.** Halving the ME quadruples the required $n$ — the $\sqrt n$ rate is the binding cost of precision. Doubling the confidence multiplier ($z_{0.975}=1.96\to z_{0.995}=2.576$) roughly **triples** $n$ (a factor of $(2.576/1.96)^2 \approx 1.73$).

The proportion-side analogue (worst-case $p(1-p)=0.25$) lives in **`g13b`** — same recipe, different SE.

</details>

<details class="master-subpart">
<summary>(d) Recovering $s$ from raw sums $\sum x_i$ and $\sum x_i^2$</summary>

Several horizontal cells (`5_2a`, `5_2b`, `5_3a`, `5_3b`, `6_13d`) feed the data only as $n$, $\sum x_i$, $\sum x_i^2$ (or as a frequency table). In that case use the **computational formula**

$$\bar x \;=\; \frac{1}{n}\sum_{i=1}^n x_i, \qquad s^2 \;=\; \frac{1}{n-1}\!\left(\sum_{i=1}^n x_i^2 \;-\; n\,\bar x^{\,2}\right), \qquad \widehat{SE}(\bar X)\;=\;\frac{s}{\sqrt n}.$$

For a **frequency table** ($x$-values $x_k$ with frequencies $f_k$, $\sum_k f_k = n$):

$$\bar x \;=\; \frac{1}{n}\sum_k x_k f_k, \qquad \overline{x^2} \;=\; \frac{1}{n}\sum_k x_k^{\,2} f_k, \qquad s^2 \;=\; \frac{n}{n-1}\bigl(\overline{x^2} - \bar x^{\,2}\bigr).$$

The Bessel factor $n/(n-1)$ is the unbiased correction; at $n=100$ it is $1.0101$ — negligible numerically, but conceptually essential.

**Worked example (matches `5_2a`/`5_3a`).** $n=15$, $\sum x_i = 2\,755$, $\sum x_i^2 = 585\,203$:
- $\bar x = 2755/15 = 183.667$.
- $s^2 = \tfrac{1}{14}(585\,203 - 15\cdot 183.667^2) \approx 5\,657.24$, so $s \approx 75.21$.
- $\widehat{SE} = 75.21/\sqrt{15} \approx 19.42$.
- 95% $t$-CI: $183.667 \pm t_{0.975,\,14}\cdot 19.42 = 183.667 \pm 2.145\cdot 19.42 \approx [142.0,\;225.3]$.

```r
n      <- 15
sum_x  <- 2755
sum_x2 <- 585203
xbar   <- sum_x / n;                            xbar      # 183.667
s2     <- (sum_x2 - n*xbar^2) / (n - 1);        s2        # 5657.24
s_hat  <- sqrt(s2);                             s_hat     # 75.21
se_hat <- s_hat / sqrt(n);                      se_hat    # 19.42
tcrit  <- qt(0.975, df = n - 1);                tcrit     # 2.145
xbar + c(-1, 1) * tcrit * se_hat                          # [141.99, 225.34]
```

If instead the problem states $\sigma^2$ as **known** (as in `5_2a`/`5_3a` part (a) with $\sigma^2 = 6\,500$), do **not** compute $s^2$ — use $\sigma/\sqrt n$ directly and the $z$-CI of subpart (a).

</details>

---

**Summary.** g13a covers the **two one-mean rows** of the master case table: $\sigma$ known ⇒ exact $z$-CI with $\sigma/\sqrt n$; $\sigma$ unknown ⇒ $t_{n-1}$-CI with the plug-in $s/\sqrt n$. The two formulas differ only in the SE (exact vs plug-in) and in the critical-value family ($z$ vs $t_{n-1}$), and converge at large $n$. The two auxiliary tools the row's horizontal cells repeatedly demand are **sample-size planning** ($n\ge (z\sigma/ME^\star)^2$) and **recovery of $s$ from raw sums**. Every other G13 row reuses the *same* three-slot template $\widehat\theta \pm c\cdot\widehat{SE}$ — only $(\widehat\theta,\widehat{SE},c,df)$ change. Continue to **`g13b`** for the one-proportion row, **`g13c`** for two-means, **`g13d`** for two-proportions, **`g13e`** for paired data, **`g13f`** for the underlying unbiased estimators.
""",
    "images": ["statistics/images/master/master_g13a_ai.png"]
}

master_exercises["g14a_one_sample"] = {
    "title": "Master Exam — One-sample tests (mean σ-known z, mean σ-unknown t, one-proportion z)",
    "content": r"""## Setup — running dataset for every numeric example below

A job agency tracks $n=47$ workers (`NewHired`) and records `Weeks` = time (in weeks) to find a new job. The sample summary is

$$\bar x \;=\; 40.1915, \qquad s \;=\; 17.2206, \qquad \widehat{\rm SE}(\bar X) \;=\; s/\sqrt{47} \;=\; 2.5119, \qquad n \;=\; 47.$$

A side count from the *same* sample: **7 of 47** workers took **more than 52 weeks** to find a job, so the binary indicator $Y_i = \mathbb{1}\{\text{Weeks}_i > 52\}$ has

$$\hat p \;=\; X/n \;=\; 7/47 \;\approx\; 0.1489, \qquad n=47.$$

We will treat this *same* sample throughout the entry — first as a one-mean problem with $\sigma$ pretended **known** at the pilot value $\sigma = 17.0$ (Case 1, $z$-test), then realistically as $\sigma$ **unknown** plug in $s$ (Case 2, $t$-test), then for the *proportion* thread on the same workers (Case 3, $z$-test under $H_0$). Targets to test: $\mu_0 = 45$ weeks (mean) and $p_0 = 0.10$ (proportion). No new dataset is introduced anywhere below.

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) <strong>Case 1</strong> — One-mean test with $\sigma$ <em>known</em> ($z$-test)</summary>

**Setting.** $X_1,\ldots,X_n$ i.i.d. with mean $\mu$ and **known** variance $\sigma^2$. We test $H_0:\mu=\mu_0$.

**Why this row matters at the exam.** Showed up in `exam_g2_2026_2a` (campaign price, $\sigma=300$ known, $H_1:\mu>850$) and `exam_sep_2025_2b`/`2c` (Wald $z$ on a coefficient with a directly-given SE). Whenever the problem statement *literally* hands you $\sigma$ (or the SE), this is the row to pick — no df, no $t$-inflation.

**Pivot.** Because $\sigma$ is *known*, the standardised mean has an exact reference distribution under $H_0$:
- If $X\sim N(\mu,\sigma^2)$: $\displaystyle Z = \frac{\bar X - \mu_0}{\sigma/\sqrt n} \overset{H_0}{\sim} \mathcal N(0,1)$ **exactly**, any $n$.
- For non-normal $X$: $Z$ is **approximately** $\mathcal N(0,1)$ by the CLT for $n\gtrsim 30$.

**Test statistic and decision (row 1).**

$$\boxed{\;\;Z \;=\; \frac{\bar X - \mu_0}{\sigma/\sqrt n}, \qquad \text{reject } H_0:\mu=\mu_0 \iff |Z|>z_{1-\alpha/2}\;\;(\text{two-sided}).\;\;}$$

**Worked numbers on the NewHired running sample** with the pilot textbook value $\sigma = 17.0$ (used here to illustrate the $\sigma$-known mechanics; the realistic $\sigma$-unknown variant is in (b)). Take the directional research claim $H_1:\mu<45$ (mirrors `7_1a`):

- Exact SE: $\sigma/\sqrt n = 17.0/\sqrt{47} \approx 2.4795$.
- Realisation: $z_{\rm obs} = (40.1915 - 45)/2.4795 \approx -1.9393$.
- Lower-tail critical values: $-z_{0.95}=-1.6449$ (5%), $-z_{0.99}=-2.3263$ (1%).
- One-sided $p$-value: $p = \Phi(-1.9393) \approx 0.0262$.

**Decision.** $z_{\rm obs} = -1.939 < -1.6449$ ⇒ **reject** $H_0$ at $\alpha = 0.05$; but $-1.939 > -2.3263$ ⇒ **retain** at $\alpha = 0.01$. Same verdict from the $p$-value: $0.01 < 0.0262 < 0.05$.

```r
# (a) Case 1 — z-test, sigma known
xbar <- 40.1915; sigma <- 17.0; n <- 47; mu0 <- 45
SE   <- sigma/sqrt(n);          SE             # 2.4795 (exact, no plug-in)
z    <- (xbar - mu0)/SE;        z              # -1.9393
qnorm(0.05); qnorm(0.01)                       # -1.6449, -2.3263 (lower-tail crits)
pnorm(z)                                       # 0.0262 (one-sided p-value)
# Course helper (BAS package): sigma argument given -> z-test used
TEST.mean(Weeks, mu0 = 45, sigma = 17.0, alternative = "less", data = NewHired)
```

**Why Case 1 is the simplest row.** No degrees of freedom, no $t$-inflation, no plug-in noise — $z$-quantiles are *the* reference distribution. The same operation extends to a difference of means with known $\sigma$'s (row 4) — see `g14b` (a1).

![Master illustration — one-sample tests: z (σ known), t (σ unknown), one-proportion](statistics/images/master/master_g14a_ai.png)

</details>

<details class="master-subpart">
<summary>(b) <strong>Case 2</strong> — One-mean test with $\sigma$ <em>unknown</em> ($t$-test) — the realistic case (Ex 7.1a, Ex 7.8a)</summary>

**Setting.** Same i.i.d. assumption, but $\sigma^2$ is **unknown** and must be estimated by $s^2 = \tfrac{1}{n-1}\sum_i (x_i-\bar x)^2$.

**Pivot.** Replacing $\sigma$ by $s$ in the standardised mean produces a $t$-distributed pivot under $H_0$:
- If $X\sim N(\mu,\sigma^2)$: $\displaystyle T = \frac{\bar X - \mu_0}{S/\sqrt n} \overset{H_0}{\sim} t_{n-1}$ **exactly**.
- For non-normal $X$ and $n\gtrsim 30$: $T$ is approximately $\mathcal N(0,1)$ by Slutsky's theorem ($S\xrightarrow{P}\sigma$ and CLT for $\bar X$). The $t_{n-1}$ and $\mathcal N(0,1)$ percentiles become numerically indistinguishable once $n$ is in the hundreds (at $n=47$: $-t_{0.95,46}=-1.679$ vs $-z_{0.95}=-1.6449$ — agree to two decimals).

**Test statistic and decision (row 2).**

$$\boxed{\;\;T \;=\; \frac{\bar X - \mu_0}{s/\sqrt n}, \qquad \text{reject } H_0:\mu=\mu_0 \iff |T|>t_{1-\alpha/2,\,n-1}\;\;(\text{two-sided}).\;\;}$$

The estimated standard error $\widehat{\rm SE}(\bar X) = s/\sqrt n$ is itself random (it has its own sampling variability); the **$t_{n-1}$ critical value is wider than $z$** by exactly the right amount to compensate, restoring nominal Type-I rate. At large $n$ the inflation is negligible; at small $n$ (e.g. $n=15$) it is $\approx 9\%$ from the reliability factor alone.

**Worked numbers on the NewHired sample, $H_1:\mu<45$ (Ex 7.1a).** $\bar x = 40.1915$, $s = 17.2206$, $n=47$, $\widehat{\rm SE} = 2.5119$:

- Realisation: $t_{\rm obs} = (40.1915 - 45)/2.5119 \approx -1.9143$ on $t_{46}$.
- Lower-tail critical values: $-t_{0.95,46} = -1.679$ (5%), $-t_{0.99,46} = -2.410$ (1%).
- One-sided $p$-value: $p = P(t_{46}\le -1.9143) \approx 0.0309$.

**Decision.** $t_{\rm obs} = -1.914 < -1.679$ ⇒ **reject** $H_0$ at $\alpha = 0.05$; $-1.914 > -2.410$ ⇒ **retain** at $\alpha = 0.01$. The $p$-value $0.0309$ sits between the two levels.

**Reading the verdict from a CI (duality cross-check).** A two-sided 95% $t$-CI for $\mu$ is $\bar x \pm t_{0.975,46}\cdot s/\sqrt n = 40.19\pm 2.013\cdot 2.5119 = [35.13,\;45.25]$. Since $\mu_0 = 45$ sits **inside** the 95% CI, the *two-sided* test at $\alpha = 0.05$ does **not** reject — but the *one-sided* lower-tail test does (the one-sided rejection region is asymmetric, all mass on the left). Direction of $H_1$ matters.

```r
# (b) Case 2 — t-test, sigma unknown (Ex 7.1a)
xbar <- 40.1915; s <- 17.2206; n <- 47; mu0 <- 45
SE   <- s/sqrt(n);                SE          # 2.5119 (plug-in)
t    <- (xbar - mu0)/SE;          t           # -1.9143
qt(0.05, df = n-1); qt(0.01, df = n-1)        # -1.679, -2.410
pt(t, df = n-1)                                # 0.0309 (one-sided p-value)
# Course helper: sigma argument omitted -> t-test used
TEST.mean(Weeks, mu0 = 45, alternative = "less", data = NewHired)
# Base R on raw data:
# t.test(NewHired$Weeks, mu = 45, alternative = "less")
```

**Second worked example — large $n$ (Ex 7.8a, `DS$Children`).** The sales manager will **drop baby products** if the mean number of children per customer does not exceed 1.5. Same template, but $n=750$ ⇒ CLT applies trivially and $t_{749}$ is indistinguishable from $\mathcal N(0,1)$:

- $\bar x = 0.9213$, $s = 1.0640$, $\widehat{\rm SE} = 0.03885$, $\mu_0 = 1.5$.
- $H_0:\mu\ge 1.5$ vs $H_1:\mu<1.5$ (the "stop selling" claim is in $H_1$).
- $t_{\rm obs} = (0.9213 - 1.5)/0.03885 \approx -14.91$ on $t_{749}$, $p < 10^{-4}$.
- CI cross-check: 95% $t$-CI for $\mu$ is $[0.845,\,0.997]$, which **does not contain** $1.5$ ⇒ reject the two-sided $H_0$, *a fortiori* the one-sided $H_0:\mu\ge 1.5$ — drop the baby-products line.

```r
# (b-bis) Case 2 on DS$Children — Ex 7.8a, large n
xbar <- 0.9213; s <- 1.0640; n <- 750; mu0 <- 1.5
t    <- (xbar - mu0)/(s/sqrt(n));  t           # -14.91
pt(t, df = n-1)                                # < 1e-4
CI.mean(Children, conf.level = 0.95, data = DS)  # [0.845, 0.997] -> misses 1.5
TEST.mean(Children, mu0 = 1.5, alternative = "less", data = DS)
```

**p-value definition.** $p$ = probability, **under $H_0$**, of a test statistic at least as extreme (in the direction of $H_1$) as the realised one. Here for `7_8a` it is the area to the left of $-14.91$ under $t_{749}$. A small $p$ means the observed value is surprising under $H_0$; a *large* $p$ never means "$H_0$ is true", only that the data lack strength to reject at the chosen $\alpha$.

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (c) <strong>Case 3</strong> — One-proportion test ($z$-test with SE under $H_0$) (Ex 7.1c)</summary>

**Setting.** $Y_1,\ldots,Y_n$ i.i.d. Bernoulli$(p)$, $X = \sum_i Y_i \sim \mathrm{Bin}(n,p)$, $\hat p = X/n$. Test $H_0:p=p_0$.

**Validity.** Large-$n$ CLT: requires $np_0\ge 5$ and $n(1-p_0)\ge 5$ — note: under $H_0$ the check uses $p_0$, not $\hat p$. If it fails, use the exact binomial $p$-value via `binom.test` instead (the small-$n$ analogue of Clopper–Pearson on the CI side; see `g13b` (c)).

**Pivot.** Under $H_0:p=p_0$, the variance of $\hat p$ is **exactly known** — namely $p_0(1-p_0)/n$ — because conditioning on $H_0$ pins it down. The standardised pivot is

$$Z \;=\; \frac{\hat p - p_0}{\sqrt{p_0(1-p_0)/n}} \;\overset{H_0}{\dot\sim}\; \mathcal N(0,1).$$

**Test statistic and decision (row 3).**

$$\boxed{\;\;Z \;=\; \frac{\hat p - p_0}{\sqrt{p_0(1-p_0)/n}}, \qquad \text{reject } H_0:p=p_0 \iff |Z|>z_{1-\alpha/2}\;\;(\text{two-sided}).\;\;}$$

**Critical SE distinction — boxed because it is the #1 G13↔G14 trap.**
$$\boxed{\;\;\widehat{\rm SE}_{\rm TEST} \;=\; \sqrt{\tfrac{p_0(1-p_0)}{n}} \quad\ne\quad \widehat{\rm SE}_{\rm CI} \;=\; \sqrt{\tfrac{\hat p(1-\hat p)}{n}}\;\;}$$
The **test** plugs in $p_0$ in the SE (sharpens the null distribution — variance is known under $H_0$); the **Wald CI** plugs in $\hat p$ (the only feasible choice when the truth is unknown). The two SEs coincide *only* when $\hat p = p_0$; otherwise the CI ⇄ test duality is **not** an exact identity for proportions (it *is* for means with known $\sigma$). Cross-reference: `g13b` (d) — same warning from the CI side. For the two-proportion analogue (CI unpooled vs test pooled), see `g13d` and `g14b` row 7.

**Worked numbers on the NewHired sample, $H_1:p>0.10$ (Ex 7.1c).** A worker claims that the proportion of agency-relying workers who **struggle more than one year** ($>52$ weeks) exceeds 10%.

- $\hat p = 7/47 \approx 0.1489$, $p_0 = 0.10$, $n = 47$.
- CLT validity (under $H_0$): $np_0 = 4.7$ and $n(1-p_0) = 42.3$ — the first is just under 5, borderline; the standard course convention is still to apply the $z$-test (the textbook reports it without comment, see Ex 7.1c).
- $\widehat{\rm SE}_0 = \sqrt{0.10\cdot 0.90/47} \approx 0.04376$.
- $z_{\rm obs} = (0.1489 - 0.10)/0.04376 \approx 1.12$.
- Upper-tail critical values: $z_{0.95} = 1.6449$ (5%), $z_{0.99} = 2.326$ (1%).
- One-sided $p$-value: $p = 1-\Phi(1.12) \approx 0.13$.

**Decision.** $z_{\rm obs} = 1.12 < 1.6449$ ⇒ **do not reject** at $\alpha = 0.05$ (a fortiori at 1%). $p = 0.13 > 0.05$ — same verdict. **Conclusion.** Insufficient evidence that more than 10% of agency-relying workers struggle over a year; the worker's claim is not statistically supported.

```r
# (c) Case 3 — one-proportion z-test, upper tail, p0 = 0.10 (Ex 7.1c)
phat <- 7/47; p0 <- 0.10; n <- 47
SE0  <- sqrt(p0*(1-p0)/n);        SE0           # 0.04376  (NOTE: uses p0, not phat!)
z    <- (phat - p0)/SE0;          z              # 1.12
qnorm(0.95); qnorm(0.99)                         # 1.6449, 2.326
1 - pnorm(z)                                     # 0.13 (one-sided p-value)
# Course helper:
TEST.prop(Weeks > 52, p0 = 0.10, alternative = "greater", data = NewHired)
# Base R: large-sample two-sided test without continuity correction
prop.test(x = 7, n = 47, p = 0.10, alternative = "greater", correct = FALSE)
# Small-n / boundary fallback (exact binomial):
# binom.test(x = 7, n = 47, p = 0.10, alternative = "greater")
```

**Why the CI built from $\hat p$ may give a *different* numerical verdict than the test.** The 95% Wald CI for $p$ from the same data is $\hat p \pm z_{0.975}\sqrt{\hat p(1-\hat p)/n} = 0.1489 \pm 1.96\sqrt{0.1267/47} = 0.1489\pm 0.1017 = [0.047,\,0.251]$ — contains $p_0 = 0.10$, so the two-sided CI does *not* reject at 5%. The one-sided test's verdict (also non-reject) agrees here, but in problems where $\hat p$ is far from $p_0$ the two SE formulas can drift apart enough to disagree on the borderline (see `exam_g1_2026_1b` — uses the CI ⇄ test duality at 99% for $H_0:p=0.3$).

</details>

<details class="master-subpart">
<summary>(d) One-sided vs two-sided — rejection regions and p-value formulas</summary>

The direction of $H_1$ must be pre-specified from subject-matter knowledge, never picked after seeing the data. The mini-table below maps every $H_1$ shape to its rejection region and $p$-value formula on the *same* observed statistic $t_{\rm obs}$:

| $H_1$ | Rejection region | $p$-value | Mass in $\alpha$ |
|---|---|---|---|
| $\theta \ne \theta_0$ (two-sided) | $\{|T|>c_{1-\alpha/2}\}$ | $p = 2\,\Pr(T\ge|t_{\rm obs}|)$ | split $\alpha/2$ each tail |
| $\theta > \theta_0$ (one-sided upper) | $\{T>c_{1-\alpha}\}$ | $p = \Pr(T\ge t_{\rm obs})$ | all $\alpha$ in right tail |
| $\theta < \theta_0$ (one-sided lower) | $\{T<-c_{1-\alpha}\}$ | $p = \Pr(T\le t_{\rm obs})$ | all $\alpha$ in left tail |

**Example on the NewHired $t$-test of (b).** Same $t_{\rm obs} = -1.9143$, but switch $H_1$ to two-sided ($H_1:\mu\ne 45$):
- $\alpha = 0.05$ two-sided RR: $\{|T| > t_{0.975,46}\} = \{|T| > 2.013\}$. $|{-1.9143}| = 1.91 < 2.013$ ⇒ **do not reject**.
- Two-sided $p$-value: $p_2 = 2\cdot 0.0309 = 0.0618 > 0.05$. Same retain verdict.

Doubling the $p$-value flips the 5%-level decision. **The same data are significant one-sided but not two-sided.** This is exactly why the direction of $H_1$ must be fixed *before* the data are seen — picking the one-sided direction after observing $\bar x < \mu_0$ is "p-hacking" and inflates the true Type-I rate from the nominal 5% to 10%.

```r
qt(0.975, df = 46)                              # 2.013 (two-sided crit)
2 * pt(-abs(-1.9143), df = 46)                  # 0.0618 (two-sided p)
```

</details>

<details class="master-subpart">
<summary>(e) Type-I, Type-II, and the size–power trade-off (vocabulary, full treatment in g14e)</summary>

Every test makes one of two correct decisions and one of two errors:

| Truth ↓ \\ Decision → | Retain $H_0$ | Reject $H_0$ |
|---|---|---|
| $H_0$ true | correct (prob $1-\alpha$) | **Type I error** (prob $\alpha$ at the boundary) |
| $H_0$ false (true $\theta=\theta_\star$) | **Type II error** (prob $\beta(\theta_\star)$) | correct (prob $1-\beta(\theta_\star)$ = **power**) |

- **$\alpha$** (significance level, size, Type-I rate) = max probability of rejecting $H_0$ when it is true. On a composite $H_0:\mu\ge\mu_0$ the max is attained at the boundary $\mu=\mu_0$.
- **$\beta(\theta_\star)$** (Type-II rate at the true value $\theta_\star\in H_1$) = probability of retaining $H_0$ when $\theta=\theta_\star$. Depends on $\theta_\star$, $\alpha$, $n$, $\sigma$.
- **Power** $= 1-\beta(\theta_\star)$ = probability of correctly rejecting $H_0$ when $\theta=\theta_\star$.

**Trade-off.** At fixed $n$ and $\sigma$, shrinking $\alpha$ (smaller false-positive rate) *automatically* raises $\beta$ (more false-negatives) ⇒ shrinks power. The only way to raise power *without* inflating $\alpha$ is to raise $n$ (or shrink $\sigma$, or pick a more extreme $\theta_\star$). Full numeric treatment + power-curve plots live in **`g14e`** (which is the proper home for `7_1b`); here we only define the vocabulary used by every other G14 entry.

</details>

<details class="master-subpart">
<summary>(f) Cross-references — where each one-sample test reappears downstream</summary>

- **Two-mean tests (rows 4–6)** — `g14b` (a1)/(a2)/(a3): same template, $\widehat\theta = \bar X_A - \bar X_B$ and three SE choices (known $\sigma$'s, pooled $s_p$, Welch). Pooled vs Welch picked by Levene (`g13c` Part 9, re-used by `g14b`).
- **Two-proportion test (row 7)** — `g14b` (b): same row-3 template, but with the **pooled** SE $\sqrt{\hat p_{\rm pool}(1-\hat p_{\rm pool})(1/n_A+1/n_B)}$. Mirror the boxed SE warning of (c).
- **Paired test (row 8)** — `g14c`: row-2 template applied to differences $d_i = x_i - y_i$. Reduces the paired problem to the one-sample $t$ of (b).
- **$\chi^2$ tests (row 9)** — `g14d`: GoF and independence, right-tail only, on the $\chi^2_{\rm df}$ reference.
- **Power & sample-size (row 10)** — `g14e`: keeps the framework constant and answers "what is the chance of detecting a true effect $\theta_\star$ at $\alpha = 0.05$ with this $n$?". Owns `7_1b`.
- **CI counterparts (G13 mirror).** Case 1 ⇄ `g13a` (a) — one-mean CI, $\sigma$ known, $z$-interval. Case 2 ⇄ `g13a` (b) — one-mean CI, $\sigma$ unknown, $t$-interval. Case 3 ⇄ `g13b` (a) — one-proportion Wald CI. The CI ⇄ test duality is exact for cases 1 and 2; the SE distinction in (c) makes it only approximate for case 3.
- **Underlying unbiased estimators.** $\bar X$, $\hat p$, $S^2$ are derived once in `g13f`. Every $\widehat\theta - \theta_0$ in the rows above is an unbiased estimator minus its null value.

</details>

---

### Side-by-side summary of the three cases on the NewHired sample

| Case | Row | $H_1$ | Stat formula | $z_{\rm obs}/t_{\rm obs}$ | $p$-value (one-sided) | @ 5% | @ 1% |
|---|---|---|---|---|---|---|---|
| (a) | 1 ($\mu$, $\sigma$ known) | $\mu<45$ | $(\bar x-45)/(\sigma/\sqrt n)$ | $-1.939$ | $0.0262$ | reject | retain |
| (b) | 2 ($\mu$, $\sigma$ unknown) | $\mu<45$ | $(\bar x-45)/(s/\sqrt n)$ | $-1.914$ | $0.0309$ | reject | retain |
| (c) | 3 ($p$, $H_0:p=p_0$) | $p>0.10$ | $(\hat p - p_0)/\sqrt{p_0(1-p_0)/n}$ | $1.12$ | $0.13$ | retain | retain |
| (b-bis) | 2 — Ex 7.8a | $\mu<1.5$ | $(\bar x-1.5)/(s/\sqrt n)$ | $-14.91$ | $<10^{-4}$ | reject | reject |

**Summary.** g14a covers the **three one-sample rows** of the universal test table. Case 1 (σ known) ⇒ exact $z$-test with $\sigma/\sqrt n$; Case 2 (σ unknown) ⇒ $t_{n-1}$-test with the plug-in $s/\sqrt n$; Case 3 (one proportion) ⇒ $z$-test with the SE evaluated **under $H_0$** as $\sqrt{p_0(1-p_0)/n}$ — *not* the CI plug-in $\sqrt{\hat p(1-\hat p)/n}$. Every other G14 row reuses the *same* three-slot template $T = (\widehat\theta - \theta_0)/\widehat{\rm SE}_{H_0}$ — only $(\widehat\theta, \theta_0, \widehat{\rm SE}_{H_0}, \text{null distribution})$ change. Continue to **`g14b`** for two-sample tests, **`g14c`** for paired, **`g14d`** for $\chi^2$, **`g14e`** for power & sample-size.
""",
    "images": ["statistics/images/master/master_g14a_ai.png"],
}

# =====================================================================
# g15a_simple_reg — Simple regression: estimation, R^2, slope test
# Consolidates: ex8.1a, ex8.1b, ex8.2a, ex8.3a, ex8.5a, ex8.8a
# Dataset: NewHired (Weeks ~ Age), n = 47
# =====================================================================
master_exercises["g15a_simple_reg"] = {
    "title": "Master Exam — Simple regression (topic anchor for G15): OLS, $R^2$, slope test/CI on NewHired (Weeks ~ Age)",
    "content": r"""## Setup — running dataset for every numeric example below

A job agency tracks $n=47$ workers who managed to find a new job. For each worker we record two variables:

- $X = \text{Age}$ (years),
- $Y = \text{Weeks}$ (weeks needed to find a new job).

Sample summary statistics (used throughout *every* subpart below — no new dataset is introduced anywhere in this entry, and the *same* numbers will be referenced by `g15b` (prediction at $x_0$) and re-used as the simple-regression benchmark by `g15c`):

$$\bar x \;=\; 38.617, \quad \bar y \;=\; 45.745, \quad s^2_x \;=\; 88.246, \quad s^2_y \;=\; 631.589, \quad s_{xy} \;=\; 149.110, \quad r_{xy} \;=\; 0.6315.$$

(Consistency check: $r_{xy} = s_{xy}/\sqrt{s^2_x s^2_y} = 149.110/\sqrt{88.246\cdot 631.589} = 0.6315$ ✓.)

Postulated model (will be reused verbatim by `g15b`, then generalised to $p\ge 2$ by `g15c`):

$$\boxed{\;\;Y_i \;=\; \beta_0 \;+\; \beta_1 X_i \;+\; \varepsilon_i,\qquad \varepsilon_i \overset{\rm iid}{\sim} \mathcal N(0,\sigma^2_\varepsilon),\qquad i = 1,\dots,n=47.\;\;}$$

```r
n      <- 47
xbar   <- 38.617;  ybar  <- 45.745
s2_x   <- 88.246;  s2_y  <- 631.589
s_xy   <- 149.110; r_xy  <- 0.6315
```

Round to 4 decimals throughout.

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span><span class="tag tag-4plus">≥4 ex</span> (a) <strong>Model + OLS estimator</strong> — closed-form derivation (rows 1, 2 of master table; Ex 8.1a, 8.2a, 8.3a, 8.5a, 8.8a, `exam_sep_2024_3a`)</summary>

**OLS criterion.** The OLS estimators are defined as the minimisers of the residual sum of squares (RSS):

$$(\hat\beta_0,\hat\beta_1) \;=\; \arg\min_{\beta_0,\beta_1}\sum_{i=1}^{n}\bigl(y_i-\beta_0-\beta_1 x_i\bigr)^2.$$

**First-order conditions (normal equations)** ⇒ closed-form solution:

$$\boxed{\;\;\hat\beta_1 \;=\; \frac{s_{xy}}{s^2_x} \;=\; r_{xy}\cdot\frac{s_y}{s_x}, \qquad \hat\beta_0 \;=\; \bar y - \hat\beta_1\bar x.\;\;}$$

The intercept formula encodes that the OLS line **always passes through the centre of mass** $(\bar x,\bar y)$. No distributional assumption on $\varepsilon$ is needed for these estimators to be defined or unbiased — Gauss–Markov requires only zero-mean, homoscedastic, uncorrelated errors. (Normality of $\varepsilon$ is needed *only* for the **distribution** of $\hat\beta_j$, hence for the $t$-test in (f) and the CIs in (g).)

**Worked numbers on NewHired.** Plug the sample moments into the formulas:

$$\hat\beta_1 \;=\; \frac{149.110}{88.246} \;=\; 1.6898,\qquad \hat\beta_0 \;=\; 45.745 \;-\; 1.6898\cdot 38.617 \;=\; -19.5262.$$

**Estimated regression line:**

$$\boxed{\;\;\widehat{\text{Weeks}} \;=\; -19.5262 \;+\; 1.6898\cdot\text{Age}.\;\;}$$

**Slope interpretation (the *ceteris-paribus* reading of $\hat\beta_1$).** $\hat\beta_1 = 1.6898$ means a $+1$-year increase in `Age` is associated, **on average**, with $\approx 1.69$ *additional* weeks needed to find a new job. Sign positive ⇒ older agency-relying workers tend to need *longer* job searches. This *is* the answer pattern for `exam_sep_2024_3a` (interpret $\hat\beta_2 = 7.84$ for `Account_length`) — only the dataset, the predictor name, and the numeric value of $\hat\beta_j$ change.

**Intercept caveat.** $\hat\beta_0 = -19.53$ is **not** economically meaningful on its own: it corresponds to $\text{Age}=0$, which is far outside the observed range; mathematically it is allowed to be negative even though Weeks $\ge 0$ in reality.

**R one-liner.**

```r
mod <- lm(Weeks ~ Age, data=NewHired)
summary(mod)        # b0, b1, t-stats, R^2, F-stat -- read everything off here
# Closed-form check from summary stats (no raw data):
b1 <- s_xy/s2_x;          b1                # 1.6898
b0 <- ybar - b1*xbar;     b0                # -19.5262
```

![Master illustration — OLS line on NewHired with slope/intercept boxed](statistics/images/master/master_g15a_ai.png)

</details>

---

<details class="master-subpart">
<summary>(b) <strong>Residual variance $\hat\sigma^2$</strong> — variance decomposition and the "Residual standard error" line of `summary(mod)` (row 2 of master table; Ex 8.1b, 8.3a, 8.5a, 8.8a)</summary>

**Variance decomposition.** Under OLS the total variability of $y$ splits orthogonally into an *explained* part (movement along the fitted line) and a *residual* part (vertical distance from the line):

$$\underbrace{\sum_i (y_i-\bar y)^2}_{SST}\;=\;\underbrace{\sum_i(\hat y_i-\bar y)^2}_{SSR\,(\text{explained})}\;+\;\underbrace{\sum_i(y_i-\hat y_i)^2}_{SSE\,(\text{residual})}.$$

**Residual variance.** $\hat\sigma^2_\varepsilon$ is the *unbiased* estimator of the error variance $\sigma^2_\varepsilon$ (Gauss–Markov: divide by $n-p-1 = n-2$ in simple regression to absorb the 2 df burned by estimating $\beta_0$ and $\beta_1$):

$$\boxed{\;\;\hat\sigma^2_\varepsilon \;=\; \frac{SSE}{n-p-1} \;=\; \frac{SSE}{n-2} \;\;\text{(simple)},\qquad s_\varepsilon \;=\; \sqrt{\hat\sigma^2_\varepsilon}.\;\;}$$

$s_\varepsilon$ is what `summary(mod)` prints as *"Residual standard error"* — the typical vertical distance between an observed $y_i$ and the fitted line. It is the **irreducible noise scale** that feeds into every SE downstream (row 1) and into the prediction interval (row 7).

**Worked numbers on NewHired.**

$$SST \;=\; (n-1)\,s^2_y \;=\; 46\cdot 631.589 \;=\; 29{,}053.1,$$
$$SSE \;=\; (1-R^2)\,SST \;=\; 0.6012\cdot 29{,}053.1 \;=\; 17{,}466.7,\qquad SSR \;=\; R^2\cdot SST \;=\; 11{,}586.4.$$
$$\hat\sigma^2_\varepsilon \;=\; \frac{17{,}466.7}{45} \;=\; 388.15,\qquad s_\varepsilon \;=\; \sqrt{388.15} \;=\; 19.70 \text{ weeks}.$$

So the typical vertical distance between an observed `Weeks` and the OLS line is $\approx 19.7$ weeks — quite large relative to $\bar y = 45.7$, reflecting the moderate fit ($R^2 \approx 0.40$).

```r
SST  <- (n-1)*s2_y;        SST              # 29053.1
R2   <- r_xy^2
SSR  <- R2*SST;  SSE <- SST - SSR           # 11586.4 ; 17466.7
s2_e <- SSE/(n-2);  s_e <- sqrt(s2_e)       # 388.15 ; 19.70
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-4plus">≥4 ex</span> (c) <strong>$R^2$ and the correlation link</strong> — proportion of variance explained (row 3 of master table; Ex 8.1b, 8.3a, 8.5a, 8.8a)</summary>

**Definition.** The **coefficient of determination** is the explained share:

$$\boxed{\;\;R^2 \;=\; \frac{SSR}{SST} \;=\; 1 - \frac{SSE}{SST} \;\in\; [0,1].\;\;}$$

**Properties.** $R^2 = 0$ when the fitted line is flat ($\hat\beta_1 = 0$, model explains nothing beyond $\bar y$); $R^2 = 1$ when residuals vanish (perfect fit). In **simple regression** the squared sample correlation collapses everything to:

$$R^2 \;=\; r_{xy}^2 \;\;(\text{simple regression only}).$$

**Worked numbers on NewHired.**

$$R^2 \;=\; 0.6315^2 \;=\; 0.3988.$$

**Interpretation.** Age alone explains $\approx \mathbf{40\%}$ of the variability of `Weeks`; the remaining $\approx 60\%$ is residual noise from omitted drivers (qualification, sector, regional labour market, network, …). **Moderate fit** — typical pattern for a single-predictor model on a labour-market outcome.

**Cross-references.**

- For comparing models of different size, use **adjusted $R^2 = 1 - (1-R^2)(n-1)/(n-p-1)$** — see `g15c` (penalised fit; plain $R^2$ is monotone in $p$ and cannot fairly compare models of different size).
- Beware of $R^2$ alone as a fit-quality verdict: a model can have a low $R^2$ but a highly significant slope (see Ex 8.4a: surface ⇒ revenues has $R^2 = 0.12$ but $p\approx 0$).

```r
R2  <- r_xy^2;            R2                # 0.3988
# Or via SS decomposition:
R2_check <- 1 - SSE/SST                     # 0.3988
# Or read directly from R:
summary(mod)$r.squared
```

</details>

---

<details class="master-subpart">
<summary>(d) Standard errors of $\hat\beta_1$ and $\hat\beta_0$ (specialisation of row 1; Ex 8.5a, 8.8a)</summary>

In the simple case the SE formulas of row 1 collapse to compact closed forms:

$$\boxed{\;\;\widehat{SE}(\hat\beta_1) \;=\; \frac{s_\varepsilon}{\sqrt{(n-1)\,s^2_x}}, \qquad \widehat{SE}(\hat\beta_0) \;=\; s_\varepsilon\sqrt{\frac{1}{n} + \frac{\bar x^2}{(n-1)\,s^2_x}}.\;\;}$$

**Intuition — three drivers of $\widehat{SE}(\hat\beta_1)$** (same drivers that show up in every regression SE):

- $\sqrt n$ in the denominator ⇒ quadruple $n$ ⇒ halve the SE.
- $\sqrt{s^2_x}$ in the denominator ⇒ wider spread of $X$ ⇒ tighter SE (more leverage to pin down the slope).
- $s_\varepsilon$ in the numerator ⇒ less noise ⇒ tighter SE.

**Worked numbers on NewHired.**

$$\widehat{SE}(\hat\beta_1) \;=\; \frac{19.70}{\sqrt{46\cdot 88.246}} \;=\; \frac{19.70}{63.7245} \;=\; 0.3092,$$
$$\widehat{SE}(\hat\beta_0) \;=\; 19.70\sqrt{0.02128 + 0.36728} \;=\; 19.70\cdot 0.6234 \;=\; 12.281.$$

```r
se_b1 <- s_e/sqrt((n-1)*s2_x);                          se_b1   # 0.3092
se_b0 <- s_e*sqrt(1/n + xbar^2/((n-1)*s2_x));           se_b0   # 12.28
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span><span class="tag tag-4plus">≥4 ex</span> (e) <strong>Slope $t$-test and 95% CI for $\beta_1$</strong> — *row 2 of g14a / row 2 of g13a with $\theta_0=0$* (Ex 8.1a, 8.3a, 8.5a, 8.8a, `exam_sep_2024_3b`)</summary>

**Cross-reference (do not re-derive).** The slope $t$-test is **row 2 of the universal test table at the top of `g14a`** with $\theta_0 = 0$ and the regression SE $\widehat{SE}(\hat\beta_1)$ replacing the one-mean $s/\sqrt n$. The slope CI is **row 2 of the universal CI table at the top of `g13a`** with $\bar X \to \hat\beta_1$ and SE $\to \widehat{SE}(\hat\beta_1)$. The CI ⇄ test duality of g13a / g14a applies *exactly*: reject $H_0:\beta_1 = \beta_1^{(0)}$ at level $\alpha$ ⇔ $(1-\alpha)$ CI does **not** contain $\beta_1^{(0)}$.

**Hypotheses.** $H_0:\beta_1 = 0$ (no linear relation between Age and Weeks) vs $H_1:\beta_1\neq 0$ (two-sided).

**Test statistic** (specialisation of g14a row 2 with $\theta_0=0$):

$$\boxed{\;\;T \;=\; \frac{\hat\beta_1 - 0}{\widehat{SE}(\hat\beta_1)} \;\overset{H_0}{\sim}\; t_{n-2}.\;\;}$$

**CI for $\beta_1$** (specialisation of g13a row 2):

$$\boxed{\;\;CI_{1-\alpha}(\beta_1) \;=\; \hat\beta_1 \;\pm\; t_{1-\alpha/2,\,n-2}\cdot\widehat{SE}(\hat\beta_1).\;\;}$$

**Worked numbers on NewHired** ($n-2 = 45$ df, $\alpha=0.05$).

- $t_\text{obs} = 1.6898/0.3092 \approx 5.464$ on $t_{45}$.
- Critical value: $t_{0.975,\,45} = 2.014$.
- Two-sided $p$-value: $p = 2\cdot P(T_{45} > 5.464) \approx 1.8\times 10^{-6}$.

(Cross-check via the algebraic identity $t^2 = (n-2)R^2/(1-R^2) = 45\cdot 0.3988/0.6012 = 29.85 \Rightarrow |t| = 5.464$ ✓ — these two routes agree exactly because $t^2 = F = \text{SSR}/\text{MSE}$ holds in any simple OLS regression; see (f).)

**Decision.** $|t_\text{obs}| = 5.464 \gg 2.014$ ⇒ **reject $H_0$** at $\alpha=0.05$ (and at 1%, 10% too). Age is a **strongly significant** predictor of Weeks.

**95% CI for $\beta_1$.**

$$CI_{0.95}(\beta_1) \;=\; 1.6898 \;\pm\; 2.014\cdot 0.3092 \;=\; 1.6898 \;\pm\; 0.6227 \;=\; [1.0671,\;2.3125].$$

**Duality cross-check.** Zero is *far* outside $[1.067,\,2.313]$ ⇒ same verdict as the $t$-test: reject $H_0$ at 5%. The CI procedure and the two-sided $t$-test are equivalent: $\beta_1^{(0)} \in CI_{1-\alpha} \iff$ retain $H_0:\beta_1 = \beta_1^{(0)}$ at level $\alpha$ — this is *exactly* the CI ⇄ test duality boxed in `g13a` and `g14a`.

**Interpretation of the CI** (the answer pattern for `exam_sep_2024_3b` — 95% CI for `Account_length` slope, multi-reg version of the same row): with 95% confidence, the population slope lies between $\approx 1.07$ and $\approx 2.31$ extra weeks per additional year of age, holding nothing else (this is a simple regression).

```r
t_obs <- b1/se_b1;                                    t_obs   # 5.464
2*(1 - pt(abs(t_obs), df=n-2))                                # p ~ 1.8e-6
qt(0.975, df=n-2)                                             # 2.014
b1 + c(-1,1) * qt(0.975, df=n-2) * se_b1                      # [1.067, 2.313]
# Or in one shot from R:
summary(mod)         # t value, Pr(>|t|) on the Age row
confint(mod, level=0.95)
```

</details>

---

<details class="master-subpart">
<summary>(f) $F$-test for overall model significance and the $F = t^2$ identity (row 5 specialised to $p=1$)</summary>

For simple regression with $p=1$ the ANOVA $F$-test of $H_0:\beta_1=0$ vs $H_1:\beta_1\neq 0$ uses:

| Source | SS | df | MS |
|---|---|---|---|
| Regression | $SSR = 11{,}586.4$ | $1$ | $MSR = 11{,}586.4$ |
| Residual   | $SSE = 17{,}466.7$ | $n-2 = 45$ | $MSE = \hat\sigma^2_\varepsilon = 388.15$ |
| Total      | $SST = 29{,}053.1$ | $n-1 = 46$ | — |

$$F_\text{obs} \;=\; \frac{MSR}{MSE} \;=\; \frac{11{,}586.4}{388.15} \;=\; 29.85 \;\overset{H_0}{\sim}\; F_{1,\,45}.$$

Critical value at $\alpha=0.05$: $F_{0.95;\,1,\,45} \approx 4.06$. $29.85 \gg 4.06$ ⇒ **reject $H_0$**, $p \approx 1.8\times 10^{-6}$.

**The $F = t^2$ identity.** In simple regression with one predictor, $F = t^2$ *exactly* — same null, same alternative, same rejection set on the squared-$t$ scale. Numerically $t^2 = 5.464^2 = 29.85 = F$ ✓. The identity collapses rows 4 (per-coefficient $t$) and 5 (global $F$) of the master table into a single test when $p=1$ — **they diverge as soon as $p\ge 2$** (see `g15c` for the joint $F$ that genuinely differs from the individual $t$'s, in particular when multicollinearity is present — see `exam_g1_2025_3b`).

```r
MSR   <- SSR/1;  MSE <- SSE/(n-2)
F_obs <- MSR/MSE;          F_obs                            # 29.85
F_obs - t_obs^2                                             # ~ 0  (F = t^2)
1 - pf(F_obs, df1=1, df2=n-2)                               # p ~ 1.8e-6
qf(0.95, df1=1, df2=n-2)                                    # 4.06
anova(mod)
```

</details>

---

<details class="master-subpart">
<summary>(g) Manual regression from raw sums $\sum x_i, \sum y_i, \sum x_i^2, \sum y_i^2, \sum x_i y_i$ (Ex 8.5a, 8.10a — recipe for exam tasks that withhold the raw data)</summary>

Several horizontal cells (Ex 8.5a, 8.10a) feed only the raw sums, not the individual data points. In that case use the **computational shortcuts**:

$$\bar x = \frac{1}{n}\sum_i x_i,\qquad \bar y = \frac{1}{n}\sum_i y_i,$$
$$s^2_x = \frac{1}{n-1}\!\left(\sum_i x_i^2 - n\bar x^2\right),\qquad s^2_y = \frac{1}{n-1}\!\left(\sum_i y_i^2 - n\bar y^2\right),$$
$$s_{xy} = \frac{1}{n-1}\!\left(\sum_i x_i y_i - n\bar x\bar y\right).$$

Then drop these into the closed-form OLS recipe from (a)–(e):

$$\hat\beta_1 = \frac{s_{xy}}{s^2_x},\quad \hat\beta_0 = \bar y - \hat\beta_1\bar x,\quad R^2 = \frac{s_{xy}^2}{s^2_x\,s^2_y} = r_{xy}^2,\quad SSE = (1-R^2)(n-1)s^2_y,\quad \hat\sigma^2 = \frac{SSE}{n-2}.$$

The slope SE follows from (d): $\widehat{SE}(\hat\beta_1) = s_\varepsilon/\sqrt{(n-1)s^2_x}$.

**Worked example — Ex 8.5a (Salary on experience).** $n=47$, $\sum y_i = 99{,}150$, $\sum x_i = 297$, $s^2_y = 345{,}722$, $s^2_x = 27.048$, $s_{xy} = 2697.96$.

$$\bar x = 297/47 = 6.3191,\quad \bar y = 99{,}150/47 = 2109.574.$$
$$\hat\beta_1 = 2697.96/27.048 = 99.7471,\quad \hat\beta_0 = 2109.574 - 99.7471\cdot 6.3191 = 1479.262.$$
$$R^2 = 2697.96^2/(27.048\cdot 345{,}722) = 0.7784\;\;(\text{i.e. } r_{xy} = 0.8823).$$
$$SSE = 0.2216\cdot 46\cdot 345{,}722 \approx 3{,}524{,}152,\quad \hat\sigma^2 = SSE/45 \approx 78{,}314.5,\quad s_\varepsilon \approx 279.8.$$
$$\widehat{SE}(\hat\beta_1) = 279.8/\sqrt{46\cdot 27.048} = 7.93,\quad t_\text{obs} = 99.7471/7.93 = 12.57\;\;(p\approx 0).$$

```r
# Ex 8.5a from sums alone -- no raw data needed
n      <- 47
sum.x  <- 297;       sum.y  <- 99150
s2_x   <- 27.048;    s2_y   <- 345722
s_xy   <- 2697.96
xbar   <- sum.x/n;   ybar   <- sum.y/n
b1     <- s_xy/s2_x;             b1            # 99.7471
b0     <- ybar - b1*xbar;        b0            # 1479.262
R2     <- s_xy^2/(s2_x*s2_y);    R2            # 0.7784
SSE    <- (1-R2)*(n-1)*s2_y;     SSE           # 3,524,152
s2_e   <- SSE/(n-2)
se_b1  <- sqrt(s2_e/((n-1)*s2_x));  se_b1      # 7.93
t_obs  <- b1/se_b1;              t_obs         # 12.57
```

**Take-away.** "Closed-form" is **not** a shortcut — it is just the *normal-equation* OLS solution expressed in sample moments. Whenever the question gives summary stats only, this recipe is the entire toolkit.

</details>

---

<details class="master-subpart">
<summary>(h) Summary box (NewHired) and cross-references to g15b–g15e + g13a/g14a/g13f/g9</summary>

**Numerical summary on the running NewHired sample.**

| Quantity | Value | Source / formula |
|---|---|---|
| $\hat\beta_0$ | $-19.5262$ | $\bar y - \hat\beta_1\bar x$ |
| $\hat\beta_1$ | $1.6898$ | $s_{xy}/s^2_x$ |
| $R^2$ | $0.3988$ | $r_{xy}^2 = SSR/SST$ |
| $SST,\,SSR,\,SSE$ | $29{,}053.1,\,11{,}586.4,\,17{,}466.7$ | variance decomposition |
| $\hat\sigma^2_\varepsilon,\,s_\varepsilon$ | $388.15,\,19.70$ | $SSE/(n-2)$ |
| $\widehat{SE}(\hat\beta_1)$ | $0.3092$ | $s_\varepsilon/\sqrt{(n-1)s^2_x}$ |
| $\widehat{SE}(\hat\beta_0)$ | $12.28$ | $s_\varepsilon\sqrt{1/n + \bar x^2/((n-1)s^2_x)}$ |
| $t_\text{obs}\;(H_0:\beta_1=0)$ | $5.464$, $p\approx 1.8\times 10^{-6}$ | $\hat\beta_1/\widehat{SE}(\hat\beta_1)$ — row 2 of g14a with $\theta_0=0$ |
| 95% CI for $\beta_1$ | $[1.067,\,2.313]$ | $\hat\beta_1 \pm t_{0.975,45}\widehat{SE}(\hat\beta_1)$ — row 2 of g13a |
| $F_\text{obs}$ | $29.85 = t_\text{obs}^2$, $p\approx 1.8\times 10^{-6}$ | $MSR/MSE$ — collapses to $t^2$ for $p=1$ |

**Verdict.** Age is a **strongly significant** linear predictor of `Weeks`. The slope is positive ($+1.69$ weeks per extra year of age), the relation explains $\approx 40\%$ of the variability of `Weeks`, and the $t$-test on $\beta_1$, the 95% CI for $\beta_1$ and the $F$-test on the full model all reach the *same* conclusion — algebraically inevitable in simple regression because $F = t^2$.

**Cross-reference hub for the rest of G15 (and the prerequisite topics).**

| Need | Go to | Why |
|---|---|---|
| **CI for mean response $E[Y\mid x_0]$ or PI for new $Y_0$** at a given $x_0$ | **`g15b`** | Rows 6–7 of master table — adds the "+1" inside the sqrt for the PI |
| **Multiple regression** ($p\ge 2$ continuous predictors), adj $R^2$, joint $F$, OVB | **`g15c`** | Rows 1–5 with $p\ge 2$; introduces "ceteris paribus" reading and the genuine $F \ne t^2$ |
| **Categorical predictors / dummies / interactions** | **`g15d`** | Rows 8–9 — dummy coding, contrasts, factor-level shifts |
| **Residual diagnostics (LINE), leverage, Cook's D, VIF for multicollinearity** | **`g15e`** | The assumptions that *license* rows 4–7 of the master table |
| **Reusable $t$-test template** (any $H_0:\theta=\theta_0$, including $H_0:\beta_j = 0$) | **`g14a`** row 2 | The slope $t$-test is this row with $\theta_0=0$, $\widehat{SE} \to \widehat{SE}(\hat\beta_j)$, df $= n-p-1$ |
| **Reusable CI template** | **`g13a`** row 2 | The slope CI is this row with $\bar X \to \hat\beta_j$, SE $\to \widehat{SE}(\hat\beta_j)$ |
| **Unbiased estimators** $\hat\beta_j$, $\hat\sigma^2$ as linear-in-$y$ statistics | **`g13f`** | Mean / variance / unbiasedness — the foundational properties |
| **Correlation $r_{xy}$, scatterplot, sign / strength of association** | **`g9`** | $R^2 = r_{xy}^2$ in simple regression — the bivariate-summary side of the same story |

</details>
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
    "title": "Master Exam — Multiple regression: matrix-form OLS, per-coefficient inference, global $F$, $R^2$ vs adj $R^2$ on superstore (MntMeat ~ IncomeK + Age + KidsAtHome, n=2200)",
    "content": r"""## Setup — running dataset for every numeric example below

A food retailer's `superstore` dataframe ($n = 2200$ customers) is the **single dataset** used throughout this entry — no new sample is introduced in any subpart. The response is `MntMeatProducts` (€ spent on meat in the last two years); the regressors are `IncomeK` (annual household income in k€), `Age` (years), `KidsAtHome` $\in\{0,1,2\}$ (number of children at home, treated as numeric — categorical version is owned by `g15d`). We fit by OLS the multiple linear model

$$\boxed{\;\;\text{MntMeat}_i \;=\; \beta_0 + \beta_1\,\text{IncomeK}_i + \beta_2\,\text{Age}_i + \beta_3\,\text{KidsAtHome}_i + \varepsilon_i,\qquad \varepsilon_i\overset{\rm iid}{\sim}\mathcal N(0,\sigma^2),\quad i = 1,\dots,n=2200.\;\;}$$

In matrix form: $y = X\boldsymbol\beta + \boldsymbol\varepsilon$ with $X$ the $n\times (p+1) = 2200\times 4$ design matrix (column of 1's + the three regressor columns), $p = 3$, df $= n-p-1 = 2196$. The **`summary(mod)` table** (rounded) reads:

\begin{tabular}{p{8cm}|p{6cm}|p{6cm}|p{5cm}|p{6cm}}
\textbf{Coefficient} & \textbf{Estimate $\hat\beta_j$} & \textbf{$\widehat{SE}(\hat\beta_j)$} & \textbf{$t_j$} & \textbf{$p$-value} \\
(Intercept) & $-74.10$ & $13.05$ & $-5.68$ & $\approx 0$ \\
IncomeK & $+6.142$ & $0.168$ & $36.56$ & $\approx 0$ \\
Age & $-2.805$ & $0.286$ & $-9.81$ & $\approx 0$ \\
KidsAtHome & $-78.40$ & $7.62$ & $-10.29$ & $\approx 0$ \\
\end{tabular}

Residual SE $s_\varepsilon = 134.50$ on df $= 2196$; multiple $R^2 = 0.6361$; adjusted $R^2 = 0.6356$; global $F(3,\,2196) = 1279.7$ ($p\approx 0$). Reference quantiles used below: $t_{2196,\,0.975}\approx 1.961$, $t_{2196,\,0.995}\approx 2.578$, $F_{3,2196,\,0.95}\approx 2.61$.

```r
mod <- lm(MntMeatProducts ~ IncomeK + Age + KidsAtHome, data = superstore)
summary(mod)                  # coefficient table, R^2, adj R^2, global F
confint(mod, level = 0.95)    # per-coefficient 95% CIs
```

Round to 4 decimals throughout.

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) <strong>Matrix-form OLS and the design matrix $X$</strong> — $\hat{\boldsymbol\beta} = (X^\top X)^{-1}X^\top y$ (row 1 of master table)</summary>

**The OLS criterion** (same as `g15a` (a), with $\boldsymbol\beta$ now a $(p+1)$-vector):

$$\hat{\boldsymbol\beta} \;=\; \arg\min_{\boldsymbol\beta\in\mathbb R^{p+1}}\;\bigl\lVert\, y - X\boldsymbol\beta\,\bigr\rVert^2 \;=\; \arg\min_{\boldsymbol\beta}\;\sum_{i=1}^{n}\bigl(y_i - \beta_0 - \beta_1 x_{1,i} - \dots - \beta_p x_{p,i}\bigr)^2.$$

The normal equations $X^\top X\,\hat{\boldsymbol\beta} = X^\top y$ deliver the **closed-form matrix OLS estimator**:

$$\boxed{\;\;\hat{\boldsymbol\beta} \;=\; (X^\top X)^{-1}\,X^\top y, \qquad \hat y \;=\; X\hat{\boldsymbol\beta} \;=\; \underbrace{X(X^\top X)^{-1}X^\top}_{H\ =\ \text{hat / projection matrix}}\,y.\;\;}$$

**Geometric reading.** The hat matrix $H = X(X^\top X)^{-1}X^\top$ is the orthogonal **projection onto the column space of $X$**: $\hat y$ is the projection of $y$ onto $\mathrm{span}(X)$, and the residual vector $\hat\varepsilon = y - \hat y = (I - H)y$ is orthogonal to *every* column of $X$. The simple-regression centroid identity "the line passes through $(\bar x,\bar y)$" of `g15a` is the $p=1$ instance: the *first* normal equation (from the intercept column $\mathbf 1$) says $\sum_i\hat\varepsilon_i = 0$.

**Variance of $\hat{\boldsymbol\beta}$** (the source of every SE in the coefficient table):

$$\Var(\hat{\boldsymbol\beta}) \;=\; \sigma^2\,(X^\top X)^{-1},\qquad \widehat{\Var}(\hat{\boldsymbol\beta}) \;=\; \hat\sigma^2\,(X^\top X)^{-1},\qquad \widehat{SE}(\hat\beta_j) \;=\; \hat\sigma\,\sqrt{\bigl[(X^\top X)^{-1}\bigr]_{jj}}.$$

For the **superstore** fit, R's `vcov(mod)` produces the $4\times 4$ matrix $\hat\sigma^2(X^\top X)^{-1}$; its diagonal gives $\widehat{SE}^2$ for each coefficient — e.g. $\widehat{SE}(\hat\beta_1) = 0.168$ for `IncomeK`. The fitted hyperplane is

$$\boxed{\;\;\widehat{\text{MntMeat}} \;=\; -74.10 \;+\; 6.142\,\text{IncomeK} \;-\; 2.805\,\text{Age} \;-\; 78.40\,\text{KidsAtHome}.\;\;}$$

```r
# Matrix-form OLS by hand (sanity check that R's lm() is exactly the projection above)
X      <- model.matrix(mod)                  # 2200 x 4 design matrix
y      <- superstore$MntMeatProducts
beta_h <- solve(t(X) %*% X) %*% t(X) %*% y   # hand-computed (X'X)^{-1} X' y
cbind(beta_h, coef(mod))                     # identical to lm() coefficients
# Variance-covariance and per-coefficient SE
vcov(mod)                                    # 4x4 matrix = sigma^2 * (X'X)^{-1}
sqrt(diag(vcov(mod)))                        # SEs: 13.05, 0.168, 0.286, 7.62
# Projection check: residuals orthogonal to every column of X
all.equal(as.numeric(t(X) %*% residuals(mod)), rep(0, 4))   # TRUE
```

**Why this matters for every later subpart.** Every SE in the coefficient table, every CI, every $t$-test and the prediction-leverage term $h_{00}$ (in subpart (f) and in `g15b`) all read off the *same* matrix $\hat\sigma^2(X^\top X)^{-1}$ computed once here.

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) <strong>Per-coefficient inference: $\widehat{SE}(\hat\beta_j)$, $t_j$, 95% CI</strong> — *row 2 of g14a / row 2 of g13a with $\theta_0 = 0$*</summary>

**Cross-reference (do not re-derive).** Each row of `summary(mod)`'s coefficient table is **two universal templates in one**: column "$t$ value" + "$p$-value" = **row 2 of g14a** with $\theta_0 = 0$; output of `confint(mod)` = **row 2 of g13a**. The decision rule, the two-sided $p$-value formula, the CI ⇄ test duality are *identical*. df $= n - p - 1 = 2196$ throughout.

**Per-coefficient $t$-statistic and 95% CI** (specialisations of g14a row 2 and g13a row 2):

$$\boxed{\;\;T_j \;=\; \frac{\hat\beta_j - 0}{\widehat{SE}(\hat\beta_j)} \;\overset{H_0}{\sim}\; t_{n-p-1}, \qquad CI_{1-\alpha}(\beta_j) \;=\; \hat\beta_j \;\pm\; t_{1-\alpha/2,\,n-p-1}\,\widehat{SE}(\hat\beta_j).\;\;}$$

**Worked numbers on `superstore`** (df $= 2196$, $\alpha = 0.05$, $t_{0.975,\,2196}\approx 1.961$). Each row $\hat\beta_j \pm 1.961\cdot\widehat{SE}(\hat\beta_j)$:

\begin{tabular}{p{8cm}|p{6cm}|p{6cm}|p{5cm}|p{12cm}|p{4cm}}
\textbf{Coefficient} & \textbf{$\hat\beta_j$} & \textbf{$\widehat{SE}$} & \textbf{$t_j$} & \textbf{95\% CI} & \textbf{Verdict} \\
IncomeK & $+6.142$ & $0.168$ & $+36.56$ & $(5.813,\;6.471)$ & reject $H_0$ \\
Age & $-2.805$ & $0.286$ & $-9.81$ & $(-3.366,\;-2.244)$ & reject $H_0$ \\
KidsAtHome & $-78.40$ & $7.62$ & $-10.29$ & $(-93.34,\;-63.46)$ & reject $H_0$ \\
\end{tabular}

All three two-sided $p$-values are $\approx 0$. **Duality cross-check** (g13a ⇄ g14a): every 95% CI excludes 0 ⇔ every two-sided $t$-test rejects at $\alpha = 0.05$ — *algebraically forced* by the duality.

**Rescaling the CI to a non-unit change** (`exam_july_2024_3a`-style sub-question: "extra MntMeat for a +10 k€ pay rise"): multiply both the point estimate and the CI endpoints by $\Delta x = 10$. Point estimate $10\hat\beta_1 = 61.42$ €; 95% CI $10\cdot(5.813,\,6.471) = (58.13,\,64.71)$ €. The CI rescaling is *exact* because $\hat\beta_1$ is the only random object on the left-hand side.

```r
summary(mod)                       # t-values and p-values
confint(mod, level = 0.95)         # per-coefficient 95% CIs
qt(0.975, df = 2196)               # 1.961
# Rescale CI to a +10 k€ income change
10 * coef(mod)["IncomeK"]                   # 61.42
10 * confint(mod, "IncomeK", level = 0.95)  # (58.13 ; 64.71)
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (c) <strong>Ceteris-paribus interpretation</strong> + omitted-variable bias (the partial-effect mental model)</summary>

The single line that defines multi-regression — **and that every exam asks you to write next to a $\hat\beta_j$**:

$$\boxed{\;\;\hat\beta_j \;\text{ = expected change in } Y \text{ for a 1-unit increase in } X_j, \;\textbf{holding all other regressors fixed.}\;\;}$$

This is the **partial-effect** reading. It is fundamentally different from the slope $\tilde\beta_j$ of the *simple* regression of $Y$ on $X_j$ alone, which absorbs the indirect channels through which $X_j$ correlates with the other regressors. Applied to the running fit:

- $\hat\beta_1 = +6.142$ €/k€ — keeping `Age` and `KidsAtHome` fixed, **+1 k€ of household income raises expected meat spend by $\approx 6.14$ €**.
- $\hat\beta_2 = -2.805$ €/yr — at fixed income and number of kids, **+1 year of age lowers expected meat spend by $\approx 2.81$ €** (older customers buy less meat *once income is controlled for*).
- $\hat\beta_3 = -78.40$ €/kid — at fixed income and age, **+1 child at home is associated with $-78.4$ € of expected meat spend**.
- $\hat\beta_0 = -74.10$ has no useful interpretation (a 0-k€-income, 0-year-old, 0-kid customer is outside the data).

**Why this differs from a simple regression of $Y$ on $X_j$ alone — omitted-variable bias (OVB).** Run instead `MntMeat ~ IncomeK` alone:

$$\widehat{\text{MntMeat}}_{\text{simple}} \;=\; -101.5 \;+\; 5.380\,\text{IncomeK}\qquad (R^2_{\text{simple}} = 0.521).$$

The simple slope is $+5.380$, **smaller** than the partial slope $+6.142$. For a two-regressor block (`IncomeK`, `KidsAtHome` after partialling out `Age`), the bias formula is

$$\boxed{\;\;E\bigl[\tilde\beta_1^{\text{simple}}\bigr] \;=\; \beta_1 \;+\; \beta_3\,\frac{\mathrm{Cov}(\text{IncomeK},\text{KidsAtHome})}{\mathrm{Var}(\text{IncomeK})}\;\;\;(\text{OVB formula}).\;\;}$$

Empirically $\mathrm{cor}(\text{IncomeK},\text{KidsAtHome}) \approx -0.34$ (richer customers tend to have fewer kids) and $\hat\beta_3 < 0$ — so the product $\hat\beta_3\cdot(\mathrm{Cov}/\mathrm{Var})$ is *positive*, shrinking the simple slope **downward** toward $5.38$. Once `KidsAtHome` is controlled for, the true partial income effect $+6.14$ re-emerges. The simple model **mis-attributes** part of the income effect to the kids channel — a textbook OVB pattern (and the exact mechanism of Ex 9.3's `Performance ~ Competition` sign-flip after `Quality` is added; and of Ex 9.8's shrinkage of `Age` / `Children` once `Income` enters).

**Take-away for exam writing.** Whenever you interpret a $\hat\beta_j$ from a multi-regression always (i) say "holding all other regressors fixed", (ii) flag the OVB risk if a *correlated* relevant predictor was omitted, (iii) be ready to compare to the simple-regression slope if asked.

```r
# Compare simple vs partial slope -- the OVB diagnosis in one screen
mod.simple <- lm(MntMeatProducts ~ IncomeK, data = superstore)
coef(mod.simple)["IncomeK"]                # 5.380 (simple)
coef(mod)["IncomeK"]                        # 6.142 (partial -- controlled for Age, KidsAtHome)
cor(superstore$IncomeK, superstore$KidsAtHome)   # ~ -0.34 (lurking link)
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (d) <strong>Global $F$-test for joint significance</strong> (row 5 of master table)</summary>

The global $F$-test answers a *joint* question that the per-coefficient $t$-tests cannot: "**is the model worth anything at all** vs the intercept-only model $Y = \beta_0 + \varepsilon$?". The null fixes *every* slope to zero simultaneously:

$$H_0\!:\;\beta_1 = \beta_2 = \dots = \beta_p = 0 \qquad\text{vs}\qquad H_1\!:\;\text{at least one }\beta_j \ne 0.$$

**Test statistic** (right-tail only — variance ratios are non-negative, large values contradict $H_0$):

$$\boxed{\;\;F \;=\; \frac{SSR/p}{SSE/(n-p-1)} \;=\; \frac{R^2/p}{(1-R^2)/(n-p-1)} \;\overset{H_0}{\sim}\; F_{p,\,n-p-1}.\;\;}$$

**Decision rule.** Reject $H_0$ at level $\alpha$ ⇔ $F_\text{obs} > F_{1-\alpha,\,p,\,n-p-1}$ (equivalently $p\text{-value} = 1 - F_{p,\,n-p-1}(F_\text{obs}) < \alpha$). Rejection means *at least one* $\beta_j$ is non-zero — it does **not** identify *which* one (the per-coefficient $t$-tests in (b) do that).

**Worked numbers on `superstore`** ($p=3$, $n-p-1 = 2196$, $R^2 = 0.6361$):

$$F_\text{obs} \;=\; \frac{0.6361/3}{0.3639/2196} \;=\; \frac{0.2120}{0.000166} \;=\; 1279.7,\qquad F_{3,\,2196,\,0.95} \approx 2.61.$$

Since $1279.7 \gg 2.61$ ($p$-value $\approx 0$), **reject $H_0$**: the model is **globally highly significant** — at least one of `IncomeK`, `Age`, `KidsAtHome` has a non-zero population slope.

**Why $F \ne t^2$ once $p\ge 2$.** In `g15a` (simple regression, $p=1$) the identity $F = t^2$ collapsed rows 4 and 5 of the master table into a single test. With $p\ge 2$ the global $F$ tests *all* slopes jointly and cannot be reduced to any single per-coefficient $t$. The classic pathological case (cf. `exam_g1_2025_3b`): under **multicollinearity** the per-coefficient $t$'s can *all* fail to reject (huge SEs from variance inflation) while the global $F$ still rejects strongly — the regressors are jointly informative but individually indistinguishable. Diagnostics for that (VIF) are owned by `g15e`.

```r
summary(mod)                                  # F-statistic line at the bottom of the summary
qf(0.95, df1 = 3, df2 = 2196)                 # 2.61
1 - pf(1279.7, df1 = 3, df2 = 2196)           # ~ 0 (p-value)
# Equivalent F computed from R^2:
(0.6361/3) / (0.3639/2196)                    # 1279.7  -- matches the summary line
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (e) <strong>$R^2$ vs adjusted $R^2$</strong> — the right metric for comparing models with different $p$ (rows 3–4 of master table)</summary>

**Plain $R^2$** is the share of $\Var(y)$ explained by the regression:

$$R^2 \;=\; 1 - \frac{SSE}{SST} \;=\; \frac{SSR}{SST} \;\in\; [0,1].$$

**Structural property — $R^2$ is monotone non-decreasing in $p$.** Adding *any* regressor (even pure noise) cannot increase $SSE$ at the OLS optimum, so $R^2$ can only weakly rise. Consequence: **plain $R^2$ cannot fairly compare two models with a different number of predictors** — a noise regressor will always look like an "improvement" on $R^2$ alone.

**Adjusted $R^2$ — the penalised metric**:

$$\boxed{\;\;R^2_\text{adj} \;=\; 1 - (1 - R^2)\,\frac{n-1}{n-p-1} \;=\; 1 - \frac{SSE/(n-p-1)}{SST/(n-1)} \;=\; 1 - \frac{\hat\sigma^2_\varepsilon}{s_y^2}.\;\;}$$

The penalty factor $(n-1)/(n-p-1)$ grows with $p$: $R^2_\text{adj}$ rises only when the new regressor explains *more* variance than the one extra degree of freedom costs (i.e. $\hat\sigma^2_\varepsilon$ goes down enough). Adding a useless regressor *lowers* $R^2_\text{adj}$ — which is exactly the property `exam_g1_2025_6` and `exam_g2_2026_4_6` exploit when they ask "would you keep the extra predictor?".

**Worked numbers on `superstore`** ($n = 2200$, $p = 3$, $R^2 = 0.6361$):

$$R^2_\text{adj} \;=\; 1 - (1 - 0.6361)\cdot\frac{2199}{2196} \;=\; 1 - 0.3639\cdot 1.001366 \;=\; 0.6356.$$

The penalty is tiny here because $n$ is huge relative to $p$ — the two metrics agree to three decimals. **The exam-relevant comparison** is across nested model sizes: a typical past-exam pattern (`exam_g1_2025_6`: Adj $R^2$ jumps from $0.5468$ on the 4-regressor model to $0.6592$ when `Steps` is added) is a clear **keep-the-extra-predictor** verdict; `exam_g2_2026_4_6`'s tiny jump $0.4132 \to 0.4151$ on adding `loyalty` is **marginal** (matches the borderline $t$-test on $\hat\beta_\text{loyalty}$). On `superstore`, dropping any of the 3 regressors would *lower* Adj $R^2$ — none is redundant.

**Take-aways.** (i) Plain $R^2$ is for "how much variance the *current* model captures"; (ii) Adj $R^2$ is for "*which* of two candidate models, of different sizes, should I prefer"; (iii) for nested models, the formal joint test is the *partial-F* (handled briefly under model comparison; owned by `g15c` extensions / `g15e`).

```r
summary(mod)$r.squared                        # 0.6361
summary(mod)$adj.r.squared                    # 0.6356
# Adj R^2 by hand (matches summary line):
1 - (1 - 0.6361) * (2200 - 1) / (2200 - 3 - 1)   # 0.6356
# Drop-test: removing KidsAtHome lowers Adj R^2 (and rejects via t/partial-F)
summary(lm(MntMeatProducts ~ IncomeK + Age, data = superstore))$adj.r.squared   # < 0.6356
```

</details>

---

<details class="master-subpart">
<summary>(f) <strong>Prediction with multiple predictors</strong> — one numeric instance, formulas owned by `g15b` (rows 6–7)</summary>

**Cross-reference (do not re-derive).** **Rows 6 (CI for mean response) and 7 (PI for individual)** of the master case table are owned by `g15b`. The formulas are *identical* to the simple-regression case there — only the simple-regression leverage $\ell(x_0) = 1/n + (x_0-\bar x)^2/((n-1)s_x^2)$ is replaced by the **matrix leverage** $h_{00} = x_0^\top(X^\top X)^{-1} x_0$, and df $= n-p-1$ instead of $n-2$. The "$+1$" inside the sqrt (= the residual variance of a single new $\varepsilon_0$) is the only structural difference between CI (row 6) and PI (row 7). R's `predict(mod, newdata, interval=...)` handles both cases identically.

$$\boxed{\;\;\hat y_0 \;\pm\; t_{1-\alpha/2,\,n-p-1}\,s_\varepsilon\cdot\begin{cases}\sqrt{x_0^\top(X^\top X)^{-1}x_0} & \text{(CI, row 6)}\\ \sqrt{1 + x_0^\top(X^\top X)^{-1}x_0} & \text{(PI, row 7)}\end{cases}\;\;}$$

**Numeric prediction on `superstore`** at the target profile $x_0 = (1,\,\text{IncomeK}=70,\,\text{Age}=45,\,\text{KidsAtHome}=1)^\top$.

Point prediction: $\hat y_0 \;=\; -74.10 + 6.142\cdot 70 - 2.805\cdot 45 - 78.40\cdot 1 \;=\; \mathbf{151.21}$ €.

R returns (from `predict(mod, newdata, interval=...)`) approximately:

\begin{tabular}{p{10cm}|p{8cm}|p{8cm}|p{8cm}}
\textbf{Interval} & \textbf{Half-width} & \textbf{95\% interval} & \textbf{Width} \\
CI for $E[\text{MntMeat}\mid x_0]$ (row 6) & $\approx 7$ € & $(144.3,\;158.1)$ € & $\approx 14$ € \\
PI for one customer at $x_0$ (row 7) & $\approx 264$ € & $(-112.6,\;414.9)$ € & $\approx 528$ € \\
\end{tabular}

The PI is $\approx 38\times$ wider — the irreducible noise $s_\varepsilon = 134.50$ dominates, *not* the sampling uncertainty of $\hat{\boldsymbol\beta}$ (i.e. $\sqrt{h_{00}}$ is small at this central $x_0$). Same structural pattern as in `g15b` (NewHired) and as in Ex 9.4, 9.9, 9.10, 9.11. **The "+1" inside the sqrt is doing all the work** — see `g15b` subpart (c) for the formal $\widehat{SE}_{\text{PI}}^2 - \widehat{SE}_{\text{CI}}^2 = s_\varepsilon^2$ identity at every $x_0$.

```r
newdata <- data.frame(IncomeK = 70, Age = 45, KidsAtHome = 1)
predict(mod, newdata, interval = "confidence", level = 0.95)
##      fit      lwr      upr
##  151.21    144.3    158.1
predict(mod, newdata, interval = "prediction", level = 0.95)
##      fit      lwr      upr
##  151.21   -112.6    414.9
# Matrix leverage by hand (the only "new" piece vs simple regression):
x0  <- c(1, 70, 45, 1)
h00 <- as.numeric(t(x0) %*% solve(t(model.matrix(mod)) %*% model.matrix(mod)) %*% x0)
# CI half-width = qt(0.975, 2196) * s_e * sqrt(h00)
# PI half-width = qt(0.975, 2196) * s_e * sqrt(1 + h00)
```

</details>

---

<details class="master-subpart">
<summary>(g) <strong>Cross-references</strong> — where each part of this entry connects back to</summary>

| Need | Go to | Why |
|---|---|---|
| **Universal regression recipe + 9-row master case table** (the structural anchor) | **`g15a`** | The 7-step workflow and the master table that this entry specialises to $p\ge 2$ |
| **Prediction at $x_0$**: CI for $E[Y\mid x_0]$ (row 6) and PI for $Y_0$ (row 7) | **`g15b`** | Identical formulas — replace simple-regression $\ell(x_0)$ with the matrix leverage $x_0^\top(X^\top X)^{-1}x_0$; subpart (f) above shows one numeric instance |
| **Categorical predictors / dummies / interactions** (rows 8–9 of master table) | **`g15d`** | Dummy coding turns a $k$-level factor into $k-1$ regressors slotting into the *same* matrix-form OLS of subpart (a); slope-by-group via interaction terms $X_j\cdot D_k$ |
| **Residual diagnostics (LINE) + VIF for multicollinearity** | **`g15e`** | The assumptions that *license* every CI, $t$-test and $F$-test of this entry; VIF $= 1/(1-R_j^2)$ explains the "all $t$'s insignificant but $F$ rejects" pathology of `exam_g1_2025_3b` |
| **Reusable $t$-test template** (row 2 of g14a with $\theta_0 = 0$) | **`g14a`** row 2 | Each $t_j$ in `summary(mod)` is this row with $\bar X \to \hat\beta_j$, $s/\sqrt n \to \widehat{SE}(\hat\beta_j)$, df $= n-p-1$ |
| **Reusable CI template** (row 2 of g13a) | **`g13a`** row 2 | Each line of `confint(mod)` is this row; the CI ⇄ test duality is the algebraic reason the two columns of (b) agree |
| **Unbiasedness of $\hat\beta_j$, $\hat\sigma^2$ as linear-in-$y$ statistics** | **`g13f`** | The mean / variance / unbiasedness foundations that justify the SE formulas of subpart (a) |

**Concept-by-subpart map.**

| Concept | Subpart | Master-table row | Universal template |
|---|---|---|---|
| Matrix-form OLS $\hat{\boldsymbol\beta} = (X^\top X)^{-1}X^\top y$; hat matrix; $\Var(\hat{\boldsymbol\beta}) = \sigma^2(X^\top X)^{-1}$ | (a) | 1 | — |
| Per-coefficient $\widehat{SE}$, $t_j$, 95% CI; CI ⇄ test duality | (b) | 1 | g14a row 2 + g13a row 2 with $\theta_0 = 0$ |
| Ceteris-paribus interpretation; OVB | (c) | 1 | — |
| Global $F$-test of $H_0:\beta_1=\dots=\beta_p=0$ | (d) | 5 | — |
| Plain $R^2$ vs adjusted $R^2$; model comparison | (e) | 3–4 | — |
| Prediction at $x_0$: CI vs PI | (f) | 6–7 | g15b (formulas) |

**Verdict on the running `superstore` fit.** The 3-regressor model is **globally highly significant** ($F(3,2196) = 1279.7$), **all three slopes are individually significant** at any usual $\alpha$ (each $|t_j| \ge 9.8$), and the regressors **jointly explain $\approx 63.6\%$** of the variance of `MntMeatProducts`. Income raises meat spend ($+6.14$ €/k€ ceteris paribus); age and the presence of kids lower it. The model is suitable for predicting *average* meat spend at a given customer profile (narrow CI) but, like every regression with $R^2$ comfortably below $1$, gives wide PIs for any *individual* customer — the structural CI/PI gap of `g15b` carries over verbatim.

</details>

![Master G15c — coefficient t-stats, added-variable plot, R² vs adj-R², CI vs PI](statistics/images/master/master_g15c_ai.png)
""",
    "images": ["statistics/images/master/master_g15c_ai.png"],
}

# (Old g15c trailing block removed — content fully rewritten above)
_OLD_G15C_TRAILER = r"""

| Subpart | Master-table row | Use |
|---|---|---|
| (a) $p$ predictors → $p+1$ coefficients, df $=n-p-1$ | row 1 | per-coefficient point estimate (partial / ceteris-paribus slope) |
| (b)–(c) multiple $R^2$ + **adjusted $R^2$** | rows 3–4 | model comparison across different $p$ |
| (d) individual $t$-test per coefficient | row 1 + g14a row 2 | $H_0:\beta_j=0$ — *exactly* the universal one-sample $t$ on $\hat\beta_j$ with $\theta_0=0$ |
| (e)–(f) CIs and marginal-effect rescaling | row 1 | $\hat\beta_j \pm t_{1-\alpha/2,\,n-p-1}\,\widehat{SE}$ — equivalent to g13a row 2 |
| (c)/(global $F$) | row 5 | $H_0:\beta_1=\dots=\beta_p = 0$ — joint significance |

The 7-step recipe (model → OLS → fit-quality → coefficient inference → global $F$ → prediction → diagnostics) is at the top of `g15a`; we are walking steps **1–5** with $p=3$. Confounding / OVB ($\hat\beta_1^{\text{simple}}$ vs $\hat\beta_1^{\text{multiple}}$, part (g)) is the substantive reason the multi-regressor model is *not* a stack of separate simple regressions.

**One consolidated exam-style exercise on multiple regression.** Distilled from Ex 9.1, 9.2, 9.3, 9.4, 9.8, 9.10, 9.11, 9.12, 9.13: a single dataset, all unique subparts asked at the exam.

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

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) Read the estimated equation; interpret each slope (ceteris paribus)</summary>

The fitted multiple-regression hyperplane is
$$\widehat{\text{MntMeat}} \;=\; -74.10 \;+\; 6.142\,\text{IncomeK} \;-\; 2.805\,\text{Age} \;-\; 78.40\,\text{KidsAtHome}.$$

Partial-slope reading — *each $\hat\beta_j$ is the expected change in `MntMeat` for a 1-unit increase in $x_j$ holding all other regressors fixed*:

- $\hat\beta_1 = +6.142$ €/k€ — keeping `Age` and `KidsAtHome` fixed, **+1 k€ of household income raises expected yearly meat spend by $\approx 6.14$ €**.
- $\hat\beta_2 = -2.805$ €/yr — at fixed income and number of kids, **+1 year of age lowers expected meat spend by $\approx 2.81$ €** (older customers buy less meat once income is controlled for).
- $\hat\beta_3 = -78.40$ €/kid — at fixed income and age, **+1 child at home is associated with $-78.4$ € on expected meat spend**.
- $\hat\beta_0 = -74.10$ has no useful interpretation (a 0-k€-income, 0-year-old customer with 0 kids — outside the data).

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) Goodness of fit: multiple $R^2$, adjusted $R^2$, and why the penalty</summary>

$$R^2 \;=\; 1 - \frac{\text{SSE}}{\text{SST}} \;=\; \frac{\text{SSR}}{\text{SST}} \;=\; 0.6361,\qquad R^2_{\text{adj}} \;=\; 1 - \frac{\text{SSE}/(n-K-1)}{\text{SST}/(n-1)} \;=\; 0.6356.$$

About **63.6%** of the variance of `MntMeat` is jointly explained by income, age and number of kids. Adjusted $R^2$ is barely smaller because it penalises the model by $K=3$ added regressors: with $n=2200$ the penalty is tiny, but with small $n$ it matters — adding a useless regressor *always* raises $R^2$ but can lower $R^2_{\text{adj}}$. Use $R^2_{\text{adj}}$ to compare models with a different number of predictors.

```r
# Fit the master multiple regression
mod <- lm(MntMeatProducts ~ IncomeK + Age + KidsAtHome, data=superstore)
summary(mod)$r.squared           # 0.6361
summary(mod)$adj.r.squared       # 0.6356
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (c) Global $F$-test (all slopes = 0)</summary>

$H_0: \beta_1=\beta_2=\beta_3=0$ vs $H_1:$ at least one $\beta_j\ne 0$, with statistic
$$F \;=\; \frac{\text{SSR}/K}{\text{SSE}/(n-K-1)} \;=\; \frac{R^2/K}{(1-R^2)/(n-K-1)} \;=\; \frac{0.6361/3}{0.3639/2196} \;=\; 1279.7,$$
on $(K,\,n-K-1)=(3,\,2196)$ df. Since $F = 1279.7 \gg F_{3,2196,\,0.95} = 2.61$ (equivalently $p\approx 0$), **reject $H_0$**: at least one slope is non-zero — the model is globally significant.

```r
qf(0.95, df1=3, df2=2196)        # ~ 2.61
# Realised F = 1279.7 >> 2.61  =>  reject H0 at any usual alpha.
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (d) Individual $t$-tests on each coefficient</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (e) 95% confidence interval for each $\beta_j$</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (f) Marginal effect over a non-unit change: $\beta\cdot\Delta x$ and its 95% CI</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (g) Confounding / omitted-variable bias — *why* including all 3 regressors matters</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (h) Prediction at a target customer profile (`IncomeK = 70, Age = 45, KidsAtHome = 1`)</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (i) Mini-diagnostic checklist (linear-model assumptions)</summary>

OLS inference rests on: $E[\varepsilon\mid X]=0$ (linearity), $\mathrm{Var}(\varepsilon\mid X)=\sigma^2$ (homoscedasticity), $\mathrm{Cor}(\varepsilon_i,\varepsilon_j)=0$, approximate normality of $\varepsilon$ (CLT covers it at $n=2200$). Standard checks: `plot(mod, which=1)` (residuals vs fitted — funnel = heteroscedasticity; curvature = non-linearity), `hist(rstandard(mod))` (right-skew / heavy tails). In `superstore`, residuals vs fitted typically show a funnel (variance grows with $\hat y$) and right-skewed residuals — so the CIs/PI above should be taken with caution; sandwich SEs (`lmtest::coeftest(mod, vcov=sandwich::vcovHC)`) or a log-transform of `MntMeat` are the usual fixes.

```r
plot(mod, which=1)               # residuals vs fitted: funnel?
hist(rstandard(mod), breaks=30)  # right-skew?
# Robust-SE fallback if assumptions fail:
# library(sandwich); library(lmtest); coeftest(mod, vcov=vcovHC(mod, type="HC3"))
```

</details>

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
"""

master_exercises["g13b_ci_one_prop"] = {
    "title": "Master Exam — CI for one proportion (Wald + sample-size + Clopper–Pearson)",
    "content": r"""## Setup — running dataset for every numeric example below

A bookstore renovated its layout and is monitoring how many customers visited the new in-store cafeteria at least once in the last month. A random sample of $n = 140$ customers is interviewed; $X = 108$ of them report at least one visit, so the sample proportion of *cafeteria-users* is

$$\hat p \;=\; \frac{X}{n} \;=\; \frac{108}{140} \;\approx\; 0.7714.$$

Let $p$ denote the unknown population proportion of cafeteria-users. We will treat this *same* sample throughout the entry — first for the **large-sample Wald CI** (Case 3a), then for **sample-size planning** (Case 3b), then for the **exact Clopper–Pearson CI** (Case 3c) on a tiny-$n$ variant of the same problem. No new dataset is introduced anywhere below.

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) <strong>Case 3a</strong> — Large-sample Wald CI for $p$ (the default)</summary>

**Setting.** $Y_1,\ldots,Y_n$ i.i.d. Bernoulli$(p)$, $X = \sum_i Y_i \sim \mathrm{Bin}(n,p)$, $\hat p = X/n$. We want a CI for $p$ and the CLT validity check $n\hat p, n(1-\hat p)\ge 5$ holds.

**Pivot.** With $\Var(\hat p) = p(1-p)/n$, the CLT delivers the asymptotic pivot
$$Z \;=\; \frac{\hat p - p}{\sqrt{p(1-p)/n}} \;\overset{d}{\to}\; \mathcal N(0,1).$$
Replacing the unknown $p$ in the SE by $\hat p$ (plug-in, valid by Slutsky) gives the **Wald** CI:

$$\boxed{\;\;CI_{1-\alpha}(p) \;=\; \hat p \;\pm\; z_{1-\alpha/2}\,\sqrt{\frac{\hat p(1-\hat p)}{n}}\;\;}$$

The critical-value family is $z$ (not $t$) — there is no separate variance parameter $\sigma^2$ to estimate; $\hat p$ alone pins down both the mean *and* the variance of the Bernoulli, so no df-correction inflation is needed. Contrast this with row 2 of the universal table (one mean, $\sigma$ unknown) which **does** pay a $t_{n-1}$ inflation precisely because $s$ is a separate estimate.

**Worked numbers on the bookstore sample** with $n = 140$, $\hat p = 108/140 = 0.7714$, $\alpha = 0.05$:
- CLT check: $n\hat p = 108\ge 5$ and $n(1-\hat p) = 32\ge 5$ — both met (well clear of the boundary).
- Estimated SE: $\widehat{SE}(\hat p) = \sqrt{0.7714\cdot 0.2286/140} = \sqrt{0.001260} \approx 0.03549$.
- Critical value: $z_{0.975} = 1.96$.
- $ME_{95} = 1.96\cdot 0.03549 \approx 0.0696$.
- $CI_{95} = 0.7714 \pm 0.0696 = [0.7019,\;0.8410]$.

```r
# Bookstore cafeteria-visits frequency table
visits <- rep(0:8, c(32,43,14,10,18,13,5,0,5))    # 140 observations
Y      <- (visits >= 1)                           # Bernoulli indicator
n      <- length(Y);                       n      # 140
phat   <- mean(Y);                         phat   # 0.7714
# CLT validity check (rule of 5)
n*phat;  n*(1-phat)                                # 108 ; 32 -- both >> 5
se     <- sqrt(phat*(1-phat)/n);           se     # 0.03549
zcrit  <- qnorm(0.975);                    zcrit  # 1.96
ME     <- zcrit * se;                      ME     # 0.0696
c(phat - ME, phat + ME)                            # [0.7019, 0.8410]
# Course helper (BAS package):
CI.prop(visits >= 1, success = TRUE, conf.level = 0.95)
# Base R equivalent (Wilson, slightly different formula):
# prop.test(x = 108, n = 140, correct = FALSE)$conf.int
```

**Reading the margin of error backwards.** Just as for the one-mean CI of `g13a`, the Wald CI is symmetric around $\hat p$, so
$$\hat p \;=\; \tfrac{L+U}{2}, \qquad ME \;=\; \tfrac{U-L}{2}, \qquad \widehat{SE} \;=\; ME/z.$$
From any printed CI we recover $\hat p$, $ME$ and $\widehat{SE}$ without raw data.

**Effect of $n$ and of the confidence level.** Both are governed by the same identity $ME = z\cdot\widehat{SE}$ with $\widehat{SE}\propto 1/\sqrt n$:
- Quadrupling $n$ halves $\widehat{SE}$ and so halves $ME$ (the $\sqrt n$ rate is the binding cost of precision — same as for the mean in `g13a`).
- Going from 95% to 99% inflates $ME$ by the factor $z_{0.995}/z_{0.975} = 2.576/1.96 \approx 1.31$ ($+31\%$ wider, same multiplier as for the mean).

![Master illustration — Wald 95% CI for one proportion + CLT validity](statistics/images/master/master_g13b_ai.png)

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) <strong>Case 3b</strong> — Sample-size planning for $p$ (target margin of error $\le m$)</summary>

A CI is useful only if it is short enough to discriminate values of $p$ that matter (e.g. ruling out 50/50). Pre-data, fix a target margin of error $m$ at confidence $1-\alpha$ and invert the ME formula:

$$ME \;=\; z_{1-\alpha/2}\,\sqrt{\frac{p(1-p)}{n}} \;\le\; m \quad\Longleftrightarrow\quad \boxed{\;\;n \;\ge\; \left(\frac{z_{1-\alpha/2}}{m}\right)^{\!2}\,p(1-p)\;\;}$$

then round **up** to the next integer. The catch is that $p$ is unknown at the design stage. Two regimes:

**Regime A — worst case (no prior info).** Maximise $g(p) = p(1-p)$ on $[0,1]$: $g'(p) = 1 - 2p = 0 \Rightarrow p^{\star} = 0.5$, giving $\max g = 0.25$. Plug $p(1-p) = 0.25$ to obtain the **conservative** sample size — guaranteed to meet the ME target whatever the true $p$ turns out to be. This collapses the formula to

$$\boxed{\;\;n \;\ge\; \left(\frac{z_{1-\alpha/2}}{2m}\right)^{\!2}\;\;}\qquad (\text{worst case }p=0.5)$$

**Regime B — pilot info ($\hat p$ available).** Plug in $\hat p(1-\hat p)$ instead of $0.25$. Saving is largest when $\hat p$ is far from $0.5$ (e.g. $\hat p = 0.1$ gives $0.09/0.25 = 36\%$ of the worst-case $n$); near $\hat p = 0.5$ the saving is minimal.

**Worked numbers on the bookstore sample.** Target a 95% CI with half-width $m = 0.03$ on the cafeteria-usage proportion:

- **Worst case** ($p=0.5$): $n \ge (1.96/0.03)^2 \cdot 0.25 = 65.33^2 \cdot 0.25 \approx 1067.1 \Rightarrow n = 1068$.
- **Pilot $\hat p = 0.7714$** ($\hat p(1-\hat p) = 0.1764$): $n \ge (1.96/0.03)^2 \cdot 0.1764 \approx 753.2 \Rightarrow n = 754$.

A pilot value far from $0.5$ (here $0.77$) **saves about 30%** of the worst-case sample.

```r
m       <- 0.03
z       <- qnorm(0.975)
# Worst-case (no prior info)
n_wc    <- (z/m)^2 * 0.25;        ceiling(n_wc)     # 1068
# Pilot using the bookstore phat
phat    <- 108/140
n_pilot <- (z/m)^2 * phat*(1-phat); ceiling(n_pilot)  # 754
# Sanity: ME at n=1068, p=0.5
qnorm(0.975) * sqrt(0.25/1068)                       # ~ 0.02998 < 0.03 OK
```

**Cross-references — past exams that ask this exact question.** `exam_g1_2024_1b` (worst case, $m=0.04$, 95% → $n=601$), `exam_g1_2026_1c` (worst case, width $\le 0.09$, 99% → $n=820$), `exam_g2_2024_5c` (worst case, width $\le 0.05$, 99% → $n=2655$), and the row's `6_6d` and `6_3b2` cells.

**Take-away.** Halving $m$ **quadruples** $n$ (the $1/\sqrt n$ rate again). Doubling the confidence multiplier ($1.96\to 2.576$) roughly **triples** $n$ (factor $\approx 1.73$). The mean-side analogue lives in `g13a` (subpart c) — identical recipe, $\sigma$ replaces the binomial variance.

</details>

<details class="master-subpart">
<summary>(c) <strong>Case 3c</strong> — Exact Clopper–Pearson CI when the CLT condition fails</summary>

If $n\hat p < 5$ or $n(1-\hat p) < 5$ (small $n$, or $\hat p$ near 0 or 1), the Wald approximation breaks down — its endpoints can even fall **outside $[0,1]$**, which is meaningless for a probability. The **Clopper–Pearson** CI inverts the **exact** binomial tails instead of the CLT normal one. The $(1-\alpha)$ CI is $[\underline p,\,\overline p]$ where

$$\underline p \;=\; B^{-1}\!\big(\alpha/2;\,X,\,n-X+1\big), \qquad \overline p \;=\; B^{-1}\!\big(1-\alpha/2;\,X+1,\,n-X\big),$$

with $B^{-1}$ the Beta-quantile (equivalently $\sum_{k\ge X}\binom{n}{k}\underline p^k(1-\underline p)^{n-k} = \alpha/2$, symmetric for $\overline p$). The interval is **asymmetric** in general and **always lies in $[0,1]$** — by construction it cannot produce negative or super-unit endpoints.

**Counter-example on a tiny-$n$ variant of the master setting.** Imagine only the first $n=30$ cafeteria interviews are available and only $X=1$ customer reports a visit ($\hat p = 1/30 \approx 0.033$, so $n\hat p = 1 < 5$, Wald fails):

- **Wald (invalid here):** $0.033 \pm 1.96\sqrt{0.033\cdot 0.967/30} = 0.033 \pm 0.064 = [-0.031,\,0.097]$ — *negative lower endpoint*, nonsense.
- **Clopper–Pearson (correct):** $[0.00084,\,0.1722]$ — strictly inside $[0,1]$, asymmetric (longer on the upper side, as is natural for $\hat p$ near zero).

**Sanity check on the actual master dataset ($n=140$, $X=108$, rule of 5 holds).** Wald gives $[0.7019,\,0.8410]$ (subpart a); Clopper–Pearson gives $\approx[0.6932,\,0.8400]$ — essentially the same (off by $<0.01$ at each endpoint), confirming that when 3a is valid, switching to 3c brings no benefit, only a slightly wider conservative interval.

```r
# Tiny-n variant: Wald fails, CP works
x  <- 1;  n  <- 30
phat <- x/n
phat + c(-1,1)*qnorm(0.975)*sqrt(phat*(1-phat)/n)   # Wald: [-0.031, 0.097]  -- negative!
binom.test(x, n)$conf.int                            # CP: 0.00084 0.1722
# Master dataset (rule of 5 holds): CP and Wald are nearly identical
binom.test(108, 140)$conf.int                        # ~ [0.6932, 0.8400]
```

**Trade-off.** Clopper–Pearson is **conservative** — its coverage is *at least* $1-\alpha$, often strictly more — so it is slightly wider than Wald when both are applicable. Wilson's score CI (`prop.test(x, n, correct = FALSE)`) is the middle ground (close-to-nominal coverage, almost as easy as Wald). For exam purposes:

**Default to Wald when the rule of 5 holds; switch to Clopper–Pearson (`binom.test`) when it does not.**

</details>

<details class="master-subpart">
<summary>(d) Warnings &amp; cross-references — the CI/test SE distinction for proportions</summary>

**Critical: the CI SE and the test SE for proportions are different formulas.** They look nearly identical and *both* live in the universal templates — but they plug in different probabilities.

**CI for $p$** (this row, 3a): $\widehat{SE}_{CI}(\hat p) = \sqrt{\hat p(1-\hat p)/n}$ — plug in the **sample** $\hat p$.

**Test of $H_0: p = p_0$** (row 3 of g14's master table, see `g14a` (c)): $\widehat{SE}_{H_0}(\hat p) = \sqrt{p_0(1-p_0)/n}$ — plug in the **null** $p_0$.

The two SEs only coincide when $\hat p = p_0$; otherwise they differ slightly and the CI-test duality is *not* an exact identity for proportions (it *is* for means with known $\sigma$). The standard convention in this course is: **use $p_0$ in the SE of the one-sample $z$-test**, not $\hat p$.

**Why two SEs?** Both are "correct" choices of where to evaluate the Bernoulli variance $p(1-p)$:
- For a CI we want a SE that does not depend on the unknown $p$, so the natural and only choice is the **plug-in** $\hat p$ (Slutsky justifies it asymptotically).
- For a test we are **conditioning on $H_0$ being true**, so the variance is *known* under $H_0$ — namely $p_0(1-p_0)/n$ — and using it sharpens the null distribution and is the textbook prescription.

**Two more cross-references the row's horizontal cells repeatedly touch.**
- **Two-proportion CI vs two-proportion test (`g13d` vs `g14b`).** Same dichotomy on steroids: the CI for $p_A - p_B$ uses the **unpooled** SE $\sqrt{\hat p_A(1-\hat p_A)/n_A + \hat p_B(1-\hat p_B)/n_B}$; the test of $H_0:p_A=p_B$ uses the **pooled** SE built from $\hat p_{\rm pool} = (X_A+X_B)/(n_A+n_B)$. CI and test will *not* coincide numerically (they do for any other case). See `g13d` (CI side) and `g14b` (test side) for the full discussion.
- **Bias from sampling design is not a CI problem to fix.** No SE formula can repair a non-representative sample. Ex `6_1d` (self-selected social-channel survey) shows how a "$95\%$" CI built on a biased sample undercovers severely (~$42\%$ instead of $95\%$). The CLT and the rule of 5 cure *sampling-noise* uncertainty only — never *selection* bias.

</details>

---

**Summary.** g13b is **row 3** of the universal CI table. The default operational rule is the **Wald CI** $\hat p \pm z_{1-\alpha/2}\sqrt{\hat p(1-\hat p)/n}$ (Case 3a), valid whenever both $n\hat p\ge 5$ and $n(1-\hat p)\ge 5$. The two auxiliary tools the row repeatedly demands are **sample-size planning** $n \ge (z_{1-\alpha/2}/(2m))^2$ at worst case $p=0.5$ (Case 3b) and the **exact Clopper–Pearson** fallback `binom.test(x, n)` when the rule of 5 fails (Case 3c). The lone subtlety to remember: **the CI uses $\hat p$ in the SE, the test uses $p_0$** — they are *not* exact duals for proportions (see `g14a` for the test side, `g13d`/`g14b` for the two-proportion analogue). Continue to **`g13c`** for two-means, **`g13d`** for two-proportions, **`g13e`** for paired data, **`g13f`** for the underlying unbiased estimators.
""",
    "images": ["statistics/images/master/master_g13b_ai.png"],
}

master_exercises["g13c_ci_diff_means"] = {
    "title": "Master Exam — CI for the difference of two independent means (rows 4–6 of the universal table)",
    "content": r"""## Setup — running dataset for every numeric example below

A workforce-development study compares two training programs **A** and **B** on the response `Performance` (a 0–100 score). Two **independent** random samples of participants are drawn:

| Group | $n$ | $\bar x$ | $s$ |
|---|---:|---:|---:|
| Activity.type **A** | $n_A = 58$  | $\bar x_A = 78.17$ | $s_A = 6.66$ |
| Activity.type **B** | $n_B = 380$ | $\bar x_B = 82.74$ | $s_B = 6.53$ |

Let $\mu_A,\mu_B$ denote the population means and $\sigma_A^2,\sigma_B^2$ their variances. The parameter of interest is the **difference** $\delta := \mu_A - \mu_B$. Throughout the entry we reuse *one* dataset across all three variance regimes, so the only moving pieces are the **SE** and the **critical-value family**.

<details class="master-subpart" open>
<summary>(a) <strong>Case 4</strong> — $\sigma_A, \sigma_B$ <em>known</em> (z-interval)</summary>

**Setting.** $X_{A,i}\overset{\text{iid}}{\sim}(\mu_A,\sigma_A^2)$ for $i=1,\ldots,n_A$ and $X_{B,j}\overset{\text{iid}}{\sim}(\mu_B,\sigma_B^2)$ for $j=1,\ldots,n_B$, the two samples **independent**, both $\sigma_A,\sigma_B$ **known** (problem statement or textbook value).

**Pivot.** Because independence makes variances add and the $\sigma$'s are known,
$$\operatorname{Var}(\bar X_A - \bar X_B) \;=\; \operatorname{Var}(\bar X_A) + \operatorname{Var}(\bar X_B) \;=\; \frac{\sigma_A^2}{n_A} + \frac{\sigma_B^2}{n_B}.$$
If the $X_{A,i},X_{B,j}$ are normal, $Z = \dfrac{(\bar X_A - \bar X_B) - (\mu_A-\mu_B)}{\sqrt{\sigma_A^2/n_A + \sigma_B^2/n_B}} \sim \mathcal N(0,1)$ **exactly**; for non-normal data the CLT delivers the same standard-normal limit for $n_A,n_B \gtrsim 30$.

**Deriving the CI.** Inverting $\Pr(-z_{1-\alpha/2} \le Z \le z_{1-\alpha/2}) = 1-\alpha$ for $\mu_A-\mu_B$ gives

$$\boxed{\;\;CI_{1-\alpha}(\mu_A-\mu_B) \;=\; (\bar x_A - \bar x_B) \;\pm\; z_{1-\alpha/2}\,\sqrt{\frac{\sigma_A^2}{n_A} + \frac{\sigma_B^2}{n_B}}\;\;}\qquad (\sigma_A,\sigma_B\ \text{known})$$

**Worked numbers on the running dataset** with the textbook value $\sigma_A = \sigma_B = 6.6$, $n_A=58,n_B=380$, $\bar x_A-\bar x_B = -4.57$, $\alpha = 0.10$:
- Exact SE: $\sqrt{6.6^2/58 + 6.6^2/380} = 6.6\sqrt{1/58 + 1/380} = 6.6\sqrt{0.01987} \approx 0.9303$.
- Critical value: $z_{0.95} = 1.645$.
- $ME_{90} = 1.645 \cdot 0.9303 \approx 1.530$.
- $CI_{90} = -4.57 \pm 1.530 = [-6.10,\;-3.04]$.

```r
nA <- 58;   nB <- 380
xbarA <- 78.17;  xbarB <- 82.74
delta_hat <- xbarA - xbarB                                # -4.57
sigmaA <- 6.6;  sigmaB <- 6.6                             # textbook "known"
alpha <- 0.10
SE_known <- sqrt(sigmaA^2/nA + sigmaB^2/nB);  SE_known    # ~ 0.9303
zcrit    <- qnorm(1 - alpha/2);                zcrit      # 1.645
ME       <- zcrit * SE_known;                  ME         # ~ 1.530
c(delta_hat - ME, delta_hat + ME)                         # [-6.10, -3.04]
```

**Why Case 4 is the simplest.** No degrees of freedom, no $t$ inflation, no plug-in noise — $z$-quantiles are *the* reference distribution. In real exam practice this row appears only when the problem statement *literally* gives you $\sigma_A,\sigma_B$ (`5_1f` part f3, `5_7a`, `5_7b` part (i), `6_4a` part (b)); otherwise you must estimate the variances and use Case 5 or 6.

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) <strong>Case 5</strong> — $\sigma_A,\sigma_B$ <em>unknown but ASSUMED EQUAL</em> (pooled t)</summary>

**Setting.** Same i.i.d. independent samples, $\sigma_A^2,\sigma_B^2$ **unknown** but **Levene fails to reject** $H_0:\sigma_A^2=\sigma_B^2$ (or the problem instructs us to assume equality). Estimate the common variance $\sigma^2$ by pooling the two sample variances by their degrees of freedom.

**Pivot.** Under $\sigma_A^2=\sigma_B^2=\sigma^2$ and i.i.d. normal samples,
$$T \;=\; \frac{(\bar X_A - \bar X_B) - (\mu_A-\mu_B)}{\sqrt{s_p^2\bigl(\tfrac{1}{n_A}+\tfrac{1}{n_B}\bigr)}} \;\sim\; t_{n_A+n_B-2}\;\text{ exactly},\qquad s_p^2 \;=\; \frac{(n_A-1)s_A^2 + (n_B-1)s_B^2}{n_A+n_B-2}.$$
The pooled variance is the **best (lowest-variance) unbiased estimator** of the common $\sigma^2$ — it spends *all* $n_A+n_B-2$ degrees of freedom on a single estimate, more than either $s_A^2$ or $s_B^2$ alone.

**Deriving the CI.** Identical inversion to Case 4 with $T$ in place of $Z$:

$$\boxed{\;\;CI_{1-\alpha}(\mu_A-\mu_B) \;=\; (\bar x_A - \bar x_B) \;\pm\; t_{1-\alpha/2,\,n_A+n_B-2}\,\sqrt{s_p^2\!\left(\frac{1}{n_A} + \frac{1}{n_B}\right)}\;\;}$$

**Worked numbers on the running dataset.** $s_A^2 = 6.66^2 = 44.36$, $s_B^2 = 6.53^2 = 42.64$, $\alpha = 0.10$:
- Pooled variance: $s_p^2 = \dfrac{57\cdot 44.36 + 379\cdot 42.64}{436} = \dfrac{2528.5 + 16160.6}{436} \approx 42.86$, so $s_p \approx 6.547$.
- Pooled SE: $\widehat{SE}_{\text{pool}} = \sqrt{42.86\cdot(1/58 + 1/380)} = \sqrt{42.86\cdot 0.01987} \approx 0.923$.
- df: $n_A+n_B-2 = 436$. Critical value $t_{0.95,\,436} \approx 1.648$ (essentially $z_{0.95}=1.645$ at this df).
- $ME_{90}^{\text{pool}} = 1.648\cdot 0.923 \approx 1.521$.
- $CI_{90}^{\text{pool}} = -4.57 \pm 1.521 = [-6.09,\;-3.05]$.

Both endpoints lie **strictly below 0**, so the 90% CI rejects $H_0:\mu_A=\mu_B$ in favour of $\mu_A < \mu_B$: program A is associated with lower mean Performance than program B by between roughly $3$ and $6$ points.

```r
sA <- 6.66;  sB <- 6.53
s2p     <- ((nA-1)*sA^2 + (nB-1)*sB^2) / (nA+nB-2);  s2p     # ~ 42.86
SE_pool <- sqrt(s2p * (1/nA + 1/nB));                SE_pool # ~ 0.923
df_pool <- nA + nB - 2;                              df_pool # 436
tc_pool <- qt(1 - alpha/2, df = df_pool);            tc_pool # 1.648
ME_pool <- tc_pool * SE_pool;                        ME_pool # ~ 1.521
c(delta_hat - ME_pool, delta_hat + ME_pool)                  # [-6.09, -3.05]

# Same numbers from raw data:
# t.test(Performance ~ Activity.type, data = Performance, var.equal = TRUE)$conf.int
# CI.diffmean(...A..., ...B..., type='independent',
#             var.test=TRUE, conf.level=0.90)   # prints pooled AND Welch AND Levene
```

**Bias warning.** If equality of variances *fails*, $s_p^2$ is biased for either group variance: it over-states $\min(\sigma_A^2,\sigma_B^2)$ and under-states $\max(\sigma_A^2,\sigma_B^2)$. The bias propagates into the SE, and the resulting CI loses its nominal coverage — under-coverage if the larger variance sits in the *smaller* sample, over-coverage in the opposite case. This is why we check Levene **first** rather than assuming pooling is safe.

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (c) <strong>Case 6</strong> — $\sigma_A,\sigma_B$ <em>unknown and DIFFERENT</em> (Welch t)</summary>

**Setting.** Same i.i.d. independent samples, $\sigma_A^2,\sigma_B^2$ **unknown** and Levene **rejects** equality (or we adopt Welch as the safe default — see the decision matrix at the end of (d)). We refuse to pool; each sample variance estimates its *own* population variance.

**Pivot.** The "natural" SE plugs in $s_A^2,s_B^2$ separately. The resulting standardised statistic does **not** follow a Student-$t$ exactly — Welch and Satterthwaite showed it is approximately $t_{\nu^W}$ where the effective df is the moment-matching solution
$$\boxed{\;\;\nu^W \;=\; \frac{\left(\dfrac{s_A^2}{n_A} + \dfrac{s_B^2}{n_B}\right)^{2}}{\dfrac{(s_A^2/n_A)^{2}}{n_A-1} + \dfrac{(s_B^2/n_B)^{2}}{n_B-1}}\;\;}$$
fractional and bounded by $\min(n_A-1, n_B-1) \le \nu^W \le n_A+n_B-2$.

**Deriving the CI.**

$$\boxed{\;\;CI_{1-\alpha}(\mu_A-\mu_B) \;=\; (\bar x_A - \bar x_B) \;\pm\; t_{1-\alpha/2,\,\nu^W}\,\sqrt{\frac{s_A^2}{n_A} + \frac{s_B^2}{n_B}}\;\;}$$

**Worked numbers on the running dataset** with $\alpha = 0.10$:
- Per-group variance contributions: $s_A^2/n_A = 44.36/58 \approx 0.7648$; $s_B^2/n_B = 42.64/380 \approx 0.1122$.
- Welch SE: $\widehat{SE}_W = \sqrt{0.7648 + 0.1122} = \sqrt{0.8770} \approx 0.9365$.
- Satterthwaite df: $\nu^W = \dfrac{0.8770^{2}}{0.7648^{2}/57 + 0.1122^{2}/379} = \dfrac{0.7692}{0.01026 + 0.0000332} \approx 74.7$.
- Critical value: $t_{0.95,\,74.7} \approx 1.666$.
- $ME_{90}^{W} = 1.666 \cdot 0.9365 \approx 1.560$.
- $CI_{90}^{W} = -4.57 \pm 1.560 = [-6.13,\;-3.01]$.

**Comparison with Case 5.** Same point estimate, same sign of the conclusion (program A's mean Performance is below B's at 90%), almost the same numbers — Welch is **0.04 pp** wider on the half-width and 0.04 units broader on each endpoint. Welch "spent" $436 - 74.7 \approx 361$ df for the privilege of dropping the equal-variance assumption (visible only at very small $n$; at our $n$ the cost is negligible).

```r
SE_W <- sqrt(sA^2/nA + sB^2/nB);   SE_W                       # ~ 0.9365
num  <- (sA^2/nA + sB^2/nB)^2
den  <- (sA^2/nA)^2/(nA-1) + (sB^2/nB)^2/(nB-1)
nu_W <- num / den;                  nu_W                      # ~ 74.7
tc_W <- qt(1 - alpha/2, df = nu_W); tc_W                      # ~ 1.666
ME_W <- tc_W * SE_W;                ME_W                      # ~ 1.560
c(delta_hat - ME_W, delta_hat + ME_W)                         # [-6.13, -3.01]

# Same from raw data (Welch is the default in R):
# t.test(Performance ~ Activity.type, data = Performance,
#        var.equal = FALSE, conf.level = 0.90)$conf.int
```

**Why Welch is the universal safe default.** When the **larger** variance sits in the **larger** sample, pooling **over-states** the SE (intervals are *too wide*, coverage above nominal). When the larger variance sits in the **smaller** sample, pooling **under-states** the SE (intervals *too narrow*, coverage *below* nominal — actively dangerous). Welch is robust to both regimes; the only cost is a possibly smaller df, which is invisible once $n_A,n_B$ are large.

</details>

<details class="master-subpart">
<summary>(d) Side-by-side comparison: Case 4 vs Case 5 vs Case 6</summary>

All three cases on the **same running dataset** ($\bar x_A - \bar x_B = -4.57$, $n_A=58$, $n_B=380$, 90% CI):

| Quantity | **Case 4** ($\sigma$ known) | **Case 5** (pooled $t$) | **Case 6** (Welch $t$) |
|---|---:|---:|---:|
| Estimator $\widehat\theta$ | $\bar x_A - \bar x_B = -4.57$ | $\bar x_A - \bar x_B = -4.57$ | $\bar x_A - \bar x_B = -4.57$ |
| $\widehat{SE}$ formula | $\sqrt{\sigma_A^2/n_A + \sigma_B^2/n_B}$ | $\sqrt{s_p^2(1/n_A+1/n_B)}$ | $\sqrt{s_A^2/n_A + s_B^2/n_B}$ |
| $\widehat{SE}$ value | $0.930$ | $0.923$ | $0.937$ |
| Critical-value family | $z_{1-\alpha/2}$ | $t_{1-\alpha/2,\,n_A+n_B-2}$ | $t_{1-\alpha/2,\,\nu^W}$ |
| df | — | $436$ (integer) | $\approx 74.7$ (fractional) |
| Critical value (90%) | $z_{0.95} = 1.645$ | $t_{0.95,\,436} \approx 1.648$ | $t_{0.95,\,74.7} \approx 1.666$ |
| $ME_{90}$ | $1.530$ | $1.521$ | $1.560$ |
| **90% CI** | $[-6.10,\;-3.04]$ | $[-6.09,\;-3.05]$ | $[-6.13,\;-3.01]$ |
| Required assumption | $\sigma_A,\sigma_B$ given | $\sigma_A^2 = \sigma_B^2$ | none on variance ratio |
| **When to use** | textbook / problem states $\sigma$'s | Levene $p > \alpha$ | Levene $p \le \alpha$, or safe default |

Three rows that look almost identical here because (i) the per-group variance estimates are very close ($s_A^2/s_B^2 \approx 1.04$, deep in the safe band), (ii) the samples are both large, and (iii) the textbook $\sigma$ used in Case 4 was set close to the sample SDs. **When variances are very unequal and/or sample sizes very unbalanced, the three rows diverge dramatically** — that is the operational reason to know which one you are applying.

**Decision matrix when no Levene output is given.**

| Situation | Use |
|---|---|
| Problem *literally* states $\sigma_A,\sigma_B$ as known | **Case 4** ($z$) |
| Variances unknown, Levene $p > \alpha$ | **Case 5** (pooled) |
| Variances unknown, Levene $p \le \alpha$ | **Case 6** (Welch) |
| Variances unknown, no Levene available, ratio $s_A^2/s_B^2 \in [\tfrac12, 2]$, balanced $n$ | Either — virtually identical |
| Variances unknown, ratio $> 3$ or $n$'s very unbalanced | **Case 6** (Welch, mandatory) |
| R default (`t.test`) | **Case 6** (Welch, `var.equal = FALSE`) |

![Master illustration](statistics/images/master/master_g13c_ai.png)

</details>

<details class="master-subpart">
<summary>(e) Cross-references and pitfalls</summary>

- **Universal CI recipe and the master case table** — see **`g13a`** (rows 1–8 of the table; this entry owns rows 4, 5, 6).
- **One-mean CI** ($\mu$, single group, σ known/unknown — rows 1–2 of the table) — see **`g13a`**.
- **One-proportion CI** (row 3) — see **`g13b`**.
- **Difference of two proportions CI** (row 7) — see **`g13d`**. *Pitfall:* the proportion-difference CI uses the **unpooled** SE; the pooled SE is *only* for the two-sample proportion **test** in `g14b`.
- **Paired-sample CI** (row 8 — "this is NOT independent") — see **`g13e`**. *Pitfall:* if the two columns come from the *same* subject (Pre/Post, Before/After), the independence assumption of rows 4–6 fails — the SE must include the within-pair covariance term $-2\,s_{AB}$, which **shrinks** the SE when $\rho > 0$ (typical) and inflates it when $\rho < 0$. Treating paired data as independent over-states the SE by a factor of roughly $1/\sqrt{1-\rho}$.
- **Two-sample hypothesis tests for $\mu_A$ vs $\mu_B$** (analogue of rows 4–6 for testing rather than estimating) — see **`g14b`**. The Case 4/5/6 split, the Levene branch, and the SE formulas carry over **verbatim**; only the pivot is recentred on the null value $\delta_0$ and the decision rule is "reject if $|T| > c_{1-\alpha/2}$" instead of "build $\hat\delta \pm c\,\widehat{SE}$".
- **Levene's test machinery** (null distribution, $F$-table, when to reject) — see **`g14d`**. From here we only **read** Levene's $p$ and pick the row.
- **Underlying unbiased estimators** ($\bar X, S^2, S_p^2$ and their sampling SEs) — see **`g13f`**.

**Bookmark.** The Case 4–6 split + Levene branch reappears identically in every two-mean problem (CI or test) you will see this semester. Master the table here once.

</details>

---

### Summary table (running dataset, $\hat\delta = -4.57$, 90% CI)

| Quantity | Case 4 (known $\sigma$, $z$) | Case 5 (pooled $t$) | Case 6 (Welch $t$) |
|---|---:|---:|---:|
| $\widehat{SE}$ | $0.930$ | $0.923$ | $0.937$ |
| df | — | $436$ | $\approx 74.7$ |
| Critical value | $z_{0.95} = 1.645$ | $t_{0.95,\,436} \approx 1.648$ | $t_{0.95,\,74.7} \approx 1.666$ |
| $ME_{90}$ | $1.530$ | $1.521$ | $1.560$ |
| **90% CI** | $[-6.10,\;-3.04]$ | $[-6.09,\;-3.05]$ | $[-6.13,\;-3.01]$ |

**One-line take-away.** Three rows of the universal CI table, one estimator ($\bar X_A - \bar X_B$), one decision rule (Levene): the only moving parts are the **SE** and the **df / critical-value family**. Memorise the table, run Levene, pick the row, plug.
""",
    "images": ["statistics/images/master/master_g13c_ai.png"],
}

master_exercises["g13d_ci_diff_prop"] = {
    "title": "Master Exam — CI for difference of two proportions (row 7 of the universal table)",
    "content": r"""## Setup — running dataset for every numeric example below

A retailer compares two regions on the share of customers who buy the **more expensive** product in the cleaning category (`category == "cleaning"`, "first product" = the premium one). Two **independent** random samples of customers are pulled:

| Region | Sample size | Sample proportion choosing the expensive product |
|---|---:|---:|
| NorthWest (group 1) | $n_1 = 278$ | $\hat p_1 = 0.640$ |
| NorthEast (group 2) | $n_2 = 189$ | $\hat p_2 = 0.418$ |

Let $p_1, p_2$ denote the corresponding population proportions and $\delta := p_1 - p_2$. The samples are drawn from disjoint customer pools so $\operatorname{Cov}(\hat p_1, \hat p_2) = 0$ — variances *add*, no covariance term. We will reuse this **one dataset** throughout the entry.

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) <strong>Case 7</strong> — Large-sample Wald CI (unpooled SE)</summary>

**Setting.** Two **independent** Bernoulli samples: $Y_i^{(A)} \overset{\text{iid}}{\sim} \text{Bernoulli}(p_A)$ for $i=1,\ldots,n_A$, and analogously $Y_j^{(B)}$ for the second group. The two sums $X_A = \sum_i Y_i^{(A)} \sim \text{Bin}(n_A, p_A)$ and $X_B \sim \text{Bin}(n_B, p_B)$ are independent, and so are $\hat p_A = X_A/n_A$ and $\hat p_B = X_B/n_B$.

**Pivot.** Independence makes variances add, so
$$\operatorname{Var}(\hat p_A - \hat p_B) \;=\; \frac{p_A(1-p_A)}{n_A} + \frac{p_B(1-p_B)}{n_B}.$$
By the CLT applied to each $\hat p_i$ and the independence of the two samples,
$$\frac{(\hat p_A - \hat p_B) - (p_A - p_B)}{\sqrt{p_A(1-p_A)/n_A + p_B(1-p_B)/n_B}} \;\overset{d}{\to}\; \mathcal N(0,1).$$
Replacing the unknown $p_i$ in the SE by $\hat p_i$ (plug-in, valid by Slutsky) gives the **unpooled estimated SE**
$$\widehat{SE}(\hat\delta) \;=\; \sqrt{\dfrac{\hat p_A(1-\hat p_A)}{n_A} + \dfrac{\hat p_B(1-\hat p_B)}{n_B}}.$$

**Deriving the CI.** Inverting $\Pr\bigl(-z_{1-\alpha/2}\le Z \le z_{1-\alpha/2}\bigr) = 1-\alpha$ for $\delta = p_A - p_B$:

$$\boxed{\;\;CI_{1-\alpha}(p_A - p_B) \;=\; (\hat p_A - \hat p_B) \;\pm\; z_{1-\alpha/2}\,\sqrt{\dfrac{\hat p_A(1-\hat p_A)}{n_A} + \dfrac{\hat p_B(1-\hat p_B)}{n_B}}\;\;}$$

**Worked numbers on the running dataset** with $\hat p_1 = 0.640$, $n_1 = 278$, $\hat p_2 = 0.418$, $n_2 = 189$, $\alpha = 0.10$:

- **Point estimate:** $\hat\delta = 0.640 - 0.418 = 0.222$.
- **CLT check:** $n_1\hat p_1 = 178$, $n_1(1-\hat p_1) = 100$, $n_2\hat p_2 \approx 79$, $n_2(1-\hat p_2) \approx 110$ — all $\gg 5$.
- **Variance contributions:** $\hat p_1(1-\hat p_1)/n_1 = 0.640\cdot 0.360/278 \approx 0.000829$; $\hat p_2(1-\hat p_2)/n_2 = 0.418\cdot 0.582/189 \approx 0.001287$.
- **Unpooled SE:** $\widehat{SE}(\hat\delta) = \sqrt{0.000829 + 0.001287} = \sqrt{0.002116} \approx 0.0460$.
- **Critical value:** $z_{0.95} = 1.6449$.
- **$ME_{90} = 1.6449 \cdot 0.0460 \approx 0.0757$.**
- **$CI_{90}(\delta) = 0.222 \pm 0.0757 = [0.147,\,0.298]$.**

**Interpretation.** With 90% confidence the true gap $p_1 - p_2$ lies between $14.7$ and $29.8$ percentage points in favour of the NorthWest region. The interval is **strictly positive** ($0 \notin [0.147, 0.298]$) ⇒ at the 10% level the two regional proportions are **not** plausibly equal, and the NorthWest share is the larger of the two.

```r
n1 <- 278;  p1 <- 0.640
n2 <- 189;  p2 <- 0.418
# CLT validity check (rule of 5)
n1*p1;  n1*(1-p1);  n2*p2;  n2*(1-p2)      # 178 ; 100 ; ~79 ; ~110  -- all >> 5
dhat    <- p1 - p2;                  dhat    # 0.222
SE_diff <- sqrt( p1*(1-p1)/n1 + p2*(1-p2)/n2 )
SE_diff                                       # ~ 0.0460
z90     <- qnorm(0.95);              z90      # 1.6449
ME90    <- z90 * SE_diff;            ME90     # ~ 0.0757
c(dhat - ME90, dhat + ME90)                   # [0.147, 0.298]

# Course helper (BAS package):
# CI.diffprop(x, y, success = "expensive", conf.level = 0.90)
# Base R equivalent (Wilson, slightly different formula):
# prop.test(c(round(p1*n1), round(p2*n2)), c(n1, n2),
#           conf.level = 0.90, correct = FALSE)$conf.int
```

**Reading the margin of error backwards.** Just as for one-mean and one-proportion CIs, the Wald CI is symmetric around $\hat\delta$, so $\hat\delta = (L+U)/2$, $ME = (U-L)/2$, and $\widehat{SE} = ME/z$. From any printed CI we recover the point estimate, the half-width and the SE without raw data.

**Effect of changing the confidence level.** $\widehat{SE}$ does not depend on $\alpha$; only the reliability factor does. At $1-\alpha = 99\%$, $z_{0.995}/z_{0.95} = 2.576/1.645 \approx 1.57$ ⇒ the CI grows by about $57\%$ in width. On our dataset: $CI_{99} = 0.222 \pm 2.576\cdot 0.0460 = 0.222 \pm 0.1185 = [0.103,\,0.340]$ — still excludes 0, so the regional gap survives a stricter confidence level.

**Effect of changing the sample sizes.** $\widehat{SE} \propto \sqrt{1/n_A + 1/n_B}$ at fixed $\hat p$'s — quadrupling **both** sample sizes halves the SE and halves the CI width; halving the desired width quadruples both required sample sizes (see subpart (b) for the explicit planning formula). Imbalanced $n$'s waste precision: if one sample is small, $\widehat{SE}$ is dominated by its $\hat p_i(1-\hat p_i)/n_i$ term regardless of how large the other sample is.

![Master illustration — 90% Wald CI for the difference of two proportions](statistics/images/master/master_g13d_ai.png)

</details>

<details class="master-subpart">
<summary>(b) Sample-size planning for a diff-prop CI (worst-case formula)</summary>

Pre-data, fix a target half-width $m$ at confidence $1-\alpha$ and **balanced** sample sizes $n_A = n_B = n$. Inverting the ME formula gives
$$ME \;=\; z_{1-\alpha/2}\,\sqrt{\frac{p_A(1-p_A) + p_B(1-p_B)}{n}} \;\le\; m \;\Longleftrightarrow\; n \;\ge\; \left(\frac{z_{1-\alpha/2}}{m}\right)^{\!2}\,[p_A(1-p_A) + p_B(1-p_B)].$$

Since $p_A, p_B$ are unknown at the design stage, take the **worst case** $p_A = p_B = 0.5$ which maximises $p(1-p)$ at $0.25$, so the bracket is $\le 0.5$:

$$\boxed{\;\;n \;\ge\; \dfrac{1}{2}\left(\dfrac{z_{1-\alpha/2}}{m}\right)^{\!2}\;\;}\qquad (\text{per group, worst case}\ p_A=p_B=0.5)$$

Round **up** to the next integer. This is *twice* the one-proportion worst-case formula $n \ge (z/(2m))^2$ of `g13b` — diff-prop spends $n$ on each of two samples to estimate one extra quantity.

**Worked numbers** for a 90% CI of half-width $m = 0.03$ on the running dataset's parameter:
$$n \;\ge\; \tfrac{1}{2}(1.6449/0.03)^2 \;=\; \tfrac{1}{2}\cdot 3006.5 \;\approx\; 1503.2 \;\Longrightarrow\; n = 1504 \text{ per group}.$$

With **pilot info** $\hat p_1 \approx 0.640,\,\hat p_2\approx 0.418$ (and assuming the pilot proportions are good design-stage proxies) the bracket is $0.640\cdot 0.360 + 0.418\cdot 0.582 \approx 0.4737$ instead of $0.5$ — saving roughly $5\%$ of the worst-case $n$. The saving is largest when both pilot $\hat p_i$ are far from $0.5$ (e.g. each near $0.1$ saves $\approx 28\%$).

```r
m <- 0.03;  z <- qnorm(0.95)
# Worst case (no prior info)
n_wc <- 0.5 * (z/m)^2;  ceiling(n_wc)        # 1504 per group
# Pilot
p1 <- 0.640;  p2 <- 0.418
bracket <- p1*(1-p1) + p2*(1-p2)
n_pilot <- (z/m)^2 * bracket;  ceiling(n_pilot)   # ~ 1424
```

The mean-side analogue (worst case for one-mean planning) lives in **`g13a` (c)**, the one-proportion analogue in **`g13b` (b)**.

</details>

<details class="master-subpart">
<summary>(c) Side-by-side: CI SE (unpooled) vs Test SE (pooled) — the G13 ↔ G14 split</summary>

The two formulas look almost identical and **both** live in the universal templates of `g13a` and `g14a` — but they plug in different probabilities. Memorise this table once:

| Quantity | **CI for $p_A - p_B$** (here, g13d) | **Test of $H_0: p_A = p_B$** (g14b) |
|---|---|---|
| Estimator | $\hat p_A - \hat p_B$ | $\hat p_A - \hat p_B$ |
| SE formula | $\widehat{SE}_{\text{CI}} = \sqrt{\dfrac{\hat p_A(1-\hat p_A)}{n_A} + \dfrac{\hat p_B(1-\hat p_B)}{n_B}}$ &nbsp; (**unpooled**, separate $\hat p_i$) | $\widehat{SE}_{H_0} = \sqrt{\hat p_{\text{pool}}(1-\hat p_{\text{pool}})\!\left(\dfrac{1}{n_A} + \dfrac{1}{n_B}\right)}$ &nbsp; (**pooled**, common $\hat p$) |
| Pooled proportion | — | $\hat p_{\text{pool}} = \dfrac{X_A + X_B}{n_A + n_B}$ |
| Critical-value family | $z_{1-\alpha/2}$ | $z_{1-\alpha/2}$ |
| When to use | Estimating *how big* the gap is | Testing whether the gap is *zero* |

**Why two SEs?** Both are correct choices of where to evaluate the Bernoulli variances $p_i(1-p_i)$:
- For a **CI**, we want a SE that does not assume any particular value of $\delta$ — the natural and only choice is the **plug-in** $\hat p_i$ for each group separately (Slutsky justifies it asymptotically).
- For a **test** of $H_0: p_A = p_B$, we are *conditioning on $H_0$ being true*, so under $H_0$ both populations share a single common $p$ that is best estimated by **pooling** all $n_A + n_B$ observations: $\hat p_{\text{pool}} = (X_A+X_B)/(n_A+n_B)$. This sharpens the null distribution and is the textbook prescription.

**Numerical illustration on the running dataset.** Successes are $X_1 = 178$ (rounding $278\cdot 0.640$) and $X_2 \approx 79$ ($189\cdot 0.418$), so $\hat p_{\text{pool}} = (178+79)/(278+189) = 257/467 \approx 0.5503$ and
$$\widehat{SE}_{H_0} \;=\; \sqrt{0.5503\cdot 0.4497\cdot (1/278 + 1/189)} \;=\; \sqrt{0.2475\cdot 0.008891} \;\approx\; 0.0469.$$
Compare with $\widehat{SE}_{\text{CI}} \approx 0.0460$ from subpart (a) — they differ by about $2\%$ here (small because $\hat p_1$ and $\hat p_2$ straddle $0.5$ and the data are not far from a common $p$). For a CI we *must* use $0.0460$; for a $z$-test of $H_0: p_1 = p_2$ we *must* use $0.0469$. The discrepancy is small in this dataset but it grows when $\hat p_A$ and $\hat p_B$ are very different.

**Consequence for the CI ⇄ test duality.** For every *other* row of the universal table, the CI and the two-sided test give exactly identical decisions ($\theta_0 \in CI \iff$ fail to reject). For two proportions this duality is **broken numerically**: the CI uses $\widehat{SE}_{\text{CI}}$ and the test uses $\widehat{SE}_{H_0}$, so the two procedures can in principle disagree on borderline cases. They usually agree to two decimals; when they do not, *follow the rule of each procedure* (CI with unpooled, test with pooled) — do not "fix" the discrepancy by mixing formulas.

The one-proportion analogue of this CI/test SE split (Wald SE $\hat p(1-\hat p)/n$ vs null SE $p_0(1-p_0)/n$) is treated in **`g13b` (d)**.

</details>

<details class="master-subpart">
<summary>(d) Cross-references and where each piece is reused</summary>

- **Universal CI recipe and the master case table** — see **`g13a`** (rows 1–8 of the table; this entry owns row 7).
- **One-mean CI** (rows 1–2, $\sigma$ known / unknown) — see **`g13a`**.
- **One-proportion CI** (row 3) — see **`g13b`**. *Bridge:* in the diff-prop CI the SE is built from **two** one-proportion variance terms $\hat p_i(1-\hat p_i)/n_i$ summed under independence — read `g13b` for the per-group anatomy and for the rule-of-5 / Clopper–Pearson fallback when $n_i\hat p_i$ or $n_i(1-\hat p_i) < 5$.
- **Difference of two means CI** (rows 4–6) — see **`g13c`**. Same "$\hat\delta \pm c\,\widehat{SE}$" template, three SEs instead of one (known $\sigma$ / pooled / Welch) and a $t$-critical value (with Levene deciding the SE row).
- **Paired CI** (row 8) — see **`g13e`**. *Pitfall:* paired data require the within-pair SE $s_d/\sqrt n$; treating paired data as two independent samples and applying the unpooled SE of this entry over-states the SE whenever $\rho > 0$ — see `g13e` for the inflation factor and the reduction rule $d_i = x_i - y_i$.
- **Two-sample hypothesis test for $p_A = p_B$** (the test counterpart of this CI) — see **`g14b`**. Same estimator $\hat p_A - \hat p_B$, **pooled** SE under $H_0$. The full warning about *not* mixing pooled and unpooled SEs lives in subpart (c) above.

**Bookmark.** Row 7 has only one CI formula (unpooled Wald). The single thing you must remember is **"CI = unpooled, test = pooled"** — and the four-counts CLT validity check. Everything else is the universal template of `g13a`.

</details>

---

### Summary table (running dataset, $\hat\delta = 0.222$, 90% CI)

| Quantity | Value | Where |
|---|---:|---|
| $\hat p_1$ (NorthWest), $n_1$ | $0.640$, $278$ | Setup |
| $\hat p_2$ (NorthEast), $n_2$ | $0.418$, $189$ | Setup |
| $\hat\delta = \hat p_1 - \hat p_2$ | $0.222$ | (a) |
| Variance contributions $\hat p_i(1-\hat p_i)/n_i$ | $0.000829,\;0.001287$ | (a) |
| Unpooled $\widehat{SE}_{\text{CI}}(\hat\delta)$ | $0.0460$ | (a) |
| Critical value $z_{0.95}$ | $1.6449$ | (a) |
| $ME_{90}$ | $0.0757$ | (a) |
| **90% CI for $\delta$** | $[0.147,\;0.298]$ | (a) |
| Is $0 \in$ CI? | **No** — reject $p_1=p_2$ at 10% | (a) |
| 99% CI (same SE, $z_{0.995}=2.576$) | $[0.103,\;0.340]$ | (a) |
| CLT counts $(n_i\hat p_i, n_i(1-\hat p_i))$ | $(178,100,79,110)$ — all $\ge 5$ | (a) |
| Pooled SE (test only, $\hat p_{\text{pool}}\approx 0.550$) | $0.0469$ | (c) |
| Sample-size planning ($m=0.03$, worst case) | $n \ge 1504$ per group | (b) |

**One-line take-away.** Row 7 of the universal CI table: **unpooled** Wald CI $(\hat p_A - \hat p_B) \pm z_{1-\alpha/2}\sqrt{\hat p_A(1-\hat p_A)/n_A + \hat p_B(1-\hat p_B)/n_B}$, validity gated by all four counts $n_i\hat p_i, n_i(1-\hat p_i) \ge 5$. The pooled SE belongs to the **test** (`g14b`), *never* to the CI. Continue to **`g13e`** for paired data (row 8) and **`g14b`** for the hypothesis test on the same parameter.
""",
    "images": ["statistics/images/master/master_g13d_ai.png"],
}

master_exercises["g13e_ci_paired"] = {
    "title": "Master Exam — CI for paired mean (row 8: reduces to one-mean CI on differences)",
    "content": r"""## Setup — running dataset for every numeric example below

A retail chain runs a targeted **TV-and-online advertising campaign** and wants to quantify its effect on store-level sales. A random sample of $n=23$ stores is drawn; for each store the manager records the **weekly sales** (in €1000s) over two equal-length periods:

- $X_i$ = sales in the week **before** the campaign,
- $Y_i$ = sales in the week **after** the campaign.

The two columns are **matched by store** — each store contributes *both* values — so the design is **paired**, not independent. Define the within-store difference
$$D_i \;=\; Y_i - X_i \;=\; \text{Sales}_{\text{after},i} - \text{Sales}_{\text{before},i}.$$
The chain's analyst summarises the $n=23$ differences as
$$\bar d \;=\; 10.1, \qquad s_d \;=\; 4.2, \qquad n \;=\; 23.$$
*(Individual store-level SDs are $s_X \approx s_Y \approx 8.0$ with strong within-store correlation $\hat\rho \approx 0.86$ — see subpart (b).)*

We reuse this **one dataset** throughout the entry.

<details class="master-subpart" open>
<summary>(a) <strong>Case 8</strong> — Paired CI as one-mean CI on the differences</summary>

**Setting.** Pairs $(X_i, Y_i)$ for $i = 1,\ldots,n$ are i.i.d. across $i$ (random sample of units), with *no* independence assumption *within* a pair. Define $D_i = Y_i - X_i$. Then $D_1,\ldots,D_n$ are i.i.d. with $\mathbb E[D_i] = \mu_Y - \mu_X =: \mu_d$ and
$$\Var(D_i) \;=\; \Var(Y_i) + \Var(X_i) - 2\,\Cov(X_i, Y_i) \;=\; \sigma_Y^2 + \sigma_X^2 - 2\,\rho\,\sigma_X\sigma_Y.$$
The within-pair covariance $\rho\sigma_X\sigma_Y$ does **not vanish** because $X_i, Y_i$ are measured on the *same* unit (same store, same developer, same patient, same game). The mean of the differences has variance
$$\Var(\bar D) \;=\; \frac{\Var(D_i)}{n} \;=\; \frac{\sigma_X^2 + \sigma_Y^2 - 2\rho\sigma_X\sigma_Y}{n}.$$

**Pivot.** Replacing $\Var(D_i)$ by its sample analogue $s_d^2 = \tfrac{1}{n-1}\sum_i (d_i - \bar d)^2$, exactly the one-mean argument of row 2 applies:
$$T \;=\; \frac{\bar D - \mu_d}{s_d/\sqrt n} \;\sim\; t_{n-1} \qquad (\text{exact under }D_i \sim \mathcal N,\ \text{approx by CLT for }n \gtrsim 30).$$

**Deriving the CI.** Inverting $\Pr(-t_{1-\alpha/2,n-1} \le T \le t_{1-\alpha/2,n-1}) = 1-\alpha$ for $\mu_d$:

$$\boxed{\;\;CI_{1-\alpha}(\mu_d) \;=\; \bar d \;\pm\; t_{1-\alpha/2,\,n-1}\,\frac{s_d}{\sqrt n}\;\;}$$

This is row 2 of the universal table, applied verbatim to the new variable $D$. The $z$-vs-$t$ rationale, the meaning of "$1-\alpha$", the symmetry around $\bar d$ ($\hat\theta = (L+U)/2$, $ME = (U-L)/2$) and the $\sqrt n$ collapse of the width are all stated *once and for all* in **`g13a`** — not re-derived here.

**Worked numbers on the running dataset** with $\bar d = 10.1$, $s_d = 4.2$, $n = 23$, $\alpha = 0.05$:

- Paired SE: $\widehat{SE}(\bar D) = s_d/\sqrt n = 4.2/\sqrt{23} \approx 0.876$.
- Critical value: $t_{0.975,\,22} \approx 2.0739$.
- $ME_{95} = 2.0739 \cdot 0.876 \approx 1.816$.
- $CI_{95}(\mu_d) = 10.1 \pm 1.816 = [8.28,\;11.92]$ (in €1000s/week).

Three levels side-by-side:

| Level | $t_{1-\alpha/2,\,22}$ | $ME = t\cdot SE$ | CI for $\mu_d$ |
|---|---:|---:|---|
| 90% | $1.7171$ | $1.504$ | $[\,8.60,\;11.60\,]$ |
| 95% | $2.0739$ | $1.816$ | $[\,8.28,\;11.92\,]$ |
| 99% | $2.8188$ | $2.469$ | $[\,7.63,\;12.57\,]$ |

All three intervals lie strictly **above $0$** — $\mu_d = 0$ is incompatible with the data at every conventional level. Conclusion: *with 99% confidence, the campaign raised weekly store sales by between €7.6k and €12.6k per store.*

```r
n     <- 23
dbar  <- 10.1                                # mean of the within-store differences
sd_d  <- 4.2                                 # sample SD of the differences
se    <- sd_d / sqrt(n);             se      # ~ 0.876
tcrit <- qt(0.975, df = n-1);        tcrit   # 2.0739
ME    <- tcrit * se;                 ME      # 1.816
c(dbar - ME, dbar + ME)                      # 95% CI -> [8.28, 11.92]

# 90% and 99% in one shot
for (cl in c(0.90, 0.95, 0.99)) {
  tc <- qt((1+cl)/2, df = n-1)
  print(c(level = cl, lo = dbar - tc*se, hi = dbar + tc*se))
}

# One-shot equivalent on raw paired data (after, before):
# CI.diffmean(x = after, y = before, type = "paired", conf.level = 0.95)
# t.test(after, before, paired = TRUE, conf.level = 0.95)$conf.int
```

**Assumptions.** (i) The pairs are i.i.d. across units — design-level requirement, not testable from one sample. (ii) For small $n$ (here $n=23$), $D_i$ approximately **normal** — *not* $X_i$ or $Y_i$ separately, only the differences. For larger $n$ (e.g. `5_4` with $n=315$, `6_8c1` with $n=820$, `6_2b`/`6_18b` with the full Action subset) the CLT makes (ii) automatic and $t_{n-1} \approx z$.

![Master illustration — paired CI is one-mean CI on the differences](statistics/images/master/master_g13e_ai.png)

</details>

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (b) Paired vs independent — SE ratio and what $\rho$ buys you</summary>

The whole point of pairing is the $-2\rho\sigma_X\sigma_Y$ term in $\Var(D_i)$. Compare the two SE flavours at fixed $\sigma_X = \sigma_Y = \sigma$:

| SE flavour | Variance of $\bar D$ | SE of $\bar D$ | Inflation vs paired |
|---|---|---|---|
| **Paired** (correct) | $\dfrac{2\sigma^2(1-\rho)}{n}$ | $\sigma\sqrt{2(1-\rho)/n}$ | $\times 1$ baseline |
| **Independent** (wrong here) | $\dfrac{2\sigma^2}{n}$ | $\sigma\sqrt{2/n}$ | $\times \dfrac{1}{\sqrt{1-\rho}}$ |

The ratio $\widehat{SE}_{\text{indep}}/\widehat{SE}_{\text{paired}} = 1/\sqrt{1-\rho}$ blows up as $\rho \uparrow 1$:

| $\rho$ | $1-\rho$ | Inflation $= 1/\sqrt{1-\rho}$ | Effective $n$ lost |
|---:|---:|---:|---|
| $0.0$ | $1.00$ | $\times 1.00$ | none (paired = indep) |
| $0.5$ | $0.50$ | $\times 1.41$ | half of $n$ |
| $0.7$ | $0.30$ | $\times 1.83$ | $70\%$ of $n$ |
| $0.86$ | $0.14$ | $\times 2.69$ | $86\%$ of $n$ |
| $0.9$ | $0.10$ | $\times 3.16$ | $90\%$ of $n$ |
| $0.99$ | $0.01$ | $\times 10.0$ | $99\%$ of $n$ |

**On the running dataset** ($\hat\rho \approx 0.86$): the wrong independent SE is $\sqrt{(s_X^2 + s_Y^2)/n} = \sqrt{128/23} \approx 2.359$ vs the correct paired SE $\approx 0.876$, a $\times 2.69$ inflation — and the CIs would inflate by the same factor (e.g. the wrong 95% CI would be $10.1 \pm 4.89 = [5.21,\,14.99]$ instead of $[8.28,\,11.92]$). Even here the wrong CI still excludes $0$ because the effect is large, but in marginal cases the wrong analysis flips "significant" $\to$ "inconclusive". The lesson is procedural: **paired design $\Rightarrow$ paired SE.**

**Derivation of $s_d^2$ from $s_X, s_Y, \hat\rho$ (when raw differences not given).** From $\Var(X-Y) = \Var(X) + \Var(Y) - 2\Cov(X,Y)$, the sample analogue is
$$s_d^2 \;=\; s_X^2 + s_Y^2 - 2\,\hat\rho\,s_X s_Y \;=\; s_X^2 + s_Y^2 - 2\,s_{XY},$$
exactly the formula used in `5_4`, `5_6d`, `6_11a`, and `6_17a`. The covariance term is the **only** difference from the independent-samples formula.

```r
sx <- 8.0;  sy <- 8.0;  n <- 23
# Recover implied sample covariance and correlation from sd_d
sxy <- ((sx^2 + sy^2) - 4.2^2) / 2;   sxy        # ~ 55.18
rho <- sxy / (sx * sy);               rho        # ~ 0.862
# Paired vs wrong-independent SE
se_paired <- sqrt(4.2^2 / n);         se_paired  # 0.876
se_indep  <- sqrt((sx^2 + sy^2) / n); se_indep   # 2.359
se_indep / se_paired                              # 2.69
# General inflation factor 1/sqrt(1 - rho)
1 / sqrt(1 - rho)                                 # 2.69 (matches)
```

The independent-samples CI for the difference of means (Welch / pooled, both $t$-based) lives in **`g13c`**. The hypothesis-test counterpart of *this* entry — testing $H_0:\mu_d = 0$ on the paired differences — is **`g14c`**.

</details>

<details class="master-subpart">
<summary>(c) Paired CI from raw data vs from summary $(\bar d, s_d, n)$</summary>

The data arrive in two formats. The reduction $d_i = x_i - y_i$ and the formula $\bar d \pm t_{1-\alpha/2,n-1}\,s_d/\sqrt n$ are identical — only the input differs.

**(c1) Raw paired columns** (as in `6_2a`, `6_2b`, `6_8c1`, `6_18b`).
```r
# Generic template: x = post, y = pre (or x = NA_Sales, y = EU_Sales, etc.)
d     <- x - y
n     <- length(d)
dbar  <- mean(d)
sd_d  <- sd(d)
se    <- sd_d / sqrt(n)
tcrit <- qt((1 + 0.95)/2, df = n-1)
c(dbar - tcrit*se, dbar + tcrit*se)

# Course one-liner (BAS package):
CI.diffmean(x = x, y = y, type = "paired", conf.level = 0.95)
# Base R one-liner:
t.test(x, y, paired = TRUE, conf.level = 0.95)$conf.int
```

**(c2) Summary statistics $(\bar x, \bar y, s_X, s_Y, \hat\rho, n)$** (as in `5_4`, `5_6d`, `6_11a`, `6_17a`). Reconstruct $\bar d$ and $s_d$ first, then proceed identically.
```r
# Generic template with summary inputs (matches Ex 6.11a numbers: blood indicator, n=25)
xbar <- 85.0;  ybar <- 81.4
sx   <- 4.8;   sy   <- 5.9;   rho <- 0.6;   n <- 25
dbar <- xbar - ybar                                  # 3.6
sd_d <- sqrt(sx^2 + sy^2 - 2*rho*sx*sy)              # ~ 4.885
se   <- sd_d / sqrt(n)                               # ~ 0.977
tcrit <- qt(0.95, df = n-1)                          # ~ 1.711  (90% CI)
c(dbar - tcrit*se, dbar + tcrit*se)                  # [1.93, 5.27]
```

**Verify equivalence.** On raw data, `sd(d)` and the summary reconstruction $\sqrt{s_X^2 + s_Y^2 - 2\,\mathrm{cov}(x,y)}$ agree exactly because the sample variance is linear in the within-pair operations:
```r
all.equal(var(d), var(x) + var(y) - 2*cov(x, y))     # TRUE
```

**Reading the margin of error backwards** (also for the row's exam-style entries). Like every other CI in G13, the paired Wald-style interval is symmetric around $\bar d$, so $\bar d = (L+U)/2$, $ME = (U-L)/2$, $\widehat{SE} = ME/t_{1-\alpha/2,n-1}$. From any printed paired CI we can recover $\bar d$, $ME$ and $\widehat{SE}$ without the raw data — useful when an exam states only the interval.

</details>

<details class="master-subpart">
<summary>(d) Cross-references and the "is there a paired CI for two proportions?" question</summary>

- **Universal CI recipe and the master case table** — see **`g13a`** (rows 1–8 of the table; this entry owns row 8). g13e contributes *nothing new* to the template — it is the row-2 $t$-CI applied to a derived variable $D$.
- **One-mean CI rows 1–2 (the row we reduce to)** — see **`g13a`**. The whole machinery of $t_{n-1}$ critical values, the $z$-vs-$t$ rationale, the $\sqrt n$ rate, sample-size planning, and reading $\bar x$ from a printed CI is there and is **not** re-derived here.
- **Difference of two means CI rows 4–6 (independent samples)** — see **`g13c`**. *Pitfall:* `g13c` is the correct destination when units are *not* matched (two disjoint groups, e.g. `6_8d` Full-time vs Freelance, `6_10a` Strategy vs Role-Playing). Using `g13c` on paired data **inflates the SE by $1/\sqrt{1-\rho}$** (subpart (b)).
- **One-proportion CI row 3 and diff-prop CI row 7** — see **`g13b`** and **`g13d`** respectively. *Out-of-scope note.* There is **no standard "paired CI for two proportions"** in this course. If the same units produce two binary outcomes (e.g. did each customer buy product A? did each buy product B?), the matched-pairs object of interest is typically a **McNemar-style** $2\times 2$ table on $(A_i, B_i)$ — handled, when it appears at all, by the McNemar test in `g14d`, not by a g13-style Wald CI. None of the horizontal cells in this row (`5_4`, `5_6d`, `6_2a`, `6_2b`, `6_8c1`, `6_11a`, `6_17a`, `6_18b`) require a paired-proportion CI — all are paired-mean CIs.
- **Paired hypothesis test on the same parameter** ($H_0: \mu_d = 0$) — see **`g14c`**. Same reduction $d_i = x_i - y_i$, then row 2 of the test table; the CI ⇄ test duality holds exactly (CI excludes 0 ⇔ two-sided test rejects).
- **Estimation foundation** (unbiasedness of $\bar D$, sampling SE) — see **`g13f`**.

**Bookmark.** Row 8 is row 2 *in disguise*. The only two things to remember are **(i) reduce $\to d_i$ and use $t_{n-1}$** and **(ii) never use the independent CI on paired data** — the within-pair covariance is exactly what pairing was designed to exploit.

</details>

---

### Summary table (running dataset, $n = 23$, $\bar d = 10.1$, $s_d = 4.2$, $\hat\rho \approx 0.86$)

| Quantity | Value | Where |
|---|---:|---|
| Point estimate $\bar d$ | $10.1$ | Setup / (a) |
| Paired $\widehat{SE}(\bar D) = s_d/\sqrt n$ | $0.876$ | (a) |
| $t_{0.95,22}$ / $t_{0.975,22}$ / $t_{0.995,22}$ | $1.7171\,/\,2.0739\,/\,2.8188$ | (a) |
| **90% CI for $\mu_d$** | $[8.60,\;11.60]$ | (a) |
| **95% CI for $\mu_d$** | $[8.28,\;11.92]$ | (a) |
| **99% CI for $\mu_d$** | $[7.63,\;12.57]$ | (a) |
| Contains $0$? | **No** (all 3 levels) | (a) |
| Wrong independent SE $\sqrt{(s_X^2 + s_Y^2)/n}$ | $2.359$ | (b) |
| SE inflation factor $1/\sqrt{1-\hat\rho}$ | $\times 2.69$ | (b) |
| $s_d$ from summaries: $\sqrt{s_X^2 + s_Y^2 - 2\hat\rho s_X s_Y}$ | $4.2$ ($\checkmark$) | (b) / (c) |

**One-line take-away.** Row 8 of the universal CI table = row 2 applied to $d_i = x_i - y_i$: $\bar d \pm t_{1-\alpha/2,\,n-1}\,(s_d/\sqrt n)$, validity gated by i.i.d. pairs and (small-$n$) normality of $D$. **Never use the independent diff-means CI of `g13c` on paired data** — the within-pair covariance is exactly what pairing was built to exploit. Continue to **`g14c`** for the paired hypothesis test and to **`g13f`** for the underlying estimation theory.
""",
    "images": ["statistics/images/master/master_g13e_ai.png"],
}

master_exercises["g14e_power"] = {
    "title": "Master Exam — Power, Type II error & sample-size effects (row 10 of the universal test table)",
    "content": r"""## Setup — running scenario for every numeric example below

The `NewHired` dataset records `Weeks` = time to find a new job for $n=47$ workers. For this entry we adopt the **σ-known** sub-row of the master table (row 1, $z$-test): the population SD is fixed at $\sigma = 4$ weeks (variance $\sigma^2 = 16$). The career-service manager runs the *one-sided lower* test of `7_1b`:

$$H_0:\ \mu \;\ge\; \mu_0 = 45 \qquad \text{vs}\qquad H_1:\ \mu \;<\; 45,\qquad \alpha = 0.10,\qquad SE \;=\; \sigma/\sqrt n \;=\; 4/\sqrt{47}\;\approx\;0.5835.$$

The **alternative truth** used to compute power is $\mu_1 = 43$ weeks (i.e. the true mean is $2$ weeks below the null boundary). Two `Weeks`-true values from `7_1b` recur below: $\mu = 50$ (well inside $H_0$ — used to read off the *Type-I face* of the same critical region) and $\mu_1 = 43$ (inside $H_1$ — used to read off the *Type-II face* and the *power*). The same one-sided $z$-statistic
$$Z \;=\; \frac{\bar X - \mu_0}{\sigma/\sqrt n} \;\overset{H_0}{\sim}\; \mathcal N(0,1)$$
is fixed throughout — only the true $\mu$, $n$, and $\alpha$ are varied to expose the four levers of power.

<details class="master-subpart" open>
<summary>(a) Type-I, Type-II, power — definitions and the 2×2 table</summary>

A hypothesis test takes a yes/no decision on $H_0$ from random data, so two distinct mistakes are possible. The 2×2 above is the universal classifier-confusion picture in statistical language:

|  | $H_0$ true | $H_0$ false ($\theta=\theta_1$) |
|---|---|---|
| **Reject $H_0$**       | **Type-I error** — prob. $\alpha$ | Correct — prob. $1-\beta(\theta_1)$ = **power** |
| **Don't reject $H_0$** | Correct — prob. $1-\alpha$        | **Type-II error** — prob. $\beta(\theta_1)$ |

* **Type-I rate** $\alpha$ is *chosen* by the analyst (the significance level, e.g. 5% or 10%). It is the *maximum* conditional probability of rejecting $H_0$ when $H_0$ holds, attained at the boundary $\theta=\theta_0$ for a composite $H_0$ (e.g. $\mu\ge\mu_0$). Deeper inside $H_0$ the rejection probability drops toward $0$.
* **Type-II rate** $\beta(\theta_1)$ is *induced* — once you fix $(\alpha,n,\sigma,\theta_1)$, the test mechanically determines $\beta$. It depends on **which** $\theta_1\in H_1$ you condition on: there is no single $\beta$, only a *function* of $\theta_1$.
* **Power** $1-\beta(\theta_1)$ flips $\beta$ around: probability of correctly detecting a true effect of size $\theta_1$. Plotting power as a function of $\theta_1$ traces the **power curve**.

**Worked illustration on `7_1b`.** With $\mu_0=45$, $\sigma=4$, $n=47$, $\alpha=0.10$, the cutoff is $\bar X^* = 45 - 1.2816\cdot(4/\sqrt{47}) \approx 44.252$ (derived in (b)). Ex 7.1b then asks two questions with the **same** critical region but **different** true values:

* **(b1 of 7.1b) True $\mu = 50$ (inside $H_0$, since $50\ge 45$).** Concluding "$\bar X < 45$" — i.e. **rejecting** $H_0$ — would be a **Type-I** mistake because $\mu=50$ belongs to $H_0$. Its probability is $\Pr(\bar X < 44.252\mid \mu=50) = \Phi\bigl((44.252-50)/0.5835\bigr) = \Phi(-9.85) \approx 0$. At $\mu=50$ the rejection probability collapses to **essentially zero** — far below the nominal $\alpha=0.10$, because $\mu=50$ sits 5 weeks above the boundary. This is the *flip side* of $\sup_{\mu\ge\mu_0}\Pr(\text{reject}) = \alpha$: equality is attained *only* at $\mu=\mu_0=45$.
* **(b2 of 7.1b) True $\mu_1 = 43$ (inside $H_1$).** Failing to reject — i.e. $\bar X\ge 44.252$ — would be a **Type-II** mistake. Its probability is $\beta = \Pr(\bar X\ge 44.252\mid\mu=43) = 1-\Phi\bigl((44.252-43)/0.5835\bigr) = 1-\Phi(2.146) \approx 0.0159$. So power $= 1-\beta \approx 0.9841$: the test detects this $2$-week effect about $98\%$ of the time.

```r
mu0 <- 45;  sigma <- 4;  n <- 47;  alpha <- 0.10
se   <- sigma/sqrt(n);                   se          # 0.5835
xstar <- mu0 - qnorm(1-alpha)*se;        xstar       # 44.252
pnorm(xstar, mean = 50, sd = se)                     # ~ 0     (Type I face at mu=50)
1 - pnorm(xstar, mean = 43, sd = se)                 # 0.0159  (beta at mu1=43)
pnorm(xstar, mean = 43, sd = se)                     # 0.9841  (power at mu1=43)
```

</details>

<details class="master-subpart">
<summary>(b) Power computation for one-sided $z$ ($\sigma$ known) — formula + worked numeric example</summary>

For the lower-tail test ($H_1:\mu<\mu_0$):

$$\boxed{\;\;\text{Power}(\mu_1) \;=\; \Phi\!\left(\frac{\bar X^* - \mu_1}{\sigma/\sqrt n}\right) \;=\; \Phi\!\left(-z_{1-\alpha} \;+\; \frac{\mu_0 - \mu_1}{\sigma/\sqrt n}\right),\qquad \bar X^* \;=\; \mu_0 - z_{1-\alpha}\,\frac{\sigma}{\sqrt n}.\;\;}$$

(For upper-tail: replace by $\text{Power}(\mu_1) = 1-\Phi\!\bigl(z_{1-\alpha} - (\mu_1-\mu_0)/(\sigma/\sqrt n)\bigr)$. For two-sided: sum the two tail contributions; the contribution from the wrong-side tail is usually negligible.)

**Worked numbers on `7_1b` (NewHired, $\sigma=4$, $n=47$, $\mu_0=45$, $\alpha=0.10$, $\mu_1=43$).**

* Step 1 — cutoff on the $\bar X$ scale: $\bar X^* = 45 - 1.2816\cdot(4/\sqrt{47}) = 45 - 0.748 \approx 44.252$.
* Step 2 — standardise the cutoff under $H_1$: $z^* = (44.252 - 43)/0.5835 = 2.146$.
* Step 3 — read off the power: $\text{Power} = \Phi(2.146) \approx 0.9841$ ⇒ $\beta\approx 0.0159$.

Geometrically, power is the area under the $H_1$ density $\mathcal N(43,\,SE^2)$ that lies **below** the cutoff $\bar X^* = 44.252$; $\beta$ is the complementary area above.

```r
mu0 <- 45;  sigma <- 4;  n <- 47;  alpha <- 0.10;  mu1 <- 43
se   <- sigma/sqrt(n);                                       # 0.5835
xstar <- mu0 - qnorm(1-alpha)*se;                            # 44.252
pnorm(xstar, mean = mu1, sd = se)                            # power = 0.9841
# Equivalent formulation via the boxed closed form:
pnorm(-qnorm(1-alpha) + (mu0 - mu1)/se)                      # 0.9841
```

![Master illustration — power and the two faces of the cutoff](statistics/images/master/master_g14e_ai.png)

</details>

<details class="master-subpart">
<summary>(c) Sample-size for a target power — formula + worked example targeting power 0.80</summary>

Invert the boxed formula of (b) to solve for $n$ at a target power $1-\beta$. Set $\Phi^{-1}(1-\beta) = z_{1-\beta}$ and rearrange:

$$z_{1-\beta} \;=\; -z_{1-\alpha} + \frac{\mu_0 - \mu_1}{\sigma/\sqrt n} \quad\Longrightarrow\quad \sqrt n \;=\; \frac{(z_{1-\alpha} + z_{1-\beta})\,\sigma}{|\mu_0 - \mu_1|}.$$

$$\boxed{\;\;n \;=\; \left(\frac{(z_{1-\alpha} + z_{1-\beta})\,\sigma}{|\mu_1 - \mu_0|}\right)^2 \quad\text{(one-sided $z$, $\sigma$ known)}\;\;}$$

For the *two-sided* analogue, replace $z_{1-\alpha}$ by $z_{1-\alpha/2}$; for a $t$-test (σ unknown) iterate the formula because the df depends on $n$ — `power.t.test` does this for you in R.

**Worked example on `7_1b`'s setup, targeting power $0.80$ at $\mu_1=43$.** With $\sigma=4$, $|\mu_1-\mu_0|=2$, $\alpha=0.10$ (so $z_{0.90}=1.2816$) and target power $0.80$ (so $z_{0.80}=0.8416$):

$$n \;=\; \left(\frac{(1.2816+0.8416)\cdot 4}{2}\right)^2 \;=\; (2.1232\cdot 2)^2 \;=\; 4.2464^2 \;\approx\; 18.03 \;\Rightarrow\; n \;=\; 19.$$

Always **round up** — $n=18$ gives power just under 80%, $n=19$ gives just over. Cross-check: the table in (d) shows $n=20$ already buys power $\approx 0.83$, consistent with the calculation.

```r
mu0 <- 45;  mu1 <- 43;  sigma <- 4;  alpha <- 0.10;  power_target <- 0.80
zalp  <- qnorm(1-alpha);          zalp     # 1.2816
zbeta <- qnorm(power_target);     zbeta    # 0.8416 = z_{1-beta}
n_req <- ((zalp + zbeta)*sigma/abs(mu1 - mu0))^2;  ceiling(n_req)   # 19
# Built-in (one-sample, sigma known via the same z-formula):
power.t.test(delta = mu1 - mu0, sd = sigma, sig.level = alpha,
             power = power_target, type = "one.sample", alternative = "one.sided")
# n ~ 19.5 (t adds a tiny inflation vs the z formula above)
```

</details>

<details class="master-subpart">
<summary>(d) The four levers of power — $\alpha\uparrow, n\uparrow, |\mu_1-\mu_0|\uparrow, \sigma\downarrow$</summary>

Holding the other three constants fixed, each lever moves power monotonically:

| Lever | Mechanism (what moves on the $\bar X$ axis) | Power | Cost |
|---|---|---|---|
| **$\alpha\uparrow$** | $\bar X^* = \mu_0 - z_{1-\alpha}\sigma/\sqrt n$ moves *further* from $\mu_0$ into the alternative side ⇒ rejection region enlarges | $\uparrow$ | More Type-I errors |
| **$n\uparrow$** | $SE = \sigma/\sqrt n$ shrinks ⇒ both densities narrow; the $H_1$ density centred at $\mu_1$ concentrates *below* $\bar X^*$ | $\uparrow$ | Data-collection cost |
| **$|\mu_1-\mu_0|\uparrow$** | The $H_1$ density (centred at $\mu_1$) sits further from $\bar X^*$ ⇒ less mass on the wrong side of the cutoff | $\uparrow$ | Not a design choice — set by reality |
| **$\sigma\downarrow$** | $SE$ shrinks ⇒ same mechanism as $n\uparrow$ (densities narrow) | $\uparrow$ | Usually requires a better measurement instrument |

**Numeric demonstration on the `7_1b` setup** (vary one lever, hold the other three at $\sigma=4$, $n=47$, $\alpha=0.10$, $\mu_1=43$):

**Lever 1 — vary $\alpha$:**

| $\alpha$ | $z_{1-\alpha}$ | $\bar X^*$ | $\beta$ at $\mu_1=43$ | Power |
|---|---|---|---|---|
| 0.01 | 2.3263 | 43.643 | 0.1361 | 0.8639 |
| 0.05 | 1.6449 | 44.040 | 0.0418 | 0.9582 |
| 0.10 | 1.2816 | 44.252 | 0.0159 | 0.9841 |
| 0.20 | 0.8416 | 44.509 | 0.0040 | 0.9960 |

**Lever 2 — vary $n$:**

| $n$ | $SE = 4/\sqrt n$ | $\bar X^* = 45 - 1.2816\,SE$ | $z^* = (\bar X^* - 43)/SE$ | $\beta$ | Power |
|---|---|---|---|---|---|
| 10  | 1.2649 | 43.379 | 0.300 | 0.3821 | 0.6179 |
| 20  | 0.8944 | 43.854 | 0.955 | 0.1697 | 0.8303 |
| 47  | 0.5835 | 44.252 | 2.146 | 0.0159 | 0.9841 |
| 100 | 0.4000 | 44.487 | 3.717 | 0.0001 | 0.9999 |
| 200 | 0.2828 | 44.638 | 5.794 | $\approx 0$ | $\approx 1$ |

**Lever 3 — vary the effect size $|\mu_1-\mu_0|$** (fix $n=47$, $\alpha=0.10$, $\bar X^*=44.252$):

| $\mu_1$ | $z^* = (\bar X^* - \mu_1)/SE$ | $\beta$ | Power |
|---|---|---|---|
| 44.5 | $-0.425$ | 0.6645 | 0.3355 |
| 44.0 | $0.432$  | 0.3328 | 0.6672 |
| 43.5 | $1.289$  | 0.0987 | 0.9013 |
| 43.0 | $2.146$  | 0.0159 | 0.9841 |
| 42.0 | $3.860$  | $\approx 6\times 10^{-5}$ | $\approx 1$ |

**Lever 4 — vary $\sigma$** (fix $n=47$, $\alpha=0.10$, $\mu_1=43$):

| $\sigma$ | $SE = \sigma/\sqrt{47}$ | $\bar X^*$ | Power |
|---|---|---|---|
| 2 | 0.292 | 44.626 | $\approx 1$  |
| 4 | 0.584 | 44.252 | 0.9841 |
| 8 | 1.167 | 43.504 | 0.6722 |
| 16 | 2.334 | 42.008 | 0.3346 |

**Edge cases** to memorise.

* As $\mu_1 \uparrow \mu_0$ (no real effect): power $\to \alpha$ — the test rejects only at its nominal Type-I rate.
* As $n\to\infty$ (or $\sigma\to 0$): power $\to 1$ for any fixed $\mu_1\ne\mu_0$ — *every* sensible test is **consistent**.
* $\alpha + \beta \ne 1$ in general — both can be made small by raising $n$; both can be made large by shrinking $n$ AND tightening $\alpha$.

```r
# Lever 1 — alpha sweep
alphas <- c(0.01, 0.05, 0.10, 0.20)
sapply(alphas, function(a){
  xstar <- mu0 - qnorm(1-a)*se;  pnorm(xstar, mean = mu1, sd = se)
})                                                # power column
# Lever 2 — n sweep
ns <- c(10, 20, 47, 100, 200)
sapply(ns, function(nn){
  se_n <- sigma/sqrt(nn);  xstar <- mu0 - qnorm(1-alpha)*se_n
  pnorm(xstar, mean = mu1, sd = se_n)
})
# Lever 3 — effect-size sweep
mus <- c(44.5, 44, 43.5, 43, 42)
sapply(mus, function(m){ pnorm(xstar, mean = m, sd = se) })
# Lever 4 — sigma sweep
sigmas <- c(2, 4, 8, 16)
sapply(sigmas, function(sg){
  se_s <- sg/sqrt(n);  xstar <- mu0 - qnorm(1-alpha)*se_s
  pnorm(xstar, mean = mu1, sd = se_s)
})
```

</details>

<details class="master-subpart">
<summary>(e) Power for the other rows of the master table (short)</summary>

The 3-step procedure of the header generalises **verbatim** — only the null distribution and the $\widehat{\rm SE}$ change.

* **Row 2 — one-mean $t$-test ($\sigma$ unknown).** The pivot under $H_1$ is *non-central $t$*: $T \mid \mu=\mu_1 \sim t_{n-1}(\lambda)$ with non-centrality $\lambda = (\mu_1-\mu_0)/(\sigma/\sqrt n)$. Closed-form power requires `pt(..., ncp = lambda)`; the small-$n$ correction is sizeable (e.g. $n=20$ loses a few percent of power vs the $z$ approximation). R: `power.t.test(n = 47, delta = mu1-mu0, sd = sigma, sig.level = alpha, type = "one.sample", alternative = "one.sided")`.

* **Rows 4–6 — two-sample mean tests.** Same template with $\widehat\theta = \bar X_A - \bar X_B$ and $\widehat{\rm SE}$ from g14b. The sample-size formula becomes $n_A = n_B = \bigl((z_{1-\alpha} + z_{1-\beta})\sigma_{\rm pool}/|\mu_A-\mu_B|\bigr)^2$ (equal $n$). R: `power.t.test(..., type = "two.sample")`.

* **Row 7 — two-proportion $z$.** Power uses the $H_1$ SE $\sqrt{p_A(1-p_A)/n_A + p_B(1-p_B)/n_B}$ (with the *true* $p_A,p_B$, not the pooled $\hat p$). R: `power.prop.test`.

* **Row 8 — paired $t$.** Reduces to row 2 on differences $d_i$ with $\sigma_d$. Same formula.

* **Row 9 — $\chi^2$ goodness-of-fit / independence.** Under $H_1$, $X^2 \sim \chi^2_{\rm df}(\lambda)$ (**non-central $\chi^2$**) with non-centrality
$$\lambda \;=\; n \sum_{k=1}^{K} \frac{(p_k - p_k^{(0)})^2}{p_k^{(0)}} \;=\; n\cdot w^2,$$
where **Cohen's $w$** = $\sqrt{\sum_k (p_k - p_k^{(0)})^2/p_k^{(0)}}$ is the standard effect-size measure. Then $\text{Power} = \Pr\bigl(\chi^2_{\rm df}(\lambda) > \chi^2_{1-\alpha,\,\rm df}\bigr)$. R: `pchisq(qchisq(1-alpha, df), df, ncp = n*w^2, lower.tail = FALSE)`. Cross-reference `g14d` for the test itself.

The qualitative picture (more $n$ ⇒ more power; bigger effect ⇒ more power; bigger $\alpha$ ⇒ more power) carries over unchanged for every row.

</details>

<details class="master-subpart">
<summary>(f) Cross-references</summary>

* **Universal test recipe** lives in **`g14a`** (rows 1–3); not re-derived here. **`g14b`** covers two-sample (rows 4–7), **`g14c`** paired (row 8), **`g14d`** $\chi^2$ (row 9). Every one of those rows has a power function obtained by the same 3-step recipe of the header — see (e) for the SE substitutions.
* **Size-power trade-off mirrors the CI confidence-precision trade-off.** In G13, the **confidence level** $1-\alpha$ and the **width** $w = 2\cdot z_{1-\alpha/2}\,\sigma/\sqrt n$ trade off at fixed $n$: higher confidence ⇒ wider CI. In G14, $\alpha$ and **power** trade off at fixed $n,\sigma,\mu_1$: smaller $\alpha$ ⇒ lower power. The CI/test duality (`g14a` (b)) extends to a *power/precision* duality — both are governed by the same SE = $\sigma/\sqrt n$, and both improve only when $n$ rises (or $\sigma$ falls). See `g13a` (width table) and `g13b` (proportion CI).
* **Underlying estimators.** $\bar X$ (mean) and $\hat p$ (proportion) — derived once in **`g13f`** — are the $\widehat\theta$ that feeds every $Z = (\widehat\theta-\theta_0)/\widehat{\rm SE}_{H_0}$ in the master table. The non-centrality $\lambda$ of the power computation is precisely the *bias* of $\widehat\theta - \theta_0$ when the truth is $\theta_1\ne\theta_0$.
* **`7_1b` (the unique horizontal cell of this row)** computes both faces of the cutoff $\bar X^* = 44.252$: the *Type-I face* at $\mu=50$ (≈ 0, far inside $H_0$) and the *Type-II face* at $\mu_1=43$ (≈ 0.016 ⇒ power ≈ 0.984). Subparts (a)–(d) above reproduce that calculation, then sweep the four levers $\alpha,n,|\mu_1-\mu_0|,\sigma$ around it.

</details>

---

### Summary

g14e fills **row 10** of the universal hypothesis-test master table — the *complementary* row that asks how the verdict probability changes as $(\alpha,n,|\mu_1-\mu_0|,\sigma)$ are varied around any fixed row of the test recipe. With the `7_1b` setup ($\mu_0=45,\sigma=4,n=47,\alpha=0.10$), the cutoff is $\bar X^* = 44.252$; at $\mu_1=43$ this gives $\beta \approx 0.016$ and power $\approx 0.984$. The closed-form is $\text{Power} = \Phi\bigl(-z_{1-\alpha} + (\mu_0-\mu_1)/(\sigma/\sqrt n)\bigr)$, with sample-size inverse $n = ((z_{1-\alpha}+z_{1-\beta})\sigma/|\mu_1-\mu_0|)^2$. The four levers all raise power monotonically: $\alpha\uparrow, n\uparrow, |\mu_1-\mu_0|\uparrow, \sigma\downarrow$. The same recipe generalises to every other row of the master table — $t$-test power uses non-central $t_{n-1}(\lambda)$; $\chi^2$ power uses non-central $\chi^2_{\rm df}(\lambda)$ with $\lambda = n\cdot w^2$ for Cohen's $w$.
""",
    "images": ["statistics/images/master/master_g14e_ai.png"],
}

master_exercises["g14b_two_sample"] = {
    "title": "Master Exam — Two-sample independent tests (means & proportions)",
    "content": r"""## Setup — running datasets for every numeric example below

g14b walks **rows 4–7** of the universal hypothesis-test master table (see **`g14a`** — *do not re-derive* the recipe). We reuse a **single numeric thread** across cases 4/5/6 so the student sees that only the **SE** and the **df / critical-value family** change between rows. A second dataset is used for case 7 (two proportions).

**Running dataset A — Standard vs Seafood cholesterol** (from Ex 7.5a, reused for cases 4/5/6):

| Diet | $n$ | $\bar x$ | $s^2$ |
|---|---:|---:|---:|
| Standard (A) | $n_A=100$ | $\bar x_A = 210.1$ | $s_A^2 = 37.4$ |
| Seafood (B)  | $n_B=100$ | $\bar x_B = 196.8$ | $s_B^2 = 33.5$ |

Difference of sample means $\bar x_A - \bar x_B = 13.3$ throughout; only $\widehat{\rm SE}$ and df change between cases (a)–(c).

**Running dataset B — Cafeteria visit, pre vs post promotion** (from Ex 7.3a, used for case 7):

| Window | $n$ | visitors ($\geq 1$ stop) | $\hat p$ |
|---|---:|---:|---:|
| PRE  | $n_\text{PRE}=140$  | $108$ | $0.7714$ |
| POST | $n_\text{POST}=159$ | $127$ | $0.7987$ |

Both datasets stay fixed within their respective cases — no new sample is introduced.

<details class="master-subpart" open>
<summary>(a) <strong>Case 4</strong> — Two-sample $z$-test on means, $\sigma_A, \sigma_B$ <em>known</em></summary>

**Setting.** Two **independent** i.i.d. samples $X_{A,i}\sim(\mu_A, \sigma_A^2)$ and $X_{B,j}\sim(\mu_B, \sigma_B^2)$ with $\sigma_A, \sigma_B$ **known** (problem statement or textbook calibration). Test $H_0: \mu_A = \mu_B$ (equivalently $\delta = \mu_A - \mu_B = 0$).

**Pivot.** Independence makes variances add, so $\operatorname{Var}(\bar X_A - \bar X_B) = \sigma_A^2/n_A + \sigma_B^2/n_B$ exactly. With known $\sigma$'s, the standardised difference is exactly $\mathcal N(0,1)$ under $H_0$ for normal data (approximately so by the CLT once $n_A, n_B \gtrsim 30$).

**Test statistic and decision (row 4).**

$$\boxed{\;\;Z \;=\; \frac{(\bar X_A - \bar X_B) - \delta_0}{\sqrt{\sigma_A^2/n_A + \sigma_B^2/n_B}}, \qquad \text{reject } H_0 \iff |Z| > z_{1-\alpha/2} \;\;(\text{two-sided}).\;\;}$$

**Worked numbers on the cholesterol dataset.** Suppose lab calibration delivers the known SDs $\sigma_A = 6.0$ (Standard) and $\sigma_B = 5.8$ (Seafood). Test two-sided $H_1: \mu_A \ne \mu_B$ at $\alpha = 0.05$:

- Exact SE: $\sqrt{6.0^2/100 + 5.8^2/100} = \sqrt{0.36 + 0.3364} = \sqrt{0.6964} \approx 0.8345$.
- Realisation: $z_{\rm obs} = (210.1 - 196.8)/0.8345 = 13.3/0.8345 \approx 15.94$.
- Two-sided critical value: $z_{0.975} = 1.96$.
- Two-sided $p$-value: $p = 2(1-\Phi(15.94)) \approx 0$.

**Decision.** $|z_{\rm obs}| = 15.94 \gg 1.96$ ⇒ **reject** $H_0$ at any conventional $\alpha$ — overwhelming evidence the two diet means differ.

```r
# (a) Case 4 — two-sample z-test, sigma's known
xA <- 210.1; sigA <- 6.0; nA <- 100
xB <- 196.8; sigB <- 5.8; nB <- 100
SE0 <- sqrt(sigA^2/nA + sigB^2/nB);  SE0          # 0.8345 (exact)
z   <- (xA - xB) / SE0;              z             # 15.94
qnorm(0.975)                                       # 1.96
2*(1 - pnorm(abs(z)))                              # ~ 0
```

**Why case 4 is the simplest row.** No degrees of freedom, no $t$-inflation, no plug-in noise — $z$-quantiles are *the* reference distribution. The one-sample analogue is **`g14a` (a)** (case 1); in exam practice this row appears only when $\sigma_A, \sigma_B$ are literally stated, which is rare for two-sample problems.

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) <strong>Case 5</strong> — Pooled two-sample $t$-test, $\sigma_A^2 = \sigma_B^2$ unknown (Ex 7.5a, Ex 7.10a, exam_july_2025_1a)</summary>

**Setting.** Same i.i.d. independent samples, but $\sigma_A^2, \sigma_B^2$ are **unknown** and **Levene's test fails to reject** $H_0: \sigma_A^2 = \sigma_B^2$ (or the problem instructs us to assume equality, as Ex 7.5a does explicitly: "*assuming the variance of cholesterol is the same regardless of diet*"). Estimate the common $\sigma^2$ by pooling both sample variances by their degrees of freedom.

**Pivot.** Under $\sigma_A^2 = \sigma_B^2 = \sigma^2$ and normal samples,
$$T \;=\; \frac{(\bar X_A - \bar X_B) - \delta_0}{\sqrt{s_p^2(1/n_A + 1/n_B)}} \;\overset{H_0}{\sim}\; t_{n_A + n_B - 2} \;\text{ exactly}, \qquad s_p^2 \;=\; \frac{(n_A-1)s_A^2 + (n_B-1)s_B^2}{n_A + n_B - 2}.$$
The pooled $s_p^2$ is the **best (lowest-variance) unbiased estimator** of the common $\sigma^2$ — it spends *all* $n_A + n_B - 2$ df on a single estimate, more than either $s_A^2$ or $s_B^2$ alone.

**Test statistic and decision (row 5).**

$$\boxed{\;\;T \;=\; \frac{(\bar X_A - \bar X_B) - \delta_0}{\sqrt{s_p^2\!\left(\tfrac{1}{n_A}+\tfrac{1}{n_B}\right)}}, \qquad \text{reject } H_0 \iff |T| > t_{1-\alpha/2,\,n_A+n_B-2}\;\;(\text{two-sided}).\;\;}$$

**Worked numbers on the cholesterol dataset, $\Delta_0 = 10$ (Ex 7.5a).** The claim under test: the mean drop $\mu_A - \mu_B$ exceeds 10 units. One-sided upper:
$$H_0: \mu_A - \mu_B \le 10 \quad\text{vs}\quad H_1: \mu_A - \mu_B > 10.$$

- Pooled variance: $s_p^2 = (99\cdot 37.4 + 99\cdot 33.5)/198 = (3702.6 + 3316.5)/198 = 35.45$.
- Pooled SE: $\widehat{\rm SE} = \sqrt{35.45\cdot(1/100 + 1/100)} = \sqrt{0.7090} \approx 0.8420$.
- df: $n_A + n_B - 2 = 198$.
- Realisation: $t_{\rm obs} = (13.3 - 10)/0.8420 \approx 3.92$ on $t_{198}$.
- One-sided critical value: $t_{0.95, 198} \approx 1.6526$ (essentially $z_{0.95} = 1.6449$ at this df).
- One-sided $p$-value: $p = \Pr(t_{198} > 3.92) \approx 6.1\times 10^{-5}$.

**Decision.** $t_{\rm obs} = 3.92 \gg 1.6526$ ⇒ **reject** $H_0$ at $\alpha = 0.05$; the data strongly support that Seafood lowers mean cholesterol by **more than 10** units.

```r
# (b) Case 5 — pooled t-test, Delta_0 = 10 (Ex 7.5a)
xA <- 210.1; vA <- 37.4; nA <- 100
xB <- 196.8; vB <- 33.5; nB <- 100
sp2 <- ((nA-1)*vA + (nB-1)*vB)/(nA + nB - 2);  sp2   # 35.45
SE  <- sqrt(sp2*(1/nA + 1/nB));                SE    # 0.8420
t   <- (xA - xB - 10)/SE;                      t     # 3.92
1 - pt(t, df = nA+nB-2)                              # 6.1e-5
qt(0.95, df = nA+nB-2)                               # 1.6526
# Course helper (BAS package) — var.test=TRUE prints Levene + pooled + Welch
TEST.diffmean(Cholesterol, by = Diet, type = "independent",
              alternative = "greater", mu0 = 10, var.test = TRUE, data = Diet_study)
```

**Second worked thread — exam_july_2025_1a (Branch A vs Branch B Savings).** $H_0: \mu_A = \mu_B$ vs $H_1: \mu_A < \mu_B$ (one-sided lower); from the `TEST.diffmean(..., var.test=TRUE)` output: $\bar y_A - \bar y_B \approx -162.835$, $\widehat{\rm SE} \approx 58.45$, $t \approx -2.786$ on $t_{n_A + n_B - 2}$, one-sided $p \approx 0.0027$. Decision: $0.0027 < 0.01 < 0.05$ ⇒ **reject at both 5% and 1%**. The companion Levene test fails to reject $\sigma_A^2 = \sigma_B^2$, validating the pooled $t$ (and not Welch). Same row, different data.

**Third worked thread — Ex 7.10a (Considered vs Competitor, summary stats, $\alpha = 0.10$).** $H_0: \mu_x = \mu_y$ vs $H_1: \mu_x > \mu_y$; from the summaries $n_x = 750, \bar x = 1228.44, s_x^2 = 940{,}900.9$; $n_y = 800, \bar y = 1300, s_y = 960$:
- $s_p^2 = (749\cdot 940{,}900.9 + 799\cdot 921{,}600)/1548 \approx 930{,}938.7$.
- $\widehat{\rm SE} = \sqrt{930{,}938.7\cdot(1/750 + 1/800)} = \sqrt{2402.92} \approx 49.02$.
- $t_{\rm obs} = (1228.44 - 1300)/49.02 = -71.56/49.02 \approx -1.459$ on $t_{1548}$ (≈ $\mathcal N(0,1)$).
- One-sided upper $p = 1 - \Phi(-1.459) \approx 0.9277 \gg 0.10$.

**Decision.** $t_{\rm obs} < z_{0.90} = 1.2816$ ⇒ **do not reject**. Observed difference is in the *wrong* direction (considered company spends *less*), so naturally no evidence for "considered > competitor".

```r
# (b-bis) Ex 7.10a: pooled t on summary stats, alpha = 0.10
xbar <- 1228.44; s2x <- 940900.9; nx <- 750
ybar <- 1300;    s2y <- 960^2;    ny <- 800           # 921600

sp2 <- ((nx-1)*s2x + (ny-1)*s2y)/(nx + ny - 2); sp2   # 930938.7
SE  <- sqrt(sp2*(1/nx + 1/ny));                 SE    # 49.02
t   <- (xbar - ybar)/SE;                        t     # -1.459
1 - pt(t, df = nx + ny - 2)                           # 0.9277
qnorm(0.90)                                           # 1.2816
```

**Bias warning.** If equality of variances *fails*, $s_p^2$ is biased for either group variance — it over-states $\min(\sigma_A^2, \sigma_B^2)$ and under-states $\max(\sigma_A^2, \sigma_B^2)$. The bias propagates into the SE, and the test loses its nominal $\alpha$ — *under*-coverage of the null (inflated Type-I rate) when the larger variance sits in the *smaller* sample, *over*-coverage in the opposite case. This is why we check Levene **first** rather than assuming pooling is safe. The CI counterpart of this row is **`g13c` (b)** — identical SE formula, identical df, just with $\hat\delta \pm c \cdot \widehat{\rm SE}$ in place of $T = (\hat\delta - \delta_0)/\widehat{\rm SE}$.

![Master illustration](statistics/images/master/master_g14b_ai.png)

</details>

<details class="master-subpart">
<summary>(c) <strong>Case 6</strong> — Welch two-sample $t$-test, $\sigma_A^2 \ne \sigma_B^2$ unknown</summary>

**Setting.** Same i.i.d. independent samples, $\sigma_A^2, \sigma_B^2$ **unknown** and Levene's test **rejects** equality (or we adopt Welch as the safe default — see `g13c` (d) for the no-Levene decision matrix). Each sample variance estimates its *own* population variance; no pooling.

**Pivot.** Plug $s_A^2, s_B^2$ separately into the SE. The standardised statistic does **not** follow a Student-$t$ exactly — Welch & Satterthwaite showed it is approximately $t_{\nu^W}$ with the moment-matched fractional df
$$\boxed{\;\;\nu^W \;=\; \dfrac{\bigl(s_A^2/n_A + s_B^2/n_B\bigr)^2}{\dfrac{(s_A^2/n_A)^2}{n_A-1} + \dfrac{(s_B^2/n_B)^2}{n_B-1}}\;\;}$$
fractional and bounded by $\min(n_A - 1, n_B - 1) \le \nu^W \le n_A + n_B - 2$.

**Test statistic and decision (row 6).**

$$\boxed{\;\;T \;=\; \frac{(\bar X_A - \bar X_B) - \delta_0}{\sqrt{s_A^2/n_A + s_B^2/n_B}}, \qquad \text{reject } H_0 \iff |T| > t_{1-\alpha/2,\,\nu^W}.\;\;}$$

**Worked numbers on the cholesterol dataset (counter-factual: suppose Levene *had* rejected).** Same $\Delta_0 = 10$, same one-sided upper $H_1: \mu_A - \mu_B > 10$:

- Welch SE: $\widehat{\rm SE}_W = \sqrt{37.4/100 + 33.5/100} = \sqrt{0.7090} \approx 0.8420$.
- Welch df: $\nu^W = (0.7090)^2 / \bigl[(0.374)^2/99 + (0.335)^2/99\bigr] \approx 0.5027/0.002546 \approx 197.4$.
- Realisation: $t_{\rm obs} = (13.3 - 10)/0.8420 \approx 3.92$ on $t_{197.4}$.
- One-sided $p$-value: $\approx 6 \times 10^{-5}$.

**Why the cholesterol numbers look identical to (b).** With $n_A = n_B$ and similar variances ($s_A^2/s_B^2 = 37.4/33.5 \approx 1.12$, well inside $[\tfrac12, 2]$), the Welch SE *equals* the pooled SE to 4 decimals and the Welch df ($\approx 197$) is nearly the pooled df ($198$). The two rows diverge meaningfully **only when sample sizes and variances are unbalanced** (large $n$ on the *small*-variance side, small $n$ on the *large*-variance side, or both). See `g13c` (c) for the unbalanced regime.

```r
# (c) Case 6 — Welch t-test (default of t.test)
vA <- 37.4; vB <- 33.5
SE_W <- sqrt(vA/nA + vB/nB);   SE_W                       # 0.8420
nu_W <- (vA/nA + vB/nB)^2 /
        ((vA/nA)^2/(nA-1) + (vB/nB)^2/(nB-1));   nu_W     # ~ 197.4
t_W  <- (xA - xB - 10)/SE_W;   t_W                        # 3.92
1 - pt(t_W, df = nu_W)                                    # ~ 6e-5
qt(0.95, df = nu_W)                                       # ~ 1.653
# Raw data: t.test(... , var.equal = FALSE)   is the R default
```

**Why Welch is the universal safe default.** When the **larger** variance sits in the **larger** sample, pooling **over-states** the SE (test conservatively *under*-rejects, Type-I below nominal). When the larger variance sits in the **smaller** sample, pooling **under-states** the SE (test *over*-rejects, true Type-I above nominal — actively dangerous). Welch is robust to both regimes; the only cost is a smaller df, invisible once $n_A, n_B$ are both large. The CI counterpart of this row is **`g13c` (c)**.

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (d) <strong>Case 7</strong> — Two-proportion $z$-test, pooled-$\hat p$ SE under $H_0$ (Ex 7.3a, Ex 7.3b, Ex 7.7a)</summary>

**Setting.** Two **independent** Bernoulli samples: $X_A \sim \mathrm{Bin}(n_A, p_A)$, $X_B \sim \mathrm{Bin}(n_B, p_B)$, with sample proportions $\hat p_A = X_A/n_A$ and $\hat p_B = X_B/n_B$. Test $H_0: p_A = p_B$ (null *gap* $\delta_0 = 0$).

**Validity.** Large-sample CLT: $n_A \hat p_A,\; n_A(1-\hat p_A),\; n_B \hat p_B,\; n_B(1-\hat p_B)$ **all $\ge 5$**. If any fails, fall back to `prop.test` (Wilson) or exact methods.

**Pivot.** Under $H_0: p_A = p_B = p$ both samples share a common $p$, optimally estimated by the **pooled** sample proportion
$$\hat p_{\rm pool} \;=\; \frac{X_A + X_B}{n_A + n_B}.$$
This is *the* feature that distinguishes row 7 (test) from the CI in `g13d` (which uses separate $\hat p_A, \hat p_B$ — there is no common $p$ to estimate outside $H_0$). Under $H_0$, $\operatorname{Var}_{H_0}(\hat p_A - \hat p_B) = p(1-p)(1/n_A + 1/n_B)$, plug-in estimated by
$$\widehat{\rm SE}_{H_0} \;=\; \sqrt{\hat p_{\rm pool}(1 - \hat p_{\rm pool})\!\left(\frac{1}{n_A} + \frac{1}{n_B}\right)}.$$

**Test statistic and decision (row 7).**

$$\boxed{\;\;Z \;=\; \frac{\hat p_A - \hat p_B}{\sqrt{\hat p_{\rm pool}(1-\hat p_{\rm pool})\!\left(\tfrac{1}{n_A}+\tfrac{1}{n_B}\right)}}, \qquad \text{reject } H_0:p_A=p_B \iff |Z| > z_{1-\alpha/2}\;\;(\text{two-sided}).\;\;}$$

### **WARNING — pooled-vs-unpooled SE (mirrored from the box above).**
$$\widehat{\rm SE}_{\rm TEST} = \sqrt{\hat p_{\rm pool}(1-\hat p_{\rm pool})(1/n_A+1/n_B)} \quad\ne\quad \widehat{\rm SE}_{\rm CI} = \sqrt{\dfrac{\hat p_A(1-\hat p_A)}{n_A} + \dfrac{\hat p_B(1-\hat p_B)}{n_B}}.$$
*Test* uses the pooled SE (this row); *CI for $p_A - p_B$* uses the unpooled SE (`g13d`). Two different formulas, two different numeric answers. The two SEs agree only when $\hat p_A = \hat p_B$; otherwise the CI ⇄ test duality is **not** an exact identity for proportions.

**Worked numbers on the cafeteria pre/post dataset (Ex 7.3a).** A "visitor" = customer with $\geq 1$ stop in the month. Pre-promotion $n_\text{PRE} = 140, X_\text{PRE} = 108 \Rightarrow \hat p_\text{PRE} = 0.7714$; post-promotion $n_\text{POST} = 159, X_\text{POST} = 127 \Rightarrow \hat p_\text{POST} = 0.7987$. Most-serious-error reasoning (don't roll out an ineffective promotion) gives one-sided upper:
$$H_0: p_\text{POST} = p_\text{PRE} \quad\text{vs}\quad H_1: p_\text{POST} > p_\text{PRE}.$$

- CLT check: $n\hat p, n(1-\hat p) \in \{108, 32, 127, 32\}$ — all $\ge 5$.
- Pooled proportion: $\hat p_{\rm pool} = (108 + 127)/(140 + 159) = 235/299 \approx 0.7860$.
- Pooled SE: $\widehat{\rm SE}_0 = \sqrt{0.7860\cdot 0.2140\cdot(1/140 + 1/159)} \approx 0.0475$.
- Realisation: $z_{\rm obs} = (0.7987 - 0.7714)/0.0475 = 0.0273/0.0475 \approx 0.575$.
- One-sided critical value: $z_{0.95} = 1.6449$.
- One-sided $p$-value: $p = 1 - \Phi(0.575) \approx 0.2827$.

**Decision.** $z_{\rm obs} = 0.575 < 1.6449$ (equivalently $p = 0.28 \gg 0.05$) ⇒ **do not reject** $H_0$. No evidence the promotion raised the visit rate — **do not extend** it.

```r
# (d) Case 7 — two-prop z-test, pooled SE, one-sided upper (Ex 7.3a)
n1 <- 140; x1 <- 108                       # PRE
n2 <- 159; x2 <- 127                       # POST
ph1 <- x1/n1; ph2 <- x2/n2;  c(ph1, ph2)   # 0.7714, 0.7987
phat <- (x1 + x2)/(n1 + n2); phat          # 0.7860 pooled
SE0  <- sqrt(phat*(1-phat)*(1/n1 + 1/n2)); SE0   # 0.0475
z    <- (ph2 - ph1)/SE0;     z             # 0.575
1 - pnorm(z)                               # 0.2827 (one-sided p)
qnorm(0.95)                                # 1.6449
# Course helper:
TEST.diffprop(x = Stops_POST >= 1, y = Stops_PRE >= 1,
              pdiff = 0, alternative = "greater")
# Base R (no continuity correction):
prop.test(c(127, 108), c(159, 140), alternative = "greater", correct = FALSE)
```

**Second thread — Ex 7.3b (heavy users, cutoff > 4).** Same one-sided framing; $\hat p_\text{PRE} = 23/140 = 0.1643, \hat p_\text{POST} = 37/159 = 0.2327, \hat p_{\rm pool} = 60/299 = 0.2007$, $\widehat{\rm SE}_0 \approx 0.0464$, $z_{\rm obs} \approx 1.474$, $p \approx 0.0703$. **Borderline:** retain at 5% ($1.474 < 1.6449$), reject at 10% ($1.474 > z_{0.90} = 1.2816$). Same row, same formulas — only the binary indicator changes.

**Third thread — Ex 7.7a (AI tools Younger vs Senior, `Developers_ITA`).** $\hat p_\text{Young} \approx 0.57, \hat p_\text{Senior} \approx 0.40$; $H_0: p_\text{Young} = p_\text{Senior}$ vs $H_1: p_\text{Young} > p_\text{Senior}$; the textbook reports $z_{\rm obs} \approx 4.77 \Rightarrow p < 10^{-4}$ ⇒ **reject at any conventional $\alpha$**. Same row, same formulas, very large effect.

```r
# Quick wrapper, also for Ex 7.7a:
TEST.diffprop(x = Developers_ITA$ChatGPT[Developers_ITA$Younger == TRUE],
              y = Developers_ITA$ChatGPT[Developers_ITA$Younger == FALSE],
              success.x = "Yes", pdiff = 0, alternative = "greater", digits = 4)
```

**CI ⇄ test mismatch on the same data.** The 95% Wald **CI** for $p_\text{POST} - p_\text{PRE}$ in the cafeteria example uses the **unpooled** SE $\sqrt{0.7987\cdot 0.2013/159 + 0.7714\cdot 0.2286/140} \approx 0.0476$ (almost identical to the pooled $0.0475$ here because $\hat p_\text{POST} \approx \hat p_\text{PRE}$). When $\hat p_A$ and $\hat p_B$ are far apart, the two SEs drift apart and the CI ⇄ test duality is only approximate. Bookmark **`g13d`** for the CI side.

![Master illustration](statistics/images/master/master_g14b_ai.png)

</details>

<details class="master-subpart">
<summary>(e) Side-by-side comparison of cases 4 / 5 / 6 / 7</summary>

All four cases on the **shared cholesterol dataset** ($\bar x_A - \bar x_B = 13.3$, $n_A = n_B = 100$, $\Delta_0 = 10$ for cases 5/6; $\delta_0 = 0$ everywhere else), plus the cafeteria example for case 7. **Same template $T = (\widehat\theta - \delta_0)/\widehat{\rm SE}_{H_0}$ throughout — only the SE and df change.**

| Quantity | **Case 4** ($\sigma$'s known, $z$) | **Case 5** (pooled $t$) | **Case 6** (Welch $t$) | **Case 7** (pooled-$\hat p$ $z$) |
|---|---|---|---|---|
| Estimator $\widehat\theta$ | $\bar X_A - \bar X_B$ | $\bar X_A - \bar X_B$ | $\bar X_A - \bar X_B$ | $\hat p_A - \hat p_B$ |
| $\widehat{\rm SE}_{H_0}$ formula | $\sqrt{\sigma_A^2/n_A + \sigma_B^2/n_B}$ | $\sqrt{s_p^2(1/n_A + 1/n_B)}$ | $\sqrt{s_A^2/n_A + s_B^2/n_B}$ | $\sqrt{\hat p_{\rm pool}(1-\hat p_{\rm pool})(1/n_A + 1/n_B)}$ |
| Null distribution | $\mathcal N(0,1)$ | $t_{n_A + n_B - 2}$ | $t_{\nu^W}$ (Satterthwaite) | $\mathcal N(0,1)$ |
| df | — | integer $n_A + n_B - 2$ | fractional $\nu^W$ | — |
| Numeric SE (cholesterol) | $0.8345$ | $0.8420$ | $0.8420$ | — |
| Numeric $t_{\rm obs}$ (cholesterol) | $15.94$ (vs $\Delta_0 = 0$) | $3.92$ (vs $\Delta_0 = 10$) | $3.92$ (vs $\Delta_0 = 10$) | — |
| Required assumption | $\sigma_A, \sigma_B$ given | $\sigma_A^2 = \sigma_B^2$ (Levene fail-to-reject) | none on variance ratio | CLT: $n\hat p, n(1-\hat p) \ge 5$ |
| **When to use** | textbook / problem states $\sigma$'s | Levene $p > \alpha$ | Levene $p \le \alpha$, or safe default | two proportions, $H_0: p_A = p_B$ |
| CI counterpart | `g13c` (a) | `g13c` (b) | `g13c` (c) | `g13d` (a) — **uses unpooled SE** |

| sub-part | App / Exercise | Row | $H_1$ | Diff. | SE | Stat | $p$-value | @ chosen $\alpha$ |
|---|---|---|---|---|---|---|---|---|
| (a) | Cholesterol, $\sigma$'s known | 4 | $\mu_A \ne \mu_B$ | $13.3$ | $0.8345$ | $z = 15.94$ | $\approx 0$ | reject |
| (b) | Cholesterol pooled $t$ — Ex 7.5a | 5 | $\mu_A - \mu_B > 10$ | $13.3$ | $0.8420$ | $t_{198} = 3.92$ | $6\!\cdot\!10^{-5}$ | reject @ 0.05 |
| (b-bis) | Branch A vs B Savings — exam_july_2025_1a | 5 | $\mu_A < \mu_B$ | $-162.84$ | $58.45$ | $t = -2.786$ | $0.0027$ | reject @ 0.05 & 0.01 |
| (b-bis2) | Considered vs Competitor — Ex 7.10a | 5 | $\mu_x > \mu_y$ | $-71.56$ | $49.02$ | $t = -1.459$ | $0.9277$ | retain @ 0.10 |
| (c) | Cholesterol Welch $t$ (counter-factual) | 6 | $\mu_A - \mu_B > 10$ | $13.3$ | $0.8420$ | $t_{\approx 197} = 3.92$ | $\approx 6\!\cdot\!10^{-5}$ | reject @ 0.05 |
| (d) | Visit $\ge 1$ pre/post — Ex 7.3a | 7 | $p_\text{POST} > p_\text{PRE}$ | $0.0273$ | $0.0475$ | $z = 0.575$ | $0.2827$ | retain @ 0.05 |
| (d-bis) | Heavy users $> 4$ — Ex 7.3b | 7 | $p_\text{POST} > p_\text{PRE}$ | $0.0684$ | $0.0464$ | $z = 1.474$ | $0.0703$ | retain @ 0.05; reject @ 0.10 |
| (d-bis2) | AI tools Younger vs Senior — Ex 7.7a | 7 | $p_\text{Young} > p_\text{Senior}$ | $\approx 0.17$ | (built-in) | $z \approx 4.77$ | $< 10^{-4}$ | reject @ any $\alpha$ |

Within cases (a)/(b)/(c) the **numerator** $\bar x_A - \bar x_B = 13.3$ is identical on the cholesterol thread — only the **SE** ($0.8345$ vs $0.8420$ vs $0.8420$) and the **critical-value family** ($z$ vs $t_{198}$ vs $t_{197.4}$) change. Between (a*)–(c*) (means) and (d*) (proportions) the *structure* is the same; what changes is just which row of the universal table the question lives in.

</details>

<details class="master-subpart">
<summary>(f) Cross-references — where each two-sample case reappears</summary>

- **Topic anchor (universal recipe + 10-row master table)** — **`g14a`**. The universal 3-slot template $T = (\widehat\theta - \theta_0)/\widehat{\rm SE}_{H_0}$, the two equivalent decision rules (critical-value vs $p$-value), the one-sided/two-sided rejection regions and the Type-I/Type-II/power vocabulary all live there; **g14b** only **applies** rows 4–7.
- **One-sample analogues (rows 1–3)** — **`g14a`** (a)/(b)/(c). Case 4 ⇄ row 1 (just two $z$ pivots stacked by independence); case 5 ⇄ row 2 ($t$ pivot with a pooled-variance plug-in); case 6 ⇄ row 2 with separate variances; case 7 ⇄ row 3 (proportion $z$ with the SE pinned by the null).
- **Paired tests (row 8)** — **`g14c`**. When the two samples are matched (same subject Pre/Post, twins, before/after), the independence assumption of rows 4–6 fails — the SE must include the within-pair covariance term and the problem reduces to a *one-sample* $t$-test of (g14a row 2) on the differences $d_i = x_i - y_i$. Always answer *"is the data paired?"* before opening this entry.
- **Chi-squared tests (row 9)** — **`g14d`**. A genuinely different testing apparatus: right-tail-only on $\chi^2_{\rm df}$, statistic is $X^2 = \sum (O - E)^2/E$, no SE in the universal template sense. Use for goodness-of-fit and independence in a contingency table; the two-proportion $z$-test of case 7 is a special case of a $2\times 2$ independence test ($Z^2 = X^2$ on 1 df under $H_0$).
- **Power / sample-size (row 10)** — **`g14e`**.
- **CI counterparts (G13 mirror).** Case 4 ⇄ **`g13c`** (a); case 5 ⇄ **`g13c`** (b); case 6 ⇄ **`g13c`** (c); case 7 ⇄ **`g13d`** (a) — **with the unpooled SE** (see the boxed warning in (d)). The CI ⇄ test duality is exact for cases 4/5/6 (same SE on both sides); only approximate for case 7 because the SE formulas differ.
- **Levene's test** (variance-equality check, drives the case-5-vs-case-6 choice) — **`g13c` Part 9**. The exam sub-question `exam_sep_2025_5b` is a pure Levene-only problem feeding the CI of `exam_sep_2025_5a`/`5c`, so it is tagged to `g13c` rather than g14b.
- **Underlying unbiased estimators** ($\bar X, \hat p, S^2, S_p^2$ and their sampling SEs) — **`g13f`**. Every $\widehat\theta - \delta_0$ in rows 4–7 is an unbiased estimator minus the null value.
- **Variance-scaling, $n$ ⇄ SE laws.** Identical to G13 — quadrupling $n$ halves $\widehat{\rm SE}$, doubles $|T|$, collapses the $p$-value. See `g13a` for the width table.

</details>

---

### Summary

g14b covers the **four two-sample independent rows** of the universal hypothesis-test table. Case 4 (means, $\sigma$'s known) ⇒ exact $z$-test with $\sqrt{\sigma_A^2/n_A + \sigma_B^2/n_B}$; case 5 (means, equal-var pooled $t$) ⇒ $t_{n_A + n_B - 2}$ with $s_p^2$; case 6 (means, Welch $t$) ⇒ $t_{\nu^W}$ with separate $s_A^2/n_A + s_B^2/n_B$; case 7 (proportions) ⇒ $z$-test with the **pooled** SE evaluated under $H_0: p_A = p_B$ — **not** the unpooled CI plug-in (see `g13d`). The Levene branch (decides 5 vs 6) is cross-referenced to `g13c` Part 9, not re-derived. Continue to **`g14c`** for paired tests, **`g14d`** for $\chi^2$, **`g14e`** for power & sample-size.
""",
    "images": ["statistics/images/master/master_g14b_ai.png"],
}

# =====================================================================
# g14c_paired — Paired hypothesis test (one-sided)
# Consolidates: ex7.6a (n=7, before/after wi-fi), ex7.6b (n=14 effect)
# Dataset: Arcade revenue, n=7 stores, d_bar = 120 EUR, s_d = 110 EUR
# =====================================================================
master_exercises["g14c_paired"] = {
    "title": "Master Exam — Paired hypothesis test (row 8: reduces to one-mean $t$ on differences)",
    "content": r"""## Setup — running dataset for every numeric example below

An arcade chain installed **free wi-fi** in one of its stores and now wants to decide whether the service should be rolled out chain-wide. For each of $n = 7$ stores the manager recorded the **weekly daily revenue** (in €100s) in a typical week **before** the installation ($X^\text{B}_i$) and again in a typical week **three months after** ($X^\text{A}_i$). Sample summaries:

| | Mean | Variance |
|---|---|---|
| PRE  | $\bar X_\text{B} = 13$ | $s_\text{B}^2 = 12$ |
| POST | $\bar X_\text{A} = 16$ | $s_\text{A}^2 = 21$ |

with cross-**covariance** $s_\text{B,A} = 11$. Because each store contributes *both* values the design is **paired**, not independent. The within-store difference

$$D_i \;=\; X^\text{A}_i \;-\; X^\text{B}_i, \qquad i = 1, \ldots, 7$$

has sample mean and SD (derived in subpart (b) below)

$$\bar d \;=\; \bar X_\text{A} - \bar X_\text{B} \;=\; 16 - 13 \;=\; 3, \qquad s_d^2 \;=\; s_\text{A}^2 + s_\text{B}^2 - 2\,s_\text{B,A} \;=\; 21 + 12 - 22 \;=\; 11, \qquad s_d \;\approx\; 3.317.$$

We reuse this **one dataset** throughout.

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) <strong>Case 8</strong> — Paired $t$-test as one-mean $t$ on the differences (Ex 7.6a, exam_g1_2025_2a, exam_sep_2025_2a)</summary>

**Setting.** Pairs $(X_i, Y_i)$ for $i = 1, \ldots, n$ are i.i.d. across $i$ (random sample of units), with *no* independence assumption *within* a pair. Define $D_i = X_i - Y_i$. Then $D_1, \ldots, D_n$ are i.i.d. with $\mathbb E[D_i] = \mu_X - \mu_Y =: \mu_d$ and

$$\Var(D_i) \;=\; \Var(X_i) + \Var(Y_i) - 2\,\Cov(X_i, Y_i) \;=\; \sigma_X^2 + \sigma_Y^2 - 2\,\rho\,\sigma_X\sigma_Y.$$

The within-pair covariance $\rho\sigma_X\sigma_Y$ does **not vanish** because $X_i, Y_i$ are measured on the *same* unit (same store, same patient, same developer). The sample mean has variance

$$\Var(\bar D) \;=\; \frac{\Var(D_i)}{n} \;=\; \frac{\sigma_X^2 + \sigma_Y^2 - 2\rho\sigma_X\sigma_Y}{n}.$$

**Pivot.** Replacing $\Var(D_i)$ by its sample analogue $s_d^2 = \tfrac{1}{n-1}\sum_i (d_i - \bar d)^2$, exactly the one-mean argument of row 2 applies:

$$T \;=\; \frac{\bar D - \mu_d}{s_d/\sqrt n} \;\sim\; t_{n-1} \qquad (\text{exact under }D_i \sim \mathcal N,\ \text{approx by CLT for }n \gtrsim 30).$$

**Test statistic and decision (row 8 = row 2 on $D$).** Under $H_0: \mu_d = 0$,

$$\boxed{\;\;T \;=\; \frac{\bar D - 0}{s_d/\sqrt n} \;\overset{H_0}{\sim}\; t_{n-1}, \qquad \text{reject } H_0 \iff |T| > t_{1-\alpha/2,\,n-1}\;\;(\text{two-sided}).\;\;}$$

The $p$-value follows the universal mini-table at the top of **`g14a`**: $p = 2\Pr(t_{n-1} \ge |t_{\rm obs}|)$ (two-sided), or $\Pr(t_{n-1} \ge t_{\rm obs})$ / $\Pr(t_{n-1} \le t_{\rm obs})$ for the one-sided variants.

**Worked numbers on the running dataset** with $\bar d = 3$, $s_d^2 = 11$, $n = 7$. Wi-fi extension is justified **only** if revenues *increase*, so the directional claim sits in $H_1$ (one-sided upper):

$$H_0:\mu_d \;\le\; 0 \qquad\text{vs}\qquad H_1:\mu_d \;>\; 0.$$

- Paired SE: $\widehat{\rm SE}(\bar D) = s_d/\sqrt n = \sqrt{11/7} \approx 1.2536$ (in €100s).
- Realisation: $t_{\rm obs} = 3 / 1.2536 \approx 2.3931$ on $t_6$.
- One-sided upper critical values: $t_{0.95,\,6} = 1.9432$ (5%), $t_{0.99,\,6} = 3.1427$ (1%).
- One-sided $p$-value: $p = \Pr(t_6 > 2.3931) \approx 0.0269$.

**Decision.** $t_{\rm obs} = 2.39 > 1.943 \Rightarrow$ **reject** $H_0$ at $\alpha = 0.05$; $2.39 < 3.143 \Rightarrow$ **retain** at $\alpha = 0.01$. Same verdict from the $p$-value: $0.01 < 0.027 < 0.05$. **Conclusion.** At the 5% level the data support rolling out free wi-fi chain-wide; at the more conservative 1% level the evidence is *not quite* enough.

**One-sided lower / two-sided variants on the same statistic.** Same $t_{\rm obs} = 2.3931$, but switch $H_1$:

| $H_1$ | Rejection region | $p$-value | Decision at 5% |
|---|---|---|---|
| $\mu_d > 0$ (one-sided upper — original) | $T > 1.9432$ | $\Pr(t_6 > 2.3931) \approx 0.0269$ | **reject** |
| $\mu_d < 0$ (one-sided lower) | $T < -1.9432$ | $\Pr(t_6 < 2.3931) \approx 0.9731$ | retain (data go the wrong way) |
| $\mu_d \ne 0$ (two-sided) | $|T| > t_{0.975,\,6} = 2.447$ | $2\Pr(t_6 \ge 2.3931) \approx 0.0538$ | **retain** (just above 0.05) |

The two-sided test fails at 5% even though the upper one-sided test rejects — doubling the $p$-value flips the decision. **The direction of $H_1$ must be pre-specified** from subject-matter knowledge (the chain wants to know if revenues *increased*, not just whether they *changed*); picking it after seeing $\bar d > 0$ would be p-hacking. Same warning as `g14a` (d).

```r
# --- (a) Case 8 — paired t-test on the differences (Ex 7.6a, running data)
dbar <- 3;  sd_d <- sqrt(11);  n <- 7      # mu_post - mu_pre, n = 7 stores
SE   <- sd_d / sqrt(n);          SE        # 1.2536
t    <- (dbar - 0)/SE;           t         # 2.3931
qt(0.95, df = n-1)                          # 1.9432   one-sided 5% crit
qt(0.99, df = n-1)                          # 3.1427   one-sided 1% crit
1 - pt(t, df = n-1)                         # 0.0269   one-sided p (H1: mu_d > 0)
pt(t, df = n-1)                             # 0.9731   one-sided p (H1: mu_d < 0)
2 * (1 - pt(abs(t), df = n-1))              # 0.0538   two-sided p
# Course helper (BAS): paired version of TEST.mean
# TEST.mean(After - Before, mu0 = 0, alternative = "greater")
# Base R on raw paired columns:
# t.test(After, Before, paired = TRUE, alternative = "greater")
```

**Assumptions.** (i) The pairs are i.i.d. across units — design-level requirement, not testable from one sample. (ii) For small $n$ (here $n=7$) the **differences $D_i$ are approximately normal** — *not* $X_i$ or $Y_i$ separately, only the differences. For larger $n$ (e.g. `exam_g1_2025_2a` with $n=161$, or `exam_sep_2025_2a` with $n$ large) the CLT makes (ii) automatic and $t_{n-1} \approx z$ — see the algorithm-performance exam for the summary-statistic form on a $z$ reference.

![Master illustration — paired test is one-mean t on the differences](statistics/images/master/master_g14c_ai.png)

</details>

<details class="master-subpart">
<summary>(b) Paired test from raw $(x_i, y_i)$ vs from summary $(\bar d, s_d, n)$ — and the doubling-$n$ scaling (Ex 7.6b)</summary>

Data arrive in **two formats**. The reduction $d_i = x_i - y_i$ and the formula $T = \bar d / (s_d/\sqrt n)$ are identical — only the input differs.

**(b1) Raw paired columns.** Compute the per-pair differences first, then call any one-sample $t$ machinery on them.

```r
# Generic template: x = After, y = Before  (or x = NA_Sales, y = EU_Sales, etc.)
d     <- x - y
n     <- length(d)
dbar  <- mean(d)
sd_d  <- sd(d)
SE    <- sd_d / sqrt(n)
t     <- dbar / SE
1 - pt(t, df = n-1)                              # one-sided upper p
2 * (1 - pt(abs(t), df = n-1))                   # two-sided p

# One-shot equivalents:
t.test(x, y, paired = TRUE, alternative = "two.sided")   # two-sided
t.test(x, y, paired = TRUE, alternative = "greater")     # H1: mu_x > mu_y
t.test(x, y, paired = TRUE, alternative = "less")        # H1: mu_x < mu_y
# Course helper (BAS):
# TEST.mean(x - y, mu0 = 0, alternative = "greater")
```

**(b2) Summary statistics $(\bar x_\text{X}, \bar x_\text{Y}, s_X, s_Y, \hat\rho \text{ or } s_{X,Y}, n)$ — Ex 7.6a as given.** Reconstruct $\bar d$ and $s_d$ first; then proceed identically. From $\Var(X-Y) = \Var(X) + \Var(Y) - 2\Cov(X,Y)$ the sample analogue is

$$\boxed{\;\;s_d^2 \;=\; s_X^2 + s_Y^2 - 2\,\hat\rho\,s_X s_Y \;=\; s_X^2 + s_Y^2 - 2\,s_{X,Y}\;\;}$$

```r
# (b2) Summary route — running arcade dataset (Ex 7.6a)
n     <- 7
xbarA <- 16;   xbarB <- 13                       # POST, PRE means
s2A   <- 21;   s2B   <- 12;   s_BA  <- 11        # POST, PRE variances + covariance
dbar  <- xbarA - xbarB                            # 3
sd2_d <- s2A + s2B - 2*s_BA;     sd2_d           # 11   (NOT 33!)
sd_d  <- sqrt(sd2_d)                              # 3.317
SE    <- sd_d / sqrt(n);         SE              # 1.2536
t     <- dbar / SE;              t                # 2.3931
1 - pt(t, df = n-1)                               # 0.0269 one-sided p
# Sleep-diet exam (exam_g1_2025_2a, n=161, r=0.71):
sd_D  <- sqrt(45.61^2 + 48^2 - 2*0.71*45.61*48)  # ~ 35.71
t     <- (414 - 402.89)/(sd_D/sqrt(161));   t    # ~ 3.95
1 - pt(t, df = 160)                               # ~ 5.85e-5
```

**Verify equivalence.** On raw data, `sd(d)` and the summary reconstruction $\sqrt{s_X^2 + s_Y^2 - 2\,\mathrm{cov}(x,y)}$ agree exactly:

```r
all.equal(var(d), var(x) + var(y) - 2*cov(x, y))     # TRUE
```

**Doubling-$n$ scaling (Ex 7.6b).** Suppose the same $\bar d = 3, s_d^2 = 11$ are obtained from a sample of $n = 14$ stores (two weeks of paired observations on the same 7 stores). Three things change *systematically*, **without** recomputing from raw data — the scaling laws are exact (and identical to `g13a` for CIs):

| Quantity | $n = 7$ | $n = 14$ | Multiplier |
|---|---|---|---|
| $\widehat{\rm SE} = s_d/\sqrt n$ | $1.2536$ | $0.8864$ | $1/\sqrt 2$ |
| $t_{\rm obs} = \bar d/\widehat{\rm SE}$ | $2.3931$ | $3.3844$ | $\sqrt 2$ |
| df $= n - 1$ | $6$ | $13$ | — |
| $t_{0.95,\,\text{df}}$ | $1.9432$ | $1.7709$ | $\downarrow$ |
| $t_{0.99,\,\text{df}}$ | $3.1427$ | $2.6503$ | $\downarrow$ |
| One-sided $p$-value | $0.0269$ | $\approx 0.0024$ | $\div\;11$ |
| Decision at $5\%$ | reject | reject | **reinforced** |
| Decision at $1\%$ | retain | **reject** | **flips to reject** |

Two effects push the tail probability down — **larger statistic** *and* **lighter tail** on more df. The 1%-level decision flips from retain to reject. The same scaling is what `g14e` formalises as "power $\uparrow$ as $n \uparrow$".

```r
# Doubling n on the same per-store summaries
n14   <- 14
SE14  <- sqrt(11/n14);            SE14            # 0.8864 = SE7 / sqrt(2)
t14   <- 3 / SE14;                t14             # 3.3844 = t7  * sqrt(2)
qt(0.95, df = n14-1)                              # 1.7709
qt(0.99, df = n14-1)                              # 2.6503
1 - pt(t14, df = n14-1)                           # ~ 0.0024 (reject at 1% AND 5%)
```

</details>

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (c) Paired vs independent — power gain (the SE-ratio table)</summary>

The whole point of pairing is the $-2\rho\sigma_X\sigma_Y$ term in $\Var(D_i)$. Compare the two SE flavours at fixed $\sigma_X = \sigma_Y = \sigma$:

| SE flavour | Variance of $\bar D$ | SE of $\bar D$ | Inflation vs paired |
|---|---|---|---|
| **Paired** (correct) | $\dfrac{2\sigma^2(1-\rho)}{n}$ | $\sigma\sqrt{2(1-\rho)/n}$ | $\times 1$ baseline |
| **Independent** (wrong here) | $\dfrac{2\sigma^2}{n}$ | $\sigma\sqrt{2/n}$ | $\times \dfrac{1}{\sqrt{1-\rho}}$ |

The ratio $\widehat{\rm SE}_{\rm indep} / \widehat{\rm SE}_{\rm paired} = 1/\sqrt{1-\rho}$ blows up as $\rho \uparrow 1$:

| $\rho$ | $1 - \rho$ | Inflation $= 1/\sqrt{1-\rho}$ | Effective $n$ lost |
|---:|---:|---:|---|
| $0.0$ | $1.00$ | $\times 1.00$ | none (paired = indep) |
| $0.5$ | $0.50$ | $\times 1.41$ | half of $n$ |
| $0.7$ | $0.30$ | $\times 1.83$ | $70\%$ of $n$ |
| $0.9$ | $0.10$ | $\times 3.16$ | $90\%$ of $n$ |

**On the running arcade dataset** ($s_X^2 = 12, s_Y^2 = 21, s_{X,Y} = 11$, implied $\hat\rho = 11/\sqrt{12\cdot 21} \approx 0.693$): the wrong independent variance for the difference is $s_X^2 + s_Y^2 = 33$ versus the correct paired variance $s_d^2 = 11$ — a $\sqrt{33/11} = \sqrt 3 \approx 1.73\times$ SE inflation. The wrong $t$-statistic would be $t_{\rm indep} = 3/\sqrt{33/7} \approx 1.382$ on $t_{12}$ (pooled two-sample df with $n_A = n_B = 7$), giving $p \approx 0.10$ — **fails to reject at 5%, opposite verdict from the correct paired test** ($p \approx 0.027$). The mistake is *procedural*, not numerical, and is the single most common g14b ↔ g14c error in the course.

```r
# Wrong-independent vs correct-paired analysis on the running dataset
# Correct (paired): t = 2.3931 on df = 6, p ~ 0.027 -> reject at 5%
# Wrong (independent, pooled): ignores Cov(X,Y) = 11
SE_indep <- sqrt(12/7 + 21/7);           SE_indep   # 2.171
t_indep  <- 3 / SE_indep;                 t_indep    # 1.382  (vs 2.393 paired)
2 * (1 - pt(abs(t_indep), df = 7+7-2))               # ~ 0.19 (two-sided)
1 - pt(t_indep, df = 7+7-2)                          # ~ 0.10  one-sided -> RETAIN at 5%
# Inflation factor on the SE
SE_indep / sqrt(11/7)                                 # ~ 1.73 = sqrt(3)
1 / sqrt(1 - 11/sqrt(12*21))                          # same number via 1/sqrt(1-rho)
```

The independent-samples *test* counterpart lives in **`g14b`** (cases 5 pooled / 6 Welch). The independent-samples *CI* counterpart is **`g13c`**. Both are the **wrong tool** when units are matched — see the boxed warning at the top of this entry.

</details>

<details class="master-subpart">
<summary>(d) CI ⇄ test duality on paired data (cross-reference to `g13e`)</summary>

For the two-sided case the paired test of (a) and the **paired CI** of `g13e` are *two views of the same operation*:

$$\boxed{\;\;\text{Reject } H_0: \mu_d = 0 \text{ at level } \alpha \iff (1-\alpha) \text{ paired CI for } \mu_d \text{ does NOT contain } 0.\;\;}$$

The two SE formulas are *identical* — both are $s_d/\sqrt n$, both use the $t_{n-1}$ critical — so the duality is **exact** here (no analogue of the one-proportion CI/test SE drift of `g14a` (c)). Whichever lens the question asks for, the numerical verdict is the same.

**On the running arcade dataset** (two-sided variant, for the duality check):

- Two-sided $p$-value at $t_{\rm obs} = 2.3931$: $p_2 \approx 0.0538$ ⇒ **retain $H_0$** at $\alpha = 0.05$.
- 95% paired CI for $\mu_d$: $\bar d \pm t_{0.975,\,6} \cdot s_d/\sqrt n = 3 \pm 2.447 \cdot 1.2536 = [-0.067,\,6.067]$ — **contains $0$** ⇒ same retain verdict.

Switch to $\alpha = 0.10$ (so $t_{0.95,\,6} = 1.9432$): the 90% CI is $3 \pm 1.9432 \cdot 1.2536 = [0.564, 5.436]$ — **excludes $0$** ⇒ the two-sided test rejects at 10%, matching $p_2 = 0.0538 < 0.10$. Both routes always agree.

For a one-sided test the duality compares to a **one-sided CI**: the upper one-sided 95% CI is $(-\infty, \bar d + t_{0.95,\,6}\cdot s_d/\sqrt n] = (-\infty, 5.436]$ — not the question one typically reads off the printed two-sided CI; for one-sided tests stick with the $p$-value comparison.

```r
# CI <-> test duality on the running dataset (two-sided)
dbar  <- 3; sd_d <- sqrt(11); n <- 7
SE    <- sd_d / sqrt(n)
# 95% two-sided paired CI
tc95  <- qt(0.975, df = n-1);   tc95              # 2.447
c(dbar - tc95*SE, dbar + tc95*SE)                 # [-0.067, 6.067]  contains 0 -> retain
# 90% two-sided paired CI
tc90  <- qt(0.95,  df = n-1);   tc90              # 1.9432
c(dbar - tc90*SE, dbar + tc90*SE)                 # [ 0.564, 5.436]  excludes 0 -> reject @ 10%
# Same via t.test on raw paired columns:
# t.test(After, Before, paired = TRUE, conf.level = 0.95)$conf.int
```

The full paired-CI machinery — derivation, ME table at 90/95/99%, reading $\bar d$ backwards from a printed CI — lives in **`g13e`** and is **not** re-derived here.

</details>

<details class="master-subpart">
<summary>(e) Cross-references — where each piece of the paired story lives</summary>

- **Universal hypothesis-test recipe and master case table** — see **`g14a`** (rows 1–10; this entry owns row 8). g14c contributes *nothing new* to the template — it is the row-2 $t$-test applied to the derived variable $D$.
- **One-mean test row 2 (the row we reduce to)** — see **`g14a`** (b). The whole machinery of $t_{n-1}$ critical values, the $z$-vs-$t$ rationale, the one-sided/two-sided $p$-value table, the Type-I/II vocabulary is there and is **not** re-derived here.
- **Independent two-sample tests rows 4–7 — `g14b` (DO NOT USE on paired data).** Pooled $t$ (row 5), Welch $t$ (row 6), two-proportion $z$ (row 7). Using `g14b` on paired data **inflates the SE by $1/\sqrt{1-\rho}$** (subpart (c)) — the very phenomenon pairing was designed to eliminate.
- **Paired CI counterpart on the same parameter** ($\mu_d$) — see **`g13e`**. Same reduction $d_i = x_i - y_i$, then row 2 of the CI table; the CI ⇄ test duality holds *exactly* (subpart (d)).
- **One-mean CI rows 1–2 (the row $\mu_d$-CI reduces to)** — see **`g13a`**.
- **Estimation foundation** (unbiasedness of $\bar D$ and $S_d^2$, sampling SE) — see **`g13f`**.
- **Power / sample-size analysis** — see **`g14e`** for how $\beta$, power and $n$ trade off; the doubling-$n$ scaling in subpart (b) is a one-step preview of that machinery applied to a paired design.

**Bookmark.** Row 8 is row 2 *in disguise*. The only two things to remember are **(i) reduce $\to d_i$ and use $t_{n-1}$** and **(ii) never use the independent test (g14b cases 5/6) on paired data** — the within-pair covariance is exactly what pairing was designed to exploit.

</details>

---

### Side-by-side summary (running arcade dataset, $\bar d = 3$, $s_d^2 = 11$)

| Quantity | $n = 7$ | $n = 14$ | Where |
|---|---:|---:|---|
| Paired SE $= s_d/\sqrt n$ | $1.2536$ | $0.8864$ | (a), (b) |
| $t_{\rm obs} = \bar d/\widehat{\rm SE}$ | $2.3931$ | $3.3844$ | (a), (b) |
| df $= n - 1$ | $6$ | $13$ | (a) |
| $t_{0.95,\,\text{df}}$ / $t_{0.99,\,\text{df}}$ | $1.9432$ / $3.1427$ | $1.7709$ / $2.6503$ | (a), (b) |
| One-sided $p$-value ($H_1:\mu_d > 0$) | $0.0269$ | $\approx 0.0024$ | (a), (b) |
| Two-sided $p$-value | $0.0538$ | $\approx 0.0048$ | (a), (d) |
| Decision @ 5% (one-sided) | **reject** | **reject** | (a), (b) |
| Decision @ 1% (one-sided) | retain | **reject** | (b) |
| 95% paired CI for $\mu_d$ | $[-0.067,\,6.067]$ (contains 0) | $[1.085,\,4.915]$ (excludes 0) | (d), `g13e` |
| Wrong-independent SE | $2.171$ | $1.535$ | (c) |
| SE inflation $\sqrt{(s_X^2+s_Y^2)/s_d^2}$ | $\times 1.73$ | $\times 1.73$ | (c) |

**One-line take-away.** Row 8 of the universal test table = row 2 applied to $d_i = x_i - y_i$: $T = \bar d / (s_d/\sqrt n) \sim t_{n-1}$ under $H_0:\mu_d = 0$, validity gated by i.i.d. pairs and (small-$n$) normality of $D$. **Never use the independent test (g14b cases 5/6) on paired data** — the within-pair covariance is exactly what pairing was built to exploit. Continue to **`g13e`** for the matching paired CI, **`g14b`** for the independent-samples counterpart (when units are NOT matched), **`g14e`** for the power / sample-size analysis.
""",
    "images": ["statistics/images/master/master_g14c_ai.png"],
}

master_exercises["g15b_prediction"] = {
    "title": "Master Exam — Prediction at $x_0$: CI for the mean response vs PI for an individual (rows 6–7 of the G15 master regression table)",
    "content": r"""## Setup — same running dataset as `g15a` (NewHired: Weeks ~ Age, $n=47$)

This entry is the natural continuation of `g15a`: the job-agency dataset, the fitted line, and the residual standard error are *all reused* from there. A reminder of the numbers (derived once in `g15a`, parts (a)–(b)):

- $n = 47$, df $= n-2 = 45$,
- $\bar x = 38.617$, $s^2_x = 88.246 \Rightarrow s_x = 9.394$,
- $\hat\beta_0 = -19.5262$, $\hat\beta_1 = 1.6898 \Rightarrow \widehat{\text{Weeks}} = -19.5262 + 1.6898\cdot\text{Age}$,
- $\hat\sigma^2_\varepsilon = 388.15 \Rightarrow s_\varepsilon = 19.70$ weeks,
- $t_{0.975,\,45} = 2.014$ (we will use $\alpha = 0.05$ throughout).

```r
n     <- 47;     xbar <- 38.617;   s2_x  <- 88.246;   s_x  <- sqrt(s2_x)   # 9.394
b0    <- -19.5262;  b1 <- 1.6898;  s_e   <- 19.70                          # from g15a
tcrit <- qt(0.975, df = n-2)                                               # 2.014
```

The questions answered below:

At a new value $x_0$ of Age, what is our best **point prediction** $\hat y_0$ of `Weeks`? What is the $95\%$ **CI for the mean** Weeks across **all** workers of that age? What is the $95\%$ **PI for one specific** new worker of that age? How do the two intervals compare as $x_0$ moves away from $\bar x$?

The three values of $x_0$ used in the worked examples below come from the past-exam horizontal cells (`exam_g1_2026_5a/5b`, `exam_g2_2026_4_5`) and from the side-by-side comparison built around $\bar x$, $\bar x + s_x$, $\bar x + 3 s_x$ in subpart (c).

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span><span class="tag tag-2plus">≥2 ex</span> (a) <strong>Row 6 — CI for the mean response</strong> at $x_0$ (Ex 8.10a part e, `exam_g1_2026_5a`)</summary>

**Quantity targeted.** $E[Y\mid X=x_0] \;=\; \beta_0 + \beta_1 x_0$ — the *population mean* of Weeks across all workers whose Age equals $x_0$. This is a fixed (non-random) parameter; the *only* source of uncertainty is sampling noise in $(\hat\beta_0,\hat\beta_1)$, i.e. *how well we have pinned down the line position at $x_0$*.

**Derivation.** Write $\hat Y_0 = \hat\beta_0 + \hat\beta_1 x_0$. Under the LINE assumptions ($\varepsilon_i \overset{\text{iid}}{\sim} \mathcal N(0,\sigma^2)$),
$$\Var(\hat Y_0) \;=\; \sigma^2\left[\frac{1}{n} + \frac{(x_0-\bar x)^2}{(n-1)s_x^2}\right] \;\equiv\; \sigma^2\,\ell(x_0),\qquad \hat Y_0 \sim \mathcal N\!\bigl(\beta_0+\beta_1 x_0,\ \sigma^2\ell(x_0)\bigr).$$
Replacing $\sigma$ by $s_\varepsilon$ and pivoting:
$$\frac{\hat Y_0 - (\beta_0+\beta_1 x_0)}{s_\varepsilon\sqrt{\ell(x_0)}} \;\sim\; t_{n-2}.$$
Inverting yields the boxed CI formula.

$$\boxed{\;\;CI_{1-\alpha}\bigl(E[Y\mid x_0]\bigr) \;=\; \hat y_0 \;\pm\; t_{1-\alpha/2,\,n-2}\;s_\varepsilon\;\sqrt{\frac{1}{n} + \frac{(x_0-\bar x)^2}{(n-1)s_x^2}}.\;\;}$$

**Worked example on NewHired at $x_0 = 50$ years.**

- Point prediction: $\hat y_0 = -19.5262 + 1.6898\cdot 50 = 64.964$ weeks.
- Leverage term: $\ell(50) = 1/47 + (50-38.617)^2/(46\cdot 88.246) = 0.02128 + 129.572/4059.32 = 0.02128 + 0.03192 = 0.05320$.
- ME: $2.014 \cdot 19.70 \cdot \sqrt{0.05320} = 39.676 \cdot 0.2307 = 9.152$.
- **95% CI for mean Weeks at Age $=50$:** $64.964 \pm 9.152 = \boxed{[55.81,\,74.12]}$ weeks.

*Interpretation.* With 95% confidence, the **average** time-to-job for the population of workers aged 50 lies between $\approx 55.8$ and $\approx 74.1$ weeks. The interval does **not** describe any single worker — it describes the *mean* outcome across all 50-year-olds.

**R one-liner.**

```r
mod <- lm(Weeks ~ Age, data = NewHired)
predict(mod, newdata = data.frame(Age = 50),
        interval = "confidence", level = 0.95)
##        fit       lwr       upr
## 1   64.96     55.81     74.12
```

**Comment.** As $n\to\infty$ with $x_0$ fixed, $\ell(x_0)\to 0$ and the CI width $\to 0$ — we eventually pin the line *exactly* at $x_0$.

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span><span class="tag tag-2plus">≥2 ex</span> (b) <strong>Row 7 — PI for an individual new observation</strong> at $x_0$ (Ex 8.1c, Ex 8.10a part d, `exam_g1_2026_5b`, `exam_g2_2026_4_5`)</summary>

**Quantity targeted.** $Y_0 = \beta_0 + \beta_1 x_0 + \varepsilon_0$ — a *future random outcome* for one specific new worker of age $x_0$. Two sources of uncertainty: (i) we don't know $(\beta_0,\beta_1)$ exactly, so the line position at $x_0$ is noisy; (ii) even if we knew the line, the new worker carries its own residual $\varepsilon_0\sim\mathcal N(0,\sigma^2)$.

**Derivation.** Write the forecast error $Y_0 - \hat Y_0 = \varepsilon_0 + \bigl[(\beta_0+\beta_1 x_0) - \hat Y_0\bigr]$. The two pieces are independent (the new $\varepsilon_0$ is independent of the training sample), so the variances **add**:
$$\Var(Y_0 - \hat Y_0) \;=\; \underbrace{\sigma^2}_{\varepsilon_0\text{ noise}} \;+\; \underbrace{\sigma^2\,\ell(x_0)}_{\text{line uncertainty}} \;=\; \sigma^2\bigl[\,1 + \ell(x_0)\,\bigr].$$
The "$1+$" inside the bracket is the entire structural difference between row 6 and row 7 — the variance of a single noise draw is added to the line-position variance. Pivoting gives the boxed PI formula.

$$\boxed{\;\;PI_{1-\alpha}(Y_0) \;=\; \hat y_0 \;\pm\; t_{1-\alpha/2,\,n-2}\;s_\varepsilon\;\sqrt{1 + \frac{1}{n} + \frac{(x_0-\bar x)^2}{(n-1)s_x^2}}.\;\;}$$

**Worked example on NewHired at the SAME $x_0 = 50$.**

- Point prediction: $\hat y_0 = 64.964$ weeks (unchanged).
- $\sqrt{1+\ell(50)} = \sqrt{1.05320} = 1.02625$.
- ME: $2.014 \cdot 19.70 \cdot 1.02625 = 39.676 \cdot 1.02625 = 40.717$.
- **95% PI for one new worker at Age $=50$:** $64.964 \pm 40.717 = \boxed{[24.25,\,105.68]}$ weeks.

*Interpretation.* With 95% confidence, a *single* worker aged 50 will find a new job within $\approx 24$ to $\approx 106$ weeks. The width is dominated by the residual noise $s_\varepsilon = 19.70$ weeks — even an infinite sample cannot shrink the PI half-width below $\approx t \cdot s_\varepsilon = 2.014\cdot 19.70 = 39.68$ weeks.

**R one-liner.**

```r
predict(mod, newdata = data.frame(Age = 50),
        interval = "prediction", level = 0.95)
##        fit       lwr       upr
## 1   64.96     24.25    105.68
```

**Comment.** As $n\to\infty$ with $x_0$ fixed, $\ell(x_0)\to 0$ but the PI half-width $\to t\cdot s_\varepsilon \approx 2\sigma$ — the **irreducible-noise floor**. PI is *always* wider than CI for the same $x_0$.

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (c) <strong>Side-by-side comparison</strong> — leverage curve + the "$+1$" effect at three $x_0$ values</summary>

To **see** both the leverage shape (intervals widen as $x_0$ moves away from $\bar x$) **and** the "$+1$" effect (PI is uniformly wider than CI), we tabulate three target values:

- $x_0 = \bar x = 38.617$ (centre — leverage minimal),
- $x_0 = \bar x + s_x = 48.011$ (one SD above the mean — moderate leverage),
- $x_0 = \bar x + 3 s_x = 66.800$ (three SDs above — large leverage, near the extreme of the modelled range).

All numbers use $s_\varepsilon = 19.70$, $t_{0.975,\,45} = 2.014$, so $t\cdot s_\varepsilon = 39.676$:

| $x_0$ | $\hat y_0 = -19.5262 + 1.6898 x_0$ | $\ell(x_0) = 1/n + (x_0-\bar x)^2/((n-1)s_x^2)$ | CI half-width $= t s_\varepsilon\sqrt{\ell}$ | PI half-width $= t s_\varepsilon\sqrt{1+\ell}$ | Ratio PI/CI |
|---|---|---|---|---|---|
| $\bar x = 38.617$ | $45.745$ | $0.02128$ | $\;\;5.789$ | $40.096$ | $\mathbf{6.93}$ |
| $\bar x + s_x = 48.011$ | $61.600$ | $0.04302$ | $\;\;8.229$ | $40.520$ | $\mathbf{4.92}$ |
| $\bar x + 3 s_x = 66.800$ | $93.343$ | $0.21693$ | $18.481$ | $43.769$ | $\mathbf{2.37}$ |

**Two patterns to internalise.**

1. **Leverage shape (CI column).** CI half-width climbs from $5.79$ at the centre to $18.48$ at $\bar x + 3 s_x$ — a $\approx 3.2\times$ inflation purely from the $(x_0-\bar x)^2$ leverage term. **Both** intervals widen as $x_0$ moves away from $\bar x$ (hourglass shape).
2. **The "$+1$" effect (Ratio column).** Where the line is most tightly pinned ($x_0 = \bar x$), CI is tiny but PI is large — the **ratio is largest at the centre** ($6.93\times$). At the extreme $x_0 = \bar x + 3 s_x$ the CI has grown so much it absorbs most of the PI width, so the ratio shrinks to $2.37\times$. **PI half-width is roughly constant ($\approx 40$–$44$ weeks)** because the "$1$" inside the sqrt dominates the leverage term for moderate $\ell$ — the PI is essentially the *noise floor* $\pm t s_\varepsilon$ regardless of $x_0$.

```r
# Three-row tabulation
x0s   <- c(xbar, xbar + s_x, xbar + 3*s_x)
yhat  <- b0 + b1*x0s
lev   <- 1/n + (x0s - xbar)^2 / ((n-1)*s2_x)
me_ci <- tcrit * s_e * sqrt(lev)
me_pi <- tcrit * s_e * sqrt(1 + lev)
data.frame(x0=x0s, yhat=yhat, lev=lev,
           ME_CI=me_ci, ME_PI=me_pi, ratio = me_pi/me_ci)
##         x0    yhat       lev    ME_CI    ME_PI    ratio
## 1   38.617  45.745   0.02128    5.789   40.096    6.927
## 2   48.011  61.600   0.04302    8.229   40.520    4.924
## 3   66.800  93.343   0.21693   18.481   43.769    2.369
```

**Algebraic identity to remember.** $\widehat{SE}_{\text{PI}}^2 - \widehat{SE}_{\text{CI}}^2 = s_\varepsilon^2$ at every $x_0$ — the *gap squared* between PI and CI half-widths (per $t$-unit) is always exactly the residual variance.

</details>

---

<details class="master-subpart">
<summary>(d) <strong>Extrapolation warning</strong> — both CI and PI are valid only inside the observed support of $X$</summary>

Look at the table in (c): at $x_0 = \bar x + 3 s_x = 66.8$ years, the leverage formula already inflates the CI to $\pm 18.5$ weeks. So *isn't the formula's automatic inflation honest enough* if we keep extrapolating?

**No.** The leverage formula inflates the CI/PI *assuming* the linear-mean model $E[Y\mid X] = \beta_0 + \beta_1 X$ **still holds** at $x_0$ — i.e. the LINE assumptions (**L**inearity, **I**ndependence, **N**ormality of residuals, **E**qual variance) still apply. Outside the observed range $[\min(x_i),\,\max(x_i)]$, the **assumption itself** may break:

1. **Linearity might bend.** The true relation could plateau (e.g. an age-vs-job-search curve might saturate beyond age 65) or even invert. The leverage formula has *no way* to detect this — it keeps drawing a straight line.
2. **Equal-variance might bend.** Heteroscedasticity (the **E** in LINE) often grows at the extremes; a single $s_\varepsilon$ becomes a bad summary of true scatter.
3. **Normality of residuals** may degrade as well, breaking the $t$-distribution pivot.

**Rule of thumb.** Flag any prediction with $x_0$ outside $[\min(x_i),\,\max(x_i)]$ as **extrapolation** and refuse to publish a single number without strong domain-knowledge justification. The leverage term inflates the bands, but the LINE assumptions are what *license* both bands in the first place — see **`g15e`** for the residual-vs-fitted, Q-Q, and Cook's-distance checks that verify them.

Concrete past-exam echoes:

- **Ex 8.10a part f** ($x_0 = 30\%$ discount on a sample with $\bar x = 10\%$, $s_x = 22.6$): the formula still spits out PI $\approx (65.3,\,102.7)$, but the model has no empirical support at a 30% discount — *refuse* the prediction.
- **Ex 8.2b part b4** (`Salary = 0` on a sample with `Salary` $\in [10{,}000,\,170{,}000]$): the PI is even *centred on a negative point prediction*, since $\hat y_0(0) < 0$. The formula's leverage inflation is **not a safety net** against extrapolation.

</details>

---

<details class="master-subpart">
<summary>(e) <strong>Cross-references</strong> — where each part of this entry connects back to</summary>

- **`g15a`** — universal regression recipe + master 9-row case table; this entry **owns rows 6 and 7** of that table. The NewHired running dataset and the fitted $(\hat\beta_0,\hat\beta_1,s_\varepsilon)$ are all derived in `g15a` parts (a)–(b); do not re-derive them.
- **`g15c`** — multiple regression. The CI/PI formulas **generalise verbatim**: just replace the simple-regression leverage $\ell(x_0) = 1/n + (x_0-\bar x)^2/((n-1)s_x^2)$ with the matrix leverage $h_{00} = x_0^\top(X^\top X)^{-1} x_0$, and use df $= n - p - 1$. R's `predict(mod, newdata, interval=...)` handles both cases identically.
- **`g15e`** — diagnostics. The LINE assumptions are what *license* both intervals; extrapolation breaks them silently. The residuals-vs-fitted, Q-Q, and Cook's-distance plots verify them on the training data, but cannot rescue an extrapolation.
- **`g13a`** — universal CI recipe. **Row 6 is a CI in disguise**: target = $E[Y\mid x_0]$, estimator = $\hat y_0$, SE = $s_\varepsilon\sqrt{\ell(x_0)}$, pivot = $t_{n-2}$ — *exactly* the g13a template.
- **`g14a`** — universal hypothesis-test recipe. A $t$-test of $H_0: E[Y\mid x_0] = \theta_0$ at a hypothesised mean response $\theta_0$ is *row 2 of g14a* with $\bar X \to \hat y_0$ and SE $\to s_\varepsilon\sqrt{\ell(x_0)}$. **Rare in practice** — usually we want the CI; if a question asks whether a benchmark $\theta_0$ is plausible, the CI-test duality of g13a/g14a gives the answer for free.

</details>

---

### Summary table (NewHired; $\hat\beta_0=-19.5262$, $\hat\beta_1=1.6898$, $s_\varepsilon = 19.70$, $n=47$, df $=45$, $t_{0.975,\,45}=2.014$)

| Quantity | Value | Where |
|---|---|---|
| Universal regression recipe + 9-row master table | — | `g15a` |
| Row 6 — CI for mean response at $x_0$ | $\hat y_0 \pm t\,s_\varepsilon\sqrt{\ell(x_0)}$ | Part (a) |
| Row 7 — PI for individual at $x_0$ | $\hat y_0 \pm t\,s_\varepsilon\sqrt{1+\ell(x_0)}$ | Part (b) |
| Structural difference (the "+1") | $\widehat{SE}_{\text{PI}}^2 - \widehat{SE}_{\text{CI}}^2 = s_\varepsilon^2$ | top callout + Part (c) |
| Leverage shape (hourglass) | $\ell(x_0)$ minimal at $\bar x$, quadratic in $(x_0-\bar x)$ | Part (c) |
| Asymptotics ($n\to\infty$, $x_0$ fixed) | CI width $\to 0$; PI width $\to 2 t\,\sigma$ | Parts (a), (b) |
| 95% CI for $E[Y\mid \text{Age}=50]$ | $[55.81,\,74.12]$ weeks | Part (a) |
| 95% PI for $Y_0$ at Age $=50$ | $[24.25,\,105.68]$ weeks | Part (b) |
| Extrapolation outside $[\min x_i,\,\max x_i]$ | leverage inflation is **not** a safety net | Part (d) |
| Multi-regression generalisation: $\ell(x_0) \to x_0^\top(X^\top X)^{-1}x_0$, df $\to n-p-1$ | — | Part (e), `g15c` |
""",
    "images": ["statistics/images/master/master_g15b_ai.png"],
}

master_exercises["g14d_chi_squared"] = {
    "title": "Master Exam — Chi-squared tests (row 9: goodness-of-fit + independence, both right-tail only)",
    "content": r"""## Setup — two running datasets (one per sub-case)

This master entry walks **row 9** of the universal hypothesis-test table — the $\chi^2$ tests. Two sub-cases share the **same Pearson statistic** $X^2 = \sum (O-E)^2/E$ but differ in how $E$ is built and how df is counted, so we anchor each to one running dataset that re-appears in the horizontal cells and the past exams.

**Dataset A — for Goodness-of-Fit (Ex 7.9a, `DS$Children`).** A retailer's customer dataset `DS` records the number of children per household, summarised in $K=4$ ordered categories. On $n=750$ customers:

| Children $k$ | 0 | 1 | 2 | 3+ | total |
|---|---|---|---|---|---|
| Observed $O_k$ | 360 | 184 | 111 | 95 | $n=750$ |

The hypothesised reference is the **Italian population** distribution $p^0 = (0.76,\, 0.13,\, 0.09,\, 0.02)$ — fully specified, no parameters estimated from `DS`.

**Dataset B — for Independence (`exam_g1_2026_2a / 2b / 3a`, Credit data).** A bank records the loan purpose `PurposeLoan` ($r=5$ levels) and the employment status `EmplStatus` ($c=3$ levels) of each applicant, summarised in an $r\times c$ contingency table. After cross-tabulation, the Pearson statistic on the observed cells is

$$X^2_\text{obs} \;=\; 11.107, \qquad \text{df} \;=\; (r-1)(c-1) \;=\; 4 \times 2 \;=\; 8.$$

We reuse these **two** datasets throughout.

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) <strong>Case 9a</strong> — Chi-squared Goodness-of-Fit on Dataset A (Ex 7.4a, Ex 7.9a/b, exam_g2_2025_2a)</summary>

**Setting.** One categorical variable $X$ takes $K$ levels with population probabilities $p = (p_1,\ldots,p_K)$, $\sum_k p_k = 1$. The reference distribution $p^{(0)} = (p_1^{(0)},\ldots,p_K^{(0)})$ is **hypothesised** by the researcher — either fully specified (no parameters estimated, $q=0$) or specified up to $q$ parameters of a parametric family (e.g. Poisson, Normal — see (a3) below).

**Hypotheses.**
$$H_0:\; p \;=\; p^{(0)} \qquad \text{vs} \qquad H_1:\; p \;\ne\; p^{(0)} \;\;\text{(at least one $k$ with $p_k \ne p_k^{(0)}$).}$$

On Dataset A (`DS$Children`) the hypothesis is "the `DS` customer base distributes children as the Italian population does":
$$H_0:\; p \;=\; (0.76,\,0.13,\,0.09,\,0.02) \qquad \text{vs} \qquad H_1:\; p \;\ne\; p^{(0)}.$$

**Expected counts.** With a fully specified $p^{(0)}$, no parameter is estimated from the data:
$$\boxed{\;\; E_k \;=\; n\,p_k^{(0)}, \qquad k=1,\ldots,K. \;\;}$$
The $E_k$ depend **only** on the null, never on the observed $O_k$. On Dataset A with $n=750$:

| $k$ | $p_k^{(0)}$ | $E_k = 750\,p_k^{(0)}$ | $O_k$ | $(O_k - E_k)^2/E_k$ |
|---|---|---|---|---|
| 0   | 0.76 | $570.0$ | 360 | $(360-570)^2/570 = 77.37$ |
| 1   | 0.13 | $97.5$  | 184 | $(184-97.5)^2/97.5 = 76.74$ |
| 2   | 0.09 | $67.5$  | 111 | $(111-67.5)^2/67.5 = 28.03$ |
| 3+  | 0.02 | $15.0$  |  95 | $(95-15)^2/15 = 426.67$ |
| **sum** | $1.00$ | $\mathbf{n=750}$ | $\mathbf{n=750}$ | $\mathbf{X^2_\text{obs} = 608.81}$ |

All $E_k \ge 15 > 5$ → Cochran's rule holds (see (c)), the $\chi^2$ approximation is valid.

**Test statistic, null distribution & df.**
$$\boxed{\;\; X^2 \;=\; \sum_{k=1}^{K} \frac{(O_k - E_k)^2}{E_k} \;\overset{H_0}{\dot\sim}\; \chi^2_{K-1-q}, \qquad \text{reject } H_0 \iff X^2 > \chi^2_{1-\alpha,\,K-1-q} \;\;(\text{right tail only}). \;\;}$$

The df rule $\text{df} = K - 1 - q$ unpacks as:
- The $-1$ comes from the constraint $\sum_k p_k = 1$ — one of the $K$ probabilities is determined by the others.
- The $-q$ subtracts **one df per parameter estimated from the data** to compute $E_k$. With a fully specified $p^{(0)}$, $q=0$ and $\text{df} = K-1$. With a Poisson($\lambda$) null where $\hat\lambda$ is plugged in, $q=1$ and $\text{df} = K-2$; with Normal($\mu,\sigma^2$) and both estimated, $q=2$ and $\text{df} = K-3$. See (a3).

For Dataset A: $K=4$ cells, $q=0$ (Italian population is fully specified), so $\text{df} = 4 - 1 - 0 = 3$.

**Critical value & p-value at $\alpha = 0.05$.**
$$\chi^2_{3,\,0.95} \;=\; \texttt{qchisq(0.95, df=3)} \;=\; 7.815, \qquad p\text{-value} \;=\; \Pr(\chi^2_3 \ge 608.81) \;\approx\; 0.$$

**Decision.** $X^2_\text{obs} = 608.81 \;\gg\; 7.815$ (equivalently $p \approx 0 < 0.05$) → **reject $H_0$** at *any* conventional level: the `DS` customer base does **not** mirror the Italian distribution. The "3+ children" cell alone contributes $426.67$ of the $608.81$ — the retailer's sample massively over-represents large families relative to the population.

```r
# (a) Case 9a — chi-squared GoF, DS$Children vs Italian distribution (Ex 7.9a)
O  <- c(360, 184, 111, 95);  p0 <- c(0.76, 0.13, 0.09, 0.02);  n <- sum(O)   # 750
E  <- n * p0;          E                            # 570.0  97.5  67.5  15.0
X2 <- sum((O - E)^2 / E);  X2                       # 608.81
qchisq(0.95, df = length(O) - 1)                    # 7.815   (critical)
1 - pchisq(X2,   df = length(O) - 1)                # ~ 0     (p-value, right tail)

# Built-in equivalent — note the `p =` argument carries the fully specified p0:
chisq.test(x = O, p = p0)
## X-squared = 608.81, df = 3, p-value < 2.2e-16
```

**Side cases on the same row — uniform null, both verdicts.**

| Source | $H_0$ | $n$ | $K$ | $X^2_\text{obs}$ | df | $p$-value | Decision @ 5% |
|---|---|---|---|---|---|---|---|
| Ex 7.4a (`DS$History`) | uniform $(0.25)^4$ | 750 | 4 | 13.104 | 3 | 0.0044 | **reject** |
| Ex 7.9a (Dataset A) | $(0.76,0.13,0.09,0.02)$ | 750 | 4 | 608.81 | 3 | $\approx 0$ | **reject** |
| Ex 7.9b (4 entrances) | uniform $(0.25)^4$ | 130 | 4 | 4.523 | 3 | 0.2104 | retain |
| `exam_g2_2025_2a` (Department) | uniform $(1/3)^3$ | — | 3 | 13.696 | 2 | 0.00106 | **reject** |

The machinery is **the same recipe** in all four — only $p^{(0)}$, $n$, $K$ change.

![Master illustration](statistics/images/master/master_g14d_ai.png)

**(a3) Composite GoF — when df shrinks by $q$.** If the null is a *parametric family* (e.g. "Children is Poisson($\lambda$) for some $\lambda > 0$"), $\lambda$ is not given — you must estimate $\hat\lambda$ from the data first (e.g. MLE $\hat\lambda = \bar X$), then plug $\hat\lambda$ into $p_k^{(0)} = \Pr_{\hat\lambda}(X = k)$ to get $E_k = n\,p_k^{(0)}(\hat\lambda)$. The $\chi^2$ approximation then has df $= K - 1 - q$ with $q$ = #(parameters estimated). Examples on $K=4$ cells:

| Null family | Parameters estimated $q$ | df |
|---|---|---|
| Fully specified $p^{(0)}$ (uniform, Italian, etc.) | $0$ | $K - 1 = 3$ |
| Poisson$(\hat\lambda)$ | $1$ | $K - 2 = 2$ |
| Normal$(\hat\mu, \hat\sigma^2)$ binned into $K$ classes | $2$ | $K - 3 = 1$ |
| Binomial$(m, \hat p)$ with $m$ known | $1$ | $K - 2 = 2$ |

Same observed $X^2_\text{obs}$ but smaller df → smaller critical value → **easier to reject**: at $K = 4$, $\chi^2_{3,0.95} = 7.815$, $\chi^2_{2,0.95} = 5.991$, $\chi^2_{1,0.95} = 3.841$. The penalty for letting the data choose the null parameters is *paid in df*, not in the form of the statistic.

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) <strong>Case 9b</strong> — Chi-squared Independence on Dataset B (Ex 7.7b, exam_g1_2026_2a / 2b / 3a)</summary>

**Setting.** Two categorical variables observed jointly on $n$ units: $X$ with $r$ levels (rows) and $Y$ with $c$ levels (columns). Cross-tabulating gives an **$r \times c$ contingency table** of observed cell counts $O_{ij}$ with row totals $n_{i\cdot} = \sum_j O_{ij}$, column totals $n_{\cdot j} = \sum_i O_{ij}$, and grand total $n = \sum_{i,j} O_{ij}$.

**Hypotheses.** Independence states that the joint distribution factorises into the product of the marginals: $p_{ij} = p_{i\cdot}\,p_{\cdot j}$ for every cell.

$$H_0:\; X \;\perp\!\!\!\perp\; Y \;\;(\text{i.e. } p_{ij} = p_{i\cdot}\,p_{\cdot j}\;\forall i,j) \qquad \text{vs} \qquad H_1:\; X,Y \text{ are associated.}$$

On Dataset B (Credit data, `exam_g1_2026_2a / 2b / 3a`) with $r=5$ purpose levels and $c=3$ employment levels:
$$H_0:\; \text{PurposeLoan} \;\perp\!\!\!\perp\; \text{EmplStatus} \qquad \text{vs} \qquad H_1:\; \text{associated.}$$

**Expected counts under $H_0$.** Under independence and replacing the unknown marginal probabilities by their sample estimates $\hat p_{i\cdot} = n_{i\cdot}/n$ and $\hat p_{\cdot j} = n_{\cdot j}/n$,
$$\boxed{\;\; \widehat E_{ij} \;=\; n\,\hat p_{i\cdot}\,\hat p_{\cdot j} \;=\; \frac{n_{i\cdot}\,n_{\cdot j}}{n}. \;\;}$$

*Derivation.* Under $H_0$, $P(X=i, Y=j) = P(X=i)\,P(Y=j) = p_{i\cdot}\,p_{\cdot j}$. Plug in the marginal MLEs $\hat p_{i\cdot}, \hat p_{\cdot j}$ and multiply by $n$ to convert probability to expected count. Unlike the GoF case, $\widehat E_{ij}$ is **estimated** from the row/column marginals — that estimation is what costs df.

**Test statistic, null distribution & df.**
$$\boxed{\;\; X^2 \;=\; \sum_{i=1}^{r} \sum_{j=1}^{c} \frac{(O_{ij} - \widehat E_{ij})^2}{\widehat E_{ij}} \;\overset{H_0}{\dot\sim}\; \chi^2_{(r-1)(c-1)}, \qquad \text{reject } H_0 \iff X^2 > \chi^2_{1-\alpha,\,(r-1)(c-1)} \;\;(\text{right tail only}). \;\;}$$

The df $(r-1)(c-1)$ falls out of a parameter count: the $r \times c$ table has $rc - 1$ free joint probabilities (one is determined by the constraint $\sum_{i,j} p_{ij} = 1$); under independence the model is determined by $(r-1) + (c-1)$ marginal probabilities (one is determined per side); subtracting,
$$\text{df} \;=\; (rc - 1) \;-\; \big[(r-1) + (c-1)\big] \;=\; (r-1)(c-1).$$
On Dataset B: $\text{df} = (5-1)(3-1) = 4 \times 2 = 8$.

**Realised statistic, critical value, p-value.** From the past-exam computation,
$$X^2_\text{obs} \;=\; 11.107, \qquad \chi^2_{8,\,0.95} \;=\; \texttt{qchisq(0.95, df=8)} \;=\; 15.51, \qquad p\text{-value} \;=\; \Pr(\chi^2_8 \ge 11.107) \;\approx\; 0.196.$$

**Decision at $\alpha = 0.05$.** $X^2_\text{obs} = 11.107 < 15.51$, equivalently $p \approx 0.196 \gg 0.05$ → **do not reject $H_0$**. The data are compatible with `PurposeLoan` and `EmplStatus` being independent: knowing a borrower's employment status does **not** measurably change the distribution of loan purposes (and vice versa). The verdict holds at $\alpha = 0.10$ too ($\chi^2_{8,\,0.90} = 13.36 > 11.107$).

```r
# (b) Case 9b — chi-squared independence on a contingency table (exam_g1_2026_2a/2b/3a)
tab <- table(Credit$PurposeLoan, Credit$EmplStatus)   # 5 x 3 observed counts
chisq.test(tab)                                       # the canonical one-liner
## X-squared = 11.107, df = 8, p-value = 0.1958

# Manual unpack of what chisq.test does:
E  <- outer(rowSums(tab), colSums(tab)) / sum(tab)    # expected = row*col / n
X2 <- sum((tab - E)^2 / E);  X2                       # 11.107
df <- (nrow(tab) - 1) * (ncol(tab) - 1);  df          # 8
qchisq(0.95, df = df)                                 # 15.51   (right-tail critical)
1 - pchisq(X2, df = df)                               # 0.1958  (right-tail p-value)

# Diagnostics: where would the association live, if any?
chisq.test(tab)$expected                              # all >= 5 here -> Cochran OK
chisq.test(tab)$stdres                                # standardized Pearson residuals
```

**Side case on the same row — Ex 7.7b ($5 \times 5$, association dominant).** `Age_Class × LearnTool` on the Developers_ITA dataset gives $X^2_\text{obs} = 115.69$ on $\text{df} = (5-1)(5-1) = 16$, with $\chi^2_{16,\,0.9} = 23.54$ and $p \approx 0$ — **reject $H_0$** at any level: developers' upskilling tool depends strongly on age group. The same $\chi^2$ recipe; only the table and the df change.

| Source | Table size | $X^2_\text{obs}$ | df | $\chi^2_{1-\alpha,\,\rm df}$ | $p$-value | Decision |
|---|---|---|---|---|---|---|
| Dataset B (`exam_g1_2026_2a/2b/3a`) | $5 \times 3$ | $11.107$ | $8$ | $15.51$ @ 5% | $0.196$ | retain |
| Ex 7.7b (Developers_ITA) | $5 \times 5$ | $115.69$ | $16$ | $23.54$ @ 10% | $\approx 0$ | **reject** |

</details>

---

<details class="master-subpart">
<summary>(c) Validity rule (Cochran), low expected counts, Yates' continuity correction, Fisher's exact test</summary>

The $\chi^2$ approximation rests on a normal approximation to each cell count: under $H_0$, $(O_{ij} - E_{ij})/\sqrt{E_{ij}} \approx \mathcal{N}(0,1)$. This breaks down when $E_{ij}$ is small — the discrete count is fat-tailed, and the squared ratio $(O - E)^2 / E$ is dominated by tiny denominators, inflating $X^2$ spuriously.

**Cochran's rule.** The asymptotic $\chi^2$ approximation is reliable when
$$\boxed{\;\;\widehat E_k \ge 5 \;\;\text{(GoF) or}\;\; \widehat E_{ij} \ge 5 \;\;\text{(Independence) in every cell.}\;\;}$$
Lenient version: at most 20% of cells with $\widehat E < 5$ and **no** cell with $\widehat E < 1$.

On the running datasets the rule is comfortably satisfied: Dataset A has $\min E_k = 15$, Dataset B has every $\widehat E_{ij}$ above 5 (visible from `chisq.test(tab)$expected`). On Ex 7.7b's part (c), pairing `Age_Class` with `AISearch` instead, the `AISearch = Other` column has $\widehat E \approx 2.77$ — the rule **fails**, R prints a warning, and the test cannot be reported as-is.

**What to do when the rule fails.**

| Situation | Remedy |
|---|---|
| One or two sparse columns, substantively mergeable | **Merge** the sparse level with a neighbour (e.g. "Other" $\cup$ "Rare") and re-run `chisq.test`. The new df adjusts automatically. |
| $2 \times 2$ table with small expected counts | **Yates' continuity correction**: replace $(O-E)^2$ by $(|O-E| - 0.5)^2$ in every cell. R does this by default in `chisq.test()` on a $2 \times 2$ table — pass `correct = FALSE` to disable. |
| $2 \times 2$ table, *very* small counts | **Fisher's exact test** (`fisher.test`): conditions on both marginals, computes the *exact* hypergeometric p-value — no asymptotic at all. Recommended whenever any $\widehat E_{ij} < 5$ in a $2 \times 2$. |
| Large $r \times c$ with several sparse cells | **Monte-Carlo p-value**: `chisq.test(tab, simulate.p.value = TRUE, B = 10000)` — bypasses the asymptotic by simulating from the null. |

**Yates' continuity correction (only for $2 \times 2$).** For a $2 \times 2$ table $\begin{pmatrix} a & b \\ c & d \end{pmatrix}$ with $n = a+b+c+d$, the Yates-corrected statistic is
$$X^2_\text{Yates} \;=\; \frac{n\,\big(|ad - bc| - n/2\big)^2}{(a+b)(c+d)(a+c)(b+d)},$$
shrinking $|O-E|$ by $\tfrac12$ in every cell to better approximate the discrete distribution by a continuous $\chi^2_1$. The correction is **conservative** — it shrinks $X^2$, enlarges the $p$-value, makes rejection harder. Use it only on $2 \times 2$ tables with moderate counts; for very small counts prefer `fisher.test`.

```r
# Diagnostic: report expected counts and Cochran's rule on Dataset B
res <- chisq.test(tab)
res$expected                                          # full E matrix
min(res$expected)                                     # smallest cell
mean(res$expected < 5)                                # share below 5 (should be 0)

# Yates correction on 2x2 (default in chisq.test)
chisq.test(matrix(c(a, b, c, d), 2, 2))               # WITH Yates correction
chisq.test(matrix(c(a, b, c, d), 2, 2), correct=FALSE)# Pearson (no correction)

# Fisher exact (2x2, any counts) and Monte-Carlo for r x c
fisher.test(matrix(c(a, b, c, d), 2, 2))
chisq.test(tab, simulate.p.value = TRUE, B = 10000)
```

</details>

---

<details class="master-subpart">
<summary>(d) Why right-tail only — the geometric reason</summary>

Both row-9 statistics have the same shape:
$$X^2 \;=\; \sum_{\text{cells}} \frac{(O - E)^2}{E} \;\ge\; 0,$$
a **sum of squared, standardised cell deviations**. Three properties follow directly from the squaring:

1. **Non-negativity.** $X^2 \ge 0$ for every possible sample, with $X^2 = 0$ iff $O = E$ in every cell — i.e. the data exactly match the null prediction. There is no value of $X^2$ that *contradicts* a small $X^2$.
2. **Direction-blindness.** The squaring kills the sign of $O - E$. A cell with $O$ *above* $E$ contributes the same as a cell with $O$ *below* $E$ by the same amount. The statistic reports **magnitude of mismatch**, not direction.
3. **Monotone evidence against $H_0$.** Larger $X^2$ = larger total mismatch = stronger evidence against $H_0$. Smaller $X^2$ = better fit / closer to independence = *consistent* with $H_0$ (never evidence *for* $H_0$ — the test is asymmetric in that sense).

This is why both tests are **strictly right-tailed**:
$$\boxed{\;\;\text{Reject } H_0 \iff X^2_\text{obs} \;>\; \chi^2_{1-\alpha,\,\rm df}, \qquad p\text{-value} \;=\; \Pr(\chi^2_{\rm df} \ge X^2_\text{obs}).\;\;}$$
There is no "two-sided" $\chi^2$ test, no "$|X^2|$" notation, no $\alpha/2$ split. A small $X^2$ is not "evidence in the opposite direction" the way a small $|Z|$ on a $z$-test is — small $X^2$ is just **good fit**, the *retain* outcome.

**Contrast with the $z$/$t$ tests of rows 1–8.** Those use $T = (\hat\theta - \theta_0)/\widehat{\rm SE}$, which is **signed** — its sign tells you whether $\hat\theta$ over- or under-shoots $\theta_0$. That direction is meaningful and is what makes "$H_1:\theta > \theta_0$" vs "$H_1:\theta < \theta_0$" vs "$H_1:\theta \ne \theta_0$" three genuinely different tests with three different rejection regions (see `g14a` decision-rules mini-table). For the $\chi^2$ tests of row 9 the squaring removes that asymmetry — every alternative collapses to "$O$ and $E$ disagree somewhere", which is always a right-tail event.

</details>

---

<details class="master-subpart">
<summary>(e) Cross-references — how row 9 connects to the rest of the course</summary>

The $\chi^2$ tests sit in a dense web of connections with the other inferential sub-topics:

- **`g14a` (one-sample tests, universal recipe).** Row 9 is one slot in the master case table that `g14a` introduces. The 3-step procedure (state $H_0/H_1$, build statistic, decide via critical value or $p$-value) carries over verbatim — only the statistic and the null distribution change.

- **`g14b` (two-sample tests, row 7).** A **$2 \times 2$ independence test** is **mathematically equivalent** to a **two-proportion $z$-test** of $H_0: p_A = p_B$ (g14b row 7): the same data, the same null, and $X^2 = Z^2$ exactly. They give identical $p$-values. The $\chi^2_1$ generalisation handles two-sided alternatives only — for a *one-sided* difference of proportions you must use the $z$-test directly (`g14b`). Independence on an $r \times c$ table with $r > 2$ or $c > 2$ generalises this to "comparing the distribution of one categorical across $J$ groups", which has no two-sample $z$ analogue — the $\chi^2$ becomes the unique tool.

- **`g13a` (CI for a single proportion), `g13b` (CI for one mean), `g13d` (CI for difference of proportions).** CIs and tests are complementary: a CI gives a *range* of plausible parameter values, a $\chi^2$ test gives a *yes/no* on a distributional hypothesis. For the $2 \times 2$ case the chi-squared independence test agrees with the two-sample test of $H_0: p_A = p_B$, which in turn is the CI-from-`g13d` reject-iff-CI-excludes-0 rule. No analogous CI exists for "two categoricals are independent" — the test is the natural lens.

- **`g13f` (point estimators).** The marginal proportions $\hat p_{i\cdot}$, $\hat p_{\cdot j}$ that feed $\widehat E_{ij} = n\,\hat p_{i\cdot}\,\hat p_{\cdot j}$ are the MLEs of the marginals under $H_0$ — exactly the unbiased / consistent estimators derived once in `g13f`. The whole independence test is built on plug-in estimation of the null model.

- **`g14c` (paired tests).** Conceptually related but mechanically distinct: paired tests live on row 8 (continuous within-pair difference reduced to a one-mean $t$), $\chi^2$ tests live on row 9 (categorical counts vs expected). The shared theme is *reducing a two-sample-looking design to a one-sample machinery*: pairs become $d_i = x_i - y_i$, contingency tables become a single $X^2$ statistic. No raw two-sample SE.

- **`g14e` (power and type-II for $\chi^2$ — next row).** Power for $\chi^2$ tests uses the **non-central $\chi^2_\text{df}(\lambda)$** distribution with non-centrality $\lambda = n\,\sum_k (p_k^{\rm true} - p_k^{(0)})^2 / p_k^{(0)}$ (GoF) or the analogous Pearson "effect size" $w^2$ (Cohen) for independence. Same $\alpha,n$ logic as rows 1–8, only the reference distribution shifts off-centre. See **`g14e`** for the full sample-size-vs-power table on row 9.

</details>

---

### Summary table (one-page recap of the two cases).

| Quantity | GoF (Dataset A, `DS$Children` vs Italian) | Independence (Dataset B, `PurposeLoan` × `EmplStatus`) |
|---|---|---|
| Variables | one categorical, $K = 4$ levels | two categorical, $r \times c = 5 \times 3$ table |
| Null | $p = (0.76, 0.13, 0.09, 0.02)$ (fully specified) | rows $\perp\!\!\!\perp$ cols |
| $E$ formula | $E_k = n\,p_k^{(0)}$ | $\widehat E_{ij} = n_{i\cdot}\,n_{\cdot j}/n$ |
| Parameters estimated $q$ | $0$ | $(r-1)+(c-1) = 6$ |
| df | $K - 1 - q = 3$ | $(r-1)(c-1) = 8$ |
| Smallest $E$ | $15$ (Cochran OK) | $\ge 5$ (Cochran OK) |
| $X^2_\text{obs}$ | $608.81$ | $11.107$ |
| $\chi^2_{1-\alpha,\,\rm df}$ at $\alpha = 0.05$ | $7.815$ | $15.51$ |
| $p$-value (right tail) | $\approx 0$ | $0.196$ |
| Decision @ $\alpha = 0.05$ | **reject** — DS does not match Italy | **retain** — purpose $\perp\!\!\!\perp$ employment |
| R one-liner | `chisq.test(O, p = p0)` | `chisq.test(table(X, Y))` |
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

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (d) How many classes? Sturges and $\sqrt n$.</summary>

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

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (e) Reading the picture: skew, mode, gaps.</summary>

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

</details>

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (f) The common error: plotting counts $f_i$ with unequal widths.</summary>

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

</details>

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

---

**Bimodality — when the picture shows *two* peaks.** A *unimodal* histogram has one tall central class; a *bimodal* histogram has **two locally-tall classes separated by a valley**. Bimodality is a diagnostic for **two mixed sub-populations** (e.g. customers with short vs long visits, two brands sharing the variable). In **Ex 1.5g** the variable `Time` ($n=1800$) has two local density highs at $[10,20)$ (the modal class) and $[60,90)$, separated by the much lower-density class $[30,60)$ — the visual signature of *two typical customer behaviours* (a quick visit and a long visit). With bimodal data, **a single mean and median both fall in the valley** and mis-represent both sub-populations: report the *bimodality* as the headline finding and consider splitting the sample.
""",
    "images": ["statistics/images/master/master_g1c_hist_ai.png"],
}

master_exercises["g13f_estimation"] = {
    "title": "Master Exam — Unbiased estimators and sampling SE (consolidated)",
    "content": r"""## Setup — running dataset (matches `5_13a1` / `5_13a2`)

Treat the Milan sub-sample of the `pizzerie` dataframe as $n=80$ i.i.d. draws of monthly turnover `Sales` from a population with mean $\mu$ and (per the question statement) **known** SD $\sigma = 11\,500$ €. To support the raw-sums computational form in subpart (d) we will use the summary
$$n = 80, \qquad \sum_{i=1}^n x_i \;=\; 2\,560\,000, \qquad \sum_{i=1}^n x_i^{\,2} \;=\; 9.2368\times 10^{10},$$
so that $\bar x = 32\,000$ € and the sample SD $s$ recovered from these sums is $\approx 11\,500$ € (the same number used as the "known $\sigma$" in `5_13a1`). For the proportion thread (subpart b, row 3) we additionally use the no-smoking indicator with $\hat p = 0.55$ on the same $n=80$ pizzerias (the illustrative numbers from `5_13a3`).

<details class="master-subpart" open>
<summary>(a) Estimator vs estimate; unbiasedness; why it is necessary but not sufficient (MSE).</summary>

**Estimator vs estimate.** An **estimator** is a *function* of the data, $T = T(X_1,\dots,X_n)$ — a random variable, evaluated before the data are observed. An **estimate** is the *realised numerical value* $T(x_1,\dots,x_n)$ obtained after observing the sample. The estimator $\bar X$ has a sampling distribution; the estimate $\bar x = 32\,000$ € is a single number on the page. Unbiasedness is a property of the *estimator* (a statement about its sampling distribution), never of a single estimate.

**Unbiasedness.** $T$ is **unbiased** for $\theta$ iff
$$\mathbb E[T] \;=\; \theta \qquad \text{for every admissible value of } \theta.$$
Equivalently, the **bias** $\operatorname{bias}(T) = \mathbb E[T] - \theta$ is identically zero. Geometrically: across the (hypothetical) population of all samples of size $n$, the sampling distribution of $T$ is *centred on the truth*. The "for every $\theta$" matters — an estimator that happens to equal $\theta$ only at one specific value is not unbiased.

**Unbiasedness is not enough on its own — spread matters too (MSE).** The **mean squared error** of $T$ decomposes as
$$\boxed{\;\;\operatorname{MSE}(T) \;=\; \mathbb E[(T-\theta)^2] \;=\; \operatorname{Var}(T) \;+\; \operatorname{bias}(T)^2\;\;}$$
so a *high-variance* unbiased estimator can be worse on average than a *slightly biased* low-variance one. The four estimators of subpart (b) are all unbiased **and** have variance shrinking like $1/n$ — the best of both worlds. This is why unbiasedness shows up in every CI: under unbiasedness, $\widehat{SE}(\widehat\theta)$ measures *the whole error* (no hidden offset), and the CI $\widehat\theta\pm c\,\widehat{SE}$ correctly covers $\theta$ at the nominal rate.

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) The four core estimators and their SEs — the table every CI in G13 reads from.</summary>

| # | Parameter | Unbiased estimator $\widehat\theta$ | $\mathrm{Var}(\widehat\theta)$ | $\widehat{SE}(\widehat\theta)$ (plug-in) | One-line proof of unbiasedness | Used in |
|---|---|---|---|---|---|---|
| 1 | $\mu$ | $\bar X = \tfrac{1}{n}\sum_i X_i$ | $\sigma^2/n$ | $\sigma/\sqrt n$ (known $\sigma$) or $s/\sqrt n$ (plug-in) | $\mathbb E[\bar X] = \tfrac{1}{n}\sum_i \mathbb E[X_i] = \mu$ | **`g13a`** rows 1–2; **`g14a`** |
| 2 | $p$ (Bernoulli) | $\hat p = \tfrac{1}{n}\sum_i X_i = X/n$ with $X\sim\mathrm{Bin}(n,p)$ | $p(1-p)/n$ | $\sqrt{\hat p(1-\hat p)/n}$ | $\mathbb E[\hat p] = \mathbb E[X]/n = np/n = p$ | **`g13b`**; **`g14a`** row 3 |
| 3 | $\sigma^2$ | $S^2 = \tfrac{1}{n-1}\sum_i (X_i-\bar X)^2$ | $\tfrac{2\sigma^4}{n-1}$ (Normal data) | — (rare in this course) | $\mathbb E[\sum(X_i-\bar X)^2] = (n-1)\sigma^2$, divide by $n-1$ | feeds $s/\sqrt n$ in row 1; subpart (c) |
| 4 | $\mu_D$ (paired) | $\bar D = \tfrac{1}{n}\sum_i D_i$, $D_i = X_i - Y_i$ | $\sigma_D^2/n$ | $s_D/\sqrt n$ | Row 1 applied to the differences $D_i$ | **`g13e`**; **`g14c`** |

**One-line derivations of the variances** (the SEs are square roots):

- **Row 1.** Independence: $\mathrm{Var}(\bar X) = \tfrac{1}{n^2}\sum_i \mathrm{Var}(X_i) = \tfrac{1}{n^2}(n\sigma^2) = \sigma^2/n$.
- **Row 2.** $X = \sum_i X_i$ is $\mathrm{Bin}(n,p)$ with variance $np(1-p)$, so $\mathrm{Var}(\hat p) = \mathrm{Var}(X)/n^2 = p(1-p)/n$. (Or apply row 1 to $X_i\in\{0,1\}$ with $\sigma^2 = p(1-p)$.)
- **Row 3.** Under Normality, $(n-1)S^2/\sigma^2 \sim \chi^2_{n-1}$, so $\mathrm{Var}(S^2) = 2\sigma^4/(n-1)$. (Derived from $\mathrm{Var}(\chi^2_{n-1}) = 2(n-1)$.)
- **Row 4.** Replace $X_i\rightsquigarrow D_i$ in row 1.

**Why these four cover every CI in G13.** Rows 1 + 3 give the **one-mean CI** (`g13a`, rows 1–2 of its universal table). Row 2 gives the **one-proportion CI** (`g13b`, row 3). Rows 1 + 3 applied to two *independent* samples and then *differenced* give the **two-mean CI** (`g13c`, rows 4–6). Row 2 differenced gives the **two-proportion CI** (`g13d`, row 7). Row 4 is the **paired CI** (`g13e`, row 8). Hence the four-row table above *is* the estimator content of all of g13.

**Plug-in on the running dataset.**
$$\hat\mu = \bar x = 32\,000,\quad SE(\bar X)\big|_{\sigma\text{ known}} = \tfrac{11\,500}{\sqrt{80}} \approx 1\,285.7,\quad \hat p = 0.55,\quad \widehat{SE}(\hat p) = \sqrt{\tfrac{0.55\cdot 0.45}{80}} \approx 0.0556.$$

```r
n     <- 80
xbar  <- 32000;   sigma <- 11500          # 'known' sigma  (5_13a1)
SE_xbar_known <- sigma / sqrt(n);  SE_xbar_known     # ~1285.7  (exact, no plug-in)

phat  <- 0.55                              # SmokingArea = No (5_13a3 illustration)
SE_phat <- sqrt(phat*(1-phat) / n);  SE_phat         # ~0.0556  (plug-in)
```

</details>

---

<details class="master-subpart">
<summary>(c) Bessel correction — why $S^2$ divides by $n-1$, not by $n$.</summary>

The "naive" variance estimator $\tilde S^2 = \tfrac{1}{n}\sum_i (X_i - \bar X)^2$ — the **MLE** of $\sigma^2$ under Normality — is **biased downward**. Using the algebraic identity $\sum_i (X_i-\bar X)^2 = \sum_i (X_i-\mu)^2 - n(\bar X-\mu)^2$ and taking expectations,
$$\mathbb E\!\Big[\sum_{i=1}^n (X_i-\bar X)^2\Big] \;=\; n\sigma^2 \;-\; n\cdot \tfrac{\sigma^2}{n} \;=\; (n-1)\sigma^2,$$
so $\mathbb E[\tilde S^2] = \tfrac{n-1}{n}\sigma^2 < \sigma^2$. Dividing by $n-1$ instead of by $n$ exactly cancels the missing factor:
$$\boxed{\;\;S^2 \;=\; \frac{1}{n-1}\sum_{i=1}^n (X_i-\bar X)^2 \quad\Longrightarrow\quad \mathbb E[S^2] \;=\; \sigma^2\;\;}$$

**Intuition.** One degree of freedom has been "spent" estimating $\mu$ via $\bar X$ — once $\bar X$ is known, only $n-1$ of the centred deviations $X_i-\bar X$ are free (they must sum to zero). Averaging by $n-1$ restores unbiasedness.

**Numeric example.** Take $n=5$ Normal draws with $\sigma^2 = 1$. Across $B=20\,000$ replicates:

```r
set.seed(1)
B <- 20000;  mu <- 0;  sig <- 1;  nn <- 5     # small n exaggerates the bias
samp     <- matrix(rnorm(B*nn, mu, sig), nrow = B)
S2_un    <- apply(samp, 1, var)               # divisor n-1 -> S^2 (unbiased)
S2_mle   <- rowMeans((samp - rowMeans(samp))^2)  # divisor n -> tilde S^2 (biased)
mean(S2_un);    mean(S2_mle)                  # ~1.000      ~0.800   ((n-1)/n=4/5=0.8)
```

The MLE systematically undershoots by the factor $(n-1)/n = 0.8$ at $n=5$. By $n=100$ the factor is $0.99$ — the bias is small but **never** zero for any finite $n$, which is why the textbook estimator always carries the Bessel correction.

</details>

---

<details class="master-subpart">
<summary>(d) Computational form — turning raw sums $\sum x_i$ and $\sum x_i^2$ into $\bar x$, $s^2$, $\widehat{SE}(\bar X)$.</summary>

When the exam gives only $n$, $\sum x_i$ and $\sum x_i^2$ (a common pattern: see `5_2a`, `5_3a`, `5_13a1`, `6_13d`, ...), use the **computational form** derived from the identity $\sum_i (x_i-\bar x)^2 = \sum_i x_i^2 - n\bar x^{\,2}$:
$$\boxed{\;\;\bar x \;=\; \frac{1}{n}\sum_{i=1}^n x_i, \qquad s^2 \;=\; \frac{1}{n-1}\!\left(\sum_{i=1}^n x_i^{\,2} \;-\; n\,\bar x^{\,2}\right), \qquad \widehat{SE}(\bar X) \;=\; \frac{s}{\sqrt n}.\;\;}$$

The $n-1$ divisor is the Bessel correction from subpart (c). For a **frequency table** ($x_k$ with counts $f_k$, $\sum_k f_k = n$): same formulas with $\sum_i x_i \rightsquigarrow \sum_k x_k f_k$ and $\sum_i x_i^2 \rightsquigarrow \sum_k x_k^{\,2} f_k$.

**Worked example on the running dataset** ($n=80$, $\sum x_i = 2\,560\,000$, $\sum x_i^2 = 9.2368\times 10^{10}$):
1. $\bar x = 2\,560\,000 / 80 = 32\,000$ €.
2. $n\bar x^{\,2} = 80\cdot 32\,000^2 = 80\cdot 1.024\times 10^9 = 8.192\times 10^{10}$.
3. $\sum x_i^2 - n\bar x^{\,2} = 9.2368\times 10^{10} - 8.192\times 10^{10} = 1.0448\times 10^{10}$.
4. $s^2 = 1.0448\times 10^{10}/79 \approx 1.3225\times 10^{8}$, so $s \approx 11\,500$ €.
5. $\widehat{SE}(\bar X) = 11\,500/\sqrt{80} \approx 1\,285.7$ €.

The recovered $s\approx 11\,500$ matches the "known $\sigma$" the question hands you — sanity check passes.

```r
n      <- 80
sum_x  <- 2.56e6
sum_x2 <- 9.2368e10
xbar   <- sum_x / n;                              xbar       # 32000
s2     <- (sum_x2 - n*xbar^2) / (n - 1);          s2         # ~1.3225e8
s_hat  <- sqrt(s2);                               s_hat      # ~11500
SE_hat <- s_hat / sqrt(n);                        SE_hat     # ~1285.7
```

</details>

---

<details class="master-subpart">
<summary>(e) Pitfall — $S$ is <strong>not</strong> unbiased for $\sigma$.</summary>

**Box.** $S^2$ is unbiased for $\sigma^2$, but $S = \sqrt{S^2}$ is **not** unbiased for $\sigma$:
$$\mathbb E[S] \;<\; \sigma \quad\text{(strict; equality only in the degenerate $\sigma=0$ case).}$$

**Why.** Jensen's inequality on the **concave** function $\sqrt{\cdot}$: for any non-degenerate positive random variable $Y$, $\mathbb E[\sqrt Y] < \sqrt{\mathbb E[Y]}$. Plug $Y = S^2$ and use $\mathbb E[S^2] = \sigma^2$:
$$\mathbb E[S] \;=\; \mathbb E[\sqrt{S^2}] \;<\; \sqrt{\mathbb E[S^2]} \;=\; \sigma.$$

**How much downward bias?** Under Normality the exact bias correction is $\mathbb E[S] = c_4(n)\,\sigma$ with $c_4(n) = \sqrt{\tfrac{2}{n-1}}\cdot \tfrac{\Gamma(n/2)}{\Gamma((n-1)/2)}$. The factor $c_4(n)$ rises rapidly to $1$: $c_4(5)\approx 0.940$, $c_4(10)\approx 0.973$, $c_4(30)\approx 0.991$, $c_4(100)\approx 0.997$. So the bias is **tiny for $n\gtrsim 30$** and is *ignored* in the entire CI/test recipe — but it is a classic exam trick to ask "is $S$ unbiased for $\sigma$?" The answer is **no** even though $S^2$ is unbiased for $\sigma^2$.

</details>

---

<details class="master-subpart">
<summary>(f) Cross-references — where each estimator gets used downstream.</summary>

- Row 1 ($\bar X$, $s/\sqrt n$) → **`g13a`** rows 1–2: the one-mean CI in $\sigma$-known ($z$) and $\sigma$-unknown ($t$) regimes.
- Row 2 ($\hat p$, $\sqrt{\hat p(1-\hat p)/n}$) → **`g13b`** row 3: the one-proportion CI (with the $n\hat p, n(1-\hat p)\ge 5$ CLT rule of thumb).
- Rows 1 + 3 applied to two independent samples and then differenced → **`g13c`** rows 4–6: the two-mean CI in the three variance regimes (known $\sigma$'s, pooled $S_p$ after Levene, Welch).
- Row 2 differenced (two independent proportions) → **`g13d`** row 7: the two-proportion CI uses the *unpooled* SE (the *pooled* SE belongs to the test in `g14b`).
- Row 4 ($\bar D$, $s_D/\sqrt n$) → **`g13e`** row 8: the paired CI is row 1 applied to within-subject differences $D_i = X_i - Y_i$.

Forward to G14: all the same estimators get reused, the only change is that the critical value $c_{1-\alpha/2}$ gets replaced by a tail-probability cutoff to control the type-I error.

</details>

---

### Summary table (running dataset — Milan pizzeria sub-sample, $n=80$).

| Parameter | Estimator $\widehat\theta$ | $\mathbb E[\widehat\theta]$ | $\mathrm{Var}(\widehat\theta)$ | $\widehat{SE}(\widehat\theta)$ | Plug-in value | Used in |
|---|---|---|---|---|---|---|
| $\mu$ | $\bar X$ | $\mu$ | $\sigma^2/n$ | $\sigma/\sqrt n$ or $s/\sqrt n$ | $\widehat{SE}\approx 1\,286$ | `g13a` rows 1–2; `g14a` |
| $p$ | $\hat p$ | $p$ | $p(1-p)/n$ | $\sqrt{\hat p(1-\hat p)/n}$ | $\widehat{SE}\approx 0.0556$ | `g13b`; `g14a` row 3 |
| $\sigma^2$ | $S^2 = \tfrac{1}{n-1}\sum(X_i-\bar X)^2$ | $\sigma^2$ | $\tfrac{2\sigma^4}{n-1}$ (Normal) | — | $s^2 \approx 1.32\times 10^8$ | feeds the SE of row 1 |
| $\mu_D$ | $\bar D$ | $\mu_D$ | $\sigma_D^2/n$ | $s_D/\sqrt n$ | (paired study) | `g13e`; `g14c` |

Key gotcha (subpart e): $S^2$ unbiased for $\sigma^2$ does **not** imply $S$ unbiased for $\sigma$ — Jensen's inequality on $\sqrt{\cdot}$ gives $\mathbb E[S] < \sigma$.

![Master illustration](statistics/images/master/master_g13f_ai.png)
""",
    "images": ["statistics/images/master/master_g13f_ai.png"],
}

master_exercises["g15d_categorical"] = {
    "title": "Master Exam — Categorical predictors, dummies & interactions (rows 8–9 of the universal regression table)",
    "content": r"""## Setup — running dataset for every numeric example below

A consultancy collected the dataframe **GS** ($n = 100$ junior employees) with

- $Y = \text{Salary}$ (net monthly salary, €),
- $X = \text{grade}$ (annual performance score, $0$–$100$, continuous),
- $Z_1 = \text{sex}$ (binary factor, levels $\{F,M\}$, $F$ = reference),
- $Z_2 = \text{course}$ ($K=3$-level factor, training track $\{a,b,c\}$, $a$ = reference).

This *one* dataset will carry **every** subpart below — first as a "$K$-level factor turned into $K-1$ dummies" example (course, with $K=3$), then as a "continuous $\times$ dummy interaction" example (sex $\times$ grade).

The OLS fit of the **full additive model**

$$\boxed{\;\;\mathcal M_1:\quad\text{Salary}_i \;=\; \beta_0 \;+\; \beta_g\,\text{grade}_i \;+\; \beta_M\,D^{sex}_{M,i} \;+\; \beta_b\,D^{course}_{b,i} \;+\; \beta_c\,D^{course}_{c,i} \;+\; \varepsilon_i\;\;}$$

returns

$$\hat\beta_0 = 1\,400,\quad \hat\beta_g = 35,\quad \hat\beta_M = 2\,000,\quad \hat\beta_b = 450,\quad \hat\beta_c = -150,$$

with residual SE $\hat\sigma = 620$, $R^2 = 0.612$, df $= n - p - 1 = 100 - 4 - 1 = 95$. Subpart **(b)** compares $\mathcal M_1$ to the **reduced** $\mathcal M_0$ that drops both course dummies; subpart **(d)** compares it to the **extended** $\mathcal M_2$ with a $sex\times grade$ interaction.

```r
GS$sex    <- factor(GS$sex,    levels = c("F","M"))      # F = baseline
GS$course <- factor(GS$course, levels = c("a","b","c"))  # a = baseline
fit1 <- lm(Salary ~ grade + sex + course, data = GS); summary(fit1)
```

Round to 4 decimals throughout.

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) Row 8 — Dummy coding for a $K$-level factor (`course`, $K=3$)</summary>

A nominal predictor with $K$ levels enters $X$ as $K-1$ binary indicators (treatment / reference coding — R's default). If we put all $K$ dummies AND an intercept the columns sum to the all-ones vector $\mathbf 1$, $X^\top X$ is singular and OLS does not exist — the **dummy-variable trap**. For `course` ($K=3$, reference $a$):

$$D^{course}_{b,i} \;=\; \mathbb 1\{course_i = b\},\qquad D^{course}_{c,i} \;=\; \mathbb 1\{course_i = c\},$$

so $course=a$ is identified by $D_b = D_c = 0$. Same for `sex` ($K=2$): one dummy $D^{sex}_M = \mathbb 1\{sex=M\}$. The design matrix $X$ of $\mathcal M_1$ has columns $[\mathbf 1,\;\text{grade},\;D^{sex}_M,\;D^{course}_b,\;D^{course}_c]$ — exactly $1+1+(K_{sex}-1)+(K_{course}-1)=5$ columns. Everything else (OLS formula $\hat{\boldsymbol\beta} = (X^\top X)^{-1}X^\top y$, the $\widehat{SE}$ from $\hat\sigma^2(X^\top X)^{-1}$, the projection $\hat y = X\hat{\boldsymbol\beta}$) is *exactly* g15c with this enlarged $X$ — **do not re-derive**, see g15c subpart (a).

**Reading each $\hat\beta_k$ — mean shift vs the reference, ceteris paribus.**

- $\hat\beta_M = +2\,000$ € : at the same `grade` and `course`, **men** earn on average $+2\,000$ €/month vs **women**.
- $\hat\beta_b = +450$ € : at the same `grade` and `sex`, attending **course $b$** is worth $+450$ €/month vs **course $a$**.
- $\hat\beta_c = -150$ € : at the same `grade` and `sex`, attending **course $c$** is worth $-150$ €/month vs **course $a$**.
- The **intercept** $\hat\beta_0 = 1\,400$ € is the expected `Salary` at the reference cell with **continuous predictors set to 0**: $\mathbb E[\text{Salary}\mid grade=0,\,sex=F,\,course=a] = 1\,400$. (Often not economically meaningful — see the centring tip in (d).)

A non-dummy contrast like "**course $b$ vs course $c$**, $sex$ and $grade$ fixed" is **not** a row of `summary(fit1)`; it is the linear combination $\hat\beta_b - \hat\beta_c = 450 - (-150) = +600$ €/month, with SE $\sqrt{L^\top\,\widehat{\Var}(\hat{\boldsymbol\beta})\,L}$ for $L = (0,0,0,1,-1)$ — this is precisely the move asked by `exam_g2_2025_4a` (IT vs Operations contrast, where the reference is HR).

```r
contrasts(GS$course)                                        # 2 dummy columns: courseb, coursec
fit1 <- lm(Salary ~ grade + sex + course, data = GS)
summary(fit1)                                               # b0=1400, bg=35, bM=2000, bb=450, bc=-150

# Three ceteris-paribus predictions
b0 <- 1400; bg <- 35; bM <- 2000; bb <- 450; bc <- -150
b0 + bg*70                                                  # Woman, grade 70, course a -> 3850
b0 + bg*70 + bM                                             # Man,   grade 70, course a -> 5850
b0 + bg*70 + bM + bb                                        # Man,   grade 70, course b -> 6300

# Contrast course b vs c (not a row of summary): point estimate + SE
L  <- c(0, 0, 0, 1, -1)
est_bc <- sum(L * coef(fit1));                est_bc        # 600
SE_bc  <- sqrt(t(L) %*% vcov(fit1) %*% L);    SE_bc         # ~ 165
est_bc / SE_bc                                              # t ~ 3.6  (df = 95)
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) Per-dummy $t$-test AND the joint partial $F$ for the whole factor</summary>

**Per-dummy $t$-test** $H_0:\beta_k = 0$ vs $H_1:\beta_k\ne 0$ is **row 2 of g14a's universal test table** with $\theta_0 = 0$, $\bar X \to \hat\beta_k$, $s/\sqrt n \to \widehat{SE}(\hat\beta_k)$, df $= n-p-1 = 95$ — there is **nothing new** beyond `g15c` (b). Example: with $\hat\beta_M = 2\,000$ and $\widehat{SE} \approx 130$, $t_M = 2\,000/130 \approx 15.4$ on 95 df, $p < 10^{-26}$: the male-vs-female gap is overwhelmingly significant.

But "**does the whole factor `course` matter?**" is *not* answered by looking at the two course-dummy $t$-stats one by one — each is **marginal** given the other dummy, and one dummy can be non-significant while the variable jointly is. The right test is the **joint partial (incremental) $F$**, comparing $\mathcal M_1$ (full, $p_1 = 4$ regressors) to the reduced $\mathcal M_0$ that drops both course dummies ($p_0 = 2$):

$$H_0:\; \beta_b = \beta_c = 0 \quad\text{vs.}\quad H_1:\; \beta_b \neq 0\;\text{or}\;\beta_c\neq 0,$$

$$\boxed{\;\;F \;=\; \frac{(RSS_0 - RSS_1)/(p_1 - p_0)}{RSS_1/(n - p_1 - 1)} \;\sim\; F_{\,p_1-p_0,\; n-p_1-1}\quad\text{under }H_0.\;\;}$$

The numerator df is $K-1 = 2$ (the number of dummies dropped); the denominator df $= n - p_1 - 1 = 95$. With $RSS_1 = 36.5\times 10^6$ and $RSS_0 = 41.8\times 10^6$,

$$F_\text{obs} \;=\; \frac{(41.8 - 36.5)\times 10^6 / 2}{36.5\times 10^6 / 95} \;=\; \frac{2.65\times 10^6}{384\,210} \;\approx\; 6.90.$$

Critical value $F_{2,\,95;\,0.95} \approx 3.094$, $p$-value $\approx 0.0016$ — **reject** $H_0$ at $\alpha=0.05$: `course` is **jointly significant**, even though $\hat\beta_c = -150$ alone would look unremarkable. *This is the categorical specialisation of row 5 of g15a's master table* — same $F$ statistic, applied to a *block* of regressors instead of the whole model.

```r
fit1 <- lm(Salary ~ grade + sex + course, data = GS)        # full   M1, p1 = 4
fit0 <- lm(Salary ~ grade + sex,          data = GS)        # reduced M0, p0 = 2
anova(fit0, fit1)                                           # partial F: df1 = 2, df2 = 95, F = 6.90

# Manual cross-check
n  <- 100;  p1 <- 4;  p0 <- 2
RSS1 <- 36.5e6;  RSS0 <- 41.8e6
F_stat <- ((RSS0 - RSS1)/(p1 - p0)) / (RSS1/(n - p1 - 1));  F_stat   # 6.90
1 - pf(F_stat, df1 = p1 - p0, df2 = n - p1 - 1)                       # ~ 0.0016
```

**Rule of thumb.** Use the per-dummy $t$ to test **one specific level vs the reference**; use the joint partial $F$ to test **"the whole factor matters"** (i.e. drop the variable as a block). Cross-reference: g14a row 2 for the $t$, g15c subpart (d) for the global $F$ — the partial $F$ here is the same statistic, applied to a sub-block instead of all regressors.

</details>

---

<details class="master-subpart">
<summary>(c) Reference-level choice does not affect predictions or the global $F$</summary>

The reference level is **arbitrary**: re-coding is a pure re-parameterisation. Fitted values $\hat y_i$, residuals, $R^2$, residual SE $\hat\sigma$, and the global $F$ statistic are **invariant**; only the *labels* and *meanings* of individual $\hat\beta_k$ change. Re-levelling `course` to baseline $c$ gives

$$\hat\beta_0^{\text{new}} \;=\; \hat\beta_0 + \hat\beta_c \;=\; 1\,400 - 150 \;=\; 1\,250,\qquad \hat\beta_a^{\text{new}} \;=\; -\hat\beta_c \;=\; +150,\qquad \hat\beta_b^{\text{new}} \;=\; \hat\beta_b - \hat\beta_c \;=\; +600.$$

Same fit, different storytelling. *Practical rule.* Pick the baseline that makes the contrasts of interest read directly off `summary()` — usually the **control / largest / most stable** group.

```r
GS$course <- relevel(GS$course, ref = "c")                  # change baseline to c
fit1c     <- lm(Salary ~ grade + sex + course, data = GS)
all.equal(fitted(fit1), fitted(fit1c))                      # TRUE -- predictions unchanged
summary(fit1c)$fstatistic                                   # same global F as fit1
```

</details>

---

<details class="master-subpart">
<summary>(d) Row 9 — Interactions (slope-by-group)</summary>

So far $\mathcal M_1$ assumes the marginal effect of `grade` on `Salary` is the **same** for women and men: $\partial \mathbb E[\text{Salary}]/\partial grade = \beta_g$ regardless of `sex`. To let the slope **differ by sex** we add the **interaction column** $D^{sex}_M\cdot grade$ to $X$:

$$\mathcal M_2:\;\; \text{Salary} \;=\; \beta_0 \;+\; \beta_g\,grade \;+\; \beta_M\,D^{sex}_M \;+\; \beta_b\,D^{course}_b \;+\; \beta_c\,D^{course}_c \;+\; \gamma\,(D^{sex}_M\cdot grade) \;+\; \varepsilon.$$

**Derivation — slope by group (continuous $\times$ dummy).** Fix `course` (so $D_b, D_c$ are constants). For a **woman** ($D^{sex}_M = 0$):

$$\widehat{\text{Salary}}_F \;=\; \hat\beta_0 + \hat\beta_g\,grade + \hat\beta_b\,D_b + \hat\beta_c\,D_c\quad\Rightarrow\quad \frac{\partial\,\widehat{\text{Salary}}_F}{\partial grade} \;=\; \hat\beta_g.$$

For a **man** ($D^{sex}_M = 1$):

$$\widehat{\text{Salary}}_M \;=\; (\hat\beta_0 + \hat\beta_M) + (\hat\beta_g + \hat\gamma)\,grade + \hat\beta_b\,D_b + \hat\beta_c\,D_c\quad\Rightarrow\quad \frac{\partial\,\widehat{\text{Salary}}_M}{\partial grade} \;=\; \hat\beta_g + \hat\gamma.$$

So the women's slope is $\hat\beta_g$, the men's slope is $\hat\beta_g + \hat\gamma$, and $\hat\gamma$ is the **slope shift** when moving from the reference group to level $M$. Suppose $\hat\beta_g = 30$ and $\hat\gamma = 12$ on the GS fit (so $\hat\beta_M$ also re-adjusts to e.g. $1\,150$): women earn $+30$ € per extra grade point, men earn $+42$ € per extra grade point — **the gender gap is no longer a constant $\beta_M$, it widens with grade**.

**Two-continuous interaction** (no dummy involved) follows the same derivation with $X_j\cdot X_k$ replacing $X_j\cdot D$: the slope of $X_j$ becomes $\hat\beta_j + \hat\beta_{jk}\cdot X_k$ — a **linear function of $X_k$**. Same row of the master table, same $t$-test.

**Testing the interaction.** $H_0:\gamma = 0$ vs $H_1:\gamma\ne 0$ is **row 2 of g14a** with $\theta_0 = 0$: read off the $t$-stat and $p$-value for the `grade:sexM` row of `summary(fit2)`. With a single interaction column the partial $F$ in `anova(fit1, fit2)` (df $= 1, n-p-1$) gives an *identical* verdict ($F = t^2$). If not rejected, *drop* the interaction — back to $\mathcal M_1$.

```r
fit2 <- lm(Salary ~ grade + sex + course + sex:grade, data = GS)
# Shortcut: Salary ~ (grade + sex)^2 + course  expands to the same model
summary(fit2)                                                # t on grade:sexM tests H0: gamma = 0
anova(fit1, fit2)                                            # equivalent partial F, df1 = 1

# Per-sex slopes, course fixed
b_g  <- coef(fit2)["grade"];          b_g                    # 30 -> women's slope
gam  <- coef(fit2)["grade:sexM"];     b_g + gam              # 42 -> men's slope
```

**Centring tip.** If $\hat\beta_M$ is hard to read because $grade = 0$ is far outside the data, **centre** `grade` (subtract its mean) before fitting: the main effect of `sex` then reads off as the gender gap *at the average grade*, not at $grade = 0$ — much more interpretable.

</details>

---

<details class="master-subpart">
<summary>(e) Parallel-vs-non-parallel lines — the mental picture</summary>

In the $(grade,\,\widehat{\text{Salary}})$ plane (fixing `course` at the baseline $a$ for clarity), $\mathcal M_1$ and $\mathcal M_2$ trace one **line per sex**:

| Model | Women: intercept | Women: slope | Men: intercept | Men: slope | Geometry |
|---|---|---|---|---|---|
| **$\mathcal M_1$** (no interaction) | $\beta_0 = 1\,400$ | $\beta_g = 35$ | $\beta_0 + \beta_M = 3\,400$ | $\beta_g = 35$ | **parallel** — same slope, constant vertical gap $\beta_M$ |
| **$\mathcal M_2$** (with interaction) | $\beta_0 = 1\,400$ | $\beta_g = 30$ | $\beta_0 + \beta_M = 2\,550$ | $\beta_g + \gamma = 42$ | **non-parallel** — slopes differ, lines may cross |

So **the interaction $\gamma$ *is* the lack of parallelism** between the two lines (or, in general, between the $K$ lines indexed by a $K$-level factor). The $t$-test on $\hat\gamma$ is literally *"are the two lines parallel?"*. The same picture generalises: in `g15e`'s residuals-vs-fitted diagnostic, a *fan* across groups is the visual signature of an unmodelled interaction.

</details>

---

<details class="master-subpart">
<summary>(f) Cross-references</summary>

- **`g15a`** — universal 7-step recipe and the 9-row master case table; this entry walks **rows 8 and 9**.
- **`g15c`** — matrix-form OLS. Dummies and interactions *slot directly into $X$* as ordinary columns; no formula in g15c changes, only the column structure of $X$.
- **`g15b`** — prediction CI/PI at $x_0$ with a categorical predictor. The "$x_0$" vector now includes the dummy values for the level you condition on; the leverage $h_{00} = x_0^\top(X^\top X)^{-1}x_0$ uses the same enlarged $X$. The PI is still wider than the CI by the same "$+1$" inside the sqrt.
- **`g15e`** — diagnostics. Adding an interaction does **not** change the L/I/N/E checks themselves; but a missing interaction can show up in `residuals vs fitted` as a **fan across groups** — a signal that the parallel-lines assumption is wrong.
- **`g14a`** — row 2 (one-sample $t$): the per-dummy and per-interaction $t$-tests are *exactly* this row with $\theta_0 = 0$, $\bar X \to \hat\beta_k$, $s/\sqrt n \to \widehat{SE}(\hat\beta_k)$. The joint partial $F$ in (b) is row 5 of g15a's master table applied to a *block* of regressors.

**Summary — the GS model family.**

| Quantity | $\mathcal M_0$ (no course) | $\mathcal M_1$ (full additive) | $\mathcal M_2$ (with $sex\times grade$) |
|---|---|---|---|
| Regressors (excl. intercept) | 2 | 4 | 5 |
| Residual df | $97$ | $95$ | $94$ |
| Course block | — | 2 dummies — joint $F_{2,95} = 6.90$, $p \approx 0.0016$ | 2 dummies |
| Sex effect | constant shift $\beta_M$ | constant shift $\beta_M = 2\,000$ | shift $\beta_M$ **+** slope shift $\gamma$ |
| Lines in $(grade, \text{Salary})$ by sex (at $course=a$) | one per sex, **parallel** | one per sex, **parallel** | one per sex, **non-parallel** |
| Test for "course matters" | n/a | partial $F_{2,95}$ on dropping $\{b,c\}$ | partial $F_{2,94}$ on dropping $\{b,c\}$ |
| Test for "slope differs by sex" | n/a | n/a | $t$ on $\hat\gamma$ $\equiv$ partial $F_{1,94}$ |

</details>
""",
    "images": ["statistics/images/master/master_g15d_ai.png"],
}

# =====================================================================
# g15e_diagnostics --- Residual diagnostics & multicollinearity
# Consolidates: ex8.4a (Restaurants surface + diagnostics)
# Dataset: Restaurants (revenues ~ surface), n = 50
# =====================================================================
master_exercises["g15e_diagnostics"] = {
    "title": "Master Exam --- Residual diagnostics & multicollinearity (LINE + influence + VIF) on Restaurants ($n=50$) and the GS dataset",
    "content": r"""## Setup --- running datasets (re-used from g15a and g15d --- not re-derived here)

**Simple regression (Restaurants, from Ex 8.4a).** $n=50$ restaurants with $X=\text{surface}$ ($m^2$ of dining area) and $Y=\text{revenues}$ (weekly revenues, kEUR). The OLS fit (`g15a` recipe) gives
$$\widehat{\text{revenues}} \;=\; 246.812 \;+\; 0.4049\,\text{surface},\qquad \widehat\sigma_\varepsilon \approx 41.7,\qquad R^2\approx 0.12,\qquad p\,(\text{slope}) \approx 0.$$
This is the model whose residual cone, $\log Y$ remedy and `evening_only`-split residual boxplot are explored in `Ex 8.4a`.

**Multi-regression (GS, from `g15d`).** $n=100$ junior employees, $\mathcal M_1:\;\text{Salary}\sim\text{grade}+\text{sex}+\text{course}$. We will use it in part (f) to compute VIFs, after the optional addition of a near-collinear regressor `experience` whose construction is given there.

```r
# Restaurants --- simple regression
mod  <- lm(revenues ~ surface, data = restaurants)
e    <- residuals(mod);   yhat <- fitted(mod)
r_std  <- rstandard(mod); r_stud <- rstudent(mod)
n    <- nobs(mod);  p <- length(coef(mod)) - 1     # p = 1 predictor, df = n - p - 1 = 48
```

The OLS estimator, $\widehat{SE}$ formula, $t$-test, $F$-test and CIs/PIs *do not* change between this entry and `g15a`/`g15b`/`g15c`/`g15d`. **What this entry adds is the certificate of validity for those formulas.**

---

<details class="master-subpart" open>
<summary>(a) <strong>L --- Linearity:</strong> Residuals-vs-Fitted plot (<code>plot(mod, which = 1)</code>)</summary>

**What it shows.** On the horizontal axis: the fitted values $\hat y_i$. On the vertical axis: the raw residuals $e_i = y_i - \hat y_i$. A red LOWESS smoother is overlaid. Under correct linear specification, $e_i$ should look like noise around 0 with **no trend in the mean** --- the LOWESS line should hug the zero line.

**What "good" looks like.** A formless cloud of points, evenly scattered above and below 0, LOWESS flat at $e=0$. No curvature, no fan, no clusters.

**What "bad" looks like and what it diagnoses.**

| Pattern in $e$ vs $\hat y$ | Diagnosis | Standard remedy |
|---|---|---|
| Random cloud, constant spread | OK | none |
| **Curvature** (U / inverted-U / S in the LOWESS) | **L violated** --- missing nonlinear term | add $X^2$, polynomial, spline, or interaction; transform $X$ |
| Spread $\uparrow$ with $\hat y$ (cone, funnel) | E violated --- *heteroscedasticity* | see part (d) |
| Trend in mean (LOWESS line away from 0) | shouldn't happen for OLS with intercept | check code |

**Why it licenses every $\hat\beta$.** If the LOWESS bends, the true conditional mean is not linear in the current $X$, the OLS line is missing a feature, and $\hat{\boldsymbol\beta}$ no longer estimates the *true* slope of the conditional mean --- it estimates a *best linear approximation* that may carry a non-trivial bias. All subsequent inference in `g15a`–`g15d` (slope $t$-tests, CIs, predictions) is then targeted at the wrong quantity.

**Worked reading on Restaurants.** Running `plot(mod, which = 1)`:

- The LOWESS dips slightly above zero in the middle of the $\hat y$ range and bends down at the right end --- mild evidence of **missing curvature** in `surface` (revenues likely saturate at very large dining rooms).
- *Plus* the dispersion of $e_i$ visibly **widens** going right: at $\hat y \approx 280$ residuals are $\pm 30$, at $\hat y \approx 540$ they span $\pm 100$ --- this is the **cone** that part (d) will diagnose as heteroscedasticity.

Conclusion for L: **mild violation**; consider adding $\text{surface}^2$ (or $\log$-transform of $Y$, which jointly fixes E as well).

```r
plot(mod, which = 1)                            # Residuals vs Fitted (with LOWESS)
# Optional manual version with each predictor:
plot(restaurants$surface, e, pch = 19, col = "steelblue",
     xlab = "surface (m^2)", ylab = "Residuals e_i")
abline(h = 0, lty = 2); lines(lowess(restaurants$surface, e),
                              col = "firebrick", lwd = 2)
```

</details>

---

<details class="master-subpart">
<summary>(b) <strong>I --- Independence:</strong> when to worry, how to check</summary>

**Assumption.** $\varepsilon_1, \varepsilon_2, \dots, \varepsilon_n$ are mutually independent. The course's default cross-sectional setting (50 different restaurants, 100 different employees) usually satisfies this *by sampling design* --- the worry is real only when there is a natural ordering or grouping in the data.

**When to worry.**

- **Time series**: $\varepsilon_t$ for the *same* unit observed across $t=1,\dots,T$ tends to be serially correlated.
- **Spatial / clustered** data: schools within districts, customers within stores, employees within firms.
- **Repeated measures**: several observations per unit.

**How to check.** For time series, plot $e_t$ vs $t$ (look for runs of same-sign residuals) and compute the **Durbin–Watson** statistic
$$\mathrm{DW} \;=\; \frac{\sum_{t=2}^{n}(e_t-e_{t-1})^2}{\sum_{t=1}^{n}e_t^2}\;\in\;[0,4],\qquad \mathrm{DW}\approx 2 \;\Leftrightarrow\; \text{no first-order autocorrelation}.$$
$\mathrm{DW} \ll 2$ flags positive autocorrelation; $\mathrm{DW} \gg 2$ negative. For clustered designs, plot residuals split by cluster (sign of within-cluster correlation).

**What breaks if violated.** $\widehat{\Var}(\hat{\boldsymbol\beta}) = \hat\sigma^2(X^\top X)^{-1}$ assumes diagonal $\Var(\boldsymbol\varepsilon) = \sigma^2 I$. If errors are correlated, the true variance has off-diagonal terms, $\widehat{SE}$'s are **wrong** (usually too small), and $t$-statistics inflate spuriously --- you reject too often.

**Remedies.** HAC ("Newey–West") robust SEs for time series; cluster-robust SEs for clustered data; mixed-effects / GLS models when correlation structure is rich.

For Restaurants (a cross-section of 50 distinct restaurants) and GS (a cross-section of 100 distinct employees), independence is *plausible by design* and no DW test is needed.

```r
library(lmtest)
dwtest(mod)                                     # H0: no autocorrelation (DW ~ 2)
acf(residuals(mod), main = "Residual ACF")       # for time-ordered data
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (c) <strong>N --- Normality of residuals:</strong> histogram + Q-Q plot (<code>plot(mod, which = 2)</code>)</summary>

**Assumption.** $\varepsilon_i \overset{\rm iid}{\sim} \mathcal N(0,\sigma^2)$. This is needed for the *exact* small-sample distribution of every inferential procedure: $t_{n-p-1}$ for the slope $t$-test (`g15a`, `g15c`), $F_{p,n-p-1}$ for the global $F$-test (`g15c`), the CI for the mean response and the PI for an individual at $x_0$ (`g15b`).

**What "good" looks like.**

1. **Histogram** of $r_i^{\text{std}}$: roughly bell-shaped, symmetric, centred at 0, with about $95\%$ of values within $[-2,2]$.
2. **Normal Q-Q plot**: the sorted $r_i^{\text{std}}$ against $\Phi^{-1}((i-0.5)/n)$ lie on the 45° reference line.

**What "bad" looks like.**

| Q-Q pattern | Diagnosis | What it suggests |
|---|---|---|
| Straight diagonal | $\mathcal N$ --- OK | --- |
| **S-shape** (both tails bend away from the line) | **Heavy tails** | outliers; consider robust regression |
| **Inverted S** | Light tails | usually benign |
| **Concave up** (right tail above line) | **Right-skewed residuals** | $\log Y$ or $\sqrt Y$ |
| **Concave down** (left tail below line) | Left-skewed residuals | reflect / transform |

**Formal supplement.** The **Shapiro–Wilk** test, $H_0:\;\varepsilon\sim\mathcal N$, $H_1:\;$ not Normal. Reject $H_0$ if $p<\alpha$. (Watch out: for large $n$ the test rejects on trivial deviations.)

**When CLT rescues you (and when it doesn't).** For large $n$, the sampling distribution of $\hat\beta_j$ is approximately Normal *regardless* of $\varepsilon_i$'s distribution, by the CLT applied to the linear estimator $\hat\beta_j = \sum_i w_i Y_i$. So $t$-tests and CIs on $\hat\beta_j$ are *asymptotically* valid even without N. **But prediction intervals for an individual $Y_0$** carry the residual term $\varepsilon_0$ itself --- *no* averaging --- and remain sensitive to Normality at *any* sample size. If N fails and you need a PI, transform $Y$.

**Worked reading on Restaurants.** Running `plot(mod, which = 2)` and `hist(rstandard(mod))`:

- Histogram is mostly bell-shaped but with a heavier *right* tail (a handful of large positive residuals from big restaurants on very busy weeks).
- Q-Q plot is straight in the middle, with the upper few points lifting **above** the 45° line --- **concave-up** pattern, i.e. **right skew**.
- `shapiro.test(rstandard(mod))` returns $p\approx 0.02 < 0.05$ → reject $H_0$ at $\alpha=5\%$.

Conclusion for N: **mild right skew**. $\log Y$ would symmetrise the residuals (and simultaneously remove the cone of part (d)). With $n=50$ the CLT broadly rescues the slope $t$-test and its CI, *but* PIs for revenues at a specific surface would mis-cover and should be reported only after $\log$-transform.

```r
plot(mod, which = 2)                            # Q-Q plot with reference line
hist(rstandard(mod), breaks = 12, col = "steelblue", border = "white",
     main = "Standardised residuals", xlab = "r_std")
shapiro.test(residuals(mod))                     # H0: Normal; reject if p < alpha
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (d) <strong>E --- Equal variance (homoscedasticity):</strong> Scale–Location plot (<code>plot(mod, which = 3)</code>)</summary>

**Assumption.** $\Var(\varepsilon_i\mid X_i) = \sigma^2$ for every $i$. The error variance is a **single constant** --- it does not depend on the predictors, on $\hat y_i$, on the index $i$, on time, or on any subgroup. This is *the* assumption that makes the OLS standard-error formula $\widehat{\Var}(\hat{\boldsymbol\beta}) = \hat\sigma^2(X^\top X)^{-1}$ correct and OLS the best linear unbiased estimator (Gauss–Markov).

**Two visual checks.**

1. **Residuals-vs-Fitted plot** ($e$ vs $\hat y$ from part (a)): under homoscedasticity the *vertical spread* of points is constant across the horizontal range. A widening (or narrowing) cone signals heteroscedasticity.
2. **Scale–Location plot** $\sqrt{\lvert r^{\text{std}}_i\rvert}$ vs $\hat y_i$ (`plot(mod, which = 3)`): designed to make heteroscedasticity stand out as a slope in the LOWESS. Under homoscedasticity the red smoother is **flat**; if it rises (or falls) with $\hat y$, the spread depends on the level → reject E.

**Formal test --- Breusch–Pagan.** Regress $e_i^2$ on the predictors (or on $\hat y_i$) and apply an $LM$-test:
$$H_0:\;\Var(\varepsilon_i)=\sigma^2 \quad\text{vs}\quad H_1:\;\Var(\varepsilon_i) = h(\mathbf z_i^\top\boldsymbol\gamma).$$
`lmtest::bptest(mod)` returns a $p$-value; reject homoscedasticity if $p<\alpha$.

**What breaks if violated.** $\widehat{SE}(\hat\beta_j)$ is **wrong** (typically too small where variance is large). $t$-tests over-reject, CIs are too narrow, $F$-tests mis-size, PI widths are wrong (too narrow on the high-variance side, too wide on the low-variance side). **Point predictions $\hat y_0 = x_0^\top\hat{\boldsymbol\beta}$ remain OK** --- only the *uncertainty* statements break.

**Remedies.** $\log Y$ (or $\sqrt Y$) if $Y>0$ and variance grows multiplicatively with the mean; **WLS** with weights $1/\widehat{\Var}(\varepsilon_i)$; or keep OLS and replace SEs with **HC ("sandwich") robust SEs** (`sandwich::vcovHC`, `coeftest`).

**Worked reading on Restaurants.** The Restaurants $e$-vs-$\hat y$ shows a clear **funnel opening to the right**: residual spread at $\hat y\approx 280$ is $\pm 30$, at $\hat y\approx 540$ is $\pm 100$. The Scale–Location plot's red smoother rises steeply with $\hat y$: $\sqrt{\lvert r^{\text{std}}\rvert}$ at $\hat y=280$ is $\sim 0.5$, at $\hat y=540$ is $\sim 1.5$. `bptest(mod)` returns $p \approx 0.005 < 0.05$ → **reject homoscedasticity**.

Conclusion for E: **clear violation**. The OLS slope $\hat\beta_1=0.4049$ is still unbiased, but its textbook SE is wrong. Two fixes:

- **Variance-stabilising transform**: refit on $\log(\text{revenues})$ --- the funnel disappears and the slope acquires a clean semi-elasticity interpretation.
- **HC robust SE**: `coeftest(mod, vcov = vcovHC(mod, type = "HC3"))` --- coefficients unchanged, SEs corrected.

```r
plot(mod, which = 3)                            # Scale-Location (sqrt|r_std| vs y_hat)

library(lmtest); library(sandwich)
bptest(mod)                                     # Breusch-Pagan; H0: homoscedasticity
coeftest(mod, vcov = vcovHC(mod, type = "HC3")) # HC3 robust SE replacement

# Variance-stabilising transform
modL <- lm(log(revenues) ~ surface, data = restaurants)
plot(modL, which = 1); plot(modL, which = 3)    # funnel should be gone
bptest(modL)                                    # p > 0.05 now
```

</details>

---

<details class="master-subpart">
<summary>(e) <strong>Influence --- leverage, studentised residuals, Cook's distance</strong> (<code>plot(mod, which = 5)</code>)</summary>

A point can be "unusual" in **three logically distinct** ways:

| Concept | Measures | Statistic | Flag |
|---|---|---|---|
| **Outlier** | Large *vertical* residual ($y_i$ far from the fitted line) | $\lvert r_i^{\text{std}}\rvert$ or $\lvert r_i^{\text{stud}}\rvert$ | $>2$ inspect, $>3$ outlier |
| **Leverage** | Far in *predictor* space ($x_i$ far from $\bar x$) | $h_{ii} = [H]_{ii}$, $\sum_i h_{ii} = p+1$ | $h_{ii} > 2(p+1)/n$ |
| **Influence** | Removing it *changes* the fit | Cook's $D_i$ | $D_i > 4/n$; $D_i > 1$ severe |

**Hat-matrix derivation of leverage.** The fitted vector is $\hat y = X(X^\top X)^{-1}X^\top y = Hy$, so $\hat y_i = \sum_j h_{ij} y_j$ with $h_{ij} = [H]_{ij}$. The diagonal $h_{ii}$ measures how much observation $i$ "pulls" its own fit: $\partial\hat y_i / \partial y_i = h_{ii}$. The trace identity $\sum_i h_{ii} = \mathrm{tr}(H) = p+1$ gives the rule of thumb: a typical $h_{ii} \approx (p+1)/n$; flag $h_{ii} > 2(p+1)/n$.

**Studentised residual.** Raw residuals do *not* share a common variance: $\Var(e_i) = \sigma^2(1-h_{ii})$, so high-leverage points have mechanically *smaller* residual spread. To compare across $i$ we rescale:
$$r_i^{\text{std}} \;=\; \frac{e_i}{\widehat\sigma_\varepsilon\sqrt{1-h_{ii}}}\;\approx\;\mathcal N(0,1),\qquad r_i^{\text{stud}} \;=\; \frac{e_i}{\widehat\sigma_{\varepsilon,(i)}\sqrt{1-h_{ii}}}\;\sim\; t_{n-p-2}.$$
(The "studentised" or "externally studentised" version replaces $\widehat\sigma_\varepsilon$ by the SD estimate from the fit *without* observation $i$.)

**Cook's distance** combines the two ingredients into a single scalar --- "how far does $\hat{\boldsymbol\beta}$ move if I delete observation $i$":
$$\boxed{\;\;D_i \;=\; \frac{(r_i^{\text{std}})^2}{p+1}\cdot\frac{h_{ii}}{1-h_{ii}}.\;\;}$$
$D_i$ is large *only* when **both** factors are big --- a high-leverage point with a tiny residual sits on the regression line and is not influential; a big residual at average $x$ has $h_{ii} \approx 1/n$ and is again not influential.

**Worked reading on Restaurants.** $n=50$, $p=1$ → leverage flag $h_{ii} > 2\cdot 2/50 = 0.08$; Cook flag $D_i > 4/50 = 0.08$. The two biggest restaurants (`surface` $\approx 280\,m^2$) sit at $h_{ii}\approx 0.15$ (above the leverage flag) but their residuals are moderate, so
$$D_i \;\approx\; \frac{(0.7)^2}{2}\cdot\frac{0.15}{0.85}\;\approx\; 0.04 \;<\; 0.08,$$
i.e. **leverage points but not influential**. No deletion needed; the OLS slope is stable. If, hypothetically, one of those points also had $r^{\text{std}}\approx 3$, then $D_i\approx 0.8$ → clearly influential, and we would refit without it and report both fits.

```r
# Numeric flags
h        <- hatvalues(mod)
D        <- cooks.distance(mod)
flag     <- data.frame(i = seq_len(n),
                       r_std = r_std, h = h, D = D,
                       lev_flag = h > 2 * (p + 1) / n,
                       inf_flag = D > 4 / n)
flag[flag$lev_flag | flag$inf_flag | abs(flag$r_std) > 2, ]

# Canonical visual: Residuals vs Leverage with Cook's contours
plot(mod, which = 5)                            # |r_std| vs h_ii, Cook contours

# 4-panel diagnostic page
par(mfrow = c(2, 2)); plot(mod); par(mfrow = c(1, 1))
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (f) <strong>Multicollinearity --- VIF</strong> (multi-regression only; <code>library(car); vif(mod)</code>)</summary>

**The problem.** In a multi-regression $Y \sim X_1 + \dots + X_p$, the SE of $\hat\beta_j$ is
$$\boxed{\;\;\widehat{\Var}(\hat\beta_j) \;=\; \frac{\hat\sigma^2}{(n-1)\,s^2_{X_j}}\cdot\underbrace{\frac{1}{1-R_j^2}}_{\mathrm{VIF}_j},\qquad R_j^2 \;=\; R^2 \text{ of } X_j \sim X_{-j}.\;\;}$$
The **variance-inflation factor**
$$\mathrm{VIF}_j \;=\; \frac{1}{1-R_j^2}$$
measures by what factor $\Var(\hat\beta_j)$ has been blown up by collinearity with the other predictors, relative to the idealised orthogonal case ($R_j^2=0$ → VIF $=1$). $R_j^2$ comes from regressing $X_j$ on **all other predictors**.

**Rules of thumb.**

| VIF | Reading | Action |
|---|---|---|
| $\le 1$ | no collinearity | none |
| $1$–$5$ | mild | none (some sources flag $>5$ as concerning) |
| $> 10$ | **severe multicollinearity** | drop / combine / ridge |

**Consequence on inference.** When $\mathrm{VIF}_j$ is large, $\widehat{SE}(\hat\beta_j)$ inflates → the $t$-statistic $\hat\beta_j/\widehat{SE}(\hat\beta_j)$ shrinks → individual $p$-value rises → **a predictor that genuinely matters can appear insignificant** purely because its information is shared with another regressor. Classical signature: huge SEs, individual $t$-tests insignificant, *but* the joint $F$-test highly significant and $R^2$ large. Predictions $\hat y_0$ stay *fine* (the joint span of $X$ is unchanged) --- only the *individual* coefficient inference suffers.

**Remedies.** (i) Drop the redundant regressor whose substantive priority is lowest; (ii) combine them into a single index (PCA, sum, average); (iii) keep both and use **ridge regression** $\hat{\boldsymbol\beta}_\text{ridge} = (X^\top X + \lambda I)^{-1}X^\top y$ (beyond this course).

**Worked example on GS (extension of `g15d`).** Suppose to the model $\mathcal M_1:\;\text{Salary}\sim\text{grade}+\text{sex}+\text{course}$ we add `experience` (years on the labour market), strongly correlated with `grade` ($r\approx 0.92$ — older employees both have more experience and higher performance scores in this firm). Then
$$R_{\text{grade}}^2 \;\approx\; R_{\text{experience}}^2 \;\approx\; 0.85,\qquad \mathrm{VIF} \;\approx\; \frac{1}{1-0.85} \;\approx\; 6.7,$$
flagged as concerning. `summary()` shows $\widehat{SE}(\hat\beta_{\text{grade}})$ doubled vs $\mathcal M_1$, the individual $t$-test on `grade` drops to $p\approx 0.10$, *but* the joint $F$ on the pair `(grade, experience)` is still strongly significant. Remedy: drop one of the two, or fit ridge.

**Worked example on Restaurants (extension).** The `restaurants` data also carries `seats`, with $r(\text{surface}, \text{seats}) \approx 0.93$. In the model $\text{revenues}\sim\text{surface}+\text{seats}+\text{evening\_only}$:
$$\mathrm{VIF}_{\text{surface}}, \mathrm{VIF}_{\text{seats}} \;\approx\; 7.7,\qquad \mathrm{VIF}_{\text{evening\_only}} \;\approx\; 1.1.$$
Drop `seats` (highly redundant with `surface`) and all VIFs return to $\approx 1$.

```r
modM <- lm(revenues ~ surface + seats + evening_only, data = restaurants)
library(car)
vif(modM)                                       # one number per regressor
1 / (1 - summary(lm(surface ~ seats + evening_only,
                    data = restaurants))$r.squared)  # manual VIF check
cor(restaurants[, c("surface", "seats", "evening_only")])

# Remedy: drop the redundant regressor
modM2 <- lm(revenues ~ surface + evening_only, data = restaurants)
vif(modM2)                                       # all ~ 1 now
```

</details>

---

<details class="master-subpart">
<summary>(g) <strong>Cross-references</strong> --- where each piece is consumed across G15</summary>

This entry is **horizontal infrastructure**: every other G15 entry relies on the certificates of validity produced here. The pointer block:

| Diagnostic | Licenses which G15 row / formula | Where the formula lives |
|---|---|---|
| **L (linearity)** | $\hat{\boldsymbol\beta} = (X^\top X)^{-1}X^\top y$ targeting the true conditional mean (rows 1–9 of master case table) | `g15a` for $p=1$; `g15c` for $p\ge 2$ |
| **I (independence)** | $\widehat{\Var}(\hat{\boldsymbol\beta}) = \hat\sigma^2(X^\top X)^{-1}$ → every $\widehat{SE}(\hat\beta_j)$, every CI, every $t$- / $F$-test | `g15a` (slope SE), `g15c` (matrix SE), `g14a` (universal test table) |
| **N (normality)** | Exact $t_{n-p-1}$ for slope tests, $F_{p,n-p-1}$ for global $F$, exact CI for mean response and PI for individual at $x_0$ | `g15a` (slope $t$), `g15c` (global $F$), `g15b` (PI / CI at $x_0$) |
| **E (homoscedasticity)** | Gauss–Markov optimality + correct $\widehat{SE}$ → all CI/test rows above | `g15a` (slope SE), `g15c` (matrix SE) |
| **Influence (leverage, Cook's $D$)** | Sensitivity of $\hat{\boldsymbol\beta}$ to single observations; sensitivity of predictions at high-leverage $x_0$ | `g15b` (PI width $\propto \sqrt{1 + h_{00}}$) |
| **Multicollinearity (VIF)** | Inflation of $\widehat{SE}(\hat\beta_j)$ → individual $t$-test loses power even when joint $F$ is significant | `g15c` (multi-regression), `g15d` (dummies + interactions) |

**Other cross-references.**

- **`g15b` (prediction at $x_0$).** PI for an individual at $x_0$ requires N (the $\varepsilon_0$ term itself is Normal) **at any sample size** --- the CLT does *not* rescue it. PI width is also inflated by $h_{00}$, i.e. the leverage of $x_0$ in the original design --- "**extrapolation = high $h_{00}$**" is the same diagnostic family as part (e).
- **`g15a` (universal recipe).** This entry is exactly **step 7** of the 7-step recipe at the top of `g15a`; the diagnostic checklist above is the operational version of step 7.
- **`g15c` (multi-regression).** VIF (part (f)) is the *only* diagnostic that has no analogue in simple regression --- it applies as soon as $p\ge 2$.
- **`g15d` (categorical predictors / interactions).** Dummy regressors do not change LINE: residuals-vs-fitted, Q-Q and Cook's $D$ are read identically. VIF on dummies of the same factor is naturally elevated (mutual exclusion) --- this is a *structural* not pathological VIF; use the `car::vif(mod, type = "predictor")` aggregated version, or just look at GVIFs.
- **`g14a` / `g13a` (inferential procedures).** Every $t$-test and CI in those entries inherits its validity from N + I + E here; without those, the rejection regions and the coverage probabilities are *not* what they claim to be.

</details>

---

### Summary diagnostic checklist (memorise once, run on every fitted model)

| Step | Plot / Statistic | What "good" looks like | If bad → |
|---|---|---|---|
| 1 | $e$ vs $\hat y$ (`which = 1`) | random cloud, LOWESS flat | L: add poly / interaction; E: see step 4 |
| 2 | Q-Q of $r^{\text{std}}$ (`which = 2`) | 45° line | N: $\log Y$ / Box–Cox; rely on CLT for tests/CI if $n$ large; *never* for PI |
| 3 | DW or $e_t$ vs $t$ | DW $\approx 2$, no runs | I: HAC / cluster-robust SE |
| 4 | Scale–Location (`which = 3`); BP test | flat smoother; $p>\alpha$ | E: $\log Y$ / WLS / HC robust SE |
| 5 | Residuals vs Leverage (`which = 5`); $h_{ii}$, $D_i$ | $h_{ii} < 2(p+1)/n$, $D_i < 4/n$ | Influence: refit without and report sensitivity |
| 6 | `vif(mod)` (multi only) | $<5$ | VIF: drop / combine / ridge |

---

**Linked snippet:** Ex 8.4a (Restaurants: revenues ~ surface, $n=50$, $\hat\beta_0=246.81$, $\hat\beta_1=0.4049$ kEUR/$m^2$, $R^2\approx 0.12$ --- the dataset whose residual plot exhibits the funnel that motivates this entire master).

![Master G15e --- residuals vs fitted (cone), Q-Q, Cook's distance, VIF](statistics/images/master/master_g15e_ai.png)
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

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) The Uniform-on-interval assumption</summary>

The only statement the grouped table makes is: **a fraction $f_j$ of the data lies in $[L_j,R_j)$.** To turn this into a function $\widehat F(x)$ at every $x$, we need an assumption about **how** those $n_j$ points are spread *inside* the class. The standard, minimal-information choice is:

$$\boxed{\;X\mid X\in[L_j,R_j)\;\sim\;\mathrm{Uniform}[L_j,R_j)\;}$$

i.e. inside each class the points are evenly spread. Equivalently, the **density** on class $j$ is constant:
$$\widehat f(x)\;=\;\frac{f_j}{w_j}\qquad\text{for }x\in[L_j,R_j).$$

This is the **histogram density** — the height of the histogram bar drawn so that *area = relative frequency*. Two consequences:
1. The grouped CDF $\widehat F$ is **piecewise linear**: flat-slope ramps inside each class, joined at the class boundaries.
2. At a boundary $R_j$, $\widehat F(R_j)=F_j$ (the cumulative entry from the table); inside the class we **linearly interpolate**.

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) Linear-interpolation formula for $\widehat F(x)$, $x\in[L_j,R_j)$</summary>

The fraction of class $j$'s width that lies to the left of $x$ is $(x-L_j)/w_j$. Under Uniform-on-interval, the same fraction of class $j$'s mass $f_j$ lies to the left of $x$:
$$\widehat F(x)\;=\;F_{j-1}\;+\;\frac{x-L_j}{w_j}\,f_j\qquad\text{(linear ramp inside class }j\text{)}.$$
Equivalently in **density-times-overlap** form,
$$\widehat F(x)\;=\;F_{j-1}\;+\;\widehat f(x)\cdot(x-L_j)\;=\;F_{j-1}\;+\;\frac{f_j}{w_j}\,(x-L_j),$$
which is the **same number** — just two different bookkeeping views of one straight-line interpolation.

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-4plus">≥4 ex</span> (c) Worked example: $\widehat{\mathbb P}(X\le 15)$</summary>

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

</details>

---

<details class="master-subpart">
<summary>(d) Two boundary checks (always do these)</summary>

1. **At $x=L_j$** (left edge of class $j$): the formula gives $\widehat F(L_j)=F_{j-1}+0\cdot f_j=F_{j-1}$ — matches the cumulative entry just before class $j$.
2. **At $x=R_j$** (right edge): $\widehat F(R_j)=F_{j-1}+1\cdot f_j=F_j$ — matches the cumulative entry at the end of class $j$.

So $\widehat F$ is **continuous and increasing**, made of straight segments whose slopes are the histogram densities $f_j/w_j$. In our table the slopes are $\{0.010,\,0.020,\,0.010,\,0.0033\}$ per second — call density is highest in class 2 (steepest ramp).

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (e) Quantile (inverse) direction — same machinery in reverse</summary>

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

</details>

---

<details class="master-subpart">
<summary>(f) When is Uniform-on-interval reasonable?</summary>

- **Reasonable** when class widths are small *relative to the variation of the true density* — within $[10,30)$ a true unimodal density is approximately flat over 20 seconds.
- **Suspect** in the *first* and *last* class when widths are large or the distribution is heavy-tailed: e.g. $[60,120)$ is 60 s wide; if calls really follow a decreasing density there, Uniform-on-interval **overestimates** $F(x)$ at $x$ just past 60 and **underestimates** near 120. This is the **grouping bias** of all summary statistics computed from the table (mean-from-midpoints, variance, etc.).
- **Diagnostic.** If you also have raw data on a subset, overlay the piecewise-linear $\widehat F$ on the empirical CDF; large deviations inside wide classes warn you to either keep classes narrow at registration time or use a smoother model (e.g., midpoint-based spline, or a parametric fit such as $\mathrm{Exp}(\lambda)$ matched to the grouped mean).

</details>
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

</details>

---

<details class="master-subpart">
<summary>(b) Why per-100k (a "rate") rather than the raw ratio?</summary>

The ratio $\text{ViolentCrimes}/\text{Population}$ is **already** a unit-free comparison; multiplying by $10^5$ just rescales for readability. Equivalently you can interpret Rate.Violent as
$$\widehat{\mathbb P}(\text{a randomly drawn inhabitant was a violent-crime victim this year})\times 100\,000.$$
So Alaska's $862$ means roughly $0.86\%$ of Alaskans were victims of a recorded violent crime — a probability statement on the **same scale** for every state regardless of size.

</details>

---

<details class="master-subpart">
<summary>(c) Bin-and-compare workflow</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (d) The same template for other common derived variables</summary>

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

</details>

---

<details class="master-subpart">
<summary>(e) Pitfalls and sanity checks</summary>

1. **Zero-denominator rows.** If any Population (or Area, Revenue, ...) is 0 or near-0, the rate is undefined or explodes. Drop or winsorise:
   ```r
   crime <- subset(crime, Population > 0)
   ```
2. **Different reference periods.** Crime counts are annual; if Population is a mid-year estimate, ensure both refer to the same year. A 5% timing mismatch becomes a 5% rate error.
3. **Small-denominator instability.** A state of $50\,000$ people with 3 recorded murders has rate $6.0$/100k, but the *uncertainty* is enormous (a Poisson SE of $\sqrt{3}/50000\times 10^5 \approx 3.5$). Always report SE or CI for rates from small denominators:
   $$\widehat{\mathrm{SE}}(\text{Rate})\;=\;\sqrt{\widehat\lambda/N}\times 10^5,\qquad \widehat\lambda=\text{count}/\text{Pop}.$$
4. **Margin% with near-zero revenue.** $(\text{Rev}-\text{Cost})/\text{Rev}$ is unstable when Rev is tiny; consider log-revenue or absolute profit instead.
5. **Do not double-normalise.** Rate.Violent is already per-capita; do not then divide by Population again when comparing across states.

</details>

---

<details class="master-subpart">
<summary>(f) Putting it together — the analysis pipeline</summary>

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

</details>
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
""",
    "images": ["statistics/images/master/master_g4a_bytype_ai.png"],
}

master_exercises["g4b_skew"] = {
    "title": "Master Exam — Mean vs median under skewness (consolidated)",
    "content": r"""**Setup.** Across Ex 1 and Ex 2 we repeatedly compute both the **mean** and the **median** of the variable `Sales` (monthly turnover of $n=100$ pizzerie, in €) and observe a *systematic gap*:

$$\widehat{\text{Me}} \;=\; 22\,350\;\text{€},\qquad \bar x \;=\; 23\,947\;\text{€},\qquad \bar x - \widehat{\text{Me}} \;=\; +1\,597\;\text{€}.$$

The mean is **above** the median by about $7\%$. The histogram of `Sales` (see master **g1c_hist**) shows a **long right tail** --- a handful of high-revenue shops up to $80\,000$€ pull the mean upward while leaving the median essentially unchanged. This master pins down exactly *why* this happens, *when* to prefer the median, and how to read the sign of $\bar x - \widehat{\text{Me}}$ as a quick **skewness diagnostic**.

---

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) Rule of thumb --- the sign of $\bar x - \widehat{\text{Me}}$ encodes the skew</summary>

| Shape of the distribution | Relation | Visual cue |
|---|---|---|
| **Right-skewed** (long tail on the right) | $\bar x > \widehat{\text{Me}}$ | mean is *pulled* into the tail $\Rightarrow$ above the median |
| **Symmetric** (no tail asymmetry) | $\bar x = \widehat{\text{Me}}$ | mean and median coincide |
| **Left-skewed** (long tail on the left) | $\bar x < \widehat{\text{Me}}$ | mean is pulled into the *left* tail $\Rightarrow$ below the median |

For pizzerie `Sales`: $\bar x - \widehat{\text{Me}} = +1\,597 > 0$ $\;\Rightarrow\;$ **right-skewed**, consistent with the histogram. Equivalently, the dimensionless **Pearson skewness coefficient**

$$\text{Sk}_P \;=\; \frac{3(\bar x - \widehat{\text{Me}})}{s} \;=\; \frac{3 \times 1\,597}{s}$$

is positive ($s$ here is the sample standard deviation, around $11\,500$€, giving $\text{Sk}_P \approx 0.42$ --- a *moderate* positive skew).

![Master illustration](statistics/images/master/master_g4b_skew_ai.png)

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) Why the mean is pulled by the tail --- **sum** vs **rank**</summary>

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

</details>

---

<details class="master-subpart">
<summary>(c) Robustness --- the **breakdown point**</summary>

The *breakdown point* of an estimator is the smallest fraction of observations that one must corrupt (replace by $\pm\infty$) to send the estimator to $\pm\infty$.

| Estimator | Breakdown point | Interpretation |
|---|:---:|---|
| **Mean** $\bar x$ | $0$ (formally $1/n \to 0$) | a *single* contaminated observation suffices |
| **Median** $\widehat{\text{Me}}$ | $50\%$ | must corrupt *half* the sample to break it |

The median is the **most robust** of all sensible location estimators --- you cannot do better than $50\%$ breakdown, and the median attains the bound. This is exactly why the toy demonstration above moves the mean but not the median: corrupting $1$ out of $9$ points is $11\%$ contamination, well within the median's tolerance but already infinite for the mean.

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (d) Decision rule --- when to report the **median** and when the **mean**</summary>

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

</details>
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

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) Approximate mean — midpoint weighting</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) Approximate median — linear interpolation inside the median class</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (c) Why these are *approximations* — the uniform-on-interval assumption</summary>

Both formulae above rely on the same hidden hypothesis:
**Within each class $[a_i,b_i)$, the original observations are uniformly distributed.**

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

</details>

---

### Summary

| Quantity | Formula | Brescia value | Interpretation |
|---|---|---:|---|
| Approx. mean   | $\bar x_g=\sum f_i m_i$ | $25.35$ | Average monthly turnover (k€) |
| Approx. median | $a_M + w_M(0.5-F_{M-1})/f_M$ | $21.91$ | Half the shops are below |
| Mean $>$ median | gap $\approx 3.4$ | --- | **Right-skew** signature |
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

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) Compute mean / median per subgroup</summary>

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

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) Compare the gap — absolute and relative</summary>

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

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (c) Interpret — which subgroup has the higher central tendency?</summary>

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

</details>

<details class="master-subpart">
<summary>(d) Simpson's paradox warning — when pooling can lie</summary>

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

</details>

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

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) Joint absolute frequencies $n_{ij}$</summary>

The entries above *are* the joint absolute frequencies: $n_{ij}$ counts customers simultaneously in row category $i$ and column category $j$. They satisfy $n_{ij} \ge 0$ and $\sum_{i,j} n_{ij} = n$. Each cell answers the question "how many customers are *both* of sex $i$ *and* history $j$?" — e.g.\ $n_{F,\text{Yes}} = 140$ means $140$ customers are female *and* repeat purchasers.

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-4plus">≥4 ex</span> (b) Marginal distributions</summary>

Sum across one index to collapse the joint table to a univariate distribution.

* **Row marginal** $n_{i\cdot} = \sum_j n_{ij}$: this is the **distribution of `Sex` ignoring `History`** — $(210, 230, 40, 20)$ for $(F, M, NB, NA)$. Proportions: $(0.420, 0.460, 0.080, 0.040)$.
* **Column marginal** $n_{\cdot j} = \sum_i n_{ij}$: distribution of `History` ignoring `Sex` — $(305, 195)$ for (Yes, No). Proportions: $(0.610, 0.390)$.

So $61\%$ of all customers are repeat buyers, and the modal sex is `M` ($46\%$).

```r
rowSums(tbl)                              # n_{i.} = 210 230 40 20
colSums(tbl)                              # n_{.j} = 305 195
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (c) Joint proportions $p_{ij} = n_{ij}/n$</summary>

Divide every joint cell by $n = 500$:

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (d) Row-conditional distributions $p(j \mid i) = n_{ij}/n_{i\cdot}$</summary>

"Given a customer is of sex $i$, what is the chance of history $j$?" Divide each cell by *its own row total*:

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (e) Column-conditional distributions $p(i \mid j) = n_{ij}/n_{\cdot j}$</summary>

"Given a customer's history is $j$, what is the chance of sex $i$?" Divide by *column totals* $(305, 195)$:

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (f) Independence check</summary>

Under statistical independence of `Sex` and `History`,
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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (g) Qualitative reading of association</summary>

Putting the conditional comparisons together:

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

</details>
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

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) Conditional mean, median, and standard deviation per group</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) Side-by-side boxplots</summary>

A boxplot per District places the five-number summary (min, Q1, median, Q3, max) on a common $y$-axis so the eye can compare **levels** (medians) and **spreads** (IQRs) at once. Schematically:

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (c) Between-group gap vs within-group spread</summary>

The substantive question is whether the District *level* effect is **large relative to within-District noise**. Define:

$$\text{Between-gap (max)} = \bar x_P - \bar x_L = 26\,400 - 21\,500 = 4\,900 \text{ €}.$$
$$\text{Within-group SD (pooled)} = s_{\text{pool}} = \sqrt{\frac{\sum_g (n_g - 1)s_g^2}{n - G}} = \sqrt{\frac{34\cdot 7800^2 + 32\cdot 8400^2 + 31\cdot 8100^2}{97}} \approx 8\,100.$$

The pooled within-group SD ($\approx 8\,100$ €) is **almost double the max between-group gap** ($4\,900$ €). Individual pizzerias scatter much more than group means. Visually, the boxes in (b) **overlap heavily** — a randomly chosen Loreto pizzeria can easily out-earn a randomly chosen Porta Romana one.

</details>

---

<details class="master-subpart">
<summary>(d) Effect size</summary>

Several standardised measures formalise (c):

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

</details>

---

<details class="master-subpart">
<summary>(e) Simpson's paradox warning</summary>

Conditional summaries can **reverse the marginal story** when a lurking variable correlates with both group membership and outcome.

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

</details>
""",
    "images": ["images/master/master_g8_condsumm_ai.png"],
}

master_exercises["g5_disp"] = {
    "title": "Master Exam — Dispersion measures: range, IQR, variance, SD, CV (consolidated)",
    "content": r"""**Setup.** Customer spending dataset `DS` with variable `AmountSpent` (in euros): $n=500$, sample mean $\bar x = 1200$, sample standard deviation $s = 850$, and five-number summary
$$\min = 200,\quad Q_1 = 600,\quad Q_2 = \text{median}\;(\text{not given}),\quad Q_3 = 1700,\quad \max = 4500.$$

**Why dispersion?** Centre alone ($\bar x$, median) does *not* describe a distribution. Two samples can share the same mean yet differ wildly in spread. Dispersion measures quantify **how far observations lie from the centre**, and (for the CV) make spreads from different variables/units **directly comparable**.

---

<details class="master-subpart" open>
<summary><span class="tag tag-4plus">≥4 ex</span> (a) Range = max − min</summary>

The crudest spread measure: distance between the two extremes.
$$\boxed{\;\text{Range} \;=\; \max - \min \;=\; 4500 - 200 \;=\; 4300\;\text{euros}.\;}$$

**Pros:** trivially fast, intuitive.
**Cons:** uses only **two observations**, completely ignores the bulk; *extremely sensitive to outliers* (a single very large purchase inflates it). It also grows mechanically with $n$ (more data $\Rightarrow$ more chances to see extremes).

```r
range(DS$AmountSpent)            # min and max
diff(range(DS$AmountSpent))      # range = max - min   -> 4300
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) Interquartile range IQR = $Q_3 - Q_1$</summary>

Spread of the **central 50%** of the data --- robust by construction.
$$\boxed{\;\text{IQR} \;=\; Q_3 - Q_1 \;=\; 1700 - 600 \;=\; 1100\;\text{euros}.\;}$$

The IQR ignores the bottom 25% and the top 25%, so it is **robust** to outliers and skew. It is the spread used by the boxplot (box width) and by Tukey's fence rule for outlier flagging: an observation is "outlying" if it lies outside $[Q_1 - 1.5\,\text{IQR},\;Q_3 + 1.5\,\text{IQR}] = [600-1650,\;1700+1650] = [-1050,\;3350]$. The recorded $\max = 4500 > 3350$ already signals at least one Tukey-outlier on the upper tail.

```r
quantile(DS$AmountSpent, c(0.25, 0.75))
IQR(DS$AmountSpent)              # Q3 - Q1             -> 1100
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (c) Sample variance $s^2 = \tfrac{1}{n-1}\sum(x_i - \bar x)^2$</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (d) Standard deviation $s = \sqrt{s^2}$</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (e) Coefficient of variation CV = $s / \bar x$ (unit-free)</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-4plus">≥4 ex</span> (f) Putting it all together --- interpretation for `AmountSpent`</summary>

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

</details>
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

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) Definition of the $q$-th quantile</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) Sorted-data (empirical) percentile rule --- "smallest $k$"</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (c) Ogive linear interpolation for grouped data</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (d) Computing $Q_1$, $Q_2$ (median), $Q_3$, $P_{90}$, deciles for `Time`</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (e) Reading quantiles off the ECDF / ogive</summary>

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

</details>
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

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (a) Covariance — the formula and what it measures</summary>

The **sample covariance** is
$$\boxed{\;\operatorname{cov}(X,Y) \;=\; \frac{1}{n-1}\sum_{i=1}^{n}(x_i-\bar x)(y_i-\bar y)\;}$$

Each term $(x_i-\bar x)(y_i-\bar y)$ is **positive** when both deviations have the *same* sign (point in the upper-right or lower-left of the scatter relative to the means) and **negative** when they have *opposite* signs (upper-left or lower-right). Summing across the $n$ points and dividing by $n-1$ (Bessel's correction, same as for $s^2$) gives a single number whose **sign** says everything about *direction*:

* $\operatorname{cov}>0$: $X$ and $Y$ tend to move **together** (high $X$ with high $Y$).
* $\operatorname{cov}<0$: they move in **opposite** directions.
* $\operatorname{cov}\approx 0$: no *linear* tendency (could still be non-linearly associated).

**The unit problem.** Covariance carries the **product of the units** of $X$ and $Y$. For pizzerie, $\operatorname{cov}(P,S)$ is measured in $\in\cdot(\text{thousand }\in)$ --- not directly interpretable. Worse, rescaling $X$ (say, expressing Price in cents instead of euros) multiplies the covariance by $100$ without changing the underlying relationship. We therefore **standardise** $\Rightarrow$ correlation.

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) Pearson correlation coefficient $r$</summary>

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

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (c) Reading a scatterplot — direction, form, strength</summary>

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

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (d) Coefficient of determination $r^2$ — variance explained</summary>

Squaring the correlation gives a percentage interpretation:
$$\boxed{\;r^2 \;=\; \text{fraction of }\operatorname{Var}(Y)\text{ linearly explained by }X.\;}$$

This is the same $R^2$ that appears in simple linear regression of $Y$ on $X$ (or $X$ on $Y$ --- it is symmetric in the bivariate case). The decomposition is
$$\operatorname{Var}(Y) \;=\; \underbrace{r^2\operatorname{Var}(Y)}_{\text{explained}} \;+\; \underbrace{(1-r^2)\operatorname{Var}(Y)}_{\text{residual}}.$$

**Pizzerie.** $r=-0.149 \Rightarrow r^2 = 0.0222 \Rightarrow$ Price linearly accounts for only **2.2%** of the variation in Sales. The remaining $97.8\%$ is driven by location, quality, marketing, customer base, etc. --- correlation alone tells us most of the story is *elsewhere*.

```r
cor(price, sales)^2             # 0.0222 -> r^2 = 2.2% variance explained
```

</details>

<details class="master-subpart">
<summary>(e) Correlation $\neq$ causation</summary>

A strong $|r|$ proves only that two variables **co-vary**, not that one **causes** the other. Three competing explanations always coexist:

1. **$X$ causes $Y$** (the intuitive read).
2. **$Y$ causes $X$** (reverse causation --- e.g. high Sales let owners raise Price).
3. **A third variable $Z$ causes both** (confounding --- e.g. tourist-area location drives both Price *and* Sales upward, *masking* the negative direct effect of Price).

For pizzerie, $Z$ = neighbourhood income is an obvious confounder. To make causal claims one needs **randomised experiments**, **controlled regression** (multivariate adjustment), **instrumental variables**, or **natural experiments** --- never $r$ alone.

</details>

<details class="master-subpart">
<summary>(f) Sensitivity to outliers</summary>

Pearson's $r$ is built from sums of *products of deviations*, so a single extreme point can **dominate** the calculation. A high-leverage outlier in the upper-right corner can drag $r$ from near $0$ up to $+0.8$, or *vice versa*. Diagnostic habits:

* Plot the data **first**; never trust $r$ without a scatter.
* Recompute $r$ after removing the most extreme 1–2 points; if $r$ changes a lot, the conclusion was outlier-driven.
* Consider a **robust** alternative (Spearman, below; or Kendall's $\tau$).

```r
# Outlier sensitivity check: drop the most extreme joint point
idx_out <- which.max(abs(scale(price)) + abs(scale(sales)))
cor(price[-idx_out], sales[-idx_out])    # r without the most extreme point
```

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (g) Spearman rank correlation $r_S$ — for monotonic but non-linear association</summary>

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

</details>
""",
    "images": ["images/master/master_g9_corr_ai.png"],
}

master_exercises["g10_normal"] = {
    "title": "Master Exam — Normal distribution N(100, 100)",
    "content": r"""**Setup.** Let $X \sim \mathcal{N}(\mu = 100,\; \sigma^2 = 100)$, so that $\sigma = \sqrt{100} = 10$. The normal (Gaussian) distribution is the **single most important continuous distribution in statistics**: it is the limit of standardised sums (Central Limit Theorem), the natural noise model in regression, and the basis of nearly every classical inference procedure.

---

**Master recipe — one mental model for *every* normal question.** Every probability/quantile problem on $X\sim\mathcal{N}(\mu,\sigma^2)$ reduces to the same four-step routine. Internalise it once and stop re-deriving:

$$\boxed{\;\;
\begin{array}{rl}
\textbf{1. Standardise:} & \; Z \;=\; \dfrac{X - \mu}{\sigma} \;\sim\; \mathcal{N}(0,1). \\[6pt]
\textbf{2. CDF lookup:} & \; \mathbb{P}(X \le x) \;=\; \Phi\!\left(\dfrac{x-\mu}{\sigma}\right). \\[6pt]
\textbf{3. Identities:} & \; \Phi(-z) = 1 - \Phi(z),\;\; \mathbb{P}(X>x) = 1-\Phi(z),\;\; \mathbb{P}(|X-\mu|>k\sigma)=2[1-\Phi(k)]. \\[6pt]
\textbf{4. Inversion:} & \; x_q \;=\; \mu + z_q\,\sigma,\quad z_q = \Phi^{-1}(q).
\end{array}\;\;}$$

**Decision flow** — which step you start at depends on what the question hands you:

| Given | Want | Path |
|---|---|---|
| value $x$ | $\mathbb{P}(X\le x)$, $\mathbb{P}(X>x)$ | 1 → 2 → 3 |
| probability $q$ | quantile $x_q$ | 4 |
| two values $a<b$ | $\mathbb{P}(a\le X\le b)$ | 1 → 2 (twice) → subtract |
| half-width $k$ | $\mathbb{P}(|X-\mu|>k\sigma)$ | 3 (symmetric tail) |

Everything below is just this recipe applied case by case. The subparts add the **density formula**, the **68–95–99.7 quick rule** (when you do not have a $\Phi$-table handy), and the **numbers** for the running example $\mathcal{N}(100,100)$.

---

<details class="master-subpart" open>
<summary>(a) Density formula</summary>

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

</details>

<details class="master-subpart">
<summary>(b) Empirical 68–95–99.7 rule</summary>

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

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (c) Standardisation $Z = (X-\mu)/\sigma$</summary>

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

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (d) Computing $\mathbb{P}(X \le x)$ via $\Phi$</summary>

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

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (e) Quantile inversion $x_q = \mu + z_q \cdot \sigma$</summary>

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

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (f) Tail probabilities</summary>

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

</details>
""",
    "images": ["images/master/master_g10_normal_ai.png"],
}

master_exercises["g11_clt"] = {
    "title": "Master — Sampling distributions & the CLT",
    "content": r"""**Setup.** Let $X_1, X_2, \dots, X_n$ be an i.i.d. sample from a population with mean $\mu = E[X]$ and variance $\sigma^2 = \text{Var}(X) < \infty$. The **sample mean**
$$\bar X \;=\; \frac{1}{n}\sum_{i=1}^n X_i$$
is itself a random variable, with its own distribution called the **sampling distribution of $\bar X$**. The whole point of inferential statistics is to know how $\bar X$ behaves so we can use it to estimate the unknown $\mu$. The same machinery covers the **sample proportion** $\hat p = \tfrac{1}{n}\sum X_i$ for $X_i\sim\text{Bernoulli}(p)$: the recipe for $\hat p$ **is** the recipe for $\bar X$ specialised to Bernoulli$(p)$, with mean $p$ and variance $p(1-p)$. Anything you can do for a mean you can do for a proportion just by plugging $\mu\!\to\!p$, $\sigma^2\!\to\!p(1-p)$.

---

**Master recipe — every sampling-distribution question.** Every problem about $\bar X$ or $\hat p$ reduces to the same four-step routine. Internalise it once and stop reinventing case by case:

$$\boxed{\;\;
\begin{array}{rl}
\textbf{1. Identify the statistic:} & \; \text{sample mean } \bar X \;\;\text{or}\;\; \text{sample proportion } \hat p. \\[6pt]
\textbf{2. Mean \& SE:} & \; E[\bar X]=\mu,\;\; \text{SE}(\bar X)=\sigma/\sqrt{n}; \quad E[\hat p]=p,\;\; \text{SE}(\hat p)=\sqrt{p(1-p)/n}. \\[6pt]
\textbf{3. Distribution:} & \; \text{exact } N \text{ (Normal parent)},\;\; \text{CLT-}N \text{ (any parent, } n\ge 30\text{)},\;\; \text{or CLT-}N \text{ for } \hat p \text{ if } np,\,n(1-p)\ge 5. \\[6pt]
\textbf{4. Standardise:} & \; Z=\dfrac{\bar X-\mu}{\sigma/\sqrt n}\;\;\text{(or}\;\;\dfrac{\hat p-p}{\sqrt{p(1-p)/n}}\text{)},\;\;\text{then use } \Phi \text{ for tail/interval/quantile probabilities.}
\end{array}\;\;}$$

**Decision flow** — which step you start at depends on what the question hands you:

| Given (parent shape, $n$, statistic) | Want | Path |
|---|---|---|
| Normal parent, any $n$, $\bar X$ | $P(\bar X\le c)$, $P(\bar X>c)$ | exact $\bar X\sim N(\mu,\sigma^2/n)$ → standardise → $\Phi$ |
| Any parent with $n\ge 30$, $\bar X$ | $P(\bar X\le c)$ or interval | CLT: $\bar X\stackrel{\cdot}{\sim} N(\mu,\sigma^2/n)$ → standardise → $\Phi$ |
| Bernoulli($p$) with $np,n(1-p)\ge 5$, $\hat p$ | $P(\hat p\le c)$ or interval | CLT: $\hat p\stackrel{\cdot}{\sim} N(p,p(1-p)/n)$ → standardise → $\Phi$ |
| Any of above, given probability $q$ | quantile $c_q$ for $\bar X$ or $\hat p$ | $c_q=\mu+z_q\cdot\text{SE}$ (or $p+z_q\cdot\text{SE}$) |
| Any of above, two values $a<b$ | $P(a\le\bar X\le b)$ | standardise twice → $\Phi(z_b)-\Phi(z_a)$ |
| Sum $S=\sum X_i$ rather than mean | $P(S\le c)$ | $S\stackrel{\cdot}{\sim} N(n\mu,n\sigma^2)$ — just $n\times$ the mean's mean and $n\times$ the variance |

Everything below is this recipe spelled out: subparts (a)–(d) build the **mean rule** with $\sqrt n$ scaling, exact-Normal and CLT cases; (e) is the **proportion specialisation**; (f) is a fully worked end-to-end example.

---

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) Mean, variance, and standard error of $\bar X$</summary>

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

</details>

---

<details class="master-subpart">
<summary>(b) Normal population $\Rightarrow$ $\bar X$ is *exactly* Normal</summary>

If $X_i \stackrel{\text{iid}}{\sim} N(\mu, \sigma^2)$ then
$$\bar X \;\sim\; N\!\left(\mu,\; \frac{\sigma^2}{n}\right) \quad \text{for every } n \ge 1.$$
**No approximation needed** --- the result is exact because linear combinations of jointly Normal variables are Normal (see master `g12_lincomb`). This is the "best case" --- even $n=2$ gives an exactly Normal $\bar X$.

```r
# Simulate X-bar for X ~ N(mu, sigma^2): exact Normal sampling distribution
xbar_sim <- replicate(10000, mean(rnorm(n, mu, sigma)))
hist(xbar_sim, breaks = 40, freq = FALSE, main = "X-bar from Normal pop")
curve(dnorm(x, mu, se), add = TRUE, col = "red", lwd = 2)
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (c) Central Limit Theorem (CLT) --- the universal saviour</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-4plus">≥4 ex</span> (d) Rule of thumb: when is $n$ "large enough"?</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (e) Sampling distribution of a proportion $\hat p$</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (f) Worked example --- computing $P(\bar X \le c)$</summary>

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

</details>
""",
    "images": ["images/master/master_g11_clt_ai.png"],
}

master_exercises["g12_lincomb"] = {
    "title": "Master — Linear combinations of Normal random variables",
    "content": r"""**Setup.** Let $X_1, \dots, X_k$ be Normal random variables with means $\mu_i$, variances $\sigma_i^2$, and (optionally) covariances $\sigma_{ij} = \text{Cov}(X_i, X_j)$. For constants $a_1, \dots, a_k, b \in \mathbb{R}$ form the **linear combination**
$$Y \;=\; b + \sum_{i=1}^{k} a_i X_i.$$
The headline fact: if the $X_i$ are **jointly Normal** (in particular, if they are independent Normals), then $Y$ is itself **exactly Normal** — no CLT approximation needed. Sum, difference, scaling, sample mean, two-asset portfolio, cost = price $\times$ quantity at fixed quantity — all are special cases of one formula.

---

**Master recipe — one mental model for *every* linear-combination question.** Every problem about $Y = b + \sum a_i X_i$ where the $X_i$ are jointly Normal reduces to the same four-step routine. Internalise it once and stop re-deriving case by case:

$$\boxed{\;\;
\begin{array}{rl}
\textbf{1. Identify } a_i, b: & \; \text{rewrite the question as } Y = b + \sum_i a_i X_i. \\[6pt]
\textbf{2. Mean (always linear):} & \; \mathbb E[Y] \;=\; b + \sum_i a_i\,\mu_i. \\[6pt]
\textbf{3. Variance:} & \; \mathrm{Var}(Y) \;=\; \sum_i a_i^2\,\sigma_i^2 \;\underbrace{+\; 2\sum_{i<j} a_i a_j\,\sigma_{ij}}_{=\,0\;\text{if independent}}. \\[6pt]
\textbf{4. Standardise (G10):} & \; Z \;=\; \dfrac{Y - \mathbb E[Y]}{\sqrt{\mathrm{Var}(Y)}}\;\sim\;\mathcal N(0,1) \;\Longrightarrow\; \text{apply the G10 recipe (}\Phi, \Phi^{-1}\text{).}
\end{array}\;\;}$$

**Decision flow** — pick the row that matches the operation in the question, then plug into the columns:

| Operation | $a_i$ | $\mathbb E[Y]$ | $\mathrm{Var}(Y)$ (independent) | $\mathrm{Var}(Y)$ (correlated) |
|---|---|---|---|---|
| **Sum** $X_1+X_2$ | $a_1=a_2=1$ | $\mu_1+\mu_2$ | $\sigma_1^2+\sigma_2^2$ | $+\,2\sigma_{12}$ |
| **Difference** $X_1-X_2$ | $a_1=1,\;a_2=-1$ | $\mu_1-\mu_2$ | $\sigma_1^2+\sigma_2^2$ | $-\,2\sigma_{12}$ |
| **Scaling/affine** $b+cX$ | $a_1=c$ | $b+c\mu$ | $c^2\sigma^2$ | — |
| **Mean of i.i.d.** $\bar X = \frac1n\sum X_i$ | $a_i=1/n$ | $\mu$ | $\sigma^2/n$ | — |
| **General LC** $\sum a_iX_i$ | given $a_i$ | $\sum a_i\mu_i$ | $\sum a_i^2\sigma_i^2$ | $+\,2\sum_{i<j}a_ia_j\sigma_{ij}$ |

**Two traps worth memorising.** (i) For $X-Y$ the variance is $\sigma_X^2+\sigma_Y^2-2\sigma_{XY}$ — **the marginal variances still add**, only the covariance term flips sign (variance can never be negative). (ii) Inside the jointly-Normal world only, $\sigma_{XY}=0 \Leftrightarrow X\perp\!\!\!\perp Y$ — so under independence the variance always reduces to $\sum a_i^2\sigma_i^2$.

Everything below is this recipe spelled out: subparts (a)–(b) handle the **mean and variance rules** including the covariance correction; (c) states the **Normality preservation** theorem; (d) lists the most-used **special cases**; (e) the **independence simplification**; (f) a fully worked $P(X+Y>k)$ example tying steps 1–4 together.

---

<details class="master-subpart" open>
<summary><span class="tag tag-4plus">≥4 ex</span> (a) Expectation --- linearity always holds</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (b) Variance --- the covariance term matters</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-4plus">≥4 ex</span> (c) Normality is preserved under linear combinations</summary>

If $(X, Y)$ is **bivariate Normal**, then for *any* constants $a, b, c$:
$$\boxed{\;aX + bY + c \;\sim\; N\!\left(a\mu_X + b\mu_Y + c,\; a^2\sigma_X^2 + b^2\sigma_Y^2 + 2ab\,\text{Cov}(X,Y)\right)\;}$$

This is the **defining feature** of the multivariate Normal family: every linear combination of jointly Normal variables is itself Normal. (Marginally-Normal-but-not-jointly-Normal counterexamples exist, but in this course "Normal + Normal" always means jointly Normal.)

**Generalisation.** For $X_1, \dots, X_k$ jointly Normal,
$$\sum_{i=1}^k a_i X_i + c \;\sim\; N\!\left(\sum a_i\mu_i + c,\;\; \sum_i a_i^2\sigma_i^2 + 2\sum_{i<j} a_i a_j \,\text{Cov}(X_i,X_j)\right).$$
This is exactly the engine behind master `g11_clt` (the sample mean $\bar X = \tfrac{1}{n}\sum X_i$ is a linear combination with $a_i = 1/n$).

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-4plus">≥4 ex</span> (d) Special cases</summary>

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

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-2plus">≥2 ex</span> (e) Independence is the magical simplifier</summary>

If $X \perp\!\!\!\perp Y$, then $\text{Cov}(X,Y) = 0$, so:
$$\text{Var}(aX + bY) \;=\; a^2\sigma_X^2 \;+\; b^2\sigma_Y^2 \qquad (\text{cross-term vanishes}).$$
And $X + Y \sim N(\mu_X + \mu_Y, \sigma_X^2 + \sigma_Y^2)$, $X - Y \sim N(\mu_X - \mu_Y, \sigma_X^2 + \sigma_Y^2)$ --- **same variance** for sum and difference under independence (a common exam trap).

**Note.** For jointly Normal variables, $\text{Cov}(X,Y) = 0$ $\Leftrightarrow$ $X \perp\!\!\!\perp Y$ (a special property of the Normal family). In general $\text{Cov}=0$ does **not** imply independence, but inside the Normal world it does.

```r
# Independence: cov_XY = 0  =>  Var(X+Y) = Var(X-Y) = sig_X^2 + sig_Y^2
var_sum_ind <- sig_X^2 + sig_Y^2              # 250000
```

</details>

---

<details class="master-subpart">
<summary><span class="tag tag-4plus">≥4 ex</span> (f) Worked example --- $P(X + Y > k)$</summary>

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

</details>
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

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) The 5-number summary --- what it captures, and what it doesn't</summary>

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

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (b) The box: from $Q_1$ to $Q_3$ --- the middle 50%</summary>

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

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (c) The median line inside the box</summary>

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

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (d) Whiskers --- to the nearest data within $1.5\cdot\text{IQR}$ of the fences</summary>

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

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (e) Outliers --- points beyond the fences</summary>

Any datum $x_i < L$ or $x_i > U$ is plotted as an **individual dot** (a "fly", in Tukey's original terminology) outside the whiskers. It is *not* part of the whisker; it is a singled-out point.

For pizzerie Sales, the only outliers are on the **upper** side: the handful of shops with monthly turnover in the $50$--$80$ k€ range (e.g. the $\max=80$ k€ shop). There are **no lower outliers** (none below $1.85$ k€).

Counting from the dataset: roughly $4$ shops are flagged as upper outliers --- exactly the right tail that pulls the mean upward and makes $\bar x > \widetilde{m}$. The full theory of outlier flagging (IQR rule, $3\cdot\text{IQR}$ extreme rule, z-score rule) is the subject of master `g6c_outliers`.

```r
sales[sales < L | sales > U]                # flagged outliers
length(sales[sales > U])                    # count of upper outliers
```

</details>

<details class="master-subpart">
<summary><span class="tag tag-exam">EXAM</span> (f) Reading the boxplot --- skew, spread, outliers in one glance</summary>

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

</details>
""",
    "images": ["statistics/images/master/master_g6b_box_ai.png"],
}

master_exercises["g6c_outliers"] = {
    "title": "Master Exam — Outliers & extreme values",
    "content": r"""**Setup.** Continuing with the pizzerie dataset ($n=100$ shops, `Sales` in k€), the five-number summary is $(\min, Q_1, \widetilde{m}, Q_3, \max) = (8.0,\; 17.9,\; 22.4,\; 28.6,\; 80.0)$ and the interquartile range is $\text{IQR} = Q_3 - Q_1 = 10.7$ k€. The sample mean and standard deviation are $\bar x = 23.9$ k€ and $s = 11.6$ k€. The questions for an **outlier** are: *Is this point a typo, an unusual real observation, or a glimpse of a heavy tail?* and *What --- if anything --- should we do about it?* This master is the rule-book.

---

<details class="master-subpart" open>
<summary><span class="tag tag-exam">EXAM</span> (a) The IQR rule (Tukey, the default in `boxplot()`)</summary>

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

</details>

<details class="master-subpart">
<summary>(b) The $3\cdot\text{IQR}$ rule --- extreme outliers</summary>

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

</details>

<details class="master-subpart">
<summary>(c) The z-score rule</summary>

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

</details>

<details class="master-subpart">
<summary>(d) Impact on summary statistics --- non-robust vs robust</summary>

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

</details>

<details class="master-subpart">
<summary>(e) What to do with outliers --- a decision protocol</summary>

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

</details>
""",
    "images": ["statistics/images/master/master_g6c_outliers_ai.png"],
}
