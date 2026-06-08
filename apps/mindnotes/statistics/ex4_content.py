"""
Ex 4 snippets — Statistics Module 4 (probability, normal distribution,
sampling distributions, CLT, linear combinations of random variables).
"""

ex4 = {}

# ========== EXERCISE 4.1 (tea in a glass) ==========

ex4["4_1a"] = {
"title": "Ex 4.1a — P(X > 10) for X ~ N(8, 1.2²)",
"content": """**Question.**

![Ex 4.1a question](statistics/images/ex4/questions/ex4_4_1a_question.png)

---

**Answer.**

![Ex 4.1a answer](statistics/images/ex4/answers/ex4_4_1a_answer.png)

---

**AI walkthrough.** Let $X$ denote the amount (in cl) of tea poured into each glass; $X \\sim N(8, 1.2^2)$. The threshold $x = 10$ sits $z = (10-8)/1.2 \\approx 1.6667$ standard deviations above the mean, so $\\Pr(X > 10) = \\Pr(Z > 1.6667) \\approx 0.0478$. The shaded right tail in the figure is the overflow probability on both the original and the standardized scales.

```r
1 - pnorm(10, mean=8, sd=1.2)
## [1] 0.04779035
```

![Ex 4.1a AI walkthrough](statistics/images/ex4/ex4_4_1a_ai.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_1a_question.png",
    "statistics/images/ex4/answers/ex4_4_1a_answer.png",
    "statistics/images/ex4/ex4_4_1a_ai.png",
],
}

ex4["4_1b"] = {
"title": "Ex 4.1b — P(X < 7.5) for X ~ N(8, 1.2²)",
"content": """**Question.** What is the probability that the amount of tea poured into a glass is less than 7.5 cl?

![Ex 4.1b question](statistics/images/ex4/questions/ex4_4_1b_question.png)

---

**Answer.** The probability that the amount of tea poured into a glass is less than 7.5 cl, $P(X < 7.5)$, is **0.3385**:

```r
pnorm(7.5, mean=8, sd=1.2)
## [1] 0.3384611
```

**Walkthrough.** The threshold $x = 7.5$ sits $z = (7.5 - 8)/1.2 \\approx -0.4167$ standard deviations below the mean — less than half a $\\sigma$ to the left of $\\mu$. So $\\Pr(X < 7.5) = \\Pr(Z < -0.4167) \\approx 0.3385$. The shaded left tail in the figure marks this mass on both the original scale (cl) and the standardized $Z$-scale, with the same area in both panels.

![Ex 4.1b AI walkthrough](statistics/images/ex4/ex4_4_1b_ai.png)

---

**Reference answer (textbook).**

![Ex 4.1b answer](statistics/images/ex4/answers/ex4_4_1b_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_1b_question.png",
    "statistics/images/ex4/ex4_4_1b_ai.png",
    "statistics/images/ex4/answers/ex4_4_1b_answer.png",
],
}

# ========== EXERCISE 4.2 (battery life) ==========

ex4["4_2a"] = {
"title": "Ex 4.2a — P(X < 24) for battery life X ~ N(27, 3.2²)",
"content": """**Question.** The battery life of a particular cell phone model, after two years of use, is normally distributed with mean 27 hours and standard deviation of 3.2 hours. What is the probability that a cell phone of that model, after two years of use, will have a battery life of less than 24 hours?

![Ex 4.2a question](statistics/images/ex4/questions/ex4_4_2a_question.png)

---

**Answer.** Let $X$ denote the battery life. Then $X \\sim N(27, 3.2^2)$. The probability that a cell phone has battery life less than 24 hours, $P(X < 24)$, is **0.1743**:

```r
pnorm(24, mean=27, sd=3.2)
## [1] 0.1742507
```

**Walkthrough.** The threshold $x = 24$ sits $z = (24-27)/3.2 = -0.9375$ standard deviations below the mean — slightly less than one $\\sigma$ to the left of $\\mu$. So $\\Pr(X < 24) = \\Pr(Z < -0.9375) \\approx 0.1743$. The shaded left tail in the figure marks this mass on both the original scale (hours) and the standardized $Z$-scale, with the same area in both panels.

![Ex 4.2a AI walkthrough](statistics/images/ex4/ex4_4_2a_ai.png)

---

**Reference answer (textbook).**

![Ex 4.2a answer](statistics/images/ex4/answers/ex4_4_2a_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_2a_question.png",
    "statistics/images/ex4/ex4_4_2a_ai.png",
    "statistics/images/ex4/answers/ex4_4_2a_answer.png",
],
}

ex4["4_2b"] = {
"title": "Ex 4.2b — Minimum life of the longest-lasting 20% (80th percentile)",
"content": """**Question.** What is the minimum battery life of the 20% of cell phones of that model that last the longest after two years of use?

![Ex 4.2b question](statistics/images/ex4/questions/ex4_4_2b_question.png)

---

**Setup.** Let $X$ denote the battery life (in hours) after two years of use. From Ex 4.2, $X \\sim N(\\mu, \\sigma^2)$ with $\\mu = 27$ and $\\sigma = 3.2$. The "longest-lasting 20%" are the phones whose battery life exceeds 80% of the others, i.e. those for which $X > q_{0.80}$, where $q_{0.80}$ is the **80-th percentile** of the distribution of $X$:

$$
\\Pr(X \\le q_{0.80}) \\;=\\; 0.80 \\qquad \\Longleftrightarrow \\qquad \\Pr(X > q_{0.80}) \\;=\\; 0.20.
$$

The minimum battery life of a "long-lasting" phone is exactly that threshold $q_{0.80}$.

---

**Closed-form via standardisation.** Using $Z = (X - \\mu)/\\sigma \\sim N(0,1)$,

$$
q_{0.80} \\;=\\; \\mu + z_{0.80}\\,\\sigma, \\qquad z_{0.80} \\;=\\; \\Phi^{-1}(0.80) \\;\\approx\\; 0.8416.
$$

Plugging in:

$$
q_{0.80} \\;=\\; 27 + 0.8416\\cdot 3.2 \\;=\\; 27 + 2.6932 \\;=\\; \\mathbf{29.6932}\\;\\text{hours}.
$$

---

**Answer.** The minimum battery life of the 20% of cell phones that last the longest after two years of use is the **80-th percentile** of the distribution and is equal to **29.6932 hours**:

```r
qnorm(0.8, 27, 3.2)
## [1] 29.69319
```

Equivalently, via standardisation:
```r
27 + qnorm(0.8) * 3.2
## [1] 29.69319
qnorm(0.8)
## [1] 0.8416212
```

A quick consistency check — the upper tail at $q_{0.80}$ should have mass $0.20$:
```r
1 - pnorm(29.69319, 27, 3.2)
## [1] 0.2
```

---

**AI walkthrough.**

![Ex 4.2b AI walkthrough](statistics/images/ex4/ex4_4_2b_ai.png)

**Interpretation.** The dashed red line marks $q_{0.80} = 29.6932$ hours. The shaded right tail has area $0.20$ — exactly the longest-lasting 20% of phones. The right-hand panel shows the equivalent picture on the **standard** scale: $z_{0.80} = 0.8416$, then unstandardised back to $x_{0.80} = \\mu + z_{0.80}\\sigma$.

---

**Reference answer (textbook).**

![Ex 4.2b answer](statistics/images/ex4/answers/ex4_4_2b_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_2b_question.png",
    "statistics/images/ex4/ex4_4_2b_ai.png",
    "statistics/images/ex4/answers/ex4_4_2b_answer.png",
],
}

# ========== EXERCISE 4.3 (private-label spending) ==========

ex4["4_3a"] = {
"title": "Ex 4.3a — P(X > 12) for spending X ~ N(13.2, 1.2²)",
"content": """**Question.** For a large retail chain, the expenditure (in a single act of purchase, in a single receipt) of a generic customer on the purchase of *private label* products (marketed under the distributor's brand name instead of the manufacturer's brand name) can be assumed to be normally distributed with mean 13.2 euros and standard deviation 1.2 euros.

What is the probability that a customer will spend more than 12 euros for private label products?

![Ex 4.3a question](statistics/images/ex4/questions/ex4_4_3a_question.png)

---

**Answer.** Let $X$ denote the expenditure on private label products; $X \\sim N(13.2, 1.2^2)$. The probability that a customer will spend more than 12 euros is **0.8413**:

```r
1 - pnorm(12, 13.2, 1.2)
## [1] 0.8413447
```

**Walkthrough.** Standardising, $z = (12 - 13.2)/1.2 = -1$, so

$$
\\Pr(X > 12) \\;=\\; \\Pr(Z > -1) \\;=\\; 1 - \\Phi(-1) \\;=\\; \\Phi(1) \\;\\approx\\; 0.8413.
$$

The threshold $x = 12$ sits **one standard deviation below the mean**, so the right tail above it covers the central mass plus the upper half — about 84% of the distribution. The figure shows the same shaded area on the original scale ($X$, left) and on the standardised scale ($Z$, right).

![Ex 4.3a AI walkthrough](statistics/images/ex4/ex4_4_3a_ai.png)

---

**Reference answer (textbook).**

![Ex 4.3a answer](statistics/images/ex4/answers/ex4_4_3a_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_3a_question.png",
    "statistics/images/ex4/ex4_4_3a_ai.png",
    "statistics/images/ex4/answers/ex4_4_3a_answer.png",
],
}

ex4["4_3b"] = {
"title": "Ex 4.3b — Minimum expenditure of the top 10% customers (90th percentile)",
"content": """**Question.** Let us consider the "top" customers with reference to expenditure on private label products, i.e., those who spend more than 90% of other customers (in a single act of purchase) on private label products. What is the minimum amount these customers spend on private label products?

![Ex 4.3b question](statistics/images/ex4/questions/ex4_4_3b_question.png)

---

**Setup.** Let $X$ denote a generic customer's expenditure on private-label products in a single receipt. From Ex 4.3, $X \\sim N(\\mu, \\sigma^2)$ with $\\mu = 13.2$ and $\\sigma = 1.2$. The "top 10% of customers" are those whose spending exceeds 90% of others, i.e. those for whom $X > q_{0.90}$, where $q_{0.90}$ is the **90-th percentile** of the distribution of $X$:

$$
\\Pr(X \\le q_{0.90}) \\;=\\; 0.90 \\qquad \\Longleftrightarrow \\qquad \\Pr(X > q_{0.90}) \\;=\\; 0.10.
$$

The minimum amount that a "top customer" spends is exactly that threshold $q_{0.90}$.

---

**Closed-form via standardisation.** Using $Z = (X - \\mu)/\\sigma \\sim N(0,1)$,

$$
q_{0.90} \\;=\\; \\mu + z_{0.90}\\,\\sigma, \\qquad z_{0.90} \\;=\\; \\Phi^{-1}(0.90) \\;\\approx\\; 1.2816.
$$

Plugging in:

$$
q_{0.90} \\;=\\; 13.2 + 1.2816\\cdot 1.2 \\;=\\; 13.2 + 1.5379 \\;=\\; \\mathbf{14.7379}\\;\\text{euros}.
$$

---

**Answer.** The minimum expenditure of the 10% of "top" customers is the 90-th percentile of the distribution, and is equal to **€14.7379**:

```r
qnorm(0.9, 13.2, 1.2)
## [1] 14.73786
```

Equivalently, via standardisation:
```r
13.2 + qnorm(0.9) * 1.2
## [1] 14.73786
qnorm(0.9)
## [1] 1.281552
```

A quick consistency check — the upper tail at $q_{0.90}$ should have mass $0.10$:
```r
1 - pnorm(14.73786, 13.2, 1.2)
## [1] 0.1
```

---

**AI walkthrough.**

![Ex 4.3b AI walkthrough](statistics/images/ex4/ex4_4_3b_ai.png)

**Interpretation.** The dashed red line marks $q_{0.90} = 14.7379$. The shaded right tail has area $0.10$ — exactly the top decile of customers. The right-hand panel shows the equivalent picture on the **standard** scale: $z_{0.90} = 1.2816$, then unstandardised back to $x_{0.90} = \\mu + z_{0.90}\\sigma$.

---

**Reference answer (textbook).**

![Ex 4.3b answer](statistics/images/ex4/answers/ex4_4_3b_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_3b_question.png",
    "statistics/images/ex4/ex4_4_3b_ai.png",
    "statistics/images/ex4/answers/ex4_4_3b_answer.png",
],
}

ex4["4_3c"] = {
"title": "Ex 4.3c — P(total receipt of 10 customers > €135)",
"content": """**Question.** What is the probability that 10 customers will have a receipt of more than €135 on *private label* products?

![Ex 4.3c question](statistics/images/ex4/questions/ex4_4_3c_question.png)

---

**Setup.** Let $X_i$ be the expenditure of customer $i$ on private-label products in a single receipt, with $X_i \\sim N(13.2,\\,1.2^2)$ i.i.d. across the 10 customers (assuming they form a random sample). Let $S = \\sum_{i=1}^{10} X_i$ denote the **total** expenditure of the 10 customers.

A linear combination of independent normals is again normal, so $S$ is exactly $N(\\mu_S, \\sigma_S^2)$ with

$$
\\mu_S \\;=\\; E(S) \\;=\\; \\sum_{i=1}^{10} E(X_i) \\;=\\; 10\\cdot 13.2 \\;=\\; 132, \\qquad
\\sigma_S^2 \\;=\\; \\mathrm{Var}(S) \\;=\\; \\sum_{i=1}^{10} \\mathrm{Var}(X_i) \\;=\\; 10\\cdot 1.2^2 \\;=\\; 14.4,
$$

so $S \\sim N(132,\\,14.4)$ and $\\sigma_S = \\sqrt{14.4} \\approx 3.7947$ euros.

---

**Standardisation.** With $Z = (S - 132)/\\sqrt{14.4} \\sim N(0,1)$,

$$
\\Pr(S > 135) \\;=\\; \\Pr\\!\\left(Z > \\frac{135 - 132}{\\sqrt{14.4}}\\right) \\;=\\; \\Pr(Z > 0.7906) \\;=\\; 1 - \\Phi(0.7906) \\;\\approx\\; 0.2146.
$$

The threshold $s = 135$ sits about $0.79$ standard deviations **above** the mean, so the right-tail mass is appreciably less than 50%.

---

**Answer.** The probability that 10 customers will have a receipt of more than €135 on private-label products, $\\Pr(S > 135)$, is **0.2146**:

```r
1 - pnorm(135, 132, sqrt(14.4))
## [1] 0.2145977
```

Equivalently, via standardisation:

```r
1 - pnorm((135 - 132) / sqrt(14.4))
## [1] 0.2145977
```

---

**AI walkthrough.** The key step is recognising that "10 customers" turns the problem about a *single* receipt $X$ into one about the *sum* $S = X_1 + \\dots + X_{10}$. Because the $X_i$ are i.i.d. normal, $S$ is **exactly** normal — no CLT approximation needed — with mean scaled by $n$ and variance scaled by $n$ (so SD scaled by $\\sqrt{n}$).

1. **Distribution of $S$.** $\\mu_S = n\\mu = 10\\cdot 13.2 = 132$ and $\\sigma_S^2 = n\\sigma^2 = 10\\cdot 1.44 = 14.4$, giving $\\sigma_S = \\sqrt{14.4} \\approx 3.7947$. So $S \\sim N(132,\\,14.4)$.
2. **Locate the threshold.** $z = (135 - 132)/\\sqrt{14.4} \\approx 0.7906$ — the cut-off sits less than one SD above the mean.
3. **Read the tail.** $\\Pr(S > 135) = 1 - \\Phi(0.7906) \\approx 0.2146$. Roughly a **21%** chance that the 10 receipts sum to more than €135.
4. **Sanity check vs the single-customer case.** A single customer has $E(X) = 13.2$, so €135 is far above their *individual* mean — but for the sum of 10 it is just barely above the aggregate mean of €132. Aggregation pulls the threshold closer to the centre of the distribution, which is why the tail probability is moderate rather than tiny.
5. **Take-away.** For sums of i.i.d. normals: $E$ scales linearly with $n$, $\\mathrm{Var}$ scales linearly with $n$, and SD scales with $\\sqrt{n}$. Always standardise before reading off `pnorm`/`1-pnorm` to avoid sign mistakes.

![Ex 4.3c AI walkthrough](statistics/images/ex4/ex4_4_3c_ai.png)

---

**Reference answer.**

![Ex 4.3c answer](statistics/images/ex4/answers/ex4_4_3c_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_3c_question.png",
    "statistics/images/ex4/ex4_4_3c_ai.png",
    "statistics/images/ex4/answers/ex4_4_3c_answer.png",
],
}

ex4["4_3d"] = {
"title": "Ex 4.3d — P(≥ 80% of 150 customers spend > 12€) — CLT for proportion",
"content": """**Question.** Consider the 150 customers in a store: what is the probability that at least 80% will spend more than 12 Euros to purchase *private label* products?

![Ex 4.3d question](statistics/images/ex4/questions/ex4_4_3d_question.png)

---

**AI walkthrough.** The trick is to go **one level up**: in 4.3a we computed the probability that *a single customer* spends more than €12 — call it $p = 0.8413$. Here we ask about the *proportion* of customers (out of $n = 150$) who do so. That proportion $\\bar P$ is itself a random variable; the CLT tells us its sampling distribution.

1. **Single customer $\\to$ Bernoulli.** Define $W_i = \\mathbb 1\\{X_i > 12\\}$, so $W_i \\sim \\mathrm{Bernoulli}(p)$ with $p = \\Pr(X > 12) = 0.8413$ from 4.3a. Then $\\bar P = \\tfrac{1}{n}\\sum_{i=1}^{n} W_i$ — exactly the sample mean of i.i.d. Bernoulli trials.
2. **CLT $\\Rightarrow$ Normal approximation.** With $n = 150$ (large), the CLT gives
$$\\bar P \\;\\approx\\; N\\!\\left(p,\\; \\tfrac{p(1-p)}{n}\\right) \\;=\\; N(0.8413,\\; 0.0298^2), \\quad \\mathrm{SE}(\\bar P) = \\sqrt{p(1-p)/n} \\approx 0.0298.$$
A quick rule-of-thumb check: $np = 126.2 \\gg 10$ and $n(1-p) = 23.8 \\gg 10$, so the normal approximation is safe.
3. **Translate "at least 80%" to a tail event.** "At least 80% out of 150" $\\Leftrightarrow \\bar P > 0.80$. The threshold $0.80$ is **below** the mean $p = 0.8413$, so we expect a large probability (well over 50%).
4. **Standardise.** $z = (0.80 - 0.8413)/0.0298 = -1.385$, so
$$\\Pr(\\bar P > 0.80) \\;=\\; \\Pr(Z > -1.385) \\;=\\; \\Phi(1.385) \\;\\approx\\; 0.9171.$$
5. **Take-away.** Because $0.80$ is about $1.4$ standard errors below the population proportion $p$, the event "$\\geq 80\\%$ of 150 customers spend more than €12" is very likely — roughly a **92%** chance. As $n$ grows, $\\mathrm{SE}(\\bar P)$ shrinks like $1/\\sqrt{n}$ and this tail probability gets even closer to 1.

![Ex 4.3d AI walkthrough](statistics/images/ex4/ex4_3d_ai.png)

---

**Answer.** Let $W$ denote the variable that indicates whether a customer spends more than 12 Euro (success). Then $W \\sim \\mathrm{Bernoulli}(p)$ with $p = 0.8413$, the probability that a generic customer spends more than 12 Euro determined in **a)**.

Let $\\bar P$ be the proportion of customers who spend more than 12 Euro among the 150 customers of the shop. By the Central Limit Theorem the distribution of $\\bar P$ can be approximated by a Normal distribution:

$$
\\bar P \\approx N\\!\\left(p,\\; \\frac{p(1-p)}{n}\\right) = N\\!\\left(0.8413,\\; \\frac{0.8413(1-0.8413)}{150}\\right) = N(0.8413,\\; 0.0298^2).
$$

The probability that at least 80% of the 150 customers will spend more than 12 Euros, $\\Pr(\\bar P > 0.8)$, is **0.9171**:
```r
1 - pnorm(0.8, 0.8413, 0.0298)
## [1] 0.9171122
```

---

**Reference answer.**

![Ex 4.3d answer](statistics/images/ex4/answers/ex4_4_3d_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_3d_question.png",
    "statistics/images/ex4/ex4_3d_ai.png",
    "statistics/images/ex4/answers/ex4_4_3d_answer.png",
],
}

# ========== EXERCISE 4.4 (call-center — duration X ~ N(180, 1600)) ==========

ex4["4_4a1"] = {
"title": "Ex 4.4 a–c — Call duration: P(X>120), 15th percentile, P(X>5·60)",
"content": """**Question.** A mobile phone company estimated that the duration of a customer's call to the call-center follows the normal distribution with mean 180 seconds and variance 1600 sec². Let $X \\sim N(180, 1600)$.

**a)** Probability that a customer's call to the call center will last for more than 120 seconds, $\\Pr(X > 120)$.
**b)** Maximum duration of the 15% shortest phone calls (15-th percentile).
**c)** To avoid an excessive waiting time, consider interrupting the call (proposing a callback) if its duration is longer than 5 minutes. What is the proportion of phone calls that would be interrupted, $\\Pr(X > 5 \\cdot 60)$?

![Ex 4.4 a–c question](statistics/images/ex4/questions/ex4_4_4a1_question.png)

---

**AI walkthrough.** The same Normal density $X\\sim N(180,\\,40^2)$ (since $\\sigma=\\sqrt{1600}=40$) drives all three questions — only the cut-off and tail change.

1. **Pin down the parameters.** Mean $\\mu=180$ s, SD $\\sigma=\\sqrt{1600}=40$ s. So $X$ is centred at 3 min with a 40-second spread.
2. **Part a — right tail at 120 s.** Standardise: $z=(120-180)/40=-1.5$. We want $\\Pr(X>120)=\\Pr(Z>-1.5)=\\Phi(1.5)\\approx 0.9332$. Most calls ($>93\\%$) last longer than 2 minutes — the cut-off sits 1.5 SDs *below* the mean.
3. **Part b — left-tail percentile.** Invert the CDF at 0.15: $z_{0.15}=\\Phi^{-1}(0.15)\\approx -1.0364$. Un-standardise: $q_{0.15}=\\mu+z_{0.15}\\,\\sigma=180-1.0364\\cdot 40\\approx 138.54$ s. The shortest 15% of calls finish within $\\approx 2$ min 18 s.
4. **Part c — far right tail at 5 min.** With cut-off 300 s, $z=(300-180)/40=3$. Three SDs in the right tail $\\Rightarrow \\Pr(X>300)=1-\\Phi(3)\\approx 0.00135$. Only $\\approx 0.135\\%$ of calls would be interrupted — a *very* permissive policy.
5. **Take-away.** Switch between "probability of a cut-off" (`pnorm`) and "cut-off at a given probability" (`qnorm`); a sketch of the density with the target tail/percentile shaded prevents 1−p mistakes.

![Ex 4.4 a–c AI walkthrough](statistics/images/ex4/ex4_4_4a1_ai.png)

---

**Answer.**

**a)** $\\Pr(X > 120)$ is **0.9332**:
```r
1 - pnorm(120, 180, sqrt(1600))
## [1] 0.9331928
```

**b)** The 15-th percentile of $X$ is **138.5427** seconds:
```r
qnorm(0.15, 180, sqrt(1600))
## [1] 138.5427
```

**c)** The proportion of phone calls that would be interrupted, $\\Pr(X > 300)$, is **0.135%**:
```r
1 - pnorm(300, 180, sqrt(1600))
## [1] 0.001349898
```

---

**Reference answer.**

![Ex 4.4 a–c answer](statistics/images/ex4/answers/ex4_4a1_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_4a1_question.png",
    "statistics/images/ex4/ex4_4_4a1_ai.png",
    "statistics/images/ex4/answers/ex4_4_4a1_answer.png",
],
}

ex4["4_4b"] = {
"title": "Ex 4.4 d–g — Sum of 5 calls, sample of 210, CLT for proportion",
"content": """**Question.** A mobile phone company estimated that the duration $X$ of a customer's call to the call-center follows $X\\sim N(180, 1600)$ (in seconds).

**d)** Let $S$ denote the total duration of 5 calls (random sample): probability that processing all the calls takes more than 15 minutes (900 sec)?
**e)** Suppose 210 calls come into the call center in a specific time slot on a business day; excluding 10% of the most extreme scenarios, what is the interval in which we can expect the total time required to process all calls to vary?
**f)** Suppose 600 calls come into the call center. Assume that 12% of calls are resolved without an intervention by the operator. What is the probability that at least 15% of these 600 calls will be resolved without operator intervention?
**g)** Discuss for each of the previous points (a–f) under what assumptions on $X$ the calculation is justified.

![Ex 4.4 d–g question](statistics/images/ex4/questions/ex4_4_4b_question.png)

---

**AI walkthrough.** All three numerical parts boil down to writing the right sampling distribution and reading a tail/quantile off the Normal. The key is whether normality is assumed for $X$ or earned via CLT.

1. **Part d — sum of $n=5$ normals.** Independence + normality give $S=\\sum_{i=1}^{5} X_i \\sim N(5\\mu,\\,5\\sigma^2) = N(900,\\,8000)$. The cut-off 900 s **is** the mean, so $\\Pr(S>900)=0.5$ exactly. No CLT needed — the answer relies on the input being Normal.
2. **Part e — sum of $n=210$ calls.** Same rule: $S\\sim N(210\\cdot 180,\\,210\\cdot 1600)=N(37800,\\,336000)$. With $n=210$ large, CLT does the heavy lifting — $X$ need *not* be Normal. The central 90% interval is just the 5-th and 95-th percentiles of this distribution: $[\\,q_{0.05},\\,q_{0.95}\\,] \\approx [36846.55,\\,38753.45]$ s ($\\approx 10.235$ to $10.765$ h).
3. **Part f — sample proportion.** Each call is Bernoulli($p=0.12$); the sample proportion of "no-operator" calls in $n=600$ trials is, by CLT for proportions, $\\bar P\\approx N\\!\\left(p,\\,\\frac{p(1-p)}{n}\\right)=N(0.12,\\,1.76\\times 10^{-4})$. Standardising the cut-off 0.15: $z=(0.15-0.12)/\\sqrt{0.12\\cdot 0.88/600}\\approx 2.26$, so $\\Pr(\\bar P>0.15)\\approx 0.0119$ — about a 1% chance.
4. **Part g — normality assumptions.** **a, b, c**: rely directly on $X$'s distribution, so Normality of $X$ is *essential*. **d**: small $n=5$, sum of normals — Normality still needed. **e**: $n=210$, CLT covers us — $X$ need not be Normal. **f**: only requires CLT for proportions ($n=600$ Bernoulli), so Normality of $X$ is irrelevant.
5. **Take-away.** Translate the question into the sampling distribution first; only then choose `pnorm` (tail) vs. `qnorm` (quantile). Whether you need an assumption on $X$ depends entirely on the **sample size** and on whether you sum the $X$'s or count Bernoulli successes.

![Ex 4.4 d–g AI walkthrough](statistics/images/ex4/ex4_4_4b_ai.png)

---

**Answer.**

**d)** $S = X_1 + \\ldots + X_5 \\sim N(5\\cdot 180,\\; 5\\cdot 1600) = N(900, 8000)$. Since 900 is exactly the mean (= median = mode), $\\Pr(S > 900) = 0.5$.

**e)** With $n = 210$, $S \\sim N(210\\cdot 180,\\; 210\\cdot 1600) = N(37800, 336000)$. The 90% central interval is bounded by the 5-th and 95-th percentiles of the distribution:
```r
qnorm(0.05, 37800, sqrt(336000))
## [1] 36846.55
qnorm(0.95, 37800, sqrt(336000))
## [1] 38753.45
```
We expect that the total time required to process all calls, excluding the 10% most extreme cases, will be between **36846.55** and **38753.45** seconds (≈ 10.235 and 10.765 hours).

**f)** The variable indicating whether a call is resolved without operator (success) follows $\\mathrm{Bernoulli}(0.12)$. With $n = 600$ calls, by CLT:

$$
\\bar P \\approx N\\!\\left(0.12,\\; \\frac{0.12(1-0.12)}{600}\\right).
$$

The probability that at least 15% of these calls will be resolved without an intervention, $\\Pr(\\bar P > 0.15)$:
```r
1 - pnorm(0.15, 0.12, sqrt((0.12*(1-0.12))/600))
## [1] 0.01186926
```

**g)** Points **a, b, c** rely directly on the distribution of $X$ — the assumption of normality is crucial there. Point **d** requires the assumption of normality (small sample $n = 5$, sum of normals). Point **e** involves a larger sample so by the CLT the distribution of $X$ need not be normal. Point **f** requires only that the proportion can be approximated by a Normal (CLT for proportions: only Bernoulli, not normal, is needed).

---

**Reference answer.**

![Ex 4.4 d–g answer](statistics/images/ex4/answers/ex4_4_4b_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_4b_question.png",
    "statistics/images/ex4/ex4_4_4b_ai.png",
    "statistics/images/ex4/answers/ex4_4_4b_answer.png",
],
}

# ========== EXERCISE 4.5 (take-away — X (order) + Y (filling), rho = 0.22) ==========

ex4["4_5"] = {
"title": "Ex 4.5 — Take-away: E[X+Y], Var(X+Y) with ρ = 0.22, and independence case",
"content": """**Question.** Consider the time $X$ required for a customer in line for a take-away to make an order and the subsequent time $Y$ required for the customer's order to be filled. Assume that $X$ and $Y$ have a linear correlation coefficient of $+0.22$ and that:

| | Expected value | Standard deviation |
|---|---|---|
| Variable $X$ | 3.5 minutes | 1.5 minutes |
| Variable $Y$ | 1.6 minutes | 0.2 minutes |

**a)** Calculate the expected value and the standard deviation of the time it takes for a customer to pick up his or her order.
**b)** Is it possible to calculate the probability that the time (the sum of point **a**) is more than 7 minutes? Under what assumptions?
**c)** Determine whether and how the previous results would change if $X$ and $Y$ were independent.

![Ex 4.5 question](statistics/images/ex4/questions/ex4_4_5_question.png)

---

**AI walkthrough.** This is the canonical "linear combination of correlated normals" template — three connected ideas, none of them more than one line of algebra each.

1. **Linearity of expectation is unconditional.** $E[X+Y]=E[X]+E[Y]=3.5+1.6=\\mathbf{5.1}$ minutes — true with *or* without independence, with *or* without normality. Always start here; it never depends on $\\rho$.
2. **Variance of a sum carries a covariance term.** $\\mathrm{Var}(X+Y)=\\sigma_X^2+\\sigma_Y^2+2\\,\\mathrm{Cov}(X,Y)$, with $\\mathrm{Cov}(X,Y)=\\rho\\,\\sigma_X\\sigma_Y=0.22\\cdot 1.5\\cdot 0.2=0.066$. Plug in: $\\mathrm{Var}(T)=2.25+0.04+2(0.066)=\\mathbf{2.422}$, so $\\sigma_T=\\sqrt{2.422}\\approx\\mathbf{1.5563}$. Panel (a) shows that $\\mathrm{Var}(X)$ dominates ($\\approx 93\\%$ of the total) but the covariance bump ($+0.132$) is still $\\approx 5.5\\%$ of $\\mathrm{Var}(T)$ — small, but not zero.
3. **Tail probabilities need a distributional assumption.** $\\mu_T$ and $\\sigma_T$ alone do *not* give $\\Pr(T>7)$. The standard add-on assumption is **bivariate normality** of $(X,Y)$: any linear combination of jointly normal r.v.s is itself normal, so $T\\sim N(5.1,\\,1.5563^2)$. Standardise: $z=(7-5.1)/1.5563\\approx 1.221$, hence $\\Pr(T>7)=\\Pr(Z>1.221)\\approx\\mathbf{0.1111}$. About **1 customer in 9** waits more than 7 minutes.
4. **Independence kills the covariance term only.** Set $\\rho=0$: $\\mathrm{Var}(T)=2.29$, $\\sigma_T\\approx 1.5133$ — the mean is unchanged. Under the same normality assumption, $\\Pr(T>7)\\approx\\mathbf{0.1046}$. Positive correlation *fattens* the right tail by $+0.0064$ ($\\approx +6\\%$ relative): when $X$ runs long, $Y$ tends to run long *too*, so extreme totals are slightly more likely. Panel (c) overlays the two densities — same centre, slightly wider when $\\rho>0$.
5. **Why a *positive* $\\rho$ makes intuitive sense here.** Busy moments at the counter lengthen both the ordering step *and* the filling step (more staff load, more queue), so the two times move together. A negative $\\rho$ would have shrunk $\\mathrm{Var}(T)$ below the independent baseline — a useful sanity check whenever the correlation flips sign.

![Ex 4.5 AI walkthrough](statistics/images/ex4/ex4_4_5_ai.png)

---

**Answer.**

**a)** Let $T = X + Y$. The expected value is

$$
E[T] = E[X] + E[Y] = 3.5 + 1.6 = 5.1 \\text{ minutes},
$$

and the variance is

$$
\\mathrm{Var}(T) = \\mathrm{Var}(X) + \\mathrm{Var}(Y) + 2\\,\\mathrm{Cov}(X, Y) = 1.5^2 + 0.2^2 + 2\\cdot 0.22\\cdot 1.5\\cdot 0.2 = 2.422 \\text{ minutes}^2.
$$

The standard deviation is $\\sqrt{2.422} \\approx \\mathbf{1.5563}$ minutes.

**b)** The probability that one customer has to wait more than 7 minutes can be computed only under the assumption that the joint distribution of $(X, Y)$ is a bivariate Normal distribution. Under this assumption, $X + Y \\sim N(5.1, 1.5563^2)$, and the probability $\\Pr(X + Y > 7)$ is:
```r
1 - pnorm(7, 5.1, 1.5563)
## [1] 0.1110725
```

**c)** If $X$ and $Y$ were independent the expected value of $X + Y$ would be the same, while the variance would be

$$
\\mathrm{Var}(X + Y) = \\mathrm{Var}(X) + \\mathrm{Var}(Y) = 1.5^2 + 0.2^2 = 2.29,
$$

leading to a standard deviation of $\\sqrt{2.29} \\approx 1.5133$. Under the assumption of independence (and joint normality), the probability $\\Pr(X + Y > 7)$ would be:
```r
1 - pnorm(7, 5.1, 1.5133)
## [1] 0.1045973
```

---

**Reference answer.**

![Ex 4.5 answer](statistics/images/ex4/answers/ex4_4_5_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_5_question.png",
    "statistics/images/ex4/ex4_4_5_ai.png",
    "statistics/images/ex4/answers/ex4_4_5_answer.png",
],
}

# ========== EXERCISE 4.6 (bus travel — find sigma from quartiles, then CLT) ==========

ex4["4_6"] = {
"title": "Ex 4.6 — Bus travel: find σ from quartiles, P(X>120), 30th pct, CLT, +15%",
"content": """**Question.** The time taken by a bus to travel its entire route on a working day without any special events is assumed to be normally distributed. Based on past experience, on 25% of days with less traffic the time taken is at most 80 minutes, while on 25% of days with more traffic it is at least 100 minutes. The two quartiles are therefore $q_1 = 80$ and $q_3 = 100$, the middle point between them being 90 minutes (the assumed mean).

**a1)** Find the expected value and the standard deviation of the time taken by the bus to travel its entire route (round intermediate results to 4 decimals).
**a2)** Probability that on a generic workday without special events the bus takes more than 2 hours, $\\Pr(X > 120)$.
**a3)** The travel time exceeded on 70% of days (30-th percentile).
**a4)** Travel time is monitored on 45 days (simple random sample). What is the probability that the bus takes less than 82 minutes on at least 10% of the days?
**b)** In case of special events that cause an increase in the level of traffic, the travel time increases by 15%, so $Y = 1.15\\,X$. Redo parts **a2–a4** under this scenario.

![Ex 4.6 question](statistics/images/ex4/questions/ex4_4_6_question.png)

---

**AI walkthrough.** All five sub-parts ride on the *same* Normal $X\\sim N(90,\\sigma^2)$ — only $\\sigma$ has to be calibrated first, then everything follows mechanically.

1. **Anchor the mean.** The "middle point" between the two quartiles is the mean by symmetry of a Normal: $\\mu = (80 + 100)/2 = 90$.
2. **Calibrate $\\sigma$ from a quartile.** $\\Pr(X<80)=0.25$ means $(80-90)/\\sigma=\\Phi^{-1}(0.25)\\approx -0.6745$, hence $\\sigma=10/0.6745\\approx \\mathbf{14.826}$. The same number falls out of $q_3=100$ by symmetry — useful as a sanity check.
3. **Part a2 — right tail at 120 min.** Standardise: $z=(120-90)/14.826\\approx 2.024$. Two SDs into the right tail $\\Rightarrow\\Pr(X>120)\\approx \\mathbf{0.0215}$ — buses *rarely* take more than 2 hours on a normal day.
4. **Part a3 — "exceeded on 70% of days" = 30-th percentile.** Invert the CDF at 0.30: $q_{0.30}=90+\\Phi^{-1}(0.30)\\cdot 14.826\\approx 90-0.5244\\cdot 14.826\\approx \\mathbf{82.25}$ min.
5. **Part a4 — CLT for a proportion.** $p=\\Pr(X<82)\\approx 0.2947$. For a 45-day sample, $\\bar P\\approx N(p,\\,p(1-p)/n)$ with SE $=\\sqrt{0.2947\\cdot 0.7053/45}\\approx 0.068$. The threshold 0.10 sits $\\approx (0.10-0.2947)/0.068\\approx -2.87$ SDs *below* the mean of $\\bar P$ — so $\\Pr(\\bar P>0.10)\\approx \\mathbf{0.998}$. Almost certainly the proportion exceeds 10%.
6. **Part b — multiply by 1.15.** $Y=1.15X$ is still Normal: $E[Y]=103.5$, $\\sigma_Y=1.15\\cdot 14.826\\approx 17.05$. The right tail shifts visibly: $\\Pr(Y>120)\\approx \\mathbf{0.1666}$ — about **8× larger** than the baseline 0.0215. Similarly $q_{0.30}(Y)\\approx 94.56$ and $\\Pr(Y<82)\\approx 0.1037$.
7. **Take-away.** A linear scaling $Y=aX$ moves *both* $\\mu$ and $\\sigma$ by the factor $a$, which is why a "small" 15% traffic shock blows up the probability of taking >2 hours from 2% to 17%. The CLT step is independent of the scaling — only $p$ and $n$ matter for the sampling distribution of $\\bar P$.

![Ex 4.6 AI walkthrough](statistics/images/ex4/ex4_4_6_ai.png)

---

**Answer.**

**a1)** Between a generic normal $N(\\mu, \\sigma^2)$ and the standard normal, $(X - 90)/\\sigma$ has a standard normal distribution. Since $\\Pr(X < 80) = 0.25$, the value $(80 - 90)/\\sigma$ is the first quartile of a standard normal:
```r
qnorm(0.25)
## [1] -0.6744898
```
So $(80 - 90)/\\sigma = -0.6745$, leading to $\\sigma = 14.826$. Therefore $X \\sim N(90, 14.826^2)$.

**a2)** $\\Pr(X > 120)$:
```r
1 - pnorm(120, 90, 14.826)
## [1] 0.02151224
```

**a3)** 30-th percentile:
```r
qnorm(0.3, 90, 14.826)
## [1] 82.2524
```

**a4)** Bernoulli with $p = \\Pr(X < 82)$:
```r
pnorm(82, 90, 14.826)
## [1] 0.294739
```

The proportion of buses over 45 taking less than 82 minutes, $\\bar P$, has a distribution approximated by the Normal via the CLT:

$$
\\bar P \\approx N\\!\\left(p,\\; \\frac{p(1-p)}{n}\\right) = N\\!\\left(0.295,\\; \\frac{0.295(1 - 0.295)}{45}\\right).
$$

Probability that the bus takes less than 82 minutes on at least 10% of the days, $\\Pr(\\bar P > 0.10)$:
```r
1 - pnorm(0.10, 0.295, sqrt((0.295*(1-0.295))/45))
## [1] 0.9979167
```

**b)** Let $Y = 1.15\\,X$ denote the travel time in case of special events. Then

$$
E[Y] = 1.15\\cdot 90 = 103.5, \\qquad \\mathrm{Var}(Y) = 1.15^2\\cdot 14.826^2 = 289.68,
$$

so $Y \\sim N(103.5,\\; (1.15\\cdot 14.826)^2)$.

Probability of taking more than 2 hours, $\\Pr(Y > 120)$:
```r
1 - pnorm(120, 103.5, 1.15*14.826)
## [1] 0.1665852
```

Travel time exceeded on 70% of days (30-th percentile):
```r
qnorm(0.3, 1.15*90, 1.15*14.826)
## [1] 94.55902
```

Probability of travel time below 82 minutes:
```r
pnorm(82, 1.15*90, 1.15*14.826)
## [1] 0.1036536
```

---

**Reference answer.**

![Ex 4.6 answer](statistics/images/ex4/answers/ex4_4_6_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_6_question.png",
    "statistics/images/ex4/ex4_4_6_ai.png",
    "statistics/images/ex4/answers/ex4_4_6_answer.png",
],
}

# ========== EXERCISE 4.7 (airport: B + D, bivariate normal) ==========

ex4["4_7"] = {
"title": "Ex 4.7 — Airport security: Y = B + D, P(Y ≥ 45), arrival time",
"content": """**Question.** At an airport, the distributions of times (in minutes) required for hand baggage screening ($B$) and document screening ($D$) in a workday are jointly normal. It is further assumed that $B \\sim N(10, 4)$ and $D \\sim N(3, 1)$, and that the linear correlation coefficient between $B$ and $D$ is $\\rho = 0.4375$.

**a)** What is the distribution of the time required to pass both checks ($Y = B + D$)?
**b)** Your flight leaves at 5 p.m. and you want to be done with security at least 45 minutes before your flight. At what time should you show up so that the probability of getting through security in the desired time is at least 90%?

![Ex 4.7 question](statistics/images/ex4/questions/ex4_4_7_question.png)

---

**AI walkthrough.** Two independent ingredients: (i) the **distribution of $Y$** (a linear combination of jointly normal r.v.s is normal — only mean and variance to compute), and (ii) the **90-th percentile** (how long security can take on the worst 10% of days).

1. **Mean of $Y$.** Linearity of expectation: $E[Y]=E[B]+E[D]=10+3=13$ min.
2. **Variance of $Y$.** Variance of a sum is *not* just the sum of variances when correlated — add $2\\,\\mathrm{Cov}(B,D)$:
$$\\mathrm{Cov}(B,D)=\\rho\\,\\sigma_B\\,\\sigma_D = 0.4375\\cdot 2\\cdot 1 = 0.875,\\qquad \\mathrm{Var}(Y)=4+1+2(0.875)=6.75.$$
So $\\sigma_Y=\\sqrt{6.75}\\approx 2.598$ min.
3. **Why normal?** Jointly normal $\\Rightarrow$ any linear combination is normal — no CLT needed, no sample size involved. $Y\\sim N(13,\\,6.75)$.
4. **90-th percentile of $Y$.** We want $q$ with $P(Y\\le q)=0.90$, i.e. plan for "all but the worst 10% of days":
$$q_{0.90}= 13 + z_{0.90}\\cdot\\sqrt{6.75} = 13 + 1.2816\\cdot 2.598 \\approx 16.33 \\text{ min}.$$
5. **From "security time" to "arrival time".** You want to be **done** with security $\\geq 45$ min before takeoff. Allow $q_{0.90}\\approx 16.33$ min for the security queue itself, then a $45$-min buffer: total $\\approx 61.33$ min before 17:00, i.e. arrival at **15:59**.

![Ex 4.7 AI walkthrough](statistics/images/ex4/ex4_4_7_ai.png)

---

**Answer.**

**a)** Let $Y = B + D$. Then

$$
E[Y] = E[B] + E[D] = 10 + 3 = 13,
$$

$$
\\mathrm{Var}(Y) = \\mathrm{Var}(B) + \\mathrm{Var}(D) + 2\\,\\mathrm{Cov}(B, D) = 4 + 1 + 2\\cdot 0.4375\\cdot 2\\cdot 1 = 6.75.
$$

(Note: $\\mathrm{Cov}(B, D) = \\rho_{B,D}\\,\\sigma_B\\,\\sigma_D = 0.4375 \\cdot 2 \\cdot 1 = 0.875$.) Since the joint distribution is bivariate normal, $Y$ is itself normal: $Y \\sim N(13,\\; 6.75)$.

**b)** We need the 90-th percentile of $Y$, corresponding to the 10% longest times required to pass both checks:
```r
qnorm(0.9, 13, sqrt(6.75))
## [1] 16.32957
```

So you need to allow about **16.33 minutes** for security. Adding 45 minutes for buffer, you should be at the airport at least $45 + 16.33 \\approx 61.33$ minutes before the flight. If the flight leaves at 5:00 p.m., you should arrive at around **3:59 p.m.**

---

**Reference answer.**

![Ex 4.7 answer](statistics/images/ex4/answers/ex4_4_7_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_7_question.png",
    "statistics/images/ex4/ex4_4_7_ai.png",
    "statistics/images/ex4/answers/ex4_4_7_answer.png",
],
}

# ========== EXERCISE 4.8 (bivariate normal G = 0.4X + 0.6Y) ==========

ex4["4_8a"] = {
"title": "Ex 4.8a — Final grade G = 0.4X + 0.6Y: E[G], Var[G], 50% central range, Pr(prop>0.5), sample-mean",
"content": """**Question.** The final grade in a college course is $G = 0.4X + 0.6Y$, where $X$ is the individual project (40%) and $Y$ is the written test (60%). From past cohorts taking the exam at the last session: $E[X] = 26.3$, $\\mathrm{Var}[X] = 22.6$, $E[Y] = 24.2$, $\\mathrm{Var}[Y] = 28.6$, $\\mathrm{Cov}(X, Y) = 22.3$. Performances of different students are independent.

**a)** Mean and variance of $G$ for a student taking the exam in the last session.
**b)** Can the range of the 50% central grades be determined? If not, additional assumption.
**c)** If 200 students show up, estimate $\\Pr(\\text{proportion of students with } G > 24 \\text{ is} > 50\\%)$.
**d)** Probability that the mean grade of the 200 students is greater than 24.

![Ex 4.8a question](statistics/images/ex4/questions/ex4_4_8a_question.png)

---

**AI walkthrough.** Four sub-parts that all hinge on the *same* linear combination $G=0.4X+0.6Y$ — the only twist is that $X$ and $Y$ are *correlated*, so the covariance term is non-zero, and that everything past **a)** needs the bivariate-normal assumption to turn moments into probabilities.

1. **Mean of $G$ (linearity).** $E[G]=0.4\\cdot 26.3+0.6\\cdot 24.2=\\mathbf{25.04}$. Linearity does *not* care about correlation.
2. **Variance of $G$ (do not drop the cross term).** With $a=0.4$, $b=0.6$: $\\mathrm{Var}(G)=a^2\\mathrm{Var}(X)+b^2\\mathrm{Var}(Y)+2ab\\,\\mathrm{Cov}(X,Y)=0.16\\cdot22.6+0.36\\cdot28.6+0.48\\cdot22.3=\\mathbf{24.616}$. If you forgot $2ab\\,\\mathrm{Cov}$ you would get only 13.912 — visibly too small.
3. **Why bivariate normal for (b)–(d)?** Only the *first two moments* of $G$ are pinned down so far. To convert "$E[G],\\mathrm{Var}(G)$" into quartiles or tail probabilities you need a shape. The cleanest extra assumption is $(X,Y)$ jointly normal — then every linear combination is normal, so $G\\sim N(25.04,\\,24.616)$.
4. **Part b — central 50% = quartile range.** $q_{0.25}=25.04+z_{0.25}\\sqrt{24.616}\\approx 21.69$; $q_{0.75}=25.04+z_{0.75}\\sqrt{24.616}\\approx 28.39$. Symmetric around the mean by Normality.
5. **Part c — proportion of passers via CLT.** First $p=\\Pr(G>24)$: $z=(24-25.04)/\\sqrt{24.616}\\approx -0.21$, so $p\\approx \\mathbf{0.583}$. Each student is an *iid* Bernoulli($p$), so $\\bar P\\approx N(p,\\,p(1-p)/200)$ with SE $\\approx 0.0349$. The threshold 0.50 sits $(0.50-0.583)/0.0349\\approx -2.38$ SDs below the mean of $\\bar P$ $\\Rightarrow \\Pr(\\bar P>0.5)\\approx \\mathbf{0.991}$.
6. **Part d — sample-mean CLT.** $\\bar G\\approx N(25.04,\\,24.616/200)$; SE $\\approx 0.351$. The threshold 24 is $(24-25.04)/0.351\\approx -2.96$ SDs below $\\mu$, well in the left tail of $\\bar G$ $\\Rightarrow \\Pr(\\bar G>24)\\approx \\mathbf{0.998}$.
7. **Take-away.** The variance dispersion for *one* student ($\\sigma_G\\approx 4.96$) shrinks to $\\sigma_G/\\sqrt{200}\\approx 0.35$ for the *mean of 200* — a 14× collapse. That is why (d) is essentially a certainty while (b) for a single student spans a 6.7-point quartile range.

![Ex 4.8a AI walkthrough](statistics/images/ex4/ex4_4_8a_ai.png)

---

**Answer.**

**a)** Using linearity of expectation and the variance of a linear combination:

$$
E[G] = 0.4\\cdot E[X] + 0.6\\cdot E[Y] = 0.4\\cdot 26.3 + 0.6\\cdot 24.2 = 25.04.
$$

$$
\\mathrm{Var}[G] = 0.4^2 \\mathrm{Var}[X] + 0.6^2 \\mathrm{Var}[Y] + 2\\cdot 0.4\\cdot 0.6\\cdot \\mathrm{Cov}(X, Y) = 0.16\\cdot 22.6 + 0.36\\cdot 28.6 + 0.48\\cdot 22.3 = 24.616.
$$

**b)** The 50% central range can be determined only under the assumption that $(X, Y)$ is jointly bivariate normal — then $G \\sim N(25.04, 24.616)$ and the central 50% lies between the 1st and 3rd quartile:
```r
qnorm(0.25, 25.04, sqrt(24.616))
## [1] 21.69355
qnorm(0.75, 25.04, sqrt(24.616))
## [1] 28.38645
```

**c)** The variable indicating whether a generic student gets $G > 24$ is Bernoulli with parameter $p = \\Pr(G > 24)$. Assuming bivariate normality of $(X, Y)$:
```r
1 - pnorm(24, mean=25.04, sd=sqrt(24.616))
## [1] 0.5830163
```

The proportion of students with $G > 24$, $\\bar P$, has by the CLT a Normal approximation:

$$
\\bar P \\approx N\\!\\left(p,\\, p(1-p)/n\\right) = N\\!\\left(0.583,\\, 0.583(1-0.583)/200\\right).
$$

Therefore $\\Pr(\\bar P > 0.50)$ is:
```r
1 - pnorm(0.5, mean=0.583, sd=sqrt((0.583*(1-0.583))/200))
## [1] 0.9913583
```

**d)** The mean final grade of the 200 students $\\bar G$ has $E[\\bar G] = E[G] = 25.04$ and $\\mathrm{Var}(\\bar G) = \\mathrm{Var}(G)/n = 24.616/200$. By the CLT $\\bar G \\approx N(25.04, 24.616/200)$:
```r
1 - pnorm(24, mean=25.04, sd=sqrt(24.616/200))
## [1] 0.9984837
```

---

**Reference answer.**

![Ex 4.8a answer](statistics/images/ex4/answers/ex4_4_8a_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_8a_question.png",
    "statistics/images/ex4/ex4_4_8a_ai.png",
    "statistics/images/ex4/answers/ex4_4_8a_answer.png",
],
}

# ========== EXERCISE 4.9 (pizzeria winter — Sales sampling distribution) ==========

ex4["4_9a"] = {
"title": "Ex 4.9a — P(sample mean Sales < 22500) — pizzeria sampling, n = 81",
"content": """**Question.**

![Ex 4.9a question](images/ex4/questions/ex4_4_9a_question.png)

---

**Answer.**

![Ex 4.9a answer](images/ex4/answers/ex4_4_9a_answer.png)

---

**AI explanation.** By the **Central Limit Theorem**, for $n = 81 \\geq 30$ the distribution of the sample mean is approximately Normal regardless of the population distribution of turnover:

$$\\bar X \\;\\sim\\; \\mathcal{N}\\!\\left(\\mu,\\,\\tfrac{\\sigma^{2}}{n}\\right) \\;=\\; \\mathcal{N}\\!\\left(25{,}000,\\;\\tfrac{9800^{2}}{81}\\right).$$

The **standard error** of the mean is $\\mathrm{SE} = \\sigma/\\sqrt{n} = 9800/9 \\approx 1088.9$ €. Standardising the threshold:

$$z \\;=\\; \\frac{22500 - 25000}{1088.9} \\;\\approx\\; -2.296,$$

so $\\Pr(\\bar X < 22500) = \\Phi(-2.296) \\approx 0.0108$. The plot below shows the sampling density of $\\bar X$ with the shaded left tail equal to the answer.

![Ex 4.9a AI plot](images/ex4/ex4_4_9a_ai.png)

**Key intuition.** The population SD is 9,800 € but the SD of the *mean of 81 draws* is only $\\approx 1089$ € — the $\\sqrt{n}$ shrinkage of dispersion is what makes a sample mean as low as 22,500 € a $\\sim 2.3\\sigma$ event (probability $\\approx 1\\%$).

```r
pnorm(22500, 25000, sqrt(9800^2/81))
## [1] 0.01083864
```
""",
"images": [
    "images/ex4/questions/ex4_4_9a_question.png",
    "images/ex4/answers/ex4_4_9a_answer.png",
    "images/ex4/ex4_4_9a_ai.png",
],
}

ex4["4_9b"] = {
"title": "Ex 4.9b — P(sample mean > 24800) — pizzeria sampling, n = 60",
"content": """**Question.**

![Ex 4.9b question](images/ex4/questions/ex4_4_9b_question.png)

---

**Answer.**

![Ex 4.9b answer](images/ex4/answers/ex4_4_9b_answer.png)

---

**AI explanation.** With $n = 60 \\geq 30$ the **Central Limit Theorem** applies regardless of the population distribution of turnover:

$$\\bar X \\;\\sim\\; \\mathcal{N}\\!\\left(\\mu,\\,\\tfrac{\\sigma^{2}}{n}\\right) \\;=\\; \\mathcal{N}\\!\\left(25{,}000,\\;\\tfrac{9800^{2}}{60}\\right).$$

The **standard error** of the mean is $\\mathrm{SE} = \\sigma/\\sqrt{n} = 9800/\\sqrt{60} \\approx 1265.1$ €. Standardising the threshold:

$$z \\;=\\; \\frac{24800 - 25000}{1265.1} \\;\\approx\\; -0.158,$$

so $\\Pr(\\bar X > 24800) = 1 - \\Phi(-0.158) = \\Phi(0.158) \\approx 0.5628$. The shaded right tail in the plot below visualises this probability.

![Ex 4.9b AI plot](images/ex4/ex4_4_9b_ai.png)

**Key intuition.** The threshold 24,800 € sits only $0.16\\,\\mathrm{SE}$ below the mean — barely off-centre — so beating it has probability "a hair above one-half". The $\\sqrt{n}$ shrinkage takes the SD from 9,800 € (population) down to $\\approx 1265$ € (sampling), which is why a 200 € shortfall in the threshold maps to such a small $z$.

```r
1 - pnorm(24800, 25000, sqrt(9800^2/60))
## [1] 0.5628035
```
""",
"images": [
    "images/ex4/questions/ex4_4_9b_question.png",
    "images/ex4/answers/ex4_4_9b_answer.png",
    "images/ex4/ex4_4_9b_ai.png",
],
}

ex4["4_9c"] = {
"title": "Ex 4.9c — P(23000 < sample mean < 27000), n = 20; d1/d2 with n = 100",
"content": """**Question.**

![Ex 4.9c question](images/ex4/questions/ex4_4_9c_question.png)

---

**Answer.**

![Ex 4.9c answer](images/ex4/answers/ex4_4_9c_answer.png)

---

**AI explanation.** With $n=20$ the CLT is too weak (small sample), so we assume the population turnover is **Normal**. Then the sampling distribution is exact:

$$\\bar X \\;\\sim\\; \\mathcal{N}\\!\\left(25000,\\,\\tfrac{9800^{2}}{20}\\right), \\qquad \\mathrm{SE}=\\tfrac{9800}{\\sqrt{20}}\\approx 2191.3.$$

Standardising $23000$ and $27000$ gives $z=\\mp\\,2000/2191.3 \\approx \\mp\\,0.9127$, so

$$\\Pr(23000<\\bar X<27000)=\\Phi(0.913)-\\Phi(-0.913)\\approx 0.6386.$$

If $n=10$ instead, $\\mathrm{SE}$ rises to $9800/\\sqrt{10}\\approx 3099$ — the density spreads out and the same $\\pm 2000$ window captures only $\\approx 0.4813$. **Lower probability for $n=10$**.

**d1)** With $n=100$ the CLT applies (no Normality assumption). $\\bar X \\sim N(25000,\\,9800^{2}/100)$, $\\mathrm{SE}=980$. With $\\bar x_{\\text{obs}}=23{,}947$:

$$z=\\frac{23947-25000}{980}\\approx -1.074, \\qquad \\Pr(\\bar X>23947)=1-\\Phi(-1.074)\\approx 0.8587.$$

So $\\approx 85.87\\%$ of size-100 samples have a mean greater than the one observed in `pizzerie`.

**d2)** The 1%-extreme region uses $q_{0.01}$ and $q_{0.99}$ of $N(25000,980^{2})$:

$$q_{0.01}\\approx 25000-2.326\\cdot 980\\approx 22{,}720, \\qquad q_{0.99}\\approx 25000+2.326\\cdot 980\\approx 27{,}280.$$

A sample is "1% extreme" if its mean is **below 22,720** or **above 27,280** Euro.

![Ex 4.9c AI plot](images/ex4/ex4_4_9c_ai.png)

**Key intuition.** The standard error $\\sigma/\\sqrt{n}$ controls everything: going from $n=10\\to 20\\to 100$ shrinks $\\mathrm{SE}$ from $\\approx 3099\\to 2191\\to 980$. Narrower sampling distributions concentrate more mass on any fixed interval around $\\mu$ (part c) and push the $1\\%/99\\%$ tails closer to $\\mu$ (part d2). The observed $\\bar x_{\\text{obs}}=23{,}947$ is only $\\approx 1.07$ SE below 25,000 — unremarkable under $H_0$.

```r
pnorm(27000, 25000, sqrt(9800^2/20)) - pnorm(23000, 25000, sqrt(9800^2/20))
## [1] 0.6385896
pnorm(27000, 25000, sqrt(9800^2/10)) - pnorm(23000, 25000, sqrt(9800^2/10))
## [1] 0.4813076
distr.summary.x(Sales, stats=c("mean","sd"), data=pizzerie)
##  n n.a  mean   sd
## 100 0  23946.99  9538.62
1 - pnorm(23947, 25000, sqrt(9800^2/100))
## [1] 0.8586984
qnorm(0.01, 25000, sqrt(9800^2/100))
## [1] 22720.18
qnorm(0.99, 25000, sqrt(9800^2/100))
## [1] 27279.82
```
""",
"images": [
    "images/ex4/questions/ex4_4_9c_question.png",
    "images/ex4/answers/ex4_4_9c_answer.png",
    "images/ex4/ex4_4_9c_ai.png",
],
}

# ========== EXERCISE 4.10 (AmountSpent in DS — CLT) ==========

ex4["4_10a"] = {
"title": "Ex 4.10a — Sample mean of AmountSpent (CLT, n=750)",
"content": """**Question.** Assume that the amount spent by all customers (population) of the company considered in the **DS** dataframe (where information on one sample only is available) has mean 1000 and variance 810000, respectively. Consider a sample of size 750 drawn from the population.

**a1)** Under what assumptions can you determine what is the distribution of the sample mean of the amount spent?

**a2)** What is the minimum value reached by the largest 5% of values of the sample mean? And what is the range of values that contains 95% of the "central" values of the distribution (of the sample mean)? Does the mean amount detected on the sample in the **DS** dataframe (variable *AmountSpent*) belong to that range?

**a3)** What is the percentage of samples of size 750 for which mean turnover would be observed to be greater than that found on customers in the **DS** dataframe?

![Ex 4.10a question](images/ex4/questions/ex4_4_10a_question.png)

---

**Answer.**

**a1)** Since the sample is large enough, by the Central Limit Theorem no assumptions on the distribution of the amount spent in the population are needed and the distribution of the mean of a sample of size 750 can be approximated by a Normal distribution, $\\bar X \\sim N(1000, 810000/750)$.

**a2)** The minimum value reached by the 5% of the largest sample means is the 95-th percentile, $p_{95}$, of the distribution of the sample mean:
```r
qnorm(0.95, mean=1000, sd=sqrt(810000/750))
## [1] 1054.055
```

The range of values including the 95% of the "central" values of the distribution (of the sample mean) is the interval between the values $x_{0.025}$ and $x_{0.975}$ such that $\\Pr(\\bar X > x_{0.975}) = 0.975$ and $\\Pr(\\bar X < x_{0.025}) = 0.025$:
```r
qnorm(0.025, mean=1000, sd=sqrt(810000/750))
## [1] 935.589
qnorm(0.975, mean=1000, sd=sqrt(810000/750))
## [1] 1064.411
```

The mean amount in the sample considered in the dataframe **DS** is 1228.4 and does not belong to that range:
```r
mean(DS$AmountSpent)
## [1] 1228.437
```

Thus, the **available sample** has an extremely high mean, being higher than the value exceeded by 2.5% of all the means of samples of size 750.

**a3)** The percentage of samples of size 750 with a mean of the amount spent greater than 1228.4 is $100\\cdot\\Pr(\\bar X > 1228.4)$ and is approximately 0:
```r
1 - pnorm(1228.437, mean=1000, sd=sqrt(810000/750))
## [1] 1.831995e-12
```

**Walkthrough.** With $n=750$ the CLT gives a tight Normal for $\\bar X$ with standard error $\\sigma_{\\bar X}=\\sqrt{810000/750}\\approx 32.86$. Panel 1 shows the central 95% band $[935.6,\\, 1064.4]$ (yellow) and the upper 5% tail beyond $p_{95}=1054.1$ (red). Panel 2 zooms out to place the observed $\\bar x_n=1228.4$ — it sits about $6.95$ SE above $\\mu=1000$, far beyond the 97.5% quantile, which is why $\\Pr(\\bar X>1228.4)\\approx 1.8\\!\\times\\!10^{-12}$.

![Ex 4.10a AI walkthrough](images/ex4/ex4_4_10a_ai.png)

---

**Reference answer (textbook).**

![Ex 4.10a answer](images/ex4/answers/ex4_4_10a_answer.png)
""",
"images": [
    "images/ex4/questions/ex4_4_10a_question.png",
    "images/ex4/ex4_4_10a_ai.png",
    "images/ex4/answers/ex4_4_10a_answer.png",
],
}

ex4["4_10b"] = {
"title": "Ex 4.10b — Sample proportion of customers living near a store (CLT for proportions)",
"content": """**Question.** Assume that the proportion of customers living near a store that sells items similar to those of the firm is known and equal to $0.69$. Assume that a sample of size 750 is randomly drawn from the population of customers, and that the sample proportion of customers living near a store (that sells items similar to those of the firm) is computed.

**b1)** What is the probability that the sample proportion of the (generic) sample is higher than that observed in the sample in the **DS** dataframe (note that the *Location* variable captures whether or not a customer lives near a store)?

**b2)** What is the interval that includes 50% of the less extreme values for the sample proportion if we consider samples of size 750?

**b3)** Consider a sample of size 750 characterized by a sample proportion of $0.72$. What is the probability of extracting samples from the population characterized by a higher sample proportion? Would the probability change if the same proportion were observed in a sample of size 1000? Why?

---

**Answer.** By the CLT, $\\bar P \\approx N(0.69,\\; 0.69(1-0.69)/750)$.

**b1)** The proportion observed in the sample in the dataframe **DS** is $0.704$:
```r
distr.table.x(Location, freq = "prop", data=DS)
##  Close   Far
##  0.704 0.296
```

Therefore, the probability that the sample proportion of a (generic) sample of size 750 is higher than that observed, $\\Pr(\\bar P > 0.704)$, is:
```r
1 - pnorm(0.704, mean=0.69, sd=sqrt((0.69*(1-0.69))/750))
## [1] 0.2035329
```

**b2)** The interval that includes 50% of the less extreme values for the sample proportion if we consider samples of size 750 is the interval between the first and third quartile of the distribution:
```r
qnorm(0.25, mean=0.69, sd=sqrt((0.69*(1-0.69))/750))
## [1] 0.6786093
qnorm(0.75, mean=0.69, sd=sqrt((0.69*(1-0.69))/750))
## [1] 0.7013907
```

**b3)** The probability of extracting samples of size 750 characterized by a sample proportion greater than $0.72$, $\\Pr(\\bar P > 0.72)$, is:
```r
1 - pnorm(0.72, mean=0.69, sd=sqrt((0.69*(1-0.69))/750))
## [1] 0.03783158
```

The probability would be **lower** if the same proportion were observed in a sample of size 1000, because the distribution of the sample proportion would be more concentrated around the centre due to a lower standard error ($0.0146$ vs $0.0169$):
```r
sqrt(0.69*0.31/750)
## [1] 0.01688787
sqrt(0.69*0.31/1000)
## [1] 0.01462532
```
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_10b_question.png",
    "statistics/images/ex4/ex4_4_10b_ai.png",
    "statistics/images/ex4/answers/ex4_4_10b_answer.png",
],
}

# ========== EXERCISE 4.11 (ad cost — linear in X) ==========

ex4["4_11a"] = {
"title": "Ex 4.11 — Mobile phone contracts: expected daily cost & 90th percentile",
"content": """**Question.** A user needs to decide which mobile phone contract to subscribe, and she/he is considering a contract with call charges of $25$ cents per minute and $9$ cents per connection charge.

**a)** Assume that this user makes about $10$ phone calls per day, with a mean duration of $4$ minutes. What is the expected daily cost? Would an alternative offer at $49$ cents per minute with no connection charge be convenient in terms of expected daily cost?

**b)** Which offer without connection charge would you have to evaluate for the expected daily cost to be lower than that with connection charge?

**c)** Assume now that the user makes about $10$ phone calls per day, again with a mean duration of $4$ minutes, but assume that the distribution of call duration is normal with variance $1$, and that the lengths of the calls are independent. What is the minimum daily cost that the user can expect to exceed in $90\\%$ of the days under the two offers' scenarios?

![Ex 4.11 question](statistics/images/ex4/questions/ex4_4_11a_question.png)

---

**AI walkthrough.** Three sub-parts, all turning on the same linear-cost identity $\\text{cost} = (\\text{rate})\\cdot(\\text{minutes}) + (\\text{connection fee})\\cdot(\\text{calls})$. Parts **a)–b)** only need linearity of expectation; **c)** upgrades the picture to *distributions* via independence + Normality.

1. **a) Expected cost = swap r.v.s for their means.** With offer 1, daily cost is $X = 25\\cdot T + 9\\cdot K$ where $T=$ total minutes and $K=$ number of calls. Plug $E[T]=10\\cdot 4=40$, $E[K]=10$: $E[X] = 25\\cdot 40 + 9\\cdot 10 = 1000 + 90 = \\mathbf{1090}$ cents. Offer 2 has no connection fee: $E[Y] = 49\\cdot 40 = \\mathbf{1960}$ cents. Offer 2 costs $\\approx 80\\%$ more on average — **not convenient**.
2. **b) Break-even per-minute rate.** Solve $10\\cdot 4\\cdot x < 1090 \\Rightarrow x < 1090/40 = \\mathbf{27.25}$ c/min. Any "no-connection" offer above 27.25 c/min is strictly worse than offer 1 on expected cost.
3. **c) From means to distributions.** Each call duration $T_i \\sim N(4,1)$ iid. With $10$ calls fixed, daily cost is a *sum of iid normals*, so itself normal:
   - Offer 1: $S_1 = 25\\sum T_i + 90$. Mean $= 25\\cdot 40 + 90 = 1090$. Variance $= 25^2\\cdot \\mathrm{Var}(\\sum T_i) = 625\\cdot 10 = \\mathbf{6250}$. The $+90$ is a constant — adds to the mean, **not** to the variance.
   - Offer 2: $S_2 = 49\\sum T_i$. Mean $= 49\\cdot 40 = 1960$. Variance $= 49^2\\cdot 10 = \\mathbf{24010}$. The higher rate amplifies *both* mean and variance.
4. **"Exceed in 90% of days" = 10th percentile.** The question asks the cost the user *beats* on 90% of days, i.e. the lower-tail threshold $q$ with $P(S \\le q) = 0.10$. Standardising:
$$q_{0.10} = \\mu + z_{0.10}\\,\\sigma, \\quad z_{0.10} = -1.2816.$$
For offer 1: $\\sigma_1 = \\sqrt{6250} \\approx 79.06$, so $q_{0.10} \\approx 1090 - 1.2816\\cdot 79.06 \\approx \\mathbf{988.7}$ c.
For offer 2: $\\sigma_2 = \\sqrt{24010} \\approx 154.95$, so $q_{0.10} \\approx 1960 - 1.2816\\cdot 154.95 \\approx \\mathbf{1761.4}$ c.
5. **Sanity check — the gap widens.** Mean difference is $1960 - 1090 = 870$ c; 10th-percentile difference is $1761.4 - 988.7 = 772.7$ c. Offer 2 is still strictly more expensive even on its *best* 10%-tail days — consistent with **a)**.

![Ex 4.11 AI walkthrough](statistics/images/ex4/ex4_4_11a_ai.png)

---

**Answer.**

**a)** Let $X$ denote the daily cost with the first option for a user that makes about $10$ phone calls per day. The expected daily cost is

$$E(X) = 10\\cdot 25\\cdot 4 + 10\\cdot 9 = 1090 \\text{ cents}.$$

Let $Y$ denote the daily cost with the second option for a user that makes about $10$ phone calls per day. The expected daily cost is

$$E(Y) = 10\\cdot 4\\cdot 49 = 1960 \\text{ cents}.$$

The alternative offer is **not convenient**.

**b)** For the offer without connection charge to be more convenient than the first offer, the cost per minute must be $x$ such that $10\\cdot 4\\cdot x < 1090$, therefore $x = 1090/40 = 27.25$ cents.

**c)** If the distribution of call duration is $N(4,1)$, then by independence

$$S_1 \\sim N\\!\\left(10\\cdot 25\\cdot 4 + 10\\cdot 9,\\; 10\\cdot 25^2\\right) = N(1090,\\, 6250),$$

$$S_2 \\sim N\\!\\left(10\\cdot 49\\cdot 4,\\; 10\\cdot 49^2\\right) = N(1960,\\, 24010),$$

where $S_1$ is the total cost of $10$ calls with the first offer and $S_2$ is the total cost of $10$ calls with the second offer (note: each per-call cost has standard deviation equal to the per-minute rate $\\times$ SD of duration $= 25\\cdot 1$ and $49\\cdot 1$ cents, so the sum of $10$ iid calls has variance $10\\cdot 25^2$ and $10\\cdot 49^2$).

The minimum daily costs that the user can expect to exceed with probability $0.9$ under the two offers' scenarios are respectively $\\approx 989$ and $\\approx 1761$ cents (the $10$-th percentile of each distribution):
```r
qnorm(0.1, 1090, sqrt(10)*25)
## [1] 988.6845
qnorm(0.1, 1960, sqrt(10)*49)
## [1] 1761.422
```

---

**Reference answer.**

![Ex 4.11 answer](statistics/images/ex4/answers/ex4_4_11a_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_11a_question.png",
    "statistics/images/ex4/ex4_4_11a_ai.png",
    "statistics/images/ex4/answers/ex4_4_11a_answer.png",
],
}

# ========== EXERCISE 4.12 (sample of 80 customers - first purchase amount) ==========

ex4["4_12a"] = {
"title": "Ex 4.12a — Pr(customer spends > 80 Euro at first purchase)",
"content": """**Question.** A company sells a narrow range of products. The amount $X$ spent by a client at the first purchase has a quite symmetric distribution with mean $60$ Euro and standard deviation $25$ Euro. Assume that this distribution is *normal*: what is the probability that a generic new customer will spend more than $80$ Euro at the first purchase?

![Ex 4.12 question](statistics/images/ex4/questions/ex4_4_12a_question.png)

---

**AI walkthrough.** The amount spent is modelled as $X \\sim N(60,\\;25^2)$, so the question is a textbook *upper-tail* probability on a normal. We standardise to $Z = (X-\\mu)/\\sigma$ with $z^\\star = (80-60)/25 = 0.80$, then look up $\\Pr(Z > 0.80) = 1 - \\Phi(0.80) \\approx 0.2119$. The shaded region in the plot is exactly this tail — geometrically, "about 21% of the area under the bell beyond the threshold 80".

![Ex 4.12a AI walkthrough](statistics/images/ex4/ex4_4_12a_ai.png)

---

**Answer.** Under $X \\sim N(60, 25^2)$, the probability that a generic client spends more than $80$ Euro is $\\Pr(X > 80) = 0.2118554$:
```r
1 - pnorm(80, 60, 25)
## [1] 0.2118554
# equivalent via standardised Z
1 - pnorm((80 - 60)/25)
## [1] 0.2118554
```

![Ex 4.12 answer](statistics/images/ex4/answers/ex4_4_12a_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_12a_question.png",
    "statistics/images/ex4/ex4_4_12a_ai.png",
    "statistics/images/ex4/answers/ex4_4_12a_answer.png",
],
}

ex4["4_12b"] = {
"title": "Ex 4.12b — Pr(sample sum > 9100)",
"content": """**Question.** Let $S$ indicate the turnover from the campaign, that is the total amount spent by 150 new clients at their first purchase. Is it possible that $\\Pr(S > 9100) = 0$?

---

**Answer.** No. Since $S = X_1 + \\ldots + X_{150}$ with $X_i \\sim N(60, 25^2)$ iid, $S \\sim N(150\\cdot 60,\\; 150\\cdot 25^2) = N(9000,\\; 93750)$, and
```r
1 - pnorm(9100, 150*60, sqrt(150*25^2))
## [1] 0.3719857
```

or
```r
1 - pnorm(9100, 9000, sqrt(93750))
## [1] 0.3719857
```

The same result could be obtained also considering that the r.v. $Y = S - 9100$ (which is the total amount spent by 150 new clients at their first purchase, less the cost) is distributed as $Y \\sim N(150\\cdot 60 - 9100,\\; 150\\cdot 25^2) = N(-100,\\; 93750)$. Therefore, the probability that the total amount spent is higher than the cost is $\\Pr(Y > 0) = 0.372$, obtained as:
```r
1 - pnorm(0, -100, sqrt(93750))
## [1] 0.3719857
```
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_12b_question.png",
    "statistics/images/ex4/ex4_4_12b_ai.png",
    "statistics/images/ex4/answers/ex4_4_12b_answer.png",
],
}

# ========== EXERCISE 4.13 (linear combination + sample of 80) ==========

ex4["4_13a"] = {
"title": "Ex 4.13a — CLT for total spending: Pr(S > 1000) with n = 80",
"content": """**Question.** A company estimates that the amount spent on its products by a customer in a generic shop where a promotion is running has an average of $12$ Euro and a standard deviation of $5$ Euro. Suppose a promotional campaign is launched at a shop and assume that exactly $n = 80$ customers take advantage of the promotion. Evaluate the probability that the $80$ customers spend a total of more than $1000$ Euro. **Specify clearly whether and what assumptions are needed** to determine the required probability.

![Ex 4.13 question](statistics/images/ex4/questions/ex4_4_13a_question.png)

---

**AI walkthrough.** The question is engineered to test whether you spot that **no distributional assumption on $X$ is needed** — only the mean, the variance, and the fact that $n = 80$ is "large enough" for the **central limit theorem (CLT)** to kick in.

1. **Identify the target.** Let $X_i$ = amount spent by customer $i$, iid with $E[X_i] = 12$, $\\Var(X_i) = 5^2 = 25$. Total spending is $S = X_1 + \\ldots + X_{80}$.
2. **Compute moments of the sum** (linearity + independence — no normality required):
   - $E[S] = n\\,\\mu = 80\\cdot 12 = \\mathbf{960}$
   - $\\Var(S) = n\\,\\sigma^2 = 80\\cdot 25 = \\mathbf{2000}$, so $\\mathrm{SD}(S) = \\sqrt{2000} \\approx 44.72$.
3. **Invoke the CLT.** With $n = 80 \\gg 30$, regardless of the shape of $X_i$:
$$
S \\;\\approx\\; N(960,\\; 2000).
$$
This is exactly what the prompt wants you to spell out as the only assumption.
4. **Standardise and compute the upper-tail.** $z^\\star = (1000 - 960)/\\sqrt{2000} \\approx 40/44.72 \\approx 0.894$, so $\\Pr(S > 1000) = 1 - \\Phi(0.894) \\approx \\mathbf{0.1855}$.
5. **Geometric reading.** $1000$ sits less than one standard deviation above the mean — the upper tail beyond it captures about $19\\%$ of the mass, comfortably above the $\\approx 16\\%$ of a "$+1\\sigma$" cutoff (left panel of the plot).
6. **Why the CLT is enough.** The right panel shows the sampling distribution of the *per-customer mean* $\\bar X$ tightening as $n$ grows: with $n = 80$ the spread is $\\sigma/\\sqrt n = 5/\\sqrt{80} \\approx 0.56$, so the threshold $\\bar X = 12.5$ (equivalent to $S = 1000$) is just $\\approx 0.9$ SE above the mean — the same Z-score, viewed on the mean scale.

![Ex 4.13a AI walkthrough](statistics/images/ex4/ex4_4_13a_ai.png)

---

**Answer.** Assumptions on the distribution of the amount spent by an individual customer are **not** necessary: since $n = 80$ is large, by the central limit theorem the distribution of $S$ is approximately normal,
$$
S \\;\\approx\\; N(80\\cdot 12,\\; 80\\cdot 25) \\;=\\; N(960,\\; 2000).
$$
The required probability is $\\Pr(S > 1000) = 0.1855$:
```r
p_S <- 1 - pnorm(1000, mean=960, sd=sqrt(2000))
p_S
## [1] 0.1855467
```

![Ex 4.13a answer](statistics/images/ex4/answers/ex4_4_13a_answer.png)
""",
"images": [
    "statistics/images/ex4/questions/ex4_4_13a_question.png",
    "statistics/images/ex4/ex4_4_13a_ai.png",
    "statistics/images/ex4/answers/ex4_4_13a_answer.png",
],
}
