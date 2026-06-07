"""
Ex 4 snippets — Statistics Module 4 (probability, normal distribution,
sampling distributions, CLT, linear combinations of random variables).
"""

ex4 = {}

# ========== EXERCISE 4.1 (tea in a glass) ==========

ex4["4_1a"] = {
"title": "Ex 4.1a — P(X > 10) for X ~ N(8, 1.2²)",
"content": """**Question.** The amount (in cl) of tea poured into each glass from a vending machine has a normal distribution with $\\mu = 8$ and $\\sigma = 1.2$. If glasses can hold a maximum of 10 cl, what is the probability that a cup will be filled beyond the limit?

---

**Answer.** Let $X$ denote the amount (in cl) of tea poured into each glass. Then $X \\sim N(8, 1.2^2)$. The probability that a cup will be filled beyond the limit, $\\Pr(X > 10)$, is **0.0478**:

```r
1 - pnorm(10, mean=8, sd=1.2)
## [1] 0.04779035
```
""",
"images": [],
}

ex4["4_1b"] = {
"title": "Ex 4.1b — P(X < 7.5) for X ~ N(8, 1.2²)",
"content": """**Question.** What is the probability that the amount of tea poured into a glass is less than 7.5 cl?

---

**Answer.** The probability that the amount of tea poured into a glass is less than 7.5 cl, $P(X < 7.5)$, is **0.3385**:

```r
pnorm(7.5, mean=8, sd=1.2)
## [1] 0.3384611
```
""",
"images": [],
}

# ========== EXERCISE 4.2 (battery life) ==========

ex4["4_2a"] = {
"title": "Ex 4.2a — P(X < 24) for battery life X ~ N(27, 3.2²)",
"content": """**Question.** The battery life of a particular cell phone model, after two years of use, is normally distributed with mean 27 hours and standard deviation of 3.2 hours. What is the probability that a cell phone of that model, after two years of use, will have a battery life of less than 24 hours?

---

**Answer.** Let $X$ denote the battery life. Then $X \\sim N(27, 3.2^2)$. The probability that a cell phone has battery life less than 24 hours, $P(X < 24)$, is **0.1743**:

```r
pnorm(24, mean=27, sd=3.2)
## [1] 0.1742507
```
""",
"images": [],
}

ex4["4_2b"] = {
"title": "Ex 4.2b — Minimum life of the longest-lasting 20% (80th percentile)",
"content": """**Question.** What is the minimum battery life of the 20% of cell phones of that model that last the longest after two years of use?

---

**Answer.** The minimum battery life of the 20% of cell phones that last the longest after two years of use is the **80-th percentile** of the distribution and is equal to **29.6932 hours**:

```r
qnorm(0.8, 27, 3.2)
## [1] 29.69319
```
""",
"images": [],
}

# ========== EXERCISE 4.3 (private-label spending) ==========

ex4["4_3a"] = {
"title": "Ex 4.3a — P(X > 12) for spending X ~ N(13.2, 1.2²)",
"content": """**Question.** For a large retail chain, the expenditure (in a single act of purchase, in a single receipt) of a generic customer on the purchase of *private label* products (marketed under the distributor's brand name instead of the manufacturer's brand name) can be assumed to be normally distributed with mean 13.2 euros and standard deviation 1.2 euros.

What is the probability that a customer will spend more than 12 euros for private label products?

---

**Answer.** Let $X$ denote the expenditure on private label products; $X \\sim N(13.2, 1.2^2)$. The probability that a customer will spend more than 12 euros is **0.8413**:

```r
1 - pnorm(12, 13.2, 1.2)
## [1] 0.8413447
```
""",
"images": [],
}

ex4["4_3b"] = {
"title": "Ex 4.3b — Minimum expenditure of the top 10% customers (90th percentile)",
"content": """**Question.** Let us consider the "top" customers with reference to expenditure on private label products, i.e., those who spend more than 90% of other customers (in a single act of purchase) on private label products. What is the minimum amount these customers spend on private label products?

---

**Answer.** The minimum expenditure of the 10% of "top" customers is the 90-th percentile of the distribution, and is equal to **14.7379**:

```r
qnorm(0.9, 13.2, 1.2)
## [1] 14.73786
```
""",
"images": [],
}

ex4["4_3c"] = {
"title": "Ex 4.3c — P(total receipt of 10 customers > €135)",
"content": """**Question.** What is the probability that 10 customers will have a receipt of more than €135 on *private label* products?

---

**Answer.** Let $S$ denote the total expenditure of the 10 customers (assuming they represent a random sample). Then $S$ is distributed according to a Normal distribution with

$$
E(S) = 10\\cdot E(X) = 10\\cdot 13.2 = 132, \\qquad \\mathrm{Var}(S) = 10\\cdot \\mathrm{Var}(X) = 10\\cdot 1.2^2 = 14.4,
$$

so $S \\sim N(132, 14.4)$. The probability that 10 customers will have a receipt of more than €135 on private label products, $\\Pr(S > 135)$, is **0.2146**:

```r
1 - pnorm(135, 132, sqrt(14.4))
## [1] 0.2145977
```
""",
"images": [],
}

ex4["4_3d"] = {
"title": "Ex 4.3d — P(≥ 80% of 150 customers spend > 12€) — CLT for proportion",
"content": """**Question.** Consider the 150 customers in a store: what is the probability that at least 80% will spend more than 12 Euros to purchase *private label* products?

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
""",
"images": [],
}

# ========== EXERCISE 4.4 (call-center — duration X ~ N(180, 1600)) ==========

ex4["4_4a1"] = {
"title": "Ex 4.4 a–c — Call duration: P(X>120), 15th percentile, P(X>5·60)",
"content": """**Question.** A mobile phone company estimated that the duration of a customer's call to the call-center follows the normal distribution with mean 180 seconds and variance 1600 sec². Let $X \\sim N(180, 1600)$.

**a)** Probability that a customer's call to the call center will last for more than 120 seconds, $\\Pr(X > 120)$.
**b)** Maximum duration of the 15% shortest phone calls (15-th percentile).
**c)** To avoid an excessive waiting time, consider interrupting the call (proposing a callback) if its duration is longer than 5 minutes. What is the proportion of phone calls that would be interrupted, $\\Pr(X > 5 \\cdot 60)$?

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
## [1] 0.000349898
```
""",
"images": [],
}

ex4["4_4b"] = {
"title": "Ex 4.4 d–g — Sum of 5 calls, sample of 210, CLT for proportion",
"content": """**Question.**
**d)** Let $S$ denote the duration of 5 calls (random sample): probability that processing all the calls takes more than 15 minutes (900 sec)?
**e)** Suppose 210 calls come into the call center in a specific time slot on a business day; excluding 10% of the most extreme scenarios, what is the interval in which we can expect the total time required to process all calls to vary?
**f)** Suppose an operator has to handle 5 calls in a queue. What is the probability that the time required to process all calls is more than 15 minutes?
**g)** Suppose 600 calls into the call center: probability that at least 15% will be resolved without an intervention by the operator (the proportion of calls of duration > 600 s requires a callback in 12% of cases)?

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
""",
"images": [],
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
""",
"images": [],
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
""",
"images": [],
}

# ========== EXERCISE 4.7 (airport: B + D, bivariate normal) ==========

ex4["4_7"] = {
"title": "Ex 4.7 — Airport security: Y = B + D, P(Y ≥ 45), arrival time",
"content": """**Question.** At an airport, the distributions of times (in minutes) required for hand baggage screening ($B$) and document screening ($D$) in a workday are jointly normal. It is further assumed that $B \\sim N(10, 4)$ and $D \\sim N(3, 1)$, and that the linear correlation coefficient between $B$ and $D$ is $\\rho = 0.4375$.

**a)** What is the distribution of the time required to pass both checks ($Y = B + D$)?
**b)** Your flight leaves at 5 p.m. and you want to be done with security at least 45 minutes before your flight. At what time should you show up so that the probability of getting through security in the desired time is at least 90%?

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
""",
"images": [],
}

# ========== EXERCISE 4.8 (bivariate normal G = 0.4X + 0.6Y) ==========

ex4["4_8a"] = {
"title": "Ex 4.8a — Final grade G = 0.4X + 0.6Y: E[G], Var[G], 50% central range, Pr(prop>0.5), sample-mean",
"content": """**Question.** The final grade in a college course is $G = 0.4X + 0.6Y$, where $X$ is the individual project (40%) and $Y$ is the written test (60%). From past cohorts taking the exam at the last session: $E[X] = 26.3$, $\\mathrm{Var}[X] = 22.6$, $E[Y] = 24.2$, $\\mathrm{Var}[Y] = 28.6$, $\\mathrm{Cov}(X, Y) = 22.3$. Performances of different students are independent.

**a)** Mean and variance of $G$ for a student taking the exam in the last session.
**b)** Can the range of the 50% central grades be determined? If not, additional assumption.
**c)** If 200 students show up, estimate $\\Pr(\\text{proportion of students with } G > 24 \\text{ is} > 50\\%)$.
**d)** Probability that the mean grade of the 200 students is greater than 24.

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
""",
"images": [],
}

# ========== EXERCISE 4.9 (pizzeria winter — Sales sampling distribution) ==========

ex4["4_9a"] = {
"title": "Ex 4.9a — P(sample mean Sales < 22500) — pizzeria sampling, n = 81",
"content": """**Question.** The monthly turnover of pizzerias in Milan, Pavia and Lodi has mean $\\mu = 25{,}000\\,\\text{\\euro}$ and standard deviation $\\sigma = 9{,}800\\,\\text{\\euro}$. We randomly draw a sample of $n = 81$ pizzerias. Can you determine the probability that the mean turnover on the drawn sample is less than 22,500? If yes, compute it; if no, explain whether it can be determined under additional assumptions and compute it under those.

---

**Answer.** Since the sample is large enough, by the Central Limit Theorem no assumption on the distribution of turnover in the population is needed: the sample mean is approximately Normal, $\\bar X \\sim N(25000,\\, 9800^2/81)$.

```r
pnorm(22500, 25000, sqrt(9800^2/81))
## [1] 0.0104027
```
""",
"images": [],
}

ex4["4_9b"] = {
"title": "Ex 4.9b — P(sample mean > 24800) — pizzeria sampling, n = 60",
"content": """**Question.** Suppose we randomly draw a sample of $n = 60$ pizzerias (population mean 25,000, SD 9,800). Can you determine the probability that the mean turnover on the drawn sample is greater than 24,800? If yes, compute it; if no, explain whether it can be determined under additional assumptions and compute it under those.

---

**Answer.** Since the sample is large enough, by the Central Limit Theorem no assumption on the distribution of turnover in the population is needed: $\\bar X \\sim N(25000,\\, 9800^2/60)$.

```r
1 - pnorm(24800, 25000, sqrt(9800^2/60))
## [1] 0.5628035
```
""",
"images": [],
}

ex4["4_9c"] = {
"title": "Ex 4.9c — P(23000 < sample mean < 27000), n = 20; d1/d2 with n = 100",
"content": """**Question.** Suppose we randomly draw a sample of $n = 20$ pizzerias (population mean 25,000, SD 9,800). Can you determine the probability that the mean turnover of the pizzerias in the sample is between 23,000 and 27,000? Without further calculations, would the probability be higher, lower, or the same if the sample size were 10?

**d1)** What is the percentage of samples whose mean turnover is greater than that observed in the `pizzerie` dataframe (sample of size 100)?
**d2)** What are the samples with 1% extreme mean turnover (1% lowest or 1% highest)?

---

**Answer.** It is necessary to assume that the turnover in the population of all pizzerias is normally distributed. Under this assumption, the distribution of the mean of a sample of size 20 is $\\bar X \\sim N(25000, 9800^2/20)$, and $\\Pr(23000 < \\bar X < 27000)$ is:

```r
pnorm(27000, 25000, sqrt(9800^2/20)) - pnorm(23000, 25000, sqrt(9800^2/20))
## [1] 0.6385896
```

Without performing further calculations, we expect this probability to be lower if the sample is of size 10 instead of 20, since the distribution of the sample mean would be more dispersed around the mean (as a consequence of a larger standard error).

**d1)** The mean of turnover in samples of size 100 has a distribution that, by the central limit theorem, can be approximated by $\\bar X \\sim N(25000, 9800^2/100)$. The mean turnover in the sample considered in the dataframe `pizzerie` is $\\bar x_n = 23947$:

```r
distr.summary.x(Sales, stats=c("mean","sd"), data=pizzerie)
##  n n.a  mean   sd
## 100 0  23946.99  9538.62
```

The probability that the mean of turnover in samples of size 100 is greater than the final found in this specific sample, $\\Pr(\\bar X > 23947)$, is:
```r
1 - pnorm(23947, 25000, 980)
## [1] 0.8586984
```

**d2)** To define the extreme mean turnover we need to find the 1-st and 99-th percentile of the distribution of the mean of samples of size 100:
```r
qnorm(0.01, 25000, sqrt(9800^2/100))
## [1] 22720.18
qnorm(0.99, 25000, sqrt(9800^2/100))
## [1] 27279.82
```

The samples with 1% extreme mean turnover are those with a mean smaller than 22720 and higher than 27280 Euro.
""",
"images": [],
}

# ========== EXERCISE 4.10 (AmountSpent in DS — CLT) ==========

ex4["4_10a"] = {
"title": "Ex 4.10a — Sample mean of AmountSpent (CLT, n=750)",
"content": """**Question.** Assume that the amount spent by all customers (population) of the company considered in the **DS** dataframe (where information on one sample only is available) has mean 1000 and variance 810000, respectively. Consider a sample of size 750 drawn from the population.

**a1)** Under what assumptions can you determine what is the distribution of the sample mean of the amount spent?

**a2)** What is the minimum value reached by the largest 5% of values of the sample mean? And what is the range of values that contains 95% of the "central" values of the distribution (of the sample mean)? Does the mean amount detected on the sample in the **DS** dataframe (variable *AmountSpent*) belong to that range?

**a3)** What is the percentage of samples of size 750 for which mean turnover would be observed to be greater than that found on customers in the **DS** dataframe?

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
""",
"images": [],
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
"images": [],
}

# ========== EXERCISE 4.11 (ad cost — linear in X) ==========

ex4["4_11a"] = {
"title": "Ex 4.11 — Mobile phone contracts: expected daily cost & 90th percentile",
"content": """**Question.** A user needs to decide which mobile phone contract to subscribe, and she/he is considering a contract with call charges of $25$ cents per minute and $9$ cents per connection charge.

**a)** Assume that this user makes about $10$ phone calls per day, with a mean duration of $4$ minutes. What is the expected daily cost? Would an alternative offer at $49$ cents per minute with no connection charge be convenient in terms of expected daily cost?

**b)** Which offer without connection charge would you have to evaluate for the expected daily cost to be lower than that with connection charge?

**c)** Assume now that the user makes about $10$ phone calls per day, again with a mean duration of $4$ minutes, but assume that the distribution of call duration is normal with variance $1$, and that the lengths of the calls are independent. What is the minimum daily cost that the user can expect to exceed in $90\\%$ of the days under the two offers' scenarios?

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
""",
"images": [],
}

# ========== EXERCISE 4.12 (sample of 80 customers - first purchase amount) ==========

ex4["4_12a"] = {
"title": "Ex 4.12a — Pr(customer spends > 80 Euro at first purchase)",
"content": """**Question.** Let $X$ be the r.v. describing the amount spent at first purchase; we assume that $X \\sim N(60, 25^2)$. The probability that a generic client spends more than 80 Euro is $\\Pr(X > 80) = 0.2118554$:
```r
1 - pnorm(80, 60, 25)
## [1] 0.2118554
```

---

**b)** Let $\\bar P$ describe the random variable describing the proportion of clients who spend more than 80 Euro at their first purchase among 150 new clients. Since the number of new clients is high enough the distribution of the sample proportion can be approximated by a normal distribution:

$$
\\bar P \\approx N\\!\\left(0.212,\\; \\frac{0.212\\,(1-0.212)}{150}\\right) = N(0.212,\\; 0.0011137).
$$

And the probability that the sample proportion is 20% maximum is $\\Pr(\\bar P \\le 0.2) = 0.36$:
```r
# using the un-rounded p_X0 = 0.2118554
pnorm(0.2, mean=0.2118554, sd=sqrt(0.2118554*(1-0.2118554)/150))
## [1] 0.361169
# using the rounded value 0.212
pnorm(0.2, mean=0.212, sd=sqrt(0.212*(1-0.212)/150))
## [1] 0.3595805
# equivalent, plugging in the precomputed variance
pnorm(0.2, mean=0.212, sd=sqrt(0.0011137))
## [1] 0.3595805
```
""",
"images": [],
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
"images": [],
}

# ========== EXERCISE 4.13 (linear combination + sample of 80) ==========

ex4["4_13a"] = {
"title": "Ex 4.13 — Linear normal + CLT for proportion",
"content": """**Question.** Let $X$ be the amount spent by a customer; $E[X] = 12,\\; \\Var(X) = 5^2$. A sample of $n = 80$ customers spends a total $S = X_1 + \\ldots + X_{80}$ ($X_i$ iid). What is $\\Pr(S > 1000)$? Specify clearly whether and what assumptions are needed to determine the required probability.

---

**Answer.** Assumptions on the distribution of the amount spent by an individual customer are not necessary, since — given the high sample size — by the central limit theorem the distribution of $S$ is approximately normal:

$$
S \\sim N(80\\cdot 12,\\; 80\\cdot 25) = N(960,\\; 2000).
$$

The required probability is $\\Pr(S > 1000) = 0.1855$:
```r
p_S <- 1 - pnorm(1000, mean=960, sd=sqrt(2000))
p_S
## [1] 0.1855467
```

---

**b)** Assume the promotion is launched in 115 shops with the same characteristics, and exactly 80 customers in each outlet take advantage of it. Let $\\bar P$ be the r.v. describing the proportion of outlets where the 80 customers spend more than 1000 Euro in total. Then approximately:

$$
\\bar P \\approx N\\!\\left(0.1855,\\; \\frac{0.1855\\,(1-0.1855)}{115}\\right).
$$

The probability of the sample proportion being less than 0.15 is $\\Pr(\\bar P < 0.15) = 0.166$, obtained using one of the following commands:
```r
# using the un-rounded p_S = 0.1855467
pnorm(0.15, p_S, sqrt(p_S*(1-p_S)/115))
## [1] 0.163397
# using p_S rounded to 4 decimals
pnorm(0.15, 0.1855, sqrt(0.1855*(1-0.1855)/115))
## [1] 0.1636914
# or rounding to 3 decimals
pnorm(0.15, 0.185, sqrt(0.185*(1-0.185)/115))
## [1] 0.166869
```
""",
"images": [],
}
