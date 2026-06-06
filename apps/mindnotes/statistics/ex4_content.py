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
"title": "Ex 4.3c — Probability that 10 customers will have a receipt > 14.3€",
"content": """**Question.** What is the probability that 10 customers will have a receipt of more than 14.3€ for private label products?

---

**Answer.** Probability that a single customer spends more than 14.3€:
```r
1 - pnorm(14.3, 13.2, 1.2)
## [1] 0.1797
```

The probability that **all 10** independent customers spend more than 14.3€ (each one separately) is $(0.1797)^{10}$:
```r
(1 - pnorm(14.3, 13.2, 1.2))^10
## ≈ 3.27e-08
```
""",
"images": [],
}

ex4["4_3d"] = {
"title": "Ex 4.3d — Probability that 150 customers spend > 1980 total",
"content": """**Question.** Consider the 150 customers in a store: what is the probability that at least 80% will spend more than 12 Euros to purchase private label products?

---

**Answer.** Define $p$ = probability one customer spends > 12 Euros = 0.8413. With $n = 150$ customers, the proportion of customers $\\bar P$ exceeding the threshold follows approximately

$$
\\bar P \\sim N\\left(0.8413,\\; \\frac{0.8413(1-0.8413)}{150}\\right).
$$

The probability that the proportion is at least 80%:
```r
1 - pnorm(0.8, 0.8413, sqrt(0.8413*(1-0.8413)/150))
## ≈ 0.9075
```
""",
"images": [],
}

# ========== EXERCISE 4.4 (delivery service Y = X - 90)... sampling ==========

ex4["4_4a1"] = {
"title": "Ex 4.4 a1 — Probability of delivery time exceeding 90 min",
"content": """**Question.** Considering the delivery time exceeding 90 minutes — the journey time $X$ is normally distributed with mean 80 and SD 25 — what is the probability that the random variable $Y = X - 90$ is positive?

---

**Answer.** $Y$ follows a normal distribution and the standard normal distribution; therefore $(80 - 90)/\\sigma$ is the first quartile of a standard normal distribution.

```r
pnorm(0, -10, 25)
## [1] 0.6554217
```

So $(80 - 90)/25 = -0.6745$, leading to $-0.6748$. Therefore $X \\sim N(90, 14.826^2)$.

**a2)** The probability that a generic workday without any special events, the bus takes more than 2 hours to travel the entire route, $\\Pr(X > 120)$:
```r
1 - pnorm(120, mean=90, sd=14.826)
## [1] 0.0211524
```
""",
"images": [],
}

ex4["4_4b"] = {
"title": "Ex 4.4b — P(travel time > 82 min) and 30th percentile",
"content": """**Question.** The bus that travels exceeded 70% of the days. What is the 30-th percentile?

---

**Answer.**
```r
qnorm(0.3, 90, 14.826)
## [1] 82.2524
```

The variable indicating whether the bus takes less than 82 minutes to complete its entire route has a Bernoulli distribution with parameter $p$ = probability that the bus takes less than 82 minutes:
```r
pnorm(82, 90, 14.826)
## [1] 0.294739
```

The proportion of buses over 45 taking less than 82 minutes, $\\bar P_n$, has a distribution that can be approximated by the Central Limit Theorem:

$$
\\bar P \\approx N(p, p(1-p)/n) \\;=\\; N(0.2954, 0.0207)
$$

The probability that the bus takes in total less than 82 minutes on at least 10% of the days:
```r
1 - pnorm(0.10, 0.295, sqrt((0.295*(1-0.295))/45))
## [1] 0.9979317
```
""",
"images": [],
}

# ========== EXERCISE 4.5 (linear combination — duration of sequential events) ==========

ex4["4_5"] = {
"title": "Ex 4.5 — Total time = sum of two normal sequential events",
"content": """**Question.** Two sequential events $D$ and $B$. The total time $D + B$ required to pass both checks: compute $E[Y]$ and $Var(Y)$.

---

**Answer.** Given $X = D + B$ and assuming independence, $E[Y] = E[D] + E[B] = 13$ and $Var(Y) = Var(D) + Var(B) + 2\\mathrm{Cov}(D, B) = 2\\rho_{D,B}\\sigma_D\\sigma_B + 1 + 2 = 4.3735 - 6.75$.

Therefore $Y \\sim N(13, 6.75)$.

**b)** We need to compute the 90-th percentile of the distribution of the total time required to pass both checks, which corresponds to the 10% longest times it could take:
```r
qnorm(0.9, 13, sqrt(6.75))
## [1] 16.32957
```
If the flight leaves at 5 pm you should therefore arrive at around 3.59 pm.
""",
"images": [],
}

# ========== EXERCISE 4.6 (sum and difference of normals) ==========

ex4["4_6"] = {
"title": "Ex 4.6 — Sum/difference of normals: E and Var",
"content": """**Question.** Combine two normals via a linear combination.

---

**Answer.** Let $D = X + B$ where $X \\sim N(\\mu_X, \\sigma_X^2)$ and $B \\sim N(\\mu_B, \\sigma_B^2)$. Then $D$ is normally distributed with mean $E[D] = E[X] + E[B]$ and variance $Var(D) = Var(X) + Var(B)$ (assuming independence).

To compute the probability that the sum exceeds a threshold:
```r
1 - pnorm(threshold, mean=E_D, sd=sqrt(Var_D))
```

Equivalent works for the difference $D - B$ — mean subtracts, variance adds.
""",
"images": [],
}

# ========== EXERCISE 4.7 (covariance of joint dist) ==========

ex4["4_7"] = {
"title": "Ex 4.7 — Probability both checks pass — joint distribution",
"content": """**Question.** What is the probability that both checks $X$ and $D$ pass — i.e. the joint distribution of $X = D + B$ requires both events?

---

**Answer.** $E(Y) = E[B] + E[D] = 13$ and $Var(Y) = Var(B) + Var(D) + 2\\mathrm{Cov}(B, D) = 1.6 + 1 + 2\\cdot 1.4375 = 6.75$.

Therefore $Y \\sim N(13, 6.75)$.

**b)** We need to compute the 90-th percentile of the distribution of the total time required to pass both checks, which corresponds to the 10% longest times it could take.
```r
qnorm(0.9, 13, sqrt(6.75))
## [1] 16.32957
```
""",
"images": [],
}

# ========== EXERCISE 4.8 (bivariate normal G = 0.4X + 0.6Y) ==========

ex4["4_8a"] = {
"title": "Ex 4.8a — Bivariate normal G = 0.4X + 0.6Y: E[G] and Var[G]",
"content": """**Question.** Let $G$ denote the final grade, $G = 0.4X + 0.6Y$. Compute $E[G]$ and $Var[G]$ given $X \\sim N(25, 4²)$ and $Y \\sim N(28, 4²)$ with $\\mathrm{Cov}(X, Y) = 0$ and $(X, Y)$ jointly Bernoulli/normal.

---

**Answer.**

$$
E[G] = 0.4\\cdot E[X] + 0.6\\cdot E[Y] = 0.4\\cdot 25 + 0.6\\cdot 28 = 26.8.
$$

$$
Var[G] = 0.4^2 Var[X] + 0.6^2 Var[Y] + 2\\cdot 0.4\\cdot 0.6\\cdot \\mathrm{Cov}(X, Y).
$$

Computing $\\mathrm{Cov}(X,Y) = \\rho_{X,Y}\\sigma_X\\sigma_Y$:
```r
0.25 * (5.04 * sqrt(24.616))
## [1] 4.0953
```

The internal consistency between the 50% central votes can be determined only under the assumption that the joint distribution of $(X, Y)$ is a bivariate normal. In this case, $G \\sim N(26.8, 24.616)$ and the range of the 50% central votes is the interval between the first and third quartile, which has amplitude:
```r
qnorm(0.25, 25.04, sqrt(24.616))
## [1] 21.66085
qnorm(0.75, 25.04, sqrt(24.616))
## [1] 28.38645
```

**c)** The variable indicating whether a generic student will get a grade higher than 24 has a Bernoulli distribution with parameter $p = \\Pr(G > 24)$. To calculate such probability it is again necessary to assume that the joint distribution of $(X, Y)$ is a bivariate normal. Under such assumption the parameter $p$ is:
```r
1 - pnorm(24, mean=25.04, sd=sqrt(24.616))
## [1] 0.5830163
```

The proportion of students with final grades greater than 24, $\\bar P$, has a distribution that, by the central limit theorem, can be approximated by a normal distribution:

$$
\\bar P \\approx N(p, p(1-p)/n) = N(0.583, 0.583(1-0.583)/200).
$$

Therefore, $\\Pr(\\bar P > 0.50)$:
```r
1 - pnorm(0.5, mean=0.583, sd=sqrt((0.583*(1-0.583))/200))
## [1] 0.9913583
```

**d)** The mean final grade of the 200 students $\\bar G$ has expected value $E[\\bar G] = E[G] = 26.8$ and variance $Var(\\bar G) = Var(G)/n = 24.616/200$. By the central limit theorem, the distribution of $\\bar G$ can be approximated by a normal: $\\bar G \\sim N(25.04, 24.616/200)$. The required probability is therefore:
```r
1 - pnorm(24, mean=25.04, sd=sqrt(24.616/200))
## [1] 0.9984837
```
""",
"images": [],
}

# ========== EXERCISE 4.9 (pizzeria winter — Sales sampling distribution) ==========

ex4["4_9a"] = {
"title": "Ex 4.9a — P(sample mean Sales < 22500) — pizzeria sampling",
"content": """**Question.** A sample of size 81 pizzerias in the winter season. Sales of single pizzerias have mean 25000 and SD 9800/$\\sqrt{81}$. Probability that the sample mean is less than 22500?

---

**Answer.** Since the sample is large enough, by the Central Limit Theorem no assumptions on the distribution of the turnover in the population are needed and the distribution of the mean of a sample of size 81 can be approximated by a Normal distribution, $\\bar X \\sim N(25000, 9800^2/81)$.

```r
pnorm(22500, 25000, sqrt(9800^2/81))
## [1] 0.0104027
```
""",
"images": [],
}

ex4["4_9b"] = {
"title": "Ex 4.9b — P(sample mean > 24800) — pizzeria sampling",
"content": """**Question.** Probability that the mean turnover on the drawn sample is greater than 24800?

---

**Answer.**
```r
1 - pnorm(24800, 25000, sqrt(9800^2/81))
## [1] 0.5728855
```
""",
"images": [],
}

ex4["4_9c"] = {
"title": "Ex 4.9c — P(mean is between 23000 and 27000)",
"content": """**Question.** To determine the probability that the mean turnover of the pizzerias in the sample is between 23000 and 27000, in a sample of size 20.

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
"title": "Ex 4.10 a1 — Pr(extreme 5% sample mean of AmountSpent)",
"content": """**Question.** Since the sample is large enough, by the Central Limit Theorem no assumptions on the distribution of the amount spent in the population is needed and the distribution of the mean of a sample of size 750 can be approximated by a Normal distribution.

---

**Answer.** $\\bar X \\sim N(1000, 810000/750)$.

**a2)** The mean volume reached by the 5% of the largest sample means is the 95-th percentile, $p95$, of the distribution of the sample mean:
```r
qnorm(0.95, mean=1000, sd=sqrt(810000/750))
## [1] 1054.055
```

The range of values including the 95% of the "central" values of the distribution of the sample mean is the interval between the values $x_{0.025}$ and $x_{0.975}$ such that $\\Pr(\\bar X > x_{0.975}) = 0.975$ and $\\Pr(\\bar X < x_{0.025}) = 0.025$:
```r
qnorm(0.025, mean=1000, sd=sqrt(810000/750))
## [1] 935.589
qnorm(0.975, mean=1000, sd=sqrt(810000/750))
## [1] 1064.411
```

The mean amount in the sample considered in the dataframe DS is 1228.44 and does not belong to that range.
```r
mean(DS$AmountSpent)
## [1] 1228.437
```

Thus, the **available sample** has an extremely high mean, being higher than the value exceeded by the 2.5% of all the means of samples size 750.

**a3)** The percentage of samples of size 750 with a mean of the amount spent greater than 1228.4 is $100\\bar P_n(1228.4)$ and is approximately 0:
```r
1 - pnorm(1228.437, mean=1000, sd=sqrt(810000/750))
## [1] 1.831995e-12
```
""",
"images": [],
}

ex4["4_10b"] = {
"title": "Ex 4.10b — Sample proportion (CLT for proportions)",
"content": """**Question.** The proportion observed in the sample is in the dataframe DS is 0.704.

---

**Answer.**

```r
distr.table.x(Location, freq="prop", data=DS)
##  Close Far
##  0.704 0.296
```

Therefore, the probability that the sample proportion of (a generic) sample of size 750 is higher than the observed, $\\Pr(\\bar P > 0.704)$, is:
```r
1 - pnorm(0.704, mean=0.69, sd=sqrt((0.69*(1-0.69))/750))
## [1] 0.2035329
```

**b2)** The interval that includes the 50% of the less extreme values for the sample proportion if we consider samples of size 750 is the interval between the first and third quartile of the distribution:
```r
qnorm(0.25, mean=0.69, sd=sqrt((0.69*(1-0.69))/750))
## [1] 0.6786093
qnorm(0.75, mean=0.69, sd=sqrt((0.69*(1-0.69))/750))
## [1] 0.7013907
```

**b3)** The probability of extracting samples of size 1000 characterized by a sample proportion greater than 0.72, $\\Pr(\\bar P > 0.72)$:
```r
1 - pnorm(0.72, mean=0.69, sd=sqrt((0.69*(1-0.69))/1000))
## [1] 0.01978318
```

The probability would be lower if the same population would be observed in the smaller sample of size 1000 because the distribution of the sample proportion would be more concentrated around the center due to a lower standard error.
""",
"images": [],
}

# ========== EXERCISE 4.11 (ad cost — linear in X) ==========

ex4["4_11a"] = {
"title": "Ex 4.11a — Daily cost of advertising with X ~ N(10, 5²)",
"content": """**Question.** Let $X$ be the daily cost with the first option for a user that makes about 10 phone calls per day. The expected daily cost is $E[Y] = 10\\cdot 2.5\\cdot 4 + 10\\cdot 100$ cents.

---

**Answer.**
```r
qnorm(0.95, 100, 10)
## [1] 116.4485
```

The first option is more convenient than the first one. There are 60 minutes and a cost of $1100 + (10\\cdot 2.5)\\cdot 60$ cents.

**b)** For the other restricted contracts due to be more convenient than the first offer, the cost per minute must be such that $E[Y] = 10\\cdot 49 = 490$ cents.

If the distribution of call duration is $N(4.1)$ then $S_n \\sim N(10\\cdot 4.1, 10\\cdot 25) = N(41, 250)$, and $Y = 25\\cdot S + 1000$, so $E[Y] = 25\\cdot 41 + 1000 = 2025$ and $Var(Y) = 25^2\\cdot 250 = 156250$.

The minimum daily costs that the user can expect to exceed with probability 0 are the value such that $\\Pr(Y > c) = 0.05$:
```r
qnorm(0.05, mean=2025, sd=sqrt(156250))
## [1] 1361.422
qnorm(0.95, mean=2025, sd=sqrt(156250))
## [1] 1701.422
```
""",
"images": [],
}

# ========== EXERCISE 4.12 (sample of 80 customers - first purchase amount) ==========

ex4["4_12a"] = {
"title": "Ex 4.12a — Pr(customer spends > 80 Euro at first purchase)",
"content": """**Question.** Let $X$ be the r.v. describing the amount spent at first purchase; we assume that $X \\sim N(60, 25^2)$. The probability that a generic clients spends more than 80 Euro is $\\Pr(X > 80)$.

---

**Answer.**
```r
1 - pnorm(80, 60, 25)
## [1] 0.2118554
```

**b)** Let $\\bar P$ describe the random variable describing the proportion of clients who spend more than 80 Euro at their first purchase among a sample of 150 new clients. Since the number of new clients is high enough the distribution of the sample proportion can be approximated by a normal distribution:

$$
\\bar P \\approx N(0.212, 0.212(1-0.212)/150).
$$

And the probability that the proportion is 20% maximum is $\\Pr(\\bar P \\le 0.2)$:
```r
pnorm(0.2, mean=0.212, sd=sqrt(0.212*(1-0.212)/150))
## [1] 0.3611689
```
""",
"images": [],
}

ex4["4_12b"] = {
"title": "Ex 4.12b — Pr(sample sum > 9100)",
"content": """**Question.** Let's indicate the turnover from the campaign that is by the total amount spent by 150 new clients at their first purchase. Is it possible that $\\Pr(S > 9100) = 0$?

---

**Answer.**
```r
1 - pnorm(9100, 150*60, 150*sqrt(25^2))
## [1] 0.3719857
```

or
```r
1 - pnorm(9100, 9000, sqrt(93750))
## [1] 0.3719857
```

The same result could be obtained also considering that the r.v. $Y = S - 9100$ is given as the total amount spent by 150 new clients at their first purchase) is distributed as $Y \\sim N(150\\cdot 60 - 9100, 150\\cdot 25^2) = N(-100, 93750)$. Therefore, the probability that the total amount spent is higher than the cost is $\\Pr(Y > 0) = 0.372$, obtained as:
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
"content": """**Question.** Let $X$ be amount spent by a customer; $E[X] = 12, Var(X) = 5^2$. The total amount spent by a sample of $n = 80$ customers is the random variable $S = X_1 + \\ldots + X_{80}$ ($X_i$ iid).

---

**Answer.** Assumptions: each customer's amount is independent. Since the sample is large enough, by the central limit theorem the distribution of $S$ is approximately normal: $S \\sim N(80\\cdot 12, 80\\cdot 25) = N(960, 2000)$. The required probability is $\\Pr(S > 1000) = 0.1855$:
```r
1 - pnorm(1000, mean=960, sd=sqrt(2000))
## [1] 0.1855467
```

**b)** Let $\\bar P$ be the r.v. describing the proportion of customers spending more than 1000 Euro in the 115 shops, then we have that approximately:

$$
\\bar P \\approx N\\left(0.1855,\\; \\frac{0.1855(1-0.1855)}{115}\\right).
$$

The probability of the sample proportion being less than 0.15 is therefore $\\Pr(\\bar P < 0.15) = 0.166$:
```r
pnorm(0.15, 0.1855, sqrt(0.1855*(1-0.1855)/115))
## [1] 0.163397
```

Or with rounding:
```r
pnorm(0.15, 0.1855, sqrt(0.1855*(1-0.1855)/115))
## [1] 0.1636914
# or rounding to 3 decimals
pnorm(0.15, 0.185, sqrt(0.185*(1-0.185)/115))
## [1] 0.166869
```
""",
"images": [],
}
