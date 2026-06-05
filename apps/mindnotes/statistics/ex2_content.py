"""
Ex 2 snippets — Statistics Module 2 (boxplot, dispersion, quantiles, outliers).
Each entry is a dict with title, content (markdown + LaTeX), and optional images.
Markdown tables and ```r``` code fences are post-processed in build_snippets.py.
"""

ex2 = {}

# ============== EXERCISE 2.1 (pizzerie) ==============

ex2["2_1a"] = {
"title": "Ex 2.1a — Interval containing 50% of central Sales values (IQR)",
"content": """**Question.** What is the interval that contains 50% of the central values of the variable `Sales`? What information does the width of this interval provide?

---

**Answer.** The required interval is the one whose endpoints are the first and the third quartile of the variable `Sales`:

```r
distr.summary.x(x=Sales, stats=c("q1","q3"), data=pizzerie)
##  n n.a    q1     q3
## 100   0  17683.25  28975
```

Thus, the interval that contains the 50% central data on `Sales` is $[17\\,683.25;\\, 28\\,975]$. The width of the interval, called the **interquartile range** ($IQR$), is approximately equal to $28\\,975 - 17\\,683 = 11\\,292$. This measure is an index of dispersion: small values of $IQR$ indicate the concentration of 50% of data in a relatively small interval and thus denote great concentration of the distribution at least "around the centre".
""",
"images": [],
}

ex2["2_1b"] = {
"title": "Ex 2.1b — Five-number summary + boxplot of Sales",
"content": """**Question.** Report the 5-number summary for the variable `Sales` and represent them in an appropriate graph.

---

**Answer.** The five-number summary includes the most important position measures for a variable: minimum, first quartile, median, third quartile and maximum. For the variable `Sales`:

```r
distr.summary.x(x=Sales, stats="fivenumbers", data=pizzerie)
##  n n.a  min      q1    median    q3      max
## 100   0  8428  17683.25  22349.5  28975  63683
```

These values summarize the distribution of `Sales` both around the center and on the tails, and are used to build the **boxplot**:

```r
distr.plot.x(x=Sales, plot.type="boxplot", data=pizzerias)
```

The graph shows that the central data are concentrated in an interval of relatively small width, and that the tails of the distribution are not particularly wide and have approximately the same length (approximately, the same length of the 'whiskers'). Nevertheless, we notice that the right tail is slightly longer and that there are (apparently) **3 outliers** corresponding to extremely high observations of the variable `Sales` (note that if there are more outliers with the same value they would be overlapping in the plot).
""",
"images": ["statistics/images/ex2_1b-sales-boxplot.png"],
}

ex2["2_1c"] = {
"title": "Ex 2.1c — Shape of the Sales distribution from the boxplot",
"content": """**Question.** Based on the graph obtained at the previous point, what can you say about the shape of the distribution of `Sales`?

---

**Answer.** The boxplot obtained at the previous point for `Sales` shows that the distribution is **positively skewed** due to the relatively long right tail; however, it must be remembered that, excluding the outliers displayed in the graph, the distribution is rather symmetrical.
""",
"images": [],
}

ex2["2_1d"] = {
"title": "Ex 2.1d — Numerical values reported in the boxplot (outlier thresholds)",
"content": """**Question.** Specify clearly what numerical values are reported in the plot obtained at point b).

---

**Answer.** The plot is based on five major values. The first three (from the third quartile $Q_3$ down to the first quartile $Q_1$) are respectively $17\\,683.25$ and $28\\,975$, and is partitioned into two parts by the median, equal to $22\\,349.5$. The two whiskers extend from the box to the minimum regular (not outliers) data and to the maximum regular value. As one can note from the boxplot obtained before, the values higher than the minimum regular value connect the minimum, $8\\,428$, to $17\\,683.25$ (first quartile). As for the right tail, the plot shows the presence of outliers. The upper whisker extends from the third quartile to the maximum regular value, that is the maximum value of `Sales` lower than $Q_3 + 1.5(Q_3 - Q_1)$:

```r
28975+1.5*(28975-17683.25)
## [1] 45912.62
```

The maximum regular value of `Sales` is:

```r
max(pizzerie$Sales[pizzerie$Sales < 45912.62])
## [1] 42987
```

Therefore, the upper whisker extends from the third quartile of $42\\,987$, since no data are observed between $42\\,987$ and $45\\,912.62$. The extreme values highlighted in the plot are the values of `Sales` higher than $45\\,912.62$; since these **are a few cases**, we can list them:

```r
> pizzerie$Sales[pizzerie$Sales > 45912.62]
## [1] 54418 63683 58762
```
""",
"images": [],
}

ex2["2_1e"] = {
"title": "Ex 2.1e — Revenue of a regular (non-outlier) pizzeria",
"content": """**Question.** What is the revenue (`Sales` variable) of a regular (non-outlier) pizzeria?

---

**Answer.** As emphasized at the previous point, the "regular" pizzerias are those whose `Sales` are lower than or equal to $45\\,912.62$.
""",
"images": [],
}

ex2["2_1f"] = {
"title": "Ex 2.1f — Minimum revenue of the 10% pizzerias with the highest Sales",
"content": """**Question.** What is the minimum revenue of the 10% of pizzeria with the highest revenues (`Sales`)?

---

**Answer.** The minimum revenue of the 10% of pizzerias with the highest revenue corresponds to the 90th-percentile. Therefore:

```r
distr.summary.x(x=Sales, stats="p90", data=pizzerie)
##   n n.a    p90
## 100   0  35773.6
```

Thus, **10% of pizzerias in the sample have a higher revenue than $35\\,774$** (or, alternatively stated, are below this value 90% of the time). This information helps better understand the characteristics of the right tail of the distribution, as noted in the previous points, making the distribution skewed to the right.
""",
"images": [],
}

ex2["2_1g"] = {
"title": "Ex 2.1g — Dispersion measures for Price",
"content": """**Question.** After defining the concept of dispersion, propose the different indices that can be used to assess the dispersion of the variable `Price`, indicating their values in the sample and how they are calculated.

---

**Answer.** The **dispersion measures** quantify the distances between the data measured on a variable. More specifically, they can also assess how far the observed data are from the center of the distribution. There are numerous indices to quantify the dispersion of a quantitative variable. The most common measures of dispersion — calculated for the variable `Price` — are:

```r
distr.summary.x(x=Price, data=pizzerie, stats="dispersion")
##  n n.a range IQrange   sd     var    cv
## 100   0  6.5    2.5    1.47  2.17   0.23
```

- The **range** or **range of variation**, the simplest measure of dispersion, is the difference between the maximum and the minimum observed values. It indicates the width of the interval including all the observed data.
- The **interquartile range** ($IQR$) is the difference between the third and first quartile and represents the width of the interval containing the 50% central observations. As for the range, this index is calculated from position measures.
- **Variance** and **standard deviation** quantify the dispersion of data with respect to a specific measure of central tendency, namely the mean, and are calculated based on the distances (deviations) of the data from it. The **variance** is the 'mean' of the squared deviations of data from the mean (more precisely, the sum of the squared deviations from the mean divided by the number of cases minus 1); the **standard deviation** is the square root of the variance and can be regarded as the 'average distance' of the data from the mean. In the case of the variable `Price`, the variance is $2.17$ EUR² and the standard deviation is $1.47$ EUR (note the difference between the unit of measurements).
- The **coefficient of variation** $cv = sd/\\bar x$ is unit-free and useful for comparison.
""",
"images": [],
}

# ============== EXERCISE 2.2 (300 primary school pupils — table in prompt) ==============

EX22_PROMPT_TABLE = """*(Distribution of weekly hours of access to entertainment devices for 300 primary-school pupils, given in the prompt.)*

| Hours of access to devices | Number of pupils |
|---------------------------:|-----------------:|
| $[0,10)$  | 45 |
| $[10,25)$ | 90 |
| $[25,30)$ | 75 |
| $[30,40]$ | 90 |

*(Working table with densities and midpoints, used throughout the exercise:)*

| Hours        | Pupils | Rel.freq $p$ | Density $c$ | Midpoint |
|-------------:|-------:|------------:|------------:|---------:|
| $[0,10)$     | 45     | 0.15        | 0.015       | 5        |
| $[10,25)$    | 90     | 0.30        | 0.020       | 17.5     |
| $[25,30)$    | 75     | 0.25        | 0.050       | 27.5     |
| $[30,40]$    | 90     | 0.30        | 0.030       | 35       |
"""

ex2["2_2a"] = {
"title": "Ex 2.2a — P(X > 14) for grouped hours-of-access data",
"content": """**Question.** What is the proportion of pupils in the sample with more than 14 hours per week of access to entertainment devices?

""" + EX22_PROMPT_TABLE + """

---

**Answer.** The proportion of pupils having access to devices for more than 14 hours a week is:

$$
\\text{Freq}(X > 14) = p_{[25,30)} + p_{[30,40]} + (25 - 14)\\cdot c_{[10,25)} = 0.25 + 0.30 + 11\\cdot 0.020 = 0.77.
$$

The same proportion could also be obtained considering that:

$$
\\text{Freq}(X < 14) = p_{[0,10)} + (14 - 10)\\cdot c_{[10,25)} = 0.15 + 4\\cdot 0.020 = 0.23,
$$

and consequently $\\text{Freq}(X > 14) = 1 - 0.23 = 0.77$ (note that $\\text{Freq}(X = 14) = 0$).
""",
"images": [],
}

ex2["2_2a1"] = {
"title": "Ex 2.2a1 — Is the reported proportion exact or approximate?",
"content": """**Question.** State whether the reported proportion is exact or is an approximation. Justify your answer.

""" + EX22_PROMPT_TABLE + """

---

**Answer.** The obtained value is an **approximation**, based upon the assumption that **each interval's frequency is uniformly distributed over the interval** (uniform-on-interval). Without that assumption we could not allocate the share of class $[10,25)$ that lies above 14 hours; the table only gives the total count per class.
""",
"images": [],
}

ex2["2_2b"] = {
"title": "Ex 2.2b — Modal class definition + identification",
"content": """**Question.** Report the definition of modal class. What is the modal class for the distribution?

""" + EX22_PROMPT_TABLE + """

---

**Answer.** When a continuous variable is presented in classes with **unequal widths**, the **modal class is the class with the highest density** $c_k = f_k/w_k$ (NOT the highest absolute frequency, because a wider class can have more cases without being denser). For the considered distribution, the densities are: 0.015, 0.020, **0.050**, 0.030. Hence the modal class is **$[25, 30)$**.
""",
"images": [],
}

ex2["2_2c"] = {
"title": "Ex 2.2c — Mean and standard deviation from grouped data",
"content": """**Question.** Determine the mean and the standard deviation of the weekly hours of access to devices for the pupils in the sample.

""" + EX22_PROMPT_TABLE + """

---

**Answer.** By **discretizing** the variable on the class midpoints:

$$
\\bar x \\approx 5\\cdot 0.15 + 17.5\\cdot 0.30 + 27.5\\cdot 0.25 + 35\\cdot 0.30 = 23.375.
$$

For the variance, the sum of squared deviations from the mean is:

$$
\\sum_k (m_k - \\bar x)^2 p_k = (5-23.375)^2 \\cdot 0.15 + (17.5-23.375)^2\\cdot 0.30 + (27.5-23.375)^2\\cdot 0.25 + (35-23.375)^2\\cdot 0.30
$$

Multiplied by the bias correction $n/(n-1) = 300/299$:

$$
\\sigma^2 \\approx \\big[(5-23.375)^2\\cdot 0.15 + (17.5-23.375)^2\\cdot 0.30 + (27.5-23.375)^2\\cdot 0.25 + (35-23.375)^2\\cdot 0.30\\big]\\cdot \\frac{300}{299} = 106.1507.
$$

Therefore the standard deviation is $\\sqrt{106.1507} = 10.303$.
""",
"images": [],
}

ex2["2_2d"] = {
"title": "Ex 2.2d — Third quartile from grouped data",
"content": """**Question.** Determine the third quartile of the distribution of the (weekly) hours of access to devices, and explain what is its substantive meaning.

""" + EX22_PROMPT_TABLE + """

---

**Answer.** The third quartile can be only approximated under the assumption that within each interval the frequency is uniformly distributed. The (approximated) third quartile is included in the interval $[30, 40]$, because the cumulative frequency up to 30 is 0.7 and that cumulated up to 40 is 1. It is the value $Q_3$ at which the area cumulated under the histogram is 0.75:

$$
0.7 + (Q_3 - 30)\\cdot 0.03 = 0.75 \\quad \\Longrightarrow \\quad Q_3 = 30 + \\frac{0.75 - 0.7}{0.03} = 31.667.
$$

The quartile indicates the **minimum number of hours** accessed by the 25% of pupils spending more time on the devices, or, equivalently, the **maximum number of hours** accessed by the 75% of the pupils spending less time on devices.
""",
"images": [],
}

ex2["2_2e"] = {
"title": "Ex 2.2e — Interquartile range from grouped data",
"content": """**Question.** Is it possible to determine the interquartile range of the distribution? If no, explain why; if yes obtain it and explain what is its substantive meaning.

""" + EX22_PROMPT_TABLE + """

---

**Answer.** The interquartile range can be only approximated, as discussed above. To derive it we need to determine the first quartile, which turns out to be:

$$
Q_1 = 10 + \\frac{0.25 - 0.15}{0.02} = 15.
$$

Therefore, **$IQR = Q_3 - Q_1 = 31.667 - 15 = 16.667$**. It represents the width of the interval including the number of hours of access to devices by the 50% of the pupils with the more "central" or standard behaviour, not spending — in relative terms — neither much nor few hours on devices. It indicates the difference between the number of hours spent on devices by the 25% of the heaviest users and the number spent by the 25% of lightest users.
""",
"images": [],
}

ex2["2_2f"] = {
"title": "Ex 2.2f — Compare SD and CV: primary vs middle school pupils",
"content": """**Question.** On a sample of 250 middle school pupils the following summaries were observed: mean equal to 35.51 hours and variance equal to 600.84 hours². Compare the standard deviation and the coefficient of variation for pupils in primary and middle school. What information is provided by the two measures, and how would you interpret them with reference to the considered data?

""" + EX22_PROMPT_TABLE + """

---

**Answer.** The standard deviation calculated on the sample of primary school pupils is $\\sqrt{106.1507} = 10.303$ (point c), and the coefficient of variation is $10.303 / 23.375 = 0.44$. For the sample of middle school users, the standard deviation is $\\sqrt{600.84} = 24.51$ and the coefficient of variation is $24.51 / 35.51 = 0.69$. Both the measures are therefore higher for middle school pupils.

The **standard deviations** indicate the average distance of the hours of access from the mean, that is the number of weekly hours above or below the average hours of access for primary and middle school pupils. Thus, for primary school pupils it is "standard" to spend on devices a number of hours between $23.375 - 10.303$ and $23.375 + 10.303$, that is between about 13 and 33 hours; instead for middle school pupils it is standard to spend on devices a number of hours between $35.51 - 24.51$ and $35.51 + 24.51$, thus between about 11 and 60 hours.

The **coefficient of variation** provides information on the relative size of the standard deviation with reference to the mean. Comparing the coefficients of variation is interesting in this case because — even if the considered variables have the same unit of measurement — the average time of access is different for primary and middle school pupils. Thus, for primary school pupils, it is standard to spend on devices a number of hours between the 56% and the 144% of the average time. Instead, for middle school pupils it is standard to spend a number of hours between the 31% and the 169% of the average number of hours.
""",
"images": [],
}

# ============== EXERCISE 2.3 (DS — AmountSpent) ==============

ex2["2_3a"] = {
"title": "Ex 2.3a — Compare dispersion of AmountSpent between two companies",
"content": """**Question.** Analysts are interested to compare some focal summaries of the variable `AmountSpent` with those claimed by a competitor company, which reports for a sample of 620 customers a total amount spent of 682 000 USD, and a variance of the amount spent of 921 486 USD². Compare the average amount spent of the two companies. For which company is the amount spent more dispersed?

---

**Answer.** The means and variances of the amounts spent by the customers of two competitors' sample statistics, observe that:

```r
distr.summary.x(AmountSpent,stats=c("mean", "dispersion"),data=DS)
##  n n.a range IQrange   sd      var      cv
## 750  0  5840 1262.75  970.508409  940900.9  0.79
##  n n.a mean
## 750  0  1228.44
```

The average amount spent by the customers in `DS` is therefore **$1\\,228.44$ USD**, slightly higher than the average amount spent by the customers of the competitor company, that is **$1\\,100$ USD** ($682\\,000/620$). The [sample] variances of the amount spent by customers in the two different companies are respectively $940\\,900.9$ USD² for `DS` (point b) and $921\\,486$ USD² for the competitor. One ought to account for the differences in the means, the comparison should be based on the **coefficient of variation**. The coefficient of variation for `DS` is $0.79$. For the competitor it is $\\sqrt{921\\,486}/1\\,100 = 0.8727204$, which is higher than that observed for `DS`. The coefficient of variation indicates the relative size of the standard deviation in relation to the mean. Thus, the amount spent by the customers' sample of the competitor company is **more variable**.
""",
"images": [],
}

ex2["2_3b"] = {
"title": "Ex 2.3b — Identify extreme-spending customers via upper outliers",
"content": """**Question.** How would you assess whether there are customers having a particularly high expenditure (variable `AmountSpent`)? How would you identify them? How many, if any, are these customers?

---

**Answer.** To assess whether there are customers with 'extreme' high expenditures, we need to verify whether there are any **upper outliers** in the distribution of the variable `AmountSpent`. At this aim, we first compute the main summary measures of the distribution:

```r
distr.summary.x(AmountSpent, stats="summary", data=DS)
##  n n.a min q1 median mean   q3   max  sd     var
## 750  0  38  451  983 1228.44 1713.75 5878 970 940900.9
```

Upper outliers are values higher than the third quartile plus 1.5 times the interquartile range, i.e. $Q_3 + 1.5(Q_3 - Q_1) = 1713.75 + 1.5(1713.75 - 451) = 3607.875$. It is evident that there is at least one superior outlier, which is the maximum. To determine how many customers spend more than 3607.875 (3608 rounded), we compute:

```r
sum(DS$AmountSpent > 3608)
## [1] 17
```

The presence of upper outliers could be easily detected by looking at the boxplot:

```r
distr.plot.x(AmountSpent, plot.type = "boxplot", data=DS)
```
""",
"images": ["statistics/images/ex2_3bc-amountspent-box-hist.png"],
}

ex2["2_3c"] = {
"title": "Ex 2.3c — Shape of the AmountSpent distribution",
"content": """**Question.** How would you describe the shape of the distribution of the variable `AmountSpent`? Please indicate which tools you use and what are your considerations.

---

**Answer.** In order to assess the shape of a distribution, the most effective tool is the **boxplot**, reported at the previous point. For the considered variable, the graph shows that the distribution is **skewed to the right** (or positively skewed): within the 'box', the line corresponding to the median is closer to the lower extreme (first quartile) than to the upper extreme (third quartile); in addition, the lower whisker is considerably shorter than the upper one; the right tail also has extreme values that are very far — in relative terms — from the center of the data.

The right skewness of the distribution of `AmountSpent` in our data could also be detected based on the **histogram**:

```r
distr.plot.x(AmountSpent, plot.type = 'hist', data=DS)
```
""",
"images": ["statistics/images/ex2_3bc-amountspent-box-hist.png"],
}

# ============== EXERCISE 2.4 (insurance company ogive — table reconstructed) ==============

EX24_TABLE = """*(Frequency table reconstructed from the cumulative ogive in the prompt.)*

| Nr contracts | Cumul. freq $F$ | Abs. freq $f$ | Rel. freq $p$ | Density $c$ | Midpoint |
|-------------:|----------------:|--------------:|--------------:|------------:|---------:|
| $[0,10)$     | 200             | 200           | 0.10          | 0.01        | 5        |
| $[10,20)$    | 400             | 200           | 0.10          | 0.01        | 15       |
| $[20,30)$    | 800             | 400           | 0.20          | 0.02        | 25       |
| $[30,60)$    | 1300            | 500           | 0.25          | 0.0083      | 45       |
| $[60,90)$    | 1800            | 500           | 0.25          | 0.0083      | 75       |
| $[90,150]$   | 2000            | 200           | 0.10          | 0.0017      | 120      |
"""

ex2["2_4a"] = {
"title": "Ex 2.4a — Identify the graph type (ogive) and its purpose",
"content": """**Question.** What type of graph is this, and what information does it report?

---

**Answer.** It is an **ogive**, reporting the absolute frequencies cumulated at the endpoints of intervals used to classify or to measure a numerical variable.
""",
"images": [],
}

ex2["2_4b"] = {
"title": "Ex 2.4b — Are there lower outliers?",
"content": """**Question.** Is it possible to state — at least approximately — if there are consultants having such a poor performance (as measured by the number of closed contracts) that they can be considered as outliers?

""" + EX24_TABLE + """

---

**Answer.** To assess whether there are lower outliers, it is necessary to calculate the quartiles and the interquartile range. The quartiles are:

$$
Q_1 = 20 + \\frac{0.25 - 0.20}{0.02} = 22.5, \\qquad Q_3 = 60 + \\frac{0.75 - 0.65}{0.0083} = 72.
$$

Thus, the **interquartile range is $49.5$**, and there are **no lower outliers**, because the minimum is 0, and $22.5 - 49.5\\cdot 1.5$ is clearly lower than $0$.
""",
"images": [],
}

ex2["2_4c"] = {
"title": "Ex 2.4c — Minimum number of contracts for top consultants (90th percentile)",
"content": """**Question.** What is the minimum number of contracts stipulated by the "top" consultants?

""" + EX24_TABLE + """

---

**Answer.** The minimum number of contracts stipulated by the "top" consultants is the **90-th percentile**, which is 90 (read directly from the cumulative table: $F = 1800$ at the upper bound 90 corresponds to cum. proportion 0.90).
""",
"images": [],
}

ex2["2_4d"] = {
"title": "Ex 2.4d — Variability: range, IQR, mean and SD from the ogive",
"content": """**Question.** To measure the variability, the range is not recommended because the lower endpoint of the first class, 0, seems not an accurate value for the minimum. Compute the IQR, variance and SD.

""" + EX24_TABLE + """

---

**Answer.** The **interquartile range is $49.5$**. The variance can be obtained based on the **mean** (approximated by discretizing on the midpoints of the intervals):

$$
\\bar x \\approx 5\\cdot 0.10 + 15\\cdot 0.10 + 25\\cdot 0.20 + 45\\cdot 0.25 + 75\\cdot 0.25 + 120\\cdot 0.10 = 49.
$$

The mean of the squared values (approximated) is $3502.5$. The variance — multiplied by the bias correction $n/(n-1) = 2000/1999$ — is approximately:

$$
\\sigma^2 \\approx (3502.5 - 49^2) \\cdot \\frac{2000}{1999} = 1102.051.
$$

The **standard deviation is therefore $\\sqrt{1102.051} = 33.197$**. *(Refer to Exercise 2.2 for comments/interpretation.)*
""",
"images": [],
}

# ============== EXERCISE 2.5 (customer_habits Age) ==============

ex2["2_5a"] = {
"title": "Ex 2.5a — Boxplot of Age in customer_habits and interpretation",
"content": """**Question.** Draw the boxplot for the variable `Age` and discuss its shape.

---

**Answer.** The boxplot can be obtained using:

```r
distr.plot.x(Age, plot.type="boxplot", data=customer_habits)
```

Note that the customers in the dataset are quite heterogeneous in terms of age: the lowest age observed is lower than 20 years, approximately 16-18 years, while the oldest customers are aged about 90 years. On the other hand, the median age is about 45 years, and the 50% of customers with age "around" the median (i.e. between the first and third quartile) are more homogeneous, with ages ranging from about 40 to 55 years (with a difference of 15 years only). Indeed, the wide range observed for the variable is due to the 25% of younger customers and especially to the 25% of older customers. Outliers are present both in the right and left tail of the distribution corresponding to ages over 75 years and below 18 years respectively. These observations are associated with relatively high deviations from the center of the distribution, especially those in the right tail. The distribution is thus skewed to the right: 75% of the customers are aged between 20 and 55 years (range 35 years); the remaining 25% are aged between 55 and 90 years, although it is not possible to precisely assess the relevance of the outliers. However, if we do not consider these extreme values, the distribution appears fairly symmetrical.
""",
"images": ["statistics/images/ex2_5a-age-boxplot.png"],
}

ex2["2_5b"] = {
"title": "Ex 2.5b — Two thresholds identifying 5% / 95% extremes",
"content": """**Question.** Identify two threshold values from below and above which lie 5% of the sample respectively.

---

**Answer.** We look for 2 threshold values identifying, on the left and right tails respectively, two groups of customers consisting of both of 5% of the sample: these values are the two percentiles of order 5% and 95%, which satisfy the following conditions:

$$
\\text{Freq}\\{Age \\le p5\\} = 0.05, \\qquad \\text{Freq}\\{Age \\ge p95\\} = 0.05 \\;\\Longleftrightarrow\\; \\text{Freq}\\{Age \\le p95\\} = 0.95.
$$

We can compute them using the following R function:

```r
distr.summary.x(Age, stats=c("p5","p95"), data=customer_habits)
##  n n.a p5 p95
## 34866  0 32 66
```

Based on the available sample, **5% of the youngest customers are at most 32 years old**, while **5% of the most mature customers are at least 66 years old**.
""",
"images": [],
}

ex2["2_5c"] = {
"title": "Ex 2.5c — Proportions in overlapping age intervals",
"content": """**Question.** Determine the proportions of customers in ages 20-40, 30-50, 40-60, and over 50. Which target group is the best?

---

**Answer.** Since the considered age classes are overlapping, it is not possible to build the frequency distribution of the variable classified in the given intervals. To determine the proportion of customers with ages in each considered interval, one can build a logical vector indicating for each customer whether or not their age falls in the interval, and calculate the average of the elements of such vector. For example, to determine the proportion of clients with ages of 20 and 40 we can compute the following:

```r
mean(customer_habits$Age >= 20 & customer_habits$Age <= 40)
## [1] 0.2801296
```

In practice, `customer_habits$Age >= 20 & customer_habits$Age <= 40` defines a logical vector that takes values TRUE or FALSE depending on whether the customer's age falls in the range $[20, 40]$ or not:

```r
head(customer_habits$Age >= 20 & customer_habits$Age <= 40)
## [1] FALSE FALSE TRUE FALSE FALSE
```

The sum of logical values returns the number of TRUE values and the mean — equal to the sum of the elements of the vector divided by the number of cases — is therefore the required proportion. Proceeding in the same way for the other considered intervals, we obtain:

```r
mean(customer_habits$Age >= 30 & customer_habits$Age <= 50)
## [1] 0.6256812
mean(customer_habits$Age >= 40 & customer_habits$Age <= 60)
## [1] 0.6470487
mean(customer_habits$Age > 50)
## [1] 0.3459244
```

Based on these results, it seems convenient to **target customers between 40 and 60 years of age** (who make up approximately 65% of the sample). This conclusion could be reached, at least approximately, also based on the box plot.
""",
"images": [],
}

ex2["2_5d"] = {
"title": "Ex 2.5d — Identify outliers and their share",
"content": """**Question.** Determine the outliers and compute their percentage on the sample.

---

**Answer.** Extreme values are those deviating from the boxplot's box (whose extremes are the first and the third quartile) more than 1.5 times the width of the box, that is the interquartile range. To identify extreme values, one can refer to the five-number summary:

```r
distr.summary.x(Age, stats="fivenumber", digits=2, data=customer_habits)
##  n n.a min q1 median q3 max
## 34866 0 16 40   46  54  96
```

The thresholds to identify lower and upper outliers are:

- $\\text{Limite outlier superiore} = Q_3 + 1.5\\cdot (Q_3 - Q_1) = 54 + 1.5\\cdot 14 = 75$
- $\\text{Limite outlier inferiore} = Q_1 - 1.5\\cdot (Q_3 - Q_1) = 40 - 1.5\\cdot 14 = 19$

The percentage of outliers in the sample is:

```r
100*mean(customer_habits$Age < 19 | customer_habits$Age > 75)
## [1] 0.9952389
```

Thus, the outliers represent **less than 1% of the dataset**, or, to be precise, 0.995% of all customers; their weight is therefore residual. This indicates that the majority of customers over the age of 54 are at most 75 years old.
""",
"images": ["statistics/images/ex2_5a-age-boxplot.png"],
}

ex2["2_5f"] = {
"title": "Ex 2.5f — Mean vs median of Age and right-skew confirmation",
"content": """**Question.** Compute the mean age and compare with the median. Confirm the skewness of the distribution.

---

**Answer.** As noted above, the distribution of the variable is **skewed to the right**, suggesting a mean age above the median age, that is 46. The range 40-45 years for the mean does not appear to be accurate. Indeed, the exact value of the mean age is:

```r
distr.summary.x(Age, stats="mean", digits=1, data=customer_habits)
##  n n.a mean
## 34866 0  47.1
```

The average age is **47.1 years**, only slightly higher than the upper endpoint of the interval 40-45 and only slightly higher than the median. Indeed, although the distribution is slightly skewed to the right, the right tail is not particularly long and the extreme values are a few, as established above and shown in the histogram below:

```r
distr.plot.x(Age, plot.type="histogram", breaks=20, data=customer_habits)
```
""",
"images": [],
}

ex2["2_5g"] = {
"title": "Ex 2.5g — Histogram with the 5-number summary as breakpoints",
"content": """**Question.** Build a histogram of `Age` using the 5-number summary as breakpoints. Interpret the densities.

---

**Answer.** Each interval defined by the five-number summary statistics ($[min, Q_1], [Q_1, Me], \\ldots$) contains about the 25% of the data: the interval with the smallest width will therefore be characterised by a higher concentration of customers, i.e. the highest density. In fact

$$
\\text{density} = c_k = \\frac{\\text{rel.freq}_k}{\\text{width}_k} = \\frac{f_k}{w_k} \\approx \\frac{0.25}{w_k},
$$

where, by construction, the numerator is constant for the 4 classes. Graphically, the interval between the first quartile and the median ($[40,46)$) is the one with the smallest width, and consequently with the highest density, as could also be deduced from the histogram built based on intervals having as endpoints the 5-number summary:

```r
distr.plot.x(Age, plot.type="histogram", breaks=c(16,40,46,54,96), data=customer_habits)
```
""",
"images": ["statistics/images/ex2_5g-age-hist-5num.png"],
}

# ============== EXERCISE 2.6 (customer_habits Revenue) ==============

ex2["2_6a1"] = {
"title": "Ex 2.6a1 — Deciles and high-tail percentiles for Revenue",
"content": """**Question.** Describe the distribution of `Revenue` with a set of position measures (deciles + p90, p95, p99).

---

**Answer.** In order to accurately describe both the center and the tails (if any) of the distribution of the variable `Revenue`, it is advisable to refer to a set of quantiles, e.g. the **deciles** that identify the endpoints of the 10 intervals between the minimum and maximum including about the 10% of the data each. A preliminary analysis of the deciles reveals the presence of some transactions with very high revenues; therefore, it is also worthwhile to report the 90-th and 95-th percentiles:

```r
distr.summary.x(Revenue, stats=c("deciles", "p95", "p99"), data=customer_habits)
##  n n.a min p10 p20  p30   p40 p50 p60 p70 p80  p90 max
## 34866 0 0.67 51 107 179.33 276 428 654 957 1412 2336 15548
##  n n.a p95 p99
## 34866 0 3351 6309
```

The deciles and percentiles of order 90 and 95 provide a concise but effective description of the distribution, highlighting its strong asymmetry to the right. Indeed, we note that the distance between the lower order deciles is much smaller than that between the higher order deciles, and that the distance between deciles increases as their order increases. In this case, reporting only the measures of central tendency would only provide a description of the center of the distribution and would not be appropriate. The five-number summary, which adds information about the minimum, maximum and the quartiles, would also provide a clear indication of the asymmetry, but would not allow appreciating the different density of the distribution on the right-hand side (see also the answer to a3).
""",
"images": [],
}

ex2["2_6a2"] = {
"title": "Ex 2.6a2 — Five-number summary + boxplot of Revenue",
"content": """**Question.** Report the five-number summary of `Revenue` and represent it as a boxplot.

---

**Answer.** The five-number summary for `Revenue`:

```r
distr.summary.x(Revenue, stats="fivenumber", data=customer_habits)
##  n n.a min q1 median  q3   max
## 34866 0 0.67 140  428 1150.5 15548
```

are represented graphically via a boxplot:

```r
distr.plot.x(Revenue, plot.type="boxplot", data=customer_habits)
```

The boxplot shows the right-hand skewness of the distribution, as well as the presence of a substantial number of extreme `Revenue` values. However, except for the fact that there are transactions with extremely high receipts, it is not possible to deduce the density of the distribution on the right tail from the graph. The deciles and percentiles shown in a1) allowed to better characterize this tail and provided greater detail.
""",
"images": ["statistics/images/ex2_6a2-revenue-boxplot.png"],
}

ex2["2_6a3"] = {
"title": "Ex 2.6a3 — Histograms with equal vs custom intervals for Revenue",
"content": """**Question.** Build histograms with 10 equal intervals and with custom intervals for the right tail. Comment.

---

**Answer.** To obtain the two histograms we use the commands:

```r
distr.plot.x(Revenue, plot.type="hist", breaks=10, data=customer_habits)
distr.plot.x(Revenue, plot.type="hist",
             breaks=c(0,51,107,179,276,428,654,957,1412,2336,15548),
             data=customer_habits)
```

Both the histograms provide a representation of the distribution that clearly shows its asymmetrical structure. The second histogram offers more details on the asymmetrical structure of the distribution also on the lower values, while the first describes better the values greater than 2500 (approximately). Probably a good compromise is to use a combination of the classes, for example:

```r
distr.plot.x(Revenue, plot.type="hist",
             breaks=c(0,300,600,1000,1500,2500,5000,10000,15548),
             data=customer_habits)
```
""",
"images": ["statistics/images/ex2_6a3-revenue-hist-comparison.png"],
}

ex2["2_6b"] = {
"title": "Ex 2.6b — Percentage of products sold below cost (margin < 0)",
"content": """**Question.** Determine the number of transactions on products sold below the unit cost, and the percentage on the total.

---

**Answer.** The number of transactions of products sold below cost, we consider the number of elements of a logical vector indicating whether the condition `Unit_Cost > Unit_Price` is satisfied. The mean of the vector by multiplying the mean of the vector by 100:

```r
100*mean(customer_habits$Unit_Cost > customer_habits$Unit_Price)
## [1] 14.05954
```

That is, the sum of the elements of a logical vector indicating whether the condition is satisfied (gives the number of products sold below cost) is divided by the total number of products. To obtain the percentage of `Unit_Price < Unit_Cost`, the proportion must be determined:

```r
diff.price.cost <- customer_habits$Unit_Price - customer_habits$Unit_Cost
distr.summary.x(diff.price.cost, stats="p95")
##  n n.a p95
## 34866 0 211
```

and is equal to 211 USD.
""",
"images": [],
}

ex2["2_6c"] = {
"title": "Ex 2.6c — Dispersion comparison of Unit_Price and Revenue",
"content": """**Question.** Compare the dispersion of `Unit_Price` and `Revenue`, having different means, by referring to the coefficient of variation.

---

**Answer.** In order to compare the dispersion of the two variables, having different means (`Revenue` is given by the product of `Unit_Price` and the number of pieces purchased), it is necessary to refer to the coefficient of variation:

```r
distr.summary.x(customer_habits$Unit_Price, stats=c("mean", "dispersion"))
##  n n.a range IQrange  sd     var       cv
## 34866 0 5081.33 467.33 525.32 275960.2 1.35
##  n n.a mean
## 34866 0  389.23

distr.summary.x(customer_habits$Revenue, stats=c("mean", "dispersion"))
##  n n.a range IQrange  sd     var       cv
## 34866 0 15547.33 1010.5 1286.83 1655919 1.41
##  n n.a mean
## 34866 0 909.72
```

As expected, the variable `Revenue` has a higher mean than `Unit_Price` and, despite a higher variance (and standard deviation), its coefficient of variation is not particularly higher than that of `Unit_Price`, which is 0.79. This indicates that in *relative* terms, the amount spent by the company's customers is more variable.
""",
"images": [],
}

# ============== EXERCISE 2.7 (Nr_visits, table given in prompt) ==============

EX27_TABLE = """*(Distribution of `Nr_visits` given in the prompt — 2200 customers.)*

| Nr_Visits | 1 | 2 | 3 | 4 | 6 | 8 | 9 | 10 | 12 | 14 | 15 | 16 | 20 | 24 |
|----------:|--:|--:|--:|--:|--:|--:|--:|---:|---:|---:|---:|---:|---:|---:|
| Count     | 193 | 272 | 138 | 115 | 92 | 60 | 118 | 228 | 309 | 130 | 113 | 104 | 122 | 206 |
"""

ex2["2_7a"] = {
"title": "Ex 2.7a — Reading the ogive of Nr_visits",
"content": """**Question.** What type of graph is reported and what values are shown on the vertical axis? Corresponding to what values does the function increase, and what do these increases refer to?

""" + EX27_TABLE + """

---

**Answer.** The graph reports on the y-axis the **cumulative relative frequencies**, i.e., for each value on the horizontal axis, the proportion of customers who visited the shop a number of times equal to or up to the value considered. For example, the value of the function at 3 visits to the shop is:

$$
\\text{Freq}(Nr\\_visits \\le 3) = \\frac{193 + 272 + 138}{2200} = 0.274.
$$

The increments of the function are the relative frequencies of the corresponding value (for example, the increment corresponding to the value 2 is its relative frequency, 0.124). Proceeding manually, we obtain the relative frequencies $p$ for every value $x$:

| Nr_Visits | 1 | 2 | 3 | 4 | 6 | 8 | 9 | 10 | 12 | 14 | 15 | 16 | 20 | 24 |
|----------:|--:|--:|--:|--:|--:|--:|--:|---:|---:|---:|---:|---:|---:|---:|
| Prop      | 0.088 | 0.124 | 0.063 | 0.052 | 0.042 | 0.027 | 0.054 | 0.104 | 0.140 | 0.059 | 0.051 | 0.047 | 0.055 | 0.094 |

Alternatively, it is possible (but not necessary) to determine the relative frequencies using R by creating a vector containing the absolute frequencies shown in the table and dividing its elements by the total number of customers in the sample:

```r
x <- c(1,2,3,4,6,8,9,10,12,14,15,16,20,24)
counts <- c(193,272,138,115,92,60,118,228,309,130,113,104,122,206)
prop <- round(counts/sum(counts),3)
names(prop) <- c(1,2,3,4,6,8,9,10,12,14,15,16,20,24)
prop
```

For example, the highest increase occurs at 2 annual visits, and is equal to 0.124, the proportion of customers in the sample who visited the shop 2 times in the last year.
""",
"images": [],
}

ex2["2_7b"] = {
"title": "Ex 2.7b — Maximum number of visits made by the 20% least regular customers",
"content": """**Question.** What is the maximum number of visits to the mall made by the 20% least regular customers?

""" + EX27_TABLE + """

---

**Answer.** The required value is the 20th percentile, $p20$, which separates (at least) the 20% of the most frequent customers from the remaining 80% of customers. Looking at the curve of the cumulative frequencies near 0.20 (20%) is 2, so that $p20 = 2$. The 20% of customers who visit the shop less frequently reported at most 2 visits per year. The same result is obtained by considering the table with the cumulative relative frequencies:

| Nr_Visits | 1 | 2 | 3 | 4 | 6 | 8 | 9 | 10 | 12 | 14 | 15 | 16 | 20 | 24 |
|----------:|--:|--:|--:|--:|--:|--:|--:|---:|---:|---:|---:|---:|---:|---:|
| Cum prop  | 0.088 | 0.212 | 0.275 | 0.327 | 0.369 | 0.396 | 0.45 | 0.554 | 0.694 | 0.753 | 0.804 | 0.851 | 0.906 | 1.000 |
""",
"images": [],
}

ex2["2_7c"] = {
"title": "Ex 2.7c — Five-number summary of Nr_visits",
"content": """**Question.** Determine the five-number summary of `Nr_visits`.

""" + EX27_TABLE + """

---

**Answer.** The minimum and maximum number of observed visits are 1 and 24. The first quartile is the value where the cumulative frequency reaches or exceeds 0.25, and the median is 10 (first value where cumulative frequency greater than or equal to 0.5); the third quartile is 14 (first value with cumulative frequency greater than or equal to 0.75):

| min | $Q_1$ | Me | $Q_3$ | max |
|----:|------:|---:|------:|----:|
| 1   | 3     | 10 | 14    | 24  |
""",
"images": [],
}

ex2["2_7d"] = {
"title": "Ex 2.7d — Range, IQR and other measures for Nr_visits",
"content": """**Question.** Based on the five-number summary in c), determine the range and the interquartile range of `Nr_visits` and discuss their interpretation.

""" + EX27_TABLE + """

---

**Answer.** From the five-number summary statistics, it is possible to determine the range of variation and the interquartile range, which are respectively $R = 24 - 1 = 23$ and $IQR = Q_3 - Q_1 = 14 - 1 = 13$. Observe that the range is high in relative terms: some clients visited the shop only once, whereas other almost twice a month. Similar considerations hold for the interquartile range, assessing the width of the interval including the 50% of the central data. Within this group there are clients who visited the shop only 3 times a year and clients who visited the shop 14 times. About one third of the average number of visits was needed; this group is quite heterogeneous too, particularly if one considers than the interviewed clients visited the shop a maximum of 24 times. Note actually that the interval for the first three (lower) quartiles covers about half of the entire range $(IQR/R = 11/23 = 0.48)$. Thus, also central data and not only the tails (the whiskers in the boxplot) are quite heterogeneous.
""",
"images": [],
}

ex2["2_7e"] = {
"title": "Ex 2.7e — Boxplot of Nr_visits and its asymmetry",
"content": """**Question.** Build the boxplot of `Nr_visits`. Are there outliers? What can be said about the asymmetry?

""" + EX27_TABLE + """

---

**Answer.** To build the boxplot, we preliminarily check whether there are outliers. Since $Q_3 + 1.5\\cdot IQR = 30.5 > \\text{max}$ and $Q_1 - 1.5\\cdot IQR = -13.5 < \\text{min}$, we conclude that there are no extreme values and based on the five-number summary reported at the previous point, we obtain:

```r
distr.plot.x(Nr_visits, plot.type="boxplot")
```

A typical form of asymmetry is not directly identifiable from the plot: we have in fact that the left whisker has length $Q_1 - \\text{min} = 3 - 1 = 2$ smaller than that of the right whisker, $\\text{max} - Q_3 = 24 - 14 = 10$, while the box is asymmetrical on the left, as $Me - Q_1 = 10 - 3 = 7$, and $Q_3 - Me = 14 - 10 = 4$. Therefore, about the 25% of the less regular clients (lower whisker in the plot) has a quite homogeneous behavior as for the number of visits to the mall, whereas the most regular clients (upper whisker in the plot), including also clients who visited the mall up to twice a month, are more heterogeneous. Opposite considerations hold for the clients with a "more standard/central" number of visits. The group of "central" less regular clients (lower part of the box) is slightly more heterogeneous compared to the group of "central" more regular clients (upper part of the box).
""",
"images": ["statistics/images/ex2_7e-nrvisits-boxplot.png"],
}

ex2["2_7f"] = {
"title": "Ex 2.7f — Mean, SD and CV for Nr_visits",
"content": """**Question.** Compute the mean, SD, and CV of `Nr_visits`. Comment on the dispersion.

""" + EX27_TABLE + """

---

**Answer.** The mean and the standard deviation can be obtained based on the standard formulas:

$$
\\bar x = \\sum_k f_k \\cdot x_k, \\qquad
\\sigma = \\sqrt{\\frac{n}{n-1}\\big[\\sum_k f_k\\cdot x_k^2 - \\bar x^2\\big]}
$$

| Nr_Visits | 1 | 2 | 3 | 4 | 6 | 8 | 9 | 10 | 12 | 14 | 15 | 16 | 20 | 24 |
|----------:|--:|--:|--:|--:|--:|--:|--:|---:|---:|---:|---:|---:|---:|---:|
| Prop $f_k$ | 0.088 | 0.124 | 0.063 | 0.052 | 0.042 | 0.027 | 0.054 | 0.104 | 0.140 | 0.059 | 0.051 | 0.047 | 0.055 | 0.094 |

Therefore, the sample mean is approximately equal to $\\sum_k f_k\\cdot x_k = 11.018$, and the sample variance is approximately $\\sum_k f_k\\cdot x_k^2 - \\bar x^2 = 49.26316$, i.e. $7.018$.

```r
x  <- c(1,2,3,4,6,8,9,10,12,14,15,16,20,24)
counts <- c(193,272,138,115,92,60,118,228,309,130,113,104,122,206)
prop <- round(counts/sum(counts),3)
x_bar <- sum(x*prop)
s <- sqrt( (sum((x^2)*prop) - x_bar^2) )
cv <- s/x_bar*100
x_bar
## [1] 10.106
s
## [1] 7.018772
cv
## [1] 69.4515%
```

Differences with the computed from the table is due to rounding. The coefficient of variation is approximately 65%. This indicates that the data are very dispersed (cv > 30%): the standard deviation is in fact between 60% and 70% of the average number of visits. This information is consistent with the heterogeneity of the data revealed by the box plot, with the heterogeneity referring to the tails (the whiskers) and the most/least regular customers (lower and upper part of the box) lying between 1 and 3 visits.
""",
"images": [],
}

# ============== EXERCISE 2.8 (customer_habits Margin_perc) ==============

ex2["2_8a"] = {
"title": "Ex 2.8a — Build the Margin_perc variable",
"content": """**Question.** Build the new variable `Margin_perc` defined as $\\text{Margin\\_perc} = 100\\cdot (Unit\\_Price/Unit\\_Cost - 1)$.

---

**Answer.** We can create the new variable as:

```r
Margin_perc <- 100*(customer_habits$Unit_Price/customer_habits$Unit_Cost - 1)
```

This command does not change the original variables but creates a new one as a separate object in the workspace: this may be reasonable if you do not intend to preserve it for later analysis. Nothing is printed in the console, but this new object is added to the workspace.
""",
"images": [],
}

ex2["2_8b"] = {
"title": "Ex 2.8b — Five-number summary + mean + SD + boxplot for Margin_perc",
"content": """**Question.** Compute the five-number summary, mean and standard deviation of `Margin_perc`, and represent through a boxplot.

---

**Answer.** We obtain all the desired synthesis measures with the following command:

```r
distr.summary.x(Margin_perc, stats=c("fivenumber", "mean", "sd"),
                digits=2, data=customer_habits)
##  n n.a min   q1 median  q3 max
## 34866 0 -40.71 6.57 17.36 29.15 99
##  n n.a mean sd
## 34866 0 18.2 17.82
```

and the corresponding boxplot is given by:

```r
distr.plot.x(Margin_perc, plot.type="boxplot")
```
""",
"images": ["statistics/images/ex2_8b-margin-boxplot.png"],
}

ex2["2_8c"] = {
"title": "Ex 2.8c — Dispersion of Margin_perc + boxplot interpretation",
"content": """**Question.** Compute the dispersion measures of `Margin_perc` and discuss what they show together with the boxplot.

---

**Answer.** The dispersion measures for the variable are:

```r
distr.summary.x(Margin_perc, stats="dispersion")
##  n n.a range IQrange   sd      var      cv
## 34866 0 139.72 22.57 17.82 317.44  0.98
```

Both the summary values and the boxplot show a very concentrated distribution, especially for the 50% of data around the median; in fact, comparing the range (137.71) with the interquartile range (22.57) one observes that the 50% central transactions in terms of margin percentage lie within a range (with extremes 6.57% and 29.15%, first and third quartiles) whose width is much smaller than that of the interval containing all the data (with extremes -40.71% and 99%, the minimum and maximum). The latter is indeed about six times wider.

In the boxplot, numerous extreme values are also highlighted, related both to purchases of products sold with high discounts (very negative extreme values) and to purchases of products on which the margin was particularly high. Further investigation of individual products and product types would be necessary to draw more precise conclusions on transactions involving products sold with such extreme margins.

The standard deviation is 17.82%, and indicates that on average the deviation from the mean is 17.82%, so transactions are "standard" if they have margins close to 0 (18.2% - 17.82% = 0.38%), being therefore sold at cost price, or margins of about 36% (18.2% + 17.82% = 36.02%), i.e. with a cost increased by about one third. Without reference or comparison values, it is difficult to assess whether the variability is low or high. However, we can assess the variability in relation to the mean percentage of 18.2% using the coefficient of variation (whose ratio between the standard deviation and the mean, possibly multiplied by 100), which is 0.98 (98%). A value equal to or close to 1 indicates that the average variation in the margin percentage is approximately of the same order of magnitude as the mean level: the products in the dataset therefore have very different margins, although, as the boxplot shows, the variability is mainly caused by the presence of very extreme values.
""",
"images": ["statistics/images/ex2_8b-margin-boxplot.png"],
}

ex2["2_8d"] = {
"title": "Ex 2.8d — Three claims on the sign of Margin_perc",
"content": """**Question.** Evaluate whether:
- at least 75% of transactions have a positive sales margin;
- in at least 60% of transactions the sales margin is between 10% and 30%;
- a maximum of 10% of transactions have a sales margin lower than $-30\\%$.

---

**Answer.** Assessing whether at least 75% of the transactions have a positive sales margin of the single product is straightforward, since the first quartile is positive and thus at least in the 75% of the cases the sales margin is greater than 6.57. One could also directly assess the percentage of interest by considering:

```r
# percentage of transactions relating to products with a positive margin
mean(Margin_perc>0)
## [1] 0.8497103
```

In the 84% of the transactions, the sales margin is greater than 0. To assess whether at least in the 60% of the transactions the margin of the single product is between 10% and 30%, we consider:

```r
mean(Margin_perc >= 10 & Margin_perc <= 30)
## [1] 0.4418345
```

So the condition **is not fulfilled**. Finally, to assess whether at most 10% of transactions have a negative margin of less than $-30\\%$ we consider:

```r
mean(Margin_perc < -30)
## [1] 0.002208455
```

which returns a very small percentage, 0.22%, of transactions with margins below $-30\\%$. The same conclusion could be reached by noting that:

```r
distr.summary.x(Margin_perc, stats="p10")
##  n n.a p10
## 34866 0 -3.89
```

Since the 10-th percentile is equal to $-3.89$, it is evident that the percentage of transactions related to products with a sales margin of less than $-30$ will necessarily be less than 10%.
""",
"images": [],
}
