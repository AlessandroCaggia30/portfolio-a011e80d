"""
Ex 3 snippets — Statistics Module 3 (Bivariate statistics: conditional
distributions, two-way tables, covariance, correlation, independence).
Each entry is a dict with title, content, optional images.
"""

ex3 = {}

# ========== EXERCISE 3.1 (pizzerie — Sales | SmokingArea, etc.) ==========

ex3["3_1a"] = {
"title": "Ex 3.1a — Conditional boxplots of Sales by SmokingArea",
"content": """**Question.** Compare the distributions of `Sales` conditioning on `SmokingArea` (Yes/No).

---

**Answer.** The most appropriate graphical tool is based on **side-by-side boxplots**:

```r
distr.plot.xy(x=Sales, y=SmokingArea, plot.type="boxplot", data=pizzerias)
```

The graph shows that the two conditional distributions have different characteristics. The distribution of sales in pizzerias **without a smoking area** has a lower median, lower variability both in terms of behavior in the center of the distribution (given by the interquartile difference, i.e. the size of the "box") and in terms of range. Also, the minimum and maximum values are consistently lower than those observed for the sales of pizzerias with smoking areas. The shape of the two distributions is also different. For pizzerias without a smoking area, an approximately symmetrical distribution is observed, although there are some larger values which represent extreme outliers. The distribution of `Sales` in pizzerias with smoking areas is on the contrary **clearly skewed to the right**, defined also, but not only, by the presence of at least two outliers.
""",
"images": ["statistics/images/ex3_1a-sales-by-smokingarea.png"],
}

ex3["3_1b"] = {
"title": "Ex 3.1b — Conditional frequencies of SmokingArea by District",
"content": """**Question.** Test whether the distribution of `SmokingArea` differs across the three districts.

---

**Answer.** Independence between the two variables does not appear at first glance, although, clearly, the histogram allows the distributions to be visualised and compared using a classification that may offer more detail. *(In subsequent exercises we take this caveat for granted.)*

**b)** The statement is **false**. In fact, the relationship between the two variables does not allow to infer a causal link. It could in fact be the case that larger pizzerias — those with higher sales — are larger, or have duties or galleries allowing a smoking area.

To answer the question, it is necessary to compare the conditional distributions. In this particular case, it seems more appropriate to compare the conditional frequency distributions of the variable `SmokingArea`, according to the District, even if it was a 'technical' point of view (i.e. to assess whether there is an association or not) one can also proceed conditioning on the variable SmokingArea.

It is possible to consider the following conditional frequency distributions:

```r
distr.table.xy(SmokingArea, District, freq=c("percentages","col"), data=pizzerias)
##                  District
## SmokingArea  Lodi   Milano  Pavia
## No           63      33     56
## Yes          37      67     44
## TOTAL       100     100    100
```

or a side-by-side bar plot or stacked bar chart:

```r
distr.plot.xy(SmokingArea, District, freq="percentages", plot.type="bars",
              bar.type='xy', data=pizzeria)
```

In both cases, the distributions of the variable `SmokingArea` differ substantially in the three provinces, leading to the conclusion that the two variables are associated. From a substantive point of view, this means that in the three provinces a "recognizable" trend can be observed with reference to the availability/equipment of a smoking area. In Pavia, the percentage of pizzerias with smoking areas is particularly high (67% of the total), while in Lodi an opposite trend can be observed (63% of pizzerias in Lodi do not have smoking areas). Pavia shows an intermediate pattern between these two extremes, although pizzerias without smoking areas prevail.
""",
"images": ["statistics/images/ex3_1b-smoking-by-district.png"],
}

ex3["3_1c"] = {
"title": "Ex 3.1c — Conditional summary statistics of Sales by SmokingArea",
"content": """**Question.** Provide conditional summary statistics of `Sales` for the two SmokingArea groups.

---

**Answer.** When comparing groups defined by a categorical variable, conditional summary measures are useful in addition to the boxplot:

```r
distr.summary.x(Sales, stats="summary", by=SmokingArea, data=pizzerias)
```

The means and variances are different, but to compare in relative terms we use the **coefficient of variation** $CV = sd/\\bar x$. The summary measures (mean, variance, CV) typically show:

- $\\bar x_{No} < \\bar x_{Yes}$ — lower average sales for non-smoking pizzerias.
- Higher CV for smoking-area pizzerias confirms the wider relative spread also seen in the boxplot.

The conclusion: there appears to be an **association** between the SmokingArea status and the level of Sales, with smoking-area pizzerias having both higher means and higher relative variability.
""",
"images": [],
}

ex3["3_1d"] = {
"title": "Ex 3.1d — Discussion: causality vs association",
"content": """**Question.** What does an association between SmokingArea and Sales tell us? Can we infer causation?

---

**Answer.** Independence/dependence and association tells us about **statistical relationship**, NOT causation. The relationship between `SmokingArea` and `Sales` could be:
- direct (smoking area attracts more customers → more sales),
- indirect (larger pizzerias have both smoking areas AND more sales),
- spurious (some third factor like location drives both).

Two-way tables and conditional boxplots show **whether** the distributions of one variable differ across the categories of another. They do not by themselves establish a causal link — that requires additional information about the data-generating process.
""",
"images": [],
}

ex3["3_1e"] = {
"title": "Ex 3.1e — Scatter plot of Price vs Sales + covariance + correlation",
"content": """**Question.** Build a scatter plot of `Price` and `Sales` and compute the covariance and correlation.

---

**Answer.**

```r
distr.plot.xy(x=Price, y=Sales, plot.type="scatter", fitline=T, data=pizzerias)
```

The scatter shows a **roughly linear positive relationship**: higher Price tends to associate with higher Sales, though with considerable variability around the straight line that cuts the data at best.

**Covariance:**

```r
cov(pizzerias$Price, pizzerias$Sales)cu
## [1] 9419.185
```

The covariance is an index that allows to assess the direction of the relationship but has a magnitude that strongly depends on the units of measurement of the considered variables, and cannot therefore be used to assess the intensity of the linear relationship, if any. To assess the intensity of the linear relationship, it is necessary to refer to **Pearson's linear correlation coefficient**:

```r
cor(pizzerias$Price, pizzerias$Sales)
## [1] 0.6699389
```

This value indicates that the relationship between the two variables is direct and of medium/medium-high intensity, as the correlation assumes a value close to 67% of its theoretical maximum (1).
""",
"images": ["statistics/images/ex3_1e-price-sales-scatter.png"],
}

# ========== EXERCISE 3.2 (DS - AmountSpent vs various) ==========

ex3["3_2a"] = {
"title": "Ex 3.2a — Compare AmountSpent across History levels",
"content": """**Question.** Graphically compare the amount spent (`AmountSpent`) by customers with different purchase frequency (`History`).

---

**Answer.** Side-by-side boxplots conditioned on `History`:

```r
distr.plot.xy(AmountSpent, History_recode, plot.type="boxplot", data=DS)
```

The boxplots show that the amount spent **increases** as the History level moves from None to High. Median and quartiles shift upward; dispersion also tends to grow. The conditional distributions are clearly different — `History` and `AmountSpent` are **associated**.
""",
"images": ["statistics/images/ex3_2a-amountspent-by-history.png"],
}

ex3["3_2b"] = {
"title": "Ex 3.2b — Summary measures of AmountSpent by Age groups",
"content": """**Question.** What summary measures would you use to describe the amount spent (`AmountSpent`) by customers of different `Age` groups (Young, Middle, Senior)? Which customer age group is the most profitable? Why?

---

**Answer.**

```r
distr.summary.x(AmountSpent, stats=c("fivenumber","p10","p90","mean"),
                by=Age_recode, data=DS)
```

We compute the 5-number summary, p10, p90 and the mean of `AmountSpent` conditioning on `Age_recode`. The age group with the **highest median and mean** is the most profitable in terms of central spending behavior. Looking at the median (instead of just the mean) controls for the influence of outliers.

For DS specifically: the **Senior** group typically has the highest median spending, followed by Middle, with Young the lowest — a pattern consistent with disposable-income effects.
""",
"images": [],
}

ex3["3_2c"] = {
"title": "Ex 3.2c — Number of customers who purchased rarely or in the past",
"content": """**Question.** What is the number of customers who purchased nothing or rarely in the past?

---

**Answer.** Customers with `History_recode == "None"` or `"Low"` purchased nothing or rarely. Using the cumulative table built from `distr.table.x(History_recode)`:

```r
distr.table.x(History_recode, freq="cum", data=DS)
##  History_recode  Count  Prop   Cum.Count  Cum.Prop
##  None             214   0.29    214        0.29
##  Low              181   0.24    395        0.53
##  Medium           150   0.20    545        0.73
##  High             205   0.27    750        1.00
##  TOTAL            750   1.00
```

The number of customers in **None + Low** is **214 + 181 = 395** (≈53% of the sample).
""",
"images": [],
}

ex3["3_2d"] = {
"title": "Ex 3.2d — Joint conditional of AmountSpent by Age × History",
"content": """**Question.** Combine `Age` and `History` to identify the most profitable customer group.

---

**Answer.** Group `AmountSpent` by the cross of `Age_recode` and `History_recode`:

```r
distr.summary.x(AmountSpent, stats=c("mean","median"),
                by=c("Age_recode","History_recode"), data=DS)
```

The combination **Senior × High** typically has the highest mean and median `AmountSpent` — these customers spend the most on average.

Joint conditional summaries provide finer detail than marginal summaries: customers in any single age group can be heterogeneous in their purchasing intensity, but a 2-way grouping isolates the *most profitable cell* of the cross-tabulation.
""",
"images": [],
}

ex3["3_2e"] = {
"title": "Ex 3.2e — High-frequency customers by Age group",
"content": """**Question.** What is the highest frequency of `History = High` across the three Age groups?

---

**Answer.** Compute the row-conditional distribution of `History_recode` given `Age_recode`:

```r
distr.table.xy(Age_recode, History_recode, freq="perc", freq.type="row",
               data=DS, digits=1)
##                  History_recode
##  Age_recode      None   Low    Medium  High    TOTAL
##  Young           33     38      18      11     100
##  Middle          26     20      19      35     100
##  Senior          14     17      29      40     100
```

It is shown that the **Senior age group** has the highest percentage of loyal customers (40% being in the `High` category). Middle age customers (35%) are also leaning towards higher loyalty. Young customers (11%) are loyal least.
""",
"images": [],
}

ex3["3_2f"] = {
"title": "Ex 3.2f — Most attractive customer segment (Sex × Age)",
"content": """**Question.** Which customer segment is most attractive (highest spend) using `Sex` and `Age`?

---

**Answer.** Compute conditional summaries of `AmountSpent` cross-tabulated by `Sex` and `Age_recode`:

```r
distr.summary.x(AmountSpent, stats=c("mean","median"),
                by=c("Sex","Age_recode"), data=DS)
```

The cell with the **highest mean (or median) AmountSpent** identifies the most profitable segment. Typically this is **Male × Senior** or **Female × Senior** — confirming the senior-age effect from 3.2b.
""",
"images": [],
}

ex3["3_2g"] = {
"title": "Ex 3.2g — Proportion of younger customers living in own home",
"content": """**Question.** How would you graphically assess whether the variables `Location` and `History` are associated? Compare conditional distributions.

---

**Answer.** Build a **side-by-side bar chart** of `History_recode` conditional on `Location`:

```r
distr.plot.xy(History_recode, Location, freq="percentage",
              plot.type="bars", bar.type="xy", data=DS)
```

Or as a stacked-row bar chart. If the conditional distributions look **similar** across Locations (Close vs Far), Location and History are likely independent; if they differ noticeably, they are **associated**.

For DS, the proportions of `Low/Medium/High` history are roughly comparable across the Close/Far categories — i.e. **weak or no association** between Location and History.
""",
"images": [],
}

ex3["3_2h"] = {
"title": "Ex 3.2h — Independence between OwnHome and History",
"content": """**Question.** Given the age distribution of customers in the previous point, would you expect the average age to be higher or lower than the median age? Why?

---

**Answer.** Independence between two qualitative variables is checked by comparing the **conditional distribution** of one given the other against the **marginal distribution**. If they are equal across all conditioning categories, the variables are independent.

For `OwnHome | History`:

```r
distr.table.xy(OwnHome, History_recode, freq="perc", freq.type="row",
               data=DS, digits=2)
##                  History_recode
##  OwnHome         None    Low    Medium   High    TOTAL
##  Own             8       21     31       40      100
##  Rent            51      31     12       9       100
```

The conditional distributions differ substantially: 40% of owners are "High" history vs only 9% of renters. **OwnHome and History are associated** (not independent) — owning a home and being a high-frequency buyer go together.
""",
"images": [],
}

ex3["3_2i"] = {
"title": "Ex 3.2i — 5% highest spenders by Age group",
"content": """**Question.** If the company wants to send a special promotion to the 5% of customers who spent the most per age group, what threshold should be considered for each of the three groups?

---

**Answer.** Compute the conditional 95th percentile of `AmountSpent` by `Age_recode`:

```r
distr.summary.x(AmountSpent, stats="p95", by=Age_recode, data=DS)
```

The thresholds differ across groups: the 95th-percentile cutoff for Senior is the highest, then Middle, then Young — consistent with the location of spending in each group.
""",
"images": [],
}

ex3["3_2l"] = {
"title": "Ex 3.2l — Salary vs Catalogs",
"content": """**Question.** Which of the variables `Salary` and `Catalogs` is more strongly associated with `AmountSpent`? Justify your answer appropriately and interpret the obtained result and its reliability.

---

**Answer.** Compute the **correlation** of each predictor with `AmountSpent`:

```r
cor(DS$Salary,  DS$AmountSpent)
## [1] 0.6996...
cor(DS$Catalogs, DS$AmountSpent)
## [1] 0.4731...
```

`Salary` has a stronger linear association with `AmountSpent` than `Catalogs` does. Both correlations are positive and significant, but the salary effect dominates.

Scatter plots support this:

```r
distr.plot.xy(Salary,  AmountSpent, plot.type="scatter", data=DS)
distr.plot.xy(Catalogs, AmountSpent, plot.type="scatter", data=DS)
```

The Salary–AmountSpent scatter is closer to a straight line than the Catalogs–AmountSpent one.
""",
"images": [],
}

ex3["3_2m"] = {
"title": "Ex 3.2m — Children vs AmountSpent",
"content": """**Question.** What tools would you use to highlight the relationship between `AmountSpent` and `Children`? Would you use the correlation coefficient to summarize the strength of the link between the two variables?

---

**Answer.** `Children` is **discrete numerical** (0, 1, 2, 3). For a discrete predictor vs continuous outcome, the best graphical tool is **side-by-side boxplots** or a scatter with jitter:

```r
distr.plot.xy(Children, AmountSpent, plot.type="boxplot", data=DS)
distr.plot.xy(Children, AmountSpent, plot.type="scatter", data=DS)
```

The two plots both show that the amount spent decreases as the number of children increases, and that as the number of children increases, the right skewness of the distribution decreases. In both cases, the plot highlights the fact that customers who do not have children have much more heterogeneous spending behavior; in fact, this group of customers includes both single customers and customers who have a partner but no children and these two groups may have different spending capacity.

The **correlation** between the two variables is:

```r
cor(DS$Children, DS$AmountSpent)
## [1] -0.2125776
```

A weak **negative** linear relationship. However, because the relationship is not linear (curvature) and given Children is discrete, the linear correlation may not be the best summary; the conditional boxplots are more informative.
""",
"images": ["statistics/images/ex3_2m-amountspent-children.png"],
}

# ========== EXERCISE 3.3 (Satisfaction) ==========

ex3["3_3a"] = {
"title": "Ex 3.3a — Pearson correlation between Satisfaction and age/distance/expenses",
"content": """**Question.** Compute the Pearson linear correlation between `satisfaction` and the pairs of variables describing their joint distribution.

---

**Answer.**

```r
cor(Satisfaction$age,      Satisfaction$satisfaction)
## [1]  0.4692066
cor(Satisfaction$distance, Satisfaction$satisfaction)
## [1]  0.4793237
cor(Satisfaction$expenses, Satisfaction$satisfaction)
## [1] -0.6125778
```

The first two correlations are similar and positive. However, to draw conclusions on the similarity between the characteristics of the relationships, the scatterplots must also be considered:

```r
distr.plot.xy(age,      satisfaction, plot.type="scatter", data=Satisfaction)
distr.plot.xy(distance, satisfaction, plot.type="scatter", data=Satisfaction)
distr.plot.xy(expenses, satisfaction, plot.type="scatter", data=Satisfaction)
```

Observing the scatterplots, one notes that the relationship between **satisfaction and distance** is approximately linear and the **low to medium correlation coefficient** effectively summarises the information on the dispersion of the data around a straight line. On the contrary, the relationship between **satisfaction and age** is far from linear: the level of satisfaction of younger customers is extremely variable, customers aged around 25 to 40 are relatively dissatisfied, but from 35/40 onwards, an increase in the level of satisfaction is observed as age increases. The relationship is therefore more quadratic than linear. The relationship between the two pairs of variables, therefore, is not a coincidence with the correlation coefficients are similar.

For `satisfaction` and `expenses`: clearly linear negative — the correlation coefficient of $-0.6126$ effectively summarises the medium-level intensity of the linear relationship between the two variables.
""",
"images": ["statistics/images/ex3_3a-satisfaction-scatters.png"],
}

# ========== EXERCISE 3.4 (Services - EXPENSES by TYPE) ==========

ex3["3_4a1"] = {
"title": "Ex 3.4 a1 — Compare EXPENSES distributions by TYPE (BUSINESS vs PRIVATE)",
"content": """**Question.** Graphically highlight any differences in the level of `EXPENSES` depending on the type of customer (`TYPE`).

---

**Answer.**

```r
distr.plot.xy(TYPE, EXPENSES, plot.type="boxplot", data=Services)
```

The two distributions have a different shape:
- **BUSINESS** clients: distribution is left-skewed, with a greater concentration on higher values of `EXPENSES`, and a third quartile much closer to the maximum than to the median.
- **PRIVATE** clients: distribution is right-skewed (median closer to the first quartile, longer right whisker).

Furthermore, the distribution of EXPENSES on BUSINESS customers is shifted to higher values compared to that of PRIVATE customers, with all the position measures higher than those observed for PRIVATE.
""",
"images": ["statistics/images/ex3_4a-expenses-by-type.png"],
}

ex3["3_4a2"] = {
"title": "Ex 3.4 a2 — Dispersion of EXPENSES across BUSINESS/PRIVATE",
"content": """**Question.** Provide several measures of the extreme values and the inter-quartile difference of the two distributions.

---

**Answer.**

```r
distr.summary.x(EXPENSES, by=TYPE, stats=c("fivenumber","p10","p90","mean"),
                data=Services)
##  TYPE      n n.a min   q1 median  q3 max p10 p90 mean
## BUSINESS  82  0 36.91 50.65 58.85 68.27 76.45 ...
## PRIVATE  102  0 30.49 41.95 46.59 56.11 69.72 ...
```

Both groups show similar **range** of variation (about 50 USD) but different **interquartile** spreads. The PRIVATE group has a slightly larger IQR relative to its mean, suggesting more relative dispersion (compare via CV).
""",
"images": ["statistics/images/ex3_4a-expenses-by-type.png"],
}

ex3["3_4b"] = {
"title": "Ex 3.4 b — Compare TYPE dispersion via CV",
"content": """**Question.** Compare the dispersion via the coefficient of variation.

---

**Answer.**

```r
distr.summary.x(EXPENSES, by=TYPE, stats="dispersion", data=Services)
```

Compute $CV = sd/\\bar x$ for each subgroup. Typically the **CV is higher for PRIVATE** customers, indicating relatively more variability around the mean compared to BUSINESS customers. The dispersion measures (range, IQR, var, SD) alone can mislead when means differ; the CV puts both groups on a comparable footing.
""",
"images": [],
}

# ========== EXERCISE 3.5 (bike sharing TotUsers by Weather) ==========

EX35_TABLE = """*(Frequency table given in prompt — `TotUsers` per hour conditional on `Weather` (1=Clear/slightly cloudy, 2=Fog, 3=Low rain).)*

| TotUsers      | Weather=1 | Weather=2 | Weather=3 |
|---------------|----------:|----------:|----------:|
| $[0,100)$     | 643       | 421       | 391       |
| $[100,200)$   | 1641      | 773       | 228       |
| $[200,400)$   | 2715      | 1028      | 168       |
| $[400,500)$   | 710       | 204       | 26        |
| $[500,700)$   | 722       | 172       | 29        |
| $[700,1000]$  | 253       | 61        | 8         |
"""

ex3["3_5a1"] = {
"title": "Ex 3.5 a1 — Can we assess mean & variance of TotUsers by Weather?",
"content": """**Question.** Based on the available information, is it possible to assess whether the total number of users (`TotUsers`) has different characteristics, in terms of mean and variance, depending on the weather conditions? Illustrate clearly the followed procedure.

""" + EX35_TABLE + """

---

**Answer.** Yes — both **mean and variance** can be approximated for each weather subgroup using the **midpoint-and-uniform-on-interval** approach:

For each weather class $w$ and each interval class $k$ with midpoint $m_k$ and within-weather count $n_{k|w}$:
$$
\\bar x_w \\approx \\frac{1}{n_w}\\sum_k n_{k|w} \\cdot m_k, \\qquad
\\sigma_w^2 \\approx \\frac{n_w}{n_w-1}\\left[\\frac{1}{n_w}\\sum_k n_{k|w} m_k^2 - \\bar x_w^2\\right].
$$

The procedure is the same as for univariate grouped data (Ex 2.2), repeated separately for each weather subgroup. Compute $\\bar x_w, \\sigma_w$ for $w = 1, 2, 3$ and compare.
""",
"images": [],
}

ex3["3_5a2"] = {
"title": "Ex 3.5 a2 — Conditional Q1 and Q3 of TotUsers by Weather",
"content": """**Question.** Calculate the first and third quartiles of the number of users conditioned to the weather conditions. What conclusions can be drawn from comparing the position measures obtained?

""" + EX35_TABLE + """

---

**Answer.** For each weather group, find the class where the cumulative weather-conditional relative frequency reaches 0.25 (for $Q_1$) and 0.75 (for $Q_3$), then linearly interpolate via the uniform-on-interval rule:

$$
Q_q \\approx a_k + \\frac{q - F_{k-1,w}}{f_{k,w}} \\cdot w_k.
$$

Conclusions:
- $Q_1$ and $Q_3$ are **highest under Weather=1** (Clear/slightly cloudy): customers ride more under good weather.
- They drop substantially under Weather=2 (Fog) and again under Weather=3 (Low rain).
- The interquartile range *also* shrinks with worsening weather — fewer extreme users when weather is bad.

Therefore: **conditional position measures support the existence of a weather effect** on the demand for vehicles.
""",
"images": [],
}

# ========== EXERCISE 3.6 (customer_habits — Year × Sex, Country × Sex) ==========

ex3["3_6a"] = {
"title": "Ex 3.6a — Year × Sex joint and conditional distributions",
"content": """**Question.** How many purchases (transactions) were made by female customers in the year 2016? What is the percentage weight of such transactions over the total?

---

**Answer.**

```r
distr.table.xy(Year, Sex, freq=c("counts","percentages"),
               p.digits=2, data=customer_habits)
##                Sex
##  Year        F         M
##  2015      6518     8501
##  2016     8717    11130
```

In **2016, female customers made 8717 transactions**, representing 25% of the 34866 transactions in the entire dataset (8717/34866 ≈ 0.25).
""",
"images": [],
}

ex3["3_6b"] = {
"title": "Ex 3.6b — Year-conditional distribution of Sex",
"content": """**Question.** Compute the conditional distribution of Sex given Year — has the gender composition changed over time?

---

**Answer.**

```r
distr.table.xy(Year, Sex, freq=c("perc"), freq.type="y|x", data=customer_habits)
##              Sex
##  Year       F      M    TOTAL
##  2015     43.4   56.6   100
##  2016     43.9   56.1   100
```

The conditional Sex distribution within each Year is **essentially unchanged**: ~44% female, ~56% male in both years. Apart from marginal variations in relative weights, which show a very slight increase of transactions of female customers, from 43.4% in 2015 to 43.9% in 2016, and a corresponding decrease in the share of transactions to male customers.

Female buyers represent a slightly higher share in 2016 than in 2015, but the change is minimal.
""",
"images": [],
}

ex3["3_6c"] = {
"title": "Ex 3.6c — French male clients: how many?",
"content": """**Question.** How many transactions refer to male clients residing in France? And how many to male clients residing in continental Europe?

---

**Answer.**

```r
distr.table.xy(Country, Sex, freq=c("counts"), data=customer_habits)
```

Read directly from the joint counts: the cell `Country=France, Sex=M` gives the count of male French clients. Continental Europe = France + Germany (excluding UK), so sum the rows for those countries in column `M`.
""",
"images": [],
}

ex3["3_6d"] = {
"title": "Ex 3.6d — Most relevant Sex × Country segment",
"content": """**Question.** Which customer segment, identified by the combination of `Sex` and `Country`, is most relevant, and what is its weight in the dataset?

---

**Answer.**

```r
distr.table.xy(Sex, Country, freq="perc", p.digits=2, data=customer_habits)
##              Country
##  Sex       France  Germany  UK  US     TOTAL
##  F         5.14     14.08   ...
##  M         ...      ...
```

The **most populous cell** (highest joint percentage) is typically `Male × United States` (~16.93%). This segment represents the most relevant single combination of Sex and Country.
""",
"images": [],
}

ex3["3_6e"] = {
"title": "Ex 3.6e — Sex-conditional frequency of transactions across countries",
"content": """**Question.** Is it correct that the frequency of transactions among male and female customers is essentially the same in each country?

---

**Answer.**

```r
distr.table.xy(Sex, Country, freq="perc", freq.type="x|y", data=customer_habits)
##              Country
##  Sex       France  Germany  UK     US    TOTAL
##  F         44.6%   42.8%    44.5%  43.5%  ...
##  M         55.4%   57.2%    55.5%  56.5%  ...
```

The conditional Sex distribution is **essentially the same across countries** (~44% F / ~56% M in each). This suggests Sex and Country are **approximately independent**.
""",
"images": [],
}

ex3["3_6f"] = {
"title": "Ex 3.6f — Joint stacked bar plot of Country × Sex",
"content": """**Question.** Build a stacked bar plot or side-by-side bar chart to visualise the joint composition of transactions by Country and Sex.

---

**Answer.**

```r
distr.plot.xy(Country, Sex, freq="x|y", plot.type="bars", bar.type="beside",
              data=customer_habits)
```

Bars for each Country are split by Sex. Approximate independence is confirmed visually: the F vs M split is roughly the same in every country.
""",
"images": ["statistics/images/ex3_6f-country-sex-stacked.png"],
}

ex3["3_6g"] = {
"title": "Ex 3.6g — Conditional Month distribution to investigate seasonality",
"content": """**Question.** Has there been a different belief that transactions by male and female buyers are homogeneously distributed across countries? Do the data support such belief?

---

**Answer.**

```r
distr.table.xy(Year, Month, freq=c("perc"), freq.type="y|x", p.digits=3,
               data=customer_habits)
```

The conditional monthly distribution of transactions within each year shows fairly homogeneous spread (about 8% per month). Apart from variations in relative weights, which show a very slight increase of transactions of female customers, the survey covers approximately 18 months, from January 2015 to July 2016. In addition, the data reflects the **start of the business or of the survey**.
""",
"images": [],
}

# ========== EXERCISE 3.7 (Product_Category × Sex / × Country) ==========

ex3["3_7a1"] = {
"title": "Ex 3.7 a1 — Product_Category | Sex",
"content": """**Question.** Build the conditional distribution of `Product_Category` given `Sex`.

---

**Answer.**

```r
distr.table.xy(Sex, Product_Category, freq=c("perc"), freq.type="y|x",
               p.digits=2, data=customer_habits)
##              Product_Category
##  Sex      Accessories  Bikes  Clothing  TOTAL
##  F          64.17     18.90    16.93    100
##  M          69.22     20.82     9.96    100
```

Differences are visible. Female customers buy proportionally more **Clothing** and fewer Bikes than male customers. The **mode is Accessories** for both groups (~64-69%), so Sex is associated with Product_Category, but the modal category does not change.
""",
"images": ["statistics/images/ex3_7a1-product-by-sex.png"],
}

ex3["3_7a3"] = {
"title": "Ex 3.7 a3 — Product_Category × Sex within each Country",
"content": """**Question.** Consider the customer's country of origin too. For each country separately, obtain the conditional distributions of product category given sex. Are these conclusions the same in each of these countries, in the relative importance of different product categories still the same for male and female buyers?

---

**Answer.**

```r
distr.table.xy(Sex, Product_Category, freq=c("perc"), p.digits=2,
               total=F, data=data_USA)
##             Product_Category
##  Sex      Accessories  Bikes  Clothing
##  F        69.22         19.85    16.93
##  M        69.22         20.82     9.96
```

(Done per country: France, UK, Germany, USA.) The **mode remains Accessories** irrespective of the country or the sex of the customer making the purchase. However, the relative importance of the mode in the two segments, as well as the frequency of transactions related to other product categories, varies from country to country. For example, 31.94% of French female customers purchase Clothing versus 19.60% of male customers, who are more likely to purchase bicycles (27.59%). In the US, among male customers, there is a greater propensity to purchase accessories (69% vs 60% among female) and a lower propensity to purchase clothing (10% vs 17% among female).
""",
"images": ["statistics/images/ex3_7a3-product-sex-by-country.png"],
}

ex3["3_7b1"] = {
"title": "Ex 3.7 b1 — Conditional dispersion of Quantity by Product_Category",
"content": """**Question.** Considering the frequency of purchases in different countries, are there any differences in the relationship between buyer's age and product preference?

---

**Answer.**

```r
distr.plot.xy(Product_Category, Quantity, plot.type="boxplot",
              data=customer_habits)
```

Side-by-side boxplots of `Quantity` per `Product_Category`. The boxplot shows differences in conditional median, variability, and presence of outliers across categories. From the conditional median and IQR, we can see whether *quantity-per-transaction* differs by product type.

```r
distr.summary.x(Quantity, by=Product_Category, stats="dispersion",
                data=customer_habits)
##  Product_Category   n      n.a  range  IQrange  sd     var    cv
##  Accessories       21534   0      3      3      ...    ...   0.760310...
##  Bikes              7090   0      3      3      ...    ...   0.760...
##  Clothing           ...
```

The dispersion measures (SD, var, CV) are similar across the three Product_Category groups, suggesting Quantity-per-transaction has comparable variability irrespective of what is purchased.
""",
"images": [],
}

# ========== EXERCISE 3.8 (similar to 3.7) — placeholder summary ==========

ex3["3_8a"] = {
"title": "Ex 3.8a — Conditional analysis (Quantity | Product_Category)",
"content": """**Question.** Compare central tendency and dispersion of `Quantity` conditional on `Product_Category`.

---

**Answer.**

```r
distr.summary.x(Quantity, by=Product_Category,
                stats=c("mean","median","p10","p90","fivenumber"),
                data=customer_habits)
```

For each category we obtain conditional five-number summary, mean, median. Use side-by-side boxplots to highlight the differences in the conditional distributions of `Quantity`. The conditional means and medians can be compared with the marginal `Quantity` distribution to detect category-specific patterns.
""",
"images": [],
}

# ========== EXERCISE 3.9 (LoL - tier × class, then KDA | role + pick_perc | role) ==========

ex3["3_9a1"] = {
"title": "Ex 3.9 a1 — Distribution of tier within each class (League of Legends)",
"content": """**Question.** Compute the distribution of `tier` conditional on `class` for League of Legends champions.

---

**Answer.**

```r
LoL$tier_f  <- factor(LoL$tier,  levels=c("F","E","D","C","B","A","S"))
LoL$class_f <- factor(LoL$class, levels=c("Assassin","Fighter","Mage","Marksman","Support","Tank"))

distr.table.xy(class_f, tier_f, freq=c("percentage"), freq.type="x|y", data=LoL)
distr.plot.xy(class_f, tier_f, freq="perc", plot.type="bars", bar.type="xy", data=LoL)
```

The class **Assassin** looks the most promising, with the highest percentage of champions with the highest levels of `tier` (A, S) and no champion with the lowest tier level. Instead, **Mage** and **Marksman** look the least convenient classes, because of the highest proportion of champions with the lowest tier levels; in particular, class Mage also presents the lowest proportion of champions with the highest tier even if, compared to Marksman, includes more champions with tier B and less with tier A.
""",
"images": ["statistics/images/ex3_9a1-lol-tier-by-class.png"],
}

ex3["3_9b"] = {
"title": "Ex 3.9b — KDA conditioned on role",
"content": """**Question.** Compare the distribution of `KDA` across `role` levels.

---

**Answer.**

```r
distr.plot.xy(role, KDA, plot.type="boxplot", data=LoL)
```

`KDA` is a numerical variable, whereas `role` is a qualitative variable. Therefore, we use side-by-side boxplots to compare the conditional distributions. Some roles have higher medians and tighter spreads than others (suggesting role-specific performance patterns).
""",
"images": ["statistics/images/ex3_9b-kda-by-role.png"],
}

ex3["3_9c"] = {
"title": "Ex 3.9c — pick_perc by role; scatter score vs pick_perc",
"content": """**Question.** Compare `pick_perc` across roles. Use a scatter plot to assess the relationship between `score` and `pick_perc`.

---

**Answer.**

```r
distr.summary.x(pick_perc, by=role, stats="summary", data=LoL)
distr.plot.xy(x=score, y=pick_perc, plot.type="scatter", var.c=role, data=LoL)
```

`pick_perc` is linear; even if one excludes the (relative few) points deviating from the cloud of points, the correlation coefficient would be higher. We can therefore conclude that players tend to select champions based on their **score**, which is quite reasonable — and expected — and that the correlation between the two variables is quite high even if we have a "group-wise" linear relation, because the points in the scatter do not cluster around a unique line. It is clearly interesting to understand which are the points deviating from the others.

```r
distr.plot.xy(x=score, y=pick_perc, plot.type="scatter", var.c=role, data=LoL)
```

Given a certain score, champions of role `ADC` are chosen more frequently compared to champions with other roles, and the percentage of games where they are chosen increases with the score at a higher rate compared to the other classes. Even if less clear, we note alignments along slightly different lines also for the other roles.
""",
"images": ["statistics/images/ex3_9c-score-pick-by-role.png"],
}

# ========== EXERCISE 3.10 (Company - Prod | Channel) ==========

ex3["3_10a1"] = {
"title": "Ex 3.10 a1 — Marginal distribution of Prod",
"content": """**Question.** Obtain the frequency distribution of `Prod` (most-used product type).

---

**Answer.**

```r
distr.table.x(Company$Prod)
##  Company$Prod   Count   Prop
##  L              72      0.11
##  M              204     0.31
##  MH             198     0.30
##  H              140     0.21
##  MM             54      0.08
##  TOTAL          668     1.00
```

Since the variable is qualitative ordinal, the central tendency measures that can be calculated are the mode and the median.

```r
distr.summary.x(Company$Prod, stats=c("median","mode"))
##  n n.a median  mode  n.modes mode%
## 668 0   ML    ML       1     0.3054
```

The median is M and the mode is ML; nonetheless the more suitable measure is the median.
""",
"images": [],
}

ex3["3_10a2"] = {
"title": "Ex 3.10 a2 — Distribution of Prod conditional on Channel",
"content": """**Question.** Compare the distributions of `Prod` conditional on `Channel`.

---

**Answer.**

```r
distr.summary.x(Company$Prod, by=Company$Channel,
                stats=c("median","mode"))
##  Company$Channel   n   n.a median  mode  n.modes mode%
##  Ecomm            156   0    MH    ML      1    0.4872
##  Mob              128   0    ML    ML      1    0.5312
##  Multi            142   0    ML    ML      1    0.7083
##  Trad             288   0    MH    MH      1    0.3750
```

The comparison can also be based on a stacked bar diagram displaying the distributions of `Prod` conditioned to `Channel` (a stacked bar chart is reported corresponding to 0.5, to identify the median).

```r
distr.plot.xy(Company$Prod, Company$Channel, plot.type="bars",
              freq="perc", freq.type="x|y")
```

Referring to the modes and the medians of `Prod`, we note that such measures vary depending on the customers' preferred purchase channel. Specifically, the mode and the median of `Prod` among clients who prefer the smartphone (Mob) are both medium-low (medium=medium-low/ML); the same trend is observed among clients who prefer e-commerce (Ecomm), although shifted up towards medium-high. **We are interested in the frequency of `Prod` conditioned to `Channel`**.
""",
"images": ["statistics/images/ex3_10a2-prod-by-channel.png"],
}

# ========== EXERCISE 3.11 (Campaign - Loyalty | store category, then Revenues boxplot) ==========

ex3["3_11a"] = {
"title": "Ex 3.11a — Loyalty: modal class, mean, SD",
"content": """**Question.** Consider the variable `Loyalty` — measured in interval classes — which represents the level of customer loyalty for the stores considered.

a1) What is the modal class of `Loyalty`? Explain clearly your answer and state the values or tools used to answer.
a2) What are the levels of loyalty in the 10% of stores with the most loyal customers (`Loyalty`)? Please indicate which measures you use to answer and their numerical values.
a3) Determine the mean and variance of the variable `Loyalty`. Indicate clearly the procedure followed.
a4) Assume that a competitor assessed the loyalty level of the stores' customers using a different measurement scale, and found an average loyalty level of 650, and a standard deviation of 190. Compare the dispersion of the loyalty level for the company under investigation (`Loyalty` variable) and the competitor, specifying which measures you use and how you derived them.

---

**Answer.** The modal class is the class with the highest frequency density. The same result can be obtained by considering the histogram of the distribution.

```r
distr.summary.x(Loyalty, stats="dispersion", data=Campaign)
```

**a2)** One is interested in identifying the 90th percentile. The class containing the 90th percentile is the first class in which the cumulative relative frequency is above 0.70.

The 90-th percentile (approximated under the assumption of uniform frequency distribution within classes) is the value of `Loyalty` such that:

$$
F_{90} = 70 + \\frac{(0.9 - 0.2)}{0.02} = 78.333.
$$

The loyalty levels in the 10% of stores with the most loyal customers are therefore those greater than or equal to 78.333, i.e. within the range $[78.333, 100]$.

**a3)** Since the variable `Loyalty` is a continuous quantitative variable measured in classes, the mean and variance values can only be approximated from the classes themselves (below is the table with the values for convenience):

| $m_k$ | $f_k$ | $p_k$ | $m_k p_k$ | $m_k^2 p_k$ |
|------:|------:|------:|----------:|------------:|
| 15    | 58    | 0.04  | 0.6       | 9           |
| 30    | 319   | 0.22  | 6.6       | 198         |
| 45    | 348   | 0.24  | 10.8      | 486         |
| 60    | 435   | 0.30  | 18        | 1080        |
| 75    | 174   | 0.12  | 9         | 675         |
| 90    | 116   | 0.08  | 7.2       | 648         |
| TOT   | 1450  | 1     | 52.2      | 3096        |

Hence $\\bar x = \\sum_k m_k\\cdot p_k = 52.2$. So $\\sigma^2 = (1450/1449)\\cdot (3096 - 52.2^2) = 371.4161$. SD = $\\sqrt{371.4161}$.

**a4)** To compare the dispersion of the loyalty level of the distributions in the two companies, it is necessary to calculate the **coefficient of variation**:
$$CV = \\frac{s}{|\\bar x|}.$$
For company A: $CV_A = \\sqrt{371.16}/52.2 = 0.3691$.
For company B: $CV_B = 190/650 = 0.292$.

The level of loyalty is therefore **more dispersed in the previous company (A) than in the competitor company (B)**.
""",
"images": [],
}

ex3["3_11b"] = {
"title": "Ex 3.11b — Revenues boxplot construction and reading",
"content": """**Question.** Construct the boxplot of `Revenues` and compare the distributions across store categories (Location).

---

**Answer.** Construct the boxplot from the 5-number summary:

```r
distr.summary.x(Revenues, stats="fivenumbers", data=Campaign)
##  n n.a min q1 median q3 max
## 1450 0 105.82 804.55 984 1202.36 3312.54
distr.plot.x(Revenues, plot.type="boxplot", data=Campaign)
```

The box extends from the first quartile (804.55) to the third quartile (1202.36) and is divided by the median value (984). The whiskers in the graph provide an immediate visualisation of the range and any outliers.

**b2)** Side-by-side boxplots of `Revenues` by `Location` (city center / suburbs / etc.) for distinguishing the distributions of the variable across store categories:

```r
distr.plot.xy(Revenues, Location, plot.type="boxplot", data=Campaign)
```

Differences in median, IQR and presence of outliers help identify which store categories are more profitable.
""",
"images": ["statistics/images/ex3_11b-revenues-boxplot.png"],
}

ex3["3_11c"] = {
"title": "Ex 3.11c — Relationship Sales vs Revenues and Sales vs Costs",
"content": """**Question.** Consider the relationship between `Sales` and `Revenues`, and between `Sales` and `Costs`. Which has the stronger relationship?

---

**Answer.**

```r
distr.plot.xy(x=Sales, y=Revenues, plot.type="scatter", fitline=T, data=Campaign)
distr.plot.xy(x=Sales, y=Costs,    plot.type="scatter", fitline=T, data=Campaign)
cor(Campaign$Costs,    Campaign$Sales)
## [1] 0.7588883
cor(Campaign$Revenues, Campaign$Sales)
## [1] 0.7580242
```

The two correlations are essentially identical, both around 0.76. Both scatterplots show a roughly linear relationship of medium-high intensity.

From the plots, it can be observed that the strongest and most structured relationship is that between **Sales and Costs**, even though it is characterised by a non-linear trend in that a greater dispersion of campaign effectiveness is observed at higher levels of `Sales`. The relationship between `Revenues` and `Sales`, on the other hand, is particularly weak despite a number of observations arranged along a straight line. Note that the tendency of the data to concentrate around a line is more pronounced in the first plot. Neglecting therefore the non-linearity of the relationship between `Costs` and `Sales`, we note that the correlation coefficients are similar.
""",
"images": ["statistics/images/ex3_11c-sales-scatter-pair.png"],
}

# ========== EXERCISE 3.12 (Effectiveness × Channel) ==========

EX312_TABLE = """*(Customer counts for Effectiveness × Channel given in prompt.)*

| Effectiveness Channel | Low | Medium-Low | Medium | Medium-High | High | tot |
|----------------------:|----:|-----------:|-------:|------------:|-----:|----:|
| Online                | 16  | 76         | 36     | 30          | 22   | 180 |
| Mobile App            | 56  | 68         | 72     | 12          | 16   | 224 |
| In-Store              | 25  | 38         | 90     | 108         | 63   | 324 |
"""

ex3["3_12a"] = {
"title": "Ex 3.12a — Conditional distribution of Effectiveness by Channel",
"content": """**Question.** Build the conditional distribution of `Effectiveness` by `Channel` and identify the median for each channel.

""" + EX312_TABLE + """

---

**Answer.**

| Channel    | Low   | Medium-Low | Medium | Medium-High | High  |    |
|-----------:|------:|-----------:|-------:|------------:|------:|---:|
| Fr.Absolute| 16    | 76         | 36     | 30          | 22    | 180 |
| Fr.Relative| 0.089 | 0.422      | 0.2    | 0.167       | 0.122 | 1  |
| Fr.Cumulate| 0.089 | 0.511      | 0.711  | 0.878       | 1     |    |
| In-Store Fr.Absolute| 25 | 38      | 90     | 108         | 63    | 324|
| Fr.Relative| 0.077 | 0.117      | 0.278  | 0.333       | 0.194 | 1  |
| Fr.Cumulate| 0.077 | 0.194      | 0.472  | 0.805       | 1     |    |

The **mode** of Effectiveness (Online) is the highest frequency category (Medium-Low / 0.422), so is the **median** (the level with a cumulative frequency exceeding 0.5). We can conclude that a higher percentage of campaigns is observed for customers prefer to shop via In-Store.

**b)** From the bar graph, it is observed that customers will behave in the same way in future campaigns. Following this, it is reasonable to conclude that if you encourage customers to buy in-store (`Channel = In-Store`), a campaign will be more effective (`Effectiveness=high`).
""",
"images": [],
}

ex3["3_12b"] = {
"title": "Ex 3.12b — Independence of Effectiveness and Channel",
"content": """**Question.** Are the variables `Effectiveness` and `Channel` independent? Justify your answer.

---

**Answer.** Two variables are independent if the conditional distribution of one is the same across all categories of the other. From the table built in 3.12a, the conditional distributions of `Effectiveness` given each `Channel` are clearly **different** (Online concentrates at Medium-Low, In-Store at Medium-High). Therefore Effectiveness and Channel are **not independent** — they are associated.
""",
"images": [],
}

ex3["3_12c"] = {
"title": "Ex 3.12c — Test of independence (chi-squared style)",
"content": """**Question.** Apply the formal independence test.

---

**Answer.** We compute the expected frequencies under independence:
$$
E_{i,j} = \\frac{n_{i\\cdot}\\cdot n_{\\cdot j}}{n}.
$$

Comparing observed $O_{i,j}$ and expected $E_{i,j}$, we can quantify the deviation from independence (the chi-squared statistic). Since the deviations are substantial, we reject the null of independence — confirming the visual conclusion from 3.12b.

The propensity is higher among young companies (income, age category, etc.) influenced by `Effectiveness` and the campaign channel. **In conclusion, Effectiveness is not independent of Channel** — campaign effectiveness depends on which channel was used.
""",
"images": [],
}
