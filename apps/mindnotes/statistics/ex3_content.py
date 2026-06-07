"""
Ex 3 snippets — Statistics Module 3 (Bivariate statistics: conditional
distributions, two-way tables, covariance, correlation, independence).
Each entry is a dict with title, content, optional images.
"""

ex3 = {}

# ========== EXERCISE 3.1 (pizzerie — Sales | SmokingArea, etc.) ==========

ex3["3_1a"] = {
"title": "Ex 3.1a — Conditional boxplots of Sales by SmokingArea",
"content": """**Question.** Propose a graph to compare the distribution of `Sales` in pizzerias with and without a `SmokingArea`, and accurately describe the conclusions you can draw based on it.

---

**Answer.** The most appropriate graphical tool is based on **side-by-side boxplots**, which allow to compare the distributions of a quantitative variable, in our case `Sales`, conditioned to a qualitative variable, in our case `SmokingArea`:

```r
distr.plot.xy(x=Sales, y=SmokingArea, plot.type="boxplot", data=pizzerias)
```

The graph shows that the two conditional distributions have different characteristics. The distribution of sales in pizzerias **without a smoking area** has a lower median, lower variability both in terms of behavior in the center of the distribution (given by the interquartile difference, i.e. the size of the "box") and in terms of range. Also, the minimum and maximum values are consistently lower than those observed for the sales of pizzerias with smoking areas. The shape of the two distributions is also different. For pizzerias without a smoking area, an approximately symmetrical distribution is observed, although the first part of the box is wider than the second, indicating a dispersion between the first quartile and the median greater than between the median and the third quartile. In contrast, the distribution for pizzerias with a smoking area is **skewed to the right**, with a definitely asymmetrical box and a relatively long tail to the right, defined also, but not only, by the presence of at least two outliers.

*Note: It would also be possible to compare the distributions using two histograms, as long as the same classification is used for both distributions to allow for comparison. However, this requires a careful selection of classes to highlight the salient features of the distribution in both subgroups. From this point of view, the boxplot provides an unambiguous representation that is independent of the analyst's choices.*
""",
"images": ["statistics/images/ex3_1a-sales-by-smokingarea.png"],
}

ex3["3_1b"] = {
"title": "Ex 3.1b — Association between District and SmokingArea",
"content": """**Question.** Based on the available data, do you think there is an association between the two variables `District` and `SmokingArea`? Indicate the tools and the evidence you base your answer on. What substantive information do they provide?

---

**Answer.** To answer the question, it is necessary to compare the **conditional distributions**. In this particular case, it seems more appropriate to compare the conditional frequency distributions of the variable `SmokingArea`, according to the District, even if from a purely 'technical' point of view (i.e. to assess whether there is an association or not) one can also proceed conditioning on the variable `SmokingArea`.

It is possible to consider the following conditional frequency distributions:

```r
distr.table.xy(SmokingArea, District, freq="percentages", freq.type="col", data=pizzerie)
## Column Percentages
##                  District
## SmokingArea  Lodi   Milano  Pavia
## No            63      33     56
## Yes           37      67     44
## TOTAL        100     100    100
```

or a side-by-side (not reported) or stacked bar chart:

```r
distr.plot.xy(SmokingArea, District, freq="percentages", plot.type="bars",
              freq.type='x|y', data=pizzerie)
```

In both cases, the distributions of the variable `SmokingArea` differ substantially in the three provinces, leading to the conclusion that the two variables are associated. From a substantive point of view, this means that in the three provinces a "recognizable" trend can be observed with reference to the availability/equipment of a smoking area. In fact, in Milan the percentage of pizzerias with smoking areas is particularly high (67% of the total), while in Lodi an opposite trend can be observed (63% of pizzerias in Lodi do not have smoking areas); Pavia shows an intermediate pattern between these two extremes, although pizzerias without smoking areas prevail.
""",
"images": ["statistics/images/ex3_1b-smoking-by-district.png"],
}

ex3["3_1c"] = {
"title": "Ex 3.1c — Conditional comparison of Price by SmokingArea",
"content": """**Question.** Based on the data at hand, how would you assess whether pizzerias with smoking areas charge higher and/or more variable prices? *(In the available dataframe, only the price of a pizza margherita is available.)*

---

**Answer.** To compare the price according to the presence or absence of the smoking area, central tendency and variability measures are required:

```r
distr.summary.x(x=Price, by=SmokingArea, stats=c("summary","cv"), data=pizzerie)
## Summary measures for Price | SmokingArea
## Summary measures
##  SmokingArea  n  n.a  min  q1  median  mean  q3  max   sd   var
##           No 51    0  3.5   5   6.0  5.88   7   9  1.31 1.72
##          Yes 49    0  4.0   6   6.5  6.71   8  10  1.53 2.33
## Requested statistics
##  SmokingArea  n  n.a  cv
##           No 51    0  0.22
##          Yes 49    0  0.23
```

The mean price in pizzerias with smoking areas is approximately **1 euro higher** than in pizzerias without smoking areas; an increase of 1 euro is also observed for all 5 summary numbers except for the median which is only 50 cents higher (in pizzerias with vs without smoking area). This suggests less price dispersion between the first and second quartiles for pizzerias with smoking areas. In absolute terms, moreover, the price is more variable in pizzerias with a smoking area, as the 'standard' deviation from the mean is greater.

In this particular case, we are comparing the price of the same product so one can actually say that in the pizzerias without smoking areas prices between **€4.57 and €7.19** are "standard" ($5.88 \\mp 1.31$) while in pizzerias with smoking areas prices between **€5.18 and €8.24** ($6.71 \\mp 1.53$) are "standard". In relative terms, on the other hand, similar coefficients of variation are observed, which in this case shows precisely that pizzerias with smoking areas tend to charge higher prices without, however, being characterized by a higher relative concentration of prices around the mean.
""",
"images": [],
}

ex3["3_1d"] = {
"title": "Ex 3.1d — Statement check: \"having a smoking area ensures higher Sales\"",
"content": """**Question.** Based on the graph in the previous point, comment on the following statement: *"for a pizzeria, having a smoking area ensures higher Sales."*

---

**Answer.** The statement is **false**. In fact, the relationship between the two variables does not allow to infer a **causal link**. It could in fact be the case that larger pizzerias or those with higher sales are larger, or have dehors or gardens allowing for a smoking area.

Two-way tables and conditional boxplots show **whether** the distributions of one variable differ across the categories of another (i.e., statistical **association**), but they do not by themselves establish a causal link — that requires additional information about the data-generating process. The association observed between `SmokingArea` and `Sales` could be:

- **direct** (smoking area attracts more customers → more sales);
- **indirect / reverse** (larger pizzerias have both smoking areas AND more sales);
- **spurious** (a third factor — e.g. location, size of the venue — drives both).
""",
"images": [],
}

ex3["3_1e"] = {
"title": "Ex 3.1e — Scatter plot of Price vs Sales + covariance + correlation",
"content": """**Question.** How would you represent the joint distribution of the variables `Sales` and `Price` for the 100 pizzerias in the dataframe `pizzerie`? What comments on the relationship between the two variables can you draw using covariance? Is it possible to assess the strength of the relationship between the two variables using covariance? What index would you use and what considerations would you draw from it?

---

**Answer.** In order to jointly represent the two variables, which are both numerical, a **scatterplot** should be used to highlight a potential trend and in particular the type (linear or not), the direction (direct or indirect) and the intensity (correlation) of the relationship.

```r
distr.plot.xy(x=Price, y=Sales, plot.type="scatter", fitline=T, data=pizzerie)
```

An analysis of the graph, in which the straight line that cuts the data at best is reported, highlights a relationship that is certainly **direct and approximately linear**, despite the presence of some pizzerias with particularly **high sales** and a relatively high deviation from the straight line (thus, these are pizzerias with much higher sales compared to other pizzerias charging the same price for a margherita pizza). However, the relationship between the two variables is fairly linear and of medium intensity.

**The covariance:**

```r
cov(pizzerias$Price, pizzerias$Sales)
## [1] 9419.185
```

is an absolute index that allows to assess the **direction** of the relationship but has a magnitude that strongly depends on the units of measurement of the considered variables, and cannot therefore be used to assess the intensity of the linear relationship, if any. To assess the intensity of the linear relationship, it is necessary to refer to **Pearson's linear correlation coefficient**:

```r
cor(pizzerias$Price, pizzerias$Sales)
## [1] 0.6699389
```

This value indicates that the relationship between the two variables is **direct and of medium/medium-high intensity**, as the index assumes a value close to 67% of its theoretical maximum (1).
""",
"images": ["statistics/images/ex3_1e-price-sales-scatter.png"],
}

# ========== EXERCISE 3.2 (DS - AmountSpent vs various) ==========

ex3["3_2a"] = {
"title": "Ex 3.2a — Compare AmountSpent across History levels",
"content": """**Question.** <span class="exam-question-text">It is of interest to compare the amount spent (`AmountSpent`) by customers with different purchase frequency (`History`). What graphical tools can be used? What considerations on the most relevant differences between different customer groups?</span>

---

**Answer.** To graphically evaluate the distribution of `AmountSpent` (continuous) across the subgroups identified by the categories of `History`, we use **side-by-side boxplots** that provide a concise but exhaustive representation of the conditional distributions. Since `History` is ordinal, we first construct a factor by reordering its levels — if `History` is used directly, the graph is still correct, but the categories are arranged alphabetically on the horizontal axis:

`DS$History_recode <- factor(x=DS$History, levels=c("None","Low","Medium","High"))`
`distr.plot.xy(x=History_recode, y=AmountSpent, plot.type="boxplot", data=DS)`

Neglecting for a moment the new customers (`None`), it can be seen from the graph that as the frequency of purchase increases from `Low` to `Medium` and to `High` there is an increase in the 5-number summary (minimum, quartiles, maximum) — note in particular how the maximum (regular) value of expenditure of the low-frequency customers coincides approximately with the first quartile of the medium-frequency customers, and the same relationship is observed between the medium and high-frequency customers. There is also a decrease in the concentration of the central values (box height) and thus a greater dispersion, with distributions characterised by progressively longer tails — in particular the right tail; the almost symmetrical distribution observed at the `Low` level (except for a few outliers) becomes skewed to the right at the `High` level. The shape of the boxplots makes it possible to conclude that an **increasing relationship** also exists for the mean values. This indicates that the most loyal customers spend more on average — both in terms of mean and median — even though they exhibit greater heterogeneity.

Finally, the category `None` includes the most diverse customers in terms of spending attitudes: high range, with a very low minimum value (aligned to the minimum observed for `Low`), interquartile difference also high, and presence of higher outliers. The median expenditure for this group of customers is slightly higher than that of the `Medium` category, while the third quartile is almost aligned with the first quartile of the distribution for the `High` category. The heterogeneity of this category is understandable: it could potentially include customers who — over time — could become occasional or assiduous customers, and who, at their first purchase, show different levels of appreciation or trust in the company.
""",
"images": ["statistics/images/ex3_2a-amountspent-by-history.png"],
}

ex3["3_2b"] = {
"title": "Ex 3.2b — Summary measures of AmountSpent by Age groups",
"content": """**Question.** <span class="exam-question-text">What summary measures would you use to describe the amount spent (`AmountSpent`) by customers of different `Age` groups (Young, Middle, Senior)? Which customer age group is the most profitable (with reference to the amount spent) and which is the least profitable? Why?</span>

---

**Answer.** To compare the distributions of `AmountSpent` conditioned on `Age` we can certainly use the **5 summary numbers**. Since right-skewed distributions can also be expected — given the results at the previous points — we can also use **high-order percentiles** (p90, p95, p99) to adequately describe the right tails. Although not strictly necessary, instead of `Age` we use a factor that reorders its levels:

`DS$Age_recode <- factor(DS$Age, levels=c("Young","Middle","Senior"))`
`distr.summary.x(x=AmountSpent, by=Age_recode, stats=c("summary","p90","p95","p99"), data=DS)`
`## Age_recode   n    min     q1    median    mean      q3    max     sd`
`## Young      216    38   240.50   391.5   564.26   721.00  3688  523.92`
`## Middle     390   157   859.00  1364.0  1539.99  2080.25  5878  974.75`
`## Senior     144   161   638.75  1037.0  1380.90  1972.25  5564  988.15`

`## Age_recode    p90       p95      p99`
`## Young      1233.50   1572.00  2449.75`
`## Middle     2731.00   3326.45  5151.00`
`## Senior     2690.80   3157.55  4315.91`

`Young` customers present a distribution of the amount spent with a median significantly lower than that of older customers, and a distribution relatively concentrated around the median value (note the interquartile range, which is very small compared to that characterising the amount spent by customers with a higher age group). Despite the presence of a right tail that makes the distribution asymmetrical (as one can see from the distances of the high-order percentiles from the third quartile), we note that the third quartile of the distribution for `Young` customers is lower or slightly higher than the first quartile of the other age groups. This indicates that 75% of the `Young` customers spend less than 25% (approximately) of the less profitable `Middle` and `Senior` customers. This makes the `Young` age group the **least promising**.

`Middle` age customers, on the other hand, are characterised by the highest median spend. The distribution of `AmountSpent` for this customer group is most markedly skewed to the right, and has higher percentiles p90, p95 and p99 than the other categories. The high dispersion of the distribution is thus related to the presence of high-spending customers. The **most promising age group is therefore the `Middle`** category.
""",
"images": [],
}

ex3["3_2c"] = {
"title": "Ex 3.2c — Young customers who purchased nothing or rarely",
"content": """**Question.** <span class="exam-question-text">What is the number of customers who purchased nothing or rarely in the past and are young?</span>

---

**Answer.** This is a joint-count question on the two qualitative variables `Age_recode` and `History_recode`. We build the **joint absolute frequency table**:

`distr.table.xy(x=Age_recode, y=History_recode, freq="Counts", data=DS)`
`## Joint frequencies`
`##              History_recode`
`## Age_recode   None  Low  Medium  High  TOTAL`
`## Young         98   94    19      5     216`
`## Middle        96   63    89    142     390`
`## Senior        20   24    42     58     144`
`## TOTAL        214  181   150    205     750`

Customers who "purchased nothing or rarely in the past" are those with `History = None` or `History = Low`. Reading the `Young` row of the joint table: 98 young customers purchased nothing in the past and 94 with low frequency. The number of young customers who purchased nothing or rarely is therefore:

$$98 + 94 = 192 \\;\\text{customers.}$$
""",
"images": [],
}

ex3["3_2d"] = {
"title": "Ex 3.2d — Most frequent Age group among History = High",
"content": """**Question.** <span class="exam-question-text">What is the most frequent age group among customers who have purchased more frequently in the past (`History = High`)?</span>

---

**Answer.** The question asks for the **distribution of `Age` conditional on `History = High`** — that is, column percentages of the joint two-way table (column total = 100% for each `History` level):

`distr.table.xy(x=Age_recode, y=History_recode, freq.type="col", freq="perc", data=DS)`
`## Column Percentages`
`##              History_recode`
`## Age_recode   None   Low   Medium   High`
`## Young         46    52     13       2`
`## Middle        45    35     59      69`
`## Senior         9    13     28      28`
`## TOTAL        100   100    100     100`

Reading the `High` column: only 2% of high-frequency customers are `Young`, 28% are `Senior`, and **69% are `Middle` age**. It can be deduced that among the customers with `High` purchase frequency, the most represented age group is the `Middle` one (69%).
""",
"images": [],
}

ex3["3_2e"] = {
"title": "Ex 3.2e — Loyal customers (History = High) by Age group",
"content": """**Question.** Customers with high frequency of purchase (`History = High`) are considered as *loyal*. Which Age group (*Young*, *Middle*, *Senior*) has the highest percentage of loyal customers? Justify and explain your answer.

---

**Answer.** Compute the **row-conditional distribution** of `History_recode` given `Age_recode` (each row gives the % split across `None / Low / Medium / High` for one age group):

`distr.table.xy(x=Age_recode, y=History_recode, freq.type="row", freq="perc", data=DS)`

```
## Row Percentages
##               History_recode
## Age_recode  None  Low  Medium  High  TOTAL
##   Young      45    44     9      2    100
##   Middle     25    16    23     36    100
##   Senior     14    17    29     40    100
```

It can be concluded that the **Senior** age group is the one with the **highest percentage of loyal customers (40%)**, compared to **36%** for the *Middle* age group; a much lower percentage of loyal customers — only **2%** — is observed among *Young* people. The loyalty share therefore grows monotonically with age.
""",
"images": [],
}

ex3["3_2f"] = {
"title": "Ex 3.2f — Female vs male loyal customers (History | Sex)",
"content": """**Question.** Is it correct to say that the percentage of **female** customers who purchased with high frequency (*loyal customers*) is **higher** than the corresponding percentage of **male** customers? Justify the answer.

---

**Answer.** **The statement is incorrect.** To verify it, we look at the row-conditional distribution of `History_recode` given `Sex` — each row gives the % split across `None / Low / Medium / High` within each gender:

`distr.table.xy(x=Sex, y=History_recode, freq.type="row", freq="perc", data=DS)`

```
## Row Percentages
##           History_recode
## Sex     None  Low  Medium  High  TOTAL
##   Female  29   31    20    21    100
##   Male    29   17    20    34    100
```

It shows that the percentage of **women** who fall in the `High` purchasing-frequency category is **21%**, while the same percentage rises to **34%** among **men**. Loyal customers are therefore *more* concentrated among males than among females — the opposite of the proposed statement.
""",
"images": [],
}

ex3["3_2g"] = {
"title": "Ex 3.2g — Proportion of younger customers: owners vs renters",
"content": """**Question.** One could reasonably expect a **lower proportion of younger individuals** among those who own their home (`OwnHome = Own`) than among those living in a rented home (`OwnHome = Rent`). **Do the data support this expectation?**

---

**Answer.** **The expectation is confirmed.** Compare the distribution of `Age_recode` (`Young / Middle / Senior`) within the two subpopulations identified by the `OwnHome` categories — the row-conditional distribution of `Age_recode | OwnHome`:

`distr.table.xy(x=OwnHome, y=Age_recode, freq.type="y|x", freq="perc", data=DS)`

```
## y|x: Percentages
##              Age_recode
## OwnHome  Young  Middle  Senior  TOTAL
##   Own      8      66      27    100
##   Rent    51      38      11    100
```

It is observed that among those who own their home **only 8%** are *Young*, while among those who live in **rented** accommodation *Young* people make up **51%** — a very large gap. The data therefore strongly support the prior expectation: home-owners are concentrated in the *Middle* and *Senior* age groups, whereas renters are dominated by *Young* customers.
""",
"images": [],
}

ex3["3_2h"] = {
"title": "Ex 3.2h — Association between Location and History",
"content": """**Question.** How would you **graphically assess** whether the variables `Location` and `History` are associated? Based on the graph, can you infer what the **central tendency measures (location)** of (`History | Location`) are? What conclusions can you draw from the graph and from the summary measures?

---

**Answer.** The graphical tool that allows one to assess the presence/absence of an association between two categorical variables is the **bar plot of the conditional frequency distributions**, either stacked or side-by-side:

`distr.plot.xy(x=Location, y=History_recode, freq.type="y|x", plot.type="bars", data=DS)`

`distr.plot.xy(x=Location, y=History_recode, freq.type="y|x", plot.type="bars", bar.type="beside", data=DS)`

The underlying row-conditional table is:

```
## Row Percentages
##            History_recode
## Location  None  Low  Medium  High  TOTAL
##   Close    29   28     23    20    100
##   Far      28   14     12    45    100
```

Since `History_recode` is **ordinal**, the appropriate central-tendency measures are the **mode** and the **median**:

- For (`History | Location = Close`) the distribution is practically **bimodal**, the frequencies of `None` and `Low` being similar (≈ 29% and ≈ 28%) and the median lying inside `Low` (cumulative 57% ≥ 50%). The mode is **None**.
- For (`History | Location = Far`) the mode is **High** (≈ 45%) and the median falls in the `Medium`–`High` region.

**Conclusion.** The two conditional distributions are clearly different — `Location` and `History` are **associated**: customers living *Far* from a similar physical shop in the neighbourhood are much more likely to be loyal (`High` history, 45% vs 20%), while customers living *Close* spread their frequency rather evenly across `None`/`Low`. The result is reasonable: the lack of a competitor in the area may increase the attractiveness of the services offered by the company.
""",
"images": [],
}

ex3["3_2i"] = {
"title": "Ex 3.2i — 5% highest spenders by Age group",
"content": """**Question.** If the company wants to offer a special promotion to the 5% of customers who spent the most per age group, what threshold (`AmountSpent`) should be considered for each of the three groups (`Young`, `Middle`, `Senior`)?

---

**Answer.** It is necessary to determine the **95th percentile** of the distribution of `AmountSpent` within each age group:

```r
distr.summary.x(AmountSpent, by=Age_recode, stats="p95", data=DS)
##           n.n.a    p95
## Young      216   1572.00
## Middle     398   3326.45
## Senior     144   3157.55
```

The promotion should be offered to:

- **Young** customers spending more than **EUR 1572.00**,
- **Middle** customers spending more than **EUR 3326.45**,
- **Senior** customers spending more than **EUR 3157.55**.

The cutoffs for `Middle` and `Senior` are similar and well above `Young`, consistent with the location of the conditional `AmountSpent` distributions observed earlier — the top 5% of middle-aged and senior customers spend more than twice as much as the top 5% of young customers.
""",
"images": [],
}

ex3["3_2l"] = {
"title": "Ex 3.2l — Salary vs Catalogs (most associated with AmountSpent)",
"content": """**Question.** Which of the variables `Salary` and `Catalogs` is most strongly associated with `AmountSpent`? Justify your answer appropriately and interpret the obtained result and its reliability.

---

**Answer.** Both `Salary` and `Catalogs` are **numerical** variables, so the strength of the linear relationship with `AmountSpent` can be summarised by the **Pearson correlation coefficient**:

```r
cor(DS$Salary,    DS$AmountSpent)
## [1] 0.6996546
cor(DS$Catalogs,  DS$AmountSpent)
## [1] 0.4732376
```

`Salary` is **more strongly associated** with `AmountSpent` than `Catalogs` is: the correlation is high (~0.70) for `Salary` and only medium (~0.47) for `Catalogs`. Both relationships are positive — larger salaries and more catalogues sent are both associated with higher spending.

Reliability of the index — look at the **scatter plots** to check whether the linear summary is appropriate:

```r
distr.plot.xy(Salary,    AmountSpent, plot.type="scatter", data=DS)
distr.plot.xy(Catalogs,  AmountSpent, plot.type="scatter", data=DS)
```

- **Salary vs AmountSpent.** The cloud has an approximately **linear** shape, so the correlation coefficient is a *reliable* summary: the linear relationship is medium-high in intensity.
- **Catalogs vs AmountSpent.** `Catalogs` takes only a few discrete values (6, 12, 18, 24), so the scatter is organised into vertical strips. The relationship is positive but the variability of `AmountSpent` within each strip is large; the correlation coefficient is less informative here, and a side-by-side boxplot of `AmountSpent` by `Catalogs` would be a better visual tool.

**Conclusion.** `Salary` is the variable most strongly (and most reliably linearly) associated with `AmountSpent`.
""",
"images": [],
}

ex3["3_2m"] = {
"title": "Ex 3.2m — Children vs AmountSpent (tools + correlation)",
"content": """**Question.** What tools would you use to highlight the relationship between `AmountSpent` and `Children`? Would you use the correlation coefficient to summarise the strength of the link between the two variables?

---

**Answer.** `Children` is **discrete numerical** (0, 1, 2, 3). For a discrete predictor vs a continuous outcome the best graphical tools are **side-by-side boxplots** of `AmountSpent` by number of children, and a scatter plot:

```r
distr.plot.xy(Children, AmountSpent, plot.type="boxplot", data=DS)
distr.plot.xy(Children, AmountSpent, plot.type="scatter", data=DS)
```

The two plots both show that **the amount spent decreases as the number of children increases**, and that as the number of children increases the **right skewness of the distribution decreases**. In both cases, the plots highlight the fact that customers without children have much more **heterogeneous** spending behaviour — this group includes both single customers and customers with a partner but no children, and the two sub-groups may have very different spending capacities.

The **correlation** between the two variables is:

```r
cor(DS$Children, DS$AmountSpent)
## [1] -0.2125776
```

A weak **negative** linear relationship. The value of the index describes the negative, weak relationship between the two variables very well and **can be used to communicate its intensity**: although the structure of the relationship cannot strictly be called linear, the **low magnitude** of the coefficient does not induce misjudgements — the correlation is a reasonable summary alongside the boxplots.
""",
"images": ["statistics/images/ex3_2m-amountspent-children.png"],
}

# ========== EXERCISE 3.3 (Satisfaction) ==========

ex3["3_3a"] = {
"title": "Ex 3.3a — Evaluate Statements (a) and (b) on Satisfaction correlations",
"content": """**Question.** With reference to the `Satisfaction` dataframe (customer satisfaction, age, distance from home, average expenses), assess the truth of the following two statements:

- **Statement (a).** The relationship between `satisfaction` and `distance` is approximately linear and the (low-to-medium) Pearson correlation coefficient effectively summarises it; the relationship with `age` is instead far from linear, although the two correlation coefficients are similar.
- **Statement (b).** The (indirect) linear relationship between `satisfaction` and `expenses` is stronger than that between `satisfaction` and `distance`, since the correlation coefficient is larger in absolute value.

---

**Answer — Statement (a): TRUE.**

To evaluate statement *a* we refer to Pearson's linear correlation indices for the two pairs of variables and to the scatterplots describing their joint distribution:

`cor(Satisfaction$age, Satisfaction$satisfaction)`
`## [1] 0.4692066`
`cor(Satisfaction$distance, Satisfaction$satisfaction)`
`## [1] 0.4793237`

The two linear correlation coefficients are similar. However, to draw conclusions on the similarity of the *characteristics* of the relationships, the scatterplots must also be considered:

`distr.plot.xy(age, satisfaction, plot.type="scatter", data=Satisfaction)`
`distr.plot.xy(distance, satisfaction, plot.type="scatter", data=Satisfaction)`

The scatterplot of `satisfaction` vs `distance` shows an approximately linear pattern: the low-to-medium correlation effectively summarises the dispersion of the points around a straight line. The scatterplot of `satisfaction` vs `age` is instead far from linear — younger customers are extremely heterogeneous in satisfaction, customers around 25–40 are relatively dissatisfied, and from 35/40 onwards satisfaction increases with age. The relationship is therefore **more quadratic than linear**, so the two pairs are profoundly different in nature even though the correlation coefficients are similar. Statement (a) is correct.

---

**Answer — Statement (b): FALSE.**

`cor(Satisfaction$expenses, Satisfaction$satisfaction)`
`## [1] -0.5251066`
`distr.plot.xy(expenses, satisfaction, plot.type="scatter", data=Satisfaction)`

The correlation with `expenses` is **larger in absolute value** ($|{-0.525}| > 0.479$), so by Statement (b)'s criterion one would conclude the satisfaction–expenses link is stronger. But inspecting the scatterplot shows that the linear pattern of `satisfaction` vs `expenses` and that of `satisfaction` vs `distance` are of **similar medium-level intensity**: in both cases the coefficient summarises well a medium-strength linear relationship, and the slightly larger $|r|$ for `expenses` does not justify the conclusion that the relationship is meaningfully stronger. Statement (b) is therefore false.
""",
"images": ["statistics/images/ex3_3a-satisfaction-scatters.png"],
}

# ========== EXERCISE 3.4 (Services - EXPENSES by TYPE) ==========

ex3["3_4a1"] = {
"title": "Ex 3.4 a1 — Compare EXPENSES distributions by TYPE (BUSINESS vs PRIVATE)",
"content": """**Question.** Graphically highlight any differences in the variable `EXPENSES` depending on the type (`TYPE`) of customer.

---

**Answer.** Side-by-side boxplots are the natural tool.

`distr.plot.xy(TYPE, EXPENSES, plot.type="boxplot", data=Services)`

The two distributions have a different **shape**:
- **BUSINESS** clients — left-skewed distribution, indicating a greater concentration on higher values of `EXPENSES`; the third quartile is much closer to the maximum than to the median.
- **PRIVATE** clients — right-skewed distribution, with rather symmetrical tails (the box and whiskers are more balanced than for BUSINESS).

Furthermore, the distribution of `EXPENSES` on BUSINESS customers is **shifted to higher values** compared to PRIVATE: all position measures (median, quartiles, mean) are higher than the corresponding ones for PRIVATE.
""",
"images": ["statistics/images/ex3_4a-expenses-by-type.png"],
}

ex3["3_4a2"] = {
"title": "Ex 3.4 a2 — Position summary of EXPENSES by TYPE (5-number + deciles + mean)",
"content": """**Question.** Given the absence of extreme values and the rather short length of the tails, the 5-number summary is sufficient to summarise the two distributions and to allow an adequate comparison of their characteristics. To offer more detail, also report the first and ninth deciles ($p_{10}$, $p_{90}$) and the mean.

---

**Answer.**

`distr.summary.x(x=EXPENSES, by1=TYPE, stats=c("fivenumber","p10","p90","mean"), data=Services)`

*Five-number summary*

| TYPE     |  n | n.a |  min  |  q1   | median |  q3   |  max  |
|----------|---:|----:|------:|------:|-------:|------:|------:|
| BUSINESS | 56 |  0  | 36.01 | 50.65 | 58.90  | 62.32 | 76.15 |
| PRIVATE  | 102|  0  | 30.09 | 41.95 | 46.59  | 56.11 | 69.72 |

*Requested statistics*

| TYPE     |  n | n.a |  p10  |  p90  |  mean  |
|----------|---:|----:|------:|------:|-------:|
| BUSINESS | 56 |  0  | 44.79 | 67.17 | 56.62  |
| PRIVATE  | 102|  0  | 38.26 | 63.02 | 48.84  |

**Reading.** The two distributions are shifted: BUSINESS has every position measure higher than PRIVATE (median 58.9 vs 46.6; mean 56.6 vs 48.8). The 5-number summary already conveys the asymmetry: for BUSINESS $\,q_3-\text{med}=3.4 < \text{med}-q_1=8.2\,$ (left-skew); for PRIVATE $\,q_3-\text{med}=9.5 > \text{med}-q_1=4.6\,$ (right-skew). The deciles $p_{10}, p_{90}$ confirm that the bulk of each distribution is well captured by the 5-number summary, so adding the tails and the mean is sufficient — no need for the full set of percentiles given the absence of extreme outliers.
""",
"images": ["statistics/images/ex3_4a-expenses-by-type.png"],
}

ex3["3_4b"] = {
"title": "Ex 3.4 b — Compare dispersion of EXPENSES via range, IQR, sd, var, CV",
"content": """**Question.** In this case, would the two ranges of variation be affected by outliers? Compare the dispersion of `EXPENSES` between BUSINESS and PRIVATE customers using the full set of dispersion measures.

---

**Answer.** Given the absence of extreme values and the similar interquartile ranges seen in (a2), the two ranges of variation are **not** materially affected by outliers and the two distributions span intervals of similar width. Looking at the dispersion measures of `EXPENSES` in the two groups:

`distr.summary.x(x=EXPENSES, by1=TYPE, stats=c("dispersion"), data=Services)`

*Measures of dispersion*

| TYPE     |  n | n.a | range | IQrange |  sd  |  var  |  cv  |
|----------|---:|----:|------:|--------:|-----:|------:|-----:|
| BUSINESS | 56 |  0  | 40.14 |  11.67  | 9.03 | 81.60 | 0.16 |
| PRIVATE  | 102|  0  | 39.64 |  14.16  | 9.58 | 91.69 | 0.20 |

**Reading.** There is substantial alignment in absolute dispersion: the **range** is slightly lower for PRIVATE (39.64 vs 40.14), while the **interquartile range** (14.16 vs 11.67) and the **standard deviation** (9.58 vs 9.03) are higher — albeit not dramatically — for PRIVATE. Providing only the range would therefore be misleading.

The **coefficient of variation** $CV = sd/\\bar x$ resolves the comparison on a scale-free footing. Using the means from (a2):

- BUSINESS: $CV = 9.03 / 56.62 \\approx 0.16$
- PRIVATE:  $CV = 9.58 / 48.84 \\approx 0.20$

So **PRIVATE customers show relatively more dispersion** around their mean than BUSINESS customers ($0.20 > 0.16$), even though absolute dispersion measures (range, sd) are close. The CV is therefore the appropriate index here: it puts both subgroups on comparable terms when means differ.
""",
"images": ["statistics/images/ex3_4a-expenses-by-type.png"],
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
"content": """**Question.** Based on the available information, is it possible to assess whether the total number of users (`TotUsers`) has different characteristics, in terms of mean and variance, depending on the weather conditions? Illustrate clearly the followed procedure and comment on the results. You may also answer this question without making calculations, provided that you offer a well founded and justified explanation of your answer based on the available data.

""" + EX35_TABLE + """

---

**Answer.** **Yes** — the variable `TotUsers` is observed in **grouped form** (closed intervals), so we can still compute approximate values of the mean and variance **conditional on Weather** by applying the **grouped-data formulas** (Ex 2.2) separately within each Weather column. Under the uniform-on-interval assumption, each class is summarised by its midpoint $m_k$ and the within-Weather absolute frequency $n_{k|w}$.

For each Weather group $w \\in \\{1,2,3\\}$ with within-group total $n_w$:
$$
\\bar x_w \\;\\approx\\; \\frac{1}{n_w}\\sum_k n_{k|w}\\, m_k, \\qquad
\\sigma_w^2 \\;\\approx\\; \\frac{1}{n_w-1}\\left[\\sum_k n_{k|w}\\, m_k^2 \\;-\\; n_w\\,\\bar x_w^{\\,2}\\right].
$$

The midpoints for the six classes are $m_k \\in \\{50,\\,150,\\,300,\\,450,\\,600,\\,850\\}$, and the within-Weather totals are $n_1 = 6684$, $n_2 = 2659$, $n_3 = 850$.

**Comment.** The procedure yields decreasing $\\bar x_w$ and $\\sigma_w$ from Weather=1 to Weather=3: the conditional distribution of `TotUsers` shifts towards smaller values and becomes less dispersed as weather worsens. So **yes**, mean and variance of the number of users depend on weather conditions — clear days bring both a higher average demand and a wider range, while bad weather compresses the distribution towards lower hourly user counts.
""",
"images": [],
}

ex3["3_5a2"] = {
"title": "Ex 3.5 a2 — Conditional Q1 and Q3 of TotUsers by Weather",
"content": """**Question.** Calculate the first and third quartiles of the number of users conditioned to the weather conditions, clearly stating the procedure followed. What conclusions can be drawn from comparing the position measures obtained?

""" + EX35_TABLE + """

---

**Answer.** Because `TotUsers` is in **classed form**, each conditional quartile is obtained by **linear interpolation** inside the class that first contains the target cumulative relative frequency $q \\in \\{0.25,\\,0.75\\}$. For Weather group $w$, let $a_k$ be the lower bound of the quartile class, $w_k$ its width, $F_{k-1,w}$ the cumulative within-group relative frequency *before* the class, and $f_{k,w}$ the within-group relative frequency of the class:

$$
Q_q^{(w)} \\;\\approx\\; a_k \\;+\\; \\frac{q - F_{k-1,w}}{f_{k,w}}\\,w_k .
$$

**Procedure.** (1) Build, column by column (i.e. for each Weather), the within-group relative frequencies $f_{k,w} = n_{k,w}/n_w$ and their cumulatives $F_{k,w}$. (2) Locate the smallest $k$ with $F_{k,w} \\ge q$. (3) Apply the formula above.

**Numerical results (approximate, in users/hour).**

| Weather | $Q_1$ | $Q_3$ | IQR |
|--------:|------:|------:|----:|
| 1 (Clear)     | $\\approx 144$ | $\\approx 376$ | $\\approx 232$ |
| 2 (Fog)       | $\\approx 89$  | $\\approx 277$ | $\\approx 188$ |
| 3 (Low rain)  | $\\approx 31$  | $\\approx 158$ | $\\approx 127$ |

**Conclusions.**
- Both $Q_1$ and $Q_3$ are **highest under Weather=1**: the central 50% of hourly demand sits between roughly 144 and 376 users on clear days.
- Quartiles drop **substantially** under Weather=2 (Fog) and again under Weather=3 (Low rain): customers ride much less when weather worsens.
- The **conditional IQR also shrinks** with worsening weather — fewer high-demand hours when conditions are bad.

The conditional position measures therefore confirm what already emerged from mean/variance considerations: **weather has a clear effect on the demand for vehicles**, both in level (median demand) and in dispersion (IQR).
""",
"images": [],
}

# ========== EXERCISE 3.6 (customer_habits — Year × Sex, Country × Sex) ==========

ex3["3_6a"] = {
"title": "Ex 3.6a — Year × Sex joint and conditional distributions",
"content": """**Question.** How many purchases (transactions) were made by female customers in the year 2016? What is the percentage weight of such transactions over the total?

---

**Answer.** From the **joint absolute and relative frequency distributions** of `Year` and `Sex`:

```r
distr.table.xy(Year, Sex, freq=c("counts","percentages"),
               p.digits=1, data=customer_habits)
## Joint counts
##              Sex
##  Year         F      M
##  2015      6518   8501
##  2016      8717  11130
##
## Joint percentages
##              Sex
##  Year         F      M
##  2015      18.7   24.4
##  2016      25.0   31.9
```

We obtain that there were **8717 transactions made by female customers in 2016**, representing **25.0% of the transactions in the entire dataset**.
""",
"images": [],
}

ex3["3_6b"] = {
"title": "Ex 3.6b — Year-conditional distribution of Sex",
"content": """**Question.** Can it be concluded that the characteristics of the available data (in this case the gender `Sex` and `Year`) are associated?

---

**Answer.** To check association between `Year` and `Sex` we look at the **conditional distribution of `Sex` given `Year`** (or equivalently the conditional distribution of `Year` given `Sex`):

```r
distr.table.xy(Year, Sex, freq=c("perc"), freq.type="y|x", data=customer_habits)
## y|x: Percentages
##              Sex
##  Year         F      M    TOTAL
##  2015      43.4   56.6    100
##  2016      43.9   56.1    100
```

Apart from **marginal variations** in relative weights, which show a very slight increase of transactions of female customers, from **43.4% in 2015** to **43.9% in 2016**, and a corresponding decrease in the share of transactions to male customers, **the gender composition is essentially unchanged across years**.

Since the two conditional distributions of `Sex` are practically identical (≈ 44% F / 56% M in both years), we conclude that `Year` and `Sex` are **not associated**: the gender mix of buyers has not changed between 2015 and 2016.
""",
"images": [],
}

ex3["3_6c"] = {
"title": "Ex 3.6c — French male clients: how many?",
"content": """**Question.** How many transactions refer to male clients residing in France? And how many to male clients residing in continental Europe?

---

**Answer.** We compute the **joint absolute and relative frequencies** of `Country` and `Sex`:

```r
distr.table.xy(Country, Sex, freq=c("counts","percentages"),
               p.digits=2, data=customer_habits)
## Joint counts
##                  Sex
##  Country           F      M
##  France         1791   2557
##  Germany        4908   5901
##  United Kingdom 2625   3727
##  United States  5911   7446
##
## Joint percentages
##                  Sex
##  Country           F      M
##  France          5.14   7.33
##  Germany        14.08  16.93
##  United Kingdom  7.53  10.69
##  United States  16.95  21.36
```

Reading directly from the joint counts:

- Transactions by **male clients residing in France**: **2 557** (≈ **7.33%** of the total).
- Transactions by **male clients residing in continental Europe** (France + Germany): $2\\,557 + 5\\,901 = $ **8 458** (≈ $7.33 + 16.93 = $ **24.26%** of the total).

(The United Kingdom is not part of continental Europe and is therefore excluded from the second figure.)
""",
"images": [],
}

ex3["3_6d"] = {
"title": "Ex 3.6d — Most relevant Sex × Country segment",
"content": """**Question.** Which customer segment, identified by the combination of `Sex` and `Country`, is most relevant, and what is its weight in the dataset?

---

**Answer.** Read the **joint distribution** of `Country × Sex` (counts and percentages):

```r
distr.table.xy(Country, Sex, freq=c("counts"), data=customer_habits)
##                  Sex
##  Country           F      M    TOTAL
##  France         3838   2765    6603
##  Germany        3499   6397    9896
##  United Kingdom 4025   4094    8119
##  United States  3873   6375   10248
##  TOTAL         15235  19631   34866

distr.table.xy(Country, Sex, freq=c("perc"), freq.type="joint",
               p.digits=2, data=customer_habits)
##                  Sex
##  Country           F      M    TOTAL
##  France         11.01   7.93   18.94
##  Germany        10.04  18.35   28.38
##  United Kingdom 11.54  11.74   23.29
##  United States  11.11  18.28   29.39
##  TOTAL          43.70  56.30  100.00
```

The largest joint frequency is `(Country = Germany, Sex = M)` with **6 397** transactions:

$$
\\widehat{\\Prob}(\\text{Germany}, M) \\;=\\; \\frac{6\\,397}{34\\,866} \\;\\approx\\; 0.1835.
$$

So the **most relevant single segment is German male customers**, weighing **18.35%** of the dataset — just below one fifth of all transactions. A close runner-up is `(United States, M)` at **18.28%** (6 375 transactions). The segments associated with male buyers from other countries are far smaller — about half the weight — i.e. **7.93% for `(France, M)`** and **11.74% for `(UK, M)`**.
""",
"images": [],
}

ex3["3_6e"] = {
"title": "Ex 3.6e — Sex | Country conditional distribution and bar plot",
"content": """**Question.** Is it correct that the frequency of transactions among male and female customers is essentially the same in each country? Support the answer with a graphical analysis.

---

**Answer.** To compare the relevance of the customer segments defined by `Sex` **within each country**, we need the **conditional distribution of `Sex` given `Country`**:

```r
distr.table.xy(Country, Sex, freq=c("perc"), freq.type="y|x",
               p.digits=2, data=customer_habits)
##                    Sex
##  Country         F       M    TOTAL
##  France        58.13   41.87   100
##  Germany       35.36   64.64   100
##  United Kingdom 49.58  50.42   100
##  United States 37.79   62.21   100
```

The female share **varies substantially across countries**: it is highest in **France at 58.13%** (the only country where women outnumber men), it sits near balance in the **UK at 49.58%**, and it drops to **37.79% in the US** and **35.36% in Germany**, where roughly two-thirds of transactions come from male buyers.

Hence the statement is **false** — the Sex frequencies are **not** essentially the same across countries, so `Sex` and `Country` are **associated** (not independent).

The same conclusion is reached graphically with stacked or side-by-side bar charts:

```r
distr.plot.xy(Country, Sex, freq="percentages", freq.type="y|x",
              plot.type="bars", bar.type="stacked", data=customer_habits)
distr.plot.xy(Country, Sex, freq="percentages", freq.type="y|x",
              plot.type="bars", bar.type="beside",  data=customer_habits)
```

The plots show that the female (red) portion is largest in France (~58%), drops to about 50% in the UK, and shrinks to roughly 36–38% in Germany and the US, mirroring the heterogeneity in the table.
""",
"images": ["statistics/images/ex3_6f-country-sex-stacked.png"],
}

ex3["3_6f"] = {
"title": "Ex 3.6f — Would a male-only promotion work equally across countries?",
"content": """**Question.** Given the greater relevance of male transactions in the entire dataframe, one might consider a special promotion dedicated to male customers. Would such a promotion have the **same effect in different countries**?

---

**Answer.** A male-only promotion would have **the same effectiveness across countries only if** the weight of the male segment were similar in each country, i.e. only if the conditional distributions of `Sex | Country` were equal. In that scenario `Sex` and `Country` would be **independent / not associated**.

From Ex 3.6e the conditional `Sex | Country` distribution is clearly **not** homogeneous:

$$
\\widehat{\\Prob}(M\\mid \\text{Germany}) = 64.64\\%,\\quad
\\widehat{\\Prob}(M\\mid \\text{US}) = 62.21\\%,\\quad
\\widehat{\\Prob}(M\\mid \\text{UK}) = 50.42\\%,\\quad
\\widehat{\\Prob}(M\\mid \\text{France}) = 41.87\\%.
$$

So **the two variables are associated**: in Germany and the US the promotion would reach ~63% of buyers, in the UK about half, and in France less than 42%. For various commercial and cultural factors the company's products are appreciated differently by the two sexes in different markets.

**Conclusion.** A male-targeted promotion would deliver **different outcomes depending on the country** — strongest in Germany and the US, weakest in France. A country-specific marketing strategy is therefore preferable to a uniform one.
""",
"images": [],
}

ex3["3_6g"] = {
"title": "Ex 3.6g — Are M/F transactions homogeneously spread across countries?",
"content": """**Question.** A general belief states that transactions by male and female buyers are **homogeneously distributed across countries**. Do the data support such belief?

---

**Answer.** Compute the **conditional distribution of `Country` given `Sex`** and visualise it with a side-by-side bar chart:

```r
distr.table.xy(Sex, Country, freq=c("perc"), freq.type="y|x",
               p.digits=2, data=customer_habits)
##              Country
##  Sex      France  Germany  United Kingdom  United States  TOTAL
##  F         25.19   22.97        26.42           25.42      100
##  M         14.08   32.59        20.85           32.47      100

distr.plot.xy(Country, Sex, freq.type="x|y",
              plot.type="bars", bar.type="beside", data=customer_habits)
```

- **Female buyers** are **almost homogeneously distributed**: each of the four countries accounts for roughly 25% of female transactions (range 22.97% – 26.42%).
- **Male buyers** are **clearly not homogeneous**: Germany (32.59%) and the United States (32.47%) together absorb about **two thirds** (~65%) of all male transactions, while France carries only **14.08%** and the UK **20.85%**.

So the belief is only **partially** supported — it holds for women but **fails for men**. Combined with Ex 3.6e/f this confirms that **`Sex` and `Country` are associated**: the country mix of buyers depends on Sex, and a country-blind targeting strategy would mis-allocate effort.
""",
"images": ["statistics/images/ex3_6g-country-by-sex-side.png"],
}

# ========== EXERCISE 3.7 (Product_Category × Sex / × Country) ==========

ex3["3_7a1"] = {
"title": "Ex 3.7 a1 — Product_Category | Sex",
"content": """**Question.** <span class="exam-question-text">Compare the product category distributions for transactions made by male and female buyers, and assess the relative importance of the product categories in the two segments identified by the sex of the customer.</span>

---

**Answer.** Both `Sex` and `Product_Category` are qualitative, so we use column-conditional frequencies `y|x` (distribution of `Product_Category` within each `Sex`).

```r
distr.table.xy(Sex, Product_Category, freq=c("perc"), freq.type="y|x",
               p.digits=2, data=customer_habits)
##  y|x: Percentages
##           Product_Category
##  Sex     Accessories  Bikes  Clothing   TOTAL
##  F          64.17     18.90    16.93    100.00
##  M          64.99     21.47    13.54    100.00

distr.plot.xy(Sex, Product_Category, freq.type="y|x", plot.type="bars",
              data=customer_habits)
```

The most popular product category is the same — **Accessories** — in both segments and is characterised by approximately the same weight (about **64-65%**) in each customer segment. Category-by-category, the percentages are rather similar; **Clothing** has a slightly higher weight in the segment of transactions made by female customers (16.93% vs 13.54%), which partly accounts for the lower weight of transactions related to **Bikes** (18.90% vs 21.47%). The two conditional distributions are very similar, suggesting essentially the same purchase frequency in each segment identified by the sex of the customer — we are therefore close to a situation of **statistical independence** at the aggregate level.
""",
"images": ["statistics/images/ex3_7a1-product-by-sex.png"],
}

ex3["3_7a3"] = {
"title": "Ex 3.7 a3 — Product_Category × Sex within each Country",
"content": """**Question.** <span class="exam-question-text">To analyse the relationship between `Product_Category` and `Sex` for each country, repeat the analysis at point a1 — relative to the totality of transactions — for each country: France, Germany, the United Kingdom and the United States. Is the relative importance of different product categories still the same for male and female buyers in each country?</span>

---

**Answer.** First build a sub-dataframe for each country, then compute `Product_Category | Sex` within each.

```r
data_France  <- customer_habits[customer_habits$Country=="France",]
data_Germany <- customer_habits[customer_habits$Country=="Germany",]
data_UK      <- customer_habits[customer_habits$Country=="United Kingdom",]
data_USA     <- customer_habits[customer_habits$Country=="United States",]

# France
distr.table.xy(Sex, Product_Category, freq=c("percentage"),
               p.digits=2, total=F, data=data_France)
##  y|x: Percentages
##           Product_Category
##  Sex     Accessories  Bikes  Clothing
##  F          55.81     12.25    31.94
##  M          52.80     27.59    19.60

# Germany
distr.table.xy(Sex, Product_Category, freq=c("percentage"),
               p.digits=2, total=F, data=data_Germany)
##  y|x: Percentages
##           Product_Category
##  Sex     Accessories  Bikes  Clothing
##  F          72.13     20.92     6.94
##  M          72.67     18.71     8.61

# UK
distr.table.xy(Sex, Product_Category, freq=c("percentage"),
               p.digits=2, total=F, data=data_UK)
##  y|x: Percentages
##           Product_Category
##  Sex     Accessories  Bikes  Clothing
##  F          69.09     19.58    11.33
##  M          54.62     22.64    22.74

# USA
distr.table.xy(Sex, Product_Category, freq=c("percentage"),
               p.digits=2, total=F, data=data_USA)
##  y|x: Percentages
##           Product_Category
##  Sex     Accessories  Bikes  Clothing
##  F          60.13     22.95    16.91
##  M          69.22     20.82     9.96

# side-by-side barplots, one per country
distr.plot.xy(Sex, Product_Category, freq=c("percentage"), freq.type="y|x",
              plot.type="bars", bar.type="beside", data=data_France)
# ... same for Germany / UK / USA
```

Regardless of country, the **mode remains `Accessories`** irrespective of the sex of the customer making the purchase. However, the **relative importance of the mode** in the two segments, as well as the frequency of transactions related to other product categories, varies from country to country. For example, **31.94%** of French female customers purchase `Clothing` versus only **19.60%** of male customers, who are instead more likely to purchase **bicycles** (27.59%). In the UK, female customers are more likely to purchase accessories (69% vs 55%), at the expense of clothing (11% vs 23%). The **exception is Germany**, where the conditional distributions of `Product_Category | Sex` are very similar, indicating similar purchase rates of product categories by males and females.

**a4*)** At the aggregate level, `Product_Category` and `Sex` appeared to be approximately independent. However, considering the variable `Country` in the analysis, transactions by male and female customers show **different purchasing habits**, and an association between sex and product category is observed. The independence observed at the aggregate level is thus the consequence of effects at the geographical level that cancel each other out (i.e. compensate each other). This is an example of **Simpson's paradox** — a case in which the possible effect of a third variable (**confounding**), here `Country`, must be accounted for when analysing the relationship between two variables.
""",
"images": ["statistics/images/ex3_7a3-product-sex-by-country.png"],
}

ex3["3_7b1"] = {
"title": "Ex 3.7 b1 — Conditional distribution of Age by Country",
"content": """**Question.** <span class="exam-question-text">Considering also the frequency of transactions made by customers of different age, look at the conditional distribution of buyer's age across countries by means of side-by-side boxplots. Are there meaningful differences in age behaviour from country to country?</span>

---

**Answer.** `Age` is **numerical** while `Country` is **qualitative**: to compare the distribution of `Age` across countries we use **side-by-side boxplots**.

```r
distr.plot.xy(y=Age, x=Country, plot.type="boxplot", col="lightblue",
              data=customer_habits)
```

The distributions seem quite similar with the exception of **Germany**, which is characterised by **higher age quartiles** than the other countries. More specifically, the median age of customers is similar — close to **43-44 years** in both France and the US — and slightly higher and just under **50 years** in the UK. German customers differ from those in other countries, with median age around **55**, the highest of all.

The age of the **youngest 25%** of customers varies roughly in the same range, from **17-18 years** to **38-39 years** in France, the UK and the US (the lowest and first quartiles are broadly aligned in the three countries). Again, **Germany stands out**, with the highest first quartile being equal to or higher than the medians of the other three countries: the youngest 25% of German customers are more heterogeneous in terms of age.

The **third quartiles** are quite different from country to country, resulting in a different dispersion of the middle 50% of the data (box). In particular, a higher concentration of the *"central"* ages is observed for US customers.
""",
"images": ["statistics/images/ex3_7b1-age-by-country.png"],
}

# ========== EXERCISE 3.8 (Quantity | Product_Category — promotional campaign analysis) ==========

ex3["3_8a"] = {
"title": "Ex 3.8 a — Mode of Quantity (marginal and | Product_Category)",
"content": """**Question.** <span class="exam-question-text">To evaluate some promotional campaigns aimed at boosting sales, analysts are interested in studying in detail how many units of product can be purchased in each transaction (`Quantity`). What is the most typical quantity of units purchased in the observed transactions? Are there differences by category (`Product_Category`)?</span>

---

**Answer.** We are interested in the **mode** of `Quantity`, both **marginally** and **conditional** on `Product_Category`.

```r
# Marginal central-tendency measures for Quantity
distr.summary.x(x=Quantity, stats="central", data=customer_habits)
##  Summary measures for Quantity
##  Central tendency measures
##      n   n.a   mode  n.modes  mode%  median  mean
##  34866    0      1      1     0.4035    3    2.64

# Conditional central tendency Quantity | Product_Category
distr.summary.x(x=Quantity, by1=Product_Category, stats="central",
                data=customer_habits)
##  Summary measures for Quantity | Product_Category
##  Central tendency measures
##  Product_Category    n   n.a  mode  n.modes  mode%  median  mean
##  Accessories      22534    0     1      1   0.4992     3    2.80
##  Bikes             7093    0     1      1   0.4984     2    1.80
##  Clothing          5239    0     1      1   0.4014     3    3.09
```

The **mode is 3** in transactions where `Accessories` are purchased and in those where `Clothing` is purchased. In the former case, the frequency of the mode is close to **50%**, while in the latter it is lower, at around **40%**.

As might be expected, for transactions involving **bicycles** — i.e. products with a higher commercial value and which are not usually bought in stock — the typical transaction (with frequency 49.8%) involves only **one piece**.

**Alternative angle.** You can also answer based on `Quantity | Product_Category` frequencies directly:

```r
distr.table.xy(Product_Category, Quantity, freq="percentage",
               freq.type="y|x", p.digits=1, total=F, data=customer_habits)
##  y|x: Percentages
##                  Quantity
##  Product_Category    1     2     3     4
##      Accessories   10.0  19.8  49.9  20.2
##      Bikes         49.8  30.2  10.1   9.9
##      Clothing      10.5   9.9  40.1  39.5
```

From the table, the mode for `Clothing` is **not particularly representative**: the frequency of transactions in which **4 pieces** were purchased is very close to **40%**, and the distribution is therefore **practically bimodal**.
""",
"images": ["statistics/images/ex3_8a-quantity-by-category.png"],
}

# ========== EXERCISE 3.9 (LoL - tier × class, then KDA | role + pick_perc | role) ==========

ex3["3_9a1"] = {
"title": "Ex 3.9 a1 — Distribution of tier within each class (League of Legends)",
"content": """**Question.** To compare `tier` across champions' classes, consider the distributions of `tier | class`. Build a factor to properly arrange the levels of `tier`, then represent the conditional distributions in a cross-table and a side-by-side bar plot. Which classes look the most/least promising? Are `tier` and `class` associated?

---

**Answer.**

**a1)** Being `tier` an ordinal variable, we preliminarily build a factor to properly arrange its levels. Since both variables take a limited number of distinct values, we arrange the conditional distributions in a cross-table and use a side-by-side bar plot to graphically display them.

```r
LoL$tier.f <- factor(LoL$tier, levels=c("F","E","D","C","B","A","S"))

distr.table.xy(x=class, y=tier.f, freq=c("percentage"), freq.type=c("y|x"), data=LoL)
distr.plot.xy(x=class, y=tier.f, freq=c("percentage"), plot.type="bars", bar.type="beside", data=LoL)
```

The class **Assassin** looks the most promising, with the highest percentage of champions with the highest levels of `tier` (A, S) and no champion with the lowest tier level. Instead, **Mage** and **Marksman** look the least convenient classes, because of the highest proportion of champions with the lowest tier levels; in particular, class Mage also presents the lowest proportion of champions with the highest tier, even if — compared to Marksman — it includes more champions with tier B and fewer with tier A.

**a2)** Since `tier` is ordinal, it is possible to determine only its mode and median across classes. The two central tendency measures differ by class. In particular, for **Assassin** both the mode and the median are equal to **A**; for **Marksman** they are both low and equal to **C**. The other classes all have median equal to **B**, and except for **Support** — with mode **A** — have mode **B**, even if the modes are characterized by a frequency only slightly higher than that of the other levels of `tier`. Summing up, the conditional distributions are characterized by different modes and medians which, if jointly reported, reflect quite accurately the distinctive characteristics of the champions in the classes with respect to `tier`.

**a3)** The variables are **associated**, because the conditional distributions are different one from another; in the case of associated variables, the latter cannot be identical to the marginal distribution of `tier`.
""",
"images": ["statistics/images/ex3_9a1-lol-tier-by-class.png"],
}

ex3["3_9b"] = {
"title": "Ex 3.9b — KDA conditioned on role",
"content": """**Question.** Compare the distribution of `KDA` across the levels of `role` using side-by-side boxplots.

---

**Answer.** `KDA` is a numerical variable, whereas `role` is a qualitative variable. Therefore, we use side-by-side boxplots to compare the conditional distributions.

```r
distr.plot.xy(x=role, y=KDA, plot.type="boxplot", data=LoL)
```

By comparing the boxplots, we observe the highest quartiles for roles **JUNGLE** and **SUPPORT**. In addition, the first quartile of **ADC** is lower than the first quartile of **TOP**; in fact, the third quartile of the latter is also at a higher level than the first quartile of the others. The IQR is similar for all the roles. The medians, instead, are aligned along the value 3.5, even if the median for **SUPPORT** is the highest. As to the upper outliers (no lower outliers in the boxplots), they are very different among groups, both for number and for value. Both the number and the value attain a peak for the group of role **SUPPORT**.
""",
"images": ["statistics/images/ex3_9b-kda-by-role.png"],
}

ex3["3_9c"] = {
"title": "Ex 3.9 c-d - score vs pick_perc, colored by role",
"content": """**Question.**

**c)** Discuss the relation between `score` and `pick_perc` of the champions, and the role of `role` in this relation.

**d)** Obtain the scatter plot of `score` vs `pick_perc`, distinguishing the points by `role`.

---

**Answer.**

**c)** The relation between `score` and `pick_perc` is linear, even if points cluster around two different lines. Indeed, if one excludes the (relatively few) points deviating from the cloud of points, the correlation coefficient would be higher. We can therefore conclude that players tend to select champions based on their **score**, which is quite reasonable - and expected - and that the correlation between the two variables is quite high, even if we have a "group-wise" linear relation, because the points in the scatter do not cluster around a unique line. It is clearly interesting to understand which are the points deviating from the others.

**d)** To obtain the scatter:

```r
distr.plot.xy(x=score, y=pick_perc, plot.type="scatter", var.c=role, data=LoL)
```

Thus, given a certain score, champions of role **ADC** are chosen more frequently compared to champions with other roles, and the percentage of games where they are chosen increases with the score at a higher rate compared to the other classes. Even if less clear, we note alignments along slightly different lines also for the other roles.
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
##  ML             204     0.31
##  M              198     0.30
##  MH             140     0.21
##  H              54      0.08
##  TOTAL          668     1.00
```

Since the variable is qualitative ordinal, the central tendency measures that can be calculated are the mode and the median.

```r
distr.summary.x(Company$Prod, stats=c("median","mode"))
##  n n.a median  mode  n.modes mode%
## 668 0   ML    ML       1     0.3054
```

The median is ML and the mode is ML; nonetheless the more suitable measure is the median. Indeed, the mode is not representative, since the distribution of `Prod` is almost bi-modal, with both the levels ML and M observed on about 30% of the clients.
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
##  Ecomm            156   0    ML    ML      1    0.4872
##  Mob              128   0    ML    ML      1    0.5312
##  Multi             96   0    M     M       1    0.7083
##  Trad             288   0    MH    MH      1    0.3750
```

The comparison can also be based on a stacked bar diagram displaying the distributions of `Prod` conditioned to `Channel` (a dashed line is reported corresponding to 0.5, to identify the median).

```r
distr.plot.xy(Company$Prod, Company$Channel, plot.type="bars",
              freq="perc", freq.type="x|y")
```

Referring to the modes and the medians of `Prod`, we note that such measures change depending on the customers' preferred purchase channel. Specifically, the mode and the median of `Prod` among clients who prefer the smartphone (Mob) are both medium-low (mode=median=ML), whereas customers keen to buy offline buy mostly products with medium-high profitability (mod=median=MH).
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

**Answer.** **a1)** In order to identify the modal class, we calculate the frequency densities defined as the ratio between the class relative frequency and the class width.

```r
distr.table.x(x=Loyalty, interval=T,
              freq=c("counts","prop","dens","cum"),
              data=Campaign)
##  Loyalty   Count   Prop   Density   Cum.Count   Cum.Prop
##  [10,20)    58     0.04   0.004      58          0.04
##  [20,40)   319     0.22   0.011     377          0.26
##  [40,50)   348     0.24   0.024     725          0.50
##  [50,70)   435     0.30   0.015    1160          0.80
##  [70,80)   174     0.12   0.012    1334          0.92
##  [80,100]  116     0.08   0.004    1450          1.00
##  TOTAL    1450     1.00
```

The modal class is **[40, 50)**, as it has the highest frequency density. The same result can be obtained by considering the histogram of the distribution.

**a2)** One is interested in identifying the 90th percentile. The class containing the 90th percentile is the first class in which the cumulative relative frequency exceeds 0.9, i.e. **[70, 80)**.

The 90-th percentile (approximated under the assumption of uniform frequency distribution within classes) is the value of `Loyalty` such that:

$$
P_{90} = 70 + \\frac{(0.9 - 0.8)}{0.012} = 78.333.
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

Hence $\\bar x = \\sum_k m_k\\cdot p_k = 52.2$. The population variance is $\\sigma^2 \\approx \\sum_k m_k^2 p_k - \\bar x^2 = 3096 - 52.2^2 = 371.16$. Since we are working with a sample, the sample variance is $s^2 = (1450/1449)\\cdot 371.16 = 371.4161$, and SD = $\\sqrt{371.4161}$.

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

**Answer.** **b1)** The boxplot is constructed from the 5-number summary:

```r
distr.summary.x(Revenues, stats="fivenumbers", data=Campaign)
##  n n.a min q1 median q3 max
## 1450 0 105.82 804.55 984 1202.36 3312.54
distr.plot.x(Revenues, plot.type="boxplot", data=Campaign)
```

The box extends from the first quartile (804.55) to the third quartile (1202.36) and is divided by the median value (984). The whiskers signal the presence of lower and upper outliers and extend from the box, respectively, to the lowest/highest observed value within the threshold for the detection of outliers. Specifically, the lower whisker extends from the first quartile (804.55) to the lowest regular value, i.e. greater than $804.55 - 1.5\\cdot(1202.36 - 804.55) = 207.285$. The upper whisker extends from the third quartile (1202.36) to the largest regular value, i.e. less than $1202.36 + 1.5\\cdot(1202.36 - 804.55) = 1799.075$.

**b2)** To graphically compare the distributions of `Revenues` at the different qualitative levels of `Location`, we use side-by-side (or parallel) boxplots:

```r
distr.plot.xy(x=Location, y=Revenues, plot.type="boxplot", data=Campaign)
```

The shapes of the distributions of `Revenues` conditioning on `Location` are different. With the exception of stores in the **Centre** — for which the distribution is close to symmetry — the other distributions show positive skew, mainly due to the presence of upper outliers, excluding which the two distributions are quite symmetrical. This asymmetry is particularly pronounced in **Hinterland** supermarkets, where the distance between the third quartile and the maximum is considerably greater than the distance between the first quartile and the minimum, and the box is also right-skewed. Higher quartiles are observed in Hinterland supermarkets, where distribution is significantly skewed to the right: in particular, the third quartile is higher than the highest non-extreme values of the other distributions. Excluding stores with particularly high Revenues (in suburbs and semi-central areas), about 25% of stores in the Hinterland have higher revenues than almost all stores in other areas. Stores in semi-central areas also have higher quartiles than stores in suburban or central areas, as well as a number of stores with very high revenues. Distributions for Hinterland and semi-central areas are also those with the highest range; we also note significant differences in terms of inter-quartile range, much higher for Revenues of stores in the Hinterland, and very small for Revenues of stores located in the suburbs and in the centre.

In conclusion, we can say that among the stores in the Hinterland, some have much higher Revenues than the stores located in other areas; even for the semi-central areas, some stores with very high Revenues can be observed. Supermarkets located in peripheral or central areas generally have lower revenues, with 75% having lower revenues than stores in semi-central and hinterland areas.
""",
"images": ["statistics/images/ex3_11b-revenues-boxplot.png"],
}

ex3["3_11c"] = {
"title": "Ex 3.11c — Relationship Sales vs Revenues and Sales vs Costs",
"content": """**Question.** Consider the relationship between `Sales` and `Revenues`, and between `Sales` and `Costs`. Which has the stronger relationship?

---

**Answer.** In order to assess the relationship between the two variables, it is essential to consider scatterplots to analyze the type of relationship between the variables. In the case of linear relationships, it is possible to refer to the correlation coefficient to quantify their intensity.

```r
distr.plot.xy(x=Revenues, y=Sales, plot.type="scatter",
              fitline=T, data=Campaign)
distr.plot.xy(x=Costs,    y=Sales, plot.type="scatter",
              fitline=T, data=Campaign)
```

From the plots, it can be observed that the strongest and most structured relationship is that between **Costs and Sales**, even though it is characterised by a non-linear trend in that a greater dispersion of campaign effectiveness is observed at higher costs (`Sales`). The relationship between `Revenues` and `Sales`, on the other hand, is particularly weak despite a number of observations arranged along a straight line. Note that the tendency of the data to concentrate around a line is more pronounced in the first plot. Neglecting therefore the non-linearity of the relationship between `Costs` and `Sales`, we note that the correlation coefficients are similar:

```r
cor(Campaign$Costs,    Campaign$Sales)
## [1] 0.7588883
cor(Campaign$Revenues, Campaign$Sales)
## [1] 0.7580242
```

Nonetheless the second coefficient is unreliable, as the link between `Sales` and `Revenues` is positive but of weak intensity, and definitely does not summarise the level of concentration of data around a straight line. The first coefficient — despite the fact that the relationship is not linear — synthesizes better the concentration of data around a straight line, although it would not explain the data relative to higher costs adequately.
""",
"images": ["statistics/images/ex3_11c-sales-scatter-pair.png"],
}

# ========== EXERCISE 3.12 (Effectiveness × Channel) ==========

EX312_TABLE = """A company launched a promotional campaign. An in-depth analysis is carried out on a sample of 720 customers. For these customers the effectiveness of the campaign (`Effectiveness`) and the purchasing channel typically used (`Channel`) is measured, among other things. The results are reported in the following table:

| Effectiveness Channel | Low | Medium-Low | Medium | Medium-High | High |
|----------------------:|----:|-----------:|-------:|------------:|-----:|
| Online                | 16  | 76         | 36     | 30          | 22   |
| Mobile App            | 56  | 68         | 72     | 12          | 16   |
| In-Store              | 25  | 38         | 90     | 108         | 63   |
"""

ex3["3_12a"] = {
"title": "<span class=\"exam-question-text\">Ex 3.12a — Effectiveness | Channel: medians for Online vs In-Store</span>",
"content": """<span class="exam-question-text">**Question.** Comment on the variable `Effectiveness` for customers who typically buy online (`Channel = Online`) and for customers who typically buy in-store (`Channel = In-Store`). What are your comments on the conditional distributions of `Effectiveness` and on the procedure followed to obtain them? What are your considerations on the median for each channel and on the procedure followed to obtain it?</span>

""" + EX312_TABLE + """

---

**Answer.** Build the conditional distribution of `Effectiveness` given `Channel = Online` and given `Channel = In-Store` (relative + cumulative frequencies):

| Channel | | Low | Medium-Low | Medium | Medium-High | High | tot |
|--------:|-:|----:|-----------:|-------:|------------:|-----:|----:|
| Online    | Fr.Absolute | 16    | 76    | 36    | 30    | 22    | 180 |
|           | Fr.Relative | 0.089 | 0.422 | 0.2   | 0.167 | 0.122 | 1   |
|           | Fr.Cumulate | 0.089 | 0.511 | 0.711 | 0.878 | 1     |     |
| In-Store  | Fr.Absolute | 25    | 38    | 90    | 108   | 63    | 324 |
|           | Fr.Relative | 0.077 | 0.117 | 0.278 | 0.333 | 0.194 | 1   |
|           | Fr.Cumulate | 0.077 | 0.194 | 0.472 | 0.805 | 1     |     |

The **mode** of `Effectiveness | Channel = Online` is the highest-frequency category (`Medium-Low`, relative frequency 0.422), and it is also the **median** (first level whose cumulative relative frequency exceeds 0.5: 0.511). For `Channel = In-Store` the mode and the median both equal `Medium-High` (first level whose cumulative relative frequency exceeds 0.5: 0.805). A higher campaign effectiveness is therefore observed for customers who prefer to shop in-store.

`Effectiveness` is an **ordinal** qualitative variable, so the median is obtained from the cumulative relative frequencies; the arithmetic mean is **not** defined on ordinal categories.

```r
distr.table.xy(x=Channel, y=Effectiveness, freq=c("percentage","cumulative"), data=Campaign)
```
""",
"images": [],
}

ex3["3_12b"] = {
"title": "<span class=\"exam-question-text\">Ex 3.12b — Proportion Medium / Medium-Low: In-Store vs Online</span>",
"content": """<span class="exam-question-text">**Question.** Can you say that the proportion of customers for whom the campaign had medium or medium-low effectiveness (`Effectiveness = Medium` or `Medium-Low`) is higher among customers who prefer to buy in-store (`Channel = In-Store`) than among customers who prefer to buy online (`Channel = Online`)? Clearly justify your answer, indicating the measures you use and their numerical values.</span>

---

**Answer.** Compare the two conditional proportions read off the table in 3.12a.

For customers who buy **in-store**:
$$
\\Pr(\\text{Eff} = \\text{Medium or Medium-Low} \\mid \\text{Channel} = \\text{In-Store}) = \\frac{38 + 90}{324} = 0.117 + 0.278 = 0.395.
$$

For customers who buy **online**:
$$
\\Pr(\\text{Eff} = \\text{Medium or Medium-Low} \\mid \\text{Channel} = \\text{Online}) = \\frac{76 + 36}{180} = 0.422 + 0.200 = 0.622.
$$

Since $0.395 < 0.622$, the proportion of customers with medium or medium-low effectiveness is in fact **lower** among in-store customers than among online customers. The statement is therefore **false**.

```r
distr.table.xy(x=Channel, y=Effectiveness, freq="percentage", data=Campaign)
```
""",
"images": [],
}

ex3["3_12c"] = {
"title": "<span class=\"exam-question-text\">Ex 3.12c — Does pushing In-Store cause higher Effectiveness?</span>",
"content": """<span class="exam-question-text">**Question.** Based on the observed data and assuming that customers will behave in the same way in future campaigns, do you think it is reasonable to conclude that if you encourage customers to buy in-store (`Channel = In-Store`), a campaign will be more effective (`Effectiveness`)? Explain why.</span>

---

**Answer.** From 3.12a–3.12b we know that, **conditional on** `Channel = In-Store`, the distribution of `Effectiveness` is shifted toward higher categories (median = `Medium-High`, only 39.5% of customers at `Medium` or below) compared with `Channel = Online` (median = `Medium-Low`, 62.2% of customers at `Medium` or below). This is a **descriptive association** between `Effectiveness` and `Channel`.

It is **not** legitimate to translate this association into a causal recommendation. The propensity to shop in-store (and the sensitivity to promotional campaigns) is likely linked to other customer characteristics — income, age category, level of loyalty, purchasing power, geographic area, etc. — which act as confounders. Forcing online customers to use the in-store channel would not automatically transfer the higher effectiveness observed on the (self-selected) in-store population.

**Conclusion.** The data show a strong association between `Channel` and `Effectiveness`, but the comparative experiment needed for a causal conclusion is missing. We cannot say that pushing customers in-store will, by itself, make campaigns more effective.

```r
distr.plot.xy(x=Channel, y=Effectiveness, plot.type="bars", freq.type="percentage", data=Campaign)
```
""",
"images": [],
}
