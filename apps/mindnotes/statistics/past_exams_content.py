"""
Past-exam snippets (13 exams, 2024–2026).
Each snippet's content wraps the question text in <span class="exam-question-text">
for blue styling, plus full verbatim answer + R commands (some inferred).
The is_exam flag tells the builder to mark the card yellow.
"""

# Helper to build snippet content
def _q(q, a, r=""):
    """Build content: blue Q + black A + R commands."""
    return (
        '<span class="exam-question-text">' + q.strip() + '</span>\n\n'
        '---\n\n'
        '**Answer.** ' + a.strip() + '\n\n'
        + (('**R commands:**\n\n`' + '`\n\n`'.join(r.strip().split('\n')) + '`\n') if r.strip() else '')
    )

past_exams = {}

# =================== 1st PARTIAL 2024 ===================

past_exams["exam_p1_2024_1a"] = {
"title": "P1-2024 Ex1a — Modal class of Loyalty (interval-class variable)",
"is_exam": True, "topic_hint": "G2",
"content": _q(
    "Consider the `Campaign` dataframe and the variable `Loyalty` — measured in interval classes — which represents the level of customer loyalty for the stores considered. What is the modal class of `Loyalty`? Explain clearly your answer and state the measures or tools you used to answer.",
    "`Loyalty` is a continuous quantitative variable measured in classes. To identify the modal class we compute the **frequency densities** (class relative frequency / class width). From the table: [10,20) dens=0.004, [20,40) 0.011, **[40,50) 0.024**, [50,70) 0.015, [70,80) 0.012, [80,100) 0.004. **The modal class is [40,50)**, as it has the highest frequency density. The same conclusion follows from the histogram (tallest bar over [40,50)).",
    "distr.table.x(x=Loyalty, interval=T, freq=c(\'counts\',\'prop\',\'dens\',\'cum\'), data=Campaign)\n## Loyalty   Count Prop Density Cum.Count Cum.Prop\n## [10,20)    58  0.04  0.004      58     0.04\n## [20,40)   319  0.22  0.011     377     0.26\n## [40,50)   348  0.24  0.024     725     0.50\n## [50,70)   435  0.30  0.015    1160     0.80\n## [70,80)   174  0.12  0.012    1334     0.92\n## [80,100)  116  0.08  0.004    1450     1.00"
), "images": []}

past_exams["exam_p1_2024_1b"] = {
"title": "P1-2024 Ex1b — Loyalty 90th percentile",
"is_exam": True, "topic_hint": "G3",
"content": _q(
    "What are the levels of loyalty in the 10% of stores with the most loyal customers (`Loyalty`)? Indicate clearly which measures you use to answer and their numerical values.",
    "Identify the **90th percentile** $P_{90}$. From the cumulative relative frequency table, class $[70,80)$ is the first whose cum.prop exceeds 0.9 (cum.prop just before = 0.80, density = 0.012). Under uniform-within-class assumption: $P_{90} = 70 + \\dfrac{0.9 - 0.8}{0.012} = 78.333$. The loyalty levels of the 10% most loyal stores therefore lie in the range $[78.333,\\,100]$.",
    "distr.table.x(x=Loyalty, interval=T,\n               freq=c('counts','prop','dens','cum'),\n               data=Campaign)\n## P90 = 70 + (0.9 - 0.8)/0.012 = 78.333"
), "images": []}

past_exams["exam_p1_2024_1c"] = {
"title": "P1-2024 Ex1c — Mean and variance of Loyalty (grouped data)",
"is_exam": True, "topic_hint": "G4",
"content": _q(
    "Determine the **mean** and **variance** of the variable `Loyalty` (continuous, measured in classes) in `Campaign`. Indicate clearly the procedure followed.",
    "Use class midpoints $m_k$ with absolute freq $f_k$ (or relative $p_k$). Classes [10,20),[20,40),[40,50),[50,70),[70,80),[80,100) → midpoints 15, 30, 45, 60, 75, 90. **Mean:** $\\bar X \\approx \\tfrac{1}{n}\\sum_k f_k m_k = \\tfrac{1}{1450}(15\\cdot58 + 30\\cdot319 + \\dots + 90\\cdot116) = 52.2$. **Variance (population):** $s^2 \\approx \\sum_k m_k^2 p_k - \\bar X^2 = 3096 - 52.2^2 = 371.16$. With sample correction: $s^2_{n-1} = \\tfrac{1450}{1449}\\cdot 371.16 \\approx 371.4161$. (Difference vs ungrouped is negligible for large $n$.)",
    "mids <- c(15,30,45,60,75,90)\nfk   <- c(58,319,348,435,174,116)\nxbar <- sum(fk*mids)/sum(fk)            # 52.2\nvar_pop <- sum(fk*mids^2)/sum(fk) - xbar^2   # 371.16"
), "images": []}

past_exams["exam_p1_2024_2a"] = {
"title": "P1-2024 Ex2a — Campaign Revenues boxplot",
"is_exam": True, "topic_hint": "G6",
"content": _q(
    "Consider the `Campaign` dataframe. Refer to the boxplot representing the distribution of the variable `Revenues` (standard profitability of the stores). Indicate what the extremes of the box and the end points of the whiskers of the boxplot represent, and report their numerical values, clarifying what are the quantities underlying your answer.",
    "n = 1450 (no missing). 5-number summary: Min 105.82, Q1 804.55, Median 984, Q3 1202.36, Max 3312.54. The box spans from $Q_1 = 804.55$ to $Q_3 = 1202.36$ and is divided by the median 984. IQR $= 1202.36 - 804.55 = 397.81$. The whiskers extend to the most extreme observed values still within $[Q_1 - 1.5\\cdot IQR,\\, Q_3 + 1.5\\cdot IQR] = [207.84, 1799.07]$; observations beyond the upper fence (up to Max 3312.54) appear as individual outlier points. The right tail is much longer than the left — distribution **right-skewed**.",
    "distr.summary.x(Revenues, stats='fivenumber', data=Campaign)\n## n.n.a   min     q1 median      q3    max\n## 1450 0 105.82 804.55    984 1202.36 3312.54\ndistr.plot.x(Revenues, plot.type='boxplot', data=Campaign)"
), "images": ["statistics/images/exam_p1_2024_revenues.png"]}

past_exams["exam_p1_2024_2b"] = {
"title": "P1-2024 Ex2b — Revenues by Location (side-by-side boxplots)",
"is_exam": True, "topic_hint": "G8",
"content": _q(
    "Refer to the side-by-side boxplots of `Revenues` conditional on `Location` (Semi-Central, Peripheral, Hinterland). Comment on the relationship between the two variables and on the strength of any association.",
    "Since `Revenues` is numeric and `Location` is categorical, association is judged by comparing the conditional distributions of `Revenues` given `Location`. The three boxplots are essentially identical: medians coincide, IQRs (box heights) are very similar, and the whisker extents / outlier patterns overlap. The conditional distributions of Revenues are practically equal across the three groups → **weak (essentially no) association** between Revenues and Location. Knowing the Location does not help predict the level of Revenues.",
    "distr.plot.xy(x=Revenues, y=Location, plot.type='boxplot', data=Campaign)\ndistr.summary.xy(Revenues, Location, stats=c('fivenumber','mean','sd'), data=Campaign)"
), "images": []}

past_exams["exam_p1_2024_3a"] = {
"title": "P1-2024 Ex3 — CV comparison Company A vs B",
"is_exam": True, "topic_hint": "G5",
"content": _q(
    "Compare the dispersion of `Loyalty` between Company A (under investigation, mean 52.2, variance 371.16, SD $\\approx 19.26$) and competitor Company B (mean 65.0, SD 19.0) via the coefficient of variation, to decide where loyalty shows higher relative variability.",
    "$CV = s/|\\bar x|$. **Company A:** $CV_A = \\sqrt{371.16}/52.2 = 19.26/52.2 = 0.3691$. **Company B:** $CV_B = 19.0/65.0 = 0.292$. Since $CV_A > CV_B$, **loyalty is more dispersed in the previous company (A) than in the competitor (B)**, in relative terms.",
    "# Company A (under investigation)\nmean_A <- 52.2\nvar_A  <- 371.16\nsd_A   <- sqrt(var_A)   # 19.2655\nCV_A   <- sd_A / mean_A # 0.3691\n\n# Company B (competitor)\nmean_B <- 65.0\nsd_B   <- 19.0\nCV_B   <- sd_B / mean_B # 0.2923\n\nc(CV_A = CV_A, CV_B = CV_B)\n## CV_A   CV_B\n## 0.3691 0.2923"
), "images": []}

past_exams["exam_p1_2024_4a"] = {
"title": "P1-2024 Ex4 — Sales vs Costs & Revenues: which correlation is stronger?",
"is_exam": True, "topic_hint": "G9",
"content": _q(
    "Refer to the `Campaign` dataframe. Using scatter plots and the linear correlation coefficient, study the relationship between `Sales` and the other two quantitative variables `Costs` and `Revenues`. With which of the two variables is `Sales` most correlated?",
    "Both scatter plots show **positive linear relationships**. Pearson's correlation gives $r(\\text{Sales}, \\text{Costs}) \\approx 0.76$ and $r(\\text{Sales}, \\text{Revenues}) \\approx 0.42$. Since $|0.76| > |0.42|$, **`Sales` is most correlated with `Costs`** (strong/medium-high positive linear relationship), whereas the link with `Revenues` is only moderate. Visually, the Sales–Costs cloud is tighter around the fit line; the Sales–Revenues cloud is more dispersed.",
    "cor(Campaign[, c('Sales','Costs','Revenues')])\ndistr.plot.xy(x=Costs, y=Sales, plot.type='scatter', fitline=T, data=Campaign)\ndistr.plot.xy(x=Revenues, y=Sales, plot.type='scatter', fitline=T, data=Campaign)"
), "images": ["statistics/images/exam_p1_2024_costs_sales.png"]}

past_exams["exam_p1_2024_5a"] = {
"title": "P1-2024 Ex5 — Channel × Effectiveness contingency (n=725)",
"is_exam": True, "topic_hint": "G7",
"content": _q(
    "A company launched a promotional campaign. An in-depth analysis is carried out on a sample of $n=725$ customers. For each customer the *Channel* used to interact with the company (E-commerce / Multi-channel / Channel 2 / Mobile App) and the perceived *Effectiveness* of the campaign (Ineffective / Low / Medium / High) are observed. Analyze the relationship between the two variables. In particular, which Channel is associated with the highest perception of effectiveness?",
    "Conditional row distributions of Effectiveness given Channel show a clear pattern: **Mobile App and Multi-channel customers have the highest combined share of Medium + High effectiveness ratings**, while **E-commerce customers concentrate the most in Ineffective / Low** (lowest perceived effectiveness). Channel 2 sits in between. Because the conditional distributions of Effectiveness vary noticeably across Channels, the two categorical variables are **associated** (non-independent): the perceived effectiveness of the campaign depends on the interaction channel.",
    "distr.table.xy(Company$Channel, Company$Effectiveness, freq=c('counts','percentages'), freq.type='y|x')\ndistr.plot.xy(x=Effectiveness, y=Channel, plot.type='bars', stack=TRUE, data=Company)"
), "images": []}

past_exams["exam_p1_2024_6a"] = {
"title": "P1-2024 Ex6a — CLT: P(total spend > 1000) for n=80 customers",
"is_exam": True, "topic_hint": "G11",
"content": _q(
    "A shop expects to make more than 1000 euros in total takings during the next hour. Specify clearly whether and what assumptions are needed to determine the required probability. Let $X$ = amount spent by a customer; we know $E[X] = 12$ and $\\mathrm{Var}(X) = 5^2 = 25$. The total amount spent by a sample of $n = 80$ customers is the random variable $S = X_1 + \\cdots + X_{80}$. Compute $P(S > 1000)$.",
    "**Assumptions:** the $X_i$ are i.i.d. (independent customers, same spend distribution). Since $n = 80$ is large, by the CLT the sum is approximately normal regardless of the distribution of an individual customer. Thus $S \\;\\dot\\sim\\; N(n\\mu,\\, n\\sigma^2) = N(80 \\cdot 12,\\, 80 \\cdot 25) = N(960,\\, 2000)$. Standardising: $z = (1000-960)/\\sqrt{2000} = 40/44.72 \\approx 0.894$, so $P(S > 1000) = 1 - \\Phi(0.894) \\approx 0.1855$.",
    "p_S <- 1 - pnorm(1000, mean=960, sd=sqrt(2000))\np_S\n## [1] 0.1855467"
), "images": []}

past_exams["exam_p1_2024_6b"] = {
"title": "P1-2024 Ex6b — Sample proportion of shops",
"is_exam": True, "topic_hint": "G11",
"content": _q(
    "Assume that in the city there are $115$ shops with the same characteristics as those considered in point a, and assume that exactly $80$ customers in each outlet take advantage of the promotion. What is the probability that the proportion of outlets where the $80$ customers spend more than $1000$ euros in total is less than $0.15$? (If you did not answer point a, assume the required probability was $0.2$).\n\nLet $\\hat P$ be the random variable describing the proportion of outlets (out of the $115$) in which the $80$ customers spend more than $1000$ euros in total. From point a we have $\\pi \\approx 0.1855$, so $\\hat P$ is approximately\n$$\\hat P \\sim N\\!\\left(0.1855,\\; \\frac{0.1855 \\cdot (1-0.1855)}{115}\\right)$$",
    "By the CLT, the sampling distribution of $\\hat P$ is approximately Normal with mean $\\pi \\approx 0.1855$ and variance $\\pi(1-\\pi)/n = 0.1855 \\cdot 0.8145 / 115$. The required probability is therefore\n$$P(\\hat P < 0.15) \\approx 0.166$$",
    "pnorm(0.15, 0.1855, sqrt(0.1855*0.8145/115))\n## [1] 0.1636914"
), "images": []}

# =================== 1st PARTIAL 2025 ===================

past_exams["exam_p1_2025_1a"] = {
"title": "P1-2025 Ex1a — Conditional Impressions distribution by Paid (Metrics2)",
"is_exam": True, "topic_hint": "G8",
"content": _q(
    "Compare the conditional distributions of `Impressions` (post views, in hundreds) between Paid (Yes) and non-Paid (No) posts. Use side-by-side boxplots and characteristics (centre/spread/shape).",
    "Side-by-side boxplots show both conditional distributions are **right-skewed** with substantial variation (large IQR), but **Paid posts have a higher median** and a markedly higher upper quartile than non-Paid: roughly **75% of Paid posts exceed the 75th percentile of non-Paid posts**. Conditional distributions differ → Paid status and Impressions are associated.",
    "distr.plot.xy(x=Impressions, y=Paid, plot.type='boxplot', data=Metrics2)\ndistr.summary.x(Impressions, by=Paid, stats='fivenumber', data=Metrics2)"
), "images": []}

past_exams["exam_p1_2025_1b"] = {
"title": "P1-2025 Ex1.a2 — Best location measure for right-skewed Impressions by Paid",
"is_exam": True, "topic_hint": "G6",
"content": _q(
    "Taking into account the **features** and the **shapes** of the two Impressions distributions (Paid vs non-Paid), which **location measure** would you use in order to suitably emphasize their differences? Explain your choice and report the values of the considered measures.",
    "**Use the median (with Q25, Q75) — not the mean.** Both Impressions distributions are **strongly right-skewed** with a long upper tail and many high outliers (and a wide IQR). Under right skewness the **mean is pulled upward by the heavy tail**, so it confounds *typical* level with *tail mass*, and it is non-robust to the outliers visible in the boxplots. The **median** (together with Q25 and Q75) is **robust** to outliers/skew and reflects the location of the bulk of the distribution, so it cleanly emphasizes the shift between Paid and non-Paid posts.\n\nFrom the side-by-side boxplots / five-number summary on Metrics2: **median(Impressions | Paid = yes) ≈ 67** vs **median(Impressions | Paid = no) ≈ 52** (Q25/Q75 = 45/185 vs 36/93). So Paid posts have a clearly higher *typical* number of impressions and a much wider spread. The corresponding **means** are **143 (Paid)** vs **107 (non-Paid)** — both are *much* larger than the medians (143 > 67 and 107 > 52), confirming the heavy right tail pulls the mean up. The mean-gap (≈36) overstates the *typical* difference relative to the median-gap (≈15). **Q25 / Q50 / Q75 per group is the right summary**, and the **median** is the location measure to report.",
    "# Compare location across Paid groups for right-skewed Impressions\ndistr.summary.x(Impressions, by=Paid, stats=\'fivenumber\', data=Metrics2)\ndistr.summary.x(Impressions, by=Paid, stats=c(\'mean\',\'median\',\'IQR\'), data=Metrics2)\n# Visual: side-by-side boxplots make the median shift obvious\ndistr.plot.xy(x=Impressions, y=Paid, plot.type=\'boxplot\', data=Metrics2)\n# Base-R equivalents\ntapply(Metrics2$Impressions, Metrics2$Paid, median)\ntapply(Metrics2$Impressions, Metrics2$Paid, mean)"
), "images": ["statistics/images/exam_p1_2025_impressions_by_paid.png"]}

past_exams["exam_p1_2025_2a"] = {
"title": "P1-2025 Ex2a — SE of sample proportion (Shares=low vs high)",
"is_exam": True, "topic_hint": "G11",
"content": _q(
    "For Shares=high ($\\hat p = 0.32$, $n = 550$) and Shares=low ($\\hat p = 0.173$, $n = 550$), compute the standard errors.",
    "$SE(\\hat p_{\\text{high}}) = \\sqrt{0.32(1-0.32)/550} = 0.020$. $SE(\\hat p_{\\text{low}}) = \\sqrt{0.173(1-0.173)/550} = 0.016$.",
    "sqrt(0.32*(1-0.32)/550)\nsqrt(0.173*(1-0.173)/550)"
), "images": []}

past_exams["exam_p1_2025_2b"] = {
"title": "P1-2025 Ex2b — Misinterpretation of SE",
"is_exam": True, "topic_hint": "G11",
"content": _q(
    "Can we conclude from a lower SE that the *specific* estimate is closer to the parameter? Justify.",
    "**No.** A lower SE means generic estimates from this estimator are more tightly clustered around the unknown parameter on average. It does NOT measure the distance of any *specific* realised estimate from the parameter. No conclusion can be drawn about accuracy of a single observed estimate.",
    "# Simulate to see SE is a property of the estimator, not a single estimate\nset.seed(1); sims <- replicate(1000, mean(rnorm(50, mean=0, sd=1)))\nsd(sims)         # empirical SE of x-bar\n## [1] 0.1410\n1/sqrt(50)       # theoretical SE\n## [1] 0.1414"
), "images": []}

# =================== 1st PARTIAL 2026 ===================

past_exams["exam_p1_2026_1a"] = {
"title": "P1-2026 Ex1a — Boxplots of Bid by Channel (Bidding)",
"is_exam": True, "topic_hint": "G6",
"content": _q(
    "Use side-by-side boxplots of `Bid` by `Channel` (Aggregator / Agency / Airline) in the `Bidding` dataset to compare the bid distributions across the three channels (shape, spread, position, outliers).",
    "**Shape.** `Airline` is strongly **left-skewed** — long lower whisker plus a column of low-side outliers pulls the lower tail down. `Agency` and `Aggregator` are roughly **symmetric** (median sits near the middle of the box); `Agency` still shows outliers on both tails.\n\n**Spread (IQR & range).** `Agency` has the **smallest IQR** (tightest box). `Aggregator` has the **smallest total range** (shortest whisker-to-whisker extent). `Airline` is the most dispersed once outliers are counted.\n\n**Position.** `Agency` is **shifted down**: its median and even its **Q3 sit below the Q1** of both `Aggregator` and `Airline`, so a typical Agency bid is lower than the bottom 25% of bids on the other two channels. `Aggregator` and `Airline` have nearly identical medians (~55).",
    "distr.plot.xy(x=Bid, y=Channel, data=Bidding, plot.type='boxplot')\n# Numeric backup of the visual reading\ndistr.summary.xy(Bid, Channel, stats=c('fivenumber','IQR'), data=Bidding)"
), "images": ["statistics/images/exam_p1_2026_bid_by_channel.png"]}

past_exams["exam_p1_2026_1b"] = {
"title": "P1-2026 Ex1b — Central tendency: median for skewed Airline",
"is_exam": True, "topic_hint": "G4",
"content": _q(
    "Which measure of central tendency would you use to summarize the three Bid distributions across Channels (Agency, Aggregator, Airline)?",
    "Pick the measure based on each channel's shape (read from the boxplots in Ex1a):\n\n- **Agency**: roughly **symmetric** → **mean** (median equally fine; both coincide).\n- **Aggregator**: fairly **symmetric** → **mean** is appropriate.\n- **Airline**: strong **left skew** with a long lower tail / low outliers → **median**, because the mean is pulled down by the tail and misrepresents the typical bid.\n\nGood practice: **report median alongside mean** for all three, so the reader sees both the typical value and the effect of skew/outliers. The mean is sensitive to extreme values; the median is robust and reflects the middle 50% better when the distribution is skewed.",
    "# Means and medians by Channel (use both — pick by shape)\ndistr.summary.xy(Bid, Channel, stats=c('mean','median'), data=Bidding)\n# Visual confirmation of skew:\ndistr.plot.xy(x=Bid, y=Channel, plot.type='boxplot', data=Bidding)\n# Rule of thumb:\n#   symmetric  -> mean  (Agency, Aggregator)\n#   skewed/outliers -> median (Airline)"
), "images": []}

past_exams["exam_p1_2026_1c"] = {
"title": "P1-2026 Ex1c — Is Bid=35 by Aggregator extremely low? (Tukey rule)",
"is_exam": True, "topic_hint": "G3",
"content": _q(
    "Can a bid of 35 by a Channel=Aggregator customer be considered extremely low?",
    "Use **Tukey's lower fence** for Aggregator: $L = Q_1 - 1.5 \\cdot IQR$. From the Aggregator summary: $Q_1 = 50.8225$, $IQR = 11.895$, so $L = 50.8225 - 1.5 \\cdot 11.895 = 50.8225 - 17.8425 = \\mathbf{32.98}$. A value is flagged as an extreme low outlier only if it falls **below** the fence. Since $35 > 32.98$, **the bid of 35 is NOT extremely low** — it is unusual but lies inside the lower whisker, not in outlier territory.",
    "# Aggregator subgroup\nQ1  <- 50.8225\nQ3  <- 62.7175\nIQR <- Q3 - Q1    # 11.895\nlower_fence <- Q1 - 1.5 * IQR\nlower_fence\n## [1] 32.98\n35 > lower_fence  # TRUE -> not an extreme low\n## [1] TRUE\n# Equivalent in R from the data:\ndistr.summary.xy(Bid, Channel, stats=c('Q1','Q3','IQR'), data=Bidding)"
), "images": []}

# =================== GENERAL 1 2024 ===================

past_exams["exam_g1_2024_1a"] = {
"title": "G1-2024 Ex1.a — Boxplot of Read2 by Lunch (free vs not-free)",
"is_exam": True, "topic_hint": "G3",
"content": _q(
    "Propose a graphical representation that effectively describes the possible differences between the distributions of the reading scores (**Read2**) of students qualified or not for free lunch (**Lunch**). Report a sketch of the graph: what conclusions can you draw about the differences in the reading abilities of more or less disadvantaged pupils?",
    "Side-by-side **boxplots** of *Read2* split by *Lunch* level (`free` / `not-free`). The boxplot is the right tool: it summarises centre, spread and tails of a continuous variable across the levels of a categorical one.\n\n**Reading the plot.** The range of *Read2* is similar across the two Lunch groups, but ignoring extreme values the dispersion of *not-free* (i.e. **not** qualified for free lunch) is clearly smaller, and both groups look roughly symmetric. The whole *not-free* box sits above the *free* box: the **median, $Q_1$ and $Q_3$ of not-free are above the corresponding quartiles of free**.\n\n**Conclusion.** Students NOT qualified for free lunch (less economically disadvantaged) perform systematically better in reading. In particular: 50% of *free*-lunch students score below the 25th percentile of *not-free* students, and 75% of *free* students score below the median of *not-free* students — a clear gap penalising more disadvantaged pupils.",
    "distr.plot.xy(y=Read2, x=Lunch, plot.type=\"boxplot\", data=Primary)"
), "images": []}

past_exams["exam_g1_2024_1b"] = {
"title": "G1-2024 Ex1b — Sample size for proportion ME ≤ 0.04 at 95%",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "What sample size is needed so that the margin of error on a proportion is at most 0.04 at the 95% level?",
    "For a proportion: $ME = z_{0.975}\\sqrt{\\hat p(1-\\hat p)/n} \\le 0.04$. Worst-case $\\hat p = 0.5 \\Rightarrow \\hat p(1-\\hat p) = 0.25$. So $n \\ge (1.96/0.04)^2 \\cdot 0.25 = 49^2 \\cdot 0.25 = 600.25$. So **$n \\ge 601$**.",
    "ceiling((qnorm(0.975)/0.04)^2 * 0.25)\n## [1] 601"
), "images": []}

past_exams["exam_g1_2024_2a"] = {
"title": "G1-2024 Ex3 — Read2 vs Math2 correlation = 0.77",
"is_exam": True, "topic_hint": "G9",
"content": _q(
    "Interpret a correlation $r = 0.77$ between PrimaryRead2 and PrimaryMath2.",
    "Strong positive linear relationship: higher reading scores associate with higher math scores. $r^2 = 0.59 \\Rightarrow$ ~59% of variance shared. From the scatter, the cloud follows a roughly straight rising line with moderate scatter.",
    "cor(PrimaryRead2, PrimaryMath2)   # 0.77\ndistr.plot.xy(x=PrimaryRead2, y=PrimaryMath2, plot.type='scatter', fitline=T)"
), "images": ["statistics/images/exam_g1_2024_read_math.png"]}

past_exams["exam_g1_2024_2b"] = {
"title": "G1-2024 Ex3b — Heteroscedasticity in the Read2/Math2 scatter",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Identify any violations of regression assumptions from the scatter.",
    "Visible **heteroscedasticity**: dispersion of points around the line increases at higher Read2 scores. Linear regression assumption of constant error variance is violated → standard errors / inference become unreliable. Consider weighted least squares or variance-stabilizing transform.",
    "mod <- lm(PrimaryMath2 ~ PrimaryRead2)\nplot(mod, which=1)   # Residuals vs fitted"
), "images": ["statistics/images/exam_g1_2024_read_math.png"]}

# =================== GENERAL 1 2025 ===================

past_exams["exam_g1_2025_1a"] = {
"title": "G1-2025 Ex1a — SleepQuality 95th percentile",
"is_exam": True, "topic_hint": "G6",
"content": _q(
    "Find the threshold separating the top 5% of subjects by `SleepQuality` from the others.",
    "Take the 95th percentile of SleepQuality: **9.64** in the sample.",
    "quantile(sleep$SleepQuality, probs=0.95)\n# OR\ndistr.summary.x(SleepQuality, stats='p95', data=sleep)"
), "images": ["statistics/images/exam_g1_2025_sleepquality.png"]}

past_exams["exam_g1_2025_1b"] = {
"title": "G1-2025 Ex1b — Plot for SleepQuality tails",
"is_exam": True, "topic_hint": "G1",
"content": _q(
    "Plot to assess the tails of `SleepQuality` accurately.",
    "Combine **histogram with ~20 bins** and **boxplot** on the same data. Distribution concentrates at central values (light tails) — the boxplot is less informative than the histogram in this specific case.",
    "distr.plot.x(SleepQuality, plot.type='histogram', breaks=20, data=sleep)\ndistr.plot.x(SleepQuality, plot.type='boxplot', data=sleep)"
), "images": ["statistics/images/exam_g1_2025_sleepquality.png"]}

past_exams["exam_g1_2025_2a"] = {
"title": "G1-2025 Ex3 — Paired t-test sleep duration (minutes) pre vs post diet",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Test whether sleep duration (in minutes) increased after the diet. Paired sample, $n = 161$, $\\bar x_{\\text{before}} = 402.89$, $s_{\\text{before}} = 45.61$, $\\bar x_{\\text{after}} = 414$, $s_{\\text{after}} = 48$, correlation $r = 0.71$. One-sided test $H_0: \\mu_{\\text{after}} = \\mu_{\\text{before}}$ vs $H_1: \\mu_{\\text{after}} > \\mu_{\\text{before}}$.",
    "Paired t-test using $\\hat\\sigma_D = \\sqrt{s_{\\text{before}}^2 + s_{\\text{after}}^2 - 2r\\cdot s_{\\text{before}}\\cdot s_{\\text{after}}} = \\sqrt{45.61^2 + 48^2 - 2(0.71)(45.61)(48)} \\approx 35.71$. Then $t_{\\text{obs}} = (414 - 402.89)/(35.71/\\sqrt{161}) \\approx 3.95$ on $df = 160$. p-value $= P(T_{160} \\ge 3.95) \\approx 5.85 \\times 10^{-5}$. **Reject $H_0$ at any conventional $\\alpha$** — sleep duration in minutes significantly increased after the diet.",
    "t.test(after, before, paired=TRUE, alternative='greater')\nsd_D <- sqrt(45.61^2 + 48^2 - 2*0.71*45.61*48)\nt_stat <- (414 - 402.89)/(sd_D/sqrt(161))\n1 - pt(t_stat, df=160)"
), "images": []}

past_exams["exam_g1_2025_3a"] = {
"title": "G1-2025 Ex4 — Multiple regression SleepQuality ~ Stress+Age+BMI+Physical",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Estimate `SleepQuality ~ Stress + Age + BMI + Physical` on `SleepData`. Interpret the fit and predict mean SleepQuality at Stress=7, Age=40, BMI='Normal', Physical=50 with a 95% CI.",
    "From `summary(mod)`: all predictors significant; **Adjusted $R^2 = 0.5468$**, $F(4, 290) = 79.39$, p-value $< 2.2 \\times 10^{-16}$ — the model explains ~55% of the variance in SleepQuality and is jointly highly significant. Predicted mean SleepQuality for the given profile: $\\hat y = 6.827$, **95% CI = [6.235, 7.419]**.",
    "mod <- lm(SleepQuality ~ Stress + Age + BMI + Physical, data=SleepData)\nsummary(mod)\npredict(mod, newdata=data.frame(Stress=7, Age=40, BMI='Normal', Physical=50), interval='confidence')"
), "images": []}

past_exams["exam_g1_2025_3b"] = {
"title": "G1-2025 Ex3b — PhysicalActivity loses significance with Steps added",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Why does PhysicalActivity stop being significant when Steps is added?",
    "**Multicollinearity.** Steps and PhysicalActivity are strongly correlated (more steps ↔ more active). When both predictors are in the model, neither contributes uniquely — each loses individual significance even though jointly they explain part of the variance.",
    "cor(sleep$Steps, sleep$PhysicalActivity)\nmod_full <- lm(SleepQuality ~ Age+PhysicalActivity+Hours+Steps, data=sleep)\nsummary(mod_full)\ncar::vif(mod_full)"
), "images": []}

past_exams["exam_g1_2025_3c"] = {
"title": "G1-2025 Ex3c — Homoscedasticity assumption",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "State the homoscedasticity assumption.",
    "**$\\mathrm{Var}(\\varepsilon_i) = \\sigma^2$ for every $i$** — error terms have constant variance, independent of the predictors' values. Diagnose by plotting residuals vs fitted; look for fanning/cones (violation).",
    "plot(mod, which=1)   # Residuals vs fitted\nplot(mod, which=3)   # Scale-location"
), "images": []}

# =================== GENERAL 1 2026 ===================

past_exams["exam_g1_2026_1a"] = {
"title": "G1-2026 Ex1a — 99% CI for PurposeLoan=Business proportion",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "We are interested in the proportion of customers who apply for a loan for reasons related to Business (variable PurposeLoan = Business). Report a 99% confidence interval for such proportion and provide its interpretation.",
    "**Interval**: $(0.15\\,;\\,0.24)$. **Interpretation**: with a level of confidence 99% we can conclude that the proportion of interest (customers who apply for a loan for reasons related to business) lies between 0.15 and 0.24.",
    "CI.prop(PurposeLoan=='Business', conf.level=0.99, data=Credit)\n## n phat   s_X    se    Lower Upper\n## ... 0.20 ...  ...   0.15  0.24"
), "images": ["statistics/images/exam_g1_2026_purposeloan.png"]}

past_exams["exam_g1_2026_1b"] = {
"title": "G1-2026 Ex1b — Hypothesis test using CI",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Using the CI from 1a (0.15, 0.24), test $H_0: p = 0.3$ vs $H_1: p \\ne 0.3$ at any level $\\alpha$.",
    "Since $0.3 \\notin [0.15, 0.24]$, the 99% CI **rejects** $H_0$ at level $\\alpha = 0.01$. Equivalently, any test at $\\alpha \\ge 0.01$ rejects. At $\\alpha < 0.01$ (e.g. 0.005), the conclusion would require a wider CI to verify.",
    "# CI-test duality: 0.3 outside the 99% CI => reject H0 at alpha = 0.01\nTEST.prop(PurposeLoan, success='Business', p0=0.3, alternative='two.sided', data=Loans)\n# manual: 1-sample prop test\nprop.test(x=sum(Loans$PurposeLoan=='Business'), n=nrow(Loans),\n          p=0.3, alternative='two.sided', conf.level=0.99)"
), "images": []}

past_exams["exam_g1_2026_1c"] = {
"title": "G1-2026 Ex1c — Sample size for CI width ≤ 0.09",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "What sample size guarantees a 99% CI with width $\\le 0.09$?",
    "$ME \\le 0.045$. Take worst-case $p = 0.5$: $n \\ge (z_{0.995}\\cdot 0.5)^2/0.045^2 = (2.576\\cdot 0.5)^2/0.045^2 = 819.12$. **Minimum n = 820**.",
    "ceiling((qnorm(0.995)*0.5/0.045)^2)\n# = 820"
), "images": []}

# =================== GENERAL 2 2024 ===================

past_exams["exam_g2_2024_5a"] = {
"title": "G2-2024 Ex5 — Analytic CI for proportion (CrimePeople > 250)",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Build a 99% CI for the proportion of US cities with CrimePeople > 250.",
    "Analytic form: $\\hat p \\pm z_{\\alpha/2}\\cdot \\sqrt{\\hat p(1-\\hat p)/n}$. With $\\hat p = 0.21$, $n = 485$, 99% CI = $[0.16, 0.26]$. Interpretation: with 99% confidence the proportion of U.S. cities with CrimePeople > 250 lies between 0.16 and 0.26.",
    "vec.binA <- CrimeUS$CrimePeople > 250\nCI.prop(vec.binA, conf.level=0.99)\n## n phat   s_X    se    Lower Upper\n## 485 0.21 0.41 0.02 0.16  0.26"
), "images": ["statistics/images/exam_g2_2024_crime.png"]}

past_exams["exam_g2_2024_5c"] = {
"title": "G2-2024 Ex5c — Sample size for CI width ≤ 0.05",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "What sample size guarantees a 99% CI with width $\\le 0.05$?",
    "$ME \\le 0.025$. Worst-case $p = 0.5$: $n \\ge (2.576 \\cdot 0.5)^2/0.025^2 = 2654.31$. **Minimum n = 2655 cities**.",
    "z <- qnorm(0.995); p_max <- 0.5; ME <- 0.025\nn_needed <- ceiling((z * sqrt(p_max*(1-p_max)) / ME)^2)\nn_needed\n## [1] 2655"
), "images": []}

# =================== GENERAL 2 2025 ===================

past_exams["exam_g2_2025_1a"] = {
"title": "G2-2025 Ex1 — Boxplots of Salary by Employment_type",
"is_exam": True, "topic_hint": "G8",
"content": _q(
    "Draw side-by-side **boxplots of `Salary` by `Employment_type`** (Junior / Senior / Manager) and compare the conditional distributions: location, IQR (spread) and shape.",
    "**Medians** clearly increase with seniority: Junior $\\approx 2{,}045$, Senior $\\approx 3{,}545$, Manager $\\approx 4{,}218$. **Spread** (IQR) is smallest for Juniors ($\\approx 755$) and largest for Seniors ($\\approx 1{,}799$), with Managers in between ($\\approx 1{,}312$). **Shape**: Junior distribution is tight and roughly symmetric; Senior is the most dispersed and slightly right-skewed (longer upper whisker); Manager sits highest with moderate spread. Conclusion: salary is **strongly associated** with employment type — both location and variability change across groups.",
    "distr.plot.xy(x=Employment_type, y=Salary, plot.type='box', data=Employee)\ndistr.summary.xy(Employment_type, Salary, stats=c('fivenumber','IQR'), data=Employee)\n# numeric backup\nboxplot(Salary ~ Employment_type, data=Employee, horizontal=TRUE, col='navy')"
), "images": ["statistics/images/exam_g2_2025_salary_by_emptype.png"]}

past_exams["exam_g2_2025_2a"] = {
"title": "G2-2025 Ex2 — Chi-square GoF on Department + CI for Senior Salary",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "(1) Test whether the `Department` distribution in `Employee` is uniform across the three departments (HR / IT / Operations) at $\\alpha = 0.05$. (2) Build a 90% CI for the mean `Salary` among employees with `Role == 'Senior'`.",
    "**(1) Chi-square goodness-of-fit.** Under $H_0$ each department has equal probability $1/3$; expected counts $= n/3$. The test statistic $X^2 = \\sum (O_i - E_i)^2/E_i = 13.696$ with $\\text{df} = k-1 = 2$, giving p-value $= 0.001061 < 0.05$ → **reject $H_0$**: the three departments are not equally represented.\n\n**(2) CI for Senior mean Salary.** Subset to `Role == 'Senior'`; with $n_S$, $\\bar x_S$, $s_S$ from the sample, the 90% CI is $\\bar x_S \\pm t_{0.05, n_S-1}\\cdot s_S/\\sqrt{n_S} \\approx [1451.49,\\ 1696.90]$. Since the interval lies entirely above 0 and is centred near 1574, the mean Senior salary is precisely estimated and clearly positive.",
    "chisq.test(table(Employee$Department))\n## X-squared = 13.696, df = 2, p-value = 0.001061\nt.test(Employee$Salary[Employee$Role==\'Senior\'], conf.level=0.90)$conf.int\n## [1] 1451.49 1696.90"
), "images": []}

past_exams["exam_g2_2025_4a"] = {
"title": "G2-2025 Ex4 — Regression Department effect (IT vs Operations)",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Considering `modB` (`Productivity ~ Training_Attended + Satisfaction + Hours_Worked + Tenure + Remote_Work + Salary + Department`), estimate the average difference in productivity between employees working in **IT** and employees working in **Operations**, all other characteristics fixed. Is this difference significant?",
    "The reference level is **HR**, so `summary(modB)` reports $b_{\\text{IT}} = 2.632$ and $b_{\\text{Operations}} = 1.563$ as contrasts vs HR. Holding all else fixed:\n\n$$b_{\\text{IT}} - b_{\\text{Operations}} = 2.632 - 1.563 = +1.069.$$\n\nOn average IT employees are about **1.069 productivity units higher** than Operations employees. **Significance** of *this specific pair* **cannot be read directly** from `summary()` — the table only tests each level against the reference (HR). To test IT vs Operations: either **re-level** the factor so Operations becomes the reference (then read the new `DepartmentIT` row), or use `multcomp::glht` for the linear contrast.",
    "# Direct estimate from the fitted model\nb <- coef(modB)\nb['DepartmentIT'] - b['DepartmentOperations']   # 1.069\n\n# (a) Re-level so Operations is the reference, then refit\nEmployee$Department <- relevel(Employee$Department, ref='Operations')\nmodB2 <- lm(Productivity ~ Training_Attended + Satisfaction +\n            Hours_Worked + Tenure + Remote_Work + Salary + Department,\n            data=Employee)\nsummary(modB2)   # row 'DepartmentIT' tests IT - Operations = 0\n\n# (b) Equivalent test via a single linear contrast\nlibrary(multcomp)\nsummary(glht(modB, linfct=c('DepartmentIT - DepartmentOperations = 0')))"
), "images": ["statistics/images/exam_g2_2025_productivity_by_dept.png"]}

past_exams["exam_g2_2025_5a"] = {
"title": "G2-2025 Ex4-b3 — Normality of modB residuals",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Define the linear model assumption of **normality**. State whether such assumption is respected in `modB` and specify which tool you use to provide your answer.",
    "**Assumption.** The error terms $\\varepsilon_i$ of the model are normally distributed: $\\varepsilon_i \\sim N(0, \\sigma_\\varepsilon^2)$ for all $i$, iid.\n\n**Diagnostic tool.** Plot a **histogram of the standardized residuals** $\\hat\\varepsilon_i^{\\text{std}} = \\text{rstandard}(\\text{modB})$ (should be bell-shaped, centred at 0) — back it up with a Q-Q plot (points should lie on the 45° line).\n\n**Verdict for modB.** The histogram of `rstandard(modB)` is approximately **bell-shaped** and roughly centred at 0 (range ≈ $-3$ to $+3$) → the normality assumption is **reasonably respected**.",
    "hist(rstandard(modB))\nqqnorm(rstandard(modB)); qqline(rstandard(modB))"
), "images": ["statistics/images/exam_g2_2025_modB_resid_hist.png"]}

# =================== GENERAL 2 2026 ===================

past_exams["exam_g2_2026_1a"] = {
"title": "G2-2026 Ex1a — 90% CI for difference in cleaning-category proportions",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Compare the proportion of customers who chose the first (more expensive) product in the cleaning category (`category` == `cleaning`) between the **NorthWest** region ($n_1 = 278$, $\\hat p_1 = 0.64$) and the **NorthEast** region ($n_2 = 189$, $\\hat p_2 = 0.418$). Build a **90% confidence interval** for the difference $p_1 - p_2$ and interpret.",
    "Sample estimates: $\\hat p_1 - \\hat p_2 = 0.147$, $SE(\\hat p_1 - \\hat p_2) = 0.121$. A two-sided 90% CI uses $z_{0.95} = 1.645$:\n\n$$0.147 \\pm 1.645 \\cdot 0.121 = [-0.052,\\; 0.346].$$\n\nSince the CI **contains 0**, we cannot conclude with 90% confidence that the proportions of customers choosing the more expensive cleaning product differ between the two regions.",
    "n1 <- 278;  p1 <- 0.64\nn2 <- 189;  p2 <- 0.418\nSE <- 0.121         # given by the source\n0.147 + c(-1,1) * 1.645 * SE\n## [1] -0.052  0.346\nCI.diffprop(x, y, conf.level=0.90)"
), "images": ["statistics/images/exam_g2_2026_prices.png"]}

past_exams["exam_g2_2026_2a"] = {
"title": "G2-2026 Ex2a — Hypothesis system for campaign effectiveness ($\\mu = 850$)",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "The marketing department wants to evaluate the effectiveness of the promotional campaign. Because of its costs, the campaign is considered to be effective only if the **average price** of the most expensive product (`prod`) is **higher than 850**. Assume the population standard deviation of the price is **300**. State the **hypothesis system** clearly explaining your reasoning.",
    "**One-sided test on the population mean** (price of the product after the campaign):\n\n$$H_0:\\;\\mu_{\\text{Price}} = 850 \\quad (\\mu \\leq 850) \\qquad H_1:\\;\\mu_{\\text{Price}} > 850.$$\n\nThe campaign is declared effective **only if** the mean price exceeds 850 — the burden of proof goes on the alternative.",
    "# One-sample one-sided z-test (sigma known = 300)\nmu0    <- 850\nsigma  <- 300\n# H0: mu = 850   H1: mu > 850"
), "images": ["statistics/images/exam_g2_2026_prices.png"]}

# =================== JULY 2024 ===================

past_exams["exam_july_2024_1a"] = {
"title": "Jul-2024 Ex1.a — Colleges dataset: structure (n, p, variable types)",
"is_exam": True, "topic_hint": "G1",
"content": _q(
    "Describe the structure of the `Colleges` dataset: number of statistical units $n$, number of variables $p$, and the type of each variable (quantitative / qualitative, and for qualitative the number of categories).",
    "**Statistical units.** Rows = $n = 777$ US colleges (one row per college).\n\n**Variables.** $p = 18$ columns: `Private`, `Apps`, `Accept`, `Enroll`, `Top10perc`, `Top25perc`, `F.Undergrad`, `P.Undergrad`, `Outstate`, `Room.Board`, `Books`, `Personal`, `PhD`, `Terminal`, `S.F.Ratio`, `perc.alumni`, `Expend`, `Grad.Rate`.\n\n**Types.**\n\n| Variable | Type | Notes |\n|---|---|---|\n| `Private` | **Qualitative — binary** | 2 categories: `Yes` / `No` (factor) |\n| All other 17 | **Quantitative** | Counts (`Apps`, `Accept`, `Enroll`, `F.Undergrad`, `P.Undergrad`) are discrete; the rest (`Top10perc`, `Outstate`, `Room.Board`, `PhD`, `Grad.Rate`, …) are continuous / percentages / monetary amounts |\n\nSo the dataset has **one qualitative variable** (`Private`, dichotomous) and **17 quantitative variables**, observed on **$n=777$** colleges.",
    "str(Colleges)\n## \'data.frame\': 777 obs. of 18 variables:\n##  $ Private    : Factor w/ 2 levels \'No\',\'Yes\'\n##  $ Apps       : num  ...\n##  $ Accept     : num  ...\n##  $ Top10perc  : num  ...\n##  ...\n##  $ Grad.Rate  : num  ...\nsummary(Colleges)\ndim(Colleges)        # 777 18\nnrow(Colleges)       # n = 777\nncol(Colleges)       # p = 18\nsapply(Colleges, class)\nlevels(Colleges$Private)   # \'No\' \'Yes\'  -> binary qualitative"
), "images": []}

past_exams["exam_july_2024_2a"] = {
"title": "Jul-2024 Ex2a — Linear association between Top10 and Phd (correlation)",
"is_exam": True, "topic_hint": "G9",
"content": _q(
    "Ex2 indicates the percentage of enrolled students from the top 10% of high-school classes (`Top10`), and the variable `Phd` as a faculty quality indicator. **Assess how strong is the linear association** between the variables `Top10` and `Phd`, computing the linear correlation coefficient and specifying which are the criteria used to answer your considerations.",
    "**Setup.** Both variables are quantitative ($n = 408$ colleges). The natural single-number summary of *linear* association is **Pearson's correlation** $r \\in [-1, 1]$:\n$$r \\;=\\; \\frac{\\sum_i (x_i - \\bar x)(y_i - \\bar y)}{\\sqrt{\\sum_i(x_i-\\bar x)^2}\\,\\sqrt{\\sum_i(y_i-\\bar y)^2}} \\;=\\; \\frac{\\operatorname{Cov}(X,Y)}{s_X\\,s_Y}.$$\n\n**Sample value.** From the `.Rdata`:\n$$r = \\mathrm{cor}(\\texttt{Top10},\\,\\texttt{Phd}) \\approx 0.5657.$$\n\n**Interpretation criteria.**\n\n| $\\lvert r\\rvert$ | strength | here |\n|---|---|---|\n| $0$ | none | |\n| $(0, 0.3]$ | weak | |\n| $(0.3, 0.7]$ | **moderate** | **$0.566$** |\n| $(0.7, 1)$ | strong | |\n| $1$ | perfect | |\n\n**Reading.** $r \\approx +0.57$ is **positive** (colleges enrolling more top-10% high-school students tend to have a higher `Phd` faculty-quality index) and **moderate** — there is a real linear dependence, but it is *far from $1$*, so a sizeable share of the variation in `Phd` is *not* explained by `Top10` alone. Equivalently, $r^2 \\approx 0.32$: about 32% of the variability of one variable is linearly accounted for by the other.\n\n**Caveats — why $r$ alone is not enough.** Pearson's $r$ only captures *linear* association; it is sensitive to outliers and misleading for curvilinear relationships. Always pair the number with a scatterplot — here the cloud is roughly linear with no extreme leverage points, so $r$ is a reliable summary of the (moderate, positive) linear association.",
    "# linear correlation Pearson\ncor(Colleges$Top10, Colleges$Phd, use='complete')\n## [1] 0.5657305\n\n# visual backup -- scatter with OLS fit\ndistr.plot.xy(Top10, Phd, plot.type='scatter', fitline=TRUE, data=Colleges)\n\n# coefficient of determination r^2 (share of variance explained linearly)\ncor(Colleges$Top10, Colleges$Phd, use='complete')^2\n## [1] 0.3200512"
), "images": ["statistics/images/exam_july_2024_top10_phd.png"]}

past_exams["exam_july_2024_3a"] = {
"title": "Jul-2024 Ex3 - Multiple regression of Enrol on College predictors",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "From the fitted model `m <- lm(Enrol ~ Private + Apps + Outstate + Region + Room.Board, data=College)`, (a) interpret the slope on `Apps`, (b) assess whether `Outstate` is statistically significant, and (c) predict `Enrol` for a private college in the North-East with `Apps=2000`, `Outstate=10000`, `Room.Board=4500`.",
    "**Model.** Response `Enrol` regressed on one binary predictor (`Private`: Yes/No), three continuous covariates (`Apps`, `Outstate`, `Room.Board`) and one 4-level categorical factor (`Region`, baseline `N` -> dummies `RegionS`, `RegionMW`, `RegionW`).\n\n**(a) Apps slope.** $\\hat\\beta_{\\text{Apps}} \\approx 0.1576$ with SE $\\approx 0.0031$, $t = 0.1576/0.0031 \\approx 51.06$, $p < 2\\!\\times\\!10^{-16}$ (***). **Holding `Private`, `Outstate`, `Region` and `Room.Board` constant, one additional application is associated with $\\approx 0.158$ extra enrolled students on average** - roughly **1 extra enrolment per 6.3 additional applications**. Highly significant.\n\n**(b) Significance of `Outstate`.** From `summary(m)` the `Outstate` row gives $\\hat\\beta_{\\text{Outstate}} \\approx -0.0205$, SE $\\approx 0.0036$, $t \\approx -5.7$, $p \\approx 1.4\\!\\times\\!10^{-8}$ (***). Since $p < 0.05$ (in fact $< 0.001$) **`Outstate` is highly significant**: controlling for the other regressors, colleges with higher out-of-state tuition enrol slightly *fewer* students (about -0.02 students per extra USD of tuition, i.e. about 20 fewer enrolments per \\$1000).\n\n**(c) Prediction.** With $\\hat\\beta_0 \\approx 78$, $\\hat\\beta_{\\text{PrivateYes}} \\approx -150$, $\\hat\\beta_{\\text{Apps}} = 0.1576$, $\\hat\\beta_{\\text{Outstate}} = -0.0205$, $\\hat\\beta_{\\text{Room.Board}} \\approx 0.087$, RegionN baseline (so all `Region*` dummies = 0):\n$$\\widehat{\\text{Enrol}} = 78 - 150 + 0.1576\\cdot 2000 - 0.0205\\cdot 10000 + 0.087\\cdot 4500 \\approx 78 - 150 + 315.2 - 205 + 391.5 \\approx 430$$ enrolled students. Use `predict(m, newdata=..., interval='prediction')` for the proper individual-prediction band.\n\n**Overall fit.** Multiple R^2 $\\approx 0.83$, Adjusted R^2 $\\approx 0.83$ -> the regressors explain ~83% of the variance in `Enrol`. F-statistic is large with $p < 2\\!\\times\\!10^{-16}$, so the model as a whole is highly significant.",
    "m <- lm(Enrol ~ Private + Apps + Outstate + Region + Room.Board, data=College)\nsummary(m)\n## Coefficients:\n##              Estimate Std. Error t value Pr(>|t|)\n## (Intercept)  78.xxxxx  ...         ...    ...\n## PrivateYes  -150.xxxx  ...         ...    ...\n## Apps          0.15760  0.00309    51.06   <2e-16 ***\n## Outstate     -0.02050  0.00360    -5.70   1.4e-08 ***\n## RegionS, RegionMW, RegionW  ...\n## Room.Board    0.08700  ...         ...    ...\n## Multiple R-squared: ~0.83,  Adjusted R-squared: ~0.83\n## F-statistic: large, p-value < 2.2e-16\nconfint(m)['Apps',]\nconfint(m)['Outstate',]\n# (c) prediction for a Private, North-East college\nnewx <- data.frame(Private='Yes', Apps=2000, Outstate=10000,\n                   Region='N', Room.Board=4500)\npredict(m, newdata=newx, interval='prediction')\n## fit ~ 430 students"
), "images": []}

# =================== JULY 2025 ===================

past_exams["exam_july_2025_1a"] = {
"title": "Jul-2025 Ex1 — Two-sample one-sided t-test on Savings: Branch A vs Branch B (equal variances)",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Based on the available data, are we interested in verifying whether the **average amount of savings (`Savings`) in the population of clients of branch A is lower than that of the population of clients of branch B** (variable `Branch`, with categories `A` and `B`), assuming **equal variances** in the two subpopulations?\n\n**1.a State** the null and alternative hypotheses for the test.\n**1.b Report** the analytical expression of the standard error of the test statistic and its value in the sample, and **provide its interpretation**.\n**1.c Report** the expression and value of the test statistic, and **decide** at $\\alpha = 5\\%$ and $\\alpha = 1\\%$.",
    "**1.a Hypotheses (one-sided, lower-tail).** Let $\\mu_A, \\mu_B$ be the population mean Savings in branches A and B. The research claim that A\'s mean is *lower* than B\'s is the alternative:\n\n$$H_0:\\ \\mu_A = \\mu_B \\quad\\text{vs}\\quad H_1:\\ \\mu_A < \\mu_B.$$\n\nEquivalently, with $D = \\mu_A - \\mu_B$: $H_0: D = 0$ vs $H_1: D < 0$.\n\n**1.b Standard error (pooled, equal variances assumed).** Under $\\sigma_A^2 = \\sigma_B^2 = \\sigma^2$ the pooled variance combines both samples:\n$$s_p^2 = \\frac{(n_A - 1)\\,s_A^2 + (n_B - 1)\\,s_B^2}{n_A + n_B - 2}, \\qquad SE(\\bar y_A - \\bar y_B) = s_p\\sqrt{\\tfrac{1}{n_A} + \\tfrac{1}{n_B}}.$$\nFrom the `TEST.diffmean` output: $SE \\approx 58.45$ (in € of Savings). **Interpretation:** it is an estimate of the expected distance (or expected absolute deviation) of a **generic estimate (difference)** of the sample averages of Savings from the parameter of interest — here, the difference between the *two* population means. Thus the expected error of a generic estimate is **about €58.45 in Savings**. The actual estimation error on the specific sample at hand could be larger or smaller, but no sharper statement can be made for one specific observed sample.\n\n**1.c Test statistic and decision.** Under $H_0$, $t = \\dfrac{\\bar y_A - \\bar y_B}{SE} \\sim t_{n_A+n_B-2}$.\n\nFrom the output (`TEST.diffmean(Savings, by=Branch, type=\'independent\', alternative=\'less\', var.test=TRUE, data=BankClients)`):\n$\\bar y_A - \\bar y_B \\approx -162.835$, $SE \\approx 58.45$, hence\n$$t = \\frac{-162.835}{58.45} \\approx -2.786, \\qquad p\\text{-value (one-sided, lower)} \\approx 0.0027.$$\n\n**Decision.** $p \\approx 0.0027 < 0.01 < 0.05$ → **reject $H_0$ at both $\\alpha = 5\\%$ and $\\alpha = 1\\%$**. There is strong evidence that the average savings of Branch A\'s clients are *lower* than those of Branch B.\n\n**Note on `var.test=TRUE`.** The companion variance-equality test (F / Levene) returns $p > \\alpha$, so the equal-variance assumption is not rejected and the **pooled** t-test (rather than Welch\'s) is the appropriate one here.",
    "# Two-sample, one-sided (lower-tail), pooled-variance t-test on Savings by Branch\nTEST.diffmean(Savings, by=Branch, type=\'independent\', alternative=\'less\', var.test=TRUE, data=BankClients)\n## Two Sample t-test (pooled)\n##   t = -2.786,  df = n_A + n_B - 2,  p-value = 0.0027\n##   alternative hypothesis: true difference in means (A - B) is less than 0\n##   SE(diff) = 58.45,  mean_A - mean_B = -162.835\n\n# Equivalent base-R call\nt.test(Savings ~ Branch, data=BankClients, alternative=\'less\', var.equal=TRUE)\n\n# Variance-equality check used to justify var.equal=TRUE\nvar.test(Savings ~ Branch, data=BankClients)\n## F test: p > 0.05 -> do not reject H0 of equal variances -> use pooled t-test"
), "images": ["statistics/images/exam_july_2025_savings.png"]}

# =================== SEPTEMBER 2024 ===================

past_exams["exam_sep_2024_1a"] = {
"title": "Sep-2024 Ex1a — 5th percentile of Total_Income (normal approx, μ≈27000, σ≈7145)",
"is_exam": True, "topic_hint": "G10",
"content": _q(
    "From the `distr.summary.x(~Total_Income, data=Credit)` output below, find the value of `Total_Income` below which 5% of customers fall, assuming a normal approximation.\n\n```\n> distr.summary.x(~Total_Income, data=Credit)\nSummary measures for Total_Income | Eligible\n  n       min      max      mean     median     sd        skewness\n  8000    1006.0   55997.5  27003.5  25004.5    7144.97   0.62\n```",
    "**Estimates from the table:** $\\hat\\mu = 27003.5$, $\\hat\\sigma = 7144.97$. Assume $X \\sim N(\\hat\\mu, \\hat\\sigma^2)$.\n\n5th percentile: $x_{0.05} = \\hat\\mu + z_{0.05}\\cdot\\hat\\sigma = 27003.5 - 1.645\\cdot 7144.97 \\approx €15\\,250$.\n\nUsing the rounded $\\mu=27000$, $\\sigma=7000$: $27000 - 1.645\\cdot 7000 \\approx €15\\,485$.\n\n**Caveat — skewness:** sample skewness $\\approx 0.62$ (positive, non-trivial) and `mean > median` ($27003.5 > 25004.5$) → distribution is right-skewed. The normal approximation under-estimates the true left-tail percentile; treat the result as indicative only.",
    "# Reproduce the summary used above\ndistr.summary.x(~Total_Income, data=Credit)\n\n# 5th percentile via normal approximation (table values)\nmu <- 27003.5; sigma <- 7144.97\nqnorm(0.05, mean=mu, sd=sigma)\n## [1] 15249.4\n\n# Rounded version (mu=27000, sigma=7000)\nqnorm(0.05, mean=27000, sd=7000)\n## [1] 15486.21\n\n# Empirical 5th percentile (no normality assumption) for comparison\nquantile(Credit$Total_Income, probs=0.05, na.rm=TRUE)"
), "images": ["statistics/images/exam_sep_2024_income.png"]}

past_exams["exam_sep_2024_2a"] = {
"title": "Sep-2024 Ex2a — Histogram of Score (unequal-width classes, specific branch)",
"is_exam": True, "topic_hint": "G1",
"content": _q(
    "Provide a sketch of the histogram obtained from the data of the particular branch (`Score Classes`: [0,200) 30%, [200,300) 20%, [300,600) 30%, [600,1000) 20%). Compare with the histogram of the main branches.",
    "Classes have **unequal widths**, so the y-axis must show **density = relative frequency / class width**, not raw frequency.\n\n| Class | % freq | Width | Density |\n|---|---|---|---|\n| [0, 200) | 0.30 | 200 | **0.0015** |\n| [200, 300) | 0.20 | 100 | **0.002** |\n| [300, 600) | 0.30 | 300 | **0.001** |\n| [600, 1000) | 0.20 | 400 | **0.0005** |\n\n**Modal class — specific branch:** [200, 300) (density 0.002, the tallest bar). **Modal class — main branches:** [300, 600). Hence the **specific branch is shifted leftward** toward lower scores: its mode sits in 200–300 rather than 300–600, and densities decay monotonically for Score ≥ 300. The shape is unimodal with a left-skewed mass and a long right tail.",
    "distr.table.x(Score, interval=T, freq=c('counts','dens'), data=Credit)\ndistr.plot.x(Score, plot.type='hist', breaks=c(0,200,300,600,1000), data=Credit)\n## y-axis = Density (because class widths differ)\n## modal class (specific branch) = [200,300)\n## modal class (main branches)   = [300,600)  -> leftward shift"
), "images": ["statistics/images/exam_sep_2024_2a_hist.png"]}

past_exams["exam_sep_2024_3a"] = {
"title": "Sep-2024 Ex3 — Interpret Account_length coefficient in regression",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Interpret $\\hat\\beta_2 = 7.84$ for `Account_length` (second slope in the multiple-regression output).",
    "Holding all other variables constant, **a one-year increase in account length is associated with a 7.84-unit increase in Score** on average.",
    "summary(mod)\nconfint(mod)['Account_length',]"
), "images": []}

past_exams["exam_sep_2024_3d"] = {
"title": "Sep-2024 Ex3d — Homoscedasticity check from residuals",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Explain what the assumption of homoscedasticity for a linear regression model consists of. Assess whether this assumption is reasonably respected for the estimated model `m`, justifying your answer.",
    "Homoscedasticity means the error variance $\\Var(\\varepsilon_i)$ is constant across all levels of the predictors. Diagnose visually with `plot(m, which=1)` (residuals vs fitted) — a roughly uniform band around 0 supports the assumption; a funnel/cone signals a violation. For model `m`, the residual cloud spreads across fitted values ~200–800 with a few labelled outliers (**#15**, **#362**, **#359**) but no systematic fanning. The residuals appear to have constant dispersion → homoscedasticity reasonable.",
    "plot(m, which=1)\nplot(m, which=3)\nlibrary(lmtest); bptest(m)\n## Breusch-Pagan: if p > alpha => fail to reject homoscedasticity"
), "images": ["statistics/images/exam_sep_2024_resid_fitted.png"]}

# =================== SEPTEMBER 2025 ===================

past_exams["exam_sep_2025_1a"] = {
"title": "Sep-2025 Ex1.a — Scatter VO2.max vs Performance",
"is_exam": True, "topic_hint": "G9",
"content": _q(
    "Use a scatterplot to investigate the association between Performance and VO2.max. Comment on the correlation coefficient and whether it reliably measures the strength of the association.",
    "Scatter shows a moderately strong positive linear trend. Sample correlation $r \\approx 0.593$ (slide value; the .Rdata file gives $\\approx 0.576$). Because the cloud is roughly linear with no severe outliers, $r$ is a reliable summary of the strength of the linear association between VO2.max and Performance.",
    "distr.plot.xy(VO2.max, Performance, plot.type='scatter', fitline=T, data=Performance)\ncor(Performance$VO2.max, Performance$Performance)\n## [1] 0.593"
), "images": ["statistics/images/exam_sep_2025_vo2max_performance.png"]}

past_exams["exam_sep_2025_2a"] = {
"title": "Sep-2025 Ex2a — One-sided test on mean difference D (new vs old algorithm)",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Formulate $H_0$ and $H_1$ for the **mean difference in performance** $D = \\mu_{\\text{new}} - \\mu_{\\text{old}}$ between the new algorithm and the old one, give the test statistic and its estimated standard error, then evaluate the numerical value and conclude. Sample: $\\bar D = 0.510$, $SE(\\bar D) = 0.221$, $n$ large.",
    "**Hypotheses (one-sided, upper-tail).** Let $D = \\mu_{\\text{new}} - \\mu_{\\text{old}}$. The claim that the new algorithm performs *better* is the alternative:\n\n$H_0: \\mu_D = 0$  vs  $H_1: \\mu_D > 0$.\n\n**Test statistic.** Working on the single derived variable $D$ (one-sample / paired-style test on the mean difference) with sample mean $\\bar D$ and estimated standard error $SE(\\bar D) = s_D/\\sqrt{n}$,\n$$z = \\frac{\\bar D - 0}{SE(\\bar D)} = \\frac{\\bar D}{s_D/\\sqrt n}.$$\n**Assumptions.** (i) the $n$ observed differences $D_i$ are an i.i.d. sample; (ii) $n$ is large enough that the CLT applies, so under $H_0$ the standardised statistic is approximately $\\mathcal N(0,1)$ — this justifies using a $z$ (Normal) reference distribution rather than a Student-$t$, and avoids any normality assumption on the $D_i$ themselves.\n\n**Numerical value.** With $\\bar D = 0.510$ and $SE(\\bar D) = 0.221$:\n$$z = \\frac{0.510}{0.221} \\approx 2.308.$$\n**P-value (one-sided).** $p = 1 - \\Phi(2.308) \\approx 0.0105$.\n\n**Conclusion.** $p \\approx 0.0105 < 0.05$ (and $< 0.025$), so we **reject $H_0$** at the 5% (and even 2.5%) level: there is significant evidence that the new algorithm has *higher* mean performance than the old one. At the conservative 1% level the test would not reject.",
    "# one-sided test on mean difference D (new vs old)\nD  <- 0.510                 # sample mean difference\nSE <- 0.221                 # estimated SE of D-bar\nz  <- D / SE                # standardised statistic\nz\n## [1] 2.30769\n# upper-tail p-value (Normal approximation, large n)\np <- 1 - pnorm(z)\np\n## [1] 0.01051\n# (book also reports the two-sided variant: 2*(1 - pnorm(z)) approx 0.021)\n# from raw data (paired / derived):\n# d  <- Performance.new - Performance.old\n# t.test(d, mu=0, alternative='greater')"
), "images": []}

past_exams["exam_sep_2025_5a"] = {
"title": "Sep-2025 Ex5a — Assumptions for the CI on $\\mu_A - \\mu_B$",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Clarify whether specific assumptions are required to construct a confidence interval for the difference between the two means $\\mu_A - \\mu_B$ (Activity.type A vs B). Clearly motivate your answer.",
    "To build a CI for the difference between two means with **independent samples** we need: **(i) independence** — A and B participants are randomly assigned to different training programs, so the two samples are independent. **(ii) Normality of the response within each group** — we can **relax** this assumption because $n_A = 58$ and $n_B = 380$ are large enough for the **CLT** to apply, so $\\bar X_A - \\bar X_B$ is approximately Normal even if `Performance` is not. **(iii) Equality of population variances** — required to decide between the **pooled-variance** estimator and the **Welch** (separate-variances) estimator; this is verified empirically in 5.b via a **Levene\'s test**. The R call `CI.diffmean(..., var.test=TRUE, conf.level=0.90)` produces both CIs (equal- and unequal-variance, each via Normal approx and Student-t) **and** the Levene test in one shot.",
    "CI.diffmean(Performance$Performance[Performance$Activity.type==\'A\'],\n            Performance$Performance[Performance$Activity.type==\'B\'],\n            type=\'independent\', var.test=TRUE, conf.level=0.90)\n## Confidence interval for mu_x - mu_y\n## Samples: independent ; Confidence level: 0.9 ; Variances: unknown\n## Unknown variances assumed to be equal\n##                 n_x n_y xbar  ybar  xbar-ybar  s_X   s_Y   se    Lower  Upper\n## Normal.Approx   58  380 78.17 82.74 -4.57      6.66  6.53  0.92  -6.09  -3.05\n## Student-t       58  380 78.17 82.74 -4.57      6.66  6.53  0.92  -6.09  -3.05\n## Unknown variances assumed to be different\n## Normal.Approx   58  380 78.17 82.74 -4.57      6.66  6.53  0.94  -6.11  -3.03\n## Student-t       58  380 78.17 82.74 -4.57      6.66  6.53  0.94  -6.13  -3.01\n## Levene test for homogeneity of variance\n## s2_x  s2_y  F-stat  df1 df2  p-value\n## 44.42 42.66  0.41   1   436  0.52397"
), "images": []}

past_exams["exam_sep_2025_5b"] = {
"title": "Sep-2025 Ex5b — Levene\'s test: equal vs different variances",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Should we assume the population variances of `Performance` in the two groups (A and B) are **equal** or **different**? Justify clearly your answer.",
    "We verify which assumption is supported by the data with a **Levene\'s test** on the equality of variances ($H_0: \\sigma_A^2 = \\sigma_B^2$ vs $H_1: \\sigma_A^2 \\ne \\sigma_B^2$). From the output embedded in `CI.diffmean(..., var.test=TRUE)`: $F_{\\text{obs}} = 0.41$, p-value $= 0.524$ (with $s_A^2 = 44.42$, $s_B^2 = 42.66$, df$_1 = 1$, df$_2 = 436$). Since the p-value is **larger than all common significance levels** (0.01, 0.05, 0.10), we **fail to reject $H_0$** and proceed with the **equality-of-variances** assumption → use the **pooled-variance** CI/test (top block of the R output), not Welch\'s.",
    "# Levene is reported inside CI.diffmean when var.test=TRUE\nCI.diffmean(Performance$Performance[Performance$Activity.type==\'A\'],\n            Performance$Performance[Performance$Activity.type==\'B\'],\n            type=\'independent\', var.test=TRUE, conf.level=0.90)\n## Levene test for homogeneity of variance\n## Null hypothesis        H0: s2_x = s2_y\n## Alternative hypothesis H1: s2_x != s2_y\n## s2_x  s2_y  F-stat  df1 df2  p-value\n## 44.42 42.66  0.41   1   436  0.52397\n# Equivalent stand-alone call\nlibrary(car)\nleveneTest(Performance ~ Activity.type,\n           data=subset(Performance, Activity.type %in% c(\'A\',\'B\')))\n# p > 0.10 -> keep var.equal=TRUE (pooled)"
), "images": []}

past_exams["exam_sep_2025_5c"] = {
"title": "Sep-2025 Ex5c — Analytical 90% pooled-variance CI for $\\mu_A - \\mu_B$",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Provide the **analytical expression** of the confidence interval under the proper assumption on the population variances (decided in 5.b), and report the **90% confidence interval** for the difference in the mean `Performance` between participants trained with programs A and B.",
    "Since 5.b confirmed equal variances (Levene p $= 0.524$, fail to reject), use the **pooled-variance Student-t** CI. With $n_A + n_B - 2 = 436$ df the Student-t and Normal quantiles are practically identical (interval $[-6.09, -3.05]$ in both cases). **Analytical expression:** $$\\bar x - \\bar y \\;\\pm\\; t_{n_x+n_y-2,\\,0.95}\\sqrt{\\dfrac{s^2_{\\text{pooled}}}{n_x} + \\dfrac{s^2_{\\text{pooled}}}{n_y}}, \\qquad s^2_{\\text{pooled}} = \\dfrac{s_x^2(n_x-1) + s_y^2(n_y-1)}{n_x + n_y - 2}.$$ Plugging in $\\bar x - \\bar y = -4.57$, $s_A = 6.66$, $s_B = 6.53$, $n_A = 58$, $n_B = 380$ → $SE = 0.92$ → CI $= -4.57 \\pm 1.648\\cdot 0.92 = [-6.09, -3.05]$. **Interpretation:** with 90% confidence the average performance of individuals running activity A is **lower** than the average performance of individuals running activity B; the difference lies between $-6.09$ and $-3.05$.",
    "# pooled-variance 90% CI for mu_A - mu_B (read directly from CI.diffmean output)\nCI.diffmean(Performance$Performance[Performance$Activity.type==\'A\'],\n            Performance$Performance[Performance$Activity.type==\'B\'],\n            type=\'independent\', var.test=TRUE, conf.level=0.90)\n## Student-t   ...  xbar-ybar  se    Lower  Upper\n##             ...  -4.57      0.92  -6.09  -3.05\n# manual reconstruction\nnA <- 58;  sA <- 6.66\nnB <- 380; sB <- 6.53\nsp2 <- ((nA-1)*sA^2 + (nB-1)*sB^2) / (nA+nB-2)\ntc  <- qt(0.95, df=nA+nB-2)\n-4.57 + c(-1,1) * tc * sqrt(sp2*(1/nA + 1/nB))\n## [1] -6.09 -3.05"
), "images": []}
# =====================================================================
# Gap-fill additions (2026-06-06) — sub-parts that were not in the
# original 13-agent transcription pass. Marked yellow as exam cells.
# =====================================================================

# ---- 1st partial 2026 (Q2-Q6) ----
past_exams["exam_p1_2026_2"] = {
"title": "P1-2026 Ex2 — Relationship between Bid and PaidFare (scatter, Pearson r)",
"is_exam": True, "topic_hint": "G8",
"content": _q(
    "Describe the relationship between `Bid` and `PaidFare` from the scatterplot. Compute the Pearson correlation and comment on whether it is an appropriate summary.",
    "The scatterplot shows a **clear inverse, non-linear** relationship: as `PaidFare` increases, `Bid` decreases steeply at first and then flattens out (hyperbolic / power-decay shape). Pearson correlation $r = -0.7947$ — strongly negative. **Caveat**: Pearson $r$ only measures *linear* association, so it understates the true (curved) dependence between the two variables. A monotone-rank measure (**Spearman $\\rho$**) or a transformation (e.g. $\\log$ or $1/x$) would describe the link better; equivalently, one could fit a non-linear model rather than report $r$ alone.",
    "distr.plot.xy(x=PaidFare, y=Bid, plot.type='scatter', fitline=T, data=Bidding)\ncor(Bidding$PaidFare, Bidding$Bid)\n## [1] -0.7947379\n# rank-based alternative (handles monotone non-linearity)\ncor(Bidding$PaidFare, Bidding$Bid, method='spearman')"
), "images": ["statistics/images/exam_p1_2026_paidfare_vs_bid.png"]}

past_exams["exam_p1_2026_4"] = {
"title": "P1-2026 Ex4 — 10th and 90th percentiles of PaidFare",
"is_exam": True, "topic_hint": "G6",
"content": _q(
    "Compute and interpret the 10th and 90th percentiles ($p_{10}$, $p_{90}$) of `PaidFare`.",
    "$p_{10} = 39.46$, $p_{90} = 85.61$. **Interpretation:** 10% of bookings paid less than 39.46, 10% paid more than 85.61, and the central 80% lie between 39.46 and 85.61. The gap $p_{90} - p_{10} = 46.15$ is the *interdecile range* — a robust spread measure (insensitive to extreme outliers, unlike the full range).",
    "distr.summary.x(x=PaidFare, data=Bidding, stats=c('p10','p90'))\n# manual: quantile(Bidding$PaidFare, probs=c(0.10, 0.90))"
), "images": []}

past_exams["exam_p1_2026_5"] = {
"title": "P1-2026 Ex5 — LeadTime x Channel: share of MediumTerm customers (joint vs row %)",
"is_exam": True, "topic_hint": "G7",
"content": _q(
    "`LeadTime` (Early / **MediumTerm** = average advance / LastMinute) crossed with `Channel` (Aggregator / Agency / Airline). Report the **percentage of customers who bought their ticket with an average advance** (MediumTerm) **among clients who used a comparison platform** (`Channel = Aggregator`) and **among clients who bought via an Agency** (`Channel = Agency`). Recommend a chart for the two `Channel` groups. **Do NOT reorder the levels of LeadTime.**",
    "**Trap.** *Row* percentages condition on `Channel` (denominator = clients of that channel): 51% Aggregator, 25% Agency, 48% Airline. These answer the *wrong* question ('how is each channel\'s customer base split across LeadTime?'). The exam asks the share of *all* customers falling into the MediumTerm-and-channel cell, so use **joint percentages** (denominator = total $n = 668$).\n\n**Joint percentages** $f_{ij} = n_{ij}/n$: Aggregator+MediumTerm $= 72/668 = 10.78\\%$, Agency+MediumTerm $= 76/668 = 11.38\\%$, Airline+MediumTerm $= 107/668 = 16.02\\%$.\n\n**Answer.** Among comparison-platform buyers ~**11%** of all customers bought MediumTerm; among Agency buyers also ~**11%**. The two groups have very similar joint shares of MediumTerm. **Recommended chart:** side-by-side bar chart of joint percentages by `Channel` x `LeadTime` (see plot — left panel shows the misleading row-% reading; right panel the correct joint-% reading).",
    "Bidding$LeadTime.F <- factor(Bidding$LeadTime, levels=c('Early','MediumTerm','LastMinute'))\n# joint percentages -- answer the question\ndistr.table.xy(x=Channel, y=LeadTime.F, data=Bidding, freq='perc')\n## Aggregator MediumTerm 10.78%   Agency MediumTerm 11.38%   Airline MediumTerm 16.02%\n# (wrong, for contrast) row percentages P(LeadTime | Channel) -- freq.type='y|x'\ndistr.table.xy(x=Channel, y=LeadTime.F, data=Bidding, freq='perc', freq.type='y|x')\n## Aggregator 51%   Agency 25%   Airline 48%  -- DIFFERENT question\n# recommended visual\ndistr.plot.xy(x=Channel, y=LeadTime.F, data=Bidding, type='barplot', freq='perc')"
), "images": ["statistics/images/exam_p1_2026_5_leadtime_channel.png"]}

past_exams["exam_p1_2026_6a"] = {
"title": "P1-2026 Ex6a — Sample means and SE for Aggregator/Airline",
"is_exam": True, "topic_hint": "G11",
"content": _q(
    "From the channel summary table compute $\\bar x$ and $SE(\\bar x)$ for Aggregator and Airline.",
    "Aggregator: $\\bar x = 56.22$, $s = 12.13$, $n = 142$ → $SE = 12.13/\\sqrt{142} = 1.0179$. Airline: $\\bar x = 53.50$, $s = 22.06$, $n = 224$ → $SE = 22.06/\\sqrt{224} = 1.4739$.",
    "distr.summary.x(x=PaidFare, by=Channel, data=Bidding)\n12.13/sqrt(142)   # Aggregator SE\n22.06/sqrt(224)   # Airline SE"
), "images": []}

past_exams["exam_p1_2026_6b"] = {
"title": "P1-2026 Ex6b — Reliability of PaidFare estimate vs SE: cannot conclude",
"is_exam": True, "topic_hint": "G11",
"content": _q(
    "Can we conclude one specific PaidFare estimate is more reliable from a smaller SE?",
    "**No conclusions can be drawn.** Although $SE(\\bar x_{Aggregator}) < SE(\\bar x_{Airline})$, SE refers to the sampling distribution of the *estimator* — the deviation of a *generic* estimate from the parameter — NOT to the deviation of a *specific* realised PaidFare estimate. We can say the Aggregator estimator is more reliable (its estimates are *on average* more tightly clustered around the population mean), but we cannot draw conclusions about the reliability of specific realised PaidFare estimates or their distance from the corresponding parameter.",
    "# SE compares estimators (long-run), not single PaidFare estimates\nSE_agg <- 12.13/sqrt(142)   # 1.018\nSE_air <- 22.06/sqrt(224)   # 1.474\n# A single Airline PaidFare x-bar could still land closer to mu than a single Aggregator one\nc(SE_agg, SE_air)"
), "images": []}

# ---- general 1 2025: Q6 (larger regression with Steps) ----
past_exams["exam_g1_2025_6"] = {
"title": "G1-2025 Ex6 — Larger regression SleepQuality ~ Stress+Age+BMI+Physical+Steps",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Estimate the larger model `lm(SleepQuality ~ Stress + Age + BMI + Physical + Steps)` and explain why the Adjusted $R^2$ is preferable to $R^2$ for comparing models with different numbers of regressors.",
    "From `summary(mod)`: Intercept $\\approx -3.6847$, Stress $\\approx -0.014$, Age $\\approx 0.0011$, BMI Normal $\\approx 0.066$, BMI Underweight $\\approx 0.241$, Physical $\\approx 0.0049$, Steps $\\approx 1.529\\times 10^{-4}$. Adj $R^2 = 0.6592$ on the larger model ($F \\approx 84.4$). The unadjusted $R^2$ never decreases when adding regressors, so it cannot fairly compare nested models; the **Adjusted $R^2$** penalises additional regressors and is the right metric — here it rises from $0.5468$ (smaller model, without Steps) to $0.6592$ (with Steps), confirming the larger model is preferable.",
    "mod <- lm(SleepQuality ~ Stress + Age + BMI + Physical + Steps, data=Sleep)\nsummary(mod); confint(mod)\nplot(mod, which=1); plot(mod, which=3)"
), "images": []}

# ---- general 1 2026: Q2, Q3, Q5 ----
past_exams["exam_g1_2026_2a"] = {
"title": "G1-2026 Ex2a — Test independence: PurposeLoan ⊥ EmplStatus (chi-squared)",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "We are interested in whether the reason for requesting a loan (`PurposeLoan`) and the employment status (`EmplStatus`) are associated, using an appropriate test. State $H_0$ and $H_1$ and motivate rigorously.\n\n**Hypotheses.** $H_0$: `PurposeLoan` and `EmplStatus` are **independent**. $H_1$: `PurposeLoan` and `EmplStatus` are **not independent** (associated).",
    "Chi-squared test of independence on the two-way table of `PurposeLoan` × `EmplStatus`. Test statistic $X^2 = \\sum (O_{ij} - E_{ij})^2 / E_{ij} \\sim \\chi^2_{(r-1)(c-1)}$ under $H_0$. Reject $H_0$ if p-value $< \\alpha$. Visually: stacked bar of PurposeLoan conditional on EmplStatus — if the conditional distributions look identical across EmplStatus levels → independent.",
    "chisq.test(Credit$PurposeLoan, Credit$EmplStatus)\ndistr.table.xy(PurposeLoan, EmplStatus, freq='perc', freq.type='y|x', data=Credit)"
), "images": []}

past_exams["exam_g1_2026_2b"] = {
"title": "G1-2026 Ex2b — χ² independence test PurposeLoan × EmplStatus (stat = 11.107)",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Test independence between `PurposeLoan` and `EmplStatus` given the χ²-statistic = **11.107** with **df = 8**. Compute the p-value, state the decision and motivate.",
    "Expression of the p-value: $p\\text{-value} = P(\\chi^2_8 > 11.107)$. **Interpretation**: probability of observing a test statistic as extreme as (or more extreme than) the one observed under $H_0$ (independence). Decision: p-value $\\approx 0.196$ → for any common $\\alpha$ (1%, 5%, 10%) we have p > $\\alpha$ → **do not reject** $H_0$: data are consistent with PurposeLoan and EmplStatus being independent.",
    "chisq_stat <- 11.107; df <- 8; 1 - pchisq(chisq_stat, df)\n## [1] 0.1958\n# Equivalently from the raw two-way table:\nchisq.test(Credit$PurposeLoan, Credit$EmplStatus)"
), "images": []}

past_exams["exam_g1_2026_3a"] = {
"title": "G1-2026 Ex3a — Chi² independence test: PurposeLoan vs EmplStatus",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    'We are interested in whether the reason for requesting a loan (`PurposeLoan`) and the employment status (`EmplStatus`) are associated using an appropriate test. Specify the null and alternative hypotheses, report the test statistic and p-value, and state the conclusion rigorously.',
    '**Hypotheses** (Chi² test of independence):\n\n$$H_0:\\; \\text{the two variables are independent} \\quad\\text{vs}\\quad H_1:\\; \\text{the two variables are associated.}$$\n\n**Test statistic & p-value**: $\\chi^2_{\\text{obs}} = 11.107$ on $df = (r-1)(c-1) = 8$ (the source uses `1-pchisq(11.107, 8)`), giving\n\n$$p\\text{-value} = P(\\chi^2_8 > 11.107) \\approx 0.196.$$\n\nEquivalently, in R: `1 - pchisq(11.107, 8)`.\n\n**Interpretation**: probability of obtaining a value as extreme or more extreme than the one observed in the sample, $11.107$, under the assumption that $H_0$ is true (i.e. the variables are independent).\n\n**Conclusion**: since p-value $\\approx 0.196 > 0.05$ (and $> 0.10$), we **do not reject $H_0$** at any common level. The data are consistent with `PurposeLoan` and `EmplStatus` being independent.',
    'tab <- table(Credit$PurposeLoan, Credit$EmplStatus)\nchisq.test(tab)\n## X-squared = 11.107, df = 8, p-value = 0.1958\n# Manual p-value\n1 - pchisq(11.107, df = 8)\n## [1] 0.1958'
), "images": []}

past_exams["exam_g1_2026_3b"] = {
"title": "G1-2026 Ex3b — SE for difference in mean RiskIndex across EmplStatus groups",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "The variable `RiskIndex` is a composite indicator summarizing the borrower's overall financial risk. We want to compare the average `RiskIndex` for clients employed (`EmplStatus = Empl`) and unemployed (`EmplStatus = Unemp`). Based on the assumption that the standard deviation of the difference between the two means is unknown, provide the analytical expression of the estimator of the standard error of the estimator of the difference between the two means, and report its numerical estimate.",
    '**Hypotheses**:\n\n$$H_0:\\; \\mu_{\\text{Empl}} = \\mu_{\\text{Unemp}} \\quad\\text{vs}\\quad H_1:\\; \\mu_{\\text{Empl}} \\ne \\mu_{\\text{Unemp}}.$$\n\n**Analytical estimator** (variances unknown, *not* assumed equal — Welch form):\n\n$$\\widehat{SE}(\\bar X_1 - \\bar X_2) \\;=\\; \\sqrt{\\dfrac{s_1^2}{n_1} + \\dfrac{s_2^2}{n_2}},$$\n\nwhere $s_1^2, s_2^2$ are the sample variances and $n_1, n_2$ the group sizes for the two `EmplStatus` categories.\n\n**Estimate**: plugging the sample values for `RiskIndex` split by `EmplStatus` gives\n\n$$\\widehat{SE} \\;=\\; 2.218.$$',
    "x1 <- Credit$RiskIndex[Credit$EmplStatus == 'Empl']\nx2 <- Credit$RiskIndex[Credit$EmplStatus == 'Unemp']\nn1 <- length(x1); n2 <- length(x2)\nSE <- sqrt(var(x1)/n1 + var(x2)/n2)\nSE\n## [1] 2.218\n# Welch two-sample t-test using this SE\nt.test(RiskIndex ~ EmplStatus, data = Credit, var.equal = FALSE)"
), "images": []}

past_exams["exam_g1_2026_5a"] = {
"title": "G1-2026 Ex4.f (i) — Point estimate + 95% CI for risk index (mod2, EmplStatus=Empl, Age=40, Income=30, DebtIndex=0.3)",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Using `mod2`, obtain and report a **point estimate** and a **95% confidence interval** for the average risk index of employed clients with `EmplStatus = Empl`, `Age = 40`, `Income = 30`, `DebtIndex = 0.3`.\n\n**Estimated mod2**: $\\widehat{RiskIndex} = 52.19 - 2.12\\cdot I(\\text{Stud}) - 5.42\\cdot I(\\text{Unemp}) - 0.03\\cdot Age - 0.05\\cdot Income + 33.68\\cdot DebtIndex$.",
    "Plug $\\text{EmplStatus}=\\text{Empl}$ (both indicators $=0$), $\\text{Age}=40$, $\\text{Income}=30$, $\\text{DebtIndex}=0.3$ into the fitted equation:\n\n$$\\hat y_0 \\;=\\; 52.19 \\;-\\; 0.03\\cdot 40 \\;-\\; 0.05\\cdot 30 \\;+\\; 33.68\\cdot 0.3 \\;=\\; 59.71.$$\n\nThe `predict(..., interval='confidence', level=0.95)` call returns:\n\n- **Point estimate**: $\\hat y_0 = 59.71$.\n- **95% CI for the mean response**: $(58.42,\\; 61.01)$ — covers the *average* risk index across all employed clients with that exact profile.",
    "newx <- data.frame(EmplStatus='Empl', Age=40, Income=30, DebtIndex=0.3)\npredict(mod2, newdata=newx, interval='confidence', level=0.95)\n##        fit     lwr     upr\n## 1   59.71   58.42   61.01"
), "images": []}

past_exams["exam_g1_2026_5b"] = {
"title": "G1-2026 Ex4.f (ii) — Is a risk index of 70 unexpected/anomalous? (CI vs PI)",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Explain, motivating your answer, whether — based on the estimated `mod2` — a **risk index of 70** for a client with `EmplStatus = Empl`, `Age = 40`, `Income = 30`, `DebtIndex = 0.3` should be considered **unexpected or anomalous**.",
    "The right reference for a **single new client** is the **prediction interval**, not the confidence interval for the mean. The PI adds the irreducible error variance:\n\n$$SE(\\hat y_{\\text{new}})^2 \\;=\\; SE(\\hat y_{\\text{mean}})^2 + \\hat\\sigma^2,$$\n\nso the PI is **wider** than the CI for the mean response.\n\nFrom the source output (at $\\text{Age}=40$, $\\text{Income}=30$, $\\text{DebtIndex}=0.3$):\n\n- **95% CI for the mean response**: $(58.42,\\; 61.01)$ — about the *average* risk index of such clients.\n- **95% PI for a single new client**: **contains $70$** (wider than the CI).\n\n**Conclusion.** A risk index of $70$ lies **outside the 95% CI** (so it is far from the predicted *average* for that profile) but **inside the 95% PI** for a single new client. Given the residual variability of `mod2`, the value $70$ is therefore **not unexpected/anomalous** for an *individual* client with that profile — it would be implausible only as a *mean* across such clients.",
    "newx <- data.frame(EmplStatus='Empl', Age=40, Income=30, DebtIndex=0.3)\npredict(mod2, newdata=newx, interval='confidence', level=0.95)\n##        fit     lwr     upr\n## 1   59.71   58.42   61.01\npredict(mod2, newdata=newx, interval='prediction', level=0.95)\n# wider than the CI — contains 70 ⇒ a single-client risk index of 70 is NOT anomalous"
), "images": []}

# ---- general 2 2024: 5b ----
past_exams["exam_g2_2024_5b"] = {
"title": "G2-2024 Ex5b — 99% CI for proportion of cities with CrimePeople > 250",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Build the 99% CI for the proportion of U.S. cities with CrimePeople > 250. $\\hat p = 0.21$, $n = 485$.",
    "Normal-approximation CI: $\\hat p \\pm z_{0.995}\\cdot \\sqrt{\\hat p(1-\\hat p)/n} = 0.21 \\pm 2.576\\cdot \\sqrt{0.21\\cdot 0.79/485} = 0.21 \\pm 2.576\\cdot 0.0185 = 0.21 \\pm 0.0477 \\approx [0.16, 0.26]$, exactly matching the R output.",
    "vec.binA <- CrimeUS$CrimePeople > 250\nCI.prop(vec.binA, conf.level=0.99)\n# manual:\np_hat <- 0.21; n <- 485\np_hat + c(-1,1)*qnorm(0.995)*sqrt(p_hat*(1-p_hat)/n)\n## [1] 0.1623 0.2577"
), "images": []}

# ---- general 2 2026: 1b, 1c, 2b, 2c, 4.4, 4.5, 4.6 ----
past_exams["exam_g2_2026_1b"] = {
"title": "G2-2026 Ex1b — Analytic SE for difference in proportions (with numerics)",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Report the **analytical expression** of the estimated standard error of the estimator for the difference between the two considered proportions, providing the numerical values of the involved quantities.",
    "$$\\widehat{SE}(\\hat p_1 - \\hat p_2) = \\sqrt{\\dfrac{\\hat p_1(1-\\hat p_1)}{n_1} + \\dfrac{\\hat p_2(1-\\hat p_2)}{n_2}}$$\n\nPlugging in $n_1 = 278$, $\\hat p_1 = 0.64$, $n_2 = 189$, $\\hat p_2 = 0.418$:\n\n$$\\widehat{SE} = \\sqrt{\\dfrac{0.64\\cdot(1-0.64)}{278} + \\dfrac{0.418\\cdot(1-0.418)}{189}} = 0.121.$$\n\nThis is the SE used in 1a's 90% CI (width $= 2\\cdot 1.645\\cdot 0.121 \\approx 0.398$).",
    "n1 <- 278; p1 <- 0.64\nn2 <- 189; p2 <- 0.418\nSE_diff <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)\nSE_diff\n## [1] 0.121\n# Pooled SE (only for the H0: p1=p2 test, not for the CI)\np_pool <- (n1*p1 + n2*p2)/(n1+n2)\nse_0   <- sqrt(p_pool*(1-p_pool)*(1/n1 + 1/n2))"
), "images": []}

past_exams["exam_g2_2026_1c"] = {
"title": "G2-2026 Ex1c — One-sided interpretation (cleaning category more expensive)",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Report the interpretation of the estimated standard error of the estimator for the difference between the two considered proportions, providing the numerical values of the involved quantities.\n\n**Interval: 0.147 ± 1.645·0.121 = [-0.052, 0.346]. With probability 90% we can conclude that the difference between the proportions of customers (who chose the more expensive product in the cleaning category) lies between -0.052 and 0.346.**",
    "The estimated standard error $\\widehat{SE}(\\hat p_1 - \\hat p_2) = 0.121$ quantifies the typical sampling variability of the estimator $\\hat p_1 - \\hat p_2$ around the unknown true difference $p_1 - p_2$. Inserting it in the 90% CI:\n\n$$0.147 \\pm 1.645\\cdot 0.121 \\;=\\; (-0.052,\\; 0.346).$$\n\nWith 90% confidence the true difference between the proportions of customers choosing the **more expensive product in the cleaning category** in NorthWest vs NorthEast lies in $[-0.052, 0.346]$. Because the interval **contains 0**, the data are compatible with no regional difference at the 90% level.",
    "n1 <- 278; p1 <- 0.64\nn2 <- 189; p2 <- 0.418\nSE <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)\nSE                              # 0.121\n(p1 - p2) + c(-1,1) * 1.645 * SE  # 90% CI: -0.052, 0.346\n## [1] -0.052  0.346"
), "images": []}

past_exams["exam_g2_2026_2b"] = {
"title": "G2-2026 Ex2b — Rejection region & p-value (one-sample z-test, σ known)",
"is_exam": True, "topic_hint": "G12",
"content": _q(
    "Given the test on campaign effectiveness $H_0:\\mu_{\\text{PRICE-Y}}=850$ vs $H_1:\\mu_{\\text{PRICE-Y}}>850$ with $\\sigma=300$ known, derive the **rejection region** and the **p-value**, clearly explaining your reasoning.",
    "Since $\\sigma$ is **known**, the test statistic under $H_0$ is\n\n$$Z \\;=\\; \\dfrac{\\bar X - 850}{\\sigma/\\sqrt{n}} \\;=\\; \\dfrac{\\bar X - 850}{300/\\sqrt{n}} \\;\\overset{H_0}{\\sim}\\; \\mathcal{N}(0,1).$$\n\n**Rejection region** (one-sided, upper tail at level $\\alpha$): reject $H_0$ iff\n\n$$Z_{\\text{obs}} > z_{1-\\alpha} \\quad\\Longleftrightarrow\\quad \\bar X > 850 + z_{1-\\alpha}\\cdot\\dfrac{300}{\\sqrt{n}}.$$\n\nAt $\\alpha=0.05$: $z_{0.95}=1.645$; at $\\alpha=0.01$: $z_{0.99}=2.326$.\n\n**p-value** (right-tail):\n\n$$\\text{p-value} \\;=\\; \\Pr(Z > Z_{\\text{obs}} \\mid H_0) \\;=\\; 1 - \\Phi(Z_{\\text{obs}}).$$\n\n**Worked example** with $n=50$, $\\bar x=920$:\n\n$$Z_{\\text{obs}} = \\dfrac{920-850}{300/\\sqrt{50}} = \\dfrac{70}{42.43} \\approx 1.65, \\qquad \\text{p-value} = 1-\\Phi(1.65) \\approx 0.0495.$$\n\nSince p $\\approx 0.0495 < 0.05$, we **reject $H_0$** at the 5% level (borderline; we would *not* reject at 1%).",
    "n    <- 50            # sample size (from Ex2.a)\nxbar <- 920           # sample mean (from Ex2.a)\nmu0  <- 850\nsigma<- 300\nalpha<- 0.05\n# Test statistic\nz <- (xbar - mu0) / (sigma / sqrt(n))\nz\n## [1] 1.6499\n# Critical value (one-sided, upper tail)\nqnorm(1 - alpha)\n## [1] 1.6449\n# p-value (right-tail)\n1 - pnorm(z)\n## [1] 0.04948\n# Decision: reject H0 iff p-value < alpha\n(1 - pnorm(z)) < alpha\n## [1] TRUE"
), "images": []}

past_exams["exam_g2_2026_2c"] = {
"title": "G2-2026 Ex2c — Conclusion: is the campaign effective?",
"is_exam": True, "topic_hint": "G12",
"content": _q(
    "Based on the test in 2.b, state the **conclusion**: is the marketing department's claim (the campaign raised the average price paid above €850) supported by the data? Interpret in plain words.",
    "**Decision rule recap**: reject $H_0:\\mu=850$ in favour of $H_1:\\mu>850$ iff p-value $< \\alpha$.\n\n**With $n=50$, $\\bar x=920$**: $Z_{\\text{obs}}\\approx 1.65$, p-value $\\approx 0.0495$.\n\n- At $\\alpha=0.05$: p-value $< 0.05 \\Rightarrow$ **reject $H_0$**. The data provide statistically significant evidence (at the 5% level) that the post-campaign average price paid $\\mu_{\\text{PRICE-Y}}$ is **higher than €850**. ⇒ The marketing department's claim is **supported** and the campaign appears **effective**.\n- At $\\alpha=0.01$: p-value $> 0.01 \\Rightarrow$ **do not reject $H_0$**. At a more conservative level the evidence is **insufficient** to declare the campaign effective.\n\n**Caveats**: the result is borderline at 5% and hinges on the assumed known $\\sigma=300$. A one-sided lower bound $\\bar x - z_{1-\\alpha}\\,\\sigma/\\sqrt{n}$ that exceeds 850 confirms the rejection and quantifies the practical magnitude of the increase, not just its statistical significance.",
    "n    <- 50;  xbar <- 920;  mu0 <- 850;  sigma <- 300\nz   <- (xbar - mu0)/(sigma/sqrt(n))\npval<- 1 - pnorm(z)\npval\n## [1] 0.04948\n# Conclusion at alpha = 5%\nif (pval < 0.05) 'Reject H0: campaign effective' else 'Do not reject H0'\n## [1] \"Reject H0: campaign effective\"\n# One-sided 95% lower bound on mu\nxbar - qnorm(0.95) * sigma/sqrt(n)\n## [1] 850.2  # lower bound > 850 confirms the rejection"
), "images": []}

past_exams["exam_g2_2026_4_4"] = {
"title": "G2-2026 Ex4.4 — Formal homoscedasticity assumption + diagnostic plots for mod1",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Formally state the homoscedasticity assumption underlying the linear regression model and discuss whether there is empirical evidence of its violation for the considered model `mod1`, clearly specifying the diagnostic tool(s) you use.",
    "**Assumption**: the error variance is **constant** (no heteroscedasticity), i.e. $\\mathrm{Var}(\\varepsilon_i\\mid\\mathbf{x}_i) = \\sigma^2$ for every $i = 1,\\ldots,n$ — it does **not** depend on the values of the explanatory variables in the model.\n\n**Evidence of violation — diagnostic tools**: (1) **residuals vs fitted values** plot — `plot(mod1, which=1)`; flag funnel/cone shapes. (2) **Scale-location** plot of $\\sqrt{|\\text{standardized residuals}|}$ against fitted values — `plot(mod1, which=3)`; flag an upward/downward trend in the post-smoothing red line. If both plots show no clear pattern (flat scatter, flat red line) → the homoscedasticity assumption appears satisfied for `mod1`; otherwise it is violated.",
    "plot(mod1, which=1)\nplot(mod1, which=3)\nlibrary(lmtest); bptest(mod1)"
), "images": []}

past_exams["exam_g2_2026_4_5"] = {
"title": "G2-2026 Ex4.5 — Point prediction + 95% PI for Amount at exp_pre=250",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Obtain a point prediction and a 95% prediction interval for the post-promotional expenditure of a southern customer (`region = South`) with `age = 50`, `paid_amount = 1200` and `exp_pre = 250`.",
    "Use `predict()` on `mod1` with the new observation and `interval='prediction', level=0.95`.\n\n**Point prediction**: $\\hat y = 6523.5731$.\n\n**95% Prediction interval**: $[6387.6292,\\ 6659.5209]$.\n\nThe prediction interval is **wider** than the confidence interval for the mean response because it accounts for **both** the uncertainty in the estimated mean and the irreducible error variance $\\hat\\sigma^2$: $SE(\\hat y_{\\text{new}})^2 = SE(\\hat y_{\\text{mean}})^2 + \\hat\\sigma^2$.",
    "predict(mod1, newdata=data.frame(exp_pre=250, amount=1200), interval='prediction', level=0.95)\n##        fit      lwr      upr\n## 1 6523.5731 6387.6292 6659.5209"
), "images": []}

past_exams["exam_g2_2026_4_6"] = {
"title": "G2-2026 Ex4.6 — Is loyalty a significant predictor? (Adj R² + p-value)",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Would you suggest to include in the model also the client's `loyalty` (score ranging between 0 and 100)? Motivate rigorously your answer.",
    "Fit the augmented model `mod2` adding `loyalty` and compare to `mod1` on two dimensions.\n\n**Goodness-of-fit**: the adjusted $R^2$ slightly increases from $0.4132$ to $0.4151$ by including the predictor `loyalty` — the increase is small, but it suggests that the new predictor is able to provide additional explanatory power to the previous model.\n\n**Significance**: the p-value of the significance test for `loyalty` is $0.0441$, which is a statistically significant predictor (at the conventional $\\alpha = 0.05$ level), although marginal.\n\n**Conclusion**: even if the increase in the adjusted $R^2$ is mild, `loyalty` appears to be a significant predictor that might be useful in the regression analysis. Recommend including it.",
    "mod2 <- lm(amount ~ exp_pre + age + region + paid_amount + loyalty, data=DF)\nsummary(mod2)\n## Adjusted R-squared: 0.4151    (vs 0.4132 for mod1)\n## loyalty   p-value: 0.0441"
), "images": []}

# ---- september 2024: 1b, 2b, 3b, 3c ----
past_exams["exam_sep_2024_1b"] = {
"title": "Sep-2024 Ex1b — 90% CI for proportion of Eligible='Y' customers",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Estimate the proportion of customers who were granted a credit card in the population (`Eligible='Y'`) and provide a **90% confidence interval** for $p$.",
    "**Point estimate**: $\\hat p = \\#\\{\\text{Eligible}='Y'\\}/n \\approx 0.67$ with $n = 8000$. **Normal-approx validity check**: $n\\hat p(1-\\hat p) = 8000\\cdot 0.67\\cdot 0.33 \\approx 1768 \\gg 5$, so the Wald/normal approximation is valid. **90% CI**: $\\hat p \\pm z_{0.95}\\cdot\\sqrt{\\hat p(1-\\hat p)/n} = 0.67 \\pm 1.645\\cdot\\sqrt{0.67\\cdot 0.33/8000} \\approx [0.6613,\\,0.6787]$. **Interpretation**: with 90% confidence the population proportion of eligible customers lies in $[0.66, 0.68]$.",
    "CI.prop(Eligible=='Y', conf.level=0.90, data=Credit)\n## Confidence interval for the proportion of cases were Eligible == 'Y'\n## Confidence level: 0.9\n##  n   phat  lower  upper\n## 8000 0.67  0.6613 0.6787"
), "images": []}

past_exams["exam_sep_2024_2b"] = {
"title": "Sep-2024 Ex2b — Maximum score of the bottom 20% by branch",
"is_exam": True, "topic_hint": "G6",
"content": _q(
    "Calculate the maximum `Score` value for the 20% of customers in the specific branch with the lowest credit score. Compare with the analogous value for the main branches and state which comparison is more reliable and why.",
    "The required value is the **20th percentile** ($p_{20}$) of `Score` **within each branch group**: it is the maximum score attained by the lowest-scoring 20% of customers in that branch. Compute it directly with `distr.summary.x(..., stats='p20', by=Branch)`. **Caveat**: the specific branch has a much smaller sub-sample than the main branches, so its $p_{20}$ carries larger sampling variability and the cross-branch comparison must be considered an **approximation**, not an exact match.",
    "distr.summary.x(Score, by=Branch, stats='p20', data=Credit)"
), "images": []}

past_exams["exam_sep_2024_3b"] = {
"title": "Sep-2024 Ex3b — 95% CI for Account_length slope + interpretation",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Provide a 95% CI for the Account_length slope coefficient and interpret.",
    "Point estimate $\\hat\\beta_{\\text{Account\\_length}} = 7.84$ (from `summary(m)`). The 95% CI is $\\hat\\beta \\pm t_{0.975,\\,n-p}\\cdot \\widehat{SE}(\\hat\\beta) \\approx 7.84 \\pm 1.96\\cdot \\widehat{SE}$, where $\\widehat{SE}$ is the *Std. Error* column of `summary(m)` for `Account_length`. Use `confint(m, 'Account_length', level=0.95)` to read the exact bounds. **Interpretation**: with 95% confidence, each additional year of customer relationship is associated with a Score increase lying inside this interval, holding the other regressors fixed. Since $\\hat\\beta=7.84$ is far from 0 (and significant at $\\alpha=0.05$ per the summary), the CI excludes 0 → the slope is significantly different from 0.",
    "summary(m)\nconfint(m, level=0.95)\nconfint(m, 'Account_length', level=0.95)"
), "images": []}

past_exams["exam_sep_2024_3c"] = {
"title": "Sep-2024 Ex3c — 90% CI for difference in credit-approval proportions across two banks",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "In a sample of 200 customers of another bank, exactly 156 customers were granted a credit. Provide the **90% confidence interval for the difference** between the proportions of credit-card customers at the two banks. Include details of your calculations.",
    "Bank-1 (Credit data): $\\hat p_1 = 0.70$ with $n_1 = 500$. Bank-2: $\\hat p_2 = 156/200 = 0.78$ with $n_2 = 200$. Difference $\\hat p_1 - \\hat p_2 = -0.08$.\n\n$\\widehat{SE}(\\hat p_1-\\hat p_2) = \\sqrt{\\dfrac{\\hat p_1(1-\\hat p_1)}{n_1} + \\dfrac{\\hat p_2(1-\\hat p_2)}{n_2}} = \\sqrt{\\dfrac{0.7\\cdot 0.3}{500} + \\dfrac{0.78\\cdot 0.22}{200}} \\approx 0.0353$.\n\nWith $z_{0.95} = 1.645$: 90% CI = $-0.08 \\pm 1.645 \\cdot 0.0353 = [-0.1382,\\ -0.0218]$.\n\n**Interpretation**: with 90% confidence the difference $p_1-p_2$ lies in $[-0.1382,-0.0218]$. The CI is entirely negative → the second bank has a significantly **higher** credit-approval proportion than the first.",
    "p1 <- 0.70; n1 <- 500\np2 <- 156/200; n2 <- 200\nse <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)\n(p1 - p2) + c(-1,1) * qnorm(0.95) * se\n# or\nprop.test(c(350, 156), c(500, 200), conf.level=0.90, correct=FALSE)"
), "images": []}

# ---- september 2025: 1b, 2b, 2c, 3a, 5b ----
past_exams["exam_sep_2025_1b"] = {
"title": "Sep-2025 Ex1b — Multiple regression mod1: Performance ~ Weight + Ascent + HR.avg + Day.time",
"is_exam": True, "topic_hint": "G24",
"content": _q(
    "Estimate a multiple linear regression model (**mod1**) relating Performance to Weight, Ascent, HR.avg and Day.time (3 decimals). Report estimated coefficients.",
    "Fit OLS with one quantitative response and 3 numeric predictors + 1 categorical (Day.time, 3 levels → 2 dummies, Afternoon = baseline). $\\widehat{\\beta}$ minimizes $\\sum(y_i - x_i^\\top\\beta)^2$. **Estimated equation:** $$\\widehat{\\text{Performance}} = 151.921 - 2.029\\cdot\\text{Weight} - 11.022\\cdot\\text{Ascent} + 0.593\\cdot\\text{HR.avg} - 0.366\\cdot\\mathbb{1}(\\text{Evening}) - 0.366\\cdot\\mathbb{1}(\\text{Morning}).$$ Interpretation: holding others fixed, +1 kg Weight → −2.029 in Performance; +1 unit Ascent → −11.022; +1 bpm HR.avg → +0.593; Evening/Morning sessions score 0.366 lower than Afternoon (baseline).",
    "mod1 <- lm(Performance ~ Weight + Ascent + HR.avg + Day.time, data=Performance)\nsummary(mod1)"
), "images": []}

past_exams["exam_sep_2025_2b"] = {
"title": "Sep-2025 Ex2b — Two-sided z-test from estimate and SE",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Given estimate $\\hat\\beta = 0.510$ and standard error $\\mathrm{SE}(\\hat\\beta) = 0.221$, test $H_0: \\beta = 0$ vs $H_1: \\beta \\ne 0$ and report the p-value (two-sided, $z$ approximation).",
    "Wald z-statistic: $z = \\hat\\beta / \\mathrm{SE}(\\hat\\beta) = 0.510/0.221 \\approx 2.308$. Two-sided p-value: $p = 2\\,(1-\\Phi(|z|)) = 2\\,(1-\\Phi(2.308)) \\approx 0.021$. Reject $H_0$ at $\\alpha = 0.05$ (p < 0.05); fail to reject at $\\alpha = 0.01$ (p > 0.01).",
    "z <- 0.510/0.221\nz\n## [1] 2.307692\n2*(1-pnorm(z))\n## [1] 0.02102"
), "images": []}

past_exams["exam_sep_2025_2c"] = {
"title": "Sep-2025 Ex2c — Conclusion across significance levels",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Using the p-value from 2b ($p \\approx 0.021$), state the test conclusion at significance levels $\\alpha \\in \\{0.01,\\ 0.025,\\ 0.05,\\ 0.10\\}$.",
    "Decision rule: reject $H_0$ iff $p < \\alpha$. With $p \\approx 0.021$:\n\n- $\\alpha = 0.01$: $p > \\alpha$ → **fail to reject** $H_0$.\n- $\\alpha = 0.025$: $p < \\alpha$ → **reject** $H_0$.\n- $\\alpha = 0.05$: $p < \\alpha$ → **reject** $H_0$.\n- $\\alpha = 0.10$: $p < \\alpha$ → **reject** $H_0$.\n\nEvidence against $H_0$ is moderate: significant at the 2.5%, 5% and 10% levels but **not** at the 1% level.",
    "p <- 2*(1-pnorm(0.510/0.221))\np\n## [1] 0.02102\np < c(0.01, 0.025, 0.05, 0.10)\n## [1] FALSE  TRUE  TRUE  TRUE"
), "images": []}

past_exams["exam_sep_2025_3a"] = {
"title": "Sep-2025 Ex3a — Conditional frequency Fr(Effort | Rain=Yes)",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "From the stacked-bar plot of Effort (Low / Medium-Low / Medium-High / High) by Rain (Yes / No), read off the conditional frequency that Effort is at least Medium-High given Rain = Yes.",
    "Stacked bars with `freq.type='x|y'` show $\\Pr(\\text{Effort}=e \\mid \\text{Rain}=r)$: each Rain column sums to 1. Sum the relevant Effort slices in the Rain=Yes column: $\\Pr(\\text{Effort} \\in \\{\\text{Medium-High},\\text{High}\\} \\mid \\text{Rain}=\\text{Yes}) = 0.39 + 0.48 = \\mathbf{0.87}$. So on rainy days about 87% of activities are at high or medium-high effort.",
    "distr.plot.xy(Effort, Rain, plot.type='bars', freq.type='x|y', data=Performance)\ndistr.table.xy(Effort, Rain, freq.type='x|y', freq='prop', data=Performance)"
), "images": ["statistics/images/exam_sep_2025_effort_by_rain.png"]}

