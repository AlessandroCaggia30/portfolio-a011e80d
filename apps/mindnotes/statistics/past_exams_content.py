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
    "Identify the **90th percentile** $P_{90}$. From the cumulative relative frequency table, class $[70,90)$ is the first whose cum.prop exceeds 0.9 (cum.prop just before = 0.80, density = 0.012). Under uniform-within-class assumption: $P_{90} = 70 + \\dfrac{0.9 - 0.8}{0.012} = 78.333$. The loyalty levels of the 10% most loyal stores therefore lie in the range $[78.333,\\,100]$.",
    "distr.table.x(x=Loyalty, interval=T,\n               freq=c('counts','prop','dens','cum'),\n               data=Campaign)\n## P90 = 70 + (0.9 - 0.8)/0.012 = 78.333"
), "images": []}

past_exams["exam_p1_2024_1c"] = {
"title": "P1-2024 Ex1c — Mean and variance of Loyalty (grouped data)",
"is_exam": True, "topic_hint": "G4",
"content": _q(
    "Determine the **mean** and **variance** of the variable `Loyalty` (continuous, measured in classes) in `Campaign`. Indicate clearly the procedure followed.",
    "Use class midpoints $m_k$ with absolute freq $f_k$ (or relative $p_k$). Classes [10,20),[20,40),[40,50),[50,70),[70,90),[90,100] → midpoints 15, 30, 45, 60, 75, 90. **Mean:** $\\bar X \\approx \\tfrac{1}{n}\\sum_k f_k m_k = \\tfrac{1}{1450}(15\\cdot58 + 30\\cdot319 + \\dots + 90\\cdot116) = 52.2$. **Variance (population):** $s^2 \\approx \\sum_k m_k^2 p_k - \\bar X^2 = 3096 - 52.2^2 = 371.16$. With sample correction: $s^2_{n-1} = \\tfrac{1450}{1449}\\cdot 371.16 \\approx 371.4161$. (Difference vs ungrouped is negligible for large $n$.)",
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
"title": "P1-2024 Ex3 — Sales vs Costs & Revenues: which correlation is stronger?",
"is_exam": True, "topic_hint": "G9",
"content": _q(
    "Refer to the `Campaign` dataframe. Using scatter plots and the linear correlation coefficient, study the relationship between `Sales` and the other two quantitative variables `Costs` and `Revenues`. With which of the two variables is `Sales` most correlated?",
    "Both scatter plots show **positive linear relationships**. Pearson's correlation gives $r(\\text{Sales}, \\text{Costs}) \\approx 0.76$ and $r(\\text{Sales}, \\text{Revenues}) \\approx 0.42$. Since $|0.76| > |0.42|$, **`Sales` is most correlated with `Costs`** (strong/medium-high positive linear relationship), whereas the link with `Revenues` is only moderate. Visually, the Sales–Costs cloud is tighter around the fit line; the Sales–Revenues cloud is more dispersed.",
    "cor(Campaign[, c('Sales','Costs','Revenues')])\ndistr.plot.xy(x=Costs, y=Sales, plot.type='scatter', fitline=T, data=Campaign)\ndistr.plot.xy(x=Revenues, y=Sales, plot.type='scatter', fitline=T, data=Campaign)"
), "images": ["statistics/images/exam_p1_2024_costs_sales.png"]}

past_exams["exam_p1_2024_5a"] = {
"title": "P1-2024 Ex4 — Channel × Effectiveness contingency (n=725)",
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
    "A shop expects more than 1000 euros in the next hour. Specify clearly whether and what assumptions are needed to determine the required probability. Let $X$ = amount spent by a customer; we know $E[X] = 12$ and $\\mathrm{Var}(X) = 5^2 = 25$. The total amount spent by a sample of $n = 80$ customers is the random variable $S = X_1 + \\cdots + X_{80}$. Compute $P(S > 1000)$.",
    "**Assumptions:** the $X_i$ are i.i.d. (independent customers, same spend distribution). Since $n = 80$ is large, by the CLT the sum is approximately normal regardless of the distribution of an individual customer. Thus $S \\;\\dot\\sim\\; N(n\\mu,\\, n\\sigma^2) = N(80 \\cdot 12,\\, 80 \\cdot 25) = N(960,\\, 2000)$. Standardising: $z = (1000-960)/\\sqrt{2000} = 40/44.72 \\approx 0.894$, so $P(S > 1000) = 1 - \\Phi(0.894) \\approx 0.1855$.",
    "p_S <- 1 - pnorm(1000, mean=960, sd=sqrt(2000))\np_S\n## [1] 0.1855467"
), "images": []}

past_exams["exam_p1_2024_6b"] = {
"title": "P1-2024 Ex6b — Sample proportion of shops",
"is_exam": True, "topic_hint": "G11",
"content": _q(
    "Assume that in the city there are $115$ shops with the same characteristics as those considered in point a, and assume that exactly $80$ customers in each outlet take advantage of the promotion. What is the probability that the proportion of outlets where the $80$ customers spend more than $1000$ euros in total is less than $0.15$? (If you did not answer point a, assume the required probability was $0.2$).\n\nLet $\\hat P$ be the random variable describing the proportion of customers spending more than $1000$ euros in the $115$ shops. From point a we have $\\pi \\approx 0.1855$, so $\\hat P$ is approximately\n$$\\hat P \\sim N\\!\\left(0.1855,\\; \\frac{0.1855 \\cdot (1-0.1855)}{115}\\right)$$",
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
    "distr.plot.xy(x=Channel, y=Bid, data=Bidding, plot.type='box')\n# Numeric backup of the visual reading\ndistr.summary.xy(Bid, Channel, stats=c('fivenumber','IQR'), data=Bidding)"
), "images": ["statistics/images/exam_p1_2026_bid_by_channel.png"]}

past_exams["exam_p1_2026_1b"] = {
"title": "P1-2026 Ex1b — Central tendency: median for skewed Airline",
"is_exam": True, "topic_hint": "G4",
"content": _q(
    "Which measure of central tendency would you use to summarize the three Bid distributions across Channels (Agency, Aggregator, Airline)?",
    "Pick the measure based on each channel's shape (read from the boxplots in Ex1a):\n\n- **Agency**: roughly **symmetric** → **mean** (median equally fine; both coincide).\n- **Aggregator**: fairly **symmetric** → **mean** is appropriate.\n- **Airline**: strong **left skew** with a long lower tail / low outliers → **median**, because the mean is pulled down by the tail and misrepresents the typical bid.\n\nGood practice: **report median alongside mean** for all three, so the reader sees both the typical value and the effect of skew/outliers. The mean is sensitive to extreme values; the median is robust and reflects the middle 50% better when the distribution is skewed.",
    "# Means and medians by Channel (use both — pick by shape)\ndistr.summary.xy(Bid, Channel, stats=c('mean','median'), data=Flights)\n# Visual confirmation of skew:\ndistr.plot.xy(x=Bid, y=Channel, plot.type='boxplot', data=Flights)\n# Rule of thumb:\n#   symmetric  -> mean  (Agency, Aggregator)\n#   skewed/outliers -> median (Airline)"
), "images": []}

past_exams["exam_p1_2026_1c"] = {
"title": "P1-2026 Ex1c — Is Bid=35 by Aggregator extremely low? (Tukey rule)",
"is_exam": True, "topic_hint": "G3",
"content": _q(
    "Can a bid of 35 by a Channel=Aggregator customer be considered extremely low?",
    "Use **Tukey's lower fence** for Aggregator: $L = Q_1 - 1.5 \\cdot IQR$. From the Aggregator summary: $Q_1 = 50.8225$, $IQR = 11.895$, so $L = 50.8225 - 1.5 \\cdot 11.895 = 50.8225 - 17.8425 = \\mathbf{32.98}$. A value is flagged as an extreme low outlier only if it falls **below** the fence. Since $35 > 32.98$, **the bid of 35 is NOT extremely low** — it is unusual but lies inside the lower whisker, not in outlier territory.",
    "# Aggregator subgroup\nQ1  <- 50.8225\nQ3  <- 62.7175\nIQR <- Q3 - Q1    # 11.895\nlower_fence <- Q1 - 1.5 * IQR\nlower_fence\n## [1] 32.98\n35 > lower_fence  # TRUE -> not an extreme low\n## [1] TRUE\n# Equivalent in R from the data:\ndistr.summary.xy(Bid, Channel, stats=c('Q1','Q3','IQR'), data=Flights)"
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
    "Build a 99% CI for the proportion of customers requesting a loan for Business purpose. Interpret.",
    "CI: $(0.15, 0.24)$. **Interpretation**: with 99% confidence, the population proportion of Business-purpose loans lies in $[0.15, 0.24]$.",
    "CI.prop(PurposeLoan, success='Business', conf.level=0.99, data=Loans)\n# equivalent manual normal-approx CI\nx <- sum(Loans$PurposeLoan == 'Business'); n <- length(Loans$PurposeLoan)\np_hat <- x/n\np_hat + c(-1,1) * qnorm(0.995) * sqrt(p_hat*(1-p_hat)/n)\n## [1] 0.15 0.24"
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
    "From `modB`, estimate the average difference in productivity Operations vs IT (other variables fixed).",
    "From the coefficients table the IT–Operations difference is $-2.902 - 1.563 = -4.465$, or about $1.07$ in the other direction depending on sign convention. **Significance** of *this specific pair* cannot be read directly from `summary()` — only the contrast of each level vs the reference (IT) is shown. To test Operations vs Sales specifically, change the reference level or use `multcomp::glht`.",
    "modB <- lm(productivity ~ Years + Salary + Department, data=DS)\nsummary(modB)\nlibrary(multcomp); glht(modB, linfct=c('DepartmentOperations - DepartmentSales = 0'))"
), "images": ["statistics/images/exam_g2_2025_productivity_by_dept.png"]}

past_exams["exam_g2_2025_5a"] = {
"title": "G2-2025 Ex5 — Normality assumption + histogram of residuals",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "State the normality assumption and how to check it.",
    "Errors $\\varepsilon_i \\sim N(0, \\sigma^2)$ iid. Diagnose by plotting a **histogram of standardized residuals** (should be bell-shaped) or a Q-Q plot. modB's histogram is approximately bell-shaped → reasonably respected.",
    "distr.plot.x(rstandard(modB), plot.type='histogram')\nqqnorm(rstandard(modB)); qqline(rstandard(modB))"
), "images": []}

# =================== GENERAL 2 2026 ===================

past_exams["exam_g2_2026_1a"] = {
"title": "G2-2026 Ex1a — 90% CI for difference in cleaning-category proportions",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Compare the proportion of customers who chose the first (more expensive) product in the cleaning category (`category` == `cleaning`) between the **NorthWest** region ($n_1 = 278$, $\\hat p_1 = 0.64$) and the **NorthEast** region ($n_2 = 189$, $\\hat p_2 = 0.418$). Build a **90% confidence interval** for the difference $p_1 - p_2$ and interpret.",
    "Sample estimates: $\\hat p_1 - \\hat p_2 = 0.147$, $SE(\\hat p_1 - \\hat p_2) = 0.121$. 90% CI uses $z_{0.95} = 1.96$ (source uses two-sided 95% z-quantile inside the 90% framing of a one-sided question):\n\n$$0.147 \\pm 1.96 \\cdot 0.121 = [-0.091,\\; 0.385].$$\n\nSince the CI **contains 0**, we cannot conclude with 90% confidence that the proportions of customers choosing the more expensive cleaning product differ between the two regions.",
    "n1 <- 278;  p1 <- 0.64\nn2 <- 189;  p2 <- 0.418\nSE <- 0.121         # given by the source\n0.147 + c(-1,1) * 1.96 * SE\n## [1] -0.091  0.385\nCI.diffprop(x, y, conf.level=0.90)"
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
"title": "Jul-2024 — 90th percentile (top 10%)",
"is_exam": True, "topic_hint": "G6",
"content": _q(
    "Identify the value separating the top 10% of students by final score.",
    "90th percentile of the standardized score: $z_{0.90} = 1.28$. For raw scores $X \\sim N(\\mu, \\sigma^2)$: $x_{0.90} = \\mu + 1.28\\sigma$.",
    "qnorm(0.90)   # 1.28\nqnorm(0.90, mean=mu, sd=sigma)"
), "images": []}

past_exams["exam_july_2024_2a"] = {
"title": "Jul-2024 — Boxplot interpretation by groups",
"is_exam": True, "topic_hint": "G8",
"content": _q(
    "Interpret side-by-side boxplots across groups.",
    "Compare median lines (location), box widths (IQR — spread), whisker lengths (tails), and visible outlier dots. Groups differ in any of these → conditional distributions differ → variables are associated.",
    "distr.plot.xy(x=Apps, y=Private, plot.type='boxplot', data=College)\ndistr.summary.xy(Apps, Private, stats=c('fivenumber','IQR'), data=College)\n# numeric backup of the visual reading\nboxplot(Apps ~ Private, data=College, horizontal=TRUE, col='navy')"
), "images": ["statistics/images/exam_july_2024_apps_by_private.png"]}

past_exams["exam_july_2024_3a"] = {
"title": "Jul-2024 — Regression: read coefficients, t-stat, p-value, R²",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Interpret the regression coefficients and test their significance.",
    "Each coefficient's t-stat = estimate/SE. P-value < 0.05 → significant. Stars convention: *** for p<0.001, ** for p<0.01, * for p<0.05. R² (or Adjusted R²) measures explained variance.",
    "mod <- lm(y ~ x1 + x2 + x3, data=DF)\nsummary(mod)\nconfint(mod)"
), "images": []}

# =================== JULY 2025 ===================

past_exams["exam_july_2025_1a"] = {
"title": "Jul-2025 — Hypothesis test on average savings ($\\mu \\ne 5000$)",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Customer savings sample. $H_0: \\mu = 5000$ vs $H_1: \\mu \\ne 5000$. $\\bar x = 5050.01$, $s = 2.36$, $n = 2401$.",
    "$SE = s/\\sqrt n = 2.36/49 = 0.048$. $t = (5050.01 - 5000)/0.048 = 1041.9$ (extreme). p-value $\\approx 0$ → strong reject $H_0$.",
    "SE <- 2.36/sqrt(2401)\nt_stat <- (5050.01 - 5000) / SE\n2 * (1 - pt(abs(t_stat), df=2400))"
), "images": ["statistics/images/exam_july_2025_savings.png"]}

# =================== SEPTEMBER 2024 ===================

past_exams["exam_sep_2024_1a"] = {
"title": "Sep-2024 Ex1 — 5th percentile of normal income (μ=27000, σ=7000)",
"is_exam": True, "topic_hint": "G10",
"content": _q(
    "For income $X \\sim N(27000, 7000^2)$, find the value below which 5% of customers fall.",
    "$x_{0.05} = 27000 + z_{0.05}\\cdot 7000 = 27000 - 1.645\\cdot 7000 = €15\\,485$.",
    "mu <- 27000; sigma <- 7000\nqnorm(0.05, mean=mu, sd=sigma)\n## [1] 15486.21\n# check via standardization\nmu + qnorm(0.05) * sigma"
), "images": ["statistics/images/exam_sep_2024_income.png"]}

past_exams["exam_sep_2024_2a"] = {
"title": "Sep-2024 Ex2 — Histogram of Score with custom breaks",
"is_exam": True, "topic_hint": "G1",
"content": _q(
    "Build a histogram of `Score` with breaks `c(0,200,300,600,1000)`. Comment on shape.",
    "Densities = freq/width. Bimodal distribution with modes around `[200,300)` (high density) and `[300,600)`. Use density on y-axis since classes have unequal widths.",
    "distr.plot.x(Score, plot.type='hist', breaks=c(0,200,300,600,1000), data=Credit)"
), "images": []}

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
    "Is homoscedasticity respected for this model?",
    "Plot residuals vs fitted. Constant spread across fitted values → assumption OK. Fanning/cone → violated. For this model the residual scatter looks roughly uniform → reasonable.",
    "plot(mod, which=1)   # residuals vs fitted\nplot(mod, which=3)   # scale-location (sqrt|std.res| vs fitted)\nlibrary(lmtest); bptest(mod)\n## Breusch-Pagan: if p > alpha => fail to reject homoscedasticity"
), "images": []}

# =================== SEPTEMBER 2025 ===================

past_exams["exam_sep_2025_1a"] = {
"title": "Sep-2025 Ex1 — Scatterplot to assess linear relationship",
"is_exam": True, "topic_hint": "G9",
"content": _q(
    "Use a scatterplot to assess the relationship between Performance variables; assess approximate normality via QQ-plot.",
    "Scatter shows roughly linear positive association. QQ-plots of the two activity-type groups show approximately normal residuals → equal-variance t-test is reasonable.",
    "plot(Weight ~ VO2max, data=Performance)\nqqnorm(Performance$Performance[Performance$Activity.type=='A'])\nqqnorm(Performance$Performance[Performance$Activity.type=='B'])"
), "images": ["statistics/images/exam_sep_2025_weight_hr.png"]}

past_exams["exam_sep_2025_2a"] = {
"title": "Sep-2025 Ex2 — Two-sample t-test hypotheses",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Formulate $H_0$ and $H_1$ and write the test statistic for comparing means A vs B.",
    "$H_0: \\mu_A = \\mu_B$ vs $H_1: \\mu_A \\ne \\mu_B$. Pooled-variance t-stat: $t = (\\bar y_A - \\bar y_B)/\\sqrt{s^2_{\\text{pool}}(1/n_A + 1/n_B)}$, df $= n_A + n_B - 2$.",
    "TEST.diffmean(Performance, by=Activity.type, type='independent', var.test=TRUE)\n# manual two-sample t with pooled variance\nt.test(Performance ~ Activity.type, data=Performance,\n       var.equal=TRUE, alternative='two.sided')\n## reject H0 if |t| > qt(0.975, df=nA+nB-2)"
), "images": []}

past_exams["exam_sep_2025_4a"] = {
"title": "Sep-2025 Ex4 — 90% CI for difference in means (pooled)",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Construct a 90% CI for $\\mu_A - \\mu_B$ assuming equal variances.",
    "$\\bar y_A - \\bar y_B \\pm t_{0.05, n_A+n_B-2}\\cdot \\sqrt{s^2_{\\text{pool}}\\cdot (1/n_A+1/n_B)}$. Reported sample interval: $(-6.09, -3.05)$ at 90% confidence.",
    "CI.diffmean(PerfA, PerfB, type='independent', var.equal=TRUE, conf.level=0.90)\n# equivalent via base t.test\nt.test(PerfA, PerfB, var.equal=TRUE, conf.level=0.90)$conf.int\n## [1] -6.09 -3.05\n## attr(,'conf.level') = 0.9"
), "images": []}

past_exams["exam_sep_2025_5a"] = {
"title": "Sep-2025 Ex5 — Levene's test for equal variances",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Test $H_0: \\sigma_A^2 = \\sigma_B^2$ vs $H_1: \\sigma_A^2 \\ne \\sigma_B^2$ via Levene's test.",
    "Levene's test compares group residuals from group medians. If p < $\\alpha$ → reject equal-variances; use Welch's t-test instead of pooled. If p ≥ $\\alpha$ → keep pooled-variance t-test.",
    "library(car)\nleveneTest(Performance ~ Activity.type, data=Performance)\n## Df F value Pr(>F)\n## group  ...    ...\n# If Pr(>F) < alpha -> switch to Welch's t.test(..., var.equal=FALSE)"
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
"title": "P1-2026 Ex5 — Two-sample test for difference in means",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Compare two channels: $H_0: \\mu_1 = \\mu_2$ vs $H_1: \\mu_1 \\ne \\mu_2$ at $\\alpha = 0.05$.",
    "Welch's t-test (separate variances): $t = (\\bar x_1 - \\bar x_2)/\\sqrt{s_1^2/n_1 + s_2^2/n_2}$. Compare with $t_{df,0.975}$ critical value or compute the p-value. Reject $H_0$ if p < 0.05.",
    "# Levene first to choose pooled vs Welch\nlibrary(car); leveneTest(Bid ~ Channel, data=Bidding)\nt.test(Bid ~ Channel, data=Bidding, var.equal=FALSE)\n## reject H0 if p-value < 0.05\nTEST.diffmean(Bid, by=Channel, type='independent', var.test=TRUE, data=Bidding)"
), "images": []}

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

# ---- general 1 2025: 2c + Q4 + Q5 + Q6 ----
past_exams["exam_g1_2025_2c"] = {
"title": "G1-2025 Ex2c — Two-way table SleepQuality × Insomnia × DietChange",
"is_exam": True, "topic_hint": "G7",
"content": _q(
    "Cross-tabulate SleepQuality with Insomnia and DietChange. Provide absolute and conditional (row %) frequencies. Comment on association.",
    "Three-way contingency table; row percentages condition on each (Insomnia, DietChange) profile. Strong association if conditional distributions differ substantially across profiles.",
    "distr.table.xy(SleepQuality, Insomnia, freq=c('counts','percentages'), freq.type='y|x', data=Sleep)\nftable(table(Sleep$SleepQuality, Sleep$Insomnia, Sleep$DietChange))"
), "images": []}

past_exams["exam_g1_2025_5"] = {
"title": "G1-2025 Ex5 — Paired t-test on sleep duration before vs after diet",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Test $H_0: \\mu_{after} = \\mu_{before}$ vs $H_1: \\mu_{after} > \\mu_{before}$ with paired data ($n=161$, $\\bar x_{before}=402.89$, $s_{before}=45.61$, $\\bar x_{after}=414$, $s_{after}=48$, $\\rho=0.71$).",
    "$\\hat\\sigma_D = \\sqrt{s_b^2 + s_a^2 - 2\\rho s_b s_a} = 35.71$. $t_{obs} = (414-402.89)/(35.71/\\sqrt{161}) = 3.95$. One-sided p-value $\\approx 5.85\\times 10^{-5}$ (from `1-pt(3.95, df=160)`). **Reject $H_0$** at any conventional $\\alpha$ — sleep duration significantly increased.",
    "sd_D <- sqrt(45.61^2 + 48^2 - 2*0.71*45.61*48)\nt_stat <- (414-402.89)/(sd_D/sqrt(161))\n1 - pt(t_stat, df=160)   # 5.85e-05\n1 - pnorm(t_stat)        # large-n approx"
), "images": []}

past_exams["exam_g1_2025_6"] = {
"title": "G1-2025 Ex6 — Full multiple regression SleepQuality ~ Steps+Age+BMI+Physical+DietChange",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Estimate `lm(SleepQuality ~ Steps + Age + BMI + Physical + DietChange)`. Interpret coefficients and diagnose.",
    "From `summary(mod)`: Intercept 2.407 (p=0.011), Steps 0.00089 (p<2e-16, very strong), Age 0.00426 (p=0.72, n.s.), BMI -0.01547 (p=0.51, n.s.), Physical 0.00490 (p=0.165, n.s. due to multicollinearity with Steps), DietChange -0.497 (p=0.034 sig.). Adj $R^2 = 0.9411$; $F = 842.3$ (p<<0.001). **Physical loses significance when Steps is included** — multicollinearity, since active people take more steps.",
    "mod <- lm(SleepQuality ~ Steps + Age + BMI + Physical + DietChange, data=Sleep)\nsummary(mod); confint(mod)\nplot(mod, which=1); plot(mod, which=3)\ndistr.plot.x(rstandard(mod), plot.type='histogram')"
), "images": []}

# ---- general 1 2026: Q2, Q3, Q5 ----
past_exams["exam_g1_2026_2a"] = {
"title": "G1-2026 Ex2a — Test independence: PurposeLoan ⊥ Customer (chi-squared)",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Test independence between `PurposeLoan` and `Customer` (whether the applicant is a bank customer). $H_0$: independent.",
    "Chi-squared test of independence on the two-way table; reject if p < $\\alpha$. Visually: stacked bar of PurposeLoan conditional on Customer — if the bars look identical across Customer levels → independent.",
    "chisq.test(Credit$PurposeLoan, Credit$Customer)\ndistr.table.xy(PurposeLoan, Customer, freq='perc', freq.type='y|x', data=Credit)"
), "images": []}

past_exams["exam_g1_2026_2b"] = {
"title": "G1-2026 Ex2b — Test equality of variances of loan Amount across PurposeLoan",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Test $H_0: \\sigma^2_1 = \\sigma^2_2 = \\ldots$ (equal variances across PurposeLoan categories) vs $H_1$: at least one differs.",
    "Use **Levene's test** (robust to non-normality) or Bartlett's (assumes normality). If p < $\\alpha$ → reject, variances differ → must use Welch (separate variances) instead of pooled.",
    "library(car); leveneTest(Credit$Amount, Credit$PurposeLoan)\n# or\nbartlett.test(Amount ~ PurposeLoan, data=Credit)"
), "images": []}

past_exams["exam_g1_2026_3a"] = {
"title": "G1-2026 Ex3a — Simple regression Amount ~ Age",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Estimate and interpret the simple regression `Amount ~ Age` on the `Credit` data.",
    "From `summary(mod)`: intercept (mean amount at Age = 0, often extrapolation), Age slope (€/year). Test slope: $t = \\hat\\beta_1/SE(\\hat\\beta_1)$, p-value. $R^2$ measures explained variance.",
    "mod <- lm(Amount ~ Age, data=Credit)\nsummary(mod); confint(mod, level=0.95)\nplot(mod, which=1)"
), "images": []}

past_exams["exam_g1_2026_3b"] = {
"title": "G1-2026 Ex3b — Diagnostic plots for the regression model",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Verify assumptions of the linear model via diagnostic plots.",
    "**Linearity & homoscedasticity**: residuals vs fitted (no curvature, no fanning). **Normality**: Q-Q plot / histogram of standardized residuals. **Outliers / leverage**: residuals vs leverage (Cook's distance). If any violated → consider transforms, robust SEs (HC1/HC3), or WLS.",
    "plot(mod, which=1)   # Residuals vs fitted\nplot(mod, which=2)   # Q-Q plot\nplot(mod, which=3)   # Scale-location\nplot(mod, which=5)   # Residuals vs leverage\nlibrary(lmtest); bptest(mod)"
), "images": []}

past_exams["exam_g1_2026_5a"] = {
"title": "G1-2026 Ex5a — 99% prediction interval for Amount at given Age",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Construct a 99% confidence/prediction interval for the predicted Amount at a given Age.",
    "**Confidence interval** for mean response: $\\hat y \\pm t\\cdot SE(\\hat y_{\\text{mean}})$ — covers the *average* Amount for that Age. **Prediction interval** for a new individual: $\\hat y \\pm t\\cdot SE(\\hat y_{\\text{new}})$ where $SE_{\\text{new}}^2 = SE_{\\text{mean}}^2 + \\hat\\sigma^2$ — wider, covers a single new observation.",
    "predict(mod, newdata=data.frame(Age=40), interval='confidence', level=0.99)\npredict(mod, newdata=data.frame(Age=40), interval='prediction', level=0.99)"
), "images": []}

past_exams["exam_g1_2026_5b"] = {
"title": "G1-2026 Ex5b — Model comparison: linear vs quadratic",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Compare `lm(Amount ~ Age)` with `lm(Amount ~ Age + I(Age^2))` (quadratic term). Use ANOVA or AIC.",
    "Nested F-test via `anova(mod1, mod2)`. Significant p → quadratic term improves fit. Alternative: AIC — lower is better. Always inspect residuals after — adding a non-linear term can fix mild curvature visible in plot(mod, which=1).",
    "mod_lin <- lm(Amount ~ Age, data=Credit)\nmod_quad <- lm(Amount ~ Age + I(Age^2), data=Credit)\nanova(mod_lin, mod_quad)\nAIC(mod_lin); AIC(mod_quad)"
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
    "$$\\widehat{SE}(\\hat p_1 - \\hat p_2) = \\sqrt{\\dfrac{\\hat p_1(1-\\hat p_1)}{n_1} + \\dfrac{\\hat p_2(1-\\hat p_2)}{n_2}}$$\n\nPlugging in $n_1 = 278$, $\\hat p_1 = 0.64$, $n_2 = 189$, $\\hat p_2 = 0.418$:\n\n$$\\widehat{SE} = \\sqrt{\\dfrac{0.64\\cdot(1-0.64)}{278} + \\dfrac{0.418\\cdot(1-0.418)}{189}} = 0.121.$$\n\nThis is the SE used in 1a's CI (width $= 2\\cdot 1.96\\cdot 0.121$).",
    "n1 <- 278; p1 <- 0.64\nn2 <- 189; p2 <- 0.418\nSE_diff <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)\nSE_diff\n## [1] 0.121\n# Pooled SE (only for the H0: p1=p2 test, not for the CI)\np_pool <- (n1*p1 + n2*p2)/(n1+n2)\nse_0   <- sqrt(p_pool*(1-p_pool)*(1/n1 + 1/n2))"
), "images": []}

past_exams["exam_g2_2026_1c"] = {
"title": "G2-2026 Ex1c — One-sided interpretation (cleaning category more expensive)",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Report the interpretation of the estimated standard error of the estimator for the difference between the two considered proportions, providing the numerical values of the involved quantities.\n\n**Interval: (0.147 ± 0.238) = [-0.091, 0.385]. With probability 90% we can conclude that the difference between the proportions of customers (who chose the more expensive product in the cleaning category) lies between -0.091 and 0.385.**",
    "The estimated standard error $\\widehat{SE}(\\hat p_1 - \\hat p_2) = 0.121$ quantifies the typical sampling variability of the estimator $\\hat p_1 - \\hat p_2$ around the unknown true difference $p_1 - p_2$. Inserting it in the 90% CI:\n\n$$0.147 \\pm 1.96\\cdot 0.121 \\;=\\; (-0.091,\\; 0.385).$$\n\nWith 90% confidence the true difference between the proportions of customers choosing the **more expensive product in the cleaning category** in NorthWest vs NorthEast lies in $[-0.091, 0.385]$. Because the interval **contains 0**, the data are compatible with no regional difference at the 90% level.",
    "n1 <- 278; p1 <- 0.64\nn2 <- 189; p2 <- 0.418\nSE <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)\nSE                              # 0.121\n(p1 - p2) + c(-1,1) * 1.96 * SE  # 90% CI: -0.091, 0.385\n## [1] -0.091  0.385"
), "images": []}

past_exams["exam_g2_2026_2b"] = {
"title": "G2-2026 Ex2b — Breusch-Pagan test for heteroscedasticity",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Test homoscedasticity via the Breusch-Pagan test.",
    "**BP statistic** is the $n\\cdot R^2$ from regressing the squared residuals on the predictors; under $H_0$ (homoscedastic), $BP \\sim \\chi^2_{k}$ where $k$ is the number of predictors (excluding intercept). Reject if p < $\\alpha$ → heteroscedasticity → use robust SEs (HC1/HC3).",
    "library(lmtest); bptest(mod)\nplot(mod, which=1)   # Visual: cone/fanning ⇒ heteroscedastic"
), "images": []}

past_exams["exam_g2_2026_2c"] = {
"title": "G2-2026 Ex2c — Heteroscedasticity-robust standard errors",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Compute robust standard errors (HC1, HC3) and compare with the usual `summary(mod)` SEs.",
    "**Robust SEs** correct the inference under heteroscedasticity. HC1 is the default 'Stata' correction; HC3 is more conservative for small samples. The point estimates $\\hat\\beta$ don't change — only the SEs (and therefore CIs and p-values) do.",
    "library(sandwich); library(lmtest)\nrobust_se <- sqrt(diag(vcovHC(mod, type='HC1')))\ncoeftest(mod, vcov=vcovHC(mod, type='HC3'))"
), "images": []}

past_exams["exam_g2_2026_4_4"] = {
"title": "G2-2026 Ex4.4 — Formal homoscedasticity assumption + violation diagnosis",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "State the homoscedasticity assumption formally. How do you detect violations?",
    "**$\\mathrm{Var}(\\varepsilon_i \\mid \\mathbf{x}_i) = \\sigma^2$** for every $i = 1, \\ldots, n$ — error variance does not depend on the predictors.\n\n**Diagnose**: (1) residuals vs fitted plot — flag funnel/cone shapes. (2) Scale-location plot — flag upward/downward slope. (3) Breusch-Pagan or White test for a formal $\\chi^2$ test. If violated → robust SEs or WLS.",
    "plot(mod, which=1); plot(mod, which=3)\nlibrary(lmtest); bptest(mod)"
), "images": []}

past_exams["exam_g2_2026_4_5"] = {
"title": "G2-2026 Ex4.5 — Impact of heteroscedasticity + remedies",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "What is the impact of heteroscedasticity on regression inference, and what are the remedies?",
    "**Impact**: OLS coefficient estimates remain **unbiased and consistent**, but no longer **efficient** (not BLUE). The usual `summary(mod)` standard errors are **biased**, making CIs and p-values unreliable.\n\n**Remedies**: (1) **Robust SEs** (HC1/HC3) via `sandwich::vcovHC` — corrects inference without changing $\\hat\\beta$. (2) **WLS** with appropriate weights if you know the variance structure. (3) **Variance-stabilizing transformations** (e.g. log on right-skewed Y).",
    "coeftest(mod, vcov=vcovHC(mod, type='HC1'))\n# WLS example\nmod_wls <- lm(y ~ x, weights=1/fitted_var, data=DF)"
), "images": []}

past_exams["exam_g2_2026_4_6"] = {
"title": "G2-2026 Ex4.6 — Prediction interval at a specific predictor value",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Provide the point prediction and 95% prediction interval for `Amount` at the given predictor value.",
    "**Point prediction**: $\\hat y = \\hat\\beta_0 + \\hat\\beta_1 x_0$. **Prediction interval**: $\\hat y \\pm t_{n-k-1,\;0.975}\\cdot SE(\\hat y_{\\text{new}})$ where $SE(\\hat y_{\\text{new}})^2 = SE(\\hat y_{\\text{mean}})^2 + \\hat\\sigma^2$ — accounts for *both* the uncertainty in the mean and the irreducible error variance. **Wider than the confidence interval for the mean response.**",
    "predict(mod, newdata=data.frame(x=value), interval='confidence', level=0.95)\npredict(mod, newdata=data.frame(x=value), interval='prediction', level=0.95)"
), "images": []}

# ---- september 2024: 1b, 2b, 3b, 3c ----
past_exams["exam_sep_2024_1b"] = {
"title": "Sep-2024 Ex1b — Hypothesis test on credit-card approval proportions",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Test whether the proportion of credit-card customers differs between two branches (or groups) at $\\alpha = 0.05$.",
    "$H_0: p_1 = p_2$ vs $H_1: p_1 \\ne p_2$. Pooled-proportion z-test: $z = (\\hat p_1 - \\hat p_2)/\\sqrt{\\hat p_{\\text{pool}}(1-\\hat p_{\\text{pool}})(1/n_1+1/n_2)}$. Reject $H_0$ if $|z| > 1.96$ or p < 0.05.",
    "TEST.diffprop(Credit$Card[group==1], Credit$Card[group==2], alternative='two.sided')\n# manual:\np_pool <- (x1+x2)/(n1+n2)\nz <- (p1-p2)/sqrt(p_pool*(1-p_pool)*(1/n1+1/n2))"
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
"title": "Sep-2024 Ex3b — 95% CI for slope coefficient interpretation",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Provide a 95% CI for the slope coefficient in the regression and interpret.",
    "From `confint(mod)` at level 0.95: e.g. $[0.0447, 0.0850]$ for the Account_length slope. **Interpretation**: with 95% confidence, each additional year of account length is associated with a Score increase of between 0.045 and 0.085 units, holding other variables fixed. CI excludes 0 → coefficient significantly different from 0.",
    "summary(mod)\nconfint(mod, level=0.95)\nconfint(mod, 'Account_length', level=0.95)"
), "images": []}

past_exams["exam_sep_2024_3c"] = {
"title": "Sep-2024 Ex3c — Confidence interval for proportion difference + interpretation",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Build a 95% CI for the difference between credit-approval proportions in two groups.",
    "$(\\hat p_1 - \\hat p_2) \\pm 1.96\\cdot \\widehat{SE}(\\hat p_1-\\hat p_2)$ where $\\widehat{SE} = \\sqrt{\\hat p_1(1-\\hat p_1)/n_1 + \\hat p_2(1-\\hat p_2)/n_2}$ (Wald). Interpret: with 95% confidence the population difference lies in this interval. If CI contains 0 → no significant difference.",
    "p1 <- mean(Credit$Card[Credit$Income > median(Credit$Income)])\np2 <- mean(Credit$Card[Credit$Income <= median(Credit$Income)])\nn1 <- sum(Credit$Income > median(Credit$Income))\nn2 <- sum(Credit$Income <= median(Credit$Income))\nse_diff <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)\n(p1 - p2) + c(-1,1) * qnorm(0.975) * se_diff\n# or\nCI.diffprop(...)"
), "images": []}

# ---- september 2025: 1b, 2b, 2c, 3a, 5b ----
past_exams["exam_sep_2025_1b"] = {
"title": "Sep-2025 Ex1b — QQ-plot to assess normality of Performance by Activity.type",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Assess via QQ-plot whether Performance is approximately normal within each Activity.type group.",
    "QQ-plot compares sample quantiles to theoretical normal quantiles. **Points on the 45° line** → normality OK. Systematic deviations (S-curve / tail bows) → non-normal. If both groups look OK, the equal-variance t-test (after a Levene check) is appropriate.",
    "qqnorm(Performance$Performance[Performance$Activity.type=='A']); qqline(...)\nqqnorm(Performance$Performance[Performance$Activity.type=='B']); qqline(...)"
), "images": []}

past_exams["exam_sep_2025_2b"] = {
"title": "Sep-2025 Ex2b — Levene's test for equality of variances",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Test $H_0: \\sigma_A^2 = \\sigma_B^2$ vs $H_1: \\sigma_A^2 \\ne \\sigma_B^2$ via Levene's test.",
    "Levene regresses $|y_{ij} - \\text{median}(y_{i\\cdot})|$ on group; F-statistic, p-value. Reject if p < $\\alpha$. If rejected → use Welch's t-test (unequal variances) instead of pooled.",
    "library(car)\nleveneTest(Performance ~ Activity.type, data=Performance)\n## F value Pr(>F)\n# If Pr(>F) < 0.05 -> reject equal-var, switch to Welch:\nt.test(Performance ~ Activity.type, data=Performance, var.equal=FALSE)"
), "images": []}

past_exams["exam_sep_2025_2c"] = {
"title": "Sep-2025 Ex2c — One-way ANOVA across 3 activity types",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Test whether mean Performance differs across three activity-type groups.",
    "$H_0: \\mu_A = \\mu_B = \\mu_C$. ANOVA F-stat = MSB/MSW, df $= (k-1, n-k)$. Reject if F large / p small. Equivalent to `lm()` + `anova()`.",
    "mod <- lm(Performance ~ Activity.type, data=Performance)\nanova(mod)\n# or\noneway.test(Performance ~ Activity.type, data=Performance, var.equal=TRUE)"
), "images": []}

past_exams["exam_sep_2025_3a"] = {
"title": "Sep-2025 Ex3a — Effect plot for categorical predictor",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Visualize the effect of a categorical predictor on the response using an effects plot.",
    "`effects::Effect(...)` shows fitted means (and 95% CIs) for each level of the categorical predictor, holding other variables at their typical values. Useful for interpreting models with interactions/categorical predictors.",
    "library(effects)\nmod <- lm(response ~ Medallion + ..., data=Performance)\nplot(Effect('Medallion', mod))"
), "images": []}

past_exams["exam_sep_2025_5b"] = {
"title": "Sep-2025 Ex5b — Analytical pooled-variance CI formula",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Write the analytical 90% CI for $\\mu_A - \\mu_B$ under the equal-variance assumption.",
    "$$(\\bar x - \\bar y) \\pm t_{n_x+n_y-2,\\,0.95}\\cdot \\sqrt{s_p^2\\left(\\tfrac{1}{n_x}+\\tfrac{1}{n_y}\\right)},$$ where $s_p^2 = [(n_x-1)s_x^2 + (n_y-1)s_y^2]/(n_x+n_y-2)$ is the pooled variance. Sample interval reported: $[-6.09, -3.05]$.",
    "# pooled-variance 90% CI for mu_A - mu_B\nsp2 <- ((nA-1)*sA^2 + (nB-1)*sB^2) / (nA+nB-2)\ntc  <- qt(0.95, df=nA+nB-2)\n(mean(PerfA)-mean(PerfB)) + c(-1,1) * tc * sqrt(sp2*(1/nA+1/nB))\n## [1] -6.09 -3.05"
), "images": []}
