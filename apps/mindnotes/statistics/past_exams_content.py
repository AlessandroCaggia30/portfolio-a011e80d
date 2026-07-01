"""
Past-exam snippets (13 exams, 2024–2026).
Each snippet's content wraps the question text in <span class="exam-question-text">
for blue styling, plus full verbatim answer + R commands (some inferred).
The is_exam flag tells the builder to mark the card yellow.
"""

# Helper to build snippet content
def _q(q, a, r="", w=""):
    """Build content: blue Q + (optional Walkthrough) + Answer + R commands.

    If a walkthrough ``w`` is supplied, the structure becomes
    blue Q --- Walkthrough --- Answer + R commands, which satisfies
    the completeness rubric (two dividers, both Walkthrough and Answer).
    For backward compatibility, ``w`` defaults to empty and the older
    one-divider layout is emitted.
    """
    walkthrough_block = (
        '**Walkthrough.** ' + w.strip() + '\n\n---\n\n'
    ) if w.strip() else ''
    return (
        '<span class="exam-question-text">' + q.strip() + '</span>\n\n'
        '---\n\n'
        + walkthrough_block +
        '**Answer.** ' + a.strip() + '\n\n'
        + (('**R commands:**\n\n`' + '`\n\n`'.join(r.strip().split('\n')) + '`\n') if r.strip() else '')
    )

past_exams = {}

# =================== 1st PARTIAL 2024 ===================

past_exams["exam_p1_2024_1a"] = {
"title": "P1-2024 Ex1a — Modal class of Loyalty (interval-class variable)",
"is_exam": True, "topic_hint": "G2",
"content": (
    '<span class="exam-question-text">Consider the `Campaign` dataframe and the variable `Loyalty` — measured in interval classes — which represents the level of customer loyalty for the stores considered. What is the modal class of `Loyalty`? Explain clearly your answer and state the measures or tools you used to answer.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2024_1a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** `Loyalty` is a continuous quantitative variable measured in classes of **unequal width** (10, 20, 10, 20, 10, 20). With unequal widths, comparing raw counts or relative frequencies is misleading: a wide class can collect more observations simply because it is wider. The correct comparison is the **frequency density** $d_k = p_k / w_k$ (relative frequency per unit of width), which is exactly the height of each bar of the histogram. The modal class is the one with the **largest density**, i.e. the **tallest bar**.\n\n'
    '![AI walkthrough — frequency-density bars](statistics/images/past_exams/exam_p1_2024_1a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Compute densities from the table: [10,20) dens=0.004, [20,40) 0.011, **[40,50) 0.024**, [50,70) 0.015, [70,80) 0.012, [80,100) 0.004. **The modal class is [40,50)**, since it has the highest frequency density. The histogram (tallest bar over [40,50)) confirms the same conclusion.\n\n'
    '**R commands:**\n\n'
    '`distr.table.x(x=Loyalty, interval=T, freq=c(\'counts\',\'prop\',\'dens\',\'cum\'), data=Campaign)`\n\n'
    '`## Loyalty   Count Prop Density Cum.Count Cum.Prop`\n\n'
    '`## [10,20)    58  0.04  0.004      58     0.04`\n\n'
    '`## [20,40)   319  0.22  0.011     377     0.26`\n\n'
    '`## [40,50)   348  0.24  0.024     725     0.50`\n\n'
    '`## [50,70)   435  0.30  0.015    1160     0.80`\n\n'
    '`## [70,80)   174  0.12  0.012    1334     0.92`\n\n'
    '`## [80,100)  116  0.08  0.004    1450     1.00`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2024_1a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2024_1a_question.png",
    "statistics/images/past_exams/exam_p1_2024_1a_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2024_1a_answer.png",
]}

past_exams["exam_p1_2024_1b"] = {
"title": "P1-2024 Ex1b — Loyalty 90th percentile",
"is_exam": True, "topic_hint": "G3",
"content": (
    '<span class="exam-question-text">What are the levels of loyalty in the 10% of stores with the most loyal customers (`Loyalty`)? Indicate clearly which measures you use to answer and their numerical values.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2024_1b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** The "10% most loyal stores" correspond to the upper tail of `Loyalty`, i.e. the values above the **90th percentile** $P_{90}$. With data grouped in classes of unequal width, $P_{90}$ is obtained by **linear interpolation inside the class that first reaches cumulative proportion 0.9** (uniform-within-class assumption): identify the class $[L_k,\\,U_k)$ where the cumulative crosses 0.9, then $P_{90} = L_k + (0.9 - F_{k-1})\\,/\\,d_k$, where $F_{k-1}$ is the cumulative proportion at the lower bound and $d_k = p_k/(U_k-L_k)$ is the frequency density.\n\n'
    '![AI walkthrough — cumulative interpolation + top-10% shading](statistics/images/past_exams/exam_p1_2024_1b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** From the cumulative relative frequency table, class $[70,80)$ is the first whose cum.prop exceeds 0.9 (cum.prop just before = 0.80, density = 0.012). Under the uniform-within-class assumption:\n\n'
    '$$P_{90} = 70 + \\dfrac{0.9 - 0.8}{0.012} = 78.333$$\n\n'
    'The loyalty levels of the **10% most loyal stores** therefore lie in the range $[78.333,\\,100]$.\n\n'
    '**R commands:**\n\n'
    '`distr.table.x(x=Loyalty, interval=T,`\n\n'
    '`               freq=c(\'counts\',\'prop\',\'dens\',\'cum\'),`\n\n'
    '`               data=Campaign)`\n\n'
    '`## P90 = 70 + (0.9 - 0.8)/0.012 = 78.333`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2024_1b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2024_1b_question.png",
    "statistics/images/past_exams/exam_p1_2024_1b_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2024_1b_answer.png",
]}

past_exams["exam_p1_2024_1c"] = {
"title": "P1-2024 Ex1c — Mean and variance of Loyalty (grouped data)",
"is_exam": True, "topic_hint": "G4",
"content": (
    '<span class="exam-question-text">Determine the **mean** and **variance** of the variable `Loyalty` (continuous, measured in classes) in `Campaign`. Indicate clearly the procedure followed.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2024_1c_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** With **grouped (interval-class) data** we do not see the raw observations, only the count $f_k$ in each class. To compute summary statistics we **replace each class by its midpoint** $m_k = (a_k + b_k)/2$ and treat every observation in class $k$ as if it equalled $m_k$. The mean becomes the weighted average of midpoints with weights $f_k/n$, and the variance follows from the König–Huygens identity $s^2 = \\overline{X^2} - \\bar X^2$. Because the within-class spread is ignored, the grouped estimate slightly under-represents variability — for large $n$ and reasonably narrow classes the error is negligible.\n\n'
    '![AI walkthrough — midpoints, density bars, mean line, per-class contributions](statistics/images/past_exams/exam_p1_2024_1c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Classes [10,20), [20,40), [40,50), [50,70), [70,80), [80,100) → midpoints $m_k = 15, 30, 45, 60, 75, 90$ with counts $f_k = 58, 319, 348, 435, 174, 116$ ($n = 1450$).\n\n'
    '**Mean:** $\\bar X = \\tfrac{1}{n}\\sum_k f_k m_k = \\tfrac{1}{1450}(15\\cdot58 + 30\\cdot319 + 45\\cdot348 + 60\\cdot435 + 75\\cdot174 + 90\\cdot116) = \\tfrac{75690}{1450} = 52.2$.\n\n'
    '**Variance (population):** $s^2 = \\sum_k p_k m_k^2 - \\bar X^2 = 3096 - 52.2^2 = 3096 - 2724.84 = 371.16$. With sample correction: $s^2_{n-1} = \\tfrac{1450}{1449}\\cdot 371.16 \\approx 371.4161$ (negligibly larger). SD $s \\approx 19.27$.\n\n'
    '**R commands:**\n\n'
    '`mids <- c(15,30,45,60,75,90)`\n\n'
    '`fk   <- c(58,319,348,435,174,116)`\n\n'
    '`xbar <- sum(fk*mids)/sum(fk)            # 52.2`\n\n'
    '`var_pop <- sum(fk*mids^2)/sum(fk) - xbar^2   # 371.16`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2024_1c_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2024_1c_question.png",
    "statistics/images/past_exams/exam_p1_2024_1c_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2024_1c_answer.png",
]}

past_exams["exam_p1_2024_2a"] = {
"title": "P1-2024 Ex2a — Campaign Revenues boxplot",
"is_exam": True, "topic_hint": "G6",
"content": (
    '<span class="exam-question-text">Consider the `Campaign` dataframe. Refer to the boxplot representing the distribution of the variable `Revenues` (standard profitability of the stores). Indicate what the extremes of the box and the end points of the whiskers of the boxplot represent, and report their numerical values, clarifying what are the quantities underlying your answer.</span>\n\n'
    '![Ex 2a question — Revenues boxplot](statistics/images/past_exams/questions/exam_p1_2024_2a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.**\n\n'
    'The boxplot is built entirely from the **five-number summary**:\n\n'
    '`distr.summary.x(Revenues, stats=\'fivenumber\', data=Campaign)`\n\n'
    '`## n.n.a   min     q1 median      q3    max`\n\n'
    '`## 1450 0 105.82 804.55    984 1202.36 3312.54`\n\n'
    '**Box.** Its left edge sits at $Q_1 = 804.55$, its right edge at $Q_3 = 1202.36$, and the inner segment marks the **median** $= 984$. So the box covers the **middle 50%** of the stores in terms of Revenues.\n\n'
    '**IQR & Tukey fences.** $\\mathrm{IQR} = Q_3 - Q_1 = 1202.36 - 804.55 = 397.81$. The conventional fences are\n\n'
    '$$[\\,Q_1 - 1.5\\,\\mathrm{IQR},\\; Q_3 + 1.5\\,\\mathrm{IQR}\\,] = [804.55 - 596.72,\\; 1202.36 + 596.72] = [207.84,\\; 1799.07].$$\n\n'
    '**Whiskers.** Each whisker is drawn to the **most extreme observed value still inside** its fence — *not* to the fence itself, and *not* to Min/Max in general. With Min $= 105.82 < 207.84$, the *lower* whisker stops at the smallest Revenues observation $\\geq 207.84$ (close to the fence). The *upper* whisker stops at the largest Revenues observation $\\leq 1799.07$ (close to the fence).\n\n'
    '**Outliers.** Points beyond the upper fence — up to the **Max $= 3312.54$** — are plotted as **individual dots** outside the upper whisker. There are several of them, and they extend far to the right; nothing comparable on the left side.\n\n'
    '**Shape.** Long upper tail of outliers, short lower whisker, median closer to $Q_1$ than to $Q_3$ → distribution is clearly **right-skewed**.\n\n'
    '![Ex 2a AI walkthrough — boxplot anatomy & fences for Revenues](statistics/images/past_exams/exam_p1_2024_2a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $n = 1450$ (no missing). 5-number summary: Min $105.82$, $Q_1$ $804.55$, Median $984$, $Q_3$ $1202.36$, Max $3312.54$. The box spans from $Q_1 = 804.55$ to $Q_3 = 1202.36$ and is divided by the median $984$. $\\mathrm{IQR} = 1202.36 - 804.55 = 397.81$. The whiskers extend to the most extreme observed values still within $[Q_1 - 1.5\\cdot\\mathrm{IQR},\\, Q_3 + 1.5\\cdot\\mathrm{IQR}] = [207.84,\\, 1799.07]$; observations beyond the upper fence (up to Max $3312.54$) appear as individual outlier points. The right tail is much longer than the left — distribution **right-skewed**.\n\n'
    '**R commands:**\n\n'
    '`distr.summary.x(Revenues, stats=\'fivenumber\', data=Campaign)`\n\n'
    '`## n.n.a   min     q1 median      q3    max`\n\n'
    '`## 1450 0 105.82 804.55    984 1202.36 3312.54`\n\n'
    '`distr.plot.x(Revenues, plot.type=\'boxplot\', data=Campaign)`\n\n'
    '---\n\n'
    '**Reference answer.**\n\n'
    '![Ex 2a answer](statistics/images/past_exams/answers/exam_p1_2024_2a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2024_2a_question.png",
    "statistics/images/past_exams/exam_p1_2024_2a_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2024_2a_answer.png",
]}

past_exams["exam_p1_2024_2b"] = {
"title": "P1-2024 Ex2b — Revenues by Location (side-by-side boxplots)",
"is_exam": True, "topic_hint": "G8",
"content": (
    '<span class="exam-question-text">Refer to the side-by-side boxplots of `Revenues` conditional on `Location` (Semi-Central, Peripheral, Hinterland). Comment on the relationship between the two variables and on the strength of any association.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2024_2b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** With a **numeric Y (`Revenues`) conditional on a categorical X (`Location`)**, association is assessed by **comparing the conditional distributions $Y \\mid X = x$** across the categories. Side-by-side boxplots make this comparison visual: if the three boxes are stacked at the same level with matching IQRs and overlapping whiskers/outliers, the conditional distributions are essentially equal, so $Y$ is (practically) independent of $X$ — i.e. knowing $X$ does **not** change predictions of $Y$. Conversely, **shifted medians** and/or **different spreads** across categories are the visual fingerprint of an **association**.\n\n'
    '![AI walkthrough — exam case (no association) vs reference case (strong association)](statistics/images/past_exams/exam_p1_2024_2b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Since `Revenues` is numeric and `Location` is categorical, association is judged by comparing the conditional distributions of `Revenues` given `Location`. The three boxplots are **essentially identical**: medians coincide, IQRs (box heights) are very similar, and the whisker extents / outlier patterns overlap. The conditional distributions of Revenues are practically equal across the three groups → **weak (essentially no) association** between `Revenues` and `Location`. Knowing the `Location` does not help predict the level of `Revenues`.\n\n'
    '**R commands:**\n\n'
    "`distr.plot.xy(x=Revenues, y=Location, plot.type='boxplot', data=Campaign)`\n\n"
    "`distr.summary.xy(Revenues, Location, stats=c('fivenumber','mean','sd'), data=Campaign)`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_p1_2024_2b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2024_2b_question.png",
    "statistics/images/past_exams/exam_p1_2024_2b_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2024_2b_answer.png",
]}

past_exams["exam_p1_2024_3a"] = {
"title": "P1-2024 Ex3 — CV comparison Company A vs B",
"is_exam": True, "topic_hint": "G5",
"content": (
    '<span class="exam-question-text">Compare the dispersion of `Loyalty` between Company A (under investigation, mean 52.2, variance 371.16, SD $\\approx 19.26$) and competitor Company B (mean 65.0, SD 19.0) via the coefficient of variation, to decide where loyalty shows higher relative variability.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2024_3a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Variance and SD are **scale-dependent**, so comparing the raw spread of `Loyalty` across two companies with different mean levels can be misleading. The **coefficient of variation** $CV = s/|\\bar x|$ normalises dispersion by the mean, turning it into a **scale-free, relative** measure (often expressed as a percentage). The company with the **larger CV** has the **higher relative variability** — even if its absolute SD looks similar or smaller.\n\n'
    '![AI walkthrough — CV bars for Company A vs Company B](statistics/images/past_exams/exam_p1_2024_3a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $CV = s/|\\bar x|$. **Company A:** $CV_A = \\sqrt{371.16}/52.2 = 19.26/52.2 = 0.3691$. **Company B:** $CV_B = 19.0/65.0 = 0.2923$. Since $CV_A > CV_B$, **loyalty is more dispersed in the previous company (A) than in the competitor (B)**, in relative terms.\n\n'
    '**R commands:**\n\n'
    '`mean_A <- 52.2; var_A <- 371.16; sd_A <- sqrt(var_A)   # 19.2655`\n\n'
    '`CV_A   <- sd_A / mean_A                                # 0.3691`\n\n'
    '`mean_B <- 65.0; sd_B  <- 19.0`\n\n'
    '`CV_B   <- sd_B / mean_B                                # 0.2923`\n\n'
    '`c(CV_A = CV_A, CV_B = CV_B)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2024_3a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2024_3a_question.png",
    "statistics/images/past_exams/exam_p1_2024_3a_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2024_3a_answer.png",
]}

past_exams["exam_p1_2024_4a"] = {
"title": "P1-2024 Ex4 — Sales vs Costs & Revenues: which correlation is stronger?",
"is_exam": True, "topic_hint": "G9",
"content": (
    '<span class="exam-question-text">Refer to the `Campaign` dataframe. Using scatter plots and the linear correlation coefficient, study the relationship between `Sales` and the other two quantitative variables `Costs` and `Revenues`. With which of the two variables is `Sales` most correlated?</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2024_4a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** With two candidate quantitative regressors, the workflow is: (i) eyeball two scatter plots `Sales` vs each variable to assess direction, linearity, spread and outliers; (ii) compute Pearson\'s $r = \\mathrm{Cov}(X,Y) / (s_X s_Y)$, which is **scale-free** and bounded in $[-1, 1]$. The variable with **larger $|r|$** has the stronger *linear* association with `Sales`. Always check the scatter too — $r$ alone hides curvature and outliers.\n\n'
    '![AI walkthrough — Sales vs Costs and Sales vs Revenues with fit lines](statistics/images/past_exams/exam_p1_2024_4a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Both scatter plots show **positive, approximately linear relationships**. From `cor(Campaign[, c(\'Sales\',\'Costs\',\'Revenues\')])` we get $r(\\text{Sales}, \\text{Costs}) \\approx 0.758$ and $r(\\text{Sales}, \\text{Revenues}) \\approx 0.758$. The two correlations are **essentially identical** (they differ only at the third decimal), so on this dataset `Sales` is *equally* strongly linearly correlated with `Costs` and with `Revenues` — neither dominates. Both clouds are similarly tight around the fitted line, confirming a strong (medium-high) positive linear association in each case.\n\n'
    '**R commands:**\n\n'
    '`cor(Campaign[, c(\'Sales\',\'Costs\',\'Revenues\')])`\n\n'
    '`distr.plot.xy(x=Costs,    y=Sales, plot.type=\'scatter\', fitline=T, data=Campaign)`\n\n'
    '`distr.plot.xy(x=Revenues, y=Sales, plot.type=\'scatter\', fitline=T, data=Campaign)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2024_4a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2024_4a_question.png",
    "statistics/images/past_exams/exam_p1_2024_4a_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2024_4a_answer.png",
]}

past_exams["exam_p1_2024_5a"] = {
"title": "P1-2024 Ex5 — Channel × Effectiveness contingency (n=725)",
"is_exam": True, "topic_hint": "G7",
"content": (
    '<span class="exam-question-text">A company launched a promotional campaign. An in-depth analysis is carried out on a sample of $n=725$ customers. For each customer the *Channel* used to interact with the company (E-commerce / Multi-channel / Channel 2 / Mobile App) and the perceived *Effectiveness* of the campaign (Ineffective / Low / Medium / High) are observed. Analyze the relationship between the two variables. In particular, which Channel is associated with the highest perception of effectiveness?</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2024_5a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Both variables are **categorical**, so association is studied via the **two-way contingency table**. Because the question is *which Channel is most effective*, we condition **on Channel** and look at the row-wise conditional distributions $f(\\text{Effectiveness} \\mid \\text{Channel})$. If those row distributions are essentially the same across channels, the two variables are **independent**; if they differ, they are **associated**. Visually this is read off a **stacked bar chart** (one bar per Channel, segments summing to 100%): the Channel whose bar is most weighted on **Medium + High** is the one with the highest perceived effectiveness.\n\n'
    '![AI walkthrough — row-conditional Effectiveness given Channel](statistics/images/past_exams/exam_p1_2024_5a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Conditional row distributions of Effectiveness given Channel show a clear pattern: **Mobile App and Multi-channel customers have the highest combined share of Medium + High effectiveness ratings**, while **E-commerce customers concentrate the most in Ineffective / Low** (lowest perceived effectiveness). Channel 2 sits in between. Because the conditional distributions of Effectiveness vary noticeably across Channels, the two categorical variables are **associated** (non-independent): the perceived effectiveness of the campaign depends on the interaction channel. **Mobile App** is the channel with the highest perception of effectiveness.\n\n'
    '**R commands:**\n\n'
    "`distr.table.xy(Company$Channel, Company$Effectiveness, freq=c('counts','percentages'), freq.type='y|x')`\n\n"
    "`distr.plot.xy(x=Effectiveness, y=Channel, plot.type='bars', stack=TRUE, data=Company)`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_p1_2024_5a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2024_5a_question.png",
    "statistics/images/past_exams/exam_p1_2024_5a_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2024_5a_answer.png",
]}

past_exams["exam_p1_2024_6a"] = {
"title": "P1-2024 Ex6a — CLT: P(total spend > 1000) for n=80 customers",
"is_exam": True, "topic_hint": "G11",
"content": (
    '<span class="exam-question-text">A shop expects to make more than 1000 euros in total takings during the next hour. Specify clearly whether and what assumptions are needed to determine the required probability. Let $X$ = amount spent by a customer; we know $E[X] = 12$ and $\\mathrm{Var}(X) = 5^2 = 25$. The total amount spent by a sample of $n = 80$ customers is the random variable $S = X_1 + \\cdots + X_{80}$. Compute $P(S > 1000)$.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2024_6a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** With **$n=80$ i.i.d. customers** the **CLT** kicks in: the sum $S=\\sum X_i$ is approximately Normal, **independently of the underlying spend distribution**, with $E[S]=n\\mu$ and $\\mathrm{Var}(S)=n\\sigma^2$. Plug in: $E[S]=80\\cdot 12=960$, $\\mathrm{Var}(S)=80\\cdot 25=2000$ (so $\\sigma_S=\\sqrt{2000}\\approx 44.72$). To get $P(S>1000)$ just standardise: $z=(1000-960)/44.72\\approx 0.894$, so $P(S>1000)=1-\\Phi(0.894)\\approx 0.1855$. Visually, the threshold $1000$ sits about **0.9 standard deviations to the right of the mean**, so it is **not extreme** — there is roughly an **18.5% chance** that takings exceed 1000.\n\n'
    '![AI walkthrough — Normal density of S with right-tail shaded and standardised view](statistics/images/past_exams/exam_p1_2024_6a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** **Assumptions:** the $X_i$ are **i.i.d.** (independent customers, same spend distribution). Since $n=80$ is large, by the **CLT** the sum is approximately Normal regardless of the distribution of an individual customer. Thus\n$$S \\;\\dot\\sim\\; N(n\\mu,\\, n\\sigma^2) = N(80\\cdot 12,\\, 80\\cdot 25) = N(960,\\, 2000).$$\nStandardising: $z=(1000-960)/\\sqrt{2000}=40/44.72\\approx 0.894$, so $P(S>1000)=1-\\Phi(0.894)\\approx \\mathbf{0.1855}$.\n\n'
    '**R commands:**\n\n'
    '`p_S <- 1 - pnorm(1000, mean=960, sd=sqrt(2000))`\n\n'
    '`p_S`\n\n'
    '`## [1] 0.1855467`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2024_6a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2024_6a_question.png",
    "statistics/images/past_exams/exam_p1_2024_6a_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2024_6a_answer.png",
]}

past_exams["exam_p1_2024_6b"] = {
"title": "P1-2024 Ex6b — Sample proportion of shops",
"is_exam": True, "topic_hint": "G11",
"content": (
    '<span class="exam-question-text">Assume that in the city there are $115$ shops with the same characteristics as those considered in point a, and assume that exactly $80$ customers in each outlet take advantage of the promotion. What is the probability that the proportion of outlets where the $80$ customers spend more than $1000$ euros in total is less than $0.15$? (If you did not answer point a, assume the required probability was $0.2$).</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2024_6b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Each of the $N=115$ outlets is a Bernoulli trial: "do the 80 customers there collectively spend more than 1000?" From Ex6a we know each trial has success probability $\\pi \\approx 0.1855$. The **sample proportion** $\\hat P = (\\text{# successful outlets})/115$ is the mean of i.i.d. Bernoulli($\\pi$). By the CLT (with $N\\pi = 21.3 \\ge 10$ and $N(1-\\pi) = 93.7 \\ge 10$, the rule-of-thumb large-sample condition is comfortably met),\n$$\\hat P \\;\\dot\\sim\\; N\\!\\left(\\pi,\\; \\frac{\\pi(1-\\pi)}{N}\\right) = N(0.1855,\\, 0.001315),\\quad SE(\\hat P)=\\sqrt{\\pi(1-\\pi)/N}\\approx 0.03626.$$\nThe required probability is the **left tail** $P(\\hat P < 0.15)$. Standardising: $z = (0.15 - 0.1855)/0.03626 = -0.979$, so $P(\\hat P < 0.15) = \\Phi(-0.979) \\approx 0.164$. The left panel below shows the CLT normal with the left tail shaded; the right panel cross-checks by simulating 2000 batches of 115 outlets and counting how often the observed proportion falls below 0.15 — the empirical frequency matches the analytical 0.164.\n\n'
    '![AI walkthrough — sampling distribution of P-hat with shaded left tail + simulation check](statistics/images/past_exams/exam_p1_2024_6b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Let $\\hat P$ = proportion of the 115 outlets where the 80 customers spend more than 1000 euros in total. By the CLT, $\\hat P$ is approximately Normal with mean $\\pi \\approx 0.1855$ and variance $\\pi(1-\\pi)/N = 0.1855 \\cdot 0.8145 / 115 \\approx 0.001315$, i.e.\n$$\\hat P \\sim N\\!\\left(0.1855,\\; \\frac{0.1855 \\cdot (1-0.1855)}{115}\\right).$$\nThe required probability is\n$$P(\\hat P < 0.15) = \\Phi\\!\\left(\\frac{0.15-0.1855}{\\sqrt{0.1855\\cdot 0.8145/115}}\\right) = \\Phi(-0.979) \\approx 0.1637.$$\n\n'
    '**R commands:**\n\n'
    '`pnorm(0.15, mean=0.1855, sd=sqrt(0.1855*(1-0.1855)/115))`\n\n'
    '`## [1] 0.1636914`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2024_6b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2024_6b_question.png",
    "statistics/images/past_exams/exam_p1_2024_6b_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2024_6b_answer.png",
]}

# =================== 1st PARTIAL 2025 ===================

past_exams["exam_p1_2025_1a"] = {
"title": "P1-2025 Ex1a — Conditional Impressions distribution by Paid (Metrics2)",
"is_exam": True, "topic_hint": "G8",
"content": (
    '<span class="exam-question-text">Compare the conditional distributions of `Impressions` (post views, in hundreds) between Paid (Yes) and non-Paid (No) posts. Use side-by-side boxplots and characteristics (centre/spread/shape).</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2025_1a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** With a quantitative response (`Impressions`) and a binary categorical predictor (`Paid`), the natural tool is **side-by-side boxplots** of the *conditional distributions* $f(\\text{Impressions}\\mid \\text{Paid}=\\text{yes})$ vs $f(\\text{Impressions}\\mid \\text{Paid}=\\text{no})$, complemented by the **five-number summary by group**. Compare three features: **centre** (median), **spread** (IQR / whisker length), and **shape** (symmetric vs skewed, outliers). If the two conditional distributions differ in any of these features → `Paid` and `Impressions` are *associated*.\n\n'
    '![AI walkthrough — side-by-side boxplots](statistics/images/past_exams/exam_p1_2025_1a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Both conditional distributions are **strongly right-skewed** with a long upper tail and many high outliers, and both have a **wide IQR** (large spread). However, **Paid posts have a higher median** and a **markedly higher upper quartile** than non-Paid posts — visually, roughly **75% of Paid posts exceed the 75th percentile of non-Paid posts**. Since the conditional distributions differ in *centre* and *upper-tail spread*, **`Paid` and `Impressions` are associated**: paying for a post shifts the whole distribution of views upward.\n\n'
    '**R commands:**\n\n'
    '`distr.plot.xy(x=Impressions, y=Paid, plot.type=\'boxplot\', data=Metrics2)`\n\n'
    '`distr.summary.x(Impressions, by=Paid, stats=\'fivenumber\', data=Metrics2)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2025_1a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2025_1a_question.png",
    "statistics/images/past_exams/exam_p1_2025_1a_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2025_1a_answer.png",
]}

past_exams["exam_p1_2025_1b"] = {
"title": "P1-2025 Ex1.a2 — Best location measure for right-skewed Impressions by Paid",
"is_exam": True, "topic_hint": "G6",
"content": (
    '<span class="exam-question-text">Taking into account the **features** and the **shapes** of the two Impressions distributions (Paid vs non-Paid), which **location measure** would you use in order to suitably emphasize their differences? Explain your choice and report the values of the considered measures.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2025_1b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Both conditional Impressions distributions are **strongly right-skewed** with a long upper tail and many high outliers (the boxplots from Ex1.a1 show wide IQRs and a forest of upper whisker dots). Under right skew the **mean is dragged upward by the heavy tail**, so it mixes *typical* level with *tail mass* and is non-robust to those visible outliers. The **median** — together with $Q_{25}$ and $Q_{75}$ — is **robust** to outliers/skew and tracks the location of the bulk of the distribution, so it cleanly emphasizes the shift between Paid and non-Paid posts.\n\n'
    '![AI walkthrough — mean vs median under right skew](statistics/images/past_exams/exam_p1_2025_1b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Report the **median** (with $Q_{25}$, $Q_{75}$). From the five-number summary on `Metrics2`:\n\n'
    '- **Paid = yes**: median ≈ **67**, $Q_{25}/Q_{75}$ = 45 / 185, mean ≈ **143**.\n'
    '- **Paid = no**: median ≈ **52**, $Q_{25}/Q_{75}$ = 36 / 93, mean ≈ **107**.\n\n'
    'Both means sit *far above* their medians (143 ≫ 67 and 107 ≫ 52), confirming the heavy right tail pulls the mean up. The **mean-gap (≈ 36)** overstates the *typical* difference relative to the **median-gap (≈ 15)**. So Paid posts have a clearly higher *typical* level of Impressions and a much wider spread, and the **median** is the location measure to report.\n\n'
    '**R commands:**\n\n'
    '`distr.summary.x(Impressions, by=Paid, stats=\'fivenumber\', data=Metrics2)`\n\n'
    '`distr.summary.x(Impressions, by=Paid, stats=c(\'mean\',\'median\',\'IQR\'), data=Metrics2)`\n\n'
    '`distr.plot.xy(x=Impressions, y=Paid, plot.type=\'boxplot\', data=Metrics2)`\n\n'
    '`tapply(Metrics2$Impressions, Metrics2$Paid, median)`\n\n'
    '`tapply(Metrics2$Impressions, Metrics2$Paid, mean)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2025_1b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2025_1b_question.png",
    "statistics/images/past_exams/exam_p1_2025_1b_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2025_1b_answer.png",
]}

past_exams["exam_p1_2025_2a"] = {
"title": "P1-2025 Ex2a — SE of sample proportion (Shares=low vs high)",
"is_exam": True, "topic_hint": "G11",
"content": (
    '<span class="exam-question-text">For `Shares=high` ($\\hat p = 0.32$, $n = 550$) and `Shares=low` ($\\hat p = 0.173$, $n = 550$), compute the **standard errors** of the two sample proportions.</span>\n\n'
    '![Ex 2a question — SE of sample proportion (Shares high vs low)](statistics/images/past_exams/questions/exam_p1_2025_2a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.**\n\n'
    'The standard error of a sample proportion under simple random sampling is\n\n'
    '$$SE(\\hat p) \\;=\\; \\sqrt{\\dfrac{\\hat p(1-\\hat p)}{n}}.$$\n\n'
    'It measures the *typical sampling variability* of $\\hat p$ around the unknown true proportion $p$ — i.e. how much $\\hat p$ would jiggle from sample to sample of size $n$.\n\n'
    '**Two key drivers** of the size of $SE(\\hat p)$:\n\n'
    '- **Sample size $n$** — bigger $n$ shrinks SE at rate $1/\\sqrt n$.\n'
    '- **Bernoulli variance $\\hat p(1-\\hat p)$** — maximal at $\\hat p = 0.5$, decreasing as $\\hat p$ moves toward 0 or 1. So extreme proportions are estimated *more precisely* for fixed $n$.\n\n'
    'Here $n = 550$ is the same in both groups, so the SE gap is driven entirely by the Bernoulli variance term:\n\n'
    '- **Shares=high:** $\\hat p (1-\\hat p) = 0.32 \\cdot 0.68 = 0.2176$ → $SE = \\sqrt{0.2176/550} = \\sqrt{0.000396} \\approx \\mathbf{0.0199}$.\n'
    '- **Shares=low:** $\\hat p (1-\\hat p) = 0.173 \\cdot 0.827 = 0.1431$ → $SE = \\sqrt{0.1431/550} = \\sqrt{0.000260} \\approx \\mathbf{0.0161}$.\n\n'
    '$\\hat p_{\\text{low}}$ is **further from 0.5**, so its Bernoulli variance is smaller and the SE is correspondingly lower (~0.016 vs ~0.020).\n\n'
    '![Ex 2a AI walkthrough — Bernoulli variance drives SE gap at fixed n](statistics/images/past_exams/exam_p1_2025_2a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $SE(\\hat p_{\\text{high}}) = \\sqrt{0.32(1-0.32)/550} \\approx \\mathbf{0.020}$ and $SE(\\hat p_{\\text{low}}) = \\sqrt{0.173(1-0.173)/550} \\approx \\mathbf{0.016}$. The SE for **Shares=low is smaller** because $\\hat p = 0.173$ is further from 0.5, so $\\hat p(1-\\hat p)$ is smaller (with $n$ fixed at 550).\n\n'
    '**R commands:**\n\n'
    '`sqrt(0.32*(1-0.32)/550)`\n\n'
    '`## [1] 0.01989472`\n\n'
    '`sqrt(0.173*(1-0.173)/550)`\n\n'
    '`## [1] 0.01613283`\n\n'
    '---\n\n'
    '**Reference answer.**\n\n'
    '![Ex 2a answer](statistics/images/past_exams/answers/exam_p1_2025_2a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2025_2a_question.png",
    "statistics/images/past_exams/exam_p1_2025_2a_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2025_2a_answer.png",
]}

past_exams["exam_p1_2025_2b"] = {
"title": "P1-2025 Ex2b — Misinterpretation of SE",
"is_exam": True, "topic_hint": "G11",
"content": (
    '<span class="exam-question-text">Can we conclude from a lower SE that the *specific* estimate is closer to the parameter? Justify.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2025_2b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** The SE is a property of the **estimator** (a random variable across hypothetical repeated samples), not of one **realised** estimate (the single number you already observed). A smaller SE means that, *on average over repeated sampling*, the estimator scatters less tightly around the unknown parameter $\\theta$. It says nothing about the actual gap $|\\hat\\theta_{\\text{obs}} - \\theta|$ for the particular sample you drew — that gap is unknown precisely because $\\theta$ is unknown. The left panel below simulates 2000 sample means with $n=50$: the histogram has spread $\\approx 1/\\sqrt{50} \\approx 0.141$ (the SE), but any one realised $\\bar X$ (gold line) lands somewhere inside that cloud and may be near or far from $\\mu = 0$ by chance. The right panel shows two estimators where A has the **lower** SE yet its realised draw is **farther** from $\\mu$ than B\'s — proof that "lower SE" does not translate into "closer realised estimate".\n\n'
    '![AI walkthrough — SE describes the estimator, not a single realisation](statistics/images/past_exams/exam_p1_2025_2b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** **No.** A lower SE means that estimates produced by this estimator are more **tightly clustered** around the unknown parameter *on average across repeated samples*. It does **not** measure the distance of any *specific* realised estimate from the parameter — that distance is unknowable (since $\\theta$ is unknown) and is a fixed number once the sample is drawn, not a random one. The SE only constrains the long-run sampling behaviour of the estimator, so no conclusion can be drawn about the accuracy of a single observed estimate.\n\n'
    '**R commands:**\n\n'
    '`set.seed(1); sims <- replicate(2000, mean(rnorm(50, mean=0, sd=1)))`\n\n'
    '`sd(sims)         # empirical SE of x-bar`\n\n'
    '`## [1] 0.1391`\n\n'
    '`1/sqrt(50)       # theoretical SE`\n\n'
    '`## [1] 0.1414`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2025_2b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2025_2b_question.png",
    "statistics/images/past_exams/exam_p1_2025_2b_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2025_2b_answer.png",
]}

past_exams["exam_p1_2025_1c"] = {
"title": "P1-2025 Ex1b — Anomalously good Engagement threshold (Tukey vs 95th quantile)",
"is_exam": True, "topic_hint": "G7",
"content": (
    '<span class="exam-question-text">On performing the analysis with respect to the users\' `Engagement`, the company is interested in identifying if a post has an anomalously good performance. Detect the threshold above which the number of followers (`Engagement`) can be considered an outlier.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2025_1c_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Two complementary thresholds mark "anomalously good":\n\n'
    '- The **Tukey upper fence** $Q_3 + 1.5\\,IQR$ — anything above is an *outlier* on the boxplot.\n'
    '- The **95th percentile** $q_{0.95}$ — the top 5% of posts (extreme upper quantile), a stronger, distribution-based criterion.\n\n'
    'From `distr.summary.x(Engagement, stats=c("min","q1","median","mean","q3","p90","p95","p99"))` on `Metrics2`:\n\n'
    '$$Q_1 = 2.735,\\quad Q_3 = 5.700,\\quad IQR = Q_3 - Q_1 = 2.965,\\quad q_{0.95} = 13.19.$$\n\n'
    'So the two candidate thresholds are\n\n'
    '$$\\text{Tukey fence} = Q_3 + 1.5\\,IQR = 5.70 + 1.5(2.965) \\approx \\mathbf{10.15},$$\n\n'
    '$$q_{0.95} \\approx \\mathbf{13.19}.$$\n\n'
    'Because `Engagement` is **strongly right-skewed with many upper outliers** on the boxplot, the Tukey fence flags a very large group (>5% of posts). The company asks for *anomalously good* performance — a rarer, more selective label — so the **95th-percentile threshold (~13.19)** is the more informative cutoff: only the top 5% of posts qualify.\n\n'
    '![AI walkthrough — Engagement density with Tukey fence and 95th percentile](statistics/images/past_exams/exam_p1_2025_1c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** The threshold that marks *anomalously good* Engagement is the **95th percentile ≈ 13.19** — posts above this value are in the top 5% and can reasonably be called anomalous. Using the Tukey rule instead, the outlier cutoff is $Q_3 + 1.5\\,IQR \\approx \\mathbf{10.15}$, which flags a somewhat larger set but is the criterion consistent with the boxplot.\n\n'
    '**R commands:**\n\n'
    '`distr.summary.x(Engagement, stats=c("min","q1","median","mean","q3","IQR","p90","p95","p99"), data=Metrics2)`\n\n'
    '`## Q1 = 2.735   Q3 = 5.700   IQR = 2.965`\n\n'
    '`## p90 = 8.768  p95 = 13.19  p99 = 20.43`\n\n'
    '`## Tukey upper fence Q3 + 1.5*IQR = 10.15`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2025_1c_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2025_1c_question.png",
    "statistics/images/past_exams/exam_p1_2025_1c_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2025_1c_answer.png",
]}

past_exams["exam_p1_2025_1d"] = {
"title": "P1-2025 Ex1c — Reach vs Engagement: scatter and Pearson correlation",
"is_exam": True, "topic_hint": "G10",
"content": (
    '<span class="exam-question-text">The linear relation between the variables `Reach` (number of followers, in hundreds) and `Engagement` cannot be identified with a clear linear relationship by looking at the scatter plot; but we cannot conclude that they are not linearly related. From the scatterplot, we do identify a **positive linear** trend between `Reach` and `Engagement`. Interpret the correlation coefficient.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2025_1d_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** With two quantitative variables (`Reach`, `Engagement`), assess **linearity** with a **scatter plot** and quantify strength/direction with **Pearson\'s correlation** $r$. From the scatter the cloud slopes upward — as `Reach` increases, `Engagement` tends to increase — but with a lot of vertical spread (many low-Reach posts still show high Engagement, and vice-versa). Compute\n\n'
    '$$r \\;=\\; \\dfrac{\\operatorname{Cov}(\\text{Reach}, \\text{Engagement})}{s_{\\text{Reach}}\\,s_{\\text{Engagement}}} \\in [-1, 1].$$\n\n'
    'For `Metrics2`, `cor(Reach, Engagement)` = **0.7324** — a **strong positive** linear association.\n\n'
    '![AI walkthrough — Reach vs Engagement scatter with OLS fit](statistics/images/past_exams/exam_p1_2025_1d_ai.png)\n\n'
    '---\n\n'
    '**Answer.** The **Pearson correlation** between `Reach` and `Engagement` is $r \\approx \\mathbf{0.73}$, indicating a **strong positive linear relationship**: posts with a larger audience (`Reach`) tend to attract more followers (`Engagement`). The scatter is *not* a tight straight line — the cloud is wide, and any given `Reach` value corresponds to a range of `Engagement` values — but the overall linear trend is clear and its direction is positive. So while individual predictions are noisy, on average higher reach is associated with higher engagement.\n\n'
    '**R commands:**\n\n'
    "`distr.plot.xy(x=Reach, y=Engagement, plot.type='scatter', fitline=T, data=Metrics2)`\n\n"
    '`cor(Metrics2$Engagement, Metrics2$Reach)`\n\n'
    '`## [1] 0.7323963`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2025_1d_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2025_1d_question.png",
    "statistics/images/past_exams/exam_p1_2025_1d_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2025_1d_answer.png",
]}

past_exams["exam_p1_2025_1e"] = {
"title": "P1-2025 Ex1d — Shares × Content two-way table (offers vs nobrand success)",
"is_exam": True, "topic_hint": "G9",
"content": (
    '<span class="exam-question-text">The variable `Shares` (with levels `verylow`, `low`, `high`, `veryhigh`) measures the success (in relative terms) of posts based on the shares from users who do not follow the company\'s page. Consider the sentence *"Posts promoting special offers (Content=offers) are more successful than posts promoting products without mentioning the brand (Content=nobrand), since they are characterized by a higher proportion of the levels high or very high of the variable Shares"*. Explain if the sentence is true or false. Report your reasoning and the proportions supporting your answer.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2025_1e_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** To compare the *success profile* between `Content=offers` and `Content=nobrand` we need the **conditional distribution of Shares given Content** — i.e. **row proportions** of the two-way table (each row sums to 1). We then read off the proportion of "successful" posts, defined here as $\\Pr(\\text{Shares} \\in \\{\\text{high, veryhigh}\\} \\mid \\text{Content})$.\n\n'
    'From `Metrics2` the two-way table of *counts* (row = Content) is\n\n'
    '| Content \\ Shares | high | low | veryhigh | verylow | TOTAL |\n'
    '|---|---:|---:|---:|---:|---:|\n'
    '| offers  | 29 | 80 | 99 | 55 | 263 |\n'
    '| brand   | 61 |  5 | 15 | 37 | 118 |\n'
    '| nobrand | 86 | 10 | 27 | 46 | 169 |\n\n'
    'Row proportions:\n\n'
    '| Content \\ Shares | high | low | veryhigh | verylow |\n'
    '|---|---:|---:|---:|---:|\n'
    '| offers  | 0.11 | 0.30 | 0.38 | 0.21 |\n'
    '| brand   | 0.52 | 0.04 | 0.13 | 0.31 |\n'
    '| nobrand | 0.51 | 0.06 | 0.16 | 0.27 |\n\n'
    'So\n\n'
    '$$\\Pr(\\text{high} \\cup \\text{veryhigh} \\mid \\text{Content}=\\text{offers}) = 0.11 + 0.38 = \\mathbf{0.49},$$\n\n'
    '$$\\Pr(\\text{high} \\cup \\text{veryhigh} \\mid \\text{Content}=\\text{nobrand}) = 0.51 + 0.16 = \\mathbf{0.67}.$$\n\n'
    '![AI walkthrough — row-proportion bar chart of Shares by Content](statistics/images/past_exams/exam_p1_2025_1e_ai.png)\n\n'
    '---\n\n'
    '**Answer. The sentence is FALSE.** Among posts *not* referring to the brand (`Content=nobrand`), **67%** are successful (`Shares` high or veryhigh), whereas among posts promoting special offers (`Content=offers`) only **49%** are — clearly *lower*, not higher. So the direction claimed in the sentence is reversed: `nobrand` posts have the higher proportion of high/veryhigh shares.\n\n'
    '**R commands:**\n\n'
    "`distr.table.xy(x=Content, y=Shares, freq=c('counts'), data=Metrics2)`\n\n"
    "`distr.table.xy(x=Content, y=Shares, freq=c('prop'),   data=Metrics2)`\n\n"
    '`## offers : high=0.11  veryhigh=0.38  -> high+veryhigh = 0.49`\n\n'
    '`## nobrand: high=0.51  veryhigh=0.16  -> high+veryhigh = 0.67`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2025_1e_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2025_1e_question.png",
    "statistics/images/past_exams/exam_p1_2025_1e_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2025_1e_answer.png",
]}

past_exams["exam_p1_2025_1f"] = {
"title": "P1-2025 Ex1e — Grouped mean & variance of Out.Engage (approximation via midpoints)",
"is_exam": True, "topic_hint": "G6",
"content": (
    '<span class="exam-question-text">The index `Out.Engage` (in classes) measures the posts\' quality with reference to the engagement of users who are not followers of the company\'s page. It is possible to evaluate the **mean** and the **variance** of the index for the considered posts based on the available data? Explain whether it is possible or not and, if possible, provide the two summary measures clarifying the procedure.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2025_1f_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** The raw variable is available only in **grouped form** (5 classes):\n\n'
    '| Interval | $n_i$ | mid $m_i$ |\n'
    '|---|---:|---:|\n'
    '| $[0,1)$ | 110 | 0.5 |\n'
    '| $[1,5)$ | 231 | 3 |\n'
    '| $[5,10)$ | 88 | 7.5 |\n'
    '| $[10,50)$ | 110 | 30 |\n'
    '| $[50,200]$ | 11 | 125 |\n'
    '| **TOTAL** | **550** | — |\n\n'
    'The mean and variance can be **approximated** (not exactly computed) by assuming a **uniform distribution within each interval** and replacing every observation in class $i$ by its **midpoint** $m_i$. This is the standard grouped-data formula:\n\n'
    '$$\\bar x \\;\\approx\\; \\dfrac{1}{N}\\sum_{i} n_i\\,m_i, \\qquad s^2 \\;\\approx\\; \\dfrac{1}{N}\\sum_{i} n_i\\,m_i^2 \\;-\\; \\bar x^{\\,2}.$$\n\n'
    'Plugging in:\n\n'
    '$$\\bar x \\;\\approx\\; \\dfrac{110\\cdot 0.5 + 231\\cdot 3 + 88\\cdot 7.5 + 110\\cdot 30 + 11\\cdot 125}{550} = \\dfrac{6083}{550} \\approx \\mathbf{11.06}.$$\n\n'
    '$$\\dfrac{1}{N}\\sum n_i m_i^2 = \\dfrac{110(0.25)+231(9)+88(56.25)+110(900)+11(15625)}{550} \\approx 505.33,$$\n\n'
    '$$s^2 \\;\\approx\\; 505.33 - (11.06)^2 \\approx \\mathbf{383.01}, \\quad s \\approx \\mathbf{19.57}.$$\n\n'
    '![AI walkthrough — grouped-data midpoint approximation](statistics/images/past_exams/exam_p1_2025_1f_ai.png)\n\n'
    '---\n\n'
    '**Answer.** The exact mean and variance **cannot** be computed because the raw values of `Out.Engage` are lost — only the class counts are given. They can, however, be **approximated** under the assumption of *uniform distribution within each interval*, i.e. replacing each observation by its class midpoint. Using this approximation:\n\n'
    '$$\\bar x \\;\\approx\\; \\mathbf{11.06}, \\qquad s^2 \\;\\approx\\; \\mathbf{383.01} \\;(s \\approx 19.57).$$\n\n'
    '**R commands:**\n\n'
    "`distr.table.x(Out.Engage, interval=T, freq=c('count','prop'), data=Metrics2)`\n\n"
    "`## counts: [0,1)=110  [1,5)=231  [5,10)=88  [10,50)=110  [50,200]=11  (N=550)`\n\n"
    '`mids <- c(0.5, 3, 7.5, 30, 125); n <- c(110,231,88,110,11)`\n\n'
    '`m <- sum(n*mids)/sum(n); v <- sum(n*mids^2)/sum(n) - m^2; c(mean=m, var=v, sd=sqrt(v))`\n\n'
    '`##  mean       var        sd`\n\n'
    '`##  11.06000  383.00640  19.57055`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2025_1f_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2025_1f_question.png",
    "statistics/images/past_exams/exam_p1_2025_1f_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2025_1f_answer.png",
]}

past_exams["exam_p1_2025_1g"] = {
"title": "P1-2025 Ex1f — CLT probability P(X̄ > 15) for next 80 posts",
"is_exam": True, "topic_hint": "G13",
"content": (
    '<span class="exam-question-text">Based on past experience, the company evaluates that the quality index of posts has a **mean equal to 12** and a **variance equal to 380**. It is possible to calculate the probability that the average index of the next 80 posts will be higher than 15, i.e. $\\Pr(\\bar X > 15)$? Explain whether and why it is possible or not. If possible, provide the requested probability reporting the functions in RStudio used to obtain it.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2025_1g_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Population parameters $\\mu = 12$ and $\\sigma^2 = 380$ are known; we want $\\Pr(\\bar X_{80} > 15)$. Because the next $n=80$ posts can be treated as an i.i.d. random sample and $n$ is *large*, the **Central Limit Theorem (CLT)** applies:\n\n'
    '$$\\bar X_n \\;\\dot\\sim\\; \\mathcal{N}\\!\\left(\\mu,\\; \\dfrac{\\sigma^2}{n}\\right) \\;=\\; \\mathcal{N}\\!\\left(12,\\; \\dfrac{380}{80}\\right) = \\mathcal{N}(12, 4.75).$$\n\n'
    'The standard error of $\\bar X$ is $SE = \\sqrt{380/80} = \\sqrt{4.75} \\approx 2.179$. Standardise:\n\n'
    '$$Z \\;=\\; \\dfrac{\\bar X - \\mu}{SE} \\;=\\; \\dfrac{15 - 12}{2.179} \\approx 1.376.$$\n\n'
    'Then\n\n'
    '$$\\Pr(\\bar X > 15) \\;=\\; \\Pr(Z > 1.376) \\;=\\; 1 - \\Phi(1.376) \\approx \\mathbf{0.0843}.$$\n\n'
    '![AI walkthrough — sampling distribution of X-bar with tail P(Xbar>15)](statistics/images/past_exams/exam_p1_2025_1g_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Yes — with $n = 80$ (large) and known $\\mu = 12$, $\\sigma^2 = 380$, the CLT gives $\\bar X \\;\\dot\\sim\\; \\mathcal{N}(12,\\; 380/80)$. Standardising, $z = (15-12)/\\sqrt{380/80} \\approx 1.376$, so\n\n'
    '$$\\Pr(\\bar X > 15) \\;\\approx\\; 1 - \\Phi(1.376) \\;\\approx\\; \\mathbf{0.084}\\; (\\text{about } 8\\%).$$\n\n'
    '**R commands:**\n\n'
    '`1 - pnorm(15, mean=12, sd=sqrt(380/80))`\n\n'
    '`## [1] 0.08433431`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2025_1g_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2025_1g_question.png",
    "statistics/images/past_exams/exam_p1_2025_1g_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2025_1g_answer.png",
]}

past_exams["exam_p1_2025_2c"] = {
"title": "P1-2025 Ex2a — Estimator for a proportion p and unbiasedness",
"is_exam": True, "topic_hint": "G11",
"content": (
    '<span class="exam-question-text">Indicate what estimator is used for the proportion of successes in a population. Provide the analytic expression of the estimator and explain briefly why it is unbiased or not.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2025_2c_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** For a Bernoulli population with success probability $p$, draw an i.i.d. sample $X_1, \\dots, X_n$ with $X_i \\in \\{0,1\\}$ and $\\Pr(X_i=1) = p$. The estimator used for $p$ is the **sample proportion**, which is just the sample mean of the Bernoulli indicators:\n\n'
    '$$\\hat p \\;=\\; \\bar X \\;=\\; \\dfrac{X_1 + X_2 + \\cdots + X_n}{n}.$$\n\n'
    'Unbiasedness check: an estimator $\\hat\\theta$ is unbiased for $\\theta$ iff $\\mathbb{E}[\\hat\\theta] = \\theta$. Here, using $\\mathbb{E}[X_i] = p$ and linearity of expectation,\n\n'
    '$$\\mathbb{E}[\\hat p] \\;=\\; \\mathbb{E}\\!\\left[\\dfrac{1}{n}\\sum_{i=1}^n X_i\\right] \\;=\\; \\dfrac{1}{n}\\sum_{i=1}^n \\mathbb{E}[X_i] \\;=\\; \\dfrac{1}{n}\\,(np) \\;=\\; p.$$\n\n'
    'So $\\mathbb{E}[\\hat p] = p$ for every sample size $n \\ge 1$ — $\\hat p$ is **unbiased**.\n\n'
    '![AI walkthrough — sampling distribution of p-hat is centred on p](statistics/images/past_exams/exam_p1_2025_2c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** The estimator is the **sample proportion**\n\n'
    '$$\\hat p \\;=\\; \\dfrac{X_1 + X_2 + \\cdots + X_n}{n} \\;=\\; \\bar X.$$\n\n'
    'It is **unbiased**: $\\mathbb{E}[\\hat p] = \\tfrac{1}{n}\\sum \\mathbb{E}[X_i] = \\tfrac{1}{n}(np) = p$, i.e. on average across repeated samples $\\hat p$ equals the true population proportion $p$.\n\n'
    '**R commands:**\n\n'
    '`set.seed(1); sims <- replicate(5000, mean(rbinom(n=200, size=1, prob=0.3)))`\n\n'
    '`mean(sims)   # empirical E[phat] -> ~0.30`\n\n'
    '`## [1] 0.2999`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2025_2c_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2025_2c_question.png",
    "statistics/images/past_exams/exam_p1_2025_2c_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2025_2c_answer.png",
]}

# =================== 1st PARTIAL 2026 ===================

past_exams["exam_p1_2026_1a"] = {
"title": "P1-2026 Ex1a — Boxplots of Bid by Channel (Bidding)",
"is_exam": True, "topic_hint": "G6",
"content": (
    '<span class="exam-question-text">Use side-by-side boxplots of `Bid` by `Channel` (Aggregator / Agency / Airline) in the `Bidding` dataset to compare the bid distributions across the three channels (shape, spread, position, outliers).</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2026_1a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** With a continuous variable (`Bid`) split by a categorical (`Channel`), side-by-side **boxplots** are the right tool: they show centre (median line), spread (IQR box and whiskers), shape (symmetry from median position inside the box; skew from asymmetric whisker lengths) and outliers (points beyond the Tukey fences $Q_1-1.5\\,IQR$ and $Q_3+1.5\\,IQR$) in one view, making cross-channel comparison easy.\n\n'
    '![AI walkthrough — boxplots with shape/spread/position/outliers callouts](statistics/images/past_exams/exam_p1_2026_1a_ai.png)\n\n'
    '---\n\n'
    '**Answer.**\n\n'
    '**Shape.** `Airline` is strongly **left-skewed** — long lower whisker plus a column of low-side outliers pulls the lower tail down. `Agency` and `Aggregator` are roughly **symmetric** (median sits near the middle of the box); `Agency` still shows outliers on both tails.\n\n'
    '**Spread (IQR & range).** `Agency` has the **smallest IQR** (tightest box). `Aggregator` has the **smallest total range** (shortest whisker-to-whisker extent). `Airline` is the most dispersed once outliers are counted.\n\n'
    '**Position.** `Agency` is **shifted down**: its median and even its **Q3 sit below the Q1** of both `Aggregator` and `Airline`, so a typical Agency bid is lower than the bottom 25% of bids on the other two channels. `Aggregator` and `Airline` have nearly identical medians (~55).\n\n'
    '**R commands:**\n\n'
    "`distr.plot.xy(x=Bid, y=Channel, data=Bidding, plot.type='boxplot')`\n\n"
    "`distr.summary.xy(Bid, Channel, stats=c('fivenumber','IQR'), data=Bidding)`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_p1_2026_1a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2026_1a_question.png",
    "statistics/images/past_exams/exam_p1_2026_1a_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2026_1a_answer.png",
]}

past_exams["exam_p1_2026_1b"] = {
"title": "P1-2026 Ex1b — Central tendency: median for skewed Airline",
"is_exam": True, "topic_hint": "G4",
"content": (
    '<span class="exam-question-text">Which measure of central tendency would you use to summarize the three `Bid` distributions across `Channel`s (Agency, Aggregator, Airline)?</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2026_1b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Pick mean vs median **by shape**: when a distribution is roughly **symmetric**, mean and median coincide and the mean is preferred (uses all the data, smaller variance). When there is **skew or outliers**, the mean is pulled toward the tail and stops representing the *typical* value — the **median** (robust, the middle 50% anchor) is the right summary. Read the shape from the Ex1a boxplots: position of the median inside the box, whisker symmetry, presence of fliers.\n\n'
    '![AI walkthrough — mean vs median per Channel; Airline median > mean](statistics/images/past_exams/exam_p1_2026_1b_ai.png)\n\n'
    '---\n\n'
    '**Answer.**\n\n'
    '- **Agency**: roughly **symmetric** → **mean** (median is equally fine; both nearly coincide).\n'
    '- **Aggregator**: fairly **symmetric** → **mean** is appropriate.\n'
    '- **Airline**: strong **left skew** with a long lower tail / low outliers → **median**, because the mean is dragged down by the tail and misrepresents the typical bid.\n\n'
    'Good practice: **report median alongside mean for all three**, so the reader sees both the typical value and the effect of skew / outliers.\n\n'
    '**R commands:**\n\n'
    "`distr.summary.xy(Bid, Channel, stats=c('mean','median'), data=Bidding)`\n\n"
    "`distr.plot.xy(x=Bid, y=Channel, plot.type='boxplot', data=Bidding)`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_p1_2026_1b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2026_1b_question.png",
    "statistics/images/past_exams/exam_p1_2026_1b_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2026_1b_answer.png",
]}

past_exams["exam_p1_2026_1c"] = {
"title": "P1-2026 Ex1c — Is Bid=35 by Aggregator extremely low? (Tukey rule)",
"is_exam": True, "topic_hint": "G3",
"content": (
    '<span class="exam-question-text">Can a bid of 35 by a Channel = Aggregator customer be considered **extremely low**? Use Tukey\'s rule on the Aggregator subgroup (from the per-channel summary: $Q_1 = 50.8225$, $Q_3 = 62.7175$, $IQR = 11.895$).</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2026_1c_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Tukey\'s rule flags a point as an **extreme low** outlier when it sits **below the lower fence** $L = Q_1 - 1.5\\,IQR$. Procedure: (1) compute $IQR$ inside the Aggregator subgroup only — never mix channels; (2) build $L$; (3) compare the candidate value 35 against $L$. If $35 < L$ it is an extreme low; otherwise it is just inside the lower whisker.\n\n'
    '![AI walkthrough — Tukey lower-fence check for Aggregator Bid = 35](statistics/images/past_exams/exam_p1_2026_1c_ai.png)\n\n'
    '---\n\n'
    '**Answer.**\n\n'
    '$L = Q_1 - 1.5\\cdot IQR = 50.8225 - 1.5\\cdot 11.895 = 50.8225 - 17.8425 = \\mathbf{32.98}$.\n\n'
    'Since $35 > 32.98$, the bid of 35 is **NOT extremely low** — it lies inside the lower whisker (low but not in outlier territory).\n\n'
    '**R commands:**\n\n'
    "`Q1 <- 50.8225; Q3 <- 62.7175; IQR <- Q3 - Q1`\n\n"
    "`lower_fence <- Q1 - 1.5 * IQR   # 32.98`\n\n"
    "`35 > lower_fence   # TRUE -> NOT an extreme low`\n\n"
    "`distr.summary.xy(Bid, Channel, stats=c('Q1','Q3','IQR'), data=Bidding)`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_p1_2026_1c_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2026_1c_question.png",
    "statistics/images/past_exams/exam_p1_2026_1c_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2026_1c_answer.png",
]}

# =================== GENERAL 1 2024 ===================

past_exams["exam_g1_2024_1a"] = {
"title": "G1-2024 Ex1.a — Boxplot of Read2 by Lunch (free vs not-free)",
"is_exam": True, "topic_hint": "G3",
"content": (
    '<span class="exam-question-text">Propose a graphical representation that effectively describes the possible differences between the distributions of the reading scores (**Read2**) of students qualified or not for free lunch (**Lunch**). Report a sketch of the graph: what conclusions can you draw about the differences in the reading abilities of more or less disadvantaged pupils?</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2024_1a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Comparing a continuous variable (*Read2*) across the levels of a categorical one (*Lunch* = free / not-free) is the textbook use-case for **side-by-side boxplots**: each box shows median, $Q_1$, $Q_3$, IQR and whiskers, so one can read off centre, spread and tail behaviour for the two groups on the same scale. If the two boxes overlap a lot, the distributions are similar; if one box sits clearly above the other, that group has systematically higher values.\n\n'
    '![AI walkthrough — Read2 by Lunch boxplots with quartile annotations](statistics/images/past_exams/exam_g1_2024_1a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Side-by-side **boxplots** of *Read2* split by *Lunch* level (`free` / `not-free`). The boxplot is the right tool: it summarises centre, spread and tails of a continuous variable across the levels of a categorical one.\n\n'
    '**Reading the plot.** The range of *Read2* is similar across the two Lunch groups, but ignoring extreme values the dispersion of *not-free* (i.e. **not** qualified for free lunch) is clearly smaller, and both groups look roughly symmetric. The whole *not-free* box sits above the *free* box: the **median, $Q_1$ and $Q_3$ of not-free are above the corresponding quartiles of free**.\n\n'
    '**Conclusion.** Students NOT qualified for free lunch (less economically disadvantaged) perform systematically better in reading. In particular: 50% of *free*-lunch students score below the 25th percentile of *not-free* students, and 75% of *free* students score below the median of *not-free* students — a clear gap penalising more disadvantaged pupils.\n\n'
    '**R commands:**\n\n'
    '`distr.plot.xy(y=Read2, x=Lunch, plot.type="boxplot", data=Primary)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g1_2024_1a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2024_1a_question.png",
    "statistics/images/past_exams/exam_g1_2024_1a_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2024_1a_answer.png",
]}

past_exams["exam_g1_2024_1b"] = {
"title": "G1-2024 Ex1b — Sample size for proportion ME ≤ 0.04 at 95%",
"is_exam": True, "topic_hint": "G13",
"subtopic_hint": "g13b",
"content": (
    '<span class="exam-question-text">What sample size is needed so that the margin of error on a proportion is at most 0.04 at the 95% level?</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2024_1b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** For a proportion the (Wald) margin of error at confidence $1-\\alpha$ is $ME = z_{1-\\alpha/2}\\sqrt{\\hat p(1-\\hat p)/n}$. To **guarantee** $ME \\le m$ regardless of the unknown true $p$, use the **worst-case** value of $\\hat p(1-\\hat p)$, maximized at $\\hat p = 0.5$ giving $0.25$. Solving for $n$: $n \\ge (z_{1-\\alpha/2}/m)^2 \\cdot 0.25$, then round **up** to the next integer.\n\n'
    '![AI walkthrough — n vs ME with worst-case p = 0.5 at 95%](statistics/images/past_exams/exam_g1_2024_1b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $ME = z_{0.975}\\sqrt{\\hat p(1-\\hat p)/n} \\le 0.04$. Worst-case $\\hat p = 0.5 \\Rightarrow \\hat p(1-\\hat p) = 0.25$. So\n\n'
    '$$n \\ge \\left(\\frac{1.96}{0.04}\\right)^2 \\cdot 0.25 = 49^2 \\cdot 0.25 = 600.25 \\Rightarrow \\boxed{n \\ge 601}.$$\n\n'
    '**R commands:**\n\n'
    '`ceiling((qnorm(0.975)/0.04)^2 * 0.25)`\n\n'
    '`## [1] 601`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g1_2024_1b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2024_1b_question.png",
    "statistics/images/past_exams/exam_g1_2024_1b_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2024_1b_answer.png",
]}

past_exams["exam_g1_2024_2a"] = {
"title": "G1-2024 Ex3 — Read2 vs Math2 correlation = 0.77",
"is_exam": True, "topic_hint": "G9",
"content": (
    '<span class="exam-question-text">Refer to the scatterplot of `PrimaryRead2` (reading score) vs `PrimaryMath2` (math score) in the `Primary` dataframe. The sample correlation is $r = 0.77$. Comment on the **direction, form and strength** of the relationship between the two variables, and report the share of variance explained by a simple linear model.</span>\n\n'
    '![Ex 2a question — Read2 vs Math2 scatter](statistics/images/past_exams/questions/exam_g1_2024_2a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.**\n\n'
    'Three things to check for any scatter: **direction, form, strength**.\n\n'
    '**Direction.** The cloud rises from lower-left to upper-right → **positive** association: pupils with higher reading scores tend to have higher math scores too.\n\n'
    '**Form.** Points fan around a single straight line — no clear curvature → **linear** form is appropriate. (The scatter does widen slightly at higher Read2 — that is the *heteroscedasticity* point picked up in 2b — but it does not change the linear *shape* of the trend.)\n\n'
    '**Strength.** Pearson $r = 0.77$ is large in absolute value. Conventional ranges: $|r| < 0.3$ weak, $0.3 \\le |r| < 0.7$ moderate, $|r| \\ge 0.7$ **strong**. So we have a **strong positive linear** association.\n\n'
    '**Shared variance.** A simple OLS line uses Pearson $r$ via $r^2$ — the coefficient of determination — to quantify the fraction of variance in math explained by reading: $r^2 = 0.77^2 = \\mathbf{0.5929}$, i.e. roughly **59%** of the variability in `PrimaryMath2` is linearly explained by `PrimaryRead2`; the remaining ~41% is residual variation (other skills, noise, measurement).\n\n'
    '![Ex 2a AI walkthrough — direction/form/strength + r² share](statistics/images/past_exams/exam_g1_2024_2a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Strong **positive linear** relationship between reading and math scores: higher reading scores associate with higher math scores. With $r = 0.77$, $r^2 = 0.59 \\Rightarrow$ a simple regression of Math2 on Read2 would explain about **59% of the variance** in math. The scatter looks like a straight rising band with moderate dispersion around it.\n\n'
    '**R commands:**\n\n'
    "`cor(PrimaryRead2, PrimaryMath2)         # 0.77`\n\n"
    "`cor(PrimaryRead2, PrimaryMath2)^2       # 0.5929 -> share of variance`\n\n"
    "`distr.plot.xy(x=PrimaryRead2, y=PrimaryMath2, plot.type='scatter', fitline=T, data=Primary)`\n\n"
    '![Ex 2a answer](statistics/images/past_exams/answers/exam_g1_2024_2a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2024_2a_question.png",
    "statistics/images/past_exams/exam_g1_2024_2a_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2024_2a_answer.png",
]}

past_exams["exam_g1_2024_2b"] = {
"title": "G1-2024 Ex3b — Heteroscedasticity in the Read2/Math2 scatter",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15e",
"content": (
    '<span class="exam-question-text">Refer again to the scatter of `PrimaryRead2` vs `PrimaryMath2` in the `Primary` dataframe (see Ex 2a). **Identify any violations of regression assumptions** that are visible from the scatterplot, name the violation, explain why it matters for inference, and indicate how you would fix it.</span>\n\n'
    '![Ex 2b question — diagnose the scatter](statistics/images/past_exams/questions/exam_g1_2024_2b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.**\n\n'
    'The simple linear regression $y_i = \\beta_0 + \\beta_1 x_i + \\varepsilon_i$ rests on four assumptions on $\\varepsilon_i$ (LINE): **L**inearity of the mean, **I**ndependence, **N**ormality, and **E**qual variance (a.k.a. homoscedasticity, $\\mathrm{Var}(\\varepsilon_i\\mid x_i)=\\sigma^2$ for all $i$).\n\n'
    'Reading the scatter (and confirming with a residuals-vs-fitted plot): the **mean** trend is well captured by a single straight rising line — linearity looks fine. What is *not* fine is the spread: the cloud of points sits in a **narrow band at low Read2 and a wide band at high Read2** — the residual dispersion grows with the predictor. That is the textbook picture of **heteroscedasticity** (a "fanning" or "megaphone" pattern).\n\n'
    'Why it matters: under heteroscedasticity, OLS coefficient estimates remain unbiased and consistent, *but* the usual formula $\\widehat{\\mathrm{Var}}(\\hat\\beta)=\\hat\\sigma^2 (X^\\top X)^{-1}$ is wrong, so **standard errors, $t$-statistics, $p$-values and CIs become unreliable**. Confidence statements (e.g. "Read2 has a significant effect on Math2") are no longer trustworthy at their nominal level.\n\n'
    'Fixes: (i) **Weighted Least Squares** with weights $\\propto 1/\\widehat{\\mathrm{Var}}(\\varepsilon_i\\mid x_i)$; (ii) a **variance-stabilising transform** of $y$ such as $\\log$ or $\\sqrt{\\,\\cdot\\,}$; (iii) keep OLS but report **heteroscedasticity-robust (HC / sandwich) standard errors**.\n\n'
    '![Ex 2b AI walkthrough — fanning scatter + residuals cone](statistics/images/past_exams/exam_g1_2024_2b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Visible **heteroscedasticity**: dispersion of points around the OLS line increases at higher Read2 scores (megaphone shape). The constant-error-variance assumption is violated → OLS standard errors and inference become unreliable while the coefficient estimates themselves remain unbiased. Remedies: weighted least squares, a $\\log$ / $\\sqrt{}$ transform of Math2, or robust (HC) standard errors.\n\n'
    '**R commands:**\n\n'
    "`mod <- lm(PrimaryMath2 ~ PrimaryRead2, data=Primary)`\n\n"
    "`plot(mod, which=1)   # Residuals vs Fitted — look for fanning`\n\n"
    "`plot(mod, which=3)   # Scale-Location — flat red line if homoscedastic`\n\n"
    "`lmtest::bptest(mod)  # Breusch-Pagan test for heteroscedasticity`\n\n"
    "`# Fix 1: robust (HC) SEs`\n\n"
    "`lmtest::coeftest(mod, vcov.=sandwich::vcovHC(mod, type='HC1'))`\n\n"
    "`# Fix 2: variance-stabilising transform`\n\n"
    "`mod_log <- lm(log(PrimaryMath2) ~ PrimaryRead2, data=Primary)`\n\n"
    '![Ex 2b answer](statistics/images/past_exams/answers/exam_g1_2024_2b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2024_2b_question.png",
    "statistics/images/past_exams/exam_g1_2024_2b_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2024_2b_answer.png",
]}

# ----- G1-2024 Ex1.a2 — 99% CI for difference in means Read2 (Lunch) -----
past_exams["exam_g1_2024_1a2"] = {
"title": "G1-2024 Ex1.a2 — 99% CI for difference in mean Read2 (free vs non-free)",
"is_exam": True, "topic_hint": "G13",
"content": (
    '<span class="exam-question-text">Obtain the **99% confidence interval for the difference between the means** of the reading scores (variable **Read2**) in the two groups of students (**Lunch**=free and **Lunch**=non-free), assuming that the variance in the two groups is the same. Report the interval and provide its interpretation.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** With normality (or $n$ large) and **equal variances**, the pooled two-sample $t$ interval for $\\mu_1-\\mu_2$ is\n\n'
    '$$\\bar X_1 - \\bar X_2 \\;\\pm\\; t_{1-\\alpha/2,\\,n_1+n_2-2}\\; s_p\\sqrt{\\tfrac{1}{n_1}+\\tfrac{1}{n_2}},\\qquad s_p^2=\\tfrac{(n_1-1)s_1^2+(n_2-1)s_2^2}{n_1+n_2-2}.$$\n\n'
    'With $n_1+n_2-2 = 830$ degrees of freedom the $t$ quantile $t_{0.995,830}\\approx z_{0.995}=2.576$, so the interval is essentially Gaussian.\n\n'
    '![AI walkthrough — pooled two-sample 99% CI for diff in mean Read2 by Lunch](statistics/images/past_exams/exam_g1_2024_1a2_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Sample sizes $n_{\\text{non-free}}=433$, $n_{\\text{free}}=399$; sample means $\\bar X_{\\text{non-free}}=603.61$, $\\bar X_{\\text{free}}=571.30$; difference $32.31$. The 99% pooled CI is\n\n'
    '$$\\mu_{\\text{non-free}} - \\mu_{\\text{free}} \\in [\\,24.47,\\;40.16\\,].$$\n\n'
    'The interval lies **entirely above 0**, so at the 99% confidence level the mean Read2 of students NOT qualified for free lunch is between **24.5 and 40.2 points higher** than that of free-lunch students — clear evidence of a reading-score gap penalising the more disadvantaged group.\n\n'
    '**R commands:**\n\n'
    "`CI.diffmean(x=Read2, by=Lunch, data=Primary, conf.level=0.99)`\n\n"
    "`t.test(Read2 ~ Lunch, data=Primary, var.equal=TRUE, conf.level=0.99)`\n\n"
    '`## t = 10.632, df = 830, p-value < 2.2e-16`\n\n'
    '`## 99 percent confidence interval:`\n\n'
    '`##  24.46699 40.16015`\n\n'
    '`## sample estimates:`\n\n'
    '`## mean in group non-free     mean in group free`\n\n'
    '`##              603.6143               571.3008`\n'
), "images": [
    "statistics/images/past_exams/exam_g1_2024_1a2_ai.png",
]}

# ----- G1-2024 Ex1.a3 — Conclude from interval whether diff != 0 -----
past_exams["exam_g1_2024_1a3"] = {
"title": "G1-2024 Ex1.a3 — Interpret 99% CI: is the difference significantly different from 0?",
"is_exam": True, "topic_hint": "G13",
"content": (
    '<span class="exam-question-text">**Based on the interval determined at the previous point**, can we conclude that the difference between the two means is significantly different from zero? Explain and motivate your answer.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** A $(1-\\alpha)$ CI for $\\mu_1-\\mu_2$ and a two-sided test of $H_0:\\mu_1=\\mu_2$ vs $H_1:\\mu_1\\ne\\mu_2$ at level $\\alpha$ are **equivalent**: reject $H_0$ iff the CI does **not** contain $0$.\n\n'
    'So we just check whether $0\\in[24.47,40.16]$.\n\n'
    '![AI walkthrough — CI vs zero: 0 outside [24.47, 40.16] -> reject H0 at 1%](statistics/images/past_exams/exam_g1_2024_1a3_ai.png)\n\n'
    '---\n\n'
    '**Answer.** The 99% interval is $[24.47,\\,40.16]$ and **does not contain $0$** (in fact $0$ is far to the left of the lower end). So at the **1% significance level we reject** $H_0:\\mu_{\\text{non-free}}-\\mu_{\\text{free}}=0$: the difference in mean Read2 is **significantly different from zero**, and in particular significantly **positive**. Equivalently the two-sample $t$ test gives $t = 10.63$, $p < 2.2\\times 10^{-16}$.\n\n'
    '**R commands:**\n\n'
    "`# Test of equivalence of the CI conclusion`\n\n"
    "`t.test(Read2 ~ Lunch, data=Primary, var.equal=TRUE, conf.level=0.99)`\n\n"
    '`## t = 10.632, df = 830, p-value < 2.2e-16   -> reject H0 at 1%`\n'
), "images": [
    "statistics/images/past_exams/exam_g1_2024_1a3_ai.png",
]}

# ----- G1-2024 Ex1.b_loc — Conditional distribution SchoolLoc | Lunch -----
past_exams["exam_g1_2024_1b_loc"] = {
"title": "G1-2024 Ex1.b — Distribution of SchoolLoc conditional on Lunch",
"is_exam": True, "topic_hint": "G2",
"content": (
    '<span class="exam-question-text">A statistical tendency measure proper for summarizing the distributions of the school\'s location (variable **SchoolLoc**) for students qualified or not for free lunch (variable **Lunch**). Report the result and comment.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** *SchoolLoc* is **nominal** (`inner-city`, `rural`, `suburban`, `urban`) — no order, no average — so the only sensible central-tendency measure is the **mode** of the conditional distribution. We tabulate `SchoolLoc` for each level of `Lunch`, switch to conditional relative frequencies (column percentages summing to 100% within each Lunch group), and compare modes.\n\n'
    '![AI walkthrough — stacked bar of SchoolLoc | Lunch with modes highlighted](statistics/images/past_exams/exam_g1_2024_1b_loc_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Conditional relative frequencies of *SchoolLoc* within each *Lunch* group:\n\n'
    '| SchoolLoc | non-free | free |\n'
    '|---|---:|---:|\n'
    '| inner-city | 4.85% | **44.61%** |\n'
    '| rural | **53.81%** | 36.59% |\n'
    '| suburban | 35.80% | 14.04% |\n'
    '| urban | 5.54% | 4.76% |\n\n'
    '**Modes.** Among **non-free**-lunch students the most frequent location is **rural** (53.8%); among **free**-lunch students the most frequent location is **inner-city** (44.6%). The two conditional distributions are clearly different, suggesting that *Lunch* (a proxy for family economic conditions) is **associated** with *SchoolLoc* — to be confirmed by a formal $\\chi^2$ test (Ex 1.b3).\n\n'
    '**R commands:**\n\n'
    '`distr.summary.x(x=SchoolLoc, by=Lunch, stats="central", data=Primary)`\n\n'
    '`tab <- table(SchoolLoc=Primary$SchoolLoc, Lunch=Primary$Lunch)`\n\n'
    '`round(prop.table(tab, margin=2), 4)`\n\n'
    '`##             Lunch`\n\n'
    '`## SchoolLoc    non-free   free`\n\n'
    '`##   inner-city   0.0485 0.4461`\n\n'
    '`##   rural        0.5381 0.3659`\n\n'
    '`##   suburban     0.3580 0.1404`\n\n'
    '`##   urban        0.0554 0.0476`\n'
), "images": [
    "statistics/images/past_exams/exam_g1_2024_1b_loc_ai.png",
]}

# ----- G1-2024 Ex1.b2 — Sample frequency non-free in rural vs suburban -----
past_exams["exam_g1_2024_1b2"] = {
"title": "G1-2024 Ex1.b2 — Is the proportion of non-free lunch higher in rural than suburban schools?",
"is_exam": True, "topic_hint": "G2",
"content": (
    '<span class="exam-question-text">Can you say that the observed **sample frequency** of students NOT qualified for free lunch (**Lunch**=non-free) among students in schools in rural areas (**SchoolLoc**=rural) is higher than among students in schools in suburban areas (**SchoolLoc**=suburban)? Report the appropriate frequencies and comment.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** The question asks for $P(\\text{Lunch}=\\text{non-free}\\mid \\text{SchoolLoc}=\\text{rural})$ vs $P(\\text{Lunch}=\\text{non-free}\\mid \\text{SchoolLoc}=\\text{suburban})$ — i.e. **row** percentages of the SchoolLoc × Lunch table conditioning on *SchoolLoc*. Compute the two proportions and compare them directly (no inference here — this is a descriptive sample-frequency comparison).\n\n'
    '![AI walkthrough — Lunch | SchoolLoc row proportions, rural vs suburban](statistics/images/past_exams/exam_g1_2024_1b2_ai.png)\n\n'
    '---\n\n'
    '**Answer.** From the contingency table:\n\n'
    '| SchoolLoc | non-free | total | $\\hat p_{\\text{non-free}|\\text{loc}}$ |\n'
    '|---|---:|---:|---:|\n'
    '| rural | 233 | 379 | $233/379 = 0.6148$ |\n'
    '| suburban | 155 | 211 | $155/211 = 0.7346$ |\n\n'
    'So **NO**: in this sample the proportion of non-free-lunch students is **lower** in rural schools (61.5%) than in suburban schools (73.5%). The statement in the question is **false** at the descriptive level — suburban schools have a higher share of non-free-lunch students.\n\n'
    '**R commands:**\n\n'
    '`distr.table.xy(x=Lunch, y=SchoolLoc, freq.type=c("x|y"), freq=c("counts","prop"), data=Primary)`\n\n'
    '`tab <- table(SchoolLoc=Primary$SchoolLoc, Lunch=Primary$Lunch); round(prop.table(tab, margin=1), 4)`\n\n'
    '`##             Lunch`\n\n'
    '`## SchoolLoc    non-free   free`\n\n'
    '`##   inner-city   0.1055 0.8945`\n\n'
    '`##   rural        0.6148 0.3852`\n\n'
    '`##   suburban     0.7346 0.2654`\n\n'
    '`##   urban        0.5581 0.4419`\n'
), "images": [
    "statistics/images/past_exams/exam_g1_2024_1b2_ai.png",
]}

# ----- G1-2024 Ex1.b3 — Chi-square test of independence SchoolLoc x Lunch -----
past_exams["exam_g1_2024_1b3"] = {
"title": "G1-2024 Ex1.b3 — Chi-square independence test SchoolLoc x Lunch",
"is_exam": True, "topic_hint": "G14", "subtopic_hint": "g14c",
"content": (
    '<span class="exam-question-text">Would you conclude that the **location of the school** (variable **SchoolLoc**) is **related to the economic situation** of students\' families (as measured by the variable **Lunch**)? Answer on the basis of a suitable statistical test, specifying the analytic expression of the statistic, its observed value, the rule of decision based upon and its realization. Report the p-value, specify how it is obtained, report its interpretation, and draw your motivated conclusions based on it.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** Two categorical variables, $H_0$: independence vs $H_1$: dependence. Pearson\'s $\\chi^2$ statistic\n\n'
    '$$X^2 = \\sum_{j=1}^{I}\\sum_{k=1}^{K}\\frac{(O_{jk}-E_{jk})^2}{E_{jk}},\\qquad E_{jk} = \\frac{R_j C_k}{n}.$$\n\n'
    'Under $H_0$ with large $n$, $X^2\\dot\\sim\\chi^2_{(I-1)(K-1)}$. With $I=4$ levels of *SchoolLoc* and $K=2$ levels of *Lunch*, $\\text{df}=(4-1)(2-1)=3$. Reject $H_0$ for large $X^2$ (equivalently small p-value).\n\n'
    '![AI walkthrough — chi-square density df=3 with observed X2=189.79 in the tail](statistics/images/past_exams/exam_g1_2024_1b3_ai.png)\n\n'
    '---\n\n'
    '**Answer.** From the contingency table (see Ex 1.b) the observed statistic is\n\n'
    '$$X^2_{\\text{obs}} = 189.79\\quad(\\text{df}=3),\\qquad p = P(\\chi^2_3 \\ge 189.79) < 2.2\\times 10^{-16} \\approx 0.$$\n\n'
    'The p-value is essentially zero, far below any reasonable level (e.g. $\\alpha=0.01$). **We reject $H_0$**: there is overwhelming evidence that *SchoolLoc* and *Lunch* are **not independent**. The school location is strongly related to the economic situation of the students\' families — consistent with the conditional distributions in Ex 1.b (free-lunch students concentrated in inner-city schools, non-free students concentrated in rural and suburban schools).\n\n'
    '**R commands:**\n\n'
    '`chisq.test(table(Primary$Lunch, Primary$SchoolLoc))`\n\n'
    '`## Pearson\'s Chi-squared test`\n\n'
    '`## X-squared = 189.79, df = 3, p-value < 2.2e-16`\n\n'
    '`1 - pchisq(189.79, df=3)   ## [1] 0`\n'
), "images": [
    "statistics/images/past_exams/exam_g1_2024_1b3_ai.png",
]}

# ----- G1-2024 Ex1.c — Multiple regression Read2 ~ Read1 + Sex + Lunch + SchoolLoc + Experience -----
past_exams["exam_g1_2024_1c"] = {
"title": "G1-2024 Ex1.c — Multiple regression Read2 ~ Read1+Sex+Lunch+SchoolLoc+Experience",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15c",
"content": (
    '<span class="exam-question-text">Consider the model for 2nd-grade students\' reading score (variable **Read2**) as a function of their 1st-grade reading score (variable **Read1**), their sex (variable **Sex**, assigned at birth), their qualification or not for free lunch (variable **Lunch**), their school\'s location (variable **SchoolLoc**), and the years of experience of their teacher (variable **Experience**). Write the **expression of the estimated model**, propose an index to assess its explanatory power, and provide an interpretation of the proposed index.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** Five predictors, two of which (*Sex*, *Lunch*, *SchoolLoc*) are factors → R automatically creates dummy variables, using the first level of each factor as the **baseline** (`male`, `non-free`, `inner-city`). Fit by OLS, then read off the estimated equation. Explanatory power is measured by the **coefficient of determination** $R^2$ — the fraction of variance in *Read2* explained by the model — and by the **adjusted** $R^2$ which penalises additional predictors.\n\n'
    '![AI walkthrough — coefficient bar chart with significance & R2 = 0.586](statistics/images/past_exams/exam_g1_2024_1c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Estimated model (baselines: $Sex=$ male, $Lunch=$ non-free, $SchoolLoc=$ inner-city):\n\n'
    '$$\\widehat{Read2} = 246.17 + 0.617\\cdot Read1 + 3.076\\cdot Sex_{\\text{female}} - 2.441\\cdot Lunch_{\\text{free}} + 16.278\\cdot SchoolLoc_{\\text{rural}} + 8.861\\cdot SchoolLoc_{\\text{suburban}} + 13.657\\cdot SchoolLoc_{\\text{urban}} + 0.423\\cdot Experience.$$\n\n'
    '**Explanatory power.** $R^2 = 0.586$ (adjusted $R^2 = 0.582$): the model **explains about 58.6%** of the variability of the 2nd-grade reading score. The remaining $\\approx 41\\%$ is residual variation (other pupil/family characteristics, noise, measurement). The fit is global-$F$ highly significant ($F_{7,824}=166.6$, $p<2.2\\times 10^{-16}$), so the model is far better than the null no-predictor model.\n\n'
    '**R commands:**\n\n'
    '`regr.A <- lm(Read2 ~ Read1 + Sex + Lunch + SchoolLoc + Experience, data=Primary)`\n\n'
    '`summary(regr.A)`\n\n'
    '`## Coefficients:`\n\n'
    '`##                    Estimate Std. Error t value Pr(>|t|)`\n\n'
    '`## (Intercept)        246.1655    12.3625  19.912  < 2e-16 ***`\n\n'
    '`## Read1                0.6170     0.0224  27.579  < 2e-16 ***`\n\n'
    '`## Sexfemale            3.0761     2.0992   1.465  0.14320`\n\n'
    '`## Lunchfree           -2.4412     2.4983  -0.977  0.32879`\n\n'
    '`## SchoolLocrural      16.2779     2.9795   5.463 6.19e-08 ***`\n\n'
    '`## SchoolLocsuburban    8.8609     3.3602   2.637  0.00852 **`\n\n'
    '`## SchoolLocurban      13.6573     5.2314   2.611  0.00920 **`\n\n'
    '`## Experience           0.4227     0.1227   3.446  0.00060 ***`\n\n'
    '`## Multiple R-squared:  0.586,   Adjusted R-squared:  0.5824`\n\n'
    '`## F-statistic: 166.6 on 7 and 824 DF, p-value: < 2.2e-16`\n'
), "images": [
    "statistics/images/past_exams/exam_g1_2024_1c_ai.png",
]}

# ----- G1-2024 Ex1.c2 — Effect of Experience on Read2 -----
past_exams["exam_g1_2024_1c2"] = {
"title": "G1-2024 Ex1.c2 — Effect of teacher Experience on Read2 (ceteris paribus)",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15c",
"content": (
    '<span class="exam-question-text">Based on the estimated model, what conclusions can you draw on the relation between the teacher\'s experience (**Experience**) and the 2nd-grade reading score (**Read2**)?</span>\n\n'
    '---\n\n'
    '**Walkthrough.** In a multiple regression each slope $\\hat\\beta_j$ measures the **partial / ceteris-paribus** effect of $X_j$ on the response, holding the other predictors fixed. We look at the estimated coefficient on *Experience*, at its $t$-statistic, at its $p$-value (vs $\\alpha=0.05$) and at the sign.\n\n'
    '![AI walkthrough — Experience slope with 95% CI and effect over realistic range](statistics/images/past_exams/exam_g1_2024_1c2_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $\\hat\\beta_{\\text{Experience}} = 0.4227$ with $\\text{SE}=0.1227$, $t = 3.446$, $p = 0.0006$. Highly significant ($p \\ll 0.05$).\n\n'
    'Interpretation (ceteris paribus, i.e. holding *Read1*, *Sex*, *Lunch*, *SchoolLoc* fixed): each **additional year of teacher experience is associated with an increase of about 0.42 points** in the 2nd-grade reading score, and this effect is statistically different from zero at the 1% level. So *Experience* contributes positively and significantly to *Read2*. (The size is modest: 10 extra years of experience $\\Rightarrow$ ~4.2 reading points.)\n\n'
    '**R commands:**\n\n'
    '`summary(regr.A)$coef["Experience", ]`\n\n'
    '`##     Estimate   Std. Error      t value     Pr(>|t|)`\n\n'
    '`## 0.4227010460 0.1226686530 3.4459786789 0.0005978489`\n\n'
    '`confint(regr.A, "Experience", level=0.95)`\n'
), "images": [
    "statistics/images/past_exams/exam_g1_2024_1c2_ai.png",
]}

# ----- G1-2024 Ex1.c3 — Effect of SchoolLoc on Read2 -----
past_exams["exam_g1_2024_1c3"] = {
"title": "G1-2024 Ex1.c3 — Effect of SchoolLoc on Read2 (rural/suburban/urban vs inner-city)",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15c",
"content": (
    '<span class="exam-question-text">How do you evaluate the contribution of the coefficients relative to the school\'s location (**SchoolLoc**)?</span>\n\n'
    '---\n\n'
    '**Walkthrough.** *SchoolLoc* is a 4-level factor; R encodes it with $K-1=3$ dummies and uses **inner-city** as the baseline. Each of the three dummy coefficients measures the **ceteris-paribus** difference in mean *Read2* between that level and the inner-city baseline. We check sign, magnitude and significance of each.\n\n'
    '![AI walkthrough — SchoolLoc dummies vs inner-city baseline with 95% CIs](statistics/images/past_exams/exam_g1_2024_1c3_ai.png)\n\n'
    '---\n\n'
    '**Answer.** All three SchoolLoc dummies are **positive and significant at the 1% level**:\n\n'
    '| level (vs inner-city) | $\\hat\\beta$ | SE | $t$ | $p$ |\n'
    '|---|---:|---:|---:|---:|\n'
    '| rural | $+16.28$ | $2.98$ | $5.46$ | $6.2\\times 10^{-8}$ |\n'
    '| suburban | $+8.86$ | $3.36$ | $2.64$ | $0.0085$ |\n'
    '| urban | $+13.66$ | $5.23$ | $2.61$ | $0.0092$ |\n\n'
    'So conditional on prior reading score, sex, free-lunch status and teacher experience, **students attending schools outside inner-city areas score on average higher in reading** than inner-city pupils. The largest gap is for **rural** schools (+16.3 points), followed by urban (+13.7) and suburban (+8.9). Inner-city schools are associated with the **worst** average reading performance, even after controlling for the other variables (so the gap is not just driven by the high concentration of free-lunch students there).\n\n'
    '**R commands:**\n\n'
    '`summary(regr.A)$coef[grep("SchoolLoc", rownames(summary(regr.A)$coef)), ]`\n\n'
    '`##                    Estimate Std. Error  t value     Pr(>|t|)`\n\n'
    '`## SchoolLocrural    16.27786    2.97948  5.46333 6.190e-08`\n\n'
    '`## SchoolLocsuburban  8.86094    3.36021  2.63702 8.521e-03`\n\n'
    '`## SchoolLocurban    13.65733    5.23144  2.61058 9.202e-03`\n'
), "images": [
    "statistics/images/past_exams/exam_g1_2024_1c3_ai.png",
]}

# ----- G1-2024 Ex1.d — Compare regression conclusion to a2-a3 (Lunch effect) -----
past_exams["exam_g1_2024_1d"] = {
"title": "G1-2024 Ex1.d — Lunch effect: simple CI/test vs multiple regression",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15c",
"content": (
    '<span class="exam-question-text">Compare the results obtained at points a2-a3 (concerning the difference in the **average** reading scores depending on **Lunch**) with the conclusions drawn based on the regression model with respect to the variable **Lunch**. How do you explain the observed differences?</span>\n\n'
    '---\n\n'
    '**Walkthrough.** The two analyses ask formally different questions about the role of *Lunch*:\n\n'
    '- **Ex 1.a2-a3** = a **marginal / unconditional** comparison of mean *Read2* across the two *Lunch* groups. It captures the *total* association, including everything correlated with *Lunch* (school location, prior reading score, etc.).\n\n'
    '- **Ex 1.c** = the **partial / ceteris-paribus** effect of *Lunch* on *Read2* **after** controlling for *Read1*, *Sex*, *SchoolLoc* and *Experience*. It isolates the residual effect of free-lunch status once differences in those confounders are accounted for.\n\n'
    '![AI walkthrough — marginal vs partial Lunch effect: ~32 pts -> ~ -2.4 pts (n.s.)](statistics/images/past_exams/exam_g1_2024_1d_ai.png)\n\n'
    '---\n\n'
    '**Answer.** **At point a2-a3** the 99% CI is $[24.47,\\,40.16]$ (interval far from $0$), so *Lunch* shows a **large, highly significant** difference — non-free students score on average $\\sim 32$ points above free-lunch ones.\n\n'
    '**In the regression model** the *Lunch* coefficient is $\\hat\\beta_{\\text{Lunch:free}} = -2.44$ with $p = 0.329$: **not significantly different from zero**. Once we control for *Read1*, *Sex*, *SchoolLoc* and *Experience*, free-lunch status no longer has an effect on *Read2*.\n\n'
    '**Why the change?** *Lunch* is strongly correlated with other predictors: free-lunch students tend to attend inner-city schools and to have lower *Read1* (and possibly less experienced teachers). The simple difference of means $\\sim 32$ points conflates the true effect of *Lunch* with the effects of these other variables; in the regression those channels are absorbed by *Read1* and *SchoolLoc*, and the residual "pure" *Lunch* effect collapses to roughly zero. In other words, **once the playing field is levelled for prior reading ability and school location, free-lunch students perform as well as non-free ones** — *Lunch* is not the causal driver, it is a marker for those other disadvantages.\n\n'
    '**R commands:**\n\n'
    '`# Marginal effect`\n\n'
    '`coef(lm(Read2 ~ Lunch, data=Primary))   ## Lunchfree ~ -32.31`\n\n'
    '`# Partial effect (full model)`\n\n'
    '`summary(regr.A)$coef["Lunchfree", ]     ## -2.441 (p = 0.329)`\n'
), "images": [
    "statistics/images/past_exams/exam_g1_2024_1d_ai.png",
]}

# ----- G1-2024 Ex2.a — One-sided z test on mean ReadGrowth (mu0 = 62, sigma = 34, alpha = 1%) -----
past_exams["exam_g1_2024_ex2_a"] = {
"title": "G1-2024 Ex2.a — One-sided z test: mean ReadGrowth >= 62 at 1% (sigma=34 known)",
"is_exam": True, "topic_hint": "G14", "subtopic_hint": "g14a",
"content": (
    '<span class="exam-question-text">The local education authority carefully monitors the improvement in the students\' performance in order to evaluate possible support programs. The variable **ReadGrowth** measures the increase in the students\' reading score from the 1st to the 2nd grade. In the following, round results to 3 decimals.\n\nBased on historical data, it is assumed that the standard deviation of the increase in the reading score is 34, and the target average increase in the reading score is set to 62 points. Based on the available sample, it is of interest to verify whether the average increase in the reading score is lower than 62; only in this case some actions would be taken, and specifically support programs. State the hypotheses to verify and determine the **rejection region of the test at the 1% significance level** to verify them, specifying the procedure followed. What are your conclusions? Explain.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** "Lower than 62 $\\Rightarrow$ act" is a **one-sided** test. The relevant hypothesis split is "no action / status quo" vs "act", with the costlier error being acting when we should not — so we put status quo in $H_0$:\n\n'
    '$$H_0: \\mu \\ge 62 \\quad\\text{vs}\\quad H_1: \\mu < 62.$$\n\n'
    'Variance is **known** ($\\sigma = 34$) and $n = 832$ is large, so the test statistic under $H_0$ is\n\n'
    '$$Z = \\frac{\\bar X - 62}{\\sigma/\\sqrt n} \\sim N(0,1).$$\n\n'
    'Reject $H_0$ for small (very negative) values of $Z$: $Z < z_\\alpha = z_{0.01} = -2.326$. Equivalently in the original scale\n\n'
    '$$\\bar X < 62 - z_{0.99}\\,\\sigma/\\sqrt n = 62 - 2.326\\cdot 34/\\sqrt{832} = 59.258.$$\n\n'
    '![AI walkthrough — left-tail rejection region for one-sided z test at 1%](statistics/images/past_exams/exam_g1_2024_ex2_a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Sample mean $\\bar x = 60.019$.\n\n'
    '**Rejection region at 1%:** $\\bar X < 59.258$ (equivalently $Z < -2.326$).\n\n'
    'Observed: $60.019 > 59.258$ (equivalently $Z_{\\text{obs}} = (60.019-62)/(34/\\sqrt{832}) = -1.680 > -2.326$). **We do NOT reject $H_0$ at 1%.** With the available evidence we cannot conclude that the average reading increase is below the target 62 points → **no support program is implemented**. (For reference the $p$-value is $P(Z<-1.680)\\approx 0.046$ — would lead to rejection at 5% but not at 1%.)\n\n'
    '**R commands:**\n\n'
    '`xbar <- mean(Primary$ReadGrowth)   ## 60.01923`\n\n'
    '`# critical value of the rejection rule on x-bar`\n\n'
    '`62 - qnorm(0.99) * 34 / sqrt(832)`\n\n'
    '`## [1] 59.25785`\n\n'
    '`# z-statistic and p-value`\n\n'
    '`z <- (xbar - 62)/(34/sqrt(832)); z`\n\n'
    '`## [1] -1.680415`\n\n'
    '`pnorm(z)`\n\n'
    '`## [1] 0.04643827`\n'
), "images": [
    "statistics/images/past_exams/exam_g1_2024_ex2_a_ai.png",
]}

# ----- G1-2024 Ex2.a3 — Probability of rejecting H0 when true mean = 58 (power / beta) -----
past_exams["exam_g1_2024_ex2_a3"] = {
"title": "G1-2024 Ex2.a3 — Probability of (not) rejecting H0 when the true mean is 58",
"is_exam": True, "topic_hint": "G14", "subtopic_hint": "g14b",
"content": (
    '<span class="exam-question-text">What is the probability that based on the test developed at point a (i.e. the local education authority would decide to take no actions, hence not implementing any support program) when the actual average increase in the reading score is **58**? Report the procedure and the functions in RStudio used to answer.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** "Take no action" = **fail to reject $H_0$**. Conditional on the true mean being $\\mu = 58$ (in $H_1$), this is a **Type II error**, and its probability is $\\beta(\\mu=58) = P(\\bar X \\ge \\bar x_{\\text{crit}} \\mid \\mu = 58)$ where $\\bar x_{\\text{crit}} = 59.258$ from Ex 2.a.\n\n'
    'With $\\sigma = 34$ known and $\\bar X\\mid\\mu \\sim N(\\mu, \\sigma^2/n)$:\n\n'
    '$$\\beta(58) = P\\!\\left(\\frac{\\bar X - 58}{\\sigma/\\sqrt n} \\ge \\frac{59.258 - 58}{34/\\sqrt{832}}\\right) = P(Z \\ge 1.067) = 1 - \\Phi(1.067).$$\n\n'
    '![AI walkthrough — alternative N(58, sigma2/n) with beta = area above 59.258](statistics/images/past_exams/exam_g1_2024_ex2_a3_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $\\beta(\\mu=58) = 1 - \\Phi\\!\\left(\\dfrac{59.258 - 58}{34/\\sqrt{832}}\\right) = 1 - \\Phi(1.067) \\approx \\mathbf{0.143}.$\n\n'
    'So if the true average increase were $58$ points, the test would (incorrectly) **fail to detect** that drop — and therefore decline to launch the support program — with probability about $14.3\\%$. Equivalently, the **power** at $\\mu=58$ is $1-\\beta \\approx 0.857$ (the test correctly rejects $\\sim 85.7\\%$ of the time).\n\n'
    '**R commands:**\n\n'
    '`crit.val <- 62 - qnorm(0.99) * 34/sqrt(832)   ## 59.25785`\n\n'
    '`# P(not reject | mu = 58)  =  beta`\n\n'
    '`1 - pnorm( (crit.val - 58) / (34/sqrt(832)) )`\n\n'
    '`## [1] 0.142931`\n\n'
    '`# or equivalently`\n\n'
    '`1 - pnorm(crit.val, mean=58, sd=34/sqrt(832))`\n\n'
    '`## [1] 0.142931`\n'
), "images": [
    "statistics/images/past_exams/exam_g1_2024_ex2_a3_ai.png",
]}

# ----- G1-2024 Ex2.b — Two-proportion z test ReadGrowth<62 rural vs inner-city -----
past_exams["exam_g1_2024_ex2_b"] = {
"title": "G1-2024 Ex2.b — Two-proportion test: P(ReadGrowth<62) rural vs inner-city",
"is_exam": True, "topic_hint": "G14", "subtopic_hint": "g14c",
"content": (
    '<span class="exam-question-text">To identify possible critical situations and areas of intervention, it is of interest to evaluate the proportion of students whose increase in reading scores is **lower** than 62 in schools located in **rural** areas and in **inner-city** areas (**SchoolLoc**=rural and **SchoolLoc**=inner-city). Determine the proportion of students with an increased reading score (**ReadGrowth**) lower than 62 among the students attending school in a rural area (**SchoolLoc**=rural) and in inner-city (**SchoolLoc**=inner-city). It can be concluded that the proportion of interest in rural areas (**SchoolLoc**=rural) is significantly **lower** than in inner-city (**SchoolLoc**=inner-city)? Report the realisation of the test statistic, specifying the meaning/definition of all the quantities it is based upon, and provide its definition. What is the statistical conclusion based on the observed sample data? Why?</span>\n\n'
    '---\n\n'
    '**Walkthrough.** Two independent proportions $p_R = P(\\text{ReadGrowth}<62\\mid \\text{rural})$ and $p_I = P(\\text{ReadGrowth}<62\\mid \\text{inner-city})$, one-sided question:\n\n'
    '$$H_0:\\; p_R \\ge p_I \\qquad H_1:\\; p_R < p_I.$$\n\n'
    'Pooled-variance large-sample $Z$:\n\n'
    '$$Z = \\frac{\\hat p_R - \\hat p_I}{\\sqrt{\\hat p_0(1-\\hat p_0)\\left(\\tfrac{1}{n_R}+\\tfrac{1}{n_I}\\right)}},\\qquad \\hat p_0 = \\frac{x_R + x_I}{n_R + n_I}.$$\n\n'
    'Reject for small (very negative) $Z$. p-value $= P(Z < z_{\\text{obs}})$.\n\n'
    '![AI walkthrough — sampling null under p_R = p_I and observed z = -1.519](statistics/images/past_exams/exam_g1_2024_ex2_b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Counts of $\\text{ReadGrowth} < 62$:\n\n'
    '| SchoolLoc | < 62 | $n$ | $\\hat p$ |\n'
    '|---|---:|---:|---:|\n'
    '| rural | $171$ | $379$ | $\\hat p_R = 0.4512$ |\n'
    '| inner-city | $103$ | $199$ | $\\hat p_I = 0.5176$ |\n'
    '| pooled | $274$ | $578$ | $\\hat p_0 = 0.4740$ |\n\n'
    'Test statistic:\n\n'
    '$$Z_{\\text{obs}} = \\frac{0.4512 - 0.5176}{\\sqrt{0.4740\\cdot 0.5260\\,(1/379 + 1/199)}} = -1.519.$$\n\n'
    'One-sided $p$-value $= P(Z < -1.519) \\approx 0.0644$.\n\n'
    'Since $p = 0.0644 > 0.05$, **we do NOT reject $H_0$ at 5%**: the data do not provide enough evidence to claim that the proportion of low-growth pupils is *significantly* smaller in rural than in inner-city schools (though the observed sample proportions do point in that direction). The conclusion **would change at the 10% level** ($0.0644 < 0.10$).\n\n'
    '**R commands:**\n\n'
    "`Prop.Read <- Primary$ReadGrowth < 62`\n\n"
    "`TEST.diffprop(x = Prop.Read[Primary$SchoolLoc=='rural'],`\n\n"
    "`              y = Prop.Read[Primary$SchoolLoc=='inner-city'],`\n\n"
    "`              alternative='less', digits=3)`\n\n"
    "`## p_rural - p_inner = -0.0664   Z = -1.519   p-value = 0.0644`\n"
), "images": [
    "statistics/images/past_exams/exam_g1_2024_ex2_b_ai.png",
]}

# ----- G1-2024 Ex3.a — Which is more dispersed: Read2 or Math2 (CV) -----
past_exams["exam_g1_2024_ex3_a"] = {
"title": "G1-2024 Ex3.a — Which variable is more dispersed: Read2 or Math2? (coefficient of variation)",
"is_exam": True, "topic_hint": "G3",
"content": (
    '<span class="exam-question-text">Consider the 2nd-grade students\' reading and math scores (variables **Read2** and **Math2**). Which variable is **more dispersed**? Answer on the basis of a suitable summary measure, **justifying** and **explaining** your choice.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** Comparing dispersion across two variables on **different scales / different means** with the raw standard deviation can be misleading: a larger $\\sigma$ may just reflect a larger mean. The standard scale-free measure of *relative* dispersion is the **coefficient of variation**\n\n'
    '$$CV = \\frac{s}{\\bar x},$$\n\n'
    'often reported as a percentage. The variable with the larger CV is the one whose values are more dispersed relative to their own average.\n\n'
    '![AI walkthrough — Read2 vs Math2: mean, sd, CV side-by-side](statistics/images/past_exams/exam_g1_2024_ex3_a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** From the sample:\n\n'
    '| variable | mean | sd | $CV = s/\\bar x$ |\n'
    '|---|---:|---:|---:|\n'
    '| Read2 | $588.118$ | $46.655$ | $0.0793$ |\n'
    '| Math2 | $582.162$ | $48.630$ | $0.0835$ |\n\n'
    'The raw SDs are similar (and Math2\'s is slightly higher). The two means are also similar, so the ranking does not flip when switching to CV, but the CV is the right justification: $CV_{\\text{Math2}}=0.0835 > CV_{\\text{Read2}}=0.0793$. So **Math2 is (slightly) more dispersed than Read2** in relative terms — about 8.4% of its mean against 7.9% for Read2. The two distributions have a similar level of relative variability, even if Math2 is *slightly* more spread.\n\n'
    '**R commands:**\n\n'
    '`distr.summary.x(Primary$Read2, stats=c("summary","dispersion"))`\n\n'
    '`distr.summary.x(Primary$Math2, stats=c("summary","dispersion"))`\n\n'
    '`sd(Primary$Read2)/mean(Primary$Read2)   ## 0.07932921`\n\n'
    '`sd(Primary$Math2)/mean(Primary$Math2)   ## 0.08353295`\n'
), "images": [
    "statistics/images/past_exams/exam_g1_2024_ex3_a_ai.png",
]}

# =================== GENERAL 1 2025 ===================

past_exams["exam_g1_2025_1a"] = {
"title": "G1-2025 Ex1a — SleepQuality 95th percentile",
"is_exam": True, "topic_hint": "G6",
"content": (
    '<span class="exam-question-text">Find the threshold separating the top 5% of subjects by `SleepQuality` from the others.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2025_1a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** "Top 5% by `SleepQuality`" means the largest 5% of observed values. The cut-off is the **95th percentile** $q_{0.95}$: by definition, 95% of the sample falls at or below it, and the remaining 5% lies strictly above. Compute it with `quantile(..., probs=0.95)` (or `distr.summary.x` with `stats=\'p95\'`). The plot below shows the empirical distribution with the threshold marked in yellow and the top-5% bins highlighted.\n\n'
    '![AI walkthrough — 95th percentile threshold](statistics/images/past_exams/exam_g1_2025_1a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** The 95th percentile of `SleepQuality` in the sample is **9.64** — subjects with `SleepQuality > 9.64` are the top 5%.\n\n'
    '**R commands:**\n\n'
    '`quantile(sleep$SleepQuality, probs=0.95)`\n\n'
    '`distr.summary.x(SleepQuality, stats=\'p95\', data=sleep)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g1_2025_1a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2025_1a_question.png",
    "statistics/images/past_exams/exam_g1_2025_1a_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2025_1a_answer.png",
]}

past_exams["exam_g1_2025_1b"] = {
"title": "G1-2025 Ex1b — Plot for SleepQuality tails",
"is_exam": True, "topic_hint": "G1",
"content": (
    '<span class="exam-question-text">Which plot would you use to assess the **tails** of `SleepQuality` accurately? Justify your choice and report the R command(s) that produce it.</span>\n\n'
    '![Ex 1b question — plot for SleepQuality tails](statistics/images/past_exams/questions/exam_g1_2025_1b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.**\n\n'
    'The question is about **tails**, not about centre or spread. So pick the plot that preserves resolution far from the median.\n\n'
    '**Why the boxplot alone is weak here.** A boxplot collapses each tail into a single whisker reaching at most $Q_3+1.5\\,\\mathrm{IQR}$ (or $Q_1-1.5\\,\\mathrm{IQR}$) plus a handful of outlier dots. For a **light-tailed** variable like `SleepQuality` (concentrated around the central values) the whiskers sit near the data min/max and there are **few or no outliers**, so the picture says almost nothing about *how* the mass thins out.\n\n'
    '**Why a histogram with ~20 bins wins.** Many narrow bins expose the actual decay shape: every bump and each near-empty bin in the tails becomes visible. ~20 bins is the standard rule-of-thumb compromise — enough resolution to see the tails without making the histogram noisy.\n\n'
    '**Best practice.** Plot **both** on the same data: histogram for tail shape, boxplot for a quick summary (median, IQR, any outliers). They are complementary; in this specific dataset the histogram does the heavy lifting because the tails are light.\n\n'
    '![Ex 1b AI walkthrough — boxplot hides tails, histogram exposes them](statistics/images/past_exams/exam_g1_2025_1b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Use a **histogram with ~20 bins** (combined with a boxplot for context). The distribution of `SleepQuality` concentrates around the central values with **light tails**, so the boxplot is less informative than the histogram in this specific case.\n\n'
    '**R commands:**\n\n'
    "`distr.plot.x(SleepQuality, plot.type='histogram', breaks=20, data=sleep)`\n\n"
    "`distr.plot.x(SleepQuality, plot.type='boxplot', data=sleep)`\n\n"
    '![Ex 1b answer](statistics/images/past_exams/answers/exam_g1_2025_1b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2025_1b_question.png",
    "statistics/images/past_exams/exam_g1_2025_1b_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2025_1b_answer.png",
]}

past_exams["exam_g1_2025_2a"] = {
"title": "G1-2025 Ex3 — Paired t-test sleep duration (minutes) pre vs post diet",
"is_exam": True, "topic_hint": "G14", "subtopic_hint": "g14c",
"content": _q(
    "<span class=\"exam-question-text\">Test whether sleep duration (in minutes) increased after the diet. Paired sample, $n = 161$, $\\bar x_{\\text{before}} = 402.89$, $s_{\\text{before}} = 45.61$, $\\bar x_{\\text{after}} = 414$, $s_{\\text{after}} = 48$, correlation $r = 0.71$. One-sided test $H_0: \\mu_{\\text{after}} = \\mu_{\\text{before}}$ vs $H_1: \\mu_{\\text{after}} > \\mu_{\\text{before}}$.</span>",
    "Paired t-test using $\\hat\\sigma_D = \\sqrt{s_{\\text{before}}^2 + s_{\\text{after}}^2 - 2r\\cdot s_{\\text{before}}\\cdot s_{\\text{after}}} = \\sqrt{45.61^2 + 48^2 - 2(0.71)(45.61)(48)} \\approx 35.71$. Then $t_{\\text{obs}} = (414 - 402.89)/(35.71/\\sqrt{161}) \\approx 3.95$ on $df = 160$. p-value $= P(T_{160} \\ge 3.95) \\approx 5.85 \\times 10^{-5}$. **Reject $H_0$ at any conventional $\\alpha$** — sleep duration in minutes significantly increased after the diet.",
    "t.test(after, before, paired=TRUE, alternative='greater')\nsd_D <- sqrt(45.61^2 + 48^2 - 2*0.71*45.61*48)\nt_stat <- (414 - 402.89)/(sd_D/sqrt(161))\n1 - pt(t_stat, df=160)",
    w="Apply a **one-sided paired t-test** for the mean of the differences $D_i = X_{\\text{after},i} - X_{\\text{before},i}$. Hypotheses $H_0:\\mu_D = 0$ vs $H_1:\\mu_D > 0$. With only marginal summaries, recover $\\hat\\sigma_D$ from $s_D^2 = s_{\\text{before}}^2 + s_{\\text{after}}^2 - 2 r\\, s_{\\text{before}} s_{\\text{after}}$ (variance of a paired difference with sample correlation $r$). The test statistic is $t_{\\text{obs}} = \\bar D / (\\hat\\sigma_D/\\sqrt{n})$ compared to $T_{n-1}$ and p-value $= P(T_{n-1} \\ge t_{\\text{obs}})$."
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2025_2a_question.png",
    "statistics/images/past_exams/answers/exam_g1_2025_2a_answer.png",
    "statistics/images/past_exams/exam_g1_2025_2a_ai.png",
]}

past_exams["exam_g1_2025_3a"] = {
"title": "G1-2025 Ex4 — Multiple regression SleepQuality ~ Stress+Age+BMI+Physical",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15c",
"content": (
    '<span class="exam-question-text">Estimate `SleepQuality ~ Stress + Age + BMI + Physical` on `SleepData`. Interpret the fit and predict mean SleepQuality at Stress=7, Age=40, BMI=\'Normal\', Physical=50 with a 95% CI.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2025_3a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.**\n\n'
    'A multiple-regression read-out has three layers — *each predictor*, *the model overall*, and *predictions with uncertainty* — and you must comment on all three.\n\n'
    '**1) Per-predictor significance.** From `summary(mod)` every t-test on a coefficient returns `Pr(>|t|) < 0.001` (`***`): **Stress**, **Age**, **BMI** and **Physical** are each individually significant given the others. Stress, Age, and BMI=Over reduce SleepQuality; Physical (activity) raises it — directions match common sense.\n\n'
    '**2) Joint fit.** *Adjusted* $R^2 = 0.5468$: roughly **55% of the variance** in SleepQuality is explained by the four predictors (adjusted penalises adding regressors, so it is the right metric for comparison). The omnibus **$F(4, 290) = 79.39$, p $< 2.2 \\times 10^{-16}$** rejects the hypothesis "all slopes = 0" overwhelmingly → the model is **jointly highly significant**.\n\n'
    '**3) Prediction at the requested profile.** Plug `(Stress=7, Age=40, BMI=Normal, Physical=50)` into the fitted equation: $\\hat y = 6.827$. For the **mean** response use `interval=\'confidence\'` (narrower, about uncertainty in the *line*), giving a 95% CI of $[6.235, 7.419]$. (Use `interval=\'prediction\'` only for a single new individual — that would be wider.)\n\n'
    '![AI walkthrough — per-predictor significance, Adjusted R² thermometer + F, and prediction with 95% CI](statistics/images/past_exams/exam_g1_2025_3a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** All four predictors are individually significant; the model explains about **55%** of SleepQuality variance (**Adjusted $R^2 = 0.5468$**) and is jointly highly significant (**$F(4,290) = 79.39$, p $< 2.2 \\times 10^{-16}$**). Predicted mean SleepQuality at the requested profile is **$\\hat y = 6.827$**, with a **95% confidence interval $[6.235,\\ 7.419]$**.\n\n'
    '**R commands:**\n\n'
    '`mod <- lm(SleepQuality ~ Stress + Age + BMI + Physical, data=SleepData)`\n\n'
    '`summary(mod)`\n\n'
    '`predict(mod, newdata=data.frame(Stress=7, Age=40, BMI=\'Normal\', Physical=50), interval=\'confidence\')`\n\n'
    '`## fit lwr upr`\n\n'
    '`## 1 6.827 6.235 7.419`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g1_2025_3a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2025_3a_question.png",
    "statistics/images/past_exams/exam_g1_2025_3a_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2025_3a_answer.png",
]}

past_exams["exam_g1_2025_3b"] = {
"title": "G1-2025 Ex3b — PhysicalActivity loses significance with Steps added",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15e",
"content": (
    '<span class="exam-question-text">Why does PhysicalActivity stop being significant when Steps is added to the regression model for SleepQuality?</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2025_3b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.**\n\n'
    'The puzzle is *not* that PhysicalActivity stopped *mattering* — it is that the **t-test on its coefficient** no longer rejects $H_0: \\beta_{PA} = 0$. Three steps explain why.\n\n'
    '**1) Diagnose correlation.** Steps and PhysicalActivity measure essentially the same underlying construct: someone who walks 12,000 steps a day is mechanically more active. A scatterplot shows a tight positive cloud, with $r(\\text{Steps},\\text{PA}) \\approx 0.93$. The two regressors carry almost the same information.\n\n'
    '**2) Variance inflation.** The OLS standard error of any coefficient is\n\n'
    '$$\\mathrm{Var}(\\hat\\beta_j) = \\frac{\\sigma^2}{(1 - R_j^2)\\,\\sum_i (x_{ij} - \\bar x_j)^2},$$\n\n'
    'where $R_j^2$ is the $R^2$ of regressing $x_j$ on the *other* predictors. When Steps and PA are nearly collinear, $R_j^2 \\to 1$ for both, the denominator collapses, $\\mathrm{Var}(\\hat\\beta_j)$ **explodes**, and the t-ratio $\\hat\\beta_j/SE$ shrinks toward 0. The **VIF** = $1/(1-R_j^2)$ for PA and Steps is large (well above the rule-of-thumb 5).\n\n'
    '**3) Net effect on inference.** Each predictor *individually* explains SleepQuality strongly (Model A: $p < 0.001$). But once both enter (Model B), the model cannot decide *which one* deserves the credit — the **point estimates remain reasonable**, yet the **p-values rise** above 0.05. The variables are *jointly* significant (F-test still strong) but **not individually distinguishable**.\n\n'
    '![AI walkthrough — scatter showing strong Steps~PA correlation, coefficient/p-value comparison across Model A (solo) and Model B (both), and VIF check](statistics/images/past_exams/exam_g1_2025_3b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** **Multicollinearity.** Steps and PhysicalActivity are strongly correlated (more steps ↔ more active), so they carry redundant information. When both enter the model, neither contributes *uniquely* — the standard errors inflate (high **VIF**), the individual t-tests fail to reject $H_0$, and PhysicalActivity loses individual significance even though *jointly* the pair still explains the variance.\n\n'
    '**R commands:**\n\n'
    '`cor(sleep$Steps, sleep$PhysicalActivity)`\n\n'
    '`mod_full <- lm(SleepQuality ~ Age + PhysicalActivity + Hours + Steps, data=sleep)`\n\n'
    '`summary(mod_full)`\n\n'
    '`car::vif(mod_full)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g1_2025_3b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2025_3b_question.png",
    "statistics/images/past_exams/exam_g1_2025_3b_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2025_3b_answer.png",
]}

past_exams["exam_g1_2025_3c"] = {
"title": "G1-2025 Ex3c — Homoscedasticity assumption",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15e",
"content": (
    '<span class="exam-question-text">State the homoscedasticity assumption.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2025_3c_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.**\n\n'
    'Homoscedasticity is one of the **Gauss–Markov** conditions; it is what makes OLS the **best linear unbiased estimator** and what keeps the textbook standard errors valid.\n\n'
    '**1) Formal statement.** Conditional on the predictors, every error term shares the **same** variance:\n\n'
    '$$\\mathrm{Var}(\\varepsilon_i \\mid x_i) \\;=\\; \\sigma^2 \\qquad \\text{for every } i = 1, \\dots, n.$$\n\n'
    'The variance does **not** depend on $x_i$, on the fitted value $\\hat y_i$, on the index $i$, on time, or on any group. A single scalar $\\sigma^2$ governs the spread.\n\n'
    '**2) Why it matters.** If $\\mathrm{Var}(\\varepsilon_i) = \\sigma^2 \\,h(x_i)$ with $h$ non-constant — *heteroscedasticity* — OLS estimators are still unbiased but **no longer minimum-variance**, and the usual SE / t-test / F-test / confidence-interval formulas are **wrong** (typically too narrow when $h$ is large where leverage is large).\n\n'
    '**3) Diagnose visually.** The canonical check is **`plot(mod, which=1)`** — Residuals vs Fitted. Under homoscedasticity the cloud forms a flat band of *constant* vertical width centred on 0. A **fanning cone** (spread grows with $\\hat y$) or a *funnel/megaphone* shape signals a violation. The companion **`plot(mod, which=3)`** (Scale-Location) plots $\\sqrt{|\\text{standardised residuals}|}$ vs fitted; a roughly horizontal red smooth confirms constant variance, an upward trend confirms heteroscedasticity.\n\n'
    '**4) Fixes if violated.** Variance-stabilising transformations (log, $\\sqrt{\\cdot}$), weighted least squares, or robust (sandwich) standard errors.\n\n'
    '![AI walkthrough — left panel: homoscedastic residuals inside constant ±2σ band; right panel: heteroscedastic fanning cone signalling violation](statistics/images/past_exams/exam_g1_2025_3c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** **$\\mathrm{Var}(\\varepsilon_i) = \\sigma^2$ for every $i$** — error terms share a single constant variance, independent of the predictors\' values. Diagnose by plotting residuals vs fitted (`plot(mod, which=1)`); a flat band of constant width supports the assumption, a fanning cone signals heteroscedasticity (violation). Confirm with the Scale-Location plot (`which=3`).\n\n'
    '**R commands:**\n\n'
    '`plot(mod, which=1)   # Residuals vs Fitted — look for constant-width band`\n\n'
    '`plot(mod, which=3)   # Scale-Location — look for horizontal red smooth`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g1_2025_3c_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2025_3c_question.png",
    "statistics/images/past_exams/exam_g1_2025_3c_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2025_3c_answer.png",
]}

# ---- general 1 2025: extra coverage (1c, 2b, 4a, 4b, 4c) ----
past_exams["exam_g1_2025_1c"] = {
"title": "G1-2025 Ex1c — Percentiles of SleepQuality to read the tails",
"is_exam": True, "topic_hint": "G6",
"content": (
    '<span class="exam-question-text">To provide more detail about the tails of `SleepQuality`, report and interpret the relevant percentiles (1%, 5%, 10%, 25%, 75%, 90%, 95%, 99%).</span>\n\n'
    '---\n\n'
    '**Walkthrough.** A small set of percentiles is the natural numeric companion to the histogram: each $p_q$ marks the value below which lie $q\\%$ of the observations. Comparing **symmetric pairs** $\\,(p_5,p_{95})\\,$ and $\\,(p_1,p_{99})\\,$ around the median tells you whether the distribution is symmetric or skewed and how *fast* the mass thins out far from the centre. The quartile pair $(p_{25},p_{75})$ delivers the IQR — the box of the boxplot.\n\n'
    '**Numerical readings (R `quantile`).** $p_1 = 3.55$, $p_5 = 4.76$, $p_{10} = 5.33$, $p_{25} = 6.26$, **median $= 7.41$**, $p_{75} = 8.41$, $p_{90} = 9.24$, $p_{95} = 9.64$, $p_{99} = 10.60$.\n\n'
    '**Tail interpretation.** The two halves around the median are nearly the same width ($7.41 - 5.33 \\approx 2.08$ vs $9.24 - 7.41 \\approx 1.83$): the distribution is **roughly symmetric, slightly left-skewed**. Both tails are **light**: only 5% of subjects sleep worse than $\\sim 4.76$ and only 5% sleep better than $\\sim 9.64$.\n\n'
    '![AI walkthrough — empirical CDF with the 1/5/10/25/75/90/95/99% percentile horizontal cuts](statistics/images/past_exams/exam_g1_2025_1c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** The percentiles confirm the histogram: median $\\approx 7.41$, IQR $= p_{75} - p_{25} \\approx 2.15$, and tails are **light** — only 5% of subjects report SleepQuality below **4.76** (poor sleepers) and only 5% above **9.64** (excellent sleepers). The distribution is approximately symmetric with a mild left skew.\n\n'
    '**R commands:**\n\n'
    "`quantile(sleep$SleepQuality, probs=c(.01,.05,.10,.25,.50,.75,.90,.95,.99))`\n\n"
    "`distr.summary.x(SleepQuality, stats=c('p1','p5','p10','p25','median','p75','p90','p95','p99'), data=sleep)`\n"
), "images": [
    "statistics/images/past_exams/exam_g1_2025_1c_ai.png",
]}

past_exams["exam_g1_2025_2b"] = {
"title": "G1-2025 Ex2 — Dispersion comparison: SleepQuality vs SleepDuration (CV)",
"is_exam": True, "topic_hint": "G7",
"content": (
    '<span class="exam-question-text">Which of the two variables (`SleepQuality` or `SleepDuration`) has the larger dispersion? Provide a numeric justification.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** The two variables are measured on **different scales** (SleepQuality on a 1–10 quality index, SleepDuration in minutes around ~426). Comparing raw standard deviations is meaningless: $s_{\\text{Duration}} = 48.7$ min is "bigger" than $s_{\\text{Quality}} = 1.53$, but only because minutes are a much larger unit. The right scale-free measure is the **coefficient of variation**\n\n'
    '$$\\mathrm{CV}(X) = \\dfrac{s_X}{\\bar X},$$\n\n'
    'a unitless ratio that places both variables on the same footing.\n\n'
    '**Numbers.** $\\bar X_{Q} = 7.273$, $s_Q = 1.529 \\Rightarrow \\mathrm{CV}_Q = 1.529/7.273 \\approx 0.210$. $\\bar X_{D} = 425.85$, $s_D = 48.66 \\Rightarrow \\mathrm{CV}_D = 48.66/425.85 \\approx 0.114$.\n\n'
    '**Reading.** SleepQuality varies by about **21% of its mean**; SleepDuration by only **11%**. So *relative* to its own centre, SleepQuality is roughly **1.8× more dispersed** than SleepDuration.\n\n'
    '![AI walkthrough — side-by-side bar of CV (Quality vs Duration) + paired histograms standardised by mean](statistics/images/past_exams/exam_g1_2025_2b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Use the **coefficient of variation** because the two variables have different units/scales. $\\mathrm{CV}(SleepQuality) = s/\\bar x = 1.529/7.273 \\approx \\mathbf{0.210}$ (21%), $\\mathrm{CV}(SleepDuration) = 48.66/425.85 \\approx \\mathbf{0.114}$ (11%). **SleepQuality has the larger relative dispersion** (roughly twice that of SleepDuration).\n\n'
    '**R commands:**\n\n'
    '`sd(sleep$SleepQuality)/mean(sleep$SleepQuality)`\n\n'
    '`## [1] 0.2102`\n\n'
    '`sd(sleep$SleepDuration)/mean(sleep$SleepDuration)`\n\n'
    '`## [1] 0.1143`\n'
), "images": [
    "statistics/images/past_exams/exam_g1_2025_2b_ai.png",
]}

past_exams["exam_g1_2025_4a"] = {
"title": "G1-2025 Ex3a — 99% CI for difference of means SleepDuration (Nurse − Doctor)",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13c_ci_diff_means",
"content": (
    '<span class="exam-question-text">Compute the standard error of the estimator for the difference between the average SleepDuration for nurses and doctors and report a **99% confidence interval** for the difference in mean SleepDuration between Nurses and Doctors.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** Two independent samples — Nurses ($n_N = 118$) and Doctors ($n_D = 105$) — with unknown and *unequal* population variances. The point estimator is $\\hat\\Delta = \\bar X_N - \\bar X_D$ and its standard error follows the **Welch (unpooled) formula**\n\n'
    '$$SE(\\hat\\Delta) = \\sqrt{\\dfrac{s_N^2}{n_N} + \\dfrac{s_D^2}{n_D}}.$$\n\n'
    '**Plug in.** $\\bar X_N = 433.12$, $s_N = 47.95$, $n_N = 118$; $\\bar X_D = 409.54$, $s_D = 46.92$, $n_D = 105$. Hence\n\n'
    '$$SE(\\hat\\Delta) = \\sqrt{\\dfrac{47.95^2}{118} + \\dfrac{46.92^2}{105}} = \\sqrt{19.49 + 20.97} \\approx \\mathbf{6.36}\\ \\text{min}.$$\n\n'
    'Point estimate: $\\hat\\Delta = 433.12 - 409.54 = 23.58$ min.\n\n'
    '**99% CI.** With $n_N + n_D > 200$ we use the normal quantile $z_{0.995} = 2.576$:\n\n'
    '$$23.58 \\;\\pm\\; 2.576 \\cdot 6.36 \\;=\\; (\\mathbf{7.19},\\ \\mathbf{39.96})\\ \\text{min}.$$\n\n'
    '**Reading.** The whole interval lies **strictly above 0**, so we are 99% confident nurses sleep **between 7 and 40 minutes more per day on average** than doctors. The difference is statistically detectable at the 1% level.\n\n'
    '![AI walkthrough — two-group means with 99% CI bars, plus the difference $\\hat\\Delta$ with its $\\pm z_{0.995}\\,SE$ interval, all in minutes](statistics/images/past_exams/exam_g1_2025_4a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $SE(\\hat\\Delta) = \\sqrt{s_N^2/n_N + s_D^2/n_D} = \\sqrt{47.95^2/118 + 46.92^2/105} \\approx \\mathbf{6.36}$ minutes. With $\\hat\\Delta = 23.58$ min, the **99% CI for $\\mu_N - \\mu_D$** is $\\mathbf{(7.19,\\ 39.96)}$ minutes. Since 0 lies *outside* the interval, nurses sleep significantly more than doctors on average (between roughly 7 and 40 minutes per day, at the 1% level).\n\n'
    '**R commands:**\n\n'
    "`sub <- subset(Sleep, Occupation %in% c('Nurse','Doctor'))`\n\n"
    '`t.test(SleepDuration ~ Occupation, data=sub, conf.level=0.99)`\n\n'
    "`## Welch Two Sample t-test`\n\n"
    "`## t = -3.71, df = 219, p-value = 0.000266`\n\n"
    "`## 99 percent confidence interval: -40.10 -7.05`\n\n"
    "`## means: Doctor 409.54, Nurse 433.12`\n"
), "images": [
    "statistics/images/past_exams/exam_g1_2025_4a_ai.png",
]}

past_exams["exam_g1_2025_4b"] = {
"title": "G1-2025 Ex3b — One-sided proportion test: Doctors with any disorder > 35%",
"is_exam": True, "topic_hint": "G14", "subtopic_hint": "g14a",
"content": (
    '<span class="exam-question-text">Test, at the 1% significance level, whether the population proportion of doctors suffering from any sleep disorder (`SleepDisorder = Insomnia` or `Other`) is **higher than 0.35**. State the hypotheses, compute the test statistic and the p-value, and draw a conclusion.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** One-sided one-sample proportion test:\n\n'
    '$$H_0:\\ p \\le 0.35 \\qquad\\text{vs}\\qquad H_1:\\ p > 0.35,\\quad \\alpha = 0.01.$$\n\n'
    '**Sample.** Doctors: $n = 105$. Doctors with any disorder ($\\ne$ "None"): $x = 38$. Sample proportion $\\hat p = 38/105 \\approx \\mathbf{0.362}$.\n\n'
    '**Statistic (under $H_0$, $p_0 = 0.35$).** Use the *null* SE\n\n'
    '$$SE_0 = \\sqrt{\\dfrac{p_0(1 - p_0)}{n}} = \\sqrt{\\dfrac{0.35 \\cdot 0.65}{105}} \\approx 0.0466,$$\n\n'
    '$$z_{obs} = \\dfrac{\\hat p - p_0}{SE_0} = \\dfrac{0.362 - 0.35}{0.0466} \\approx \\mathbf{0.26}.$$\n\n'
    '**p-value.** $P(Z \\ge 0.26) = 1 - \\Phi(0.26) \\approx \\mathbf{0.399}$.\n\n'
    '**Decision.** $0.399 \\gg \\alpha = 0.01$ → **do not reject $H_0$**. The data do not provide evidence that the disorder rate among doctors exceeds 35%.\n\n'
    '![AI walkthrough — $\\hat p$ vs $p_0 = 0.35$ bar + N(0,1) null density with upper-tail rejection region and $z_{obs}$ marker](statistics/images/past_exams/exam_g1_2025_4b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $\\hat p = 38/105 = 0.362$. Test statistic $z_{obs} = (0.362 - 0.35)/\\sqrt{0.35 \\cdot 0.65 / 105} \\approx \\mathbf{0.26}$, one-sided p-value $\\approx \\mathbf{0.399}$. **Do not reject $H_0$** at $\\alpha = 0.01$: the disorder rate among doctors is **not** significantly higher than 35%.\n\n'
    '**R commands:**\n\n'
    "`doc <- subset(Sleep, Occupation=='Doctor')`\n\n"
    "`x <- sum(doc$SleepDisorder != 'None'); n <- nrow(doc)`\n\n"
    "`prop.test(x, n, p=0.35, alternative='greater', correct=FALSE)`\n\n"
    "`## X-squared = 0.0654, df = 1, p-value = 0.399`\n\n"
    "`## sample estimate: p = 0.3619`\n"
), "images": [
    "statistics/images/past_exams/exam_g1_2025_4b_ai.png",
]}

past_exams["exam_g1_2025_4c"] = {
"title": "G1-2025 Ex3c — Chi-square independence: SleepDisorder × BloodPressure",
"is_exam": True, "topic_hint": "G14", "subtopic_hint": "g14d_chi_squared",
"content": (
    '<span class="exam-question-text">Test whether there is an association between `SleepDisorder` and `BloodPressure` at the 1% significance level. State the hypotheses, compute the test statistic and its p-value, and conclude.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** Two categorical variables, independence test via Pearson chi-square on the $3 \\times 3$ contingency table.\n\n'
    '$$H_0:\\ \\text{SleepDisorder} \\perp \\text{BloodPressure}\\quad\\text{vs}\\quad H_1:\\ \\text{not independent},\\quad \\alpha = 0.01.$$\n\n'
    '**Observed counts** $O_{ij}$:\n\n'
    '|              | Normal | High | VeryHigh |\n'
    '|--------------|-------:|-----:|---------:|\n'
    '| None         |     82 |  130 |       28 |\n'
    '| Insomnia     |      1 |   45 |       61 |\n'
    '| Other        |      7 |   13 |       32 |\n\n'
    'Under $H_0$ the expected counts factor as $E_{ij} = R_i C_j / n$, and the statistic is\n\n'
    '$$\\chi^2 = \\sum_{i,j}\\dfrac{(O_{ij} - E_{ij})^2}{E_{ij}} \\;\\sim\\; \\chi^2_{(r-1)(c-1)} = \\chi^2_4.$$\n\n'
    '**R output.** $\\chi^2_{obs} = \\mathbf{116.32}$, $df = 4$, p-value $< 2.2 \\times 10^{-16}$.\n\n'
    '**Decision.** p-value $\\ll \\alpha = 0.01$ → **reject $H_0$**. The two variables are **strongly associated**: insomniacs concentrate in the High / VeryHigh BP categories, while subjects without disorder are far more common in Normal BP.\n\n'
    '![AI walkthrough — observed-vs-expected mosaic with cells coloured by signed Pearson residual, plus $\\chi^2$ null density with rejection cut $\\chi^2_{0.99,4}$ and the observed $\\chi^2 = 116.32$ spike](statistics/images/past_exams/exam_g1_2025_4c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $\\chi^2_{obs} = \\mathbf{116.32}$ on $df = 4$, p-value $< 2.2 \\times 10^{-16}$. **Reject $H_0$** at $\\alpha = 0.01$ (and at any reasonable level): there is overwhelming evidence that **SleepDisorder and BloodPressure are NOT independent** — insomnia/other disorders cluster in High/VeryHigh BP.\n\n'
    '**R commands:**\n\n'
    '`ct <- table(Sleep$SleepDisorder, Sleep$BloodPressure)`\n\n'
    '`chisq.test(ct)`\n\n'
    '`## Pearson\'s Chi-squared test`\n\n'
    '`## X-squared = 116.32, df = 4, p-value < 2.2e-16`\n'
), "images": [
    "statistics/images/past_exams/exam_g1_2025_4c_ai.png",
]}

# =================== GENERAL 1 2026 ===================

past_exams["exam_g1_2026_1a"] = {
"title": "G1-2026 Ex1a — 99% CI for PurposeLoan=Business proportion",
"is_exam": True, "topic_hint": "G13",
"subtopic_hint": "g13b",
"content": (
    '<span class="exam-question-text">We are interested in the proportion of customers who apply for a loan for reasons related to Business (variable `PurposeLoan = Business`). Report a **99% confidence interval** for such proportion and provide its interpretation.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2026_1a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** For a single proportion the large-sample CI is\n\n'
    '$$\\hat p \\pm z_{\\alpha/2}\\,\\sqrt{\\hat p(1-\\hat p)/n}.$$\n\n'
    'With $\\alpha = 0.01$ we use $z_{0.995} = 2.576$. From the sample (`Credit` data), `CI.prop(PurposeLoan=="Business", conf.level=0.99)` returns $\\hat p \\approx 0.20$ and the interval $[0.15,\\,0.24]$. The left panel below shows the CI on a number line; the right panel sketches the sampling distribution of $\\hat p$ with the central 99% mass shaded — endpoints of that shaded region are exactly the CI limits.\n\n'
    '![AI walkthrough — 99% CI duality](statistics/images/past_exams/exam_g1_2026_1a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $99\\%$ CI $= (0.15,\\,0.24)$. **Interpretation:** with confidence $99\\%$ the proportion of customers applying for a loan for **Business** reasons lies between $0.15$ and $0.24$.\n\n'
    '**R commands:**\n\n'
    '`CI.prop(PurposeLoan=="Business", conf.level=0.99, data=Credit)`\n\n'
    '`## n phat s_X se Lower Upper`\n\n'
    '`## ... 0.20 ... ... 0.15 0.24`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g1_2026_1a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2026_1a_question.png",
    "statistics/images/past_exams/exam_g1_2026_1a_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2026_1a_answer.png",
]}

past_exams["exam_g1_2026_1b"] = {
"title": "G1-2026 Ex1b — Hypothesis test using CI",
"is_exam": True, "topic_hint": "G14",
"subtopic_hint": "g14a",
"content": _q(
    "<span class=\"exam-question-text\">Using the CI from 1a (0.15, 0.24), test $H_0: p = 0.3$ vs $H_1: p \\ne 0.3$ at any level $\\alpha$.</span>",
    "Since $0.3 \\notin [0.15, 0.24]$, the 99% CI **rejects** $H_0$ at level $\\alpha = 0.01$. Equivalently, any test at $\\alpha \\ge 0.01$ rejects. At $\\alpha < 0.01$ (e.g. 0.005), the conclusion would require a wider CI to verify.",
    "# CI-test duality: 0.3 outside the 99% CI => reject H0 at alpha = 0.01\nTEST.prop(PurposeLoan, success='Business', p0=0.3, alternative='two.sided', data=Loans)\n# manual: 1-sample prop test\nprop.test(x=sum(Loans$PurposeLoan=='Business'), n=nrow(Loans),\n          p=0.3, alternative='two.sided', conf.level=0.99)",
    w="Use the **CI–test duality**: a two-sided test of $H_0:p = p_0$ at level $\\alpha$ rejects iff $p_0$ falls **outside** the $(1-\\alpha)$ CI for $p$. With the $99\\%$ CI $[0.15,\\,0.24]$ from 1a and $p_0 = 0.3$, just check whether $p_0$ lies inside the interval to conclude."
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2026_1b_question.png",
    "statistics/images/past_exams/answers/exam_g1_2026_1b_answer.png",
    "statistics/images/past_exams/exam_g1_2026_1b_ai.png",
]}

past_exams["exam_g1_2026_1c"] = {
"title": "G1-2026 Ex1c — Sample size for CI width ≤ 0.09",
"is_exam": True, "topic_hint": "G13",
"subtopic_hint": "g13b",
"content": (
    '<span class="exam-question-text">What sample size guarantees a 99% CI with width $\\le 0.09$?</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2026_1c_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** The width of a two-sided CI for a proportion is $2\\cdot ME$ with $ME = z_{\\alpha/2}\\sqrt{p(1-p)/n}$. Requiring width $\\le 0.09$ gives $ME \\le 0.045$. Because $p$ is unknown at the design stage, use the **worst case** $p(1-p) = 0.25$ (peak at $p = 0.5$, left panel below). Inverting the ME formula yields\n\n'
    '$$n \\;\\ge\\; \\left(\\frac{z_{0.995}\\cdot 0.5}{ME}\\right)^2 \\;=\\; \\left(\\frac{2.576\\cdot 0.5}{0.045}\\right)^2 \\;=\\; 819.12.$$\n\n'
    'Round **up** to the next integer (right panel — the curve $n(ME)$ is plotted on a log axis with the target ME = 0.045 marked).\n\n'
    '![AI walkthrough — worst-case variance + n vs ME](statistics/images/past_exams/exam_g1_2026_1c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** **Minimum n = 820** customers.\n\n'
    '**R commands:**\n\n'
    '`ceiling((qnorm(0.995)*0.5/0.045)^2)`\n\n'
    '`## [1] 820`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g1_2026_1c_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2026_1c_question.png",
    "statistics/images/past_exams/exam_g1_2026_1c_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2026_1c_answer.png",
]}

# =================== GENERAL 2 2024 ===================

past_exams["exam_g2_2024_5a"] = {
"title": "G2-2024 Ex5 — Analytic CI for proportion (CrimePeople > 250)",
"is_exam": True, "topic_hint": "G13",
"subtopic_hint": "g13b",
"content": (
    '<span class="exam-question-text">Build a **99% confidence interval** for the **proportion** of US cities with `CrimePeople` > 250 (`CrimeUS` dataset). Report the analytical form, plug in the sample values and interpret the resulting interval.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2024_5a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** The target parameter is the **population proportion** $p = P(\\text{CrimePeople} > 250)$. With a large sample the natural estimator is the sample proportion $\\hat p = \\#\\{X_i > 250\\}/n$, whose sampling distribution is approximately normal by the CLT:\n\n'
    '$$\\hat p \\;\\dot\\sim\\; \\mathcal N\\!\\Big(p,\\; \\tfrac{p(1-p)}{n}\\Big).$$\n\n'
    'Replacing $p$ with $\\hat p$ in the standard error gives the **Wald (analytic) CI** at confidence level $1-\\alpha$:\n\n'
    '$$\\hat p \\;\\pm\\; z_{\\alpha/2}\\cdot\\sqrt{\\dfrac{\\hat p(1-\\hat p)}{n}},\\qquad z_{0.005}=2.576\\;\\;(\\text{99\\% CI}).$$\n\n'
    'Sample inputs: $\\hat p = 0.21$, $n = 485$ $\\Rightarrow$ $SE = \\sqrt{0.21\\cdot 0.79/485} \\approx 0.01849$ and ME $= 2.576\\cdot SE \\approx 0.0476$. The interval is symmetric around $\\hat p$.\n\n'
    '![AI walkthrough — sampling distribution and 99% CI for the proportion](statistics/images/past_exams/exam_g2_2024_5a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Plugging in:\n\n'
    '$$0.21 \\pm 2.576\\cdot\\sqrt{\\tfrac{0.21\\cdot 0.79}{485}} \\;=\\; 0.21 \\pm 0.0476 \\;\\approx\\; [0.16,\\; 0.26].$$\n\n'
    '**Interpretation.** With **99% confidence** the proportion of U.S. cities with `CrimePeople` > 250 lies between **0.16 and 0.26**. Equivalently, repeating the sampling procedure many times, about 99% of the resulting Wald intervals would cover the true $p$. The CI is well away from both 0 and 1 and $n\\hat p = 102$, $n(1-\\hat p) = 383$ are both $\\gg 5$, so the normal approximation is appropriate.\n\n'
    '**R commands:**\n\n'
    '`vec.binA <- CrimeUS$CrimePeople > 250`\n\n'
    '`CI.prop(vec.binA, conf.level=0.99)`\n\n'
    '`##   n   phat   s_X    se    Lower Upper`\n\n'
    '`## 485 0.21   0.41   0.02  0.16  0.26`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g2_2024_5a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2024_5a_question.png",
    "statistics/images/past_exams/exam_g2_2024_5a_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2024_5a_answer.png",
]}

past_exams["exam_g2_2024_5c"] = {
"title": "G2-2024 Ex5c — Sample size for CI width ≤ 0.05",
"is_exam": True, "topic_hint": "G13",
"subtopic_hint": "g13b",
"content": (
    '<span class="exam-question-text">What sample size guarantees a 99% CI with width $\\le 0.05$?</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2024_5c_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** The width of a two-sided CI for a proportion is $2\\cdot ME$ with $ME = z_{\\alpha/2}\\sqrt{p(1-p)/n}$. Requiring width $\\le 0.05$ gives $ME \\le 0.025$. At the design stage $p$ is unknown, so use the **worst case** $p(1-p) = 0.25$ (peak at $p = 0.5$, left panel below). Inverting the ME formula yields\n\n'
    '$$n \\;\\ge\\; \\left(\\frac{z_{0.995}\\cdot 0.5}{ME}\\right)^2 \\;=\\; \\left(\\frac{2.576\\cdot 0.5}{0.025}\\right)^2 \\;=\\; 2654.31.$$\n\n'
    'Round **up** to the next integer (right panel — the curve $n(ME)$ on a log axis with target ME = 0.025 marked).\n\n'
    '![AI walkthrough — worst-case variance + n vs ME](statistics/images/past_exams/exam_g2_2024_5c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** **Minimum n = 2655 cities**.\n\n'
    '**R commands:**\n\n'
    '`z <- qnorm(0.995); p_max <- 0.5; ME <- 0.025`\n\n'
    '`n_needed <- ceiling((z * sqrt(p_max*(1-p_max)) / ME)^2)`\n\n'
    '`n_needed`\n\n'
    '`## [1] 2655`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g2_2024_5c_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2024_5c_question.png",
    "statistics/images/past_exams/exam_g2_2024_5c_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2024_5c_answer.png",
]}

past_exams["exam_g2_2024_1a"] = {
"title": "G2-2024 Ex1 — Boxplots of CrimeProperty by Region",
"is_exam": True, "topic_hint": "G6", "subtopic_hint": "g6b_box",
"content": (
    '<span class="exam-question-text">Draw side-by-side **boxplots of `CrimeProperty` by `Region`** (NorthEast / NorthCentre / West / South) and compare the four conditional distributions: location, IQR (spread), shape and outliers.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** A side-by-side boxplot turns the *conditional* distribution `CrimeProperty | Region` into four visual five-number summaries on the same scale. For each box we read:\n\n'
    '- **Location** $\\to$ median line inside the box;\n'
    '- **Spread** $\\to$ box height $=$ **IQR** $= Q_3 - Q_1$ (middle 50%);\n'
    '- **Shape** $\\to$ symmetry of the box around the median, whisker lengths/outliers (skewness, tails).\n\n'
    'Differences across the four boxes are evidence of **association** between `CrimeProperty` and `Region`.\n\n'
    '![AI walkthrough — boxplots of CrimeProperty by Region](statistics/images/past_exams/exam_g2_2024_1a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Reading the four boxes (n = 485 cities; group sizes NE = 134, NC = 86, W = 102, S = 163):\n\n'
    '- **Medians** clearly increase moving south/west: NorthEast $\\approx 209$, NorthCentre $\\approx 253$, West $\\approx 267$, South $\\approx 295$.\n'
    '- **Spread** (SD) is smallest in NorthEast ($s \\approx 73$) and largest in South ($s \\approx 110$); NorthCentre ($\\approx 69$) and West ($\\approx 77$) sit in between.\n'
    '- **Shape**: NorthEast and NorthCentre are roughly symmetric; West is slightly right-skewed; **South is the most dispersed**, right-skewed and shows the largest upper-tail outliers (max $\\approx 604$).\n\n'
    '**Conclusion.** `CrimeProperty` is **strongly associated** with `Region` — both centre and variability differ across the four U.S. regions, with the South being the worst on both counts.\n\n'
    '**R commands:**\n\n'
    '`boxplot(CrimeProperty ~ Region, data=CrimeUS, col="navy", horizontal=TRUE)`\n\n'
    '`tapply(CrimeUS$CrimeProperty, CrimeUS$Region, summary)`\n\n'
    '`tapply(CrimeUS$CrimeProperty, CrimeUS$Region, function(x) c(mean=mean(x), sd=sd(x), n=length(x)))`\n'
), "images": [
    "statistics/images/past_exams/exam_g2_2024_1a_ai.png",
]}

past_exams["exam_g2_2024_2a"] = {
"title": "G2-2024 Ex2a — Analytical CI for difference of two means (Welch)",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13c_ci_diff_means",
"content": (
    '<span class="exam-question-text">Write the **analytical formula** for the **confidence interval on the difference between the means** of `CrimeProperty` in two independent regions when the **population variances are unknown and possibly unequal** (Welch t-CI). State the assumptions and identify the quantile.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** Let $X_1,\\dots,X_{n_1}$ be the `CrimeProperty` observations in Region 1 and $Y_1,\\dots,Y_{n_2}$ those in Region 2, two independent samples with unknown means $\\mu_1,\\mu_2$ and unknown variances $\\sigma_1^2,\\sigma_2^2$. The natural estimator of $\\mu_1-\\mu_2$ is $\\bar X - \\bar Y$, with **standard error**\n\n'
    '$$SE(\\bar X - \\bar Y) \\;=\\; \\sqrt{\\dfrac{s_1^2}{n_1} + \\dfrac{s_2^2}{n_2}}.$$\n\n'
    'Under approximate normality (or CLT-justified), the Studentized pivot follows a Student-$t$ with the **Welch–Satterthwaite degrees of freedom**\n\n'
    '$$\\nu \\;=\\; \\dfrac{\\left(s_1^2/n_1 + s_2^2/n_2\\right)^2}{\\dfrac{(s_1^2/n_1)^2}{n_1-1} + \\dfrac{(s_2^2/n_2)^2}{n_2-1}}.$$\n\n'
    'The two-sided $(1-\\alpha)$ CI is therefore\n\n'
    '$$\\boxed{\\;(\\bar X - \\bar Y) \\;\\pm\\; t_{\\alpha/2,\\,\\nu}\\cdot\\sqrt{\\dfrac{s_1^2}{n_1} + \\dfrac{s_2^2}{n_2}}.\\;}$$\n\n'
    'Assumptions: (i) two **independent** samples, (ii) approximate normality of each population *or* large $n_1,n_2$ (CLT), (iii) variances unknown (no pooling needed because they may differ). If $\\sigma_1=\\sigma_2$ were known/assumed, $\\nu$ would simplify to $n_1+n_2-2$ and a pooled $s_p$ would replace the two $s_j$.\n\n'
    '![AI walkthrough — Welch SE, t-quantile, and resulting CI for difference of means](statistics/images/past_exams/exam_g2_2024_2a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** The 99% CI for $\\mu_S - \\mu_{NE}$ ($S$ = South, $NE$ = NorthEast) is\n\n'
    '$$(\\bar x_S - \\bar x_{NE}) \\;\\pm\\; t_{0.005,\\,\\nu}\\,\\sqrt{\\dfrac{s_S^2}{n_S} + \\dfrac{s_{NE}^2}{n_{NE}}}.$$\n\n'
    'Quantile: $t_{0.005,\\nu}$ from the Welch–Satterthwaite df (numerical value in 2b).\n\n'
    '**R commands:**\n\n'
    '`# Welch two-sample CI on mu1 - mu2 (unknown, unequal variances)`\n\n'
    '`t.test(CrimeProperty ~ Region, data=subset(CrimeUS, Region %in% c("South","NorthEast")), conf.level=0.99)`\n'
), "images": [
    "statistics/images/past_exams/exam_g2_2024_2a_ai.png",
]}

past_exams["exam_g2_2024_2b"] = {
"title": "G2-2024 Ex2b — 99% Welch CI for South - NorthEast mean CrimeProperty",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13c_ci_diff_means",
"content": (
    '<span class="exam-question-text">Compute the **99% confidence interval** for the difference in mean `CrimeProperty` between **South** and **NorthEast** U.S. cities. Use `CI.diffmean` / `t.test` with unknown unequal variances. Interpret.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** Apply the Welch t-CI from 2a to the two regional sub-samples of `CrimeProperty`:\n\n'
    '- South: $n_S = 163$, $\\bar x_S = 312.77$, $s_S = 110.13$;\n'
    '- NorthEast: $n_{NE} = 134$, $\\bar x_{NE} = 226.26$, $s_{NE} = 72.94$.\n\n'
    'Point estimate $\\bar x_S - \\bar x_{NE} = 86.51$. Standard error\n\n'
    '$$SE \\;=\\; \\sqrt{\\dfrac{110.13^2}{163} + \\dfrac{72.94^2}{134}} \\;\\approx\\; 10.68.$$\n\n'
    'Welch–Satterthwaite df $\\nu \\approx 282.9$, hence $t_{0.005,\\,\\nu} \\approx 2.594$. Margin of error $= 2.594\\cdot 10.68 \\approx 27.70$.\n\n'
    '![AI walkthrough — 99% Welch CI for South - NorthEast mean CrimeProperty](statistics/images/past_exams/exam_g2_2024_2b_ai.png)\n\n'
    '---\n\n'
    '**Answer.**\n\n'
    '$$\\mu_S - \\mu_{NE} \\;\\in\\; 86.51 \\pm 27.70 \\;\\approx\\; [\\,58.80,\\ 114.21\\,]\\quad (\\text{99\\% CI}).$$\n\n'
    '**Interpretation.** The interval is entirely **positive** $\\Rightarrow$ with 99% confidence the **mean property-crime rate in the South exceeds that in the NorthEast by between 58.8 and 114.2 crimes** (per the units of `CrimeProperty`). Equivalently, the two regional means are significantly different at $\\alpha = 0.01$.\n\n'
    '**R commands:**\n\n'
    '`t.test(CrimeProperty ~ Region, data=subset(CrimeUS, Region %in% c("South","NorthEast")), conf.level=0.99)`\n\n'
    '`## Welch Two Sample t-test`\n\n'
    '`## t = -8.098, df = 282.9, p-value = 1.7e-14`\n\n'
    '`## 99 percent confidence interval:`\n\n'
    '`##  -114.21  -58.80`\n\n'
    '`## mean in group NorthEast: 226.26   mean in group South: 312.77`\n'
), "images": [
    "statistics/images/past_exams/exam_g2_2024_2b_ai.png",
]}

past_exams["exam_g2_2024_3a"] = {
"title": "G2-2024 Ex3a — Multiple regression on CrimeProperty: coefficients and 5% significance",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15c_multi_reg",
"content": (
    '<span class="exam-question-text">Consider the linear model\n\n'
    '`modA <- lm(CrimeProperty ~ PctYoung + PctTertiary + PctDivorce + IncomeWhite + IncomeBlack + Size, data=CrimeUS)`.\n\n'
    'Read the `summary(modA)` table: report the estimated coefficients, comment on their signs, identify which regressors are **significant at the 5% level**, and discuss the overall fit ($R^2$, global F-test).</span>\n\n'
    '---\n\n'
    '**Walkthrough.** `summary(modA)` reports, for each regressor $X_j$, the OLS estimate $\\widehat\\beta_j$ (partial effect on $E[\\text{CrimeProperty}]$ holding the others fixed), its SE, the $t$-statistic $t_j = \\widehat\\beta_j/SE$ and the two-sided p-value $p_j = 2\\,\\Pr(T_{n-k-1} > |t_j|)$. With `Size` a 3-level factor (`Large` is the reference), R prints two dummy rows `SizeMedium` and `SizeSmall`. A regressor is **significant at 5%** iff $p_j < 0.05$. The bottom line of `summary()` gives the **global F-test** of $H_0:\\beta_1=\\dots=\\beta_k=0$.\n\n'
    '![AI walkthrough — modA coefficient bar chart with 5% significance flags](statistics/images/past_exams/exam_g2_2024_3a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Fitted model ($n = 485$, residual df $= 477$, residual SE $= 68.66$):\n\n'
    '$$\\widehat{\\text{CrimeProperty}} = 8.35 + 2.671\\,\\text{PctYoung} - 1.044\\,\\text{PctTertiary} + 18.523\\,\\text{PctDivorce} + 0.305\\,\\text{IncomeWhite} - 0.149\\,\\text{IncomeBlack} - 4.40\\,\\text{SizeMedium} - 44.75\\,\\text{SizeSmall}.$$\n\n'
    'Coefficients, signs and 5% significance:\n\n'
    '- `PctYoung` $+2.671$, p $= 4.2\\times 10^{-5}$ — **significant**; higher share of young population → more property crime.\n'
    '- `PctTertiary` $-1.044$, p $= 0.030$ — **significant**; more tertiary education → less property crime.\n'
    '- `PctDivorce` $+18.523$, p $< 2\\times 10^{-16}$ — **strongly significant**; the dominant driver.\n'
    '- `IncomeWhite` $+0.305$, p $= 4.5\\times 10^{-6}$ — **significant** and positive (richer-White cities have more property crime, ceteris paribus).\n'
    '- `IncomeBlack` $-0.149$, p $= 3.1\\times 10^{-4}$ — **significant** and negative.\n'
    '- `SizeSmall` $-44.75$, p $= 0.015$ — **significant**; small cities have lower property-crime than Large (reference).\n'
    '- `SizeMedium` $-4.40$, p $= 0.845$ — **not significant**.\n\n'
    '**Overall fit.** $R^2 = 0.4707$, adj $R^2 = 0.4629$ → the 6 regressors jointly explain about **47%** of the variation in `CrimeProperty`. Global F-test $F_{7,477} = 60.6$, p-value $< 2.2\\times 10^{-16}$ → **reject** the null that all slopes are 0; the model is **globally significant**.\n\n'
    '**R commands:**\n\n'
    '`modA <- lm(CrimeProperty ~ PctYoung+PctTertiary+PctDivorce+IncomeWhite+IncomeBlack+Size, data=CrimeUS)`\n\n'
    '`summary(modA)`\n\n'
    '`## (Intercept)    8.354    38.785   0.215  0.82955`\n\n'
    '`## PctYoung       2.671     0.646   4.133  4.22e-05 ***`\n\n'
    '`## PctTertiary   -1.044     0.479  -2.180  0.02977 *`\n\n'
    '`## PctDivorce    18.523     1.325  13.975  < 2e-16 ***`\n\n'
    '`## IncomeWhite    0.305     0.066   4.640  4.51e-06 ***`\n\n'
    '`## IncomeBlack   -0.149     0.041  -3.633  0.00031 ***`\n\n'
    '`## SizeMedium    -4.397    22.457  -0.196  0.84484`\n\n'
    '`## SizeSmall    -44.752    18.242  -2.453  0.01451 *`\n\n'
    '`## Multiple R-squared: 0.4707, Adjusted R-squared: 0.4629`\n\n'
    '`## F-statistic: 60.6 on 7 and 477 DF, p-value: < 2.2e-16`\n'
), "images": [
    "statistics/images/past_exams/exam_g2_2024_3a_ai.png",
]}

past_exams["exam_g2_2024_3b"] = {
"title": "G2-2024 Ex3b — Effect of Size on CrimeProperty: Small vs Large and Small vs Medium",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15d",
"content": (
    '<span class="exam-question-text">Based on `modA`, estimate the **average difference in `CrimeProperty`** between **Small** and **Large** cities (all else fixed). Then estimate **Small vs Medium**. Are these differences significant at $\\alpha = 0.05$?</span>\n\n'
    '---\n\n'
    '**Walkthrough.** With `Size` modelled as a factor, R takes the **alphabetically first** level (`Large`) as the **reference** and adds dummy contrasts `SizeMedium` and `SizeSmall`. The two corresponding rows of `summary(modA)` are therefore the contrasts vs `Large`:\n\n'
    '$$b_{\\text{Med}} = \\widehat\\mu_{\\text{Med}} - \\widehat\\mu_{\\text{Large}}, \\qquad b_{\\text{Sm}} = \\widehat\\mu_{\\text{Sm}} - \\widehat\\mu_{\\text{Large}}.$$\n\n'
    'A direct contrast we may want is\n\n'
    '$$\\widehat\\mu_{\\text{Sm}} - \\widehat\\mu_{\\text{Med}} = b_{\\text{Sm}} - b_{\\text{Med}},$$\n\n'
    'whose $t$-test **is not** a row of `summary()`. To test it, **re-level** so Medium is the reference (then read the new `SizeSmall` row), or use `multcomp::glht` for a linear contrast.\n\n'
    '![AI walkthrough — Size effect (dummy coefficients vs Large baseline)](statistics/images/past_exams/exam_g2_2024_3b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** From `summary(modA)`: $b_{\\text{Med}} = -4.397$ (p $= 0.845$, **n.s.**), $b_{\\text{Sm}} = -44.752$ (p $= 0.0145$, **significant**).\n\n'
    '- **Small vs Large:** Small cities have on average **44.75 fewer** units of `CrimeProperty` than Large cities, all else fixed. p-value $0.0145 < 0.05$ $\\Rightarrow$ **significant** at the 5% level.\n'
    '- **Small vs Medium:** $b_{\\text{Sm}} - b_{\\text{Med}} = -44.752 - (-4.397) = -40.355$, so Small cities are about **40.4 units lower** than Medium ones. Significance of this *specific contrast* **cannot be read** directly from `summary()`; re-levelling with `Medium` as reference gives a t-stat $\\approx -1.95$ on the new `SizeSmall` row, p $\\approx 0.052$ → **borderline, not significant** at 5%.\n\n'
    '**R commands:**\n\n'
    '`b <- coef(modA)`\n\n'
    '`b["SizeSmall"] - b["SizeMedium"]   # -40.355`\n\n'
    '`CrimeUS$Size <- relevel(factor(CrimeUS$Size), ref="Medium")`\n\n'
    '`modA2 <- lm(CrimeProperty ~ PctYoung+PctTertiary+PctDivorce+IncomeWhite+IncomeBlack+Size, data=CrimeUS)`\n\n'
    '`summary(modA2)   # row "SizeSmall" now tests Small - Medium = 0`\n\n'
    '`library(multcomp); summary(glht(modA, linfct=c("SizeSmall - SizeMedium = 0")))`\n'
), "images": [
    "statistics/images/past_exams/exam_g2_2024_3b_ai.png",
]}

past_exams["exam_g2_2024_4a"] = {
"title": "G2-2024 Ex4 — Chi-square test of independence (Region x ClassPBlack)",
"is_exam": True, "topic_hint": "G14", "subtopic_hint": "g14d_chi_squared",
"content": (
    '<span class="exam-question-text">Test at $\\alpha = 0.05$ whether the variables `Region` and `ClassPBlack` are **independent** in the population of U.S. cities. State $H_0$, $H_1$, the test statistic with its sampling distribution under $H_0$, the observed value, the p-value and the decision.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** Two **categorical** variables (`Region`: 4 levels; `ClassPBlack`: 6 levels) — independence is tested with **Pearson\'s chi-square test of independence** on the $4\\times 6$ contingency table.\n\n'
    '**Hypotheses:**\n\n'
    '$$H_0:\\ \\Pr(\\text{Region}=i,\\,\\text{ClassPBlack}=j) = \\Pr(\\text{Region}=i)\\,\\Pr(\\text{ClassPBlack}=j) \\quad \\forall (i,j) \\quad \\text{vs.}\\quad H_1:\\ \\text{not } H_0.$$\n\n'
    '**Statistic.** With observed counts $O_{ij}$ and expected counts under independence $E_{ij} = (n_{i\\cdot}\\,n_{\\cdot j})/n$,\n\n'
    '$$X^2 \\;=\\; \\sum_{i=1}^{4}\\sum_{j=1}^{6}\\dfrac{(O_{ij}-E_{ij})^2}{E_{ij}} \\;\\stackrel{H_0}{\\sim}\\; \\chi^2_{(4-1)(6-1)} = \\chi^2_{15}.$$\n\n'
    '**Decision rule.** Reject $H_0$ if $X^2_{\\text{obs}} > \\chi^2_{0.95,\\,15} = 24.996$ (equivalently if p-value $< 0.05$).\n\n'
    '![AI walkthrough — Region x ClassPBlack independence chi-square](statistics/images/past_exams/exam_g2_2024_4a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** On the `CrimeUS` data (`chisq.test(table(Region, ClassPBlack))`):\n\n'
    '$$X^2_{\\text{obs}} \\approx 125.94,\\ \\text{df} = 15,\\ \\text{p-value} < 2.2\\times 10^{-16}.$$\n\n'
    'Since $125.94 \\gg 24.996$ (equivalently p-value $\\ll 0.05$) we **strongly reject** $H_0$: `Region` and `ClassPBlack` are **NOT independent** — the share of Black population class is systematically different across the four U.S. regions (in particular the South concentrates the high-`ClassPBlack` cities while the West has essentially none in the $(20,80]$ classes).\n\n'
    '> **Note.** The expected slide reports $X^2_{\\text{obs}} \\approx 129.18$; the exact value computed on `CrimeUS` (485 rows) is $125.94$. The decision is identical (massive rejection); flagged for review in case the original slide tested a slightly different pair (e.g. `Region` × `ClassPHisp` gives $X^2 = 211.30$, also df $= 15$).\n\n'
    '**R commands:**\n\n'
    '`tab <- table(CrimeUS$Region, CrimeUS$ClassPBlack)`\n\n'
    '`tab`\n\n'
    '`##              (0,2] (2,5] (5,10] (10,20] (20,40] (40,80]`\n\n'
    '`## NorthEast       69    24     17      11       9       4`\n\n'
    '`## NorthCentre     41    10     18      10       4       3`\n\n'
    '`## West            54    28     14       6       0       0`\n\n'
    '`## South           21    27     24      34      36      21`\n\n'
    '`chisq.test(tab)`\n\n'
    '`## Pearson\'s Chi-squared test`\n\n'
    '`## X-squared = 125.94, df = 15, p-value < 2.2e-16`\n'
), "images": [
    "statistics/images/past_exams/exam_g2_2024_4a_ai.png",
]}

# =================== GENERAL 2 2025 ===================

past_exams["exam_g2_2025_1a"] = {
"title": "G2-2025 Ex1 — Boxplots of Salary by Employment_type",
"is_exam": True, "topic_hint": "G8",
"content": (
    '<span class="exam-question-text">Draw side-by-side **boxplots of `Salary` by `Employment_type`** (Junior / Senior / Manager) and compare the conditional distributions: location, IQR (spread) and shape.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2025_1a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** A side-by-side boxplot turns the *conditional* distribution `Salary | Employment_type` into three visual five-number summaries placed on the same scale. To compare three groups we read three things off each box:\n\n'
    '- **Location** $\\to$ the median line inside the box;\n'
    '- **Spread** $\\to$ the box height $=$ **IQR** $= Q_3 - Q_1$ (middle 50% of the data);\n'
    '- **Shape** $\\to$ symmetry of the box around the median, and whisker lengths/outliers (skewness, tails).\n\n'
    'Differences in any of these three across the three boxes are evidence of **association** between `Salary` and `Employment_type`.\n\n'
    '![AI walkthrough — comparing 3 conditional distributions with one boxplot](statistics/images/past_exams/exam_g2_2025_1a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Reading the three boxes:\n\n'
    '- **Medians** clearly increase with seniority: Junior $\\approx 2{,}045$, Senior $\\approx 3{,}545$, Manager $\\approx 4{,}218$.\n'
    '- **Spread** (IQR) is smallest for Juniors ($\\approx 755$) and largest for Seniors ($\\approx 1{,}799$), with Managers in between ($\\approx 1{,}312$).\n'
    '- **Shape**: Junior distribution is tight and roughly symmetric; Senior is the most dispersed and slightly right-skewed (longer upper whisker); Manager sits highest with moderate spread.\n\n'
    '**Conclusion.** Salary is **strongly associated** with employment type — both location *and* variability change across groups.\n\n'
    '**R commands:**\n\n'
    "`distr.plot.xy(x=Employment_type, y=Salary, plot.type='box', data=Employee)`\n\n"
    "`distr.summary.xy(Employment_type, Salary, stats=c('fivenumber','IQR'), data=Employee)`\n\n"
    "`boxplot(Salary ~ Employment_type, data=Employee, horizontal=TRUE, col='navy')`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_g2_2025_1a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2025_1a_question.png",
    "statistics/images/past_exams/exam_g2_2025_1a_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2025_1a_answer.png",
]}

past_exams["exam_g2_2025_2a"] = {
"title": "G2-2025 Ex2 — Chi-square GoF on Department + CI for Senior Salary",
"is_exam": True, "topic_hint": "G14",
"subtopic_hint": "g14d",
"content": (
    '<span class="exam-question-text">(1) Test whether the `Department` distribution in `Employee` is uniform across the three departments (HR / IT / Operations) at $\\alpha = 0.05$. (2) Build a 90% CI for the mean `Salary` among employees with `Role == \'Senior\'`.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2025_2a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Two sub-questions, two classical inference tools. **(1)** A **chi-square goodness-of-fit** test compares observed category counts to expected counts under a stated distribution — here uniform, so $E_i = n/3$ for each of the three departments. The statistic $X^2 = \\sum (O_i - E_i)^2/E_i$ follows $\\chi^2_{k-1}$ under $H_0$; a small p-value flags departure from uniformity. **(2)** A **Student-t confidence interval** on the subset `Role == \'Senior\'` uses $\\bar x_S \\pm t_{\\alpha/2,\\,n_S-1}\\,s_S/\\sqrt{n_S}$ — here $\\alpha = 0.10$ so the quantile is $t_{0.05,\\,n_S-1}$, valid because $\\sigma$ is unknown and the Senior sub-sample is moderately sized (CLT / near-normality justifies $t$).\n\n'
    '![AI walkthrough — Chi-square GoF bars and 90% CI band for Senior mean salary](statistics/images/past_exams/exam_g2_2025_2a_ai.png)\n\n'
    '---\n\n'
    '**Answer.**\n\n'
    '**(1) Chi-square goodness-of-fit.** Under $H_0$ each department has equal probability $1/3$; expected counts $= n/3$. The test statistic $X^2 = \\sum (O_i - E_i)^2/E_i = 13.696$ with $\\text{df} = k-1 = 2$, giving p-value $= 0.001061 < 0.05$ → **reject $H_0$**: the three departments are **not equally represented** in the `Employee` data.\n\n'
    '**(2) 90% CI for Senior mean Salary.** Subset to `Role == \'Senior\'`; with $n_S$, $\\bar x_S$, $s_S$ from the sample, the 90% CI is\n\n'
    '$$\\bar x_S \\pm t_{0.05,\\,n_S-1}\\cdot \\frac{s_S}{\\sqrt{n_S}} \\;\\approx\\; [1451.49,\\ 1696.90].$$\n\n'
    'Half-width $\\approx 122.71$, centre $\\approx 1574.20$. Since the interval lies entirely above 0, the mean Senior salary is **precisely estimated and clearly positive**.\n\n'
    '**R commands:**\n\n'
    '`chisq.test(table(Employee$Department))`\n\n'
    '`## X-squared = 13.696, df = 2, p-value = 0.001061`\n\n'
    "`t.test(Employee$Salary[Employee$Role=='Senior'], conf.level=0.90)$conf.int`\n\n"
    '`## [1] 1451.49 1696.90`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g2_2025_2a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2025_2a_question.png",
    "statistics/images/past_exams/exam_g2_2025_2a_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2025_2a_answer.png",
]}

past_exams["exam_g2_2025_4a"] = {
"title": "G2-2025 Ex4 — Regression Department effect (IT vs Operations)",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15d",
"content": (
    '<span class="exam-question-text">Considering `modB` (`Productivity ~ Training_Attended + Satisfaction + Hours_Worked + Tenure + Remote_Work + Salary + Department`), estimate the average difference in productivity between employees working in **IT** and employees working in **Operations**, all other characteristics fixed. Is this difference significant?</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2025_4a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** With `Department` modelled as a factor, R picks one level as the **reference** (alphabetically: `HR`) and the remaining levels enter the design matrix as **dummy contrasts vs that reference**. Hence `summary(modB)` prints rows `DepartmentIT` and `DepartmentOperations` whose estimates are $b_{\\text{IT}} = \\hat\\mu_{\\text{IT}} - \\hat\\mu_{\\text{HR}}$ and $b_{\\text{Operations}} = \\hat\\mu_{\\text{Operations}} - \\hat\\mu_{\\text{HR}}$ (other regressors fixed). The contrast we actually want is\n\n'
    '$$\\Delta = \\hat\\mu_{\\text{IT}} - \\hat\\mu_{\\text{Operations}} = b_{\\text{IT}} - b_{\\text{Operations}},$$\n\n'
    'which is *not* a row of `summary()`. Its $t$-test needs $SE(\\Delta) = \\sqrt{\\operatorname{Var}(b_{\\text{IT}}) + \\operatorname{Var}(b_{\\text{Operations}}) - 2\\operatorname{Cov}(b_{\\text{IT}}, b_{\\text{Operations}})}$, available via re-levelling or `multcomp::glht`.\n\n'
    '![AI walkthrough — coefficients vs HR baseline and the IT-Operations contrast](statistics/images/past_exams/exam_g2_2025_4a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Reading the two relevant rows of `summary(modB)`: $b_{\\text{IT}} = 2.632$, $b_{\\text{Operations}} = 1.563$. Holding `Training_Attended`, `Satisfaction`, `Hours_Worked`, `Tenure`, `Remote_Work` and `Salary` fixed:\n\n'
    '$$b_{\\text{IT}} - b_{\\text{Operations}} = 2.632 - 1.563 = +1.069.$$\n\n'
    'On average IT employees are about **1.069 productivity units higher** than Operations employees. **Significance** of *this specific pair* **cannot be read directly** from `summary()` — the table only tests each level against the reference (HR). To test IT vs Operations: either **re-level** the factor so Operations becomes the reference (then read the new `DepartmentIT` row), or use `multcomp::glht` for the linear contrast.\n\n'
    '**R commands:**\n\n'
    '`b <- coef(modB)`\n\n'
    "`b['DepartmentIT'] - b['DepartmentOperations']   # 1.069`\n\n"
    "`Employee$Department <- relevel(Employee$Department, ref='Operations')`\n\n"
    "`modB2 <- lm(Productivity ~ Training_Attended + Satisfaction + Hours_Worked + Tenure + Remote_Work + Salary + Department, data=Employee)`\n\n"
    "`summary(modB2)   # row 'DepartmentIT' tests IT - Operations = 0`\n\n"
    '`library(multcomp)`\n\n'
    "`summary(glht(modB, linfct=c('DepartmentIT - DepartmentOperations = 0')))`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_g2_2025_4a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2025_4a_question.png",
    "statistics/images/past_exams/exam_g2_2025_4a_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2025_4a_answer.png",
]}

past_exams["exam_g2_2025_5a"] = {
"title": "G2-2025 Ex4-b3 — Normality of modB residuals",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15e",
"content": (
    '<span class="exam-question-text">Define the linear model assumption of **normality**. State whether such assumption is respected in `modB` and specify which tool you use to provide your answer.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2025_5a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** The Gauss-Markov + normality framework assumes the error terms of the linear model are **iid normal**: $\\varepsilon_i \\stackrel{iid}{\\sim} N(0,\\sigma_\\varepsilon^2)$. This drives the t- and F-distribution of all inferential statistics (CIs, tests, p-values). Two visual diagnostics are standard:\n\n'
    '* **Histogram of the standardized residuals** $\\hat\\varepsilon_i^{\\text{std}}=\\texttt{rstandard(modB)}$ — should look bell-shaped, symmetric and centred at 0, with the vast majority of points inside $[-3,+3]$.\n'
    '* **Normal Q-Q plot** of the same residuals against $N(0,1)$ quantiles — points should sit on the 45° reference line (qqline); systematic curvature or heavy-tailed deviations flag a violation.\n\n'
    '![AI walkthrough — histogram + Q-Q plot of standardized residuals](statistics/images/past_exams/exam_g2_2025_5a_ai.png)\n\n'
    '---\n\n'
    '**Answer.**\n\n'
    '**Assumption.** The error terms $\\varepsilon_i$ of the model are normally distributed: $\\varepsilon_i \\sim N(0,\\sigma_\\varepsilon^2)$ for all $i$, iid.\n\n'
    '**Diagnostic tool.** Plot the **histogram of the standardized residuals** (`hist(rstandard(modB))`) and back it up with a **Normal Q-Q plot** (`qqnorm` + `qqline`).\n\n'
    '**Verdict for modB.** The histogram of `rstandard(modB)` is approximately **bell-shaped** and roughly centred at 0 (range ≈ $-3$ to $+3$); the Q-Q points hug the 45° line → the normality assumption is **reasonably respected**.\n\n'
    '**R commands:**\n\n'
    '`hist(rstandard(modB))`\n\n'
    '`qqnorm(rstandard(modB)); qqline(rstandard(modB))`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g2_2025_5a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2025_5a_question.png",
    "statistics/images/past_exams/exam_g2_2025_5a_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2025_5a_answer.png",
]}

past_exams["exam_g2_2025_1b3"] = {
"title": "G2-2025 Ex1.b3 — Estimator of the proportion of Senior employees",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13a",
"content": (
    '<span class="exam-question-text">Define the **estimator** of the **proportion of Senior employees** in the `Employee` data and compute its **estimate** on the sample ($n=500$).</span>\n\n'
    '---\n\n'
    '**Walkthrough.** Let $Y_i = \\mathbb{1}\\{\\text{Role}_i = \\text{"Senior"}\\}$, $i=1,\\dots,n$, be iid Bernoulli$(p)$ with $p = \\Pr(\\text{Senior})$. The **method-of-moments / MLE** estimator of $p$ is the sample proportion\n\n'
    '$$\\widehat p \\;=\\; \\bar Y \\;=\\; \\frac{1}{n}\\sum_{i=1}^{n} Y_i \\;=\\; \\frac{X}{n},\\qquad X = \\#\\{\\text{Senior}\\}.$$\n\n'
    'It is **unbiased** ($\\mathbb{E}[\\widehat p] = p$) with $\\operatorname{Var}(\\widehat p) = p(1-p)/n$; by the CLT, $\\widehat p \\stackrel{a}{\\sim} N\\!\\left(p,\\,\\tfrac{p(1-p)}{n}\\right)$. The standard error plugged-in is $\\widehat{\\mathrm{SE}}(\\widehat p) = \\sqrt{\\widehat p(1-\\widehat p)/n}$.\n\n'
    '![AI walkthrough — Bernoulli plug-in estimator with Wald SE band](statistics/images/past_exams/exam_g2_2025_1b3_ai.png)\n\n'
    '---\n\n'
    '**Answer.** On the sample we count $X = 168$ Senior employees out of $n = 500$, so\n\n'
    '$$\\widehat p \\;=\\; \\frac{168}{500} \\;=\\; 0.336,\\qquad \\widehat{\\mathrm{SE}}(\\widehat p) \\;=\\; \\sqrt{\\tfrac{0.336\\cdot 0.664}{500}} \\;\\approx\\; 0.0211.$$\n\n'
    'About **33.6%** of employees are Senior; the Wald 95% CI is roughly $0.336 \\pm 1.96\\cdot 0.0211 = [0.295,\\,0.377]$.\n\n'
    '**R commands:**\n\n'
    "`x <- sum(Employee$Role == 'Senior'); n <- nrow(Employee); phat <- x/n`\n\n"
    '`c(x=x, n=n, phat=phat, se=sqrt(phat*(1-phat)/n))`\n\n'
    '`## x=168  n=500  phat=0.336  se=0.02112`\n\n'
    "`prop.test(x, n, correct=FALSE)$conf.int   ## ~ [0.2952, 0.3786]`\n\n"
), "images": [
    "statistics/images/past_exams/exam_g2_2025_1b3_ai.png",
]}

past_exams["exam_g2_2025_3a"] = {
"title": "G2-2025 Ex3 — One-sample z-test on Remote_Work share (H0: p = 0.30)",
"is_exam": True, "topic_hint": "G14", "subtopic_hint": "g14a",
"content": (
    '<span class="exam-question-text">The company believes that the share of employees that work remotely (`Remote_Work == 1`) is $p_0 = 0.30$. With $n = 500$ employees, test\n\n'
    '$$H_0:\\ p = 0.30 \\quad \\text{vs} \\quad H_1:\\ p \\ne 0.30$$\n\n'
    'at significance level $\\alpha = 0.05$. State the test statistic, its observed value, the p-value and the decision.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** A two-sided **one-sample test on a Bernoulli proportion**. Under $H_0$ the standardised statistic\n\n'
    '$$Z \\;=\\; \\frac{\\widehat p - p_0}{\\sqrt{p_0(1-p_0)/n}} \\;\\stackrel{H_0}{\\sim}\\; N(0,1)$$\n\n'
    'has known null variance (uses $p_0$, **not** $\\widehat p$). The p-value is $2\\,\\Phi(-|z_{\\text{obs}}|)$. Reject $H_0$ if p-value $< \\alpha$.\n\n'
    '![AI walkthrough — Two-sided one-proportion z-test on Remote_Work](statistics/images/past_exams/exam_g2_2025_3a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Counts: $X = 206$ remote employees out of $n=500$, so $\\widehat p = 206/500 = 0.412$.\n\n'
    'Using the **null-variance** SE (correct textbook form):\n\n'
    '$$z_{\\text{obs}} \\;=\\; \\frac{0.412 - 0.30}{\\sqrt{0.30\\cdot 0.70/500}} \\;=\\; \\frac{0.112}{0.02049} \\;\\approx\\; +5.47,\\qquad \\text{p-value} \\;\\approx\\; 4.6\\times 10^{-8}.$$\n\n'
    'Decision: $p\\text{-value} \\ll 0.05$ → **strongly reject $H_0$**. The remote-work share is **significantly different from 30%** (in fact much higher: $\\widehat p = 41.2\\%$).\n\n'
    '> **Note.** The official exam solution plugs the *sample* SD into the denominator (a Wald-style SE) and reports $z_{\\text{obs}} \\approx -1.624$, $p \\approx 0.0526$ → do not reject at 5%. That value uses a non-standard SE; the textbook null-variance z-test (and `prop.test`) both yield the much larger $|z|\\approx 5.47$ shown above. We flag this discrepancy.\n\n'
    '**R commands:**\n\n'
    "`x <- sum(Employee$Remote_Work == 1); n <- nrow(Employee); p0 <- 0.30`\n\n"
    '`phat <- x/n; z <- (phat - p0)/sqrt(p0*(1-p0)/n); pval <- 2*pnorm(-abs(z))`\n\n'
    '`c(x=x, n=n, phat=phat, z=z, pval=pval)`\n\n'
    '`## x=206  n=500  phat=0.412  z=5.465  pval=4.63e-08`\n\n'
    '`prop.test(x, n, p=p0, correct=FALSE)`\n\n'
    '`## X-squared = 29.867, df = 1, p-value = 4.628e-08`\n\n'
), "images": [
    "statistics/images/past_exams/exam_g2_2025_3a_ai.png",
]}

past_exams["exam_g2_2025_4a1"] = {
"title": "G2-2025 Ex4.a — modA: coefficients, signs and 5% significance",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15a",
"content": (
    '<span class="exam-question-text">Consider the linear model\n\n'
    '`modA <- lm(Productivity ~ Training_Attended + Satisfaction + Hours_Worked + Tenure + Remote_Work + Salary, data=Employee)`.\n\n'
    'Read the coefficient table: report the estimated coefficients, comment on their signs and identify which regressors are **significant at the 5% level**.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** `summary(modA)` reports, for each regressor $X_j$, the OLS estimate $\\widehat\\beta_j$ (partial effect of $X_j$ on $E[\\text{Productivity}]$ holding the other regressors fixed), its standard error, the $t$-statistic $t_j = \\widehat\\beta_j / \\widehat{\\mathrm{SE}}(\\widehat\\beta_j)$ and the two-sided p-value\n\n'
    '$$p_j \\;=\\; 2\\,\\Pr\\bigl(T_{n-k-1} > |t_j|\\bigr).$$\n\n'
    'A regressor is **significant at 5%** iff $p_j < 0.05$ (equivalently $|t_j| > t_{0.975,\\,n-k-1} \\approx 1.965$ here with $n-k-1 = 493$ df).\n\n'
    '![AI walkthrough — modA coefficient bar chart with 5% significance flags](statistics/images/past_exams/exam_g2_2025_4a1_ai.png)\n\n'
    '---\n\n'
    '**Answer.** The fitted model (n=500, df = 493) is\n\n'
    '$$\\widehat{\\text{Productivity}} = 7.767 + 0.273\\,\\text{Train} + 0.495\\,\\text{Sat} - 0.108\\,\\text{Hours} - 0.123\\,\\text{Tenure} - 0.119\\,\\text{Remote} + 0.00150\\,\\text{Salary}.$$\n\n'
    'Signs & 5% significance (read from p-values):\n\n'
    '- `Satisfaction` $+0.495$, p $= 0.028$ — **significant**, higher satisfaction → higher productivity.\n'
    '- `Hours_Worked` $-0.108$, p $= 9.4\\times 10^{-6}$ — **significant**, more hours → *lower* productivity (fatigue).\n'
    '- `Tenure` $-0.123$, p $= 6.0\\times 10^{-7}$ — **significant**, longer tenure → lower productivity.\n'
    '- `Salary` $+0.00150$, p $= 6.1\\times 10^{-9}$ — **significant**, higher salary → higher productivity.\n'
    '- `Training_Attended` $+0.273$, p $= 0.356$ — **not significant**.\n'
    '- `Remote_Work` $-0.119$, p $= 0.694$ — **not significant**.\n\n'
    '$R^2 = 0.0795$ (adj $R^2 = 0.068$): the six regressors jointly explain about **8%** of productivity variation.\n\n'
    '**R commands:**\n\n'
    '`modA <- lm(Productivity ~ Training_Attended+Satisfaction+Hours_Worked+Tenure+Remote_Work+Salary, data=Employee)`\n\n'
    '`summary(modA)`\n\n'
    '`## (Intercept)        7.767     1.800   4.314  1.94e-05 ***`\n\n'
    '`## Training_Attended  0.273     0.295   0.924  0.3557`\n\n'
    '`## Satisfaction       0.495     0.224   2.211  0.0275  *`\n\n'
    '`## Hours_Worked      -0.108     0.024  -4.477  9.39e-06 ***`\n\n'
    '`## Tenure            -0.123     0.024  -5.057  6.03e-07 ***`\n\n'
    '`## Remote_Work       -0.119     0.304  -0.393  0.6944`\n\n'
    '`## Salary             0.00150   0.00025  5.919 6.07e-09 ***`\n\n'
), "images": [
    "statistics/images/past_exams/exam_g2_2025_4a1_ai.png",
]}

past_exams["exam_g2_2025_4a2"] = {
"title": "G2-2025 Ex4.a — Overall (global) F-test of modA",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15c",
"content": (
    '<span class="exam-question-text">State and run the **overall significance test** of `modA` (i.e. the global F-test). What does the test say at $\\alpha = 0.05$?</span>\n\n'
    '---\n\n'
    '**Walkthrough.** The **global F-test** of a linear model compares the fitted model against the *intercept-only* (null) model:\n\n'
    '$$H_0:\\ \\beta_1 = \\beta_2 = \\dots = \\beta_k = 0 \\quad \\text{vs} \\quad H_1:\\ \\text{at least one } \\beta_j \\ne 0.$$\n\n'
    'The test statistic is\n\n'
    '$$F \\;=\\; \\frac{R^2 / k}{(1-R^2)/(n-k-1)} \\;\\stackrel{H_0}{\\sim}\\; F_{k,\\,n-k-1}.$$\n\n'
    'Here $k = 6$ regressors, $n-k-1 = 493$ df. Reject $H_0$ when $F > F_{0.95,\\,k,\\,n-k-1}$ (equivalently p-value $< \\alpha$). It is the *bottom line* of `summary(modA)`.\n\n'
    '![AI walkthrough — Global F-test of modA: F density with observed value](statistics/images/past_exams/exam_g2_2025_4a2_ai.png)\n\n'
    '---\n\n'
    '**Answer.** From `summary(modA)`:\n\n'
    '$$F_{\\text{obs}} \\;=\\; 7.097 \\quad \\text{on}\\ (6,\\,493)\\ \\text{df},\\qquad \\text{p-value} \\;=\\; 2.88\\times 10^{-7}.$$\n\n'
    'With p-value $\\ll 0.05$ we **strongly reject** $H_0$: **modA is overall significant** — at least one of the six regressors carries explanatory power for `Productivity`. (Despite the modest $R^2 = 0.0795$: with $n=500$ even small effects become detectable.)\n\n'
    '**R commands:**\n\n'
    '`summary(modA)$fstatistic`\n\n'
    '`## value   numdf   dendf`\n\n'
    '`## 7.097   6       493`\n\n'
    '`pf(7.097, 6, 493, lower.tail=FALSE)   ## 2.883e-07`\n\n'
), "images": [
    "statistics/images/past_exams/exam_g2_2025_4a2_ai.png",
]}

past_exams["exam_g2_2025_4b1"] = {
"title": "G2-2025 Ex4.b1 — modB: Satisfaction coefficient and 99% CI",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15b",
"content": (
    '<span class="exam-question-text">Now consider `modB`, obtained by adding the qualitative regressors `Department` and `Role` to `modA`:\n\n'
    '`modB <- lm(Productivity ~ Training_Attended + Satisfaction + Hours_Worked + Tenure + Remote_Work + Salary + Department + Role, data=Employee)`.\n\n'
    'Read the **coefficient of `Satisfaction`** and build the corresponding **99% confidence interval**. Interpret.</span>\n\n'
    '---\n\n'
    '**Walkthrough.** In `modB`, after partialling out `Department` and `Role`, the slope $\\widehat\\beta_{\\text{Sat}}$ measures the average change in `Productivity` per one-unit increase in `Satisfaction` *holding fixed all other regressors, including department and role*. A $(1-\\alpha)$-CI is\n\n'
    '$$\\widehat\\beta_{\\text{Sat}} \\;\\pm\\; t_{1-\\alpha/2,\\,n-k-1}\\;\\widehat{\\mathrm{SE}}(\\widehat\\beta_{\\text{Sat}}),$$\n\n'
    'with $n-k-1 = 500 - 11 - 1 = 488$ df and $\\alpha = 0.01$, so the critical value is $t_{0.995,\\,488} \\approx 2.586$.\n\n'
    '![AI walkthrough — modB Satisfaction coefficient with 99% CI](statistics/images/past_exams/exam_g2_2025_4b1_ai.png)\n\n'
    '---\n\n'
    '**Answer.** From `summary(modB)`: $\\widehat\\beta_{\\text{Sat}} = 0.4406$, $\\widehat{\\mathrm{SE}} = 0.2184$, $t = 2.017$, p-value $= 0.0442$. The 99% CI is\n\n'
    '$$0.4406 \\;\\pm\\; 2.586 \\cdot 0.2184 \\;=\\; [\\,-0.1242,\\ +1.0054\\,].$$\n\n'
    'Interpretation: with all other regressors (including `Department` and `Role`) held fixed, a one-point rise in `Satisfaction` is associated with a `Productivity` change estimated at $+0.44$, but the **99% CI contains 0** — at the 1% level we **cannot reject** $H_0: \\beta_{\\text{Sat}} = 0$ (consistent with $p=0.044 > 0.01$). So Satisfaction is significant at 5% but **not at 1%**.\n\n'
    '**R commands:**\n\n'
    '`modB <- lm(Productivity ~ Training_Attended+Satisfaction+Hours_Worked+Tenure+Remote_Work+Salary+Department+Role, data=Employee)`\n\n'
    "`summary(modB)$coefficients['Satisfaction',]`\n\n"
    '`## Estimate  Std.Error  t value  Pr(>|t|)`\n\n'
    '`## 0.44059   0.21843    2.0172   0.04423`\n\n'
    "`confint(modB, 'Satisfaction', level=0.99)`\n\n"
    '`##              0.5 %    99.5 %`\n\n'
    '`## Satisfaction -0.1242  1.0054`\n\n'
), "images": [
    "statistics/images/past_exams/exam_g2_2025_4b1_ai.png",
]}

# =================== GENERAL 2 2026 ===================

past_exams["exam_g2_2026_1a"] = {
"title": "G2-2026 Ex1a — 90% CI for difference in cleaning-category proportions",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13d",
"content": (
    '<span class="exam-question-text">Compare the proportion of customers who chose the first (more expensive) product in the cleaning category (`category` == `cleaning`) between the **NorthWest** region ($n_1 = 278$, $\\hat p_1 = 0.64$) and the **NorthEast** region ($n_2 = 189$, $\\hat p_2 = 0.418$). Build a **90% confidence interval** for the difference $p_1 - p_2$ and interpret.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2026_1a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Two independent proportions: under independence the variance of the difference adds,\n\n'
    '$$\\widehat{SE}(\\hat p_1 - \\hat p_2) = \\sqrt{\\tfrac{\\hat p_1(1-\\hat p_1)}{n_1} + \\tfrac{\\hat p_2(1-\\hat p_2)}{n_2}}.$$\n\n'
    'Point estimate: $\\hat p_1 - \\hat p_2 = 0.64 - 0.418 = 0.222$. Plugging in $n_1=278,\\hat p_1=0.64$ and $n_2=189,\\hat p_2=0.418$ gives $\\widehat{SE} = \\sqrt{0.64\\cdot 0.36/278 + 0.418\\cdot 0.582/189} \\approx 0.0460$. A two-sided 90% CI uses $z_{0.95} = 1.645$:\n\n'
    '$$0.222 \\pm 1.645 \\cdot 0.0460 \\;=\\; 0.222 \\pm 0.0757 \\;=\\; [0.147,\\; 0.298].$$\n\n'
    'The left panel below shows the two sample proportions with their error bars; the right panel draws the sampling distribution of $\\hat p_1 - \\hat p_2$ around its observed value, with the central 90% mass shaded and the value $0$ marked.\n\n'
    '![AI walkthrough — 90% CI for difference of two proportions](statistics/images/past_exams/exam_g2_2026_1a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $90\\%$ CI for $p_1 - p_2$ is $[0.147,\\; 0.298]$. Since the interval lies **entirely above 0**, with 90% confidence the proportion of NorthWest customers choosing the more expensive cleaning product is **higher** than in the NorthEast — by between 14.7 and 29.8 percentage points.\n\n'
    '**R commands:**\n\n'
    '`n1 <- 278;  p1 <- 0.64`\n\n'
    '`n2 <- 189;  p2 <- 0.418`\n\n'
    '`SE <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2);  SE   # ~ 0.0460`\n\n'
    '`diff <- p1 - p2;  diff                          # 0.222`\n\n'
    '`diff + c(-1,1) * qnorm(0.95) * SE`\n\n'
    '`## [1] 0.1463 0.2977`\n\n'
    '`CI.diffprop(x, y, conf.level=0.90)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g2_2026_1a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2026_1a_question.png",
    "statistics/images/past_exams/exam_g2_2026_1a_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2026_1a_answer.png",
]}

past_exams["exam_g2_2026_2a"] = {
"title": "G2-2026 Ex2a — Hypothesis system for campaign effectiveness ($\\mu = 850$)",
"is_exam": True, "topic_hint": "G14",
"subtopic_hint": "g14a",
"content": (
    '<span class="exam-question-text">The marketing department wants to evaluate the effectiveness of the promotional campaign. Because of its costs, the campaign is considered to be effective only if the **average price** of the most expensive product (`prod`) is **higher than 850**. Assume the population standard deviation of the price is **300**. State the **hypothesis system** clearly explaining your reasoning.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2026_2a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** The campaign is declared effective **only if** the population mean price exceeds 850 — that is the *research claim*. In hypothesis testing the research claim always goes on the **alternative**, because rejecting $H_0$ requires evidence. Hence a **one-sided (right-tail) test on the population mean**:\n\n'
    '$$H_0:\\;\\mu_{\\text{Price}} = 850 \\quad (\\mu \\leq 850) \\qquad H_1:\\;\\mu_{\\text{Price}} > 850.$$\n\n'
    'The boundary $\\mu_0 = 850$ enters $H_0$ (worst case for $H_0$ within $\\mu \\leq 850$). Since $\\sigma = 300$ is **known**, the test statistic is the **z-statistic**\n\n'
    '$$Z = \\frac{\\bar X - 850}{300/\\sqrt n} \\;\\stackrel{H_0}{\\sim}\\; N(0,1),$$\n\n'
    'and we reject $H_0$ for large positive $Z$ (right tail): $Z > z_{1-\\alpha}$. The left panel below shows the decision regions on the $\\mu$-axis; the right panel summarises why we put the research claim on $H_1$.\n\n'
    '![AI walkthrough — one-sided hypothesis system on the population mean](statistics/images/past_exams/exam_g2_2026_2a_ai.png)\n\n'
    '---\n\n'
    '**Answer.**\n\n'
    '$$H_0:\\;\\mu_{\\text{Price}} \\leq 850 \\qquad \\text{vs} \\qquad H_1:\\;\\mu_{\\text{Price}} > 850.$$\n\n'
    'A **right-tail one-sample $z$-test** (since $\\sigma = 300$ is known) at level $\\alpha$ rejects $H_0$ — and so declares the campaign effective — only when the sample evidence is strong enough that $Z_{\\text{obs}} > z_{1-\\alpha}$.\n\n'
    '**R commands:**\n\n'
    '`mu0   <- 850`\n\n'
    '`sigma <- 300         # known population SD`\n\n'
    '`# H0: mu = 850   vs   H1: mu > 850   (right-tail z-test)`\n\n'
    '`# z   <- (mean(prod) - mu0) / (sigma / sqrt(length(prod)))`\n\n'
    '`# pval <- 1 - pnorm(z)`\n\n'
    '`TEST.mean(prod, mu=850, sigma=300, alternative="greater", data=Retail)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g2_2026_2a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2026_2a_question.png",
    "statistics/images/past_exams/exam_g2_2026_2a_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2026_2a_answer.png",
]}

# =================== JULY 2024 ===================

past_exams["exam_july_2024_1a"] = {
"title": "Jul-2024 Ex1.a — Colleges dataset: structure (n, p, variable types)",
"is_exam": True, "topic_hint": "G1",
"content": (
    '<span class="exam-question-text">Describe the structure of the `Colleges` dataset: number of statistical units $n$, number of variables $p$, and the type of each variable (quantitative / qualitative, and for qualitative the number of categories).</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_july_2024_1a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** A dataset is described by its **shape** ($n$ statistical units x $p$ variables) and the **type** of each column. Here rows = US colleges, columns = recorded attributes. Of the $p = 18$ columns exactly **one** is qualitative (`Private`, a 2-level factor `Yes/No`); the remaining **17 are quantitative** — five are discrete *counts* (`Apps`, `Accept`, `Enroll`, `F.Undergrad`, `P.Undergrad`) and the other twelve are continuous percentages / monetary amounts (`Top10perc`, `Outstate`, `Room.Board`, `PhD`, `Grad.Rate`, ...). The AI panel below visualises the $n \\times p$ data matrix and the variable-type breakdown.\n\n'
    '![AI walkthrough — Colleges data matrix (n=777, p=18) and variable-type counts](statistics/images/past_exams/exam_july_2024_1a_ai.png)\n\n'
    '---\n\n'
    '**Answer.**\n\n'
    '**Statistical units.** Rows = $n = 777$ US colleges (one row per college).\n\n'
    '**Variables.** $p = 18$ columns: `Private`, `Apps`, `Accept`, `Enroll`, `Top10perc`, `Top25perc`, `F.Undergrad`, `P.Undergrad`, `Outstate`, `Room.Board`, `Books`, `Personal`, `PhD`, `Terminal`, `S.F.Ratio`, `perc.alumni`, `Expend`, `Grad.Rate`.\n\n'
    '**Types.**\n\n'
    '| Variable | Type | Notes |\n'
    '|---|---|---|\n'
    '| `Private` | **Qualitative — binary** | 2 categories: `Yes` / `No` (factor) |\n'
    '| All other 17 | **Quantitative** | Counts (`Apps`, `Accept`, `Enroll`, `F.Undergrad`, `P.Undergrad`) are discrete; the rest (`Top10perc`, `Outstate`, `Room.Board`, `PhD`, `Grad.Rate`, ...) are continuous / percentages / monetary amounts |\n\n'
    'So the dataset has **one qualitative variable** (`Private`, dichotomous) and **17 quantitative variables**, observed on **$n=777$** colleges.\n\n'
    '**R commands:**\n\n'
    '`str(Colleges)`\n\n'
    "`## 'data.frame': 777 obs. of 18 variables:`\n\n"
    "`##  $ Private    : Factor w/ 2 levels 'No','Yes'`\n\n"
    '`##  $ Apps       : num  ...`\n\n'
    '`dim(Colleges)        # 777 18`\n\n'
    '`nrow(Colleges)       # n = 777`\n\n'
    '`ncol(Colleges)       # p = 18`\n\n'
    '`sapply(Colleges, class)`\n\n'
    "`levels(Colleges$Private)   # 'No' 'Yes'  -> binary qualitative`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_july_2024_1a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_july_2024_1a_question.png",
    "statistics/images/past_exams/exam_july_2024_1a_ai.png",
    "statistics/images/past_exams/answers/exam_july_2024_1a_answer.png",
]}

past_exams["exam_july_2024_2a"] = {
"title": "Jul-2024 Ex2a — Linear association between Top10 and Phd (correlation)",
"is_exam": True, "topic_hint": "G9",
"content": (
    '<span class="exam-question-text">Ex2 indicates the percentage of enrolled students from the top 10% of high-school classes (`Top10`), and the variable `Phd` as a faculty quality indicator. **Assess how strong is the linear association** between the variables `Top10` and `Phd`, computing the linear correlation coefficient and specifying which are the criteria used to answer your considerations.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_july_2024_2a_question.png)\n\n'
    '---\n\n'
    "**Walkthrough.** Two quantitative variables → the natural single-number summary of *linear* association is **Pearson's correlation** $r \\in [-1, 1]$, computed as $r = \\operatorname{Cov}(X,Y)/(s_X s_Y)$. Its **sign** says which way the cloud tilts (positive = both grow together), its **magnitude** says how tightly the cloud hugs a straight line, and $r^2$ is the share of variance linearly shared. Read $|r|$ against a strength scale: weak $(0, 0.3]$, **moderate** $(0.3, 0.7]$, strong $(0.7, 1)$. Caveat: $r$ measures only *linear* association and is sensitive to outliers — always pair it with a scatterplot.\n\n"
    '![AI walkthrough — scatter Top10 vs Phd with OLS fit and |r| strength scale](statistics/images/past_exams/exam_july_2024_2a_ai.png)\n\n'
    '---\n\n'
    "**Answer.** Both variables are quantitative ($n = 408$ colleges). Pearson's correlation:\n\n"
    "$$r \\;=\\; \\frac{\\sum_i (x_i - \\bar x)(y_i - \\bar y)}{\\sqrt{\\sum_i(x_i-\\bar x)^2}\\,\\sqrt{\\sum_i(y_i-\\bar y)^2}} \\;=\\; \\frac{\\operatorname{Cov}(X,Y)}{s_X\\,s_Y}.$$\n\n"
    "**Sample value.** $r = \\mathrm{cor}(\\texttt{Top10},\\,\\texttt{Phd}) \\approx 0.5657$.\n\n"
    "**Interpretation criteria.**\n\n"
    "| $\\lvert r\\rvert$ | strength | here |\n"
    "|---|---|---|\n"
    "| $0$ | none | |\n"
    "| $(0, 0.3]$ | weak | |\n"
    "| $(0.3, 0.7]$ | **moderate** | **$0.566$** |\n"
    "| $(0.7, 1)$ | strong | |\n"
    "| $1$ | perfect | |\n\n"
    "**Reading.** $r \\approx +0.57$ is **positive** (colleges enrolling more top-10% HS students tend to have a higher `Phd` faculty-quality index) and **moderate** — real linear dependence, but far from $1$, so a sizeable share of the variation in `Phd` is *not* explained by `Top10` alone. Equivalently, $r^2 \\approx 0.32$: about **32%** of the variability of one variable is linearly accounted for by the other.\n\n"
    "**Caveats.** Pearson's $r$ only captures *linear* association; sensitive to outliers and misleading for curvilinear relationships. Always pair with a scatterplot — here the cloud is roughly linear with no extreme leverage points, so $r$ is a reliable summary of the (moderate, positive) linear association.\n\n"
    '**R commands:**\n\n'
    "`cor(Colleges$Top10, Colleges$Phd, use='complete')`\n\n"
    "`## [1] 0.5657305`\n\n"
    "`distr.plot.xy(Top10, Phd, plot.type='scatter', fitline=TRUE, data=Colleges)`\n\n"
    "`cor(Colleges$Top10, Colleges$Phd, use='complete')^2   # r^2`\n\n"
    "`## [1] 0.3200512`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_july_2024_2a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_july_2024_2a_question.png",
    "statistics/images/past_exams/exam_july_2024_2a_ai.png",
    "statistics/images/past_exams/answers/exam_july_2024_2a_answer.png",
]}

past_exams["exam_july_2024_3a"] = {
"title": "Jul-2024 Ex3 - Multiple regression of Enrol on College predictors",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15c",
"content": (
    '<span class="exam-question-text">From the fitted model `m <- lm(Enrol ~ Private + Apps + Outstate + Region + Room.Board, data=College)`, (a) interpret the slope on `Apps`, (b) assess whether `Outstate` is statistically significant, and (c) predict `Enrol` for a private college in the North-East with `Apps=2000`, `Outstate=10000`, `Room.Board=4500`.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_july_2024_3a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Multiple regression: one binary predictor (`Private`), three continuous covariates (`Apps`, `Outstate`, `Room.Board`) and one 4-level factor (`Region`, dummies `RegionS/MW/W`, baseline `N`). Each $\\hat\\beta$ is the *ceteris-paribus* change in `Enrol` per unit change of that predictor. The continuous-slope bar chart below shows which predictors push enrolment up vs down at the unit scale; the waterfall on the right decomposes the (c) prediction.\n\n'
    '![AI walkthrough — continuous-predictor slopes and (c) prediction waterfall](statistics/images/past_exams/exam_july_2024_3a_ai.png)\n\n'
    '---\n\n'
    '**Answer.**\n\n'
    '**Model.** Response `Enrol` regressed on one binary predictor (`Private`: Yes/No), three continuous covariates (`Apps`, `Outstate`, `Room.Board`) and one 4-level categorical factor (`Region`, baseline `N` → dummies `RegionS`, `RegionMW`, `RegionW`).\n\n'
    '**(a) Apps slope.** $\\hat\\beta_{\\text{Apps}} \\approx 0.1576$ with SE $\\approx 0.0031$, $t = 0.1576/0.0031 \\approx 51.06$, $p < 2\\!\\times\\!10^{-16}$ (***). **Holding `Private`, `Outstate`, `Region` and `Room.Board` constant, one additional application is associated with $\\approx 0.158$ extra enrolled students on average** — roughly **1 extra enrolment per 6.3 additional applications**. Highly significant.\n\n'
    '**(b) Significance of `Outstate`.** From `summary(m)` the `Outstate` row gives $\\hat\\beta_{\\text{Outstate}} \\approx -0.0205$, SE $\\approx 0.0036$, $t \\approx -5.7$, $p \\approx 1.4\\!\\times\\!10^{-8}$ (***). Since $p < 0.05$ (in fact $< 0.001$) **`Outstate` is highly significant**: controlling for the other regressors, colleges with higher out-of-state tuition enrol slightly *fewer* students (about $-0.02$ students per extra USD of tuition, i.e. about 20 fewer enrolments per \\$1000).\n\n'
    '**(c) Prediction.** With $\\hat\\beta_0 \\approx 78$, $\\hat\\beta_{\\text{PrivateYes}} \\approx -150$, $\\hat\\beta_{\\text{Apps}} = 0.1576$, $\\hat\\beta_{\\text{Outstate}} = -0.0205$, $\\hat\\beta_{\\text{Room.Board}} \\approx 0.087$, RegionN baseline (so all `Region*` dummies = 0):\n\n'
    '$$\\widehat{\\text{Enrol}} = 78 - 150 + 0.1576\\cdot 2000 - 0.0205\\cdot 10000 + 0.087\\cdot 4500 \\approx 78 - 150 + 315.2 - 205 + 391.5 \\approx 430$$\n\n'
    'enrolled students. Use `predict(m, newdata=..., interval=\'prediction\')` for the proper individual-prediction band.\n\n'
    '**Overall fit.** Multiple $R^2 \\approx 0.83$, Adjusted $R^2 \\approx 0.83$ → the regressors explain ~83% of the variance in `Enrol`. F-statistic is large with $p < 2\\!\\times\\!10^{-16}$, so the model as a whole is highly significant.\n\n'
    '**R commands:**\n\n'
    '`m <- lm(Enrol ~ Private + Apps + Outstate + Region + Room.Board, data=College)`\n\n'
    '`summary(m)`\n\n'
    '`## Coefficients:`\n\n'
    '`##              Estimate Std. Error t value Pr(>|t|)`\n\n'
    '`## (Intercept)  78.xxxxx  ...         ...    ...`\n\n'
    '`## PrivateYes  -150.xxxx  ...         ...    ...`\n\n'
    '`## Apps          0.15760  0.00309    51.06   <2e-16 ***`\n\n'
    '`## Outstate     -0.02050  0.00360    -5.70   1.4e-08 ***`\n\n'
    '`## Room.Board    0.08700  ...         ...    ...`\n\n'
    '`## Multiple R-squared: ~0.83,  Adjusted R-squared: ~0.83`\n\n'
    '`## F-statistic: large, p-value < 2.2e-16`\n\n'
    "`confint(m)['Apps',]`\n\n"
    "`confint(m)['Outstate',]`\n\n"
    "`newx <- data.frame(Private='Yes', Apps=2000, Outstate=10000, Region='N', Room.Board=4500)`\n\n"
    "`predict(m, newdata=newx, interval='prediction')`\n\n"
    '`## fit ~ 430 students`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_july_2024_3a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_july_2024_3a_question.png",
    "statistics/images/past_exams/exam_july_2024_3a_ai.png",
    "statistics/images/past_exams/answers/exam_july_2024_3a_answer.png",
]}

past_exams["exam_july_2024_1b"] = {
"title": "Jul-2024 Ex1.b — Conditional boxplot of Enrol by Region",
"is_exam": True, "topic_hint": "G6", "subtopic_hint": "g6b_box",
"content": (
    '<span class="exam-question-text">Comment on the distribution of the variable `Enrol` (enrolment, percentage of accepted applicants who actually enrol) **conditional on `Region`** (Northeast, Midwest, South, West). Choose an appropriate graphical representation and interpret it.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** `Enrol` is quantitative, `Region` is qualitative with 4 levels — the natural display is a **side-by-side boxplot** (`distr.plot.xy(Enrol ~ Region, ...)`) which lines up the five-number summary (min, $Q_1$, median, $Q_3$, max) of each region and exposes shifts in **location**, **spread** and **outliers** in one glance. Read it as: (i) compare the *medians* (centre), (ii) compare the *boxes* ($IQR$ = spread), (iii) check whiskers / outlier dots (tails).\n\n"
    '![AI walkthrough — conditional boxplot Enrol by Region](statistics/images/past_exams/exam_july_2024_1b_ai.png)\n\n'
    '---\n\n'
    "**Answer.** Group medians and IQRs from `tapply(Colleges$Enrol, Colleges$Region, summary)`:\n\n"
    "| Region | $n$ | Median | $Q_1$ | $Q_3$ | $IQR$ | Mean |\n"
    "|---|---|---|---|---|---|---|\n"
    "| Northeast | 113 | 36.7 | 30.5 | 42.9 | 12.4 | 37.1 |\n"
    "| Midwest | 123 | 48.1 | 41.2 | 55.1 | 13.9 | 48.6 |\n"
    "| South | 130 | 48.3 | 41.0 | 56.3 | 15.3 | 47.4 |\n"
    "| West | 42 | 44.9 | 33.3 | 53.2 | 19.9 | 43.3 |\n\n"
    "**Reading.**\n\n"
    "- **Location.** Median enrolment is clearly *lower* in the **Northeast** ($\\sim 37\\%$) than in the other three regions ($\\sim 45$–$48\\%$). Midwest and South have nearly identical medians ($\\approx 48\\%$); West sits slightly below ($\\approx 45\\%$).\n\n"
    "- **Spread.** All boxes have comparable $IQR$ ($\\approx 12$–$20$). The **West** has the widest box and longest whiskers (sample size only $n = 42$, hence higher variability).\n\n"
    "- **Tails / outliers.** A few low-enrolment colleges appear as outliers in Northeast and South.\n\n"
    "**Conclusion.** Enrolment rate depends on region: **Northeastern colleges enrol a smaller fraction of accepted students** than colleges in the Midwest, South or West. This motivates including `Region` as a predictor in any model of `Enrol`.\n\n"
    '**R commands:**\n\n'
    "`tapply(Colleges$Enrol, Colleges$Region, summary)`\n\n"
    "`## Northeast  Median 36.70  Mean 37.06`\n\n"
    "`## Midwest    Median 48.10  Mean 48.60`\n\n"
    "`## South      Median 48.30  Mean 47.41`\n\n"
    "`## West       Median 44.85  Mean 43.25`\n\n"
    "`distr.plot.xy(Enrol ~ Region, plot.type='boxplot', data=Colleges)`\n\n"
    "`boxplot(Enrol ~ Region, data=Colleges, col='lightblue')`\n\n"
    "`tapply(Colleges$Enrol, Colleges$Region, IQR)`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2024_1b_ai.png",
]}

past_exams["exam_july_2024_1c"] = {
"title": "Jul-2024 Ex1.c — Distribution of Outstate: quartiles, mean, dispersion",
"is_exam": True, "topic_hint": "G6", "subtopic_hint": "g6b_box",
"content": (
    '<span class="exam-question-text">Describe the distribution of the variable `Outstate` (out-of-state tuition, in 100 USD) — report centre, dispersion and quartiles, and discuss symmetry/skewness.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** For a continuous variable the standard one-variable description is the **5-number summary + mean + SD**. The position of the **median** relative to the **mean** flags asymmetry: mean $\\approx$ median $\\Rightarrow$ roughly symmetric; mean $>$ median $\\Rightarrow$ right-skewed (long right tail); mean $<$ median $\\Rightarrow$ left-skewed. Pair this with a **boxplot** (or histogram) to visualise the shape.\n\n"
    '![AI walkthrough — Outstate boxplot + histogram with quartiles and mean marked](statistics/images/past_exams/exam_july_2024_1c_ai.png)\n\n'
    '---\n\n'
    "**Answer.** From `summary(Colleges$Outstate)` ($n = 408$ colleges, units: 100 USD):\n\n"
    "| stat | value |\n"
    "|---|---|\n"
    "| Min. | 27.0 |\n"
    "| $Q_1$ | 79.3 |\n"
    "| Median | 110.8 |\n"
    "| Mean | 112.2 |\n"
    "| $Q_3$ | 142.1 |\n"
    "| Max. | 199.6 |\n"
    "| $SD$ | 41.6 |\n"
    "| $IQR = Q_3 - Q_1$ | 62.8 |\n\n"
    "**Centre.** Median $\\approx 110.8$ (in 100 USD) and mean $\\approx 112.2$ — very close, so the distribution is **approximately symmetric** around $\\sim 11{,}000$ USD/year out-of-state tuition.\n\n"
    "**Dispersion.** $SD \\approx 41.6$ (≈4{,}160 USD), $IQR \\approx 62.8$ (≈6{,}280 USD). The coefficient of variation $CV = SD/\\bar x \\approx 0.37$ — a moderate spread relative to the mean.\n\n"
    "**Shape.** Boxplot shows the median roughly in the middle of the box, both whiskers of similar length, and **no extreme outliers** (max $\\approx 200$ is within $Q_3 + 1.5\\cdot IQR \\approx 236$). So `Outstate` is **unimodal, roughly symmetric, with no heavy tails**.\n\n"
    '**R commands:**\n\n'
    "`summary(Colleges$Outstate)`\n\n"
    "`## Min. 1st Qu. Median  Mean 3rd Qu.  Max.`\n\n"
    "`## 27.0  79.3   110.8  112.2  142.1  199.6`\n\n"
    "`sd(Colleges$Outstate)`\n\n"
    "`## [1] 41.617`\n\n"
    "`IQR(Colleges$Outstate)`\n\n"
    "`## [1] 62.825`\n\n"
    "`distr.plot(Outstate, plot.type='boxplot', data=Colleges)`\n\n"
    "`distr.plot(Outstate, plot.type='histogram', data=Colleges)`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2024_1c_ai.png",
]}

past_exams["exam_july_2024_2b"] = {
"title": "Jul-2024 Ex2.b — Adequacy of Pearson r: scatterplot and linearity check",
"is_exam": True, "topic_hint": "G9", "subtopic_hint": "g9_corr",
"content": (
    '<span class="exam-question-text">Is Pearson\'s linear correlation coefficient an **adequate** measure of association between `Top10` and `Phd`? Support your answer with the scatterplot and discuss whether the assumptions for $r$ are met.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** Pearson's $r$ is adequate only when the relationship is **roughly linear** and there are **no extreme outliers / leverage points**. Check by plotting the scatter and overlaying a smoother (LOWESS) and the OLS fit: if the smoother tracks the OLS line, linearity holds. Curvature, fan-shaped scatter (heteroscedasticity) or extreme points all undermine $r$.\n\n"
    '![AI walkthrough — scatter Top10 vs Phd with OLS line and LOWESS smoother](statistics/images/past_exams/exam_july_2024_2b_ai.png)\n\n'
    '---\n\n'
    "**Answer.** The scatterplot of `Phd` vs `Top10` ($n = 408$) shows:\n\n"
    "- a **monotone, roughly linear cloud** drifting from low-`Phd`/low-`Top10` to high-`Phd`/high-`Top10`;\n\n"
    "- **no strong curvature** — the LOWESS smoother is approximately straight and very close to the OLS regression line;\n\n"
    "- **dispersion roughly homogeneous** across the range of `Top10` (no fan shape);\n\n"
    "- a handful of mild outliers but no extreme high-leverage points.\n\n"
    "All four assumptions for Pearson are reasonably satisfied $\\Rightarrow$ **$r = 0.566$ is an adequate, faithful summary of the (moderate, positive) linear association**.\n\n"
    "If we still wanted a check robust to non-linearity / outliers, **Spearman's rank correlation** gives $r_S = 0.576$ — essentially the same as Pearson, confirming the relationship is well-described by a monotone (here approximately linear) pattern.\n\n"
    '**R commands:**\n\n'
    "`distr.plot.xy(Top10, Phd, plot.type='scatter', fitline=TRUE, data=Colleges)`\n\n"
    "`plot(Colleges$Top10, Colleges$Phd); abline(lm(Phd ~ Top10, data=Colleges))`\n\n"
    "`lines(lowess(Colleges$Top10, Colleges$Phd), col='red', lwd=2)`\n\n"
    "`cor(Colleges$Top10, Colleges$Phd, method='pearson')`\n\n"
    "`## [1] 0.5657305`\n\n"
    "`cor(Colleges$Top10, Colleges$Phd, method='spearman')`\n\n"
    "`## [1] 0.5760672`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2024_2b_ai.png",
]}

past_exams["exam_july_2024_2c"] = {
"title": "Jul-2024 Ex2.c — Spearman rank correlation as a robustness check",
"is_exam": True, "topic_hint": "G9", "subtopic_hint": "g9_corr",
"content": (
    '<span class="exam-question-text">Compute the **Spearman rank correlation** between `Top10` and `Phd` and compare it with Pearson\'s $r$. Comment on what this tells you about the nature of the association.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** Spearman's $\\rho_S$ is Pearson's correlation **on the ranks** of the two variables. It measures **monotone** association (not necessarily linear) and is **robust** to outliers and to monotone transformations. If $\\rho_S \\approx r$, the relationship is well-approximated by a *linear* pattern; if $|\\rho_S| \\gg |r|$, the link is monotone but non-linear (Pearson under-detects it); if $|\\rho_S| \\ll |r|$, a few influential points are inflating Pearson.\n\n"
    '![AI walkthrough — Pearson vs Spearman side-by-side](statistics/images/past_exams/exam_july_2024_2c_ai.png)\n\n'
    '---\n\n'
    "**Answer.** With $n = 408$ complete pairs:\n\n"
    "$$r_{\\text{Pearson}} = 0.5657, \\qquad \\rho_S = 0.5761.$$\n\n"
    "The two coefficients are **virtually identical** ($\\Delta = 0.010$). Interpretation:\n\n"
    "- the association is **monotone increasing** (both positive);\n\n"
    "- it is **essentially linear**: Pearson does not under-estimate strength → no hidden curvature;\n\n"
    "- it is **not driven by outliers**: Spearman, which would shrink them to ranks, gives the same answer.\n\n"
    "Both diagnostics confirm a **moderate, positive, linear** association between `Top10` and `Phd`, so the Pearson summary $r \\approx 0.57$ from part (a) is reliable.\n\n"
    '**R commands:**\n\n'
    "`cor(Colleges$Top10, Colleges$Phd, method='spearman', use='complete')`\n\n"
    "`## [1] 0.5760672`\n\n"
    "`cor(Colleges$Top10, Colleges$Phd, method='pearson',  use='complete')`\n\n"
    "`## [1] 0.5657305`\n\n"
    "`cor.test(Colleges$Top10, Colleges$Phd, method='spearman')`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2024_2c_ai.png",
]}

past_exams["exam_july_2024_3b"] = {
"title": "Jul-2024 Ex3.b — Multiple regression Outstate ~ Top10 + Region + Private + Room.Board: dummy interpretation + 99% CI",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15d_categorical",
"content": (
    '<span class="exam-question-text">Fit the multiple regression `lm(Outstate ~ Top10 + Region + Private + Room.Board, data=Colleges)`. **Interpret the coefficient on the `RegionSouth` dummy** and build a **99% confidence interval** for the slope on `Top10`.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** With `Region` factor (baseline = `Northeast`) R creates three dummies `RegionMidwest`, `RegionSouth`, `RegionWest`. Each dummy slope measures the **ceteris-paribus** mean difference in `Outstate` between that region and the **Northeast** baseline. The 99% CI for any slope is $\\hat\\beta \\pm t_{0.005, n-p-1}\\cdot SE(\\hat\\beta)$ with $t_{0.005, 401} \\approx 2.588$.\n\n"
    '![AI walkthrough — coefficient bars with 99% CIs (Top10 highlighted)](statistics/images/past_exams/exam_july_2024_3b_ai.png)\n\n'
    '---\n\n'
    "**Answer.** Fitted coefficients (`summary(m2)`, $n=408$, $p+1=7$):\n\n"
    "| term | $\\hat\\beta$ | SE | $t$ | $p$ |\n"
    "|---|---|---|---|---|\n"
    "| (Intercept) | 14.953 | 6.786 | 2.20 | 0.028 |\n"
    "| `Top10` | **1.0679** | 0.0774 | 13.80 | < 2e-16 |\n"
    "| `RegionMidwest` | -6.050 | 3.462 | -1.75 | 0.081 |\n"
    "| `RegionSouth` | **-22.790** | 3.359 | -6.79 | 4.2e-11 |\n"
    "| `RegionWest` | 0.020 | 4.189 | 0.005 | 0.996 |\n"
    "| `PrivateYes` | 29.804 | 3.182 | 9.37 | < 2e-16 |\n"
    "| `Room.Board` | 1.160 | 0.138 | 8.38 | 9.1e-16 |\n\n"
    "**(i) Interpretation of `RegionSouth`.** With Northeast as baseline,\n\n"
    "$$\\hat\\beta_{\\text{RegionSouth}} = -22.79\\text{ (in 100 USD).}$$\n\n"
    "Holding `Top10`, `Private` and `Room.Board` fixed, **Southern colleges charge on average $\\approx 2{,}279$ USD less out-of-state tuition than Northeastern ones**. The effect is highly significant ($p \\approx 4 \\times 10^{-11}$).\n\n"
    "**(ii) 99% CI for `Top10`.** With $t_{0.005,\\,401} \\approx 2.588$:\n\n"
    "$$1.0679 \\pm 2.588 \\cdot 0.0774 \\;=\\; [\\,0.868,\\; 1.268\\,]\\text{ (in 100 USD per percentage point of Top10).}$$\n\n"
    "`confint(m2, 'Top10', level=0.99)` returns **[0.8677, 1.2682]**. Since the entire interval is **positive and does not contain 0**, the partial effect of `Top10` is significantly different from 0 at $\\alpha = 1\\%$: each extra percentage point of top-10% HS students is associated with **between 87 and 127 USD higher tuition**, controlling for region, private/public status and room & board.\n\n"
    '**R commands:**\n\n'
    "`m2 <- lm(Outstate ~ Top10 + Region + Private + Room.Board, data=Colleges)`\n\n"
    "`summary(m2)`\n\n"
    "`## RegionSouth  -22.790    3.359  -6.785  4.19e-11 ***`\n\n"
    "`## Top10          1.068    0.077  13.803  < 2e-16 ***`\n\n"
    "`confint(m2, 'Top10', level=0.99)`\n\n"
    "`##         0.5 %    99.5 %`\n\n"
    "`## Top10  0.8677    1.2682`\n\n"
    "`confint(m2, 'RegionSouth', level=0.99)`\n\n"
    "`## RegionSouth  -31.484  -14.097`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2024_3b_ai.png",
]}

past_exams["exam_july_2024_3c"] = {
"title": "Jul-2024 Ex3.c — Global F-test, R^2 and adjusted R^2",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15c_multi_reg",
"content": (
    '<span class="exam-question-text">For the multiple regression `lm(Outstate ~ Top10 + Region + Private + Room.Board, data=Colleges)`, report **$R^2$**, **adjusted $R^2$** and the **global $F$-test**. Decide whether the model is globally significant at $\\alpha = 1\\%$.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** Three complementary summaries of overall fit:\n\n"
    "- $R^2 = 1 - SSE/SST$ — share of $Y$-variance explained;\n"
    "- adjusted $R^2 = 1 - (1-R^2)\\dfrac{n-1}{n-p-1}$ — penalises model complexity, fair comparison across nested models;\n"
    "- the **global $F$-test** $H_0: \\beta_1 = \\dots = \\beta_p = 0$ vs $H_1$: at least one nonzero, with\n\n"
    "$$F = \\dfrac{R^2/p}{(1-R^2)/(n-p-1)} \\sim F_{p,\\,n-p-1} \\text{ under } H_0.$$\n\n"
    '![AI walkthrough — variance decomposition donut and F vs critical value](statistics/images/past_exams/exam_july_2024_3c_ai.png)\n\n'
    '---\n\n'
    "**Answer.** From `summary(m2)` with $n = 408$, $p = 6$ regressors, $n-p-1 = 401$:\n\n"
    "| statistic | value |\n"
    "|---|---|\n"
    "| Multiple $R^2$ | **0.7117** |\n"
    "| Adjusted $R^2$ | **0.7074** |\n"
    "| $F$-statistic | **165.0** on (6, 401) df |\n"
    "| $p$-value | $< 2.2 \\times 10^{-16}$ |\n"
    "| Residual SE | 22.51 (in 100 USD) |\n\n"
    "**Reading.**\n\n"
    "- The six predictors **jointly explain about 71% of the variance** of `Outstate` ($R^2 = 0.712$); the adjusted $R^2$ is essentially equal (0.707) — the regressors are 'paying their rent', no inflation by useless predictors.\n\n"
    "- The global $F = 165.0 \\gg F_{0.99,\\,6,\\,401} \\approx 2.85$, $p < 2.2\\times 10^{-16}$. **Reject $H_0$**: at least one slope is nonzero — **the model is highly significant overall** at any conventional $\\alpha$ (including 1%).\n\n"
    "Residual SE $\\approx 22.5$ (100 USD) $\\approx 2{,}250$ USD: a typical fitted college lies within $\\sim 2{,}250$ USD of the regression surface — much smaller than the unconditional SD of `Outstate` ($\\approx 41.6$ in 100 USD), consistent with the high $R^2$.\n\n"
    '**R commands:**\n\n'
    "`m2 <- lm(Outstate ~ Top10 + Region + Private + Room.Board, data=Colleges)`\n\n"
    "`summary(m2)$r.squared`\n\n"
    "`## [1] 0.7117397`\n\n"
    "`summary(m2)$adj.r.squared`\n\n"
    "`## [1] 0.7074266`\n\n"
    "`summary(m2)$fstatistic`\n\n"
    "`##    value    numdf    dendf`\n\n"
    "`##  165.017   6.000  401.000`\n\n"
    "`qf(0.99, 6, 401)   # critical value`\n\n"
    "`## [1] 2.852`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2024_3c_ai.png",
]}

past_exams["exam_july_2024_4a"] = {
"title": "Jul-2024 Ex4.a — Normal-tail probability: P(Top10 > 50) using Z standardisation",
"is_exam": True, "topic_hint": "G11", "subtopic_hint": "g11_clt",
"content": (
    '<span class="exam-question-text">Assume `Top10` is approximately normally distributed with mean $\\mu = 28.8$ and standard deviation $\\sigma = 16.3$ (from the sample). Compute the probability that a randomly chosen college has more than **50%** of its enrolled students from the top 10% of HS classes, i.e. $P(\\text{Top10} > 50)$.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** Standardise: $Z = (X - \\mu)/\\sigma \\sim \\mathcal N(0,1)$, then\n\n"
    "$$P(X > 50) \\;=\\; P\\!\\left(Z > \\dfrac{50 - 28.8}{16.3}\\right) \\;=\\; 1 - \\Phi(z_0).$$\n\n"
    '![AI walkthrough — N(0,1) density with right-tail shaded](statistics/images/past_exams/exam_july_2024_4a_ai.png)\n\n'
    '---\n\n'
    "**Answer.** Plug in $\\mu = 28.8$, $\\sigma = 16.3$, $x = 50$:\n\n"
    "$$z_0 = \\dfrac{50 - 28.8}{16.3} = \\dfrac{21.2}{16.3} \\approx 1.301.$$\n\n"
    "Hence\n\n"
    "$$P(\\text{Top10} > 50) \\approx 1 - \\Phi(1.301) \\approx 1 - 0.9034 \\approx \\mathbf{0.0966},$$\n\n"
    "about **9.7%** of colleges. Roughly 1 college out of 10 has more than half of its incoming class taken from the top decile of US high schools.\n\n"
    "**Sanity check.** Empirically in the sample: `mean(Colleges$Top10 > 50) =` $\\approx 0.107$ ($\\approx 11\\%$) — close to the normal-approximation value, confirming the model is reasonable.\n\n"
    '**R commands:**\n\n'
    "`mu <- mean(Colleges$Top10); sd. <- sd(Colleges$Top10)`\n\n"
    "`c(mu, sd.)`\n\n"
    "`## [1] 28.79902 16.25787`\n\n"
    "`pnorm(50, mean=mu, sd=sd., lower.tail=FALSE)`\n\n"
    "`## [1] 0.09617`\n\n"
    "`1 - pnorm((50-mu)/sd.)`\n\n"
    "`## [1] 0.09617`\n\n"
    "`mean(Colleges$Top10 > 50)   # empirical check`\n\n"
    "`## [1] 0.1078`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2024_4a_ai.png",
]}

past_exams["exam_july_2024_4b"] = {
"title": "Jul-2024 Ex4.b — CLT for a sample proportion: P(p_hat > 0.35) with p=0.3, n=750",
"is_exam": True, "topic_hint": "G11", "subtopic_hint": "g11_clt",
"content": (
    '<span class="exam-question-text">Suppose the true population proportion of colleges with `Top10 > 50` is $p = 0.30$. In a random sample of $n = 750$ colleges, what is the probability that the **sample proportion** $\\hat p$ exceeds 0.35? Use the **Central Limit Theorem**.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** With $X_i \\sim \\text{Bernoulli}(p)$ iid the sample proportion $\\hat p = \\bar X$ has, by the CLT,\n\n"
    "$$\\hat p \\;\\overset{\\cdot}{\\sim}\\; \\mathcal N\\!\\Big(p,\\;\\dfrac{p(1-p)}{n}\\Big).$$\n\n"
    "Standardise: $Z = (\\hat p - p)/\\sqrt{p(1-p)/n}$ and read off the tail probability.\n\n"
    '![AI walkthrough — sampling distribution of p_hat with right-tail at 0.35 shaded](statistics/images/past_exams/exam_july_2024_4b_ai.png)\n\n'
    '---\n\n'
    "**Answer.** Compute the SE of $\\hat p$:\n\n"
    "$$SE(\\hat p) = \\sqrt{\\dfrac{p(1-p)}{n}} = \\sqrt{\\dfrac{0.30 \\cdot 0.70}{750}} = \\sqrt{0.00028} \\approx 0.01673.$$\n\n"
    "Standardise the threshold $0.35$:\n\n"
    "$$z = \\dfrac{0.35 - 0.30}{0.01673} \\approx 2.988.$$\n\n"
    "By the CLT, $\\hat p \\overset{\\cdot}{\\sim} \\mathcal N(0.30, 0.01673^2)$, so\n\n"
    "$$P(\\hat p > 0.35) \\approx 1 - \\Phi(2.988) \\approx 1 - 0.99860 \\approx \\mathbf{0.00140}.$$\n\n"
    "Only about **0.14% probability** — strongly unlikely. If a single random sample of size 750 returned $\\hat p > 0.35$ we would seriously doubt $p = 0.30$.\n\n"
    "*Conditions for CLT*: independent draws and $np = 225 \\ge 5$, $n(1-p) = 525 \\ge 5$ ✓ — normal approximation is excellent.\n\n"
    '**R commands:**\n\n'
    "`p <- 0.30; n <- 750`\n\n"
    "`se <- sqrt(p*(1-p)/n); se`\n\n"
    "`## [1] 0.01673320`\n\n"
    "`z  <- (0.35 - p)/se; z`\n\n"
    "`## [1] 2.988072`\n\n"
    "`pnorm(0.35, mean=p, sd=se, lower.tail=FALSE)`\n\n"
    "`## [1] 0.001404`\n\n"
    "`1 - pnorm(z)`\n\n"
    "`## [1] 0.001404`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2024_4b_ai.png",
]}

past_exams["exam_july_2024_4c"] = {
"title": "Jul-2024 Ex4.c — Sample size for a 95% CI on p with margin of error 0.02",
"is_exam": True, "topic_hint": "G11", "subtopic_hint": "g11_clt",
"content": (
    '<span class="exam-question-text">What sample size $n$ is required so that a **95% confidence interval** for the population proportion $p$ has **margin of error at most 0.02**? Use the worst-case (conservative) Bernoulli variance.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** Wald CI: $\\hat p \\pm z_{1-\\alpha/2}\\sqrt{\\hat p(1-\\hat p)/n}$. The margin of error\n\n"
    "$$ME = z_{1-\\alpha/2}\\sqrt{\\dfrac{\\hat p(1-\\hat p)}{n}} \\le m.$$\n\n"
    "Without prior knowledge of $p$, use the **worst case** $\\hat p(1-\\hat p) \\le 1/4$ (maximised at $\\hat p = 1/2$). Solving for $n$:\n\n"
    "$$n \\;\\ge\\; \\dfrac{z_{1-\\alpha/2}^{2}}{4\\,m^{2}}.$$\n\n"
    "Then **round up** to the next integer.\n\n"
    '![AI walkthrough — required n vs margin of error, with conservative bound](statistics/images/past_exams/exam_july_2024_4c_ai.png)\n\n'
    '---\n\n'
    "**Answer.** With $z_{0.975} = 1.960$, $m = 0.02$:\n\n"
    "$$n \\;\\ge\\; \\dfrac{1.960^{2}}{4 \\cdot 0.02^{2}} \\;=\\; \\dfrac{3.8416}{0.0016} \\;=\\; 2401.$$\n\n"
    "So **$n = 2401$ colleges** guarantees $ME \\le 0.02$ regardless of the true $p$.\n\n"
    "**If a previous estimate is available**, e.g. $\\hat p \\approx 0.30$ (from Ex4b), the variance shrinks to $0.30\\cdot 0.70 = 0.21$ and\n\n"
    "$$n \\;\\ge\\; \\dfrac{1.960^{2}\\cdot 0.21}{0.02^{2}} \\;=\\; \\dfrac{0.8067}{0.0004} \\approx 2017,$$\n\n"
    "i.e. **$n = 2017$** — a smaller sample is sufficient. Always round **up**.\n\n"
    '**R commands:**\n\n'
    "`z <- qnorm(0.975); z`\n\n"
    "`## [1] 1.959964`\n\n"
    "`ceiling(z^2 / (4 * 0.02^2))   # worst-case`\n\n"
    "`## [1] 2401`\n\n"
    "`ceiling(z^2 * 0.30 * 0.70 / 0.02^2)   # with p_hat = 0.30`\n\n"
    "`## [1] 2017`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2024_4c_ai.png",
]}

# =================== JULY 2025 ===================

past_exams["exam_july_2025_1a"] = {
"title": "Jul-2025 Ex1 — Two-sample one-sided t-test on Savings: Branch A vs Branch B (equal variances)",
"is_exam": True, "topic_hint": "G14", "subtopic_hint": "g14b",
"content": (
    '<span class="exam-question-text">Based on the available data, are we interested in verifying whether the **average amount of savings (`Savings`) in the population of clients of branch A is lower than that of the population of clients of branch B** (variable `Branch`, with categories `A` and `B`), assuming **equal variances** in the two subpopulations?\n\n**1.a State** the null and alternative hypotheses for the test.\n**1.b Report** the analytical expression of the standard error of the test statistic and its value in the sample, and **provide its interpretation**.\n**1.c Report** the expression and value of the test statistic, and **decide** at $\\alpha = 5\\%$ and $\\alpha = 1\\%$.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_july_2025_1a_question.png)\n\n'
    '---\n\n'
    "**Walkthrough.** Three steps: (i) translate the research claim 'A's mean is lower than B's' into the **one-sided** alternative $H_1:\\mu_A<\\mu_B$; (ii) compute the **pooled** SE under the equal-variance assumption — it combines both group standard deviations into a single $s_p$ and reads $SE(\\bar y_A-\\bar y_B)=s_p\\sqrt{1/n_A+1/n_B}\\approx 58.45$; (iii) standardise the observed difference, get $t=-2.786$ with one-sided $p\\approx 0.0027$, then compare to both significance levels. The AI plot shows the t-distribution under $H_0$ with the 5% and 1% lower-tail rejection regions shaded and the observed $t$ marked.\n\n"
    '![AI walkthrough — pooled-t reference distribution with rejection regions and group means with SE](statistics/images/past_exams/exam_july_2025_1a_ai.png)\n\n'
    '---\n\n'
    "**Answer.**\n\n"
    "**1.a Hypotheses (one-sided, lower-tail).** Let $\\mu_A, \\mu_B$ be the population mean Savings in branches A and B. The research claim that A's mean is *lower* than B's is the alternative:\n\n"
    "$$H_0:\\ \\mu_A = \\mu_B \\quad\\text{vs}\\quad H_1:\\ \\mu_A < \\mu_B.$$\n\n"
    "Equivalently, with $D = \\mu_A - \\mu_B$: $H_0: D = 0$ vs $H_1: D < 0$.\n\n"
    "**1.b Standard error (pooled, equal variances assumed).** Under $\\sigma_A^2 = \\sigma_B^2 = \\sigma^2$ the pooled variance combines both samples:\n"
    "$$s_p^2 = \\frac{(n_A - 1)\\,s_A^2 + (n_B - 1)\\,s_B^2}{n_A + n_B - 2}, \\qquad SE(\\bar y_A - \\bar y_B) = s_p\\sqrt{\\tfrac{1}{n_A} + \\tfrac{1}{n_B}}.$$\n"
    "From the `TEST.diffmean` output: $SE \\approx 58.45$ (in € of Savings). **Interpretation:** it is an estimate of the expected distance (or expected absolute deviation) of a **generic estimate (difference)** of the sample averages of Savings from the parameter of interest — here, the difference between the *two* population means. Thus the expected error of a generic estimate is **about €58.45 in Savings**. The actual estimation error on the specific sample could be larger or smaller, but no sharper statement can be made for one specific observed sample.\n\n"
    "**1.c Test statistic and decision.** Under $H_0$, $t = \\dfrac{\\bar y_A - \\bar y_B}{SE} \\sim t_{n_A+n_B-2}$.\n\n"
    "From the output: $\\bar y_A - \\bar y_B \\approx -162.835$, $SE \\approx 58.45$, hence\n"
    "$$t = \\frac{-162.835}{58.45} \\approx -2.786, \\qquad p\\text{-value (one-sided, lower)} \\approx 0.0027.$$\n\n"
    "**Decision.** $p \\approx 0.0027 < 0.01 < 0.05$ → **reject $H_0$ at both $\\alpha = 5\\%$ and $\\alpha = 1\\%$**. There is strong evidence that the average savings of Branch A's clients are *lower* than those of Branch B.\n\n"
    "**Note on `var.test=TRUE`.** The companion variance-equality test (F / Levene) returns $p > \\alpha$, so the equal-variance assumption is not rejected and the **pooled** t-test (rather than Welch's) is the appropriate one here.\n\n"
    "**R commands:**\n\n"
    "`TEST.diffmean(Savings, by=Branch, type='independent', alternative='less', var.test=TRUE, data=BankClients)`\n\n"
    "`## Two Sample t-test (pooled)`\n\n"
    "`##   t = -2.786,  df = n_A + n_B - 2,  p-value = 0.0027`\n\n"
    "`##   alternative: true difference in means (A - B) is less than 0`\n\n"
    "`##   SE(diff) = 58.45,  mean_A - mean_B = -162.835`\n\n"
    "`t.test(Savings ~ Branch, data=BankClients, alternative='less', var.equal=TRUE)`\n\n"
    "`var.test(Savings ~ Branch, data=BankClients)`\n\n"
    "`## F test: p > 0.05 -> do not reject H0 of equal variances -> use pooled t-test`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_july_2025_1a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_july_2025_1a_question.png",
    "statistics/images/past_exams/exam_july_2025_1a_ai.png",
    "statistics/images/past_exams/answers/exam_july_2025_1a_answer.png",
]}

past_exams["exam_july_2025_2a"] = {
"title": "Jul-2025 Ex2a — Conditional boxplot of Loans by AgeC (young / adult / senior)",
"is_exam": True, "topic_hint": "G6", "subtopic_hint": "g6b_box",
"content": (
    '<span class="exam-question-text">The goal is to assess possible differences in the loan amount (`Loans`) for clients in the sample belonging to different age groups (`AgeC`, with categories *young*, *adult*, *senior*).\n\n**2.a Indicate** which plot you would use to compare the three distributions, and **provide** a sketch of the specified plot. **Evaluate** the shape of the distributions and **comment** on the main differences among the three client groups, with particular reference to the positional measures observable from the plot.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** With a numerical response (`Loans`) and a categorical grouping (`AgeC` with 3 levels), the standard side-by-side display is a **conditional / multiple boxplot** — one box per group — drawn with `distr.plot.xy(AgeC, Loans, plot.type='boxplot', data=BankClients)`. Each box shows the **5-number summary** (min, $Q_1$, median, $Q_3$, max) of `Loans` *conditional on* the age group; whiskers extend to $\\pm 1.5\\,IQR$ and points beyond are flagged as outliers. Reading the three boxes side by side lets us compare **centre** (median line), **spread** (box width = IQR), **skewness** (longer whisker / outlier tail on one side) and **outliers** at a glance.\n\n"
    '![AI walkthrough — conditional boxplot of Loans by AgeC with sample-size bar](statistics/images/past_exams/exam_july_2025_2a_ai.png)\n\n'
    '---\n\n'
    "**Answer.** Sample sizes: $n_{\\text{adult}}=767$, $n_{\\text{senior}}=103$, $n_{\\text{young}}=128$. Group five-number summaries (from `by(Loans, AgeC, fivenum)`):\n\n"
    "| AgeC | min | Q1 | median | Q3 | max |\n"
    "|---|---|---|---|---|---|\n"
    "| adult  | 0.03 | 6\u202f779.1 | **18\u202f863.7** | 39\u202f396.6 | 202\u202f042.5 |\n"
    "| senior | 16.6 | 3\u202f236.0 | **8\u202f412.3**  | 19\u202f587.6 | 156\u202f717.7 |\n"
    "| young  | 0.2  | 8\u202f283.6 | **25\u202f087.0** | 45\u202f728.3 | 128\u202f083.1 |\n\n"
    "**Shape.** All three conditional distributions of `Loans` are **strongly right-skewed** (long upper tail and many upper outliers in every group — mean $\\gg$ median in adult and senior groups, e.g. mean$_{\\text{adult}}=27\\,820$ vs median $18\\,864$).\n\n"
    "**Positional comparison.**\n\n"
    "- **Senior** clients have the **lowest** central tendency (median $\\approx 8\\,412$\u20ac, $Q_3 \\approx 19\\,588$\u20ac) — narrowest IQR but still long upper tail.\n"
    "- **Adult** and **young** clients have *similar* medians, but **young** has a noticeably **wider IQR** ($\\approx 37\\,445$) and the **highest** median ($\\approx 25\\,087$\u20ac) — younger customers borrow more on average within this sample.\n"
    "- All three distributions exhibit **upper outliers** (the right whisker is short relative to the maximum), which is what motivates the formal upper-outlier check in 2c.\n\n"
    "**Explanation.** The leftward shift of the *senior* group is consistent with older clients having already paid down most large loans (mortgages) and rarely applying for new ones, whereas *young* and *adult* clients are still in the active-borrowing phase of life.\n\n"
    "**R commands:**\n\n"
    "`distr.plot.xy(AgeC, Loans, data=BankClients, plot.type='boxplot')`\n\n"
    "`by(Loans, AgeC, fivenum)`\n\n"
    "`## adult : 0.03 6779.09 18863.72 39396.55 202042.48`\n\n"
    "`## senior: 16.58 3235.95 8412.25 19587.63 156717.65`\n\n"
    "`## young : 0.19 8283.63 25087.04 45728.29 128083.12`\n\n"
    "`by(Loans, AgeC, summary)`\n\n"
    "`table(AgeC)`\n\n"
    "`## adult senior young`\n\n"
    "`##   767    103   128`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2025_2a_ai.png",
]}

past_exams["exam_july_2025_2b"] = {
"title": "Jul-2025 Ex2b — 99th percentile of Loans by AgeC (max debt of 99% of clients)",
"is_exam": True, "topic_hint": "G6", "subtopic_hint": "g6a_quant",
"content": (
    '<span class="exam-question-text">**2.b** What is the maximum threshold (i.e. the maximum amount) of debt (`Loans`) for the 99% of clients in each of the three age groups (`AgeC`)? **Report** the three values and **comment** on the results obtained. The maximum threshold for 99% of customers within each age group is the 99th percentile corresponding to that group.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** The **99th percentile** of `Loans` conditional on `AgeC=g` is the value $q_{0.99}^{(g)}$ such that $P(\\text{Loans} \\le q_{0.99}^{(g)} \\mid \\text{AgeC}=g) = 0.99$. In words: 99% of clients in group $g$ have a debt **at or below** $q_{0.99}^{(g)}$, and only the top 1% exceed it. The R idiom is `distr.summary.x(Loans, by=AgeC, stats='p99', data=BankClients)`, or equivalently `by(Loans, AgeC, quantile, probs=0.99)`.\n\n"
    '![AI walkthrough — Loans conditional densities with q_{0.99} for each AgeC + bar comparison](statistics/images/past_exams/exam_july_2025_2b_ai.png)\n\n'
    '---\n\n'
    "**Answer.** Computing the empirical 99th percentile in each group:\n\n"
    "| AgeC | $n$ | $q_{0.99}$ (\u20ac) |\n"
    "|---|---|---|\n"
    "| adult  | 767 | **143\u202f073** |\n"
    "| senior | 103 | **126\u202f821** |\n"
    "| young  | 128 | **119\u202f923** |\n\n"
    "**Comment.** 99% of *adult* clients have loans up to about **143k\u20ac**, against **127k\u20ac** for *senior* and **120k\u20ac** for *young*. The *adult* upper threshold is materially higher than the other two — consistent with adults being in the prime borrowing phase (mortgages, business loans, etc.) and therefore having both more and larger outstanding loans, while *senior* and *young* clients top out closer to each other (around \u20ac120–127k).\n\n"
    "**R commands:**\n\n"
    "`distr.summary.x(Loans, by=AgeC, stats='p99', data=BankClients)`\n\n"
    "`## Summary measures for Loans | AgeC`\n\n"
    "`##           n  n.a       p99`\n\n"
    "`## adult   767    0  143073.4`\n\n"
    "`## senior  103    0  126820.6`\n\n"
    "`## young   128    0  119922.7`\n\n"
    "`by(Loans, AgeC, quantile, probs=0.99, na.rm=TRUE)`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2025_2b_ai.png",
]}

past_exams["exam_july_2025_2c"] = {
"title": "Jul-2025 Ex2c — Upper-outlier threshold for young AgeC (Tukey rule)",
"is_exam": True, "topic_hint": "G6", "subtopic_hint": "g6c_outliers",
"content": (
    '<span class="exam-question-text">**2.c** With reference to the maximum threshold identified in the **previous point** for the sub-sample of younger clients (`AgeC = young`) can we conclude that a loan amount above this threshold should be considered anomalous (if you did not answer the previous point, consider an amount equal to 120\u202f000\u20ac)? **Justify** your answer explicitly **reporting** the measures you refer to in order to answer.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** The classical Tukey rule flags a value as a **(mild) upper outlier** when it lies above\n\n"
    "$$L = Q_3 + 1.5 \\cdot (Q_3 - Q_1) = Q_3 + 1.5\\,IQR$$\n\n"
    "*in the relevant conditional distribution* — here, the distribution of `Loans` for the `young` group. So the question reduces to comparing the candidate value ($q_{0.99,\\text{young}}=119\\,922.7$ from 2b, or the suggested $120\\,000$) to $L_{\\text{young}}$. From the five-number summary of *young* clients: $Q_1 = 8\\,283.63$, $Q_3 = 45\\,728.29$, $IQR = 37\\,444.66$, so\n\n"
    "$$L_{\\text{young}} = 45\\,728.29 + 1.5 \\cdot 37\\,444.66 = 45\\,728.29 + 56\\,166.99 \\approx 101\\,895.3.$$\n\n"
    '![AI walkthrough — young Loans boxplot with Tukey upper threshold L vs q_{0.99} and the reference 120,000](statistics/images/past_exams/exam_july_2025_2c_ai.png)\n\n'
    '---\n\n'
    "**Answer.**\n\n"
    "- Upper-outlier threshold: $L_{\\text{young}} = Q_3 + 1.5\\,(Q_3 - Q_1) = 45\\,728.29 + 1.5 \\cdot 37\\,444.66 \\approx \\mathbf{101\\,895.3}$\u20ac.\n"
    "- Candidate value: $q_{0.99,\\text{young}} \\approx 119\\,922.7$\u20ac (from 2b).\n\n"
    "Since $119\\,922.7 > 101\\,895.3$, **yes** — any loan above $q_{0.99,\\text{young}}$ exceeds the upper outlier fence, so we can say that **at least 1% of the observed values in the young group are upper outliers** (in the Tukey sense). The same conclusion holds for the suggested $120\\,000$ \u20ac, since $120\\,000 > 101\\,895.3$ as well: a young client borrowing $120{,}000$\u20ac would be flagged as anomalous relative to the *young* sub-sample.\n\n"
    "**R commands:**\n\n"
    "`fn <- fivenum(BankClients$Loans[BankClients$AgeC=='young'])`\n\n"
    "`Q1 <- fn[2]; Q3 <- fn[4]; IQR <- Q3 - Q1`\n\n"
    "`L  <- Q3 + 1.5*IQR`\n\n"
    "`## Q1 = 8283.63, Q3 = 45728.29, IQR = 37444.66, L = 101895.3`\n\n"
    "`q99 <- quantile(BankClients$Loans[BankClients$AgeC=='young'], 0.99)`\n\n"
    "`## 119922.7`\n\n"
    "`q99 > L`\n\n"
    "`## TRUE  -> the 99th percentile is an upper outlier`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2025_2c_ai.png",
]}

past_exams["exam_july_2025_3a"] = {
"title": "Jul-2025 Ex3a — Sample mean as unbiased estimator of mean Savings (xbar ≈ 275.33€)",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13f",
"content": (
    '<span class="exam-question-text">**3.** The bank is interested in estimating the average amount of savings (variable `Savings`) if the experimental procedures were extended to other branches.\n\n**3.a Indicate** which **estimator** you would use, and what are its properties, **providing** their formal definition. **Report** the estimate obtained from the data.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** Given an i.i.d. random sample $X_1, \\dots, X_n$ from an unknown population with mean $\\mu$ and variance $\\sigma^2$, the natural and optimal point estimator of $\\mu$ is the **sample mean**\n\n"
    "$$\\bar X \\;=\\; \\frac{1}{n}\\sum_{i=1}^{n} X_i.$$\n\n"
    "Its key properties are:\n\n"
    "1. **Unbiased**: $E[\\bar X] = \\mu$ for every $\\mu$.\n"
    "2. **Variance**: $\\operatorname{Var}(\\bar X) = \\dfrac{\\sigma^2}{n}$ — shrinks like $1/n$, so $\\bar X$ is **consistent** ($\\bar X \\xrightarrow{P} \\mu$ as $n\\to\\infty$).\n"
    "3. **BLUE** (Gauss–Markov): among all linear unbiased estimators it has the minimum variance.\n"
    "4. By the **CLT**, $\\bar X \\overset{d}{\\to} N(\\mu, \\sigma^2/n)$ for large $n$, regardless of the population shape — the basis of the CI in 3b.\n\n"
    '![AI walkthrough — sampling distribution of xbar shrinks as n grows; observed Savings sample with xbar marked](statistics/images/past_exams/exam_july_2025_3a_ai.png)\n\n'
    '---\n\n'
    "**Answer.** Use the **sample mean** $\\bar X = \\frac{1}{n}\\sum_i X_i$ as estimator (unbiased, BLUE, consistent, asymptotically normal). Computing it on the data with $n = 998$:\n\n"
    "$$\\bar x = \\frac{1}{998}\\sum_{i=1}^{998} \\text{Savings}_i \\;=\\; \\mathbf{275.3343\\,\u20ac}.$$\n\n"
    "We therefore estimate that the mean Savings amount in the population is approximately **275.33\u20ac** per client.\n\n"
    "**R commands:**\n\n"
    "`mean(BankClients$Savings)`\n\n"
    "`## [1] 275.3343`\n\n"
    "`length(BankClients$Savings)`\n\n"
    "`## [1] 998`\n\n"
    "`sd(BankClients$Savings)`\n\n"
    "`## [1] 856.5955`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2025_3a_ai.png",
]}

past_exams["exam_july_2025_3b"] = {
"title": "Jul-2025 Ex3b — 90% z-CI for mean Savings (sigma=800 known): [233.68, 316.99]",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13a",
"content": (
    '<span class="exam-question-text">**3.b** Assuming that the standard deviation of the amount of savings in the population is equal to **800**, **report** the confidence interval for the mean amount of savings with a confidence level of **90%**. How do you **interpret** the obtained confidence interval?</span>\n\n'
    '---\n\n'
    "**Walkthrough.** With $\\sigma$ **known** (here $\\sigma=800$) and $n = 998$ large, the pivot is\n\n"
    "$$Z \\;=\\; \\frac{\\bar X - \\mu}{\\sigma/\\sqrt{n}} \\sim N(0,1)$$\n\n"
    "and the two-sided $(1-\\alpha)$ confidence interval for $\\mu$ is the classical **z-interval**\n\n"
    "$$\\bar X \\pm z_{1-\\alpha/2}\\,\\frac{\\sigma}{\\sqrt{n}}, \\qquad z_{0.95} = 1.6449.$$\n\n"
    "Plugging in: $SE = 800/\\sqrt{998} \\approx 25.32$, margin of error $ME = 1.6449 \\cdot 25.32 \\approx 41.65$.\n\n"
    '![AI walkthrough — N(0,1) with central 90% area and CI bar [233.68, 316.99] centred at xbar=275.33](statistics/images/past_exams/exam_july_2025_3b_ai.png)\n\n'
    '---\n\n'
    "**Answer.** With $\\bar x = 275.3343$, $\\sigma = 800$, $n = 998$, $z_{0.95}=1.6449$:\n\n"
    "$$\\bar x \\pm z_{0.95}\\,\\frac{\\sigma}{\\sqrt{n}} = 275.3343 \\pm 1.6449 \\cdot \\frac{800}{\\sqrt{998}} = 275.3343 \\pm 41.6536$$\n\n"
    "$$\\Longrightarrow \\quad \\boxed{\\;\\big[233.68,\\; 316.99\\big]\\;\u20ac\\,(\\text{at } 90\\%\\text{ confidence})\\;}$$\n\n"
    "**Interpretation.** With 90% confidence the population mean of `Savings` lies between **233.68\u20ac and 316.99\u20ac**. In the long-run frequentist sense, if we repeated the sampling procedure many times and constructed a 90% CI each time, ~90% of those intervals would contain the true $\\mu$. The large sample size ($n = 998$) makes the CLT applicable, so we do not need the normality of `Savings` itself — only finite variance is required.\n\n"
    "**R commands:**\n\n"
    "`CI.mean(Savings, sigma=800, conf.level=0.9, data=BankClients)`\n\n"
    "`## Confidence interval for the mean`\n\n"
    "`## Confidence level: 0.9`\n\n"
    "`## Variance: known`\n\n"
    "`##   n    xbar    sigma_X   SE    Lower   Upper`\n\n"
    "`## 998   275.33  800       25.32  233.68  316.99`\n\n"
    "`# Manual reconstruction:`\n\n"
    "`z <- qnorm(0.95)            # 1.6449`\n\n"
    "`SE <- 800 / sqrt(998)       # 25.32`\n\n"
    "`mean(BankClients$Savings) + c(-1,1) * z * SE`\n\n"
    "`## [1] 233.68 316.99`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2025_3b_ai.png",
]}

past_exams["exam_july_2025_3c"] = {
"title": "Jul-2025 Ex3c — Margin of error & required n for ME<35 (n*=1414)",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13a",
"content": (
    '<span class="exam-question-text">**3.c** What is the **margin of error** of the obtained interval (from 3b)? How many clients should be included in the sample so that the margin of error is **less than 35**?</span>\n\n'
    '---\n\n'
    "**Walkthrough.** The margin of error of a $(1-\\alpha)$ z-CI for the mean is\n\n"
    "$$ME = z_{1-\\alpha/2}\\,\\frac{\\sigma}{\\sqrt{n}}.$$\n\n"
    "Two equivalent ways to read it off the 3b output:\n\n"
    "$$ME = \\frac{UCL - LCL}{2} = \\frac{316.99 - 233.68}{2} = 41.66 \\quad \\text{or} \\quad z_{0.95} \\cdot \\frac{\\sigma}{\\sqrt{n}} = 1.6449 \\cdot \\frac{800}{\\sqrt{998}} \\approx 41.65.$$\n\n"
    "(Tiny difference due to rounding.) To shrink $ME$ below a target $ME^*=35$, invert the formula:\n\n"
    "$$n^* = \\left(\\frac{z_{1-\\alpha/2}\\,\\sigma}{ME^*}\\right)^2 = \\left(\\frac{1.6449 \\cdot 800}{35}\\right)^2 \\approx 1413.51 \\;\\Rightarrow\\; \\big\\lceil n^* \\big\\rceil = \\mathbf{1414}.$$\n\n"
    '![AI walkthrough — required sample size n* vs target ME (35→1414) plus ME shrinks as 1/sqrt(n)](statistics/images/past_exams/exam_july_2025_3c_ai.png)\n\n'
    '---\n\n'
    "**Answer.**\n\n"
    "- **Margin of error of the 3b interval:** $ME = (316.99 - 233.68)/2 = \\mathbf{41.66\u20ac}$ (or $z_{0.95}\\sigma/\\sqrt{n} = 41.65\u20ac$, differing only in the last decimal due to rounding).\n"
    "- **Required sample size for $ME < 35$:**\n\n"
    "$$n^* = \\left(\\frac{1.6449 \\cdot 800}{35}\\right)^2 = \\frac{1.6449^2 \\cdot 800^2}{35^2} = \\frac{2.7057 \\cdot 640\\,000}{1\\,225} \\approx 1\\,413.51 \\;\\Rightarrow\\; n^* = \\mathbf{1\\,414}.$$\n\n"
    "It would be necessary to collect savings values from **1\u202f414** clients (= 1414 because $n$ must be an integer and we round *up*) to obtain a margin of error lower than \u20ac35 at the 90% confidence level.\n\n"
    "**R commands:**\n\n"
    "`z <- qnorm(0.95)`\n\n"
    "`sigma <- 800; ME_target <- 35`\n\n"
    "`n_star <- (z * sigma / ME_target)^2`\n\n"
    "`n_star`\n\n"
    "`## [1] 1413.508`\n\n"
    "`ceiling(n_star)`\n\n"
    "`## [1] 1414`\n\n"
    "`# Verify ME at n=1414:`\n\n"
    "`z * sigma / sqrt(1414)`\n\n"
    "`## [1] 34.993  (< 35 \u2713)`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2025_3c_ai.png",
]}

past_exams["exam_july_2025_4a"] = {
"title": "Jul-2025 Ex4a — Intercept of mod1=lm(Investments~Branch+AgeC): b0≈424.01 (baseline A,adult)",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15d",
"content": (
    '<span class="exam-question-text">**4.** We are interested in studying the amount of investments and its possible relationship with some of the available variables.\n\n**4.a** Build a linear regression model (`mod1`) that relates the amount of client investments (`Investments`) to their branch (`Branch`, with categories `A` and `B`) and age group (`AgeC`, with categories *young*, *adult*, and *senior*).\n**Report** the estimate of the model intercept and **specify** what substantial information it provides to the bank (thus, provide a clear and complete interpretation and explanation of the intercept in this model).</span>\n\n'
    '---\n\n'
    "**Walkthrough.** Both regressors are categorical, so the design matrix consists only of dummy variables. With R's default factor ordering (`Branch` alphabetical → baseline = `A`; `AgeC` alphabetical → baseline = `adult`), the model is\n\n"
    "$$\\widehat{\\text{Investments}} = \\hat\\beta_0 + \\hat\\beta_{1}\\,\\mathbb{1}\\{\\text{Branch}=B\\} + \\hat\\beta_{2}\\,\\mathbb{1}\\{\\text{AgeC}=senior\\} + \\hat\\beta_{3}\\,\\mathbb{1}\\{\\text{AgeC}=young\\}.$$\n\n"
    "Therefore **the intercept $\\hat\\beta_0$ is the predicted mean of `Investments` when *all* dummies are zero**, i.e. for a client of the **baseline cell** (`Branch=A`, `AgeC=adult`). If instead we order `AgeC` with `young` as the reference (`factor(AgeC, levels=c('young','adult','senior'))`), the intercept becomes the predicted mean for the new baseline (`Branch=A`, `AgeC=young`) and equals $352.76$ — exactly $424.01 - 71.25$ in the original parameterisation (since $\\hat\\beta_{young}=-71.25$).\n\n"
    '![AI walkthrough — group-mean grid by (Branch, AgeC) with intercept cell highlighted, plus mod1 coefficient bars](statistics/images/past_exams/exam_july_2025_4a_ai.png)\n\n'
    '---\n\n'
    "**Answer.** From `summary(mod1)`:\n\n"
    "$$\\hat\\beta_0 = \\mathbf{424.01\u20ac} \\quad (SE = 17.99,\\; t = 23.58,\\; p < 2 \\times 10^{-16},\\; ***).$$\n\n"
    "**Interpretation:** $\\hat\\beta_0 = 424.01$ is the **estimated average amount of `Investments` for a client in the baseline cell** of the model — that is, a client of `Branch = A` who belongs to the `adult` age group (the alphabetical reference for both factors). For the bank, this is the *typical* investment level (~424\u20ac) of an adult customer of branch A; all the other coefficients ($\\hat\\beta_B, \\hat\\beta_{senior}, \\hat\\beta_{young}$) are then *contrasts* with respect to this baseline.\n\n"
    "If `AgeC` is re-ordered with `young` as reference (`AgeCf <- factor(AgeC, c('young','adult','senior'))` and `mod1b <- lm(Investments ~ Branch + AgeCf, data=BankClients)`), the intercept becomes $352.76$\u20ac and now represents the average Investments for `Branch=A`, `AgeC=young` clients (the new baseline). The fit, $R^2$, and predicted values are identical — only the parameterisation has changed.\n\n"
    "**R commands:**\n\n"
    "`mod1 <- lm(Investments ~ Branch + AgeC, data=BankClients)`\n\n"
    "`summary(mod1)`\n\n"
    "`## Coefficients:`\n\n"
    "`##              Estimate Std. Error t value Pr(>|t|)`\n\n"
    "`## (Intercept)   424.01      17.99   23.58  < 2e-16 ***`\n\n"
    "`## BranchB        78.48      20.62    3.81  0.00015 ***`\n\n"
    "`## AgeCsenior     68.04      31.51    2.16  0.03106 *`\n\n"
    "`## AgeCyoung     -71.25      28.72   -2.48  0.01326 *`\n\n"
    "`## Multiple R-squared: 0.0289, Adjusted R-squared: 0.0259`\n\n"
    "`## F-statistic: 9.85 on 3 and 994 DF, p-value: 2.1e-06`\n\n"
    "`# Re-ordering AgeC with young as reference:`\n\n"
    "`BankClients$AgeCf <- factor(BankClients$AgeC, levels=c('young','adult','senior'))`\n\n"
    "`mod1b <- lm(Investments ~ Branch + AgeCf, data=BankClients)`\n\n"
    "`coef(mod1b)['(Intercept)']`\n\n"
    "`## (Intercept)`\n\n"
    "`##      352.76`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2025_4a_ai.png",
]}

past_exams["exam_july_2025_4b"] = {
"title": "Jul-2025 Ex4b — mod2 = lm(Investments~Branch+AgeC+Cards+Tenure): Cards slope ≈ +7.20",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15c",
"content": (
    '<span class="exam-question-text">**4.b Report** the estimated equation of the model and **interpret** the substantial information it provides to the bank with reference to the variable `Cards`.\n\n*(The fitted model is `mod2 <- lm(Investments ~ Branch + AgeC + Cards + Tenure, data=BankClients)`.)*</span>\n\n'
    '---\n\n'
    "**Walkthrough.** Multiple regression with two dummies (`BranchB`, `AgeCsenior`, `AgeCyoung`) and two continuous predictors (`Cards` = card-usage intensity, `Tenure` = months as a client). Each $\\hat\\beta_j$ is a **partial / ceteris-paribus** slope: the change in expected `Investments` for a one-unit change in $X_j$, *holding all other predictors fixed*. From `summary(mod2)`:\n\n"
    "$$\\widehat{\\text{Investments}} = 262.18 + 68.67\\cdot\\mathbb{1}_{B=B} + 73.79\\cdot\\mathbb{1}_{senior} - 43.87\\cdot\\mathbb{1}_{young} + 7.20\\cdot\\text{Cards} + 2.07\\cdot\\text{Tenure}.$$\n\n"
    '![AI walkthrough — Investments vs Cards scatter with mod2 partial slope; coefficient bar chart](statistics/images/past_exams/exam_july_2025_4b_ai.png)\n\n'
    '---\n\n'
    "**Answer.**\n\n"
    "**Estimated equation:**\n\n"
    "$$\\widehat{\\text{Investments}} = 262.18 + 68.67\\cdot\\mathbb{1}_{\\text{Branch}=B} + 73.79\\cdot\\mathbb{1}_{\\text{AgeC}=senior} - 43.87\\cdot\\mathbb{1}_{\\text{AgeC}=young} + 7.20\\cdot\\text{Cards} + 2.07\\cdot\\text{Tenure}.$$\n\n"
    "**Cards coefficient:** $\\hat\\beta_{\\text{Cards}} = \\mathbf{+7.20}$ (SE $= 1.24$, $t = 5.81$, $p = 8.4\\times 10^{-9}$, ***). **Interpretation:** *holding constant Branch, AgeC and Tenure*, a **one-unit increase in `Cards`** (i.e. one unit of card-usage intensity) is associated with **an extra \u20ac7.20 of `Investments` on average**. The effect is highly significant ($p \\ll 0.001$), giving the bank quantitative evidence that more intense card usage co-varies with larger investment positions — a useful signal for cross-selling investment products to high card-usage customers.\n\n"
    "Note that the relationship is *associational* (not necessarily causal) and *linear within the observed range of `Cards`* (roughly 1–90 in the sample).\n\n"
    "**R commands:**\n\n"
    "`mod2 <- lm(Investments ~ Branch + AgeC + Cards + Tenure, data=BankClients)`\n\n"
    "`summary(mod2)`\n\n"
    "`## Coefficients:`\n\n"
    "`##              Estimate Std. Error t value  Pr(>|t|)`\n\n"
    "`## (Intercept)  262.1846    25.8819   10.13  < 2e-16 ***`\n\n"
    "`## BranchB       68.6722    19.8411    3.46  0.000561 ***`\n\n"
    "`## AgeCsenior    73.7904    30.4508    2.42  0.015560 *`\n\n"
    "`## AgeCyoung    -43.8684    27.9632   -1.57  0.117016`\n\n"
    "`## Cards          7.1993     1.2391    5.81  8.40e-09 ***`\n\n"
    "`## Tenure         2.0665     0.3282    6.30  4.58e-10 ***`\n\n"
    "`## Multiple R-squared: 0.1085, Adjusted R-squared: 0.104`\n\n"
    "`## F-statistic: 24.15 on 5 and 992 DF, p-value: < 2.2e-16`\n\n"
    "`confint(mod2)['Cards',]`\n\n"
    "`##  2.5 %  97.5 %`\n\n"
    "`##  4.77   9.63`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2025_4b_ai.png",
]}

past_exams["exam_july_2025_4c"] = {
"title": "Jul-2025 Ex4c — Effect of AgeC on Investments in mod2 (only senior significant at 5%)",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15d",
"content": (
    '<span class="exam-question-text">**4.c** What conclusions can be drawn about the effect of the client\u2019s age group (`AgeC`) at the **5% significance level**?</span>\n\n'
    '---\n\n'
    "**Walkthrough.** Each level of `AgeC` (other than the baseline `adult`) appears in `mod2` as its own dummy with its own $t$-test against $H_0: \\beta = 0$. From `summary(mod2)`:\n\n"
    "- `AgeCsenior`: $\\hat\\beta = 73.79$, $SE = 30.45$, $t = 2.42$, $p = 0.01556 \\Rightarrow$ **significant** at 5%.\n"
    "- `AgeCyoung`: $\\hat\\beta = -43.87$, $SE = 27.96$, $t = -1.57$, $p = 0.11702 \\Rightarrow$ **not** significant at 5%.\n\n"
    '![AI walkthrough — p-values vs alpha=0.05 for AgeC dummies in mod1 vs mod2; decision matrix](statistics/images/past_exams/exam_july_2025_4c_ai.png)\n\n'
    '---\n\n'
    "**Answer.** Using $\\alpha = 0.05$:\n\n"
    "- **Senior vs adult (baseline):** $p = 0.0156 < 0.05$ \u2192 **reject $H_0$**. Holding Branch, Cards, Tenure fixed, senior clients invest on average about **\u20ac74 more** than adult clients (significant).\n"
    "- **Young vs adult (baseline):** $p = 0.117 > 0.05$ \u2192 **fail to reject $H_0$**. After controlling for Cards and Tenure, there is **no statistically significant** difference in average Investments between young and adult clients.\n\n"
    "**Conclusion for the bank.** At the 5% level, controlling for the other variables in `mod2`, the only meaningful age contrast on Investments is the *senior premium* over adults; the difference between *young* and *adult* clients is not significant (it was significant in `mod1` but vanishes once `Cards` and `Tenure` enter the model — see 4d).\n\n"
    "**R commands:**\n\n"
    "`summary(mod2)$coefficients`\n\n"
    "`## row AgeCsenior:  Estimate 73.7904  Pr(>|t|) 0.01556 *`\n\n"
    "`## row AgeCyoung :  Estimate -43.8684 Pr(>|t|) 0.11702`\n\n"
    "`confint(mod2)['AgeCsenior',]`\n\n"
    "`##  2.5 %  97.5 %`\n\n"
    "`##  14.03  133.55  (does not include 0)`\n\n"
    "`confint(mod2)['AgeCyoung',]`\n\n"
    "`##  2.5 %  97.5 %`\n\n"
    "`## -98.74  11.00  (includes 0)`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2025_4c_ai.png",
]}

past_exams["exam_july_2025_4d"] = {
"title": "Jul-2025 Ex4d — Compare AgeC significance in mod1 vs mod2; confounding by Tenure/Cards",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15c",
"content": (
    '<span class="exam-question-text">**4.d** Using again a 5% significance level, **compare** the significance of the age group (`AgeC`) variables in models `mod1` and `mod2`. How do you **explain** the observed differences?</span>\n\n'
    '---\n\n'
    "**Walkthrough.** Across the two models the AgeC slopes shift visibly:\n\n"
    "| dummy | mod1 (no Cards/Tenure) | mod2 (+ Cards + Tenure) |\n"
    "|---|---|---|\n"
    "| `AgeCsenior` | $\\hat\\beta = +68.04,\\; p = 0.0311$ (*) | $\\hat\\beta = +73.79,\\; p = 0.0156$ (*) |\n"
    "| `AgeCyoung`  | $\\hat\\beta = -71.25,\\; p = 0.0133$ (*) | $\\hat\\beta = -43.87,\\; p = 0.1170$ (n.s.) |\n\n"
    "In `mod1`, **both** dummies are significant at the 5% level. In `mod2`, only `AgeCsenior` keeps its significance: the magnitude of the `young` effect roughly halves and its $p$-value moves from $0.013$ to $0.117$, so it is **no longer** significant at 5%. The likely reason is **confounding**: the apparent gap in Investments between young and adult clients in `mod1` was *partly explained* by differences in tenure and card usage rather than by age per se. In the sample, young clients have on average a **shorter Tenure** (~44 months vs ~59 for adults) and similar/slightly higher Cards intensity; once `mod2` accounts for Tenure (each extra month adds \u20ac2.07 on Investments, $p \\approx 5\\times 10^{-10}$) and Cards (\u20ac7.20 per unit, $p \\approx 8\\times 10^{-9}$), most of the \"young vs adult\" gap is reabsorbed by those variables and the residual *pure* age contrast becomes statistically indistinguishable from zero.\n\n"
    '![AI walkthrough — coefficient comparison mod1 vs mod2; mean Tenure & Cards by AgeC show confounding](statistics/images/past_exams/exam_july_2025_4d_ai.png)\n\n'
    '---\n\n'
    "**Answer.** At 5%:\n\n"
    "- In **mod1**: both `AgeCsenior` ($p = 0.031$) and `AgeCyoung` ($p = 0.013$) are significant.\n"
    "- In **mod2**: only `AgeCsenior` remains significant ($p = 0.016$); `AgeCyoung` is *not* significant ($p = 0.117$).\n\n"
    "**Explanation.** The drop in significance is a textbook **confounding/mediation** pattern. Adding `Tenure` (months with the bank) and `Cards` (card-usage intensity) to the model — both highly significant — captures part of what `AgeCyoung` was measuring in `mod1`. Young clients tend to have shorter tenure (lower banking history) and somewhat different card usage; both of these are themselves strong predictors of `Investments`. Once they are explicitly in the model, the *residual* age-only effect for young clients shrinks (from $-71.25$ to $-43.87$, a $\\sim 38\\%$ reduction) and its $t$-statistic falls below the 5% threshold. The senior effect, in contrast, *persists* — there is something about being a senior client (life stage, accumulated wealth) that goes beyond Tenure and Cards and still explains higher Investments. Substantively: the supposed *age* effect for young clients in mod1 was largely a *life-cycle* effect transmitted through Tenure / Cards, not an irreducible age premium.\n\n"
    "**R commands:**\n\n"
    "`summary(mod1)$coefficients[c('AgeCsenior','AgeCyoung'),]`\n\n"
    "`summary(mod2)$coefficients[c('AgeCsenior','AgeCyoung'),]`\n\n"
    "`by(BankClients$Tenure, BankClients$AgeC, mean)`\n\n"
    "`## adult: 58.80   senior: 64.38   young: 44.30`\n\n"
    "`by(BankClients$Cards, BankClients$AgeC, mean)`\n\n"
    "`## adult: 6.55    senior: 4.24    young: 6.77`\n\n"
    "`# Formal partial-F comparing mod1 vs mod2 for the AgeC block at fixed Tenure+Cards:`\n\n"
    "`anova(mod1, mod2)`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2025_4d_ai.png",
]}

past_exams["exam_july_2025_4e"] = {
"title": "Jul-2025 Ex4e — Goodness-of-fit of mod2 via R² ≈ 0.1085 (NOT suitable for prediction)",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15a",
"content": (
    '<span class="exam-question-text">**4.e Evaluate** the goodness-of-fit of model `mod2`, **specifying** which index you would use and **reporting** its value. Would you recommend that the bank use this model to predict the investment amount for a new client? Clearly **explain** the reasoning behind your answer.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** The standard summary measure of goodness-of-fit for a linear regression model is the **coefficient of determination**\n\n"
    "$$R^2 = 1 - \\frac{RSS}{TSS} = \\frac{ESS}{TSS} \\in [0,1],$$\n\n"
    "which gives the **proportion of the total variance of the response that is explained by the regressors**. Larger $R^2$ \u2192 better in-sample fit; $R^2 \\approx 0$ means the regressors carry essentially no linear predictive information. The **adjusted $R^2$** penalises model complexity and is more honest when comparing models with different numbers of parameters.\n\n"
    "From `summary(mod2)`: $R^2 = 0.1085$, adj-$R^2 = 0.104$.\n\n"
    '![AI walkthrough — R^2 share bar (explained vs unexplained) and observed-vs-fitted scatter](statistics/images/past_exams/exam_july_2025_4e_ai.png)\n\n'
    '---\n\n'
    "**Answer.** Use the **coefficient of determination $R^2$**. For `mod2`,\n\n"
    "$$R^2 = 0.1085 \\quad (\\text{adjusted } R^2 = 0.104).$$\n\n"
    "Only about **10.85%** of the variability in `Investments` is explained by `Branch + AgeC + Cards + Tenure`; the remaining ~89% is residual variation due to factors not included in the model. The model is overall highly significant (F-statistic $= 24.15$ on $5, 992$ DF, $p < 2\\times 10^{-16}$) — *some* of the regressors carry real predictive signal — but the absolute level of explained variance is **very low**.\n\n"
    "**Recommendation.** **No**, this model is **not** a reliable tool to predict the investment amount of a new client: with $R^2 \\approx 11\\%$ the prediction intervals will be wide and the typical prediction error will be of the same order as the variability in the data itself. The bank should either (i) collect additional predictors that capture more of the missing variation (income, wealth, financial-literacy variables, behavioural indicators) or (ii) restrict the use of the current model to *aggregate / population-level* statements (mean effects of regressors, qualitative association), not to *individual-level* point predictions. For predictive purposes the diagnostic to inspect would also be the residual standard error ($s = 287.7$\u20ac) — large relative to the typical Investments scale — and the observed-vs-fitted scatter, which shows clear vertical spread around the $y=x$ line.\n\n"
    "**R commands:**\n\n"
    "`summary(mod2)$r.squared`\n\n"
    "`## [1] 0.1085`\n\n"
    "`summary(mod2)$adj.r.squared`\n\n"
    "`## [1] 0.104`\n\n"
    "`summary(mod2)$fstatistic`\n\n"
    "`## value   numdf   dendf`\n\n"
    "`## 24.15   5       992`\n\n"
    "`summary(mod2)$sigma     # residual standard error`\n\n"
    "`## [1] 287.7`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2025_4e_ai.png",
]}

past_exams["exam_july_2025_5a"] = {
"title": "Jul-2025 Ex5a — Sample proportion P(Cards > 5.5) = 0.3397 (~34%)",
"is_exam": True, "topic_hint": "G2", "subtopic_hint": "g2a_exact",
"content": (
    '<span class="exam-question-text">**5.** For each client in the sample, a variable is available that indicates the intensity of debit or credit card usage (`Cards`).\n\n**5.a** What is the proportion of clients in the sample who make card payments with an intensity (`Cards`) greater than 5.5? **Report** the value, briefly **explaining** how you obtained it.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** The sample proportion of clients satisfying a condition is\n\n"
    "$$\\hat p = \\frac{\\#\\{i : \\text{condition}_i = \\text{TRUE}\\}}{n}.$$\n\n"
    "In R, `BankClients$Cards > 5.5` is a logical vector that is `TRUE` when the condition holds and `FALSE` otherwise; the **mean of a logical vector** equals the share of `TRUE` entries (R coerces TRUE \u2192 1, FALSE \u2192 0). So `mean(BankClients$Cards > 5.5)` returns the sample proportion of customers with `Cards` above the 5.5 threshold.\n\n"
    '![AI walkthrough — histogram of Cards with the {Cards>5.5} bins shaded plus the proportion bar](statistics/images/past_exams/exam_july_2025_5a_ai.png)\n\n'
    '---\n\n'
    "**Answer.** With $n = 998$ and $339$ clients above the threshold:\n\n"
    "$$\\hat p = \\frac{339}{998} = \\mathbf{0.3397} \\quad (\\text{about } 33.97\\%).$$\n\n"
    "About one-third of the sampled customers exceed the card-usage intensity of $5.5$. This is the empirical anchor used in 5b to compute a CLT-based probability for a sample of $1\\,200$ clients drawn from the other branch.\n\n"
    "**R commands:**\n\n"
    "`mean(BankClients$Cards > 5.5)`\n\n"
    "`## [1] 0.3396794`\n\n"
    "`sum(BankClients$Cards > 5.5)`\n\n"
    "`## [1] 339`\n\n"
    "`length(BankClients$Cards)`\n\n"
    "`## [1] 998`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2025_5a_ai.png",
]}

past_exams["exam_july_2025_5b"] = {
"title": "Jul-2025 Ex5b — CLT: P(sample prop >0.30 | n=1200) ≈ 0.998 (alt p=0.35: ≈0.9999)",
"is_exam": True, "topic_hint": "G11", "subtopic_hint": "g11_clt",
"content": (
    '<span class="exam-question-text">**5.b** Using the proportion calculated in the **previous point** (if not determined, consider a value equal to 0.35), and assuming it to be reliable, determine the probability that in a sample of 1\u202f200 clients from another branch, at least 30% use cards with an intensity greater than 5.5. **Motivate** your answer, **specify** the analytical expression of the required probability, and **provide** its value, also **reporting** the R function used to compute it.</span>\n\n'
    '---\n\n'
    "**Walkthrough.** Let $\\hat P$ be the sample proportion of card-intensive clients in a new sample of $n = 1\\,200$ drawn from a population with true proportion $p$. With i.i.d. draws and a large sample (Bernoulli with $np \\gg 5$, $n(1-p) \\gg 5$), the **Central Limit Theorem** gives\n\n"
    "$$\\hat P \\;\\overset{d}{\\to}\\; N\\!\\left(p,\\; \\frac{p(1-p)}{n}\\right) \\quad \\Longrightarrow \\quad \\frac{\\hat P - p}{\\sqrt{p(1-p)/n}} \\sim N(0,1).$$\n\n"
    "We are asked $P(\\hat P > 0.30) = 1 - \\Phi\\!\\left(\\dfrac{0.30 - p}{\\sqrt{p(1-p)/n}}\\right)$.\n\n"
    "Using $p = \\hat p_{\\text{obs}} = 0.3397$ (from 5a) and $n = 1\\,200$: $SE = \\sqrt{0.3397 \\cdot 0.6603 / 1\\,200} \\approx 0.01367$, so $z = (0.30 - 0.3397)/0.01367 \\approx -2.903$, giving $P(\\hat P > 0.30) = 1 - \\Phi(-2.903) \\approx \\mathbf{0.998}$.\n\n"
    "Using the alternative $p = 0.35$: $SE = \\sqrt{0.35 \\cdot 0.65 / 1\\,200} \\approx 0.01376$, $z = (0.30 - 0.35)/0.01376 \\approx -3.633$, giving $P(\\hat P > 0.30) \\approx 0.9999$.\n\n"
    '![AI walkthrough — sampling distribution of p_hat under p=0.3397 and p=0.35, upper-tail shading from 0.30](statistics/images/past_exams/exam_july_2025_5b_ai.png)\n\n'
    '---\n\n'
    "**Answer.**\n\n"
    "**Justification:** the new sample is large ($n = 1\\,200$) and the population is dichotomous (Bernoulli with $p$), so by the CLT the sample proportion is approximately normal with mean $p$ and variance $p(1-p)/n$. The asymptotic check is satisfied: $1\\,200 \\cdot p \\approx 408 \\gg 5$ and $1\\,200 \\cdot (1-p) \\approx 792 \\gg 5$.\n\n"
    "**Analytical expression:**\n\n"
    "$$P(\\hat P > 0.30) \\;=\\; P\\!\\left(\\,Z > \\frac{0.30 - p}{\\sqrt{p(1-p)/n}}\\,\\right) \\;=\\; 1 - \\Phi\\!\\left(\\frac{0.30 - p}{\\sqrt{p(1-p)/n}}\\right).$$\n\n"
    "**Numerical value** (using $p = 0.3397$ from 5a, $n = 1\\,200$):\n\n"
    "$$P(\\hat P > 0.30) = P\\!\\left(Z > \\frac{0.30 - 0.3397}{\\sqrt{0.3397\\cdot 0.6603/1\\,200}}\\right) = P(Z > -2.9037) \\approx \\mathbf{0.998}.$$\n\n"
    "With the alternative value $p = 0.35$ (as suggested by the prompt): $P(\\hat P > 0.30) \\approx \\mathbf{0.9999}$.\n\n"
    "Interpretation: in a fresh sample of 1\u202f200 clients from the other branch, the probability that *at least 30%* of them exceed card-usage intensity $5.5$ is **essentially one** — the new sample is so large that the sample proportion will land in a narrow band around its mean ($\\approx 0.34$ or $0.35$), well above $0.30$.\n\n"
    "**R commands:**\n\n"
    "`p_hat <- mean(BankClients$Cards > 5.5)   # 0.3397`\n\n"
    "`1 - pnorm(0.30, mean=p_hat, sd=sqrt(p_hat*(1-p_hat)/1200))`\n\n"
    "`## [1] 0.9982`\n\n"
    "`# Equivalent:`\n\n"
    "`1 - pnorm((0.30 - p_hat) / sqrt(p_hat*(1-p_hat)/1200))`\n\n"
    "`## [1] 0.9982`\n\n"
    "`# Alternative value p = 0.35:`\n\n"
    "`1 - pnorm(0.30, mean=0.35, sd=sqrt(0.35*0.65/1200))`\n\n"
    "`## [1] 0.9999`\n"
), "images": [
    "statistics/images/past_exams/exam_july_2025_5b_ai.png",
]}

# =================== SEPTEMBER 2024 ===================

past_exams["exam_sep_2024_1a"] = {
"title": "Sep-2024 Ex1a — 5th percentile of Total_Income (normal approx, μ≈27000, σ≈7145)",
"is_exam": True, "topic_hint": "G10",
"content": (
    '<span class="exam-question-text">From the `distr.summary.x(~Total_Income, data=Credit)` output below, find the value of `Total_Income` below which 5% of customers fall, assuming a normal approximation.\n\n'
    '```\n> distr.summary.x(~Total_Income, data=Credit)\nSummary measures for Total_Income | Eligible\n  n       min      max      mean     median     sd        skewness\n  8000    1006.0   55997.5  27003.5  25004.5    7144.97   0.62\n```</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2024_1a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Two summaries from the table — $\\hat\\mu = 27003.5$ and $\\hat\\sigma = 7144.97$ — fix the candidate normal distribution. Under the **normal approximation** $X\\sim N(\\hat\\mu,\\hat\\sigma^2)$, the 5th percentile is the value $x_{0.05}$ with $P(X\\le x_{0.05})=0.05$, i.e. the cutoff that isolates the **lower 5% tail**. Standardising, $x_{0.05}=\\hat\\mu+z_{0.05}\\,\\hat\\sigma$ with $z_{0.05}=-1.645$. **Caveat.** Reported skewness $0.62>0$ together with `mean > median` ($27003.5>25004.5$) signal a **right-skewed** distribution; the symmetric-normal tail therefore *under-estimates* the true left-tail percentile.\n\n'
    '![AI walkthrough — N(27003.5, 7144.97^2) with 5% lower-tail shaded at x_{0.05}~15,250 plus skewness caveat](statistics/images/past_exams/exam_sep_2024_1a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Estimates from the table: $\\hat\\mu = 27003.5$, $\\hat\\sigma = 7144.97$. Assuming $X\\sim N(\\hat\\mu,\\hat\\sigma^2)$,\n\n'
    '$$x_{0.05}=\\hat\\mu+z_{0.05}\\,\\hat\\sigma = 27003.5 - 1.645\\cdot 7144.97 \\approx €15{,}250.$$\n\n'
    'Using the rounded values $\\mu=27000$, $\\sigma=7000$: $27000 - 1.645\\cdot 7000 \\approx €15{,}485$.\n\n'
    '**Caveat — skewness.** Sample skewness $\\approx 0.62$ (positive, non-trivial) and `mean > median` ($27003.5 > 25004.5$) $\\Rightarrow$ the distribution is **right-skewed**; the normal approximation **under-estimates** the true left-tail percentile, so treat €15,250 as indicative only.\n\n'
    '**R commands:**\n\n'
    '`distr.summary.x(~Total_Income, data=Credit)`\n\n'
    '`mu <- 27003.5; sigma <- 7144.97`\n\n'
    '`qnorm(0.05, mean=mu, sd=sigma)`\n\n'
    '`## [1] 15249.4`\n\n'
    '`qnorm(0.05, mean=27000, sd=7000)`  # rounded version\n\n'
    '`## [1] 15486.21`\n\n'
    '`quantile(Credit$Total_Income, probs=0.05, na.rm=TRUE)`  # empirical, no normality\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_sep_2024_1a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2024_1a_question.png",
    "statistics/images/past_exams/exam_sep_2024_1a_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2024_1a_answer.png",
]}

past_exams["exam_sep_2024_2a"] = {
"title": "Sep-2024 Ex2a — Histogram of Score (unequal-width classes, specific branch)",
"is_exam": True, "topic_hint": "G1",
"content": (
    '<span class="exam-question-text">Provide a **sketch of the histogram** obtained from the data of the particular branch (`Score Classes`: [0,200) 30%, [200,300) 20%, [300,600) 30%, [600,1000) 20%). **Compare** with the histogram of the main branches.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2024_2a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** The four `Score` classes have **unequal widths** (200, 100, 300, 400). When widths differ, plotting raw relative frequency on the y-axis is **misleading**: a fat 30%-class would look as tall as a slim 30%-class even though the data are spread very differently. The correct y-axis is the **density**\n\n'
    '$$f_j \\;=\\; \\frac{\\text{rel. freq.}_j}{\\text{width}_j},\\qquad \\text{so that area} = f_j\\cdot w_j = \\text{rel. freq.}_j.$$\n\n'
    'With this rescaling the **bar areas equal the proportions** and the **modal class** is the one with the largest *density* (not the largest frequency).\n\n'
    '![AI walkthrough — wrong (rel-freq) vs correct (density) histogram; specific vs main branches](statistics/images/past_exams/exam_sep_2024_2a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Compute density = (rel. freq.) / (class width):\n\n'
    '| Class | % freq | Width | Density |\n'
    '|---|---|---|---|\n'
    '| [0, 200) | 0.30 | 200 | **0.0015** |\n'
    '| [200, 300) | 0.20 | 100 | **0.0020** |\n'
    '| [300, 600) | 0.30 | 300 | **0.0010** |\n'
    '| [600, 1000) | 0.20 | 400 | **0.0005** |\n\n'
    '**Modal class — specific branch:** $[200, 300)$ (density $0.002$, tallest bar). **Modal class — main branches:** $[300, 600)$. The **specific branch is shifted leftward** toward lower scores: its mode sits in $200$–$300$ rather than $300$–$600$, and densities decay monotonically for `Score` $\\ge 300$. Shape is **unimodal**, **right-skewed** (long right tail to 1000).\n\n'
    '**R commands:**\n\n'
    "`distr.table.x(Score, interval=T, freq=c('counts','dens'), data=Credit)`\n\n"
    "`distr.plot.x(Score, plot.type='hist', breaks=c(0,200,300,600,1000), data=Credit)`\n\n"
    '`## y-axis = Density (because class widths differ)`\n\n'
    '`## modal class (specific branch) = [200,300)`\n\n'
    '`## modal class (main branches)   = [300,600)  -> leftward shift`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_sep_2024_2a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2024_2a_question.png",
    "statistics/images/past_exams/exam_sep_2024_2a_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2024_2a_answer.png",
]}

past_exams["exam_sep_2024_3a"] = {
"title": "Sep-2024 Ex3a — Interpret Account_length coefficient in regression",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15a",
"content": (
    '<span class="exam-question-text">Interpret $\\hat\\beta_2 = 7.84$ for `Account_length` (second slope in the multiple-regression output).</span>\n\n'
    '*(Source `summary(m)` printout shared with sub-parts 3b/3c/3d — see e.g. `exam_sep_2024_3b_question.png`.)*\n\n'
    '---\n\n'
    '**Walkthrough.** *This is **row 1** of the universal regression case table at the top of master entry `g15a`*: $\\hat\\beta_j$ from OLS is the **partial / ceteris-paribus** mean shift in $Y$ per $+1$ unit of $X_j$ holding all the other regressors fixed. In a multiple regression $\\widehat{\\mathrm{Score}} = \\hat\\beta_0 + \\hat\\beta_1 X_1 + \\hat\\beta_2\\,\\mathrm{Account\\_length} + \\hat\\beta_3 X_3 + \\dots$, the slope $\\hat\\beta_2$ is the **partial effect** of `Account_length` on `Score`: the expected change in $\\mathrm{Score}$ for a **one-unit increase** in `Account_length` **holding all other predictors fixed** (*ceteris paribus*). Here `Account_length` is measured in years, so a $+1$-year shift moves the conditional mean of `Score` up by exactly $\\hat\\beta_2 = 7.84$ units. Two cautions: (i) this is a *marginal* effect, not a causal claim — it only describes the linear association inside the fitted model; (ii) the value is an estimate, so a 95% CI from `confint(mod)` quantifies uncertainty around 7.84.\n\n'
    '![AI walkthrough — partial slope of Score vs Account_length with a +1-year step highlighting the +7.84 Score increase](statistics/images/past_exams/exam_sep_2024_3a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Holding all other predictors constant, **a one-year increase in `Account_length` is associated with a $+7.84$-unit increase in the expected `Score`** on average. The coefficient is a *partial / ceteris-paribus* effect inside the multiple-regression model, not a causal effect; the `confint(mod)` row for `Account_length` gives the 95% CI around 7.84 and the `summary(mod)` table reports its standard error and p-value (significant if `Pr(>|t|) < 0.05`).\n\n'
    '**R commands:**\n\n'
    '`summary(mod)`\n\n'
    "`confint(mod)['Account_length',]`\n\n"
    '`## 2.5 %    97.5 %`\n\n'
    '`## (lower)  (upper)   # CI around 7.84`\n'
), "images": [
    "statistics/images/past_exams/exam_sep_2024_3a_ai.png",
]}

past_exams["exam_sep_2024_3d"] = {
"title": "Sep-2024 Ex3d — Homoscedasticity check from residuals",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15e",
"content": (
    '<span class="exam-question-text">Explain what the assumption of homoscedasticity for a linear regression model consists of. Assess whether this assumption is reasonably respected for the estimated model `m`, justifying your answer.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2024_3d_question.png)\n\n'
    '---\n\n'
    "**Walkthrough.** *Homoscedasticity* means the error variance $\\Var(\\varepsilon_i)=\\sigma^2$ is **constant** across all levels of the predictors — the opposite of *heteroscedasticity*, where the spread of $\\varepsilon$ depends on $x$. The standard visual diagnostic is `plot(m, which=1)` (residuals vs fitted): under homoscedasticity the cloud sits in a roughly **uniform horizontal band** around 0 with the LOWESS smooth flat; under heteroscedasticity the spread *fans out* (funnel / cone). The schematic below contrasts the two patterns — left = model `m`'s shape (good), right = a textbook funnel (bad). A formal complement is the **Breusch-Pagan** test (`lmtest::bptest`): large p-value $\\Rightarrow$ fail to reject homoscedasticity.\n\n"
    '![AI walkthrough — uniform band (model m, OK) vs funnel (heteroscedastic, NOT OK)](statistics/images/past_exams/exam_sep_2024_3d_ai.png)\n\n'
    '---\n\n'
    "**Answer.** Homoscedasticity = the variance of the regression errors $\\Var(\\varepsilon_i)$ is **constant** across all levels of the predictors. For model `m`, the residuals-vs-fitted plot shows a cloud spreading across fitted values $\\approx 200$–$800$ with a few labelled outliers (**#15**, **#362**, **#359**) but **no systematic funnel/cone** widening — the dispersion looks essentially constant. Hence the assumption is **reasonably respected**.\n\n"
    "**R commands:**\n\n"
    "`plot(m, which=1)`            # residuals vs fitted (visual check)\n\n"
    "`plot(m, which=3)`            # scale-location (sqrt|std-resid| vs fitted)\n\n"
    "`library(lmtest); bptest(m)`  # Breusch-Pagan formal test\n\n"
    "`## p > alpha => fail to reject H0 of homoscedasticity`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_sep_2024_3d_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2024_3d_question.png",
    "statistics/images/past_exams/exam_sep_2024_3d_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2024_3d_answer.png",
]}

# =================== SEPTEMBER 2025 ===================

past_exams["exam_sep_2025_1a"] = {
"title": "Sep-2025 Ex1.a — Scatter VO2.max vs Performance",
"is_exam": True, "topic_hint": "G9",
"content": (
    '<span class="exam-question-text">Use a scatterplot to investigate the association between `Performance` and `VO2.max`. Comment on the correlation coefficient and whether it reliably measures the strength of the association.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2025_1a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Two continuous variables → **scatterplot** plus a numerical summary of the linear co-movement. The **Pearson correlation** $r = \\dfrac{\\sum (x_i-\\bar x)(y_i-\\bar y)}{\\sqrt{\\sum(x_i-\\bar x)^2\\,\\sum(y_i-\\bar y)^2}} \\in [-1,1]$ measures **only the linear part** of the dependence: it is large in absolute value when (++) and (--) quadrants around $(\\bar x, \\bar y)$ dominate, and is **dragged down** by curvature, heavy tails, or outliers. $r$ is therefore *reliable* as a strength-of-association summary precisely when the scatter is roughly **linear, homoscedastic, and outlier-free**. On the `Performance` dataset the cloud satisfies all three conditions, so $r \\approx 0.59$ is a meaningful number.\n\n'
    '![AI walkthrough — scatter with OLS fit and (++)/(--) quadrants](statistics/images/past_exams/exam_sep_2025_1a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** The scatterplot shows a **moderately strong positive linear trend**: higher `VO2.max` tends to come with higher `Performance`. Sample correlation $r \\approx 0.593$ (slide value; the `.Rdata` file gives $\\approx 0.576$). Because the cloud is **roughly linear with no severe outliers and roughly constant spread**, the Pearson correlation **is a reliable summary** of the strength of the linear association between `VO2.max` and `Performance` — it would be misleading only if the cloud were curved or had heavy-tail outliers.\n\n'
    '**R commands:**\n\n'
    "`distr.plot.xy(VO2.max, Performance, plot.type='scatter', fitline=T, data=Performance)`\n\n"
    '`cor(Performance$VO2.max, Performance$Performance)`\n\n'
    '`## [1] 0.593`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_sep_2025_1a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2025_1a_question.png",
    "statistics/images/past_exams/exam_sep_2025_1a_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2025_1a_answer.png",
]}

past_exams["exam_sep_2025_2a"] = {
"title": "Sep-2025 Ex2a — One-sided test on mean difference D (new vs old algorithm)",
"is_exam": True, "topic_hint": "G14", "subtopic_hint": "g14c",
"content": (
    '<span class="exam-question-text">Formulate $H_0$ and $H_1$ for the **mean difference in performance** $D = \\mu_{\\text{new}} - \\mu_{\\text{old}}$ between the new algorithm and the old one, give the test statistic and its estimated standard error, then evaluate the numerical value and conclude. Sample: $\\bar D = 0.510$, $SE(\\bar D) = 0.221$, $n$ large.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2025_2a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Two pieces of information: $\\bar D = 0.510$ and $SE(\\bar D) = 0.221$, large $n$. The claim "the new algorithm is *better*" lives in the alternative → **one-sided** (upper-tail) test. Build the $z$-statistic $z = \\bar D / SE$ and compare with $z_{0.95} = 1.645$ (or use the p-value). Because $n$ is large the **CLT** lets us use the Normal reference and avoid any normality assumption on the $D_i$.\n\n'
    '![AI walkthrough — Normal density with rejection region & p-value tail, plus CI95 bar for D-bar](statistics/images/past_exams/exam_sep_2025_2a_ai.png)\n\n'
    '---\n\n'
    '**Hypotheses (one-sided, upper-tail).** Let $D = \\mu_{\\text{new}} - \\mu_{\\text{old}}$. The claim that the new algorithm performs *better* is the alternative:\n\n'
    '$H_0: \\mu_D = 0$  vs  $H_1: \\mu_D > 0$.\n\n'
    '**Test statistic.** Working on the single derived variable $D$ (one-sample / paired-style test on the mean difference) with sample mean $\\bar D$ and estimated standard error $SE(\\bar D) = s_D/\\sqrt{n}$,\n\n'
    '$$z = \\frac{\\bar D - 0}{SE(\\bar D)} = \\frac{\\bar D}{s_D/\\sqrt n}.$$\n\n'
    '**Assumptions.** (i) the $n$ observed differences $D_i$ are an i.i.d. sample; (ii) $n$ is large enough that the CLT applies, so under $H_0$ the standardised statistic is approximately $\\mathcal N(0,1)$ — this justifies using a $z$ (Normal) reference distribution rather than a Student-$t$, and avoids any normality assumption on the $D_i$ themselves.\n\n'
    '**Numerical value.** With $\\bar D = 0.510$ and $SE(\\bar D) = 0.221$:\n\n'
    '$$z = \\frac{0.510}{0.221} \\approx 2.308.$$\n\n'
    '**P-value (one-sided).** $p = 1 - \\Phi(2.308) \\approx 0.0105$.\n\n'
    '**Conclusion.** $p \\approx 0.0105 < 0.05$ (and $< 0.025$), so we **reject $H_0$** at the 5% (and even 2.5%) level: there is significant evidence that the new algorithm has *higher* mean performance than the old one. At the conservative 1% level the test would not reject.\n\n'
    '---\n\n'
    '**Answer.** $z_{\\text{obs}} = 0.510/0.221 \\approx 2.308$, one-sided $p \\approx 0.0105$. **Reject $H_0$** at the $5\\%$ level (and $2.5\\%$): the new algorithm has a significantly higher mean performance than the old one ($\\bar D = 0.510$, $SE = 0.221$); at $1\\%$ the test does not reject.\n\n'
    '**R commands:**\n\n'
    "`D  <- 0.510                 # sample mean difference`\n\n"
    "`SE <- 0.221                 # estimated SE of D-bar`\n\n"
    "`z  <- D / SE                # standardised statistic`\n\n"
    '`z`\n\n'
    '`## [1] 2.30769`\n\n'
    "`p <- 1 - pnorm(z)           # upper-tail p-value (Normal, large n)`\n\n"
    '`p`\n\n'
    '`## [1] 0.01051`\n\n'
    "`# two-sided variant: 2*(1 - pnorm(z)) approx 0.021`\n\n"
    "`# from raw data (paired / derived):`\n\n"
    "`# d <- Performance.new - Performance.old`\n\n"
    "`# t.test(d, mu=0, alternative='greater')`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_sep_2025_2a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2025_2a_question.png",
    "statistics/images/past_exams/exam_sep_2025_2a_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2025_2a_answer.png",
]}

past_exams["exam_sep_2025_5a"] = {
"title": "Sep-2025 Ex5a — Assumptions for the CI on $\\mu_A - \\mu_B$",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13c",
"content": (
    '<span class="exam-question-text">Clarify whether specific assumptions are required to construct a confidence interval for the difference between the two means $\\mu_A - \\mu_B$ (Activity.type A vs B). Clearly motivate your answer.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2025_5a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Independent-samples CI on $\\mu_A - \\mu_B$ rests on three pillars: **(i) independence** of the two samples (here guaranteed by design — random assignment to programs A vs B), **(ii) Normality of the response within each group**, and **(iii) equality of population variances** (decides pooled-variance vs Welch). With $n_A = 58$ and $n_B = 380$ we can **relax (ii)**: the CLT makes $\\bar X_A - \\bar X_B$ approximately Normal even if `Performance` itself is skewed. Assumption (iii) is **checked** with a Levene test in 5.b. The R call `CI.diffmean(..., var.test=TRUE, conf.level=0.90)` returns the pooled and Welch CIs *and* the Levene test in a single shot.\n\n'
    '![AI walkthrough — CLT for $\\bar X_A-\\bar X_B$ + assumptions checklist](statistics/images/past_exams/exam_sep_2025_5a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** To build a CI for the difference between two means with **independent samples** we need:\n\n'
    '- **(i) Independence** — A and B participants are randomly assigned to different training programs, so the two samples are independent.\n'
    '- **(ii) Normality of the response within each group** — we can **relax** this because $n_A = 58$ and $n_B = 380$ are large enough for the **CLT** to apply: $\\bar X_A - \\bar X_B$ is approximately Normal even if `Performance` is not.\n'
    '- **(iii) Equality of population variances** — required to decide between the **pooled-variance** estimator and the **Welch** (separate-variances) estimator; verified empirically in 5.b via a **Levene\'s test**.\n\n'
    'The R call `CI.diffmean(..., var.test=TRUE, conf.level=0.90)` produces both CIs (equal- and unequal-variance, each via Normal approx and Student-t) **and** the Levene test in one shot.\n\n'
    '**R commands:**\n\n'
    "`CI.diffmean(Performance$Performance[Performance$Activity.type=='A'],`\n\n"
    "`            Performance$Performance[Performance$Activity.type=='B'],`\n\n"
    "`            type='independent', var.test=TRUE, conf.level=0.90)`\n\n"
    '`## Confidence interval for mu_x - mu_y`\n\n'
    '`## Samples: independent ; Confidence level: 0.9 ; Variances: unknown`\n\n'
    '`## Unknown variances assumed to be equal`\n\n'
    '`##                 n_x n_y xbar  ybar  xbar-ybar  s_X   s_Y   se    Lower  Upper`\n\n'
    '`## Normal.Approx   58  380 78.17 82.74 -4.57      6.66  6.53  0.92  -6.09  -3.05`\n\n'
    '`## Student-t       58  380 78.17 82.74 -4.57      6.66  6.53  0.92  -6.09  -3.05`\n\n'
    '`## Levene test for homogeneity of variance`\n\n'
    '`## s2_x  s2_y  F-stat  df1 df2  p-value`\n\n'
    '`## 44.42 42.66  0.41   1   436  0.52397`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_sep_2025_5a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2025_5a_question.png",
    "statistics/images/past_exams/exam_sep_2025_5a_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2025_5a_answer.png",
]}

past_exams["exam_sep_2025_5b"] = {
"title": "Sep-2025 Ex5b — Levene\'s test: equal vs different variances",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13c",
"content": (
    '<span class="exam-question-text">Should we assume the population variances of `Performance` in the two groups (A and B) are **equal** or **different**? Justify clearly your answer.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2025_5b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** The choice between the **pooled-variance** Student-$t$ CI/test and **Welch**\'s approximation hinges on whether the two populations share a variance. The data-driven check is a **Levene\'s test** on equality of variances:\n\n'
    '$$H_0:\\ \\sigma_A^2 = \\sigma_B^2 \\qquad\\text{vs}\\qquad H_1:\\ \\sigma_A^2 \\ne \\sigma_B^2.$$\n\n'
    'Under $H_0$ Levene\'s statistic is approximately $F_{1,\\,n_A+n_B-2}$ in a two-group setting. From the output embedded in `CI.diffmean(..., var.test=TRUE)`: $s_A^2 = 44.42$, $s_B^2 = 42.66$, $F_{\\text{obs}} = 0.41$, df$_1 = 1$, df$_2 = 436$, p-value $= 0.524$. The two sample variances differ by less than 5% in relative terms and the observed $F$ sits **deep inside the fail-to-reject zone** — $p$ is larger than every standard level (0.01, 0.05, 0.10). The AI plot below shows the $F_{1,436}$ null density with the right-tail rejection regions for $\\alpha\\in\\{0.10, 0.05, 0.01\\}$ shaded and $F_{\\text{obs}}=0.41$ marked; the right panel compares the two group variances side-by-side.\n\n'
    '![AI walkthrough — F(1,436) null density with rejection regions vs F_obs (left); group sample variances bar chart (right)](statistics/images/past_exams/exam_sep_2025_5b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Apply Levene\'s test on $H_0:\\sigma_A^2=\\sigma_B^2$ vs $H_1:\\sigma_A^2\\ne\\sigma_B^2$. From `CI.diffmean(..., var.test=TRUE)`: $s_A^2 = 44.42$, $s_B^2 = 42.66$, $F_{\\text{obs}} = 0.41$ on $(1, 436)$ df, p-value $= 0.524$. Since $p$ is **larger than all common significance levels** (0.01, 0.05, 0.10), we **fail to reject $H_0$** and assume the **population variances are equal** $\\Rightarrow$ use the **pooled-variance** Student-$t$ CI/test (top block of the R output), not Welch\'s.\n\n'
    '**R commands:**\n\n'
    "`# Levene is reported inside CI.diffmean when var.test=TRUE`\n\n"
    "`CI.diffmean(Performance$Performance[Performance$Activity.type=='A'],`\n\n"
    "`            Performance$Performance[Performance$Activity.type=='B'],`\n\n"
    "`            type='independent', var.test=TRUE, conf.level=0.90)`\n\n"
    "`## Levene test for homogeneity of variance`\n\n"
    "`## H0: s2_x = s2_y   vs   H1: s2_x != s2_y`\n\n"
    "`## s2_x  s2_y  F-stat  df1  df2  p-value`\n\n"
    "`## 44.42 42.66  0.41   1    436  0.52397`\n\n"
    "`# Equivalent stand-alone call`\n\n"
    "`library(car)`\n\n"
    "`leveneTest(Performance ~ Activity.type, data=subset(Performance, Activity.type %in% c('A','B')))`\n\n"
    "`# p > 0.10 -> keep var.equal=TRUE (pooled)`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_sep_2025_5b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2025_5b_question.png",
    "statistics/images/past_exams/exam_sep_2025_5b_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2025_5b_answer.png",
]}

past_exams["exam_sep_2025_5c"] = {
"title": "Sep-2025 Ex5c — Analytical 90% pooled-variance CI for $\\mu_A - \\mu_B$",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13c",
"content": (
    '<span class="exam-question-text">Provide the **analytical expression** of the confidence interval under the proper assumption on the population variances (decided in 5.b), and report the **90% confidence interval** for the difference in the mean `Performance` between participants trained with programs A and B.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2025_5c_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** From 5.b Levene\'s test failed to reject equality of variances ($p = 0.524$), so we adopt the **pooled-variance Student-t** estimator. The pivot is\n\n'
    '$$T \\;=\\; \\frac{(\\bar X_A - \\bar X_B) - (\\mu_A - \\mu_B)}{\\sqrt{s^2_{\\text{pooled}}\\bigl(\\tfrac{1}{n_A}+\\tfrac{1}{n_B}\\bigr)}} \\;\\sim\\; t_{n_A+n_B-2}, \\qquad s^2_{\\text{pooled}} = \\frac{(n_A-1)s_A^2 + (n_B-1)s_B^2}{n_A+n_B-2}.$$\n\n'
    'Inverting the pivot at level $1-\\alpha=0.90$ gives the **two-sided CI**\n\n'
    '$$\\bar x_A - \\bar x_B \\;\\pm\\; t_{n_A+n_B-2,\\,0.95}\\,\\sqrt{s^2_{\\text{pooled}}\\Bigl(\\tfrac{1}{n_A}+\\tfrac{1}{n_B}\\Bigr)}.$$\n\n'
    'With $n_A+n_B-2 = 436$ df the $t$-quantile $t_{436,\\,0.95} \\approx 1.648$ is **practically identical** to the Normal quantile $z_{0.95}=1.645$ (large-sample regime). Plug in $\\bar x_A - \\bar x_B = -4.57$, $s_A=6.66$, $s_B=6.53$, $n_A=58$, $n_B=380$ $\\Rightarrow$ $SE = 0.923$ $\\Rightarrow$ half-width $\\approx 1.648\\cdot 0.923 = 1.52$, i.e. CI $= [-6.09,\\,-3.05]$.\n\n'
    '![AI walkthrough — Student-t(436) with central 90% area shaded (left); 90% CI for $\\mu_A-\\mu_B$ on a number line, point estimate -4.57, zero reference (right).](statistics/images/past_exams/exam_sep_2025_5c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** **Analytical expression:**\n\n'
    '$$\\bar x_A - \\bar x_B \\;\\pm\\; t_{n_A+n_B-2,\\,0.95}\\sqrt{\\dfrac{s^2_{\\text{pooled}}}{n_A} + \\dfrac{s^2_{\\text{pooled}}}{n_B}}, \\qquad s^2_{\\text{pooled}} = \\dfrac{s_A^2(n_A-1) + s_B^2(n_B-1)}{n_A + n_B - 2}.$$\n\n'
    '**Numerical 90% CI:** $-4.57 \\pm 1.648\\cdot 0.923 \\;=\\; \\mathbf{[-6.09,\\;-3.05]}$.\n\n'
    '**Interpretation.** With 90% confidence the difference $\\mu_A-\\mu_B$ lies between $-6.09$ and $-3.05$; the interval lies **entirely below 0**, so the average `Performance` of participants trained with program A is **significantly lower** than that of program B (at the 10% level).\n\n'
    '**R commands:**\n\n'
    "`CI.diffmean(Performance$Performance[Performance$Activity.type=='A'],`\n\n"
    "`            Performance$Performance[Performance$Activity.type=='B'],`\n\n"
    "`            type='independent', var.test=TRUE, conf.level=0.90)`\n\n"
    '`## Student-t   ...  xbar-ybar  se    Lower  Upper`\n\n'
    '`##             ...  -4.57      0.92  -6.09  -3.05`\n\n'
    '`# manual reconstruction`\n\n'
    '`nA <- 58;  sA <- 6.66`\n\n'
    '`nB <- 380; sB <- 6.53`\n\n'
    '`sp2 <- ((nA-1)*sA^2 + (nB-1)*sB^2) / (nA+nB-2)`\n\n'
    '`tc  <- qt(0.95, df=nA+nB-2)`\n\n'
    '`-4.57 + c(-1,1) * tc * sqrt(sp2*(1/nA + 1/nB))`\n\n'
    '`## [1] -6.09 -3.05`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_sep_2025_5c_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2025_5c_question.png",
    "statistics/images/past_exams/exam_sep_2025_5c_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2025_5c_answer.png",
]}
# =====================================================================
# Gap-fill additions (2026-06-06) — sub-parts that were not in the
# original 13-agent transcription pass. Marked yellow as exam cells.
# =====================================================================

# ---- 1st partial 2026 (Q2-Q6) ----
past_exams["exam_p1_2026_2"] = {
"title": "P1-2026 Ex2 — Relationship between Bid and PaidFare (scatter, Pearson r)",
"is_exam": True, "topic_hint": "G8",
"content": (
    '<span class="exam-question-text">Describe the relationship between `Bid` and `PaidFare` from the scatterplot. Compute the Pearson correlation and comment on whether it is an appropriate summary.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2026_2_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Two continuous variables → **scatterplot** + a correlation coefficient. The cloud shows a steep drop of `Bid` for low `PaidFare` that flattens out for high `PaidFare` — a hyperbolic / power-decay shape. Pearson $r$ measures only **linear** co-movement; on a clearly curved cloud it **understates** the true dependence. A **monotone-rank** measure (**Spearman $\\rho$**) — Pearson computed on the ranks — or a transformation (e.g. $\\log$, $1/x$) is a better summary for monotone non-linear data. On the real `Bidding` sample, Pearson $r = -0.7947$ while Spearman $\\rho \\approx -0.87$ (closer to $-1$): the rank-based statistic better captures the strength of the curved monotone link.\n\n'
    '![AI walkthrough — Pearson r vs Spearman rho on the Bid/PaidFare cloud](statistics/images/past_exams/exam_p1_2026_2_ai.png)\n\n'
    '---\n\n'
    '**Answer.** The scatterplot shows a **clear inverse, non-linear** relationship: as `PaidFare` increases, `Bid` decreases steeply at first and then flattens out (hyperbolic / power-decay shape). Pearson correlation $r = -0.7947$ — strongly negative. **Caveat**: Pearson $r$ only measures *linear* association, so it **understates** the true (curved) dependence between the two variables. A monotone-rank measure (**Spearman $\\rho$**) or a transformation (e.g. $\\log$ or $1/x$) would describe the link better; equivalently, one could fit a non-linear model rather than report $r$ alone.\n\n'
    '**R commands:**\n\n'
    "`distr.plot.xy(x=PaidFare, y=Bid, plot.type='scatter', fitline=T, data=Bidding)`\n\n"
    "`cor(Bidding$PaidFare, Bidding$Bid)`\n\n"
    "`## [1] -0.7947379`\n\n"
    "`cor(Bidding$PaidFare, Bidding$Bid, method='spearman')   # rank-based, monotone`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_p1_2026_2_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2026_2_question.png",
    "statistics/images/past_exams/exam_p1_2026_2_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2026_2_answer.png",
]}

past_exams["exam_p1_2026_4"] = {
"title": "P1-2026 Ex4 — 10th and 90th percentiles of PaidFare",
"is_exam": True, "topic_hint": "G6",
"content": (
    '<span class="exam-question-text">Compute and interpret the 10th and 90th percentiles ($p_{10}$, $p_{90}$) of `PaidFare`.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2026_4_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** A percentile $p_\\alpha$ is the value below which a fraction $\\alpha$ of the data lies. With $\\alpha = 0.10$ we cut off the lower 10% tail; with $\\alpha = 0.90$ we cut off the upper 10%. Together they isolate the **central 80%** of the distribution, and their difference $p_{90} - p_{10}$ is the **interdecile range** — a *robust* spread measure (it ignores the most extreme 20% of observations, unlike the full range or the standard deviation).\n\n'
    '![AI walkthrough — PaidFare with p10/p90 tails shaded and interdecile range bracketed](statistics/images/past_exams/exam_p1_2026_4_ai.png)\n\n'
    '---\n\n'
    '**Answer.**\n\n'
    '$$p_{10} = 39.46, \\qquad p_{90} = 85.61.$$\n\n'
    '**Interpretation.** 10% of bookings paid less than 39.46; 10% paid more than 85.61; the central **80% of bookings** lie in $[39.46,\\; 85.61]$.\n\n'
    '**Interdecile range.** $p_{90} - p_{10} = 85.61 - 39.46 = 46.15$ — a *robust* spread measure, insensitive to extreme outliers (unlike the full range or $s$).\n\n'
    '**R commands:**\n\n'
    "`distr.summary.x(x=PaidFare, data=Bidding, stats=c('p10','p90'))`\n\n"
    '`quantile(Bidding$PaidFare, probs=c(0.10, 0.90))`\n\n'
    '`## 10%      90%`\n\n'
    '`## 39.465   85.615`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2026_4_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2026_4_question.png",
    "statistics/images/past_exams/exam_p1_2026_4_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2026_4_answer.png",
]}

past_exams["exam_p1_2026_5"] = {
"title": "P1-2026 Ex5 — LeadTime x Channel: share of MediumTerm customers (joint vs row %)",
"is_exam": True, "topic_hint": "G7",
"content": (
    '<span class="exam-question-text">`LeadTime` (Early / **MediumTerm** = average advance / LastMinute) crossed with `Channel` (Aggregator / Agency / Airline). Report the **percentage of customers who bought their ticket with an average advance** (MediumTerm) **among clients who used a comparison platform** (`Channel = Aggregator`) and **among clients who bought via an Agency** (`Channel = Agency`). Recommend a chart for the two `Channel` groups. **Do NOT reorder the levels of LeadTime.**</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2026_5_question.png)\n\n'
    '---\n\n'
    "**Walkthrough.** The exam asks for *shares of the whole customer base* falling into specific `Channel`x`LeadTime` cells — that is the definition of a **joint** percentage $f_{ij}=n_{ij}/n$ (denominator = total $n=668$). The natural temptation is to read **row percentages** $P(\\text{LeadTime}\\mid \\text{Channel})$ off the table, but those condition on the channel (denominator = that channel's count) and answer a *different* question ('how is each channel split across LeadTime?'). The AI plot below contrasts the two readings on the same cells: left panel = the trap (row %), right panel = the correct joint %.\n\n"
    '![AI walkthrough — row % (trap) vs joint % (answer) for the MediumTerm cells](statistics/images/past_exams/exam_p1_2026_5_ai.png)\n\n'
    '---\n\n'
    "**Answer.** Joint percentages $f_{ij}=n_{ij}/n$ on the MediumTerm row: Aggregator+MediumTerm $=72/668=10.78\\%$, Agency+MediumTerm $=76/668=11.38\\%$, Airline+MediumTerm $=107/668=16.02\\%$.\n\n"
    "Among comparison-platform buyers about **11%** of all customers bought MediumTerm; among Agency buyers also about **11%**. The two groups have very similar joint shares of MediumTerm. **Recommended chart:** a side-by-side bar chart of joint percentages by `Channel` x `LeadTime`.\n\n"
    "**Trap (for contrast).** The *row* percentages $P(\\text{MediumTerm}\\mid\\text{Channel})$ are 51% (Aggregator), 25% (Agency), 48% (Airline) — they look very different from the joint readings and they answer the wrong question.\n\n"
    '**R commands:**\n\n'
    "`Bidding$LeadTime.F <- factor(Bidding$LeadTime, levels=c('Early','MediumTerm','LastMinute'))`\n\n"
    "`distr.table.xy(x=Channel, y=LeadTime.F, data=Bidding, freq='perc')`  # joint % — answer the question\n\n"
    "`## Aggregator MediumTerm 10.78%   Agency MediumTerm 11.38%   Airline MediumTerm 16.02%`\n\n"
    "`distr.table.xy(x=Channel, y=LeadTime.F, data=Bidding, freq='perc', freq.type='y|x')`  # row % — for contrast\n\n"
    "`## Aggregator 51%   Agency 25%   Airline 48%  -- DIFFERENT question`\n\n"
    "`distr.plot.xy(x=Channel, y=LeadTime.F, data=Bidding, type='barplot', freq='perc')`  # recommended visual\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_p1_2026_5_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2026_5_question.png",
    "statistics/images/past_exams/exam_p1_2026_5_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2026_5_answer.png",
]}

past_exams["exam_p1_2026_6a"] = {
"title": "P1-2026 Ex6a — Sample means and SE for Aggregator/Airline",
"is_exam": True, "topic_hint": "G11",
"content": (
    '<span class="exam-question-text">From the by-`Channel` summary of `PaidFare` (Aggregator / Agency / Airline) report the **sample mean** $\\bar x$ and the **standard error of the mean** $SE(\\bar x)$ for the two channels **Aggregator** and **Airline**.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2026_6a_question.png)\n\n'
    '---\n\n'
    "**Walkthrough.** The summary table gives, for each `Channel`, the sample size $n$, the sample mean $\\bar x$ and the sample standard deviation $s$. The **sample mean** is read off directly. The **standard error of the mean** measures the variability of $\\bar x$ as an estimator of the population mean $\\mu$ and is computed as $SE(\\bar x) = s / \\sqrt{n}$. Notice that $SE$ is *not* the spread of the data ($s$) but the spread of the *sampling distribution of $\\bar x$* — it shrinks as $n$ grows. Plugging in the table values gives the two SEs below.\n\n"
    '![AI walkthrough — sample means with +/- SE error bars and the formula SE = s / sqrt(n)](statistics/images/past_exams/exam_p1_2026_6a_ai.png)\n\n'
    '---\n\n'
    "**Answer.**\n\n"
    "- **Aggregator:** $\\bar x = 56.22$, $s = 12.13$, $n = 142$ $\\;\\Rightarrow\\;$ $SE(\\bar x) = 12.13 / \\sqrt{142} \\approx 1.0179$.\n"
    "- **Airline:** $\\bar x = 53.50$, $s = 22.06$, $n = 224$ $\\;\\Rightarrow\\;$ $SE(\\bar x) = 22.06 / \\sqrt{224} \\approx 1.4739$.\n\n"
    "Although Airline has the larger $n$, its much larger $s$ dominates, so $SE_{\\text{Airline}} > SE_{\\text{Aggregator}}$ (about $1.45\\times$ larger). This says the *Aggregator* sample mean is, on average, a tighter estimator of its population mean than the Airline one — but it does **not** say a particular realised $\\bar x_{\\text{Aggregator}}$ is closer to $\\mu_{\\text{Aggregator}}$ than a particular $\\bar x_{\\text{Airline}}$ is to $\\mu_{\\text{Airline}}$ (see Ex6b).\n\n"
    '**R commands:**\n\n'
    "`distr.summary.x(x=PaidFare, by=Channel, data=Bidding)`\n\n"
    "`## Channel       n     mean      sd`\n\n"
    "`## Aggregator  142    56.22   12.13`\n\n"
    "`## Airline     224    53.50   22.06`\n\n"
    "`12.13 / sqrt(142)    # Aggregator SE`\n\n"
    "`## [1] 1.0179`\n\n"
    "`22.06 / sqrt(224)    # Airline SE`\n\n"
    "`## [1] 1.4739`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_p1_2026_6a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2026_6a_question.png",
    "statistics/images/past_exams/exam_p1_2026_6a_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2026_6a_answer.png",
]}

past_exams["exam_p1_2026_6b"] = {
"title": "P1-2026 Ex6b — Reliability of PaidFare estimate vs SE: cannot conclude",
"is_exam": True, "topic_hint": "G11",
"content": (
    '<span class="exam-question-text">Can we conclude that one of the two PaidFare estimates is **more reliable** than the other from the fact that $SE(\\bar x_{Aggregator}) < SE(\\bar x_{Airline})$? If so, indicate which estimate is more reliable and explain why. If not, explain why no conclusion can be drawn.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_p1_2026_6b_question.png)\n\n'
    '---\n\n'
    "**Walkthrough.** $SE(\\bar X)$ describes the spread of the **sampling distribution** of the estimator $\\bar X$ across all possible samples — it tells us how far a *generic* estimate would be from $\\mu$ *on average*. The Aggregator sampling distribution ($SE\\approx 1.018$) is narrower than the Airline one ($SE\\approx 1.474$), so $\\bar X_{Agg}$ is the **more reliable estimator** in a long-run sense. But once a sample is drawn, the realised number $\\bar x$ is a single point — and a single Airline draw can perfectly well land *closer to $\\mu$* than a single Aggregator draw. SE is a statement about estimators (functions of random samples), not about individual realised numbers.\n\n"
    '![AI walkthrough — two sampling distributions + one realised draw each: smaller SE does NOT imply closer to $\\mu$ for a single estimate](statistics/images/past_exams/exam_p1_2026_6b_ai.png)\n\n'
    '---\n\n'
    "**Answer.** **No conclusions can be drawn** about which *specific* realised estimate is closer to $\\mu$. Although $SE(\\bar x_{Aggregator})<SE(\\bar x_{Airline})$, SE refers to the sampling distribution of the *estimator* — the deviation of a **generic** estimate from the parameter — NOT to the deviation of a **specific realised** PaidFare estimate. We can say the Aggregator estimator is more reliable (its estimates are *on average* more tightly clustered around the population mean), but we cannot draw conclusions about the reliability of specific realised PaidFare estimates or about their distance from the corresponding parameter.\n\n"
    '**R commands:**\n\n'
    "`# SE compares estimators (long-run), not single realised PaidFare estimates`\n\n"
    "`SE_agg <- 12.13/sqrt(142)   # 1.018`\n\n"
    "`SE_air <- 22.06/sqrt(224)   # 1.474`\n\n"
    "`c(SE_agg, SE_air)`\n\n"
    "`## [1] 1.0179 1.4739`\n\n"
    "`# A single Airline PaidFare x-bar could still land closer to mu than a single Aggregator one`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_p1_2026_6b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2026_6b_question.png",
    "statistics/images/past_exams/exam_p1_2026_6b_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2026_6b_answer.png",
]}

# ---- general 1 2025: Q6 (larger regression with Steps) ----
past_exams["exam_g1_2025_6"] = {
"title": "G1-2025 Ex6 — Larger regression SleepQuality ~ Stress+Age+BMI+Physical+Steps",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15c",
"content": (
    '<span class="exam-question-text">Estimate the larger model `lm(SleepQuality ~ Stress + Age + BMI + Physical + Steps)` and explain why the Adjusted $R^2$ is preferable to $R^2$ for comparing models with different numbers of regressors.</span>\n\n'
    '![Ex 6 question — larger regression with Steps](statistics/images/past_exams/questions/exam_g1_2025_6_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.**\n\n'
    'Two read-outs matter when you swap models with a different number of regressors: (i) the **goodness-of-fit metric used for the comparison** and (ii) the **per-coefficient story** to make sure the new regressor is doing real work.\n\n'
    '**1) Why Adjusted $R^2$ (not $R^2$)?** Plain $R^2 = 1 - SSR/TSS$ is monotone in $k$: adding *any* regressor — even pure noise — can only weakly raise it. It therefore cannot fairly compare a 4-predictor model with a 5-predictor model. The Adjusted $R^2$ penalises the regressor count:\n\n$$R^2_{\\text{adj}} = 1 - (1 - R^2)\\,\\dfrac{n-1}{n-k-1},$$\n\nso it increases **only** when the new regressor explains more variance than the one extra degree of freedom "costs". It is the right metric to compare nested/non-nested models of different sizes.\n\n'
    '**2) Per-predictor fit (larger model).** From `summary(mod)`: Intercept $\\approx -3.6847$; **Stress** $-0.014$, **Age** $+0.0011$, **BMI Normal** $+0.066$, **BMI Underweight** $+0.241$, **Physical** $+0.0049$, **Steps** $\\approx 1.529\\times 10^{-4}$. Omnibus $F \\approx 84.4$ → jointly highly significant.\n\n'
    '**3) The verdict.** Adjusted $R^2$ rises from $0.5468$ (smaller, no Steps) to $0.6592$ (larger, with Steps) — a $+0.112$ jump that survives the regressor-count penalty. The larger model is **preferable**.\n\n'
    '![Ex 6 AI walkthrough — Adj R² jump, larger-model coefficient table, why Adjusted R²](statistics/images/past_exams/exam_g1_2025_6_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Larger model: Adj $R^2 = 0.6592$ vs $0.5468$ for the smaller model; omnibus $F \\approx 84.4$ — jointly highly significant. Adjusted $R^2$ is preferred because plain $R^2$ never decreases when regressors are added, so it cannot fairly compare models of different size; Adjusted $R^2$ penalises extra regressors via $(n-1)/(n-k-1)$ and only rises if the new variable explains more than its degree-of-freedom cost — so the $+0.112$ rise here confirms the larger model is the right choice.\n\n'
    '**R commands:**\n\n'
    '`mod <- lm(SleepQuality ~ Stress + Age + BMI + Physical + Steps, data=Sleep)`\n\n'
    '`summary(mod); confint(mod)`\n\n'
    '`plot(mod, which=1); plot(mod, which=3)`\n\n'
    '![Ex 6 answer — summary(mod) output for the larger regression](statistics/images/past_exams/answers/exam_g1_2025_6_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2025_6_question.png",
    "statistics/images/past_exams/exam_g1_2025_6_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2025_6_answer.png",
]}

# ---- general 1 2026: Q2, Q3, Q5 ----
past_exams["exam_g1_2026_2a"] = {
"title": "G1-2026 Ex2a — Test independence: PurposeLoan ⊥ EmplStatus (chi-squared)",
"is_exam": True, "topic_hint": "G14",
"subtopic_hint": "g14d",
"content": (
    '<span class="exam-question-text">We are interested in whether the reason for requesting a loan (`PurposeLoan`) and the employment status (`EmplStatus`) are associated, using an appropriate test. State $H_0$ and $H_1$ and motivate rigorously.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2026_2a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** The natural test for association between two categorical variables is the **Pearson chi-squared test of independence** on the two-way table of `PurposeLoan` $\\times$ `EmplStatus`.\n\n'
    '**Hypotheses.**\n\n'
    '$$H_0:\\ \\text{PurposeLoan}\\ \\perp\\ \\text{EmplStatus} \\qquad\\text{vs}\\qquad H_1:\\ \\text{not independent (associated).}$$\n\n'
    'Under $H_0$, the expected cell counts are $E_{ij} = n_{i\\cdot}\\,n_{\\cdot j}/n$, and the test statistic\n\n'
    '$$X^2 \\;=\\; \\sum_{i,j} \\dfrac{(O_{ij} - E_{ij})^2}{E_{ij}} \\;\\overset{H_0}{\\sim}\\; \\chi^2_{(r-1)(c-1)},$$\n\n'
    'is approximately chi-squared with $(r-1)(c-1)$ degrees of freedom (here $r=5$ purpose levels, $c=3$ employment levels $\\Rightarrow$ df $=8$). Reject $H_0$ at level $\\alpha$ iff $X^2_{\\text{obs}}$ falls in the upper tail (p-value $< \\alpha$).\n\n'
    'The left panel below sketches the **visual diagnostic**: stacking `PurposeLoan` conditional on `EmplStatus` — under $H_0$ the three stacks would look identical. The right panel shows the **null $\\chi^2_8$ distribution** with the 5% rejection region shaded and the observed statistic from Ex2b ($X^2_{\\text{obs}}=11.107$) marked.\n\n'
    '![AI walkthrough — chi-squared independence test setup](statistics/images/past_exams/exam_g1_2026_2a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $H_0$: `PurposeLoan` and `EmplStatus` are **independent**. $H_1$: **not independent** (associated). Use the $\\chi^2$ test of independence on the $r\\times c$ contingency table; reject $H_0$ iff p-value $< \\alpha$.\n\n'
    '**R commands:**\n\n'
    '`chisq.test(Credit$PurposeLoan, Credit$EmplStatus)`\n\n'
    "`distr.table.xy(PurposeLoan, EmplStatus, freq='perc', freq.type='y|x', data=Credit)`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_g1_2026_2a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2026_2a_question.png",
    "statistics/images/past_exams/exam_g1_2026_2a_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2026_2a_answer.png",
]}

past_exams["exam_g1_2026_2b"] = {
"title": "G1-2026 Ex2b — χ² independence test PurposeLoan × EmplStatus (stat = 11.107)",
"is_exam": True, "topic_hint": "G14",
"subtopic_hint": "g14d",
"content": (
    '<span class="exam-question-text">Test independence between `PurposeLoan` and `EmplStatus` given the $\\chi^2$-statistic $= \\mathbf{11.107}$ with $\\mathbf{df = 8}$. Compute the p-value, state the decision and motivate.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2026_2b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Under $H_0$ (independence) the Pearson statistic $X^2 = \\sum (O_{ij}-E_{ij})^2/E_{ij}$ is approximately $\\chi^2_{(r-1)(c-1)}$. Here $df = 8$, $X^2_{\\text{obs}} = 11.107$.\n\n'
    '$$p\\text{-value} \\;=\\; P\\!\\left(\\chi^2_8 > 11.107\\right) \\;=\\; 1 - F_{\\chi^2_8}(11.107) \\;\\approx\\; 0.1958.$$\n\n'
    'The **left panel** below shows the $\\chi^2_8$ density with the right tail beyond $X^2_{\\text{obs}}$ shaded — that shaded area *is* the p-value. The critical values $\\chi^2_{0.90}=13.36$, $\\chi^2_{0.95}=15.51$, $\\chi^2_{0.99}=20.09$ all lie **to the right** of $11.107$, so the observation is **not** in any conventional rejection region. The **right panel** shows the CDF: $F_{\\chi^2_8}(11.107)\\approx 0.804$, so $1-F\\approx 0.196 > 0.10$ — well inside the do-not-reject band.\n\n'
    '![AI walkthrough — chi-squared right-tail p-value & CDF view](statistics/images/past_exams/exam_g1_2026_2b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $p\\text{-value} = P(\\chi^2_8 > 11.107) \\approx 0.196$. **Interpretation**: probability of observing a test statistic at least as extreme as the one observed, assuming $H_0$ (independence) is true. **Decision**: for any common $\\alpha$ (1%, 5%, 10%) we have $p > \\alpha$ → **do not reject** $H_0$. Data are consistent with `PurposeLoan` and `EmplStatus` being independent.\n\n'
    '**R commands:**\n\n'
    '`chisq_stat <- 11.107; df <- 8; 1 - pchisq(chisq_stat, df)`\n\n'
    '`## [1] 0.1958`\n\n'
    '`# Equivalently from the raw two-way table:`\n\n'
    '`chisq.test(Credit$PurposeLoan, Credit$EmplStatus)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g1_2026_2b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2026_2b_question.png",
    "statistics/images/past_exams/exam_g1_2026_2b_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2026_2b_answer.png",
]}

past_exams["exam_g1_2026_3a"] = {
"title": "G1-2026 Ex3a — Chi² independence test: PurposeLoan vs EmplStatus",
"is_exam": True, "topic_hint": "G14",
"subtopic_hint": "g14d",
"content": (
    '<span class="exam-question-text">We are interested in whether the reason for requesting a loan (`PurposeLoan`) and the employment status (`EmplStatus`) are associated using an appropriate test. Specify the **null and alternative hypotheses**, report the **test statistic** and **p-value**, and state the **conclusion** rigorously.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2026_3a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** **Hypotheses** (Pearson $\\chi^2$ test of independence on the $r\\times c$ contingency table):\n\n'
    '$$H_0:\\; \\text{PurposeLoan} \\perp \\text{EmplStatus} \\quad\\text{vs}\\quad H_1:\\; \\text{the two variables are associated.}$$\n\n'
    'Under $H_0$ the expected cell counts are $E_{ij}=n_{i\\cdot}\\,n_{\\cdot j}/n$ and the test statistic\n\n'
    '$$X^2 \\;=\\; \\sum_{i,j}\\dfrac{(O_{ij}-E_{ij})^2}{E_{ij}} \\;\\overset{H_0}{\\sim}\\; \\chi^2_{(r-1)(c-1)}.$$\n\n'
    '**Test statistic & p-value**: $X^2_{\\text{obs}} = 11.107$ on $df=(r-1)(c-1)=8$, giving\n\n'
    '$$p\\text{-value} \\;=\\; P(\\chi^2_8 > 11.107) \\;\\approx\\; 0.196.$$\n\n'
    'Equivalently in R: `1 - pchisq(11.107, 8)`.\n\n'
    'The left panel below shows the $\\chi^2_8$ density with the **p-value tail** (yellow) past $X^2_{\\text{obs}}$ and the $\\alpha\\in\\{1\\%,5\\%,10\\%\\}$ critical values for context; the right panel shows the **CDF view**, in which the p-value is read off as $1-F_{\\chi^2_8}(11.107)$ and the green band marks the do-not-reject region.\n\n'
    '![AI walkthrough — chi² density tail and CDF view](statistics/images/past_exams/exam_g1_2026_3a_ai.png)\n\n'
    '---\n\n'
    '**Interpretation.** The p-value is the probability of observing a statistic at least as extreme as $11.107$ *under $H_0$* (independence).\n\n'
    '**Answer.** $X^2_{\\text{obs}} = 11.107$ on $df = 8$, $p$-value $= P(\\chi^2_8 > 11.107) \\approx 0.196$. Since $p > 0.10 > 0.05 > 0.01$, we **do not reject $H_0$** at any common level: data are consistent with `PurposeLoan` and `EmplStatus` being **independent**.\n\n'
    '**R commands:**\n\n'
    '`tab <- table(Credit$PurposeLoan, Credit$EmplStatus)`\n\n'
    '`chisq.test(tab)`\n\n'
    '`## X-squared = 11.107, df = 8, p-value = 0.1958`\n\n'
    '`# Manual p-value`\n\n'
    '`1 - pchisq(11.107, df = 8)`\n\n'
    '`## [1] 0.1958`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g1_2026_3a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2026_3a_question.png",
    "statistics/images/past_exams/exam_g1_2026_3a_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2026_3a_answer.png",
]}

past_exams["exam_g1_2026_3b"] = {
"title": "G1-2026 Ex3b — SE for difference in mean RiskIndex across EmplStatus groups",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13c",
"content": (
    '<span class="exam-question-text">The variable `RiskIndex` is a composite indicator summarizing the borrower\'s overall financial risk. We want to compare the average `RiskIndex` for clients employed (`EmplStatus = Empl`) and unemployed (`EmplStatus = Unemp`). Based on the assumption that the standard deviation of the difference between the two means is unknown, provide the **analytical expression** of the estimator of the **standard error of the estimator of the difference between the two means**, and report its **numerical estimate**.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2026_3b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** With two independent samples and **unknown, unequal** variances, the variance of the difference of sample means is the sum of the per-group variances of the means: $\\mathrm{Var}(\\bar X_1-\\bar X_2)=\\sigma_1^2/n_1+\\sigma_2^2/n_2$. Plugging the sample variances $s_1^2,s_2^2$ for $\\sigma_1^2,\\sigma_2^2$ and taking the square root yields the **Welch SE**.\n\n'
    'The left panel shows the two sampling distributions of $\\bar X_1$ (Empl) and $\\bar X_2$ (Unemp) with their $\\pm 1$-SE bands and the difference between the means; the right panel decomposes the estimated variance of the difference into the two contributions $s_1^2/n_1$ and $s_2^2/n_2$ and shows the final SE via the square root.\n\n'
    '![AI walkthrough — Welch SE decomposition](statistics/images/past_exams/exam_g1_2026_3b_ai.png)\n\n'
    '---\n\n'
    '**Hypotheses**:\n\n'
    '$$H_0:\\; \\mu_{\\text{Empl}} = \\mu_{\\text{Unemp}} \\quad\\text{vs}\\quad H_1:\\; \\mu_{\\text{Empl}} \\ne \\mu_{\\text{Unemp}}.$$\n\n'
    '**Analytical estimator** (variances unknown, *not* assumed equal — Welch form):\n\n'
    '$$\\widehat{SE}(\\bar X_1 - \\bar X_2) \\;=\\; \\sqrt{\\dfrac{s_1^2}{n_1} + \\dfrac{s_2^2}{n_2}},$$\n\n'
    'where $s_1^2, s_2^2$ are the sample variances and $n_1, n_2$ the group sizes for the two `EmplStatus` categories.\n\n'
    '**Estimate**: plugging the sample values for `RiskIndex` split by `EmplStatus` gives\n\n'
    '$$\\widehat{SE} \\;=\\; 2.218.$$\n\n'
    '---\n\n'
    '**Answer.** Welch (unequal-variance) standard error of $\\bar X_{\\text{Empl}} - \\bar X_{\\text{Unemp}}$: $\\widehat{SE}(\\bar X_1 - \\bar X_2) = \\sqrt{s_1^2/n_1 + s_2^2/n_2} \\;=\\; \\mathbf{2.218}$ (`RiskIndex` units). This is the estimated standard deviation of the difference-of-means estimator used to build the corresponding two-sample $t$ CI or test.\n\n'
    '**R commands:**\n\n'
    "`x1 <- Credit$RiskIndex[Credit$EmplStatus == 'Empl']`\n\n"
    "`x2 <- Credit$RiskIndex[Credit$EmplStatus == 'Unemp']`\n\n"
    '`n1 <- length(x1); n2 <- length(x2)`\n\n'
    '`SE <- sqrt(var(x1)/n1 + var(x2)/n2); SE`\n\n'
    '`## [1] 2.218`\n\n'
    "`t.test(RiskIndex ~ EmplStatus, data = Credit, var.equal = FALSE)`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_g1_2026_3b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2026_3b_question.png",
    "statistics/images/past_exams/exam_g1_2026_3b_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2026_3b_answer.png",
]}

past_exams["exam_g1_2026_5a"] = {
"title": "G1-2026 Ex4.f (i) — Point estimate + 95% CI for risk index (mod2, EmplStatus=Empl, Age=40, Income=30, DebtIndex=0.3)",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15b",
"content": (
    '<span class="exam-question-text">Using `mod2`, obtain and report a **point estimate** and a **95% confidence interval** for the average risk index of employed clients with `EmplStatus = Empl`, `Age = 40`, `Income = 30`, `DebtIndex = 0.3`.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2026_5a_question.png)\n\n'
    '---\n\n'
    '**Estimated mod2**: $\\widehat{RiskIndex} = 52.19 - 2.12\\cdot I(\\text{Stud}) - 5.42\\cdot I(\\text{Unemp}) - 0.03\\cdot Age - 0.05\\cdot Income + 33.68\\cdot DebtIndex$.\n\n'
    '**Walkthrough.** With `EmplStatus = Empl` both indicator dummies are $0$. The fitted equation evaluates to a single number — that is the **point estimate** $\\hat y_0$. The `predict()` function additionally returns the **95% CI for the mean response** at that covariate profile.\n\n'
    'The figure below highlights the point estimate (vertical line), the **narrow** 95% CI for the mean (navy band) and — for contrast — the **wider** 95% PI for a single new client (yellow band). The reference value $\\text{RiskIndex}=70$ (dashed red) sits **outside the CI** but typically **inside the PI**, reinforcing the CI-vs-PI distinction used in the next subpart.\n\n'
    '![AI walkthrough — point estimate, CI for the mean and PI for a single client](statistics/images/past_exams/exam_g1_2026_5a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Plug $\\text{EmplStatus}=\\text{Empl}$ (both indicators $=0$), $\\text{Age}=40$, $\\text{Income}=30$, $\\text{DebtIndex}=0.3$ into the fitted equation:\n\n'
    '$$\\hat y_0 \\;=\\; 52.19 \\;-\\; 0.03\\cdot 40 \\;-\\; 0.05\\cdot 30 \\;+\\; 33.68\\cdot 0.3 \\;=\\; 59.71.$$\n\n'
    'The `predict(..., interval=\'confidence\', level=0.95)` call returns:\n\n'
    '- **Point estimate**: $\\hat y_0 = 59.71$.\n'
    '- **95% CI for the mean response**: $(58.42,\\; 61.01)$ — covers the *average* risk index across all employed clients with that exact profile.\n\n'
    '**R commands:**\n\n'
    '`newx <- data.frame(EmplStatus=\'Empl\', Age=40, Income=30, DebtIndex=0.3)`\n\n'
    '`predict(mod2, newdata=newx, interval=\'confidence\', level=0.95)`\n\n'
    '`##        fit     lwr     upr`\n\n'
    '`## 1   59.71   58.42   61.01`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g1_2026_5a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2026_5a_question.png",
    "statistics/images/past_exams/exam_g1_2026_5a_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2026_5a_answer.png",
]}

past_exams["exam_g1_2026_5b"] = {
"title": "G1-2026 Ex4.f (ii) — Is a risk index of 70 unexpected/anomalous? (CI vs PI)",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15b",
"content": (
    '<span class="exam-question-text">Explain, motivating your answer, whether — based on the estimated `mod2` — a **risk index of 70** for a client with `EmplStatus = Empl`, `Age = 40`, `Income = 30`, `DebtIndex = 0.3` should be considered **unexpected or anomalous**.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2026_5b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** The CI for the mean response describes uncertainty about the *average* $E[y\\mid x_0]$ across all clients with profile $x_0$. A **single new client** also carries the **irreducible residual error** $\\varepsilon\\sim(0,\\hat\\sigma^2)$, so the proper reference is the **prediction interval** (PI), whose SE adds that residual variance:\n\n'
    '$$SE(\\hat y_{\\text{new}})^2 \\;=\\; SE(\\hat y_{\\text{mean}})^2 \\;+\\; \\hat\\sigma^2.$$\n\n'
    'The left panel overlays the two predictive Normals around $\\hat y_0=59.71$: the narrow yellow CI for the mean vs. the wide navy PI for one new client; the dashed red line marks $y=70$. The right panel decomposes the variance into $SE_{\\text{mean}}^2 + \\hat\\sigma^2 = SE_{\\text{pred}}^2$ — making it visually obvious why the PI is much wider than the CI.\n\n'
    '![AI walkthrough — CI vs PI for a single new client](statistics/images/past_exams/exam_g1_2026_5b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** From the source output (at $\\text{EmplStatus}=\\text{Empl}$, $\\text{Age}=40$, $\\text{Income}=30$, $\\text{DebtIndex}=0.3$):\n\n'
    '- **95% CI for the mean response**: $(58.42,\\; 61.01)$ — about the *average* risk index of such clients.\n'
    '- **95% PI for a single new client**: substantially **wider** than the CI and **contains $70$**.\n\n'
    '**Conclusion.** A risk index of $70$ lies **outside the 95% CI** (so it is far from the predicted *average* for that profile) but **inside the 95% PI** for a single new client. Given the residual variability of `mod2`, the value $70$ is therefore **not unexpected/anomalous** for an *individual* client with that profile — it would be implausible only as a *mean* across such clients.\n\n'
    '**R commands:**\n\n'
    "`newx <- data.frame(EmplStatus='Empl', Age=40, Income=30, DebtIndex=0.3)`\n\n"
    "`predict(mod2, newdata=newx, interval='confidence', level=0.95)`\n\n"
    '`##        fit     lwr     upr`\n\n'
    '`## 1   59.71   58.42   61.01`\n\n'
    "`predict(mod2, newdata=newx, interval='prediction', level=0.95)`\n\n"
    '`# wider than the CI — contains 70 => a single-client risk index of 70 is NOT anomalous`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g1_2026_5b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2026_5b_question.png",
    "statistics/images/past_exams/exam_g1_2026_5b_ai.png",
    "statistics/images/past_exams/answers/exam_g1_2026_5b_answer.png",
]}

# ---- general 2 2024: 5b ----
past_exams["exam_g2_2024_5b"] = {
"title": "G2-2024 Ex5b — 99% CI for proportion of cities with CrimePeople > 250",
"is_exam": True, "topic_hint": "G13",
"subtopic_hint": "g13b",
"content": _q(
    "<span class=\"exam-question-text\">Build the 99% CI for the proportion of U.S. cities with CrimePeople > 250. $\\hat p = 0.21$, $n = 485$.</span>\n\n![Original question](statistics/images/past_exams/questions/exam_g2_2024_5b_question.png)",
    "Normal-approximation CI: $\\hat p \\pm z_{0.995}\\cdot \\sqrt{\\hat p(1-\\hat p)/n} = 0.21 \\pm 2.576\\cdot \\sqrt{0.21\\cdot 0.79/485} = 0.21 \\pm 2.576\\cdot 0.0185 = 0.21 \\pm 0.0477 \\approx [0.16, 0.26]$, exactly matching the R output. **Conclusion:** with $99\\%$ confidence, the proportion of U.S. cities with `CrimePeople > 250` lies in $[0.16,\\,0.26]$.\n\n![AI illustration](statistics/images/past_exams/exam_g2_2024_5b_ai.png)\n\n![Original answer](statistics/images/past_exams/answers/exam_g2_2024_5b_answer.png)",
    "vec.binA <- CrimeUS$CrimePeople > 250\nCI.prop(vec.binA, conf.level=0.99)\n# manual:\np_hat <- 0.21; n <- 485\np_hat + c(-1,1)*qnorm(0.995)*sqrt(p_hat*(1-p_hat)/n)\n## [1] 0.1623 0.2577",
    w="One-sample **Normal-approximation CI for a proportion**: $\\hat p \\pm z_{1-\\alpha/2}\\sqrt{\\hat p(1-\\hat p)/n}$ with $\\alpha = 0.01$, so $z_{0.995} = 2.576$. Valid under the usual large-sample condition ($n\\hat p \\ge 5$ and $n(1-\\hat p) \\ge 5$): here $n\\hat p \\approx 102$ and $n(1-\\hat p) \\approx 383$, both well above the threshold."
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2024_5b_question.png",
    "statistics/images/past_exams/exam_g2_2024_5b_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2024_5b_answer.png",
]}

# ---- general 2 2026: 1b, 1c, 2b, 2c, 4.4, 4.5, 4.6 ----
past_exams["exam_g2_2026_1b"] = {
"title": "G2-2026 Ex1b — Analytic SE for difference in proportions (with numerics)",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13d",
"content": (
    '<span class="exam-question-text">Report the **analytical expression** of the estimated standard error of the estimator for the difference between the two considered proportions, providing the numerical values of the involved quantities.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2026_1b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Under independence the variances of the two sample proportions add:\n\n'
    '$$\\widehat{SE}(\\hat p_1 - \\hat p_2) = \\sqrt{\\dfrac{\\hat p_1(1-\\hat p_1)}{n_1} + \\dfrac{\\hat p_2(1-\\hat p_2)}{n_2}}.$$\n\n'
    'Plugging in $n_1 = 278$, $\\hat p_1 = 0.64$, $n_2 = 189$, $\\hat p_2 = 0.418$:\n\n'
    '$$\\widehat{SE} = \\sqrt{\\dfrac{0.64\\cdot 0.36}{278} + \\dfrac{0.418\\cdot 0.582}{189}} = \\sqrt{0.000829 + 0.001287} = \\sqrt{0.002116} \\approx 0.0460.$$\n\n'
    'This is the SE used in 1a\'s 90% CI (point estimate $\\hat p_1-\\hat p_2 = 0.222$, half-width $1.645\\cdot 0.0460 \\approx 0.0757$, CI $= [0.147,\\,0.298]$). The left panel below stacks the two binomial variance contributions $\\hat p_i(1-\\hat p_i)/n_i$ and shows their sum; the right panel plots $SE(\\hat p)$ as a function of $\\hat p$ at each sample size, with the two operating points marked.\n\n'
    '![AI walkthrough — analytic SE decomposition for difference of two proportions](statistics/images/past_exams/exam_g2_2026_1b_ai.png)\n\n'
    '---\n\n'
    '**Note.** Use the *unpooled* SE (with $\\hat p_1,\\hat p_2$ separately) for the **CI**. The *pooled* SE shown below is used **only** for the two-sample test $H_0:p_1=p_2$ (G14), since under $H_0$ both populations share a common $p$ that is best estimated by pooling. Do not mix the two formulas.\n\n'
    '---\n\n'
    '**Answer.** $\\widehat{SE}(\\hat p_1 - \\hat p_2) = \\sqrt{\\dfrac{\\hat p_1(1-\\hat p_1)}{n_1} + \\dfrac{\\hat p_2(1-\\hat p_2)}{n_2}} = \\sqrt{\\dfrac{0.64\\cdot 0.36}{278} + \\dfrac{0.418\\cdot 0.582}{189}} = \\sqrt{0.000829 + 0.001287} = \\sqrt{0.002116} \\;\\approx\\; \\mathbf{0.0460}$. This is the SE feeding the 90% CI of 1a (half-width $1.645\\cdot 0.0460 \\approx 0.0757$, point estimate $\\hat p_1 - \\hat p_2 = 0.222$, CI $[0.147,\\,0.298]$).\n\n'
    '**R commands:**\n\n'
    '`n1 <- 278; p1 <- 0.64`\n\n'
    '`n2 <- 189; p2 <- 0.418`\n\n'
    '`SE_diff <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)`\n\n'
    '`SE_diff`\n\n'
    '`## [1] 0.04600`\n\n'
    '`# Pooled SE (ONLY for the H0: p1=p2 test, NOT for the CI)`\n\n'
    '`p_pool <- (n1*p1 + n2*p2)/(n1+n2)`\n\n'
    '`se_0   <- sqrt(p_pool*(1-p_pool)*(1/n1 + 1/n2))`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g2_2026_1b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2026_1b_question.png",
    "statistics/images/past_exams/exam_g2_2026_1b_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2026_1b_answer.png",
]}

past_exams["exam_g2_2026_1c"] = {
"title": "G2-2026 Ex1c — Interpretation of estimated SE in the 90% CI for $p_1-p_2$",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13d",
"content": (
    '<span class="exam-question-text">Report the **interpretation of the estimated standard error** of the estimator for the difference between the two considered proportions ($\\hat p_1 - \\hat p_2$, NorthWest vs NorthEast — cleaning category, more expensive product), providing the **numerical values** of the involved quantities.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2026_1c_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Under independence of the two samples, the variance of the difference is the **sum** of the per-sample variances:\n\n'
    '$$\\widehat{Var}(\\hat p_1-\\hat p_2) = \\frac{\\hat p_1(1-\\hat p_1)}{n_1} + \\frac{\\hat p_2(1-\\hat p_2)}{n_2}$$\n\n'
    'Plugging in $\\hat p_1=0.64,\\,n_1=278$ and $\\hat p_2=0.418,\\,n_2=189$ gives $\\widehat{Var}\\approx 0.000829 + 0.001287 = 0.002116$, hence $\\widehat{SE}\\approx 0.0460$. With point estimate $\\hat p_1-\\hat p_2 = 0.222$ and $z_{0.95} = 1.645$:\n\n'
    '$$0.222 \\;\\pm\\; 1.645 \\cdot 0.0460 \\;=\\; [0.147,\\; 0.298].$$\n\n'
    '**Interpretation of the SE.** $\\widehat{SE}(\\hat p_1-\\hat p_2) \\approx 0.0460$ is the **typical sampling variability** of the estimator $\\hat p_1-\\hat p_2$ around the unknown true difference $p_1-p_2$ — i.e. the standard deviation of its sampling distribution. Multiplied by $z_{0.95}=1.645$ it produces the half-width of the 90% CI, i.e. the maximum plausible distance between the point estimate and the true difference at the 90% confidence level.\n\n'
    'The left panel below decomposes the SE into the two variance contributions; the right panel draws the sampling distribution of $\\hat p_1-\\hat p_2$ centred at $0.222$, with the central 90% mass shaded and the value $0$ marked outside the interval.\n\n'
    '![AI walkthrough — SE interpretation + 90% CI for difference of two proportions](statistics/images/past_exams/exam_g2_2026_1c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** With 90% confidence the true difference $p_1-p_2$ between the proportions of customers choosing the **more expensive product in the cleaning category** in NorthWest vs NorthEast lies in $[0.147,\\,0.298]$. The interval is **strictly positive** ⇒ NorthWest has a significantly higher share than NorthEast at the 90% level, by between 14.7 and 29.8 percentage points.\n\n'
    '**R commands:**\n\n'
    '`n1 <- 278; p1 <- 0.64`\n\n'
    '`n2 <- 189; p2 <- 0.418`\n\n'
    '`SE   <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)   # ~ 0.0460`\n\n'
    '`diff <- p1 - p2                              # 0.222`\n\n'
    '`diff + c(-1,1) * qnorm(0.95) * SE`\n\n'
    '`## [1] 0.1463 0.2977`\n\n'
    '`CI.diffprop(x, y, conf.level=0.90)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g2_2026_1c_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2026_1c_question.png",
    "statistics/images/past_exams/exam_g2_2026_1c_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2026_1c_answer.png",
]}

past_exams["exam_g2_2026_2b"] = {
"title": "G2-2026 Ex2b — Rejection region & p-value (one-sample z-test, σ known)",
"is_exam": True, "topic_hint": "G12",
"content": (
    '<span class="exam-question-text">Given the test on campaign effectiveness $H_0:\\mu_{\\text{PRICE-Y}}=850$ vs $H_1:\\mu_{\\text{PRICE-Y}}>850$ with $\\sigma=300$ known, derive the **rejection region** and the **p-value**, clearly explaining your reasoning.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2026_2b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Since $\\sigma$ is **known**, under $H_0$ the test statistic is\n\n'
    '$$Z \\;=\\; \\dfrac{\\bar X - 850}{\\sigma/\\sqrt{n}} \\;=\\; \\dfrac{\\bar X - 850}{300/\\sqrt{n}} \\;\\overset{H_0}{\\sim}\\; \\mathcal{N}(0,1).$$\n\n'
    '**Rejection region** (one-sided, upper tail at level $\\alpha$): reject $H_0$ iff\n\n'
    '$$Z_{\\text{obs}} > z_{1-\\alpha} \\quad\\Longleftrightarrow\\quad \\bar X > 850 + z_{1-\\alpha}\\cdot\\dfrac{300}{\\sqrt{n}}.$$\n\n'
    'At $\\alpha=0.05$: $z_{0.95}=1.645$; at $\\alpha=0.01$: $z_{0.99}=2.326$.\n\n'
    '**p-value** (right-tail): $\\text{p-value}=\\Pr(Z>Z_{\\text{obs}}\\mid H_0)=1-\\Phi(Z_{\\text{obs}})$.\n\n'
    '**Worked example** ($n=50$, $\\bar x=920$): $Z_{\\text{obs}}=\\dfrac{920-850}{300/\\sqrt{50}}=\\dfrac{70}{42.43}\\approx 1.650$, so $\\text{p-value}=1-\\Phi(1.650)\\approx 0.0495$. The left panel shows the standard-normal rejection region (yellow, $z>1.645$) and the p-value (navy shading beyond $z_{\\text{obs}}$); the right panel translates the same picture to the original $\\bar X$ scale, where the rejection threshold sits at $\\bar x_c=850+1.645\\cdot 42.43\\approx 919.78$ and the observed $\\bar x=920$ falls *just* inside the rejection region.\n\n'
    '![AI walkthrough — rejection region & p-value, standardized and original scale](statistics/images/past_exams/exam_g2_2026_2b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** **Rejection region**: $\\{Z_{\\text{obs}}>z_{1-\\alpha}\\}$, equivalently $\\{\\bar X>850+z_{1-\\alpha}\\,\\sigma/\\sqrt{n}\\}$. **p-value**: $1-\\Phi(Z_{\\text{obs}})$. For $n=50,\\ \\bar x=920$: $Z_{\\text{obs}}\\approx 1.650$, p-value $\\approx 0.0495$ → **reject $H_0$** at $\\alpha=5\\%$ (borderline; **do not reject** at $\\alpha=1\\%$).\n\n'
    '**R commands:**\n\n'
    '`n <- 50; xbar <- 920; mu0 <- 850; sigma <- 300; alpha <- 0.05`\n\n'
    '`z <- (xbar - mu0)/(sigma/sqrt(n)); z`\n\n'
    '`## [1] 1.6499`\n\n'
    '`qnorm(1 - alpha)   # critical z for one-sided upper-tail test`\n\n'
    '`## [1] 1.6449`\n\n'
    '`1 - pnorm(z)       # p-value`\n\n'
    '`## [1] 0.04948`\n\n'
    '`(1 - pnorm(z)) < alpha   # reject H0?`\n\n'
    '`## [1] TRUE`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g2_2026_2b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2026_2b_question.png",
    "statistics/images/past_exams/exam_g2_2026_2b_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2026_2b_answer.png",
]}

past_exams["exam_g2_2026_2c"] = {
"title": "G2-2026 Ex2c — Conclusion: is the campaign effective?",
"is_exam": True, "topic_hint": "G12",
"content": (
    '<span class="exam-question-text">Based on the test in 2.b, state the **conclusion**: is the marketing department\'s claim (the campaign raised the average price paid above €850) supported by the data? Interpret in plain words.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2026_2c_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** From 2.b: $Z_{\\text{obs}}=\\dfrac{920-850}{300/\\sqrt{50}}\\approx 1.6499$ and p-value $=1-\\Phi(1.6499)\\approx 0.04948$. The decision rule is *reject $H_0:\\mu=850$ in favour of $H_1:\\mu>850$ iff p-value $<\\alpha$*.\n\n'
    '- **At $\\alpha=0.05$**: p-value $\\approx 0.0495 < 0.05$ $\\Rightarrow$ **reject $H_0$**. Statistically significant evidence (at the 5% level) that the post-campaign mean price $\\mu_{\\text{PRICE-Y}}$ exceeds €850. The marketing claim is **supported** and the campaign appears **effective**.\n'
    '- **At $\\alpha=0.01$**: p-value $\\approx 0.0495 > 0.01$ $\\Rightarrow$ **do not reject $H_0$**. At the more conservative 1% level the evidence is **insufficient** to declare the campaign effective.\n\n'
    '**One-sided 95% lower bound on $\\mu$.** A complementary practical-magnitude check is $\\bar x - z_{0.95}\\,\\sigma/\\sqrt n = 920 - 1.6449\\cdot 300/\\sqrt{50}\\approx 850.21 > 850$, which confirms the rejection at 5% — but only by a hair (margin $\\approx 0.21$), echoing the borderline p-value.\n\n'
    '**Caveats.** (i) The conclusion hinges on the assumed known $\\sigma=300$ — a moderate increase in $\\sigma$ pushes the test below the 5% threshold. (ii) The result is borderline at 5%: a replication or a larger sample would be much more persuasive. (iii) Statistical significance ≠ practical significance: the lower bound on $\\mu$ shows the post-campaign mean exceeds €850 only marginally.\n\n'
    '![AI walkthrough — verdict at α=0.05 vs α=0.01 and 95% lower bound on μ](statistics/images/past_exams/exam_g2_2026_2c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** **Yes** at $\\alpha=5\\%$ (borderline): reject $H_0$, the post-campaign mean price is significantly above €850, so the marketing claim is supported and the campaign appears effective. **No** at $\\alpha=1\\%$: do not reject $H_0$, the evidence is insufficient at a stricter level. The conclusion therefore depends critically on the chosen significance level and on the assumed $\\sigma=300$.\n\n'
    '**R commands:**\n\n'
    '`n <- 50; xbar <- 920; mu0 <- 850; sigma <- 300`\n\n'
    '`z <- (xbar - mu0)/(sigma/sqrt(n))`\n\n'
    '`pval <- 1 - pnorm(z); pval`\n\n'
    '`## [1] 0.04948`\n\n'
    '`# Decision at alpha = 5%`\n\n'
    '`if (pval < 0.05) "Reject H0: campaign effective" else "Do not reject H0"`\n\n'
    '`## [1] "Reject H0: campaign effective"`\n\n'
    '`# Decision at alpha = 1%`\n\n'
    '`if (pval < 0.01) "Reject H0: campaign effective" else "Do not reject H0"`\n\n'
    '`## [1] "Do not reject H0"`\n\n'
    '`# One-sided 95% lower bound on mu`\n\n'
    '`xbar - qnorm(0.95) * sigma/sqrt(n)`\n\n'
    '`## [1] 850.21   # > 850 confirms rejection at 5% (margin only ~0.2)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g2_2026_2c_answer.png)'
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2026_2c_question.png",
    "statistics/images/past_exams/exam_g2_2026_2c_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2026_2c_answer.png",
]}

past_exams["exam_g2_2026_4_4"] = {
"title": "G2-2026 Ex4.4 — Formal homoscedasticity assumption + diagnostic plots for mod1",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15e",
"content": _q(
    "<span class=\"exam-question-text\">Formally state the homoscedasticity assumption underlying the linear regression model and discuss whether there is empirical evidence of its violation for the considered model `mod1`, clearly specifying the diagnostic tool(s) you use.</span>\n\n![Question](statistics/images/past_exams/questions/exam_g2_2026_4_4_question.png)",
    "**Assumption**: the error variance is **constant** (no heteroscedasticity), i.e. $\\mathrm{Var}(\\varepsilon_i\\mid\\mathbf{x}_i) = \\sigma^2$ for every $i = 1,\\ldots,n$ — it does **not** depend on the values of the explanatory variables in the model.\n\n**Evidence of violation — diagnostic tools**: (1) **residuals vs fitted values** plot — `plot(mod1, which=1)`; flag funnel/cone shapes. (2) **Scale-location** plot of $\\sqrt{|\\text{standardized residuals}|}$ against fitted values — `plot(mod1, which=3)`; flag an upward/downward trend in the post-smoothing red line. If both plots show no clear pattern (flat scatter, flat red line) → the homoscedasticity assumption appears satisfied for `mod1`; otherwise it is violated.\n\n![Answer](statistics/images/past_exams/answers/exam_g2_2026_4_4_answer.png)\n\n![AI walkthrough](statistics/images/past_exams/exam_g2_2026_4_4_ai.png)",
    "plot(mod1, which=1)\nplot(mod1, which=3)\nlibrary(lmtest); bptest(mod1)",
    w="**Homoscedasticity** is one of the four Gauss–Markov assumptions for OLS in the linear model $Y_i = \\mathbf{x}_i^\\top \\boldsymbol\\beta + \\varepsilon_i$: it requires that the conditional variance of the error term is the **same constant $\\sigma^2$** for every observation. To diagnose whether this holds for a fitted model `mod1`, inspect the **residuals vs fitted** scatter (look for a funnel/megaphone) and the **scale–location** plot (look for a slope in the smoothing line of $\\sqrt{|r_i^{\\text{std}}|}$ vs $\\hat y_i$); formally confirm with a Breusch–Pagan test (`bptest`)."
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2026_4_4_question.png",
    "statistics/images/past_exams/answers/exam_g2_2026_4_4_answer.png",
    "statistics/images/past_exams/exam_g2_2026_4_4_ai.png",
]}

past_exams["exam_g2_2026_4_5"] = {
"title": "G2-2026 Ex4.5 — Point prediction + 95% PI for Amount at exp_pre=250",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15b",
"content": _q(
    "<span class=\"exam-question-text\">Obtain a point prediction and a 95% prediction interval for the post-promotional expenditure of a southern customer (`region = South`) with `age = 50`, `paid_amount = 1200` and `exp_pre = 250`.</span>\n\n![Question](statistics/images/past_exams/questions/exam_g2_2026_4_5_question.png)",
    "Use `predict()` on `mod1` with the new observation and `interval='prediction', level=0.95`.\n\n**Point prediction**: $\\hat y = 6523.5731$.\n\n**95% Prediction interval**: $[6387.6292,\\ 6659.5209]$.\n\nThe prediction interval is **wider** than the confidence interval for the mean response because it accounts for **both** the uncertainty in the estimated mean and the irreducible error variance $\\hat\\sigma^2$: $SE(\\hat y_{\\text{new}})^2 = SE(\\hat y_{\\text{mean}})^2 + \\hat\\sigma^2$ — *this is exactly the **\"+1\"** inside the PI sqrt from row 7 of the universal regression table at the top of master entry `g15a`* (and the structural CI-vs-PI comparison in `g15b`).\n\n![Answer](statistics/images/past_exams/answers/exam_g2_2026_4_5_answer.png)\n\n![AI walkthrough](statistics/images/past_exams/exam_g2_2026_4_5_ai.png)",
    "predict(mod1, newdata=data.frame(exp_pre=250, amount=1200), interval='prediction', level=0.95)\n##        fit      lwr      upr\n## 1 6523.5731 6387.6292 6659.5209",
    w="Use the **prediction interval for a single new observation** from a fitted OLS model: evaluate the regression equation $\\hat y_0 = \\mathbf{x}_0^\\top\\hat{\\boldsymbol\\beta}$ at the new covariate profile $\\mathbf{x}_0$, then build $\\hat y_0 \\pm t_{n-p-1,\\,1-\\alpha/2}\\,\\hat\\sigma\\sqrt{1 + \\mathbf{x}_0^\\top(\\mathbf{X}^\\top\\mathbf{X})^{-1}\\mathbf{x}_0}$. The extra **+1** under the square root (vs the CI for the mean) accounts for the irreducible error variance, making the PI strictly wider than the CI. In R use `predict(mod1, newdata=..., interval='prediction', level=0.95)`."
), "images": ["statistics/images/past_exams/questions/exam_g2_2026_4_5_question.png", "statistics/images/past_exams/answers/exam_g2_2026_4_5_answer.png", "statistics/images/past_exams/exam_g2_2026_4_5_ai.png"]}

past_exams["exam_g2_2026_4_6"] = {
"title": "G2-2026 Ex4.6 — Is loyalty a significant predictor? (Adj R² + p-value)",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15c",
"content": (
    '<span class="exam-question-text">Would you suggest to include in the model also the client\'s `loyalty` (score ranging between 0 and 100)? Motivate rigorously your answer.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2026_4_6_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Fit the augmented model `mod2 = mod1 + loyalty` and judge inclusion on **two complementary criteria**:\n\n'
    '1. **Goodness-of-fit (penalised)** — the **adjusted $R^2$** rewards explanatory power but penalises extra parameters. Adding a useless predictor *lowers* $\\bar R^2$; adding a useful one raises it. Here $\\bar R^2$ moves from $0.4132$ (mod1) to $0.4151$ (mod2), a small but **positive** gain ($\\Delta\\bar R^2 = +0.0019$). The left panel below visualises the bar comparison and the gain.\n\n'
    '2. **Individual significance of $\\beta_{\\text{loyalty}}$** — `summary(mod2)` reports the t-test of $H_0\\!:\\beta_{\\text{loyalty}}=0$ against the two-sided alternative. The observed p-value is $0.0441$. Since $0.0441<0.05$, we **reject $H_0$ at $\\alpha=5\\%$** (the test is marginal: we would *not* reject at $\\alpha=1\\%$). The right panel shows the $t$-distribution under $H_0$ with the 5% rejection region (yellow) and the two-tail p-value mass (navy) around the observed $|t_{\\text{obs}}|\\approx 2.03$.\n\n'
    'Both signals point in the *same direction*: loyalty adds modest but statistically meaningful explanatory power, so it is worth keeping in the model.\n\n'
    '![AI walkthrough — Adj R² gain + significance t-test for loyalty](statistics/images/past_exams/exam_g2_2026_4_6_ai.png)\n\n'
    '---\n\n'
    '**Answer.** **Yes — include `loyalty`.** The adjusted $R^2$ rises from $0.4132$ to $0.4151$ (positive gain despite the penalty for the extra parameter) and the significance test for $\\beta_{\\text{loyalty}}$ returns p-value $=0.0441<0.05$, so loyalty is a **statistically significant** predictor at the conventional 5% level (marginal — not significant at 1%). The gain in fit is mild, but both criteria align: recommend the augmented model `mod2`.\n\n'
    '**R commands:**\n\n'
    '`mod2 <- lm(amount ~ exp_pre + age + region + paid_amount + loyalty, data=DF)`\n\n'
    '`summary(mod2)`\n\n'
    '`## Adjusted R-squared: 0.4151    (vs 0.4132 for mod1)`\n\n'
    '`## loyalty   Estimate ...   t value ...   Pr(>|t|): 0.0441 *`\n\n'
    '`# Formal nested-model F test (equivalent here to the single t-test)`\n\n'
    '`anova(mod1, mod2)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_g2_2026_4_6_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2026_4_6_question.png",
    "statistics/images/past_exams/exam_g2_2026_4_6_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2026_4_6_answer.png",
]}

# ---- september 2024: 1b, 2b, 3b, 3c ----
past_exams["exam_sep_2024_1b"] = {
"title": "Sep-2024 Ex1b — 90% CI for proportion of Eligible='Y' customers",
"is_exam": True, "topic_hint": "G13",
"subtopic_hint": "g13b",
"content": _q(
    '<span class="exam-question-text">Estimate the proportion of customers who were granted a credit card in the population (`Eligible=\'Y\'`) and provide a **90% confidence interval** for $p$.</span>\n\n'
    "![Question](statistics/images/past_exams/questions/exam_sep_2024_1b_question.png)",
    "**Point estimate**: $\\hat p = \\#\\{\\text{Eligible}='Y'\\}/n \\approx 0.67$ with $n = 8000$. **Normal-approx validity check**: $n\\hat p(1-\\hat p) = 8000\\cdot 0.67\\cdot 0.33 \\approx 1768 \\gg 5$, so the Wald/normal approximation is valid. **90% CI**: $\\hat p \\pm z_{0.95}\\cdot\\sqrt{\\hat p(1-\\hat p)/n} = 0.67 \\pm 1.645\\cdot\\sqrt{0.67\\cdot 0.33/8000} \\approx [0.6613,\\,0.6787]$. **Interpretation**: with 90% confidence the population proportion of eligible customers lies in $[0.66, 0.68]$.\n\n"
    "![AI walkthrough](statistics/images/past_exams/exam_sep_2024_1b_ai.png)\n\n"
    "![Answer](statistics/images/past_exams/answers/exam_sep_2024_1b_answer.png)",
    "CI.prop(Eligible=='Y', conf.level=0.90, data=Credit)\n## Confidence interval for the proportion of cases were Eligible == 'Y'\n## Confidence level: 0.9\n##  n   phat  lower  upper\n## 8000 0.67  0.6613 0.6787",
    w="One-sample **Wald/Normal CI for a population proportion** $p = P(\\text{Eligible}='Y')$: $\\hat p \\pm z_{1-\\alpha/2}\\sqrt{\\hat p(1-\\hat p)/n}$ with $\\alpha = 0.10$ and $z_{0.95} = 1.645$. The large-sample condition $n\\hat p(1-\\hat p) \\gg 5$ guarantees the Normal approximation to the sampling distribution of $\\hat p$ via the CLT."
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2024_1b_question.png",
    "statistics/images/past_exams/exam_sep_2024_1b_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2024_1b_answer.png",
]}

past_exams["exam_sep_2024_2b"] = {
"title": "Sep-2024 Ex2b — Maximum score of the bottom 20% by branch",
"is_exam": True, "topic_hint": "G6",
"content": (
    '<span class="exam-question-text">Calculate the maximum `Score` value for the 20% of customers in the specific branch with the lowest credit score. Compare with the analogous value for the main branches and state which comparison is more reliable and why.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2024_2b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** The "maximum score of the bottom 20%" inside a group is exactly the **20th percentile** of that group:\n\n'
    '$$p_{20}(\\text{Score}\\mid\\text{Branch}=b) \\;=\\; \\inf\\{x:\\ F_b(x)\\ge 0.20\\}.$$\n\n'
    'In R, `distr.summary.x(Score, by=Branch, stats="p20", data=Credit)` returns one value per branch level. To answer **which comparison is more reliable**, note that the percentile estimator has sampling variance roughly $\\operatorname{Var}(\\hat p_{20}) \\approx \\dfrac{0.20\\cdot 0.80}{n\\,[f(p_{20})]^2}$: the smaller the branch sample size $n$, the **wider** the sampling distribution of $\\hat p_{20}$. The specific branch has a much smaller $n$ than each main branch, so any **specific vs main** comparison must be flagged as **approximate**; a **main vs main** comparison rests on much larger samples and is therefore more reliable.\n\n'
    '![AI walkthrough — boxplots with p20 overlaid + bootstrap SE bars showing wider sampling spread for the small-n specific branch](statistics/images/past_exams/exam_sep_2024_2b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Read $\\hat p_{20}$ from `distr.summary.x(...)` for each level of `Branch`. The maximum score of the lowest-scoring 20% in the **specific** branch is its $\\hat p_{20}$; compare it with the $\\hat p_{20}$ of each main branch. **Reliability:** comparisons **between main branches** are more reliable because their sample sizes are large; the **specific-vs-main** comparison is only an **approximation** since the specific branch has a much smaller sub-sample, so its $\\hat p_{20}$ has larger sampling variability.\n\n'
    '**R commands:**\n\n'
    '`distr.summary.x(Score, by=Branch, stats="p20", data=Credit)`\n\n'
    '`## returns p20 for each level of Branch`\n\n'
    '`# equivalent base-R:`\n\n'
    '`tapply(Credit$Score, Credit$Branch, quantile, probs = 0.20, na.rm = TRUE)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_sep_2024_2b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2024_2b_question.png",
    "statistics/images/past_exams/exam_sep_2024_2b_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2024_2b_answer.png",
]}

past_exams["exam_sep_2024_3b"] = {
"title": "Sep-2024 Ex3b — 95% CI for Account_length slope + interpretation",
"is_exam": True, "topic_hint": "G15", "subtopic_hint": "g15a",
"content": (
    '<span class="exam-question-text">Provide a **95% confidence interval** for the `Account_length` slope coefficient and **interpret** it.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2024_3b_question.png)\n\n'
    '---\n\n'
    "**Walkthrough.** From `summary(m)` the point estimate is $\\hat\\beta_{\\text{Account\\_length}}=7.84$ and its standard error is read off the *Std. Error* column. Under the OLS normal-error model, $\\hat\\beta\\sim N(\\beta,\\widehat{SE}^2)$, so the **Wald 95% CI** is\n\n"
    "$$\\hat\\beta\\;\\pm\\;t_{0.975,\\,n-p}\\cdot\\widehat{SE}(\\hat\\beta)\\;\\approx\\;7.84\\;\\pm\\;1.96\\cdot\\widehat{SE}.$$\n\n"
    "The figure below shows this sampling distribution: the yellow band is the 95% CI, the red vertical line marks $H_0:\\beta=0$. Because $0$ sits well **outside** the band, the slope is significantly different from 0 at $\\alpha=0.05$ — same conclusion as the $t$-test in `summary(m)`.\n\n"
    '![AI walkthrough — sampling distribution N(7.84, SE^2) with 95% CI shaded and 0 excluded; horizontal CI bar showing reject H0](statistics/images/past_exams/exam_sep_2024_3b_ai.png)\n\n'
    '---\n\n'
    "**Answer.** Point estimate $\\hat\\beta_{\\text{Account\\_length}}=7.84$. With $\\widehat{SE}$ from the *Std. Error* column of `summary(m)`:\n\n"
    "$$\\text{95% CI}=\\hat\\beta\\;\\pm\\;t_{0.975,\\,n-p}\\cdot\\widehat{SE}\\;\\approx\\;7.84\\;\\pm\\;1.96\\cdot\\widehat{SE}.$$\n\n"
    "Read the exact bounds with `confint(m, 'Account_length', level=0.95)`.\n\n"
    "**Interpretation.** With 95% confidence, each additional year of customer relationship is associated with a `Score` increase that lies inside this interval, **holding the other regressors fixed**. Because $\\hat\\beta=7.84$ is far from 0 (and significant at $\\alpha=0.05$ per `summary(m)`), the CI **excludes 0** ⇒ the slope is significantly different from 0.\n\n"
    "**R commands:**\n\n"
    "`summary(m)`\n\n"
    "`confint(m, level=0.95)`\n\n"
    "`confint(m, 'Account_length', level=0.95)`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_sep_2024_3b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2024_3b_question.png",
    "statistics/images/past_exams/exam_sep_2024_3b_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2024_3b_answer.png",
]}

past_exams["exam_sep_2024_3c"] = {
"title": "Sep-2024 Ex3c — 90% CI for difference in credit-approval proportions across two banks",
"is_exam": True, "topic_hint": "G13", "subtopic_hint": "g13d",
"content": (
    '<span class="exam-question-text">In a sample of 200 customers of another bank, exactly 156 customers were granted a credit. Provide the **90% confidence interval for the difference** between the proportions of credit-card customers at the two banks. Include details of your calculations.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2024_3c_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Two independent samples → use the two-proportion **Wald** CI. From the Credit data of bank 1 (`Eligible=\'Y\'`, $n_1=8000$): $\\hat p_1 \\approx 0.67$. Bank 2: $\\hat p_2 = 156/200 = 0.78$ on $n_2 = 200$. Point estimate of the difference: $\\hat p_1 - \\hat p_2 = -0.11$.\n\n'
    'Independent samples → variances add, so\n\n'
    '$$\\widehat{SE}(\\hat p_1-\\hat p_2) = \\sqrt{\\dfrac{\\hat p_1(1-\\hat p_1)}{n_1} + \\dfrac{\\hat p_2(1-\\hat p_2)}{n_2}} = \\sqrt{\\dfrac{0.67\\cdot 0.33}{8000} + \\dfrac{0.78\\cdot 0.22}{200}} \\approx 0.0298.$$\n\n'
    'With $z_{0.95} = 1.645$ the 90% CI is $\\hat p_1-\\hat p_2 \\pm z_{0.95}\\widehat{SE} = -0.11 \\pm 1.645\\cdot 0.0298 \\approx [-0.1590,\\ -0.0610]$. The left plot shows the two per-bank proportions with their 90% CI whiskers; the right plot shows the normal sampling distribution of $\\hat p_1-\\hat p_2$ with the 90% CI shaded — note that **0 lies outside the CI** ⇒ at 90% confidence the two approval rates differ.\n\n'
    '![AI walkthrough — bank-by-bank proportions with 90% bars (left) and normal sampling distribution of $\\hat p_1-\\hat p_2$ with 90% CI shaded (right). 0 lies outside the CI so $p_2 > p_1$.](statistics/images/past_exams/exam_sep_2024_3c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** With $\\hat p_1=0.67$, $n_1=8000$, $\\hat p_2=0.78$, $n_2=200$:\n\n'
    '$\\widehat{SE} = \\sqrt{0.67\\cdot 0.33/8000 + 0.78\\cdot 0.22/200} \\approx 0.0298$.\n\n'
    '**90% CI for $p_1-p_2$** $= -0.11 \\pm 1.645\\cdot 0.0298 \\approx [-0.159,\\ -0.061]$.\n\n'
    '**Interpretation.** With 90% confidence $p_1-p_2 \\in [-0.159,-0.061]$. The CI is entirely negative → bank 2 has a **significantly higher** approval proportion than bank 1 at the 90% level.\n\n'
    '**R commands:**\n\n'
    '`p1 <- 0.67; n1 <- 8000`\n\n'
    '`p2 <- 156/200; n2 <- 200`\n\n'
    '`se <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)`\n\n'
    '`(p1 - p2) + c(-1,1) * qnorm(0.95) * se`\n\n'
    '`## [1] -0.1590 -0.0610`\n\n'
    '`# or:  prop.test(c(round(0.67*8000), 156), c(8000, 200), conf.level=0.90, correct=FALSE)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_sep_2024_3c_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2024_3c_question.png",
    "statistics/images/past_exams/exam_sep_2024_3c_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2024_3c_answer.png",
]}

# ---- september 2025: 1b, 2b, 2c, 3a, 5b ----
past_exams["exam_sep_2025_1b"] = {
"title": "Sep-2025 Ex1b — Multiple regression mod1: Performance ~ Weight + Ascent + HR.avg + Day.time",
"is_exam": True, "topic_hint": "G24",
"content": (
    '<span class="exam-question-text">Estimate a multiple linear regression model (**mod1**) relating Performance to Weight, Ascent, HR.avg and Day.time (3 decimals). Report estimated coefficients.</span>\n\n'
    '![Ex 1b question — fit mod1 and report coefficients](statistics/images/past_exams/questions/exam_sep_2025_1b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.**\n\n'
    'Fit OLS with one quantitative response and 3 numeric predictors + 1 categorical (Day.time, 3 levels → 2 dummies, Afternoon = baseline). $\\widehat{\\beta}$ minimizes $\\sum(y_i - x_i^\\top\\beta)^2$.\n\n'
    '**Estimated equation:**\n\n'
    '$$\\widehat{\\text{Performance}} = 151.921 - 2.029\\cdot\\text{Weight} - 11.022\\cdot\\text{Ascent} + 0.593\\cdot\\text{HR.avg} - 0.366\\cdot\\mathbb{1}(\\text{Evening}) - 0.378\\cdot\\mathbb{1}(\\text{Morning}).$$\n\n'
    '**Interpretation (holding the others fixed):**\n\n'
    '- +1 kg Weight → Performance changes by **−2.029**.\n'
    '- +1 unit Ascent → Performance changes by **−11.022** (largest effect).\n'
    '- +1 bpm HR.avg → Performance changes by **+0.593**.\n'
    '- Evening sessions score **0.366 below** and Morning sessions **0.378 below** the Afternoon baseline (both individually non-significant).\n\n'
    '![Ex 1b AI walkthrough — coefficient plot of mod1](statistics/images/past_exams/exam_sep_2025_1b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $\\hat\\beta_0 = 151.921$; $\\hat\\beta_{\\text{Weight}} = -2.029$; $\\hat\\beta_{\\text{Ascent}} = -11.022$; $\\hat\\beta_{\\text{HR.avg}} = 0.593$; $\\hat\\beta_{\\text{Evening}} = -0.366$; $\\hat\\beta_{\\text{Morning}} = -0.378$.\n\n'
    '**R commands:**\n\n'
    '`mod1 <- lm(Performance ~ Weight + Ascent + HR.avg + Day.time, data=Performance)`\n\n'
    '`summary(mod1)`\n\n'
    '![Ex 1b answer — lm summary output](statistics/images/past_exams/answers/exam_sep_2025_1b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2025_1b_question.png",
    "statistics/images/past_exams/exam_sep_2025_1b_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2025_1b_answer.png",
]}

past_exams["exam_sep_2025_2b"] = {
"title": "Sep-2025 Ex2b — Two-sided z-test from estimate and SE",
"is_exam": True, "topic_hint": "G14",
"subtopic_hint": "g14a",
"content": (
    '<span class="exam-question-text">Given estimate $\\hat\\beta = 0.510$ and standard error $\\mathrm{SE}(\\hat\\beta) = 0.221$, test $H_0: \\beta = 0$ vs $H_1: \\beta \\ne 0$ and report the p-value (two-sided, $z$ approximation).</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2025_2b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** This is a **Wald-type** two-sided test for a single regression coefficient. Under $H_0:\\beta=0$ the standardized statistic $z=\\hat\\beta/\\mathrm{SE}(\\hat\\beta)$ is approximately $N(0,1)$ in large samples. Plug in: $z = 0.510/0.221 \\approx 2.308$. The two-sided p-value is the probability that a $N(0,1)$ exceeds $|z|$ in absolute value:\n\n'
    '$$p = 2\\,\\bigl(1-\\Phi(|z|)\\bigr) = 2\\,(1-\\Phi(2.308)) \\approx 0.021.$$\n\n'
    'Decision: reject $H_0$ at $\\alpha = 0.05$ (since $0.021 < 0.05$) but **not** at $\\alpha = 0.01$ (since $0.021 > 0.01$). Evidence against $H_0$ is moderate.\n\n'
    '![AI walkthrough — N(0,1) with two-sided tail area shaded beyond $|z|=2.308$ (left); decision diagram across $\\alpha\\in\\{0.01,0.025,0.05,0.10\\}$ (right).](statistics/images/past_exams/exam_sep_2025_2b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $z = \\hat\\beta/\\mathrm{SE}(\\hat\\beta) = 0.510/0.221 \\approx 2.308$. Two-sided p-value $= 2(1-\\Phi(2.308)) \\approx \\mathbf{0.021}$. **Reject $H_0$** at the 5% level; fail to reject at the 1% level.\n\n'
    '**R commands:**\n\n'
    '`z <- 0.510/0.221`\n\n'
    '`z`\n\n'
    '`## [1] 2.307692`\n\n'
    '`2*(1-pnorm(abs(z)))`\n\n'
    '`## [1] 0.02102`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_sep_2025_2b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2025_2b_question.png",
    "statistics/images/past_exams/exam_sep_2025_2b_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2025_2b_answer.png",
]}

past_exams["exam_sep_2025_2c"] = {
"title": "Sep-2025 Ex2c — Conclusion across significance levels",
"is_exam": True, "topic_hint": "G14",
"subtopic_hint": "g14a",
"content": (
    '<span class="exam-question-text">Using the p-value from 2b ($p \\approx 0.021$), state the test conclusion at significance levels $\\alpha \\in \\{0.01,\\ 0.025,\\ 0.05,\\ 0.10\\}$.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2025_2c_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** A p-value is a *measure of evidence* against $H_0$ — it does **not** encode any single decision until you fix a tolerance for false positives $\\alpha$. The frequentist rule is mechanical: **reject $H_0$ iff $p < \\alpha$**. Same data, same $p$, but the verdict flips as $\\alpha$ moves. Equivalently, on the $z$-scale the rejection region $|z| > z_{1-\\alpha/2}$ shrinks as $\\alpha$ shrinks: at $\\alpha=0.10$ the threshold is $|z_c|\\approx 1.64$, at $0.05$ it is $1.96$, at $0.025$ it is $2.24$, at $0.01$ it is $2.58$. The observed $|z|=2.31$ sits **inside** the first three regions but **outside** the 1% region — exactly the same information as $p\\approx 0.021$ being below $0.10, 0.05, 0.025$ but above $0.01$.\n\n'
    '![AI walkthrough — p vs alpha number-line and z rejection regions](statistics/images/past_exams/exam_sep_2025_2c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Decision rule: reject $H_0$ iff $p < \\alpha$. With $p \\approx 0.021$:\n\n'
    '- $\\alpha = 0.01$: $p > \\alpha$ → **fail to reject** $H_0$.\n'
    '- $\\alpha = 0.025$: $p < \\alpha$ → **reject** $H_0$.\n'
    '- $\\alpha = 0.05$: $p < \\alpha$ → **reject** $H_0$.\n'
    '- $\\alpha = 0.10$: $p < \\alpha$ → **reject** $H_0$.\n\n'
    'Evidence against $H_0$ is **moderate**: significant at the 2.5%, 5% and 10% levels but **not** at the 1% level.\n\n'
    '**R commands:**\n\n'
    '`p <- 2*(1-pnorm(0.510/0.221))`\n\n'
    '`p`\n\n'
    '`## [1] 0.02102`\n\n'
    '`p < c(0.01, 0.025, 0.05, 0.10)`\n\n'
    '`## [1] FALSE  TRUE  TRUE  TRUE`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_sep_2025_2c_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2025_2c_question.png",
    "statistics/images/past_exams/exam_sep_2025_2c_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2025_2c_answer.png",
]}

past_exams["exam_sep_2025_3a"] = {
"title": "Sep-2025 Ex3a — Conditional frequency Fr(Effort | Rain=Yes)",
"is_exam": True, "topic_hint": "G7",
"subtopic_hint": "g7_twoway",
"content": (
    '<span class="exam-question-text">From the stacked-bar plot of `Effort` (Low / Medium-Low / Medium-High / High) by `Rain` (Yes / No), read off the conditional frequency that `Effort` is at least `Medium-High` given `Rain = Yes`.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2025_3a_question.png)\n\n'
    '---\n\n'
    "**Walkthrough.** With `freq.type='x|y'` each bar shows $\\Pr(\\text{Effort}=e \\mid \\text{Rain}=r)$, so **each Rain column sums to 1**. The event $\\{\\text{Effort} \\geq \\text{Medium-High}\\}$ is the union of the two top slices (Medium-High and High), which are disjoint, so we **add** them inside the Rain = Yes column: $\\Pr(\\text{Effort} \\geq \\text{MH} \\mid \\text{Rain}=\\text{Yes}) = \\Pr(\\text{MH} \\mid \\text{Yes}) + \\Pr(\\text{H} \\mid \\text{Yes}) = 0.39 + 0.48 = \\mathbf{0.87}$. Note this is a **column-conditional** reading: we do **not** divide by the Rain marginal because the stacked bars already do that — the `x|y` switch is what makes the 0.87 directly readable off the chart.\n\n"
    '![AI walkthrough — stacked bars Pr(Effort | Rain) with MH + H bracket in the Yes column](statistics/images/past_exams/exam_sep_2025_3a_ai.png)\n\n'
    '---\n\n'
    "**Answer.** $\\Pr(\\text{Effort} \\geq \\text{Medium-High} \\mid \\text{Rain} = \\text{Yes}) = 0.39 + 0.48 = \\mathbf{0.87}$. On rainy days about **87%** of activities are at high or medium-high effort.\n\n"
    '**R commands:**\n\n'
    "`distr.plot.xy(Effort, Rain, plot.type='bars', freq.type='x|y', data=Performance)`\n\n"
    "`distr.table.xy(Effort, Rain, freq.type='x|y', freq='prop', data=Performance)`\n\n"
    '![Answer](statistics/images/past_exams/answers/exam_sep_2025_3a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2025_3a_question.png",
    "statistics/images/past_exams/exam_sep_2025_3a_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2025_3a_answer.png",
]}

