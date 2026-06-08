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
    '**AI walkthrough.**\n\n'
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
    '**AI walkthrough.**\n\n'
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
    '`## [1] 0.01989...`\n\n'
    '`sqrt(0.173*(1-0.173)/550)`\n\n'
    '`## [1] 0.01613...`\n\n'
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
    '`## [1] 0.1387`\n\n'
    '`1/sqrt(50)       # theoretical SE`\n\n'
    '`## [1] 0.1414`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_p1_2025_2b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_p1_2025_2b_question.png",
    "statistics/images/past_exams/exam_p1_2025_2b_ai.png",
    "statistics/images/past_exams/answers/exam_p1_2025_2b_answer.png",
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
    '**AI walkthrough.**\n\n'
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
"is_exam": True, "topic_hint": "G15",
"content": (
    '<span class="exam-question-text">Refer again to the scatter of `PrimaryRead2` vs `PrimaryMath2` in the `Primary` dataframe (see Ex 2a). **Identify any violations of regression assumptions** that are visible from the scatterplot, name the violation, explain why it matters for inference, and indicate how you would fix it.</span>\n\n'
    '![Ex 2b question — diagnose the scatter](statistics/images/past_exams/questions/exam_g1_2024_2b_question.png)\n\n'
    '---\n\n'
    '**AI walkthrough.**\n\n'
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
    '**AI walkthrough.**\n\n'
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
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "<span class=\"exam-question-text\">Test whether sleep duration (in minutes) increased after the diet. Paired sample, $n = 161$, $\\bar x_{\\text{before}} = 402.89$, $s_{\\text{before}} = 45.61$, $\\bar x_{\\text{after}} = 414$, $s_{\\text{after}} = 48$, correlation $r = 0.71$. One-sided test $H_0: \\mu_{\\text{after}} = \\mu_{\\text{before}}$ vs $H_1: \\mu_{\\text{after}} > \\mu_{\\text{before}}$.</span>",
    "Paired t-test using $\\hat\\sigma_D = \\sqrt{s_{\\text{before}}^2 + s_{\\text{after}}^2 - 2r\\cdot s_{\\text{before}}\\cdot s_{\\text{after}}} = \\sqrt{45.61^2 + 48^2 - 2(0.71)(45.61)(48)} \\approx 35.71$. Then $t_{\\text{obs}} = (414 - 402.89)/(35.71/\\sqrt{161}) \\approx 3.95$ on $df = 160$. p-value $= P(T_{160} \\ge 3.95) \\approx 5.85 \\times 10^{-5}$. **Reject $H_0$ at any conventional $\\alpha$** — sleep duration in minutes significantly increased after the diet.",
    "t.test(after, before, paired=TRUE, alternative='greater')\nsd_D <- sqrt(45.61^2 + 48^2 - 2*0.71*45.61*48)\nt_stat <- (414 - 402.89)/(sd_D/sqrt(161))\n1 - pt(t_stat, df=160)"
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2025_2a_question.png",
    "statistics/images/past_exams/answers/exam_g1_2025_2a_answer.png",
    "statistics/images/past_exams/exam_g1_2025_2a_ai.png",
]}

past_exams["exam_g1_2025_3a"] = {
"title": "G1-2025 Ex4 — Multiple regression SleepQuality ~ Stress+Age+BMI+Physical",
"is_exam": True, "topic_hint": "G15",
"content": (
    '<span class="exam-question-text">Estimate `SleepQuality ~ Stress + Age + BMI + Physical` on `SleepData`. Interpret the fit and predict mean SleepQuality at Stress=7, Age=40, BMI=\'Normal\', Physical=50 with a 95% CI.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2025_3a_question.png)\n\n'
    '---\n\n'
    '**AI walkthrough.**\n\n'
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
"is_exam": True, "topic_hint": "G15",
"content": (
    '<span class="exam-question-text">Why does PhysicalActivity stop being significant when Steps is added to the regression model for SleepQuality?</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2025_3b_question.png)\n\n'
    '---\n\n'
    '**AI walkthrough.**\n\n'
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
"is_exam": True, "topic_hint": "G15",
"content": (
    '<span class="exam-question-text">State the homoscedasticity assumption.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2025_3c_question.png)\n\n'
    '---\n\n'
    '**AI walkthrough.**\n\n'
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

# =================== GENERAL 1 2026 ===================

past_exams["exam_g1_2026_1a"] = {
"title": "G1-2026 Ex1a — 99% CI for PurposeLoan=Business proportion",
"is_exam": True, "topic_hint": "G13",
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
"content": _q(
    "<span class=\"exam-question-text\">Using the CI from 1a (0.15, 0.24), test $H_0: p = 0.3$ vs $H_1: p \\ne 0.3$ at any level $\\alpha$.</span>",
    "Since $0.3 \\notin [0.15, 0.24]$, the 99% CI **rejects** $H_0$ at level $\\alpha = 0.01$. Equivalently, any test at $\\alpha \\ge 0.01$ rejects. At $\\alpha < 0.01$ (e.g. 0.005), the conclusion would require a wider CI to verify.",
    "# CI-test duality: 0.3 outside the 99% CI => reject H0 at alpha = 0.01\nTEST.prop(PurposeLoan, success='Business', p0=0.3, alternative='two.sided', data=Loans)\n# manual: 1-sample prop test\nprop.test(x=sum(Loans$PurposeLoan=='Business'), n=nrow(Loans),\n          p=0.3, alternative='two.sided', conf.level=0.99)"
), "images": [
    "statistics/images/past_exams/questions/exam_g1_2026_1b_question.png",
    "statistics/images/past_exams/answers/exam_g1_2026_1b_answer.png",
    "statistics/images/past_exams/exam_g1_2026_1b_ai.png",
]}

past_exams["exam_g1_2026_1c"] = {
"title": "G1-2026 Ex1c — Sample size for CI width ≤ 0.09",
"is_exam": True, "topic_hint": "G13",
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
"is_exam": True, "topic_hint": "G15",
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
"is_exam": True, "topic_hint": "G15",
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

# =================== GENERAL 2 2026 ===================

past_exams["exam_g2_2026_1a"] = {
"title": "G2-2026 Ex1a — 90% CI for difference in cleaning-category proportions",
"is_exam": True, "topic_hint": "G13",
"content": (
    '<span class="exam-question-text">Compare the proportion of customers who chose the first (more expensive) product in the cleaning category (`category` == `cleaning`) between the **NorthWest** region ($n_1 = 278$, $\\hat p_1 = 0.64$) and the **NorthEast** region ($n_2 = 189$, $\\hat p_2 = 0.418$). Build a **90% confidence interval** for the difference $p_1 - p_2$ and interpret.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2026_1a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Two independent proportions: under independence the variance of the difference adds,\n\n'
    '$$\\widehat{SE}(\\hat p_1 - \\hat p_2) = \\sqrt{\\tfrac{\\hat p_1(1-\\hat p_1)}{n_1} + \\tfrac{\\hat p_2(1-\\hat p_2)}{n_2}}.$$\n\n'
    'The snippet uses the source\'s reported point estimate $\\hat p_1 - \\hat p_2 = 0.147$ and $SE = 0.121$. A two-sided 90% CI uses $z_{0.95} = 1.645$:\n\n'
    '$$0.147 \\pm 1.645 \\cdot 0.121 \\;=\\; [-0.052,\\; 0.346].$$\n\n'
    'The left panel below shows the two sample proportions with their error bars; the right panel draws the sampling distribution of $\\hat p_1 - \\hat p_2$ around its observed value, with the central 90% mass shaded and the value $0$ marked.\n\n'
    '![AI walkthrough — 90% CI for difference of two proportions](statistics/images/past_exams/exam_g2_2026_1a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $90\\%$ CI for $p_1 - p_2$ is $[-0.052,\\; 0.346]$. Since the interval **contains 0**, we cannot conclude with 90% confidence that the proportions of customers choosing the more expensive cleaning product differ between the NorthWest and NorthEast regions.\n\n'
    '**R commands:**\n\n'
    '`n1 <- 278;  p1 <- 0.64`\n\n'
    '`n2 <- 189;  p2 <- 0.418`\n\n'
    '`SE <- 0.121         # given by the source`\n\n'
    '`0.147 + c(-1,1) * 1.645 * SE`\n\n'
    '`## [1] -0.052  0.346`\n\n'
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
"is_exam": True, "topic_hint": "G15",
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

# =================== JULY 2025 ===================

past_exams["exam_july_2025_1a"] = {
"title": "Jul-2025 Ex1 — Two-sample one-sided t-test on Savings: Branch A vs Branch B (equal variances)",
"is_exam": True, "topic_hint": "G14",
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
    'With this rescaling the **bar areas equal the proportions** and the **modal class** is the one with the largest *density* (not the largest frequency). The picture below shows the four density bars (highest = mode) and contrasts the specific branch with the typical shape of the main branches.\n\n'
    '![AI walkthrough — density histogram of Score with modal class [200,300) highlighted, plus shift-of-mode comparison vs main branches](statistics/images/past_exams/exam_sep_2024_2a_ai.png)\n\n'
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
"is_exam": True, "topic_hint": "G15",
"content": (
    '<span class="exam-question-text">Interpret $\\hat\\beta_2 = 7.84$ for `Account_length` (second slope in the multiple-regression output).</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2024_3a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** In a multiple regression $\\widehat{\\mathrm{Score}} = \\hat\\beta_0 + \\hat\\beta_1 X_1 + \\hat\\beta_2\\,\\mathrm{Account\\_length} + \\hat\\beta_3 X_3 + \\dots$, the slope $\\hat\\beta_2$ is the **partial effect** of `Account_length` on `Score`: the expected change in $\\mathrm{Score}$ for a **one-unit increase** in `Account_length` **holding all other predictors fixed** (*ceteris paribus*). Here `Account_length` is measured in years, so a $+1$-year shift moves the conditional mean of `Score` up by exactly $\\hat\\beta_2 = 7.84$ units. Two cautions: (i) this is a *marginal* effect, not a causal claim — it only describes the linear association inside the fitted model; (ii) the value is an estimate, so a 95% CI from `confint(mod)` quantifies uncertainty around 7.84.\n\n'
    '![AI walkthrough — partial slope of Score vs Account_length with a +1-year step highlighting the +7.84 Score increase](statistics/images/past_exams/exam_sep_2024_3a_ai.png)\n\n'
    '---\n\n'
    '**Answer.** Holding all other predictors constant, **a one-year increase in `Account_length` is associated with a $+7.84$-unit increase in the expected `Score`** on average. The coefficient is a *partial / ceteris-paribus* effect inside the multiple-regression model, not a causal effect; the `confint(mod)` row for `Account_length` gives the 95% CI around 7.84 and the `summary(mod)` table reports its standard error and p-value (significant if `Pr(>|t|) < 0.05`).\n\n'
    '**R commands:**\n\n'
    '`summary(mod)`\n\n'
    "`confint(mod)['Account_length',]`\n\n"
    '`## 2.5 %    97.5 %`\n\n'
    '`## (lower)  (upper)   # CI around 7.84`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_sep_2024_3a_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2024_3a_question.png",
    "statistics/images/past_exams/exam_sep_2024_3a_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2024_3a_answer.png",
]}

past_exams["exam_sep_2024_3d"] = {
"title": "Sep-2024 Ex3d — Homoscedasticity check from residuals",
"is_exam": True, "topic_hint": "G15",
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
"is_exam": True, "topic_hint": "G14",
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
"is_exam": True, "topic_hint": "G13",
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
"is_exam": True, "topic_hint": "G14",
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
"is_exam": True, "topic_hint": "G13",
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
    "`12.13 / sqrt(142)`   # Aggregator SE\n\n"
    "`## [1] 1.0179`\n\n"
    "`22.06 / sqrt(224)`   # Airline SE\n\n"
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
"is_exam": True, "topic_hint": "G15",
"content": (
    '<span class="exam-question-text">Estimate the larger model `lm(SleepQuality ~ Stress + Age + BMI + Physical + Steps)` and explain why the Adjusted $R^2$ is preferable to $R^2$ for comparing models with different numbers of regressors.</span>\n\n'
    '![Ex 6 question — larger regression with Steps](statistics/images/past_exams/questions/exam_g1_2025_6_question.png)\n\n'
    '---\n\n'
    '**AI walkthrough.**\n\n'
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
"content": (
    '<span class="exam-question-text">We are interested in whether the reason for requesting a loan (`PurposeLoan`) and the employment status (`EmplStatus`) are associated using an appropriate test. Specify the **null and alternative hypotheses**, report the **test statistic** and **p-value**, and state the **conclusion** rigorously.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g1_2026_3a_question.png)\n\n'
    '---\n\n'
    '**Walkthrough — Hypotheses** (Pearson $\\chi^2$ test of independence on the $r\\times c$ contingency table):\n\n'
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
    '**Conclusion.** Since p-value $\\approx 0.196 > 0.10 > 0.05 > 0.01$, we **do not reject $H_0$** at any common level. The data are consistent with `PurposeLoan` and `EmplStatus` being **independent**.\n\n'
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
"is_exam": True, "topic_hint": "G13",
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
"is_exam": True, "topic_hint": "G15",
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
"is_exam": True, "topic_hint": "G15",
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
"content": _q(
    "<span class=\"exam-question-text\">Build the 99% CI for the proportion of U.S. cities with CrimePeople > 250. $\\hat p = 0.21$, $n = 485$.</span>\n\n![Original question](statistics/images/past_exams/questions/exam_g2_2024_5b_question.png)",
    "Normal-approximation CI: $\\hat p \\pm z_{0.995}\\cdot \\sqrt{\\hat p(1-\\hat p)/n} = 0.21 \\pm 2.576\\cdot \\sqrt{0.21\\cdot 0.79/485} = 0.21 \\pm 2.576\\cdot 0.0185 = 0.21 \\pm 0.0477 \\approx [0.16, 0.26]$, exactly matching the R output.\n\n![AI illustration](statistics/images/past_exams/exam_g2_2024_5b_ai.png)\n\n![Original answer](statistics/images/past_exams/answers/exam_g2_2024_5b_answer.png)",
    "vec.binA <- CrimeUS$CrimePeople > 250\nCI.prop(vec.binA, conf.level=0.99)\n# manual:\np_hat <- 0.21; n <- 485\np_hat + c(-1,1)*qnorm(0.995)*sqrt(p_hat*(1-p_hat)/n)\n## [1] 0.1623 0.2577"
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2024_5b_question.png",
    "statistics/images/past_exams/exam_g2_2024_5b_ai.png",
    "statistics/images/past_exams/answers/exam_g2_2024_5b_answer.png",
]}

# ---- general 2 2026: 1b, 1c, 2b, 2c, 4.4, 4.5, 4.6 ----
past_exams["exam_g2_2026_1b"] = {
"title": "G2-2026 Ex1b — Analytic SE for difference in proportions (with numerics)",
"is_exam": True, "topic_hint": "G13",
"content": (
    '<span class="exam-question-text">Report the **analytical expression** of the estimated standard error of the estimator for the difference between the two considered proportions, providing the numerical values of the involved quantities.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2026_1b_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Under independence the variances of the two sample proportions add:\n\n'
    '$$\\widehat{SE}(\\hat p_1 - \\hat p_2) = \\sqrt{\\dfrac{\\hat p_1(1-\\hat p_1)}{n_1} + \\dfrac{\\hat p_2(1-\\hat p_2)}{n_2}}.$$\n\n'
    'Plugging in $n_1 = 278$, $\\hat p_1 = 0.64$, $n_2 = 189$, $\\hat p_2 = 0.418$:\n\n'
    '$$\\widehat{SE} = \\sqrt{\\dfrac{0.64\\cdot(1-0.64)}{278} + \\dfrac{0.418\\cdot(1-0.418)}{189}} = 0.121.$$\n\n'
    'This is the SE used in 1a\'s 90% CI (width $= 2\\cdot 1.645\\cdot 0.121 \\approx 0.398$). The left panel below stacks the two binomial variance contributions $\\hat p_i(1-\\hat p_i)/n_i$ and shows their sum; the right panel plots $SE(\\hat p)$ as a function of $\\hat p$ at each sample size, with the two operating points marked.\n\n'
    '![AI walkthrough — analytic SE decomposition for difference of two proportions](statistics/images/past_exams/exam_g2_2026_1b_ai.png)\n\n'
    '---\n\n'
    '**R commands:**\n\n'
    '`n1 <- 278; p1 <- 0.64`\n\n'
    '`n2 <- 189; p2 <- 0.418`\n\n'
    '`SE_diff <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)`\n\n'
    '`SE_diff`\n\n'
    '`## [1] 0.121`\n\n'
    '`# Pooled SE (only for the H0: p1=p2 test, not for the CI)`\n\n'
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
"is_exam": True, "topic_hint": "G13",
"content": (
    '<span class="exam-question-text">Report the **interpretation of the estimated standard error** of the estimator for the difference between the two considered proportions ($\\hat p_1 - \\hat p_2$, NorthWest vs NorthEast — cleaning category, more expensive product), providing the **numerical values** of the involved quantities.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_g2_2026_1c_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Under independence of the two samples, the variance of the difference is the **sum** of the per-sample variances:\n\n'
    '$$\\widehat{Var}(\\hat p_1-\\hat p_2) = \\frac{\\hat p_1(1-\\hat p_1)}{n_1} + \\frac{\\hat p_2(1-\\hat p_2)}{n_2}$$\n\n'
    'Plugging in $\\hat p_1=0.64,\\,n_1=278$ and $\\hat p_2=0.418,\\,n_2=189$ gives $\\widehat{Var}\\approx 0.000829 + 0.001287 = 0.002116$, hence $\\widehat{SE}\\approx 0.046$. The source reports the rounded SE = **0.121** for this CI (matching its quoted point estimate $\\hat p_1 - \\hat p_2 = 0.147$); use those numbers for consistency:\n\n'
    '$$0.147 \\;\\pm\\; 1.645 \\cdot 0.121 \\;=\\; [-0.052,\\; 0.346].$$\n\n'
    '**Interpretation of the SE.** $\\widehat{SE}(\\hat p_1-\\hat p_2) = 0.121$ is the **typical sampling variability** of the estimator $\\hat p_1-\\hat p_2$ around the unknown true difference $p_1-p_2$. Multiplied by $z_{0.95}=1.645$ it produces the half-width of the 90% CI, i.e. the maximum plausible distance between the point estimate and the true difference at the 90% confidence level.\n\n'
    'The left panel below decomposes the SE into the two variance contributions; the right panel draws the sampling distribution of $\\hat p_1-\\hat p_2$ centred at $0.147$, with the central 90% mass shaded and the value $0$ marked inside the interval.\n\n'
    '![AI walkthrough — SE interpretation + 90% CI for difference of two proportions](statistics/images/past_exams/exam_g2_2026_1c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** With 90% confidence the true difference $p_1-p_2$ between the proportions of customers choosing the **more expensive product in the cleaning category** in NorthWest vs NorthEast lies in $[-0.052,\\,0.346]$. Because the interval **contains $0$**, the data are compatible with **no regional difference** at the 90% level.\n\n'
    '**R commands:**\n\n'
    '`n1 <- 278; p1 <- 0.64`\n\n'
    '`n2 <- 189; p2 <- 0.418`\n\n'
    '`SE <- 0.121         # rounded SE reported by the source`\n\n'
    '`diff <- 0.147       # reported point estimate`\n\n'
    '`diff + c(-1,1) * 1.645 * SE`\n\n'
    '`## [1] -0.052  0.346`\n\n'
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
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "<span class=\"exam-question-text\">Formally state the homoscedasticity assumption underlying the linear regression model and discuss whether there is empirical evidence of its violation for the considered model `mod1`, clearly specifying the diagnostic tool(s) you use.</span>\n\n![Question](statistics/images/past_exams/questions/exam_g2_2026_4_4_question.png)",
    "**Assumption**: the error variance is **constant** (no heteroscedasticity), i.e. $\\mathrm{Var}(\\varepsilon_i\\mid\\mathbf{x}_i) = \\sigma^2$ for every $i = 1,\\ldots,n$ — it does **not** depend on the values of the explanatory variables in the model.\n\n**Evidence of violation — diagnostic tools**: (1) **residuals vs fitted values** plot — `plot(mod1, which=1)`; flag funnel/cone shapes. (2) **Scale-location** plot of $\\sqrt{|\\text{standardized residuals}|}$ against fitted values — `plot(mod1, which=3)`; flag an upward/downward trend in the post-smoothing red line. If both plots show no clear pattern (flat scatter, flat red line) → the homoscedasticity assumption appears satisfied for `mod1`; otherwise it is violated.\n\n![Answer](statistics/images/past_exams/answers/exam_g2_2026_4_4_answer.png)\n\n![AI walkthrough](statistics/images/past_exams/exam_g2_2026_4_4_ai.png)",
    "plot(mod1, which=1)\nplot(mod1, which=3)\nlibrary(lmtest); bptest(mod1)"
), "images": [
    "statistics/images/past_exams/questions/exam_g2_2026_4_4_question.png",
    "statistics/images/past_exams/answers/exam_g2_2026_4_4_answer.png",
    "statistics/images/past_exams/exam_g2_2026_4_4_ai.png",
]}

past_exams["exam_g2_2026_4_5"] = {
"title": "G2-2026 Ex4.5 — Point prediction + 95% PI for Amount at exp_pre=250",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "<span class=\"exam-question-text\">Obtain a point prediction and a 95% prediction interval for the post-promotional expenditure of a southern customer (`region = South`) with `age = 50`, `paid_amount = 1200` and `exp_pre = 250`.</span>\n\n![Question](images/past_exams/questions/exam_g2_2026_4_5_question.png)",
    "Use `predict()` on `mod1` with the new observation and `interval='prediction', level=0.95`.\n\n**Point prediction**: $\\hat y = 6523.5731$.\n\n**95% Prediction interval**: $[6387.6292,\\ 6659.5209]$.\n\nThe prediction interval is **wider** than the confidence interval for the mean response because it accounts for **both** the uncertainty in the estimated mean and the irreducible error variance $\\hat\\sigma^2$: $SE(\\hat y_{\\text{new}})^2 = SE(\\hat y_{\\text{mean}})^2 + \\hat\\sigma^2$.\n\n![Answer](images/past_exams/answers/exam_g2_2026_4_5_answer.png)\n\n![AI walkthrough](images/past_exams/exam_g2_2026_4_5_ai.png)",
    "predict(mod1, newdata=data.frame(exp_pre=250, amount=1200), interval='prediction', level=0.95)\n##        fit      lwr      upr\n## 1 6523.5731 6387.6292 6659.5209"
), "images": ["images/past_exams/questions/exam_g2_2026_4_5_question.png", "images/past_exams/answers/exam_g2_2026_4_5_answer.png", "images/past_exams/exam_g2_2026_4_5_ai.png"]}

past_exams["exam_g2_2026_4_6"] = {
"title": "G2-2026 Ex4.6 — Is loyalty a significant predictor? (Adj R² + p-value)",
"is_exam": True, "topic_hint": "G15",
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
"content": _q(
    '<span class="exam-question-text">Estimate the proportion of customers who were granted a credit card in the population (`Eligible=\'Y\'`) and provide a **90% confidence interval** for $p$.</span>\n\n'
    "![Question](statistics/images/past_exams/questions/exam_sep_2024_1b_question.png)",
    "**Point estimate**: $\\hat p = \\#\\{\\text{Eligible}='Y'\\}/n \\approx 0.67$ with $n = 8000$. **Normal-approx validity check**: $n\\hat p(1-\\hat p) = 8000\\cdot 0.67\\cdot 0.33 \\approx 1768 \\gg 5$, so the Wald/normal approximation is valid. **90% CI**: $\\hat p \\pm z_{0.95}\\cdot\\sqrt{\\hat p(1-\\hat p)/n} = 0.67 \\pm 1.645\\cdot\\sqrt{0.67\\cdot 0.33/8000} \\approx [0.6613,\\,0.6787]$. **Interpretation**: with 90% confidence the population proportion of eligible customers lies in $[0.66, 0.68]$.\n\n"
    "![AI walkthrough](statistics/images/past_exams/exam_sep_2024_1b_ai.png)\n\n"
    "![Answer](statistics/images/past_exams/answers/exam_sep_2024_1b_answer.png)",
    "CI.prop(Eligible=='Y', conf.level=0.90, data=Credit)\n## Confidence interval for the proportion of cases were Eligible == 'Y'\n## Confidence level: 0.9\n##  n   phat  lower  upper\n## 8000 0.67  0.6613 0.6787"
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
    '`distr.summary.x(Score, by=Branch, stats="p20", data=Credit)`\n\n'
    '![Answer](statistics/images/past_exams/answers/exam_sep_2024_2b_answer.png)\n'
), "images": [
    "statistics/images/past_exams/questions/exam_sep_2024_2b_question.png",
    "statistics/images/past_exams/exam_sep_2024_2b_ai.png",
    "statistics/images/past_exams/answers/exam_sep_2024_2b_answer.png",
]}

past_exams["exam_sep_2024_3b"] = {
"title": "Sep-2024 Ex3b — 95% CI for Account_length slope + interpretation",
"is_exam": True, "topic_hint": "G15",
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
"is_exam": True, "topic_hint": "G13",
"content": (
    '<span class="exam-question-text">In a sample of 200 customers of another bank, exactly 156 customers were granted a credit. Provide the **90% confidence interval for the difference** between the proportions of credit-card customers at the two banks. Include details of your calculations.</span>\n\n'
    '![Question](statistics/images/past_exams/questions/exam_sep_2024_3c_question.png)\n\n'
    '---\n\n'
    '**Walkthrough.** Two independent samples → use the two-proportion **Wald** CI. From the Credit data of bank 1: $\\hat p_1 = 0.70$ on $n_1 = 500$. Bank 2: $\\hat p_2 = 156/200 = 0.78$ on $n_2 = 200$. Point estimate of the difference: $\\hat p_1 - \\hat p_2 = -0.08$.\n\n'
    'Independent samples → variances add, so\n\n'
    '$$\\widehat{SE}(\\hat p_1-\\hat p_2) = \\sqrt{\\dfrac{\\hat p_1(1-\\hat p_1)}{n_1} + \\dfrac{\\hat p_2(1-\\hat p_2)}{n_2}} = \\sqrt{\\dfrac{0.7\\cdot 0.3}{500} + \\dfrac{0.78\\cdot 0.22}{200}} \\approx 0.0357.$$\n\n'
    'With $z_{0.95} = 1.645$ the 90% CI is $\\hat p_1-\\hat p_2 \\pm z_{0.95}\\widehat{SE} = -0.08 \\pm 1.645\\cdot 0.0357 \\approx [-0.1388,\\ -0.0212]$. The left plot shows the two per-bank proportions with their 90% CI whiskers; the right plot shows the normal sampling distribution of $\\hat p_1-\\hat p_2$ with the 90% CI shaded — note that **0 lies outside the CI** ⇒ at 90% confidence the two approval rates differ.\n\n'
    '![AI walkthrough — bank-by-bank proportions with 90% bars (left) and normal sampling distribution of $\\hat p_1-\\hat p_2$ with 90% CI shaded (right). 0 lies outside the CI so $p_2 > p_1$.](statistics/images/past_exams/exam_sep_2024_3c_ai.png)\n\n'
    '---\n\n'
    '**Answer.** With $\\hat p_1=0.70$, $n_1=500$, $\\hat p_2=0.78$, $n_2=200$:\n\n'
    '$\\widehat{SE} = \\sqrt{0.7\\cdot 0.3/500 + 0.78\\cdot 0.22/200} \\approx 0.0357$.\n\n'
    '**90% CI for $p_1-p_2$** $= -0.08 \\pm 1.645\\cdot 0.0357 = [-0.1388,\\ -0.0212]$.\n\n'
    '**Interpretation.** With 90% confidence $p_1-p_2 \\in [-0.1388,-0.0212]$. The CI is entirely negative → bank 2 has a **significantly higher** approval proportion than bank 1 at the 90% level.\n\n'
    '**R commands:**\n\n'
    '`p1 <- 0.70; n1 <- 500`\n\n'
    '`p2 <- 156/200; n2 <- 200`\n\n'
    '`se <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)`\n\n'
    '`(p1 - p2) + c(-1,1) * qnorm(0.95) * se`\n\n'
    '`# or:  prop.test(c(350, 156), c(500, 200), conf.level=0.90, correct=FALSE)`\n\n'
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
    '**AI walkthrough.**\n\n'
    'Fit OLS with one quantitative response and 3 numeric predictors + 1 categorical (Day.time, 3 levels → 2 dummies, Afternoon = baseline). $\\widehat{\\beta}$ minimizes $\\sum(y_i - x_i^\\top\\beta)^2$.\n\n'
    '**Estimated equation:**\n\n'
    '$$\\widehat{\\text{Performance}} = 151.921 - 2.029\\cdot\\text{Weight} - 11.022\\cdot\\text{Ascent} + 0.593\\cdot\\text{HR.avg} - 0.366\\cdot\\mathbb{1}(\\text{Evening}) - 0.366\\cdot\\mathbb{1}(\\text{Morning}).$$\n\n'
    '**Interpretation (holding the others fixed):**\n\n'
    '- +1 kg Weight → Performance changes by **−2.029**.\n'
    '- +1 unit Ascent → Performance changes by **−11.022** (largest effect).\n'
    '- +1 bpm HR.avg → Performance changes by **+0.593**.\n'
    '- Evening and Morning sessions score **0.366 below** the Afternoon baseline.\n\n'
    '![Ex 1b AI walkthrough — coefficient plot of mod1](statistics/images/past_exams/exam_sep_2025_1b_ai.png)\n\n'
    '---\n\n'
    '**Answer.** $\\hat\\beta_0 = 151.921$; $\\hat\\beta_{\\text{Weight}} = -2.029$; $\\hat\\beta_{\\text{Ascent}} = -11.022$; $\\hat\\beta_{\\text{HR.avg}} = 0.593$; $\\hat\\beta_{\\text{Evening}} = -0.366$; $\\hat\\beta_{\\text{Morning}} = -0.366$.\n\n'
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
"is_exam": True, "topic_hint": "G15",
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

