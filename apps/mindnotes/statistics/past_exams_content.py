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
"title": "P1-2024 Ex1a — Company Age distribution (categorical ordinal)",
"is_exam": True, "topic_hint": "G2",
"content": _q(
    "Describe the distribution of the variable `Age` (categorical ordinal) in the `Company` dataset. Specify which measures you use and why.",
    "All 668 cases observed (no missing). Frequencies: VeryYoung 93 (13.9%), Young 265 (39.7%), Adult 236 (35.3%), Senior 74 (11.1%). Non-uniform distribution; the Young category is modal. For ordinal categorical use *frequencies, proportions, mode, median* — NOT mean.",
    "distr.table.x(Company$Age, freq=c('counts','perc'))\ndistr.summary.x(Company$Age, stats=c('mode','median'))"
), "images": []}

past_exams["exam_p1_2024_1b"] = {
"title": "P1-2024 Ex1b — Channel × Age two-way",
"is_exam": True, "topic_hint": "G7",
"content": _q(
    "Analyze the relationship between `Channel` (categorical) and `Age`. Which Channel is most prevalent for each Age category?",
    "Multi-channel is the modal category in every Age group: VeryYoung 39, Young 126, Adult 102, Senior 32. Ecomm and Mob are 2nd/3rd across all ages. The conditional distribution of Channel given Age is essentially homogeneous across ages — suggesting weak association.",
    "distr.table.xy(Company$Age, Company$Channel, freq=c('counts','percentages'), freq.type='y|x')"
), "images": []}

past_exams["exam_p1_2024_1c"] = {
"title": "P1-2024 Ex1c — Profitability descriptive",
"is_exam": True, "topic_hint": "G4",
"content": _q(
    "Describe the distribution of the numeric variable `Profitability` (5-number summary, mean, SD).",
    "Min 501, Q1 724, Median 839, Mean 857.7, Q3 971, Max 1261, SD 167.4. Approximately symmetric with slight right skew (mean ≈ median + 19). No extreme outliers ($Q_3+1.5\\cdot IQR = 1342.5 >$ max).",
    "distr.summary.x(Company$Profitability, stats=c('fivenumber','mean','sd'))"
), "images": ["statistics/images/exam_p1_2024_profitability.png"]}

past_exams["exam_p1_2024_2a"] = {
"title": "P1-2024 Ex2a — Campaign Revenues boxplot",
"is_exam": True, "topic_hint": "G6",
"content": _q(
    "Analyze the distribution of `Revenues` in the `Campaign` dataset. Indicate the box extremes and any outliers.",
    "5-number summary: Min 151.48, Q1 504.56, Median 752.36, Q3 1202.36, Max 2792. The box spans [504.56, 1202.36] split by median 752.36. The whiskers extend to the max regular values within Q1±1.5·IQR and Q3+1.5·IQR. The right tail is longer — distribution right-skewed.",
    "distr.summary.x(Campaign$Revenues, stats='fivenumber')\ndistr.plot.x(Revenues, plot.type='boxplot', data=Campaign)"
), "images": ["statistics/images/exam_p1_2024_revenues.png"]}

past_exams["exam_p1_2024_2b"] = {
"title": "P1-2024 Ex2b — Loyalty distribution",
"is_exam": True, "topic_hint": "G2",
"content": _q(
    "Report the distribution of `Loyalty` in `Campaign`.",
    "Frequencies: [10,20) 316 (21.8%), [20,40) 341 (23.5%), [40,50) 233 (16.1%), [50,70) 314 (21.7%), [70,90) 246 (17.0%). Total 1450. Relatively uniform, with slight concentration in [20,40).",
    "distr.table.x(Campaign$Loyalty, freq=c('counts','perc'))\ndistr.plot.x(Loyalty, plot.type='bars', data=Campaign)"
), "images": []}

past_exams["exam_p1_2024_3a"] = {
"title": "P1-2024 Ex3 — CV comparison Company A vs B",
"is_exam": True, "topic_hint": "G5",
"content": _q(
    "Compare the dispersion of `Loyalty` between Company A (under investigation, mean 40.50, SD 14.25) and competitor Company B (mean 44.95, SD 13.04) via the coefficient of variation.",
    "$CV_A = 14.25/40.50 = 0.3519 \\;(35.19\\%)$; $CV_B = 13.04/44.95 = 0.2902 \\;(29.02\\%)$. **Loyalty is more dispersed in the previous company (A) than in the competitor (B)**, in relative terms.",
    "sd(LoyaltyA)/mean(LoyaltyA)*100\nsd(LoyaltyB)/mean(LoyaltyB)*100"
), "images": []}

past_exams["exam_p1_2024_4a"] = {
"title": "P1-2024 Ex4 — Scatter Costs vs Sales, correlation",
"is_exam": True, "topic_hint": "G9",
"content": _q(
    "Analyze the scatter between `Costs` and `Sales` in `Campaign`. Comment on the correlation and the strength of the relationship.",
    "Strong positive linear relationship; Pearson's $r \\approx 0.76$ (medium-high). Points cluster around a straight upward-sloping line with moderate dispersion.",
    "cor(Campaign$Costs, Campaign$Sales)\ndistr.plot.xy(x=Costs, y=Sales, plot.type='scatter', fitline=T, data=Campaign)"
), "images": ["statistics/images/exam_p1_2024_costs_sales.png"]}

past_exams["exam_p1_2024_5a"] = {
"title": "P1-2024 Ex5 — Location × Loyalty contingency",
"is_exam": True, "topic_hint": "G7",
"content": _q(
    "Analyze the relationship between `Location` (Semi-Central/Peripheral/Hinterland) and `Loyalty` categories in `Campaign`.",
    "Joint counts row-by-row (Location × Loyalty bins): Semi-Central concentrates in [20,40); Peripheral and Hinterland are more uniform across loyalty bins. Conditional distributions differ somewhat → mild association.",
    "distr.table.xy(Campaign$Location, Campaign$Loyalty, freq='perc', freq.type='y|x')"
), "images": []}

past_exams["exam_p1_2024_6a"] = {
"title": "P1-2024 Ex6 — Normal P(X > 506) for X ~ N(500, 25²)",
"is_exam": True, "topic_hint": "G10",
"content": _q(
    "For $X \\sim N(500, 25^2)$, compute $P(X > 506)$.",
    "Standardize: $z = (506-500)/25 = 0.24$. $P(X>506) = 1 - \\Phi(0.24) \\approx 1 - 0.5948 = 0.4052$.",
    "1 - pnorm(506, mean=500, sd=25)\n# = 0.4052"
), "images": []}

past_exams["exam_p1_2024_6b"] = {
"title": "P1-2024 Ex6 — Normal P(480 < X < 520)",
"is_exam": True, "topic_hint": "G10",
"content": _q(
    "Compute $P(480 < X < 520)$ for $X \\sim N(500, 25^2)$.",
    "Standardize: $z_1 = -0.8$, $z_2 = 0.8$. $P(480<X<520) = \\Phi(0.8) - \\Phi(-0.8) = 0.7881 - 0.2119 = 0.5762$.",
    "pnorm(520, 500, 25) - pnorm(480, 500, 25)\n# = 0.5762"
), "images": []}

past_exams["exam_p1_2024_7a"] = {
"title": "P1-2024 Ex7 — Sampling distribution + CLT",
"is_exam": True, "topic_hint": "G11",
"content": _q(
    "$E[X] = 500, \\mathrm{Var}(X) = 5500$. Sample of $n = 100$ customers. Find the probability that the sample mean exceeds 503.",
    "By CLT, $\\bar X \\sim N(500, 5500/100) = N(500, 55)$, SE = $\\sqrt{55} \\approx 7.42$. $P(\\bar X > 503) = 1 - \\Phi((503-500)/7.42) = 1 - \\Phi(0.4044) \\approx 0.3429$.",
    "1 - pnorm(503, mean=500, sd=sqrt(5500/100))"
), "images": []}

# =================== 1st PARTIAL 2025 ===================

past_exams["exam_p1_2025_1a"] = {
"title": "P1-2025 Ex1a — Conditional Reach distribution by Engagement (Metrics2)",
"is_exam": True, "topic_hint": "G8",
"content": _q(
    "Compare the conditional distributions of `Reach` for upper vs lower quintile posts (by Engagement). Describe via Q25, Q50, Q75.",
    "Side-by-side boxplots show **upper-quintile posts have a higher median and wider IQR** than lower-quintile posts. Conditional distributions differ → Engagement and Reach are associated.",
    "distr.plot.xy(x=Reach, y=Out.Engage, plot.type='boxplot', data=Metrics2)\ndistr.summary.x(Reach, by=Out.Engage, stats='fivenumber', data=Metrics2)"
), "images": ["statistics/images/exam_p1_2025_reach_by_engage.png"]}

past_exams["exam_p1_2025_1b"] = {
"title": "P1-2025 Ex1b — Multiple regression Engagement ~ Reach + Paid + Content",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Propose a linear model to estimate Engagement based on Reach, Paid (Yes/No), Content (Brand/NoBrand).",
    "Model: `Engagement = β₀ + β₁·Reach + β₂·PaidYes + β₃·ContentNoBrand + ε`. From the `summary(model)` shown: intercept = 0.07143 (p<2e-16); Reach slope = 0.00168 (p<2e-16); PaidYes = 0.04321 (p≈3.6e-07). $R^2 \\approx 0.40$. All coefficients significant.",
    "model <- lm(Engagement ~ Reach + Paid + Content, data=Metrics2)\nsummary(model)\nconfint(model)"
), "images": []}

past_exams["exam_p1_2025_2a"] = {
"title": "P1-2025 Ex2a — SE of sample proportion (Shares=low vs high)",
"is_exam": True, "topic_hint": "G11",
"content": _q(
    "For Shares=low ($\\hat p = 0.32$, $n = 550$) and Shares=high ($\\hat p = 0.173$, $n = 550$), compute the standard errors.",
    "$SE(\\hat p_{\\text{low}}) = \\sqrt{0.32(1-0.32)/550} = 0.020$. $SE(\\hat p_{\\text{high}}) = \\sqrt{0.173(1-0.173)/550} = 0.016$.",
    "sqrt(0.32*(1-0.32)/550)\nsqrt(0.173*(1-0.173)/550)"
), "images": []}

past_exams["exam_p1_2025_2b"] = {
"title": "P1-2025 Ex2b — Misinterpretation of SE",
"is_exam": True, "topic_hint": "G11",
"content": _q(
    "Can we conclude from a lower SE that the *specific* estimate is closer to the parameter? Justify.",
    "**No.** A lower SE means generic estimates from this estimator are more tightly clustered around the unknown parameter on average. It does NOT measure the distance of any *specific* realised estimate from the parameter. No conclusion can be drawn about accuracy of a single observed estimate.",
    "# Conceptual question — no R needed"
), "images": []}

# =================== 1st PARTIAL 2026 ===================

past_exams["exam_p1_2026_1a"] = {
"title": "P1-2026 Ex1a — Boxplots of delays by route",
"is_exam": True, "topic_hint": "G6",
"content": _q(
    "Construct and interpret boxplots showing the distribution of departure delays for three flight routes.",
    "Side-by-side boxplots reveal: differences in median, IQR (spread), and presence of outliers per route. Routes with longer whiskers / outliers indicate heavier-tailed delay distributions.",
    "distr.plot.xy(x=delay, y=route, plot.type='boxplot', data=Flights)"
), "images": ["statistics/images/exam_p1_2026_bid_boxplot.png"]}

past_exams["exam_p1_2026_1b"] = {
"title": "P1-2026 Ex1b — SE of mean (3 booking channels)",
"is_exam": True, "topic_hint": "G11",
"content": _q(
    "Compute SE of sample mean for: Aggregator ($\\bar x = 56.22, s = 12.13, n = 142$) and Airline ($\\bar x = 53.50, s = 22.06, n = 224$).",
    "$SE_{\\text{Aggregator}} = 12.13/\\sqrt{142} = 1.018$. $SE_{\\text{Airline}} = 22.06/\\sqrt{224} = 1.474$. Aggregator estimate is *more precise* in expectation.",
    "12.13/sqrt(142)   # 1.018\n22.06/sqrt(224)   # 1.474"
), "images": []}

past_exams["exam_p1_2026_1c"] = {
"title": "P1-2026 Ex1c — SE interpretation: cannot judge a single estimate",
"is_exam": True, "topic_hint": "G11",
"content": _q(
    "Can we conclude one estimate is more reliable from a lower SE? Explain.",
    "**No.** The SE describes the *sampling distribution* of the estimator — the spread of generic estimates around the population parameter. It does NOT measure the distance of any *specific* realised estimate from the parameter. We only know estimates from Aggregator are *on average* tighter around the population mean.",
    "# Conceptual question — no R needed"
), "images": []}

# =================== GENERAL 1 2024 ===================

past_exams["exam_g1_2024_1a"] = {
"title": "G1-2024 Ex1 — CI for mean ($\\bar x = 50, s = 5, n = 100$, 95%)",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Sample of $n = 100$: $\\bar x = 50$, $s^2 = 25$. Build a 95% CI for $\\mu$.",
    "$\\sigma$ unknown but $n$ large → $\\bar X \\pm z_{0.975}\\cdot s/\\sqrt n = 50 \\pm 1.96 \\cdot 5/10 = 50 \\pm 0.98 = [49.02, 50.98]$.",
    "n <- 100; xbar <- 50; s <- 5\nci_lower <- xbar - 1.96 * s/sqrt(n)\nci_upper <- xbar + 1.96 * s/sqrt(n)\nc(ci_lower, ci_upper)"
), "images": []}

past_exams["exam_g1_2024_1b"] = {
"title": "G1-2024 Ex1b — Sample size for ME ≤ 0.5 at 95%",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "What sample size is needed for the margin of error to be at most 0.5?",
    "$ME = 1.96 \\cdot 5/\\sqrt n \\le 0.5 \\Rightarrow \\sqrt n \\ge 19.6 \\Rightarrow n \\ge 384.16$. So **$n = 385$**.",
    "n_needed <- ceiling((1.96 * 5 / 0.5)^2)"
), "images": []}

past_exams["exam_g1_2024_2a"] = {
"title": "G1-2024 Ex2 — Read2 vs Math2 correlation = 0.77",
"is_exam": True, "topic_hint": "G9",
"content": _q(
    "Interpret a correlation $r = 0.77$ between PrimaryRead2 and PrimaryMath2.",
    "Strong positive linear relationship: higher reading scores associate with higher math scores. $r^2 = 0.59 \\Rightarrow$ ~59% of variance shared. From the scatter, the cloud follows a roughly straight rising line with moderate scatter.",
    "cor(PrimaryRead2, PrimaryMath2)   # 0.77\ndistr.plot.xy(x=PrimaryRead2, y=PrimaryMath2, plot.type='scatter', fitline=T)"
), "images": ["statistics/images/exam_g1_2024_read_math.png"]}

past_exams["exam_g1_2024_2b"] = {
"title": "G1-2024 Ex2b — Heteroscedasticity in the Read2/Math2 scatter",
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
    "Take the 95th percentile of SleepQuality: **9.04** in the sample.",
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
"title": "G1-2025 Ex2 — Paired t-test SleepQuality pre vs post diet",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Test whether sleep duration changed after diet ($n = 161$, $\\bar x_{\\text{before}} = 402.89, s_{\\text{before}} = 45.61, \\bar x_{\\text{after}} = 414, s_{\\text{after}} = 48$, correlation = 0.71).",
    "Paired t-test:  $\\hat\\sigma_D = \\sqrt{s_{\\text{before}}^2 + s_{\\text{after}}^2 - 2r\\cdot s_{\\text{before}}\\cdot s_{\\text{after}}} = 35.71$. $t_{\\text{obs}} = (414-402.89)/(35.71/\\sqrt{161}) = 3.95$. p-value $= P(T_{160} \\ge 3.95) \\approx 5.8 \\times 10^{-5}$. **Reject $H_0$ at any conventional $\\alpha$** — sleep duration significantly increased after the diet.",
    "sd_D <- sqrt(45.61^2 + 48^2 - 2*0.71*45.61*48)\nt_stat <- (414-402.89)/(sd_D/sqrt(161))\n1 - pt(t_stat, df=160)\n1 - pnorm(t_stat)"
), "images": []}

past_exams["exam_g1_2025_3a"] = {
"title": "G1-2025 Ex3 — Multiple regression SleepQuality ~ Age+PhysAct+Hours",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Estimate `SleepQuality ~ Age + PhysicalActivity + Hours` and interpret. Predict mean SleepQuality for a customer profile with 95% CI.",
    "From `summary(mod)`: Intercept 3.782 (p<<0.01), Age 0.0163 (p=0.009), PhysicalActivity 0.0075 (p=0.586 — not sig), Hours 0.182 (p=0.004). Adjusted $R^2 = 0.42$, F p-value ≈ $10^{-12}$. Predicted mean for the given covariates: 6.866, 95% CI = [6.236, 7.496].",
    "mod <- lm(SleepQuality ~ Age + PhysicalActivity + Hours, data=sleep)\nsummary(mod)\npredict(mod, newdata=..., interval='confidence', level=0.95)"
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
    "CI.prop(PurposeLoan, success='Business', conf.level=0.99, data=Loans)"
), "images": ["statistics/images/exam_g1_2026_purposeloan.png"]}

past_exams["exam_g1_2026_1b"] = {
"title": "G1-2026 Ex1b — Hypothesis test using CI",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Using the CI from 1a (0.15, 0.24), test $H_0: p = 0.3$ vs $H_1: p \\ne 0.3$ at any level $\\alpha$.",
    "Since $0.3 \\notin [0.15, 0.24]$, the 99% CI **rejects** $H_0$ at level $\\alpha = 0.01$. Equivalently, any test at $\\alpha \\ge 0.01$ rejects. At $\\alpha < 0.01$ (e.g. 0.005), the conclusion would require a wider CI to verify.",
    "# Duality between CI and 2-sided test"
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
    "Analytic form: $\\hat p \\pm z_{\\alpha/2}\\cdot \\sqrt{\\hat p(1-\\hat p)/n}$. With $\\hat p = 0.21$, $n = 45$, 99% CI = $[0.16, 0.26]$. Interpretation: with 99% confidence the proportion lies between 0.16 and 0.26.",
    "vec.bin <- CrimePeople > 250\nCI.prop(vec.bin, conf.level=0.99, data=USCities)"
), "images": ["statistics/images/exam_g2_2024_crime.png"]}

past_exams["exam_g2_2024_5c"] = {
"title": "G2-2024 Ex5c — Sample size for CI width ≤ 0.05",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "What sample size guarantees a 99% CI with width $\\le 0.05$?",
    "$ME \\le 0.025$. Worst-case $p = 0.5$: $n \\ge (2.576 \\cdot 0.5)^2/0.025^2 = 2654.31$. **Minimum n = 2655 cities**.",
    "ceiling((qnorm(0.995)*0.5/0.025)^2)"
), "images": []}

# =================== GENERAL 2 2025 ===================

past_exams["exam_g2_2025_1a"] = {
"title": "G2-2025 Ex1 — Hypothesis test new process (μ > 100)",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Sample $n=36$, $\\bar x = 105$, $s = 15$. Test $H_0: \\mu = 100$ vs $H_1: \\mu > 100$ at $\\alpha = 0.05$.",
    "$t = (105-100)/(15/\\sqrt{36}) = 2$. With df $= 35$, one-sided p-value $\\approx 0.0274 < 0.05$ → **reject $H_0$**. There is evidence the new process increased the mean.",
    "t_stat <- (105 - 100) / (15 / sqrt(36))\np_value <- 1 - pt(t_stat, df=35)\nqt(0.95, df=35)"
), "images": []}

past_exams["exam_g2_2025_2a"] = {
"title": "G2-2025 Ex2 — Type II error and CI formula",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Define type II error and the CI formula for $\\mu$ when $\\sigma$ unknown.",
    "**Type II error** $\\beta$: $\\Pr(\\text{fail to reject }H_0 | H_1 \\text{ true})$. **CI**: $\\bar x \\pm t_{\\alpha/2, n-1}\\cdot s/\\sqrt n$ where $t_{\\alpha/2, n-1}$ is the upper $\\alpha/2$ critical value of a $t$-distribution with $n-1$ df.",
    "qt(1 - alpha/2, df=n-1)\nci_lower <- mean_x - qt(1-alpha/2, n-1)*sd_x/sqrt(n)\nci_upper <- mean_x + qt(1-alpha/2, n-1)*sd_x/sqrt(n)"
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
"title": "G2-2026 Ex1 — CI for proportion difference (campaign)",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Given the difference in two proportions $\\hat p_1 - \\hat p_2 = 0.147$ with 95% CI $[-0.091, 0.385]$, interpret.",
    "Interval contains 0 → no significant difference between the two campaign proportions at 95% level. We cannot reject $H_0: p_1 = p_2$.",
    "CI.diffprop(x, y, conf.level=0.95)\n# OR: TEST.diffprop(x, y, pdiff=0, alternative='two.sided')"
), "images": ["statistics/images/exam_g2_2026_prices.png"]}

past_exams["exam_g2_2026_2a"] = {
"title": "G2-2026 Ex2 — Homoscedasticity diagnosis + adjusted SEs",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "State the homoscedasticity assumption, evidence of violation, and remedies.",
    "$\\mathrm{Var}(\\varepsilon_i | x) = \\sigma^2$ for every $i$. Diagnose via residuals vs fitted (look for cones), Breusch-Pagan or White test. If violated → use **heteroscedasticity-robust SEs** (HC1/HC3), weighted least squares, or log-transform.",
    "plot(mod, which=1)\nlibrary(lmtest); bptest(mod)\nlibrary(sandwich); coeftest(mod, vcov=vcovHC(mod, 'HC3'))"
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
    "distr.plot.xy(x=var, y=group, plot.type='boxplot', data=DF)"
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
    "qnorm(0.05, mean=27000, sd=7000)"
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
    "Interpret $\\hat\\beta_1 = 7.84$ for `Account_length`.",
    "Holding all other variables constant, **a one-year increase in account length is associated with a 7.84-unit increase in Score** on average.",
    "summary(mod)\nconfint(mod)['Account_length',]"
), "images": []}

past_exams["exam_sep_2024_3d"] = {
"title": "Sep-2024 Ex3d — Homoscedasticity check from residuals",
"is_exam": True, "topic_hint": "G15",
"content": _q(
    "Is homoscedasticity respected for this model?",
    "Plot residuals vs fitted. Constant spread across fitted values → assumption OK. Fanning/cone → violated. For this model the residual scatter looks roughly uniform → reasonable.",
    "plot(mod, which=1)"
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
    "TEST.diffmean(Performance, by=Activity.type, type='independent', var.test=TRUE)"
), "images": []}

past_exams["exam_sep_2025_4a"] = {
"title": "Sep-2025 Ex4 — 90% CI for difference in means (pooled)",
"is_exam": True, "topic_hint": "G13",
"content": _q(
    "Construct a 90% CI for $\\mu_A - \\mu_B$ assuming equal variances.",
    "$\\bar y_A - \\bar y_B \\pm t_{0.05, n_A+n_B-2}\\cdot \\sqrt{s^2_{\\text{pool}}\\cdot (1/n_A+1/n_B)}$. Reported sample interval: $(-6.09, -3.05)$ at 90% confidence.",
    "CI.diffmean(PerfA, PerfB, type='independent', var.equal=TRUE, conf.level=0.90)"
), "images": []}

past_exams["exam_sep_2025_5a"] = {
"title": "Sep-2025 Ex5 — Levene's test for equal variances",
"is_exam": True, "topic_hint": "G14",
"content": _q(
    "Test $H_0: \\sigma_A^2 = \\sigma_B^2$ vs $H_1: \\sigma_A^2 \\ne \\sigma_B^2$ via Levene's test.",
    "Levene's test compares group residuals from group medians. If p < $\\alpha$ → reject equal-variances; use Welch's t-test instead of pooled. If p ≥ $\\alpha$ → keep pooled-variance t-test.",
    "library(car); leveneTest(Performance ~ Activity.type, data=Performance)"
), "images": []}
