"""Ex 6 — Inferential statistics: confidence intervals & hypothesis tests (CI focus)."""

ex6 = {}

ex6["6_1a"] = {"title": "Ex 6.1a — Point estimate: avg transaction change post-restocking",
"content": """**Question.** From sample data ($\\bar x = 46.7, s_x = 6.1, \\bar y = 56.8, s_y = 6.9, s_{xy} = 34.6$), estimate the average increase in amount per transaction after restocking.

---

**Answer.** Point estimate: $\\bar y - \\bar x = 56.8 - 46.7 = 10.1$ (per transaction).

```r
xbar <- 46.7; sx <- 6.1
ybar <- 56.8; sy <- 6.9; sxy <- 34.6
ybar - xbar    # 10.1
```
""", "images": []}

ex6["6_1b"] = {"title": "Ex 6.1b — 95% CI for the paired mean difference",
"content": """**Question.** Assess the precision of the estimate. Does it depend on assumptions?

---

**Answer.** Build a 95% paired-data CI for the mean change. The CI relies on either large $n$ (CLT) or approximate normality.

```r
n <- 24   # sample size
se_D <- sqrt((sx^2 + sy^2 - 2*sxy)/n)
ME   <- qt(0.975, df=n-1)*se_D
c(ybar-xbar - ME, ybar-xbar + ME)
```
""", "images": []}

ex6["6_1c"] = {"title": "Ex 6.1c — Sample size for width < 5 at 95% confidence",
"content": """**Question.** What sample size is needed so the CI width is below 5?

---

**Answer.**
```r
# Required: ME = z * sigma/sqrt(n) <= 2.5
# n >= (1.96 * 6.9 / 2.5)^2 = 29.44  ->  n = 30
# From a previous sample of size 24, ADDITIONAL n needed: 30 - 24 = 6.
ceiling((1.96 * 6.9 / 2.5)^2)
```
""", "images": []}

ex6["6_1d"] = {"title": "Ex 6.1d — Conservative sample size for proportion CI",
"content": """**Question.** Determine the sample size for a proportion CI when the target proportion is unknown.

---

**Answer.** The variance $p(1-p)$ is maximized at $p = 0.5$, so we use $p = 0.5$ (worst case).

```r
# ME = z * sqrt(p*(1-p)/n) <= 0.0166
# Take p=0.5 (worst case):
# n >= z^2 * 0.25 / ME^2
n_min <- ceiling( (1.96^2) * 0.25 / 0.0166^2 )  # ~ 3487
n_min
```
""", "images": []}

ex6["6_2a"] = {"title": "Ex 6.2a — Mean difference NA vs EU sales (vgsales, paired)",
"content": """**Question.** Estimate the mean difference between NA and EU sales for Action games. What assumptions on the relationship between the two variables matter?

---

**Answer.** The two markets are *paired* (same games observed in both). Use the paired-data formula.

```r
diff <- vgsales$NA_Sales - vgsales$EU_Sales
# Mean difference
mean(diff[vgsales$Genre=="Action"])
sd(diff[vgsales$Genre=="Action"]) / sqrt(sum(vgsales$Genre=="Action"))
```
""", "images": []}

ex6["6_2b"] = {"title": "Ex 6.2b — 98% CI for the NA-EU mean difference",
"content": """**Question.** Construct a 98% CI.

---

**Answer.**
```r
# With pooled / large-sample SE
SE.diff <- sqrt(sigma_NF^2/10 + sigma_F^2/10)
qnorm(0.99)
ME <- qnorm(0.99) * SE.diff
c(diff.bar - ME, diff.bar + ME)
```
""", "images": []}

ex6["6_3a"] = {"title": "Ex 6.3a — CI for mean Salary at 90% (DS)",
"content": """**Question.** Build a 90% CI for the mean `Salary` in `DS`, assuming $\\sigma$ known.

---

**Answer.**
```r
distr.summary.x(Salary, stats="mean", data=DS)
CI.mean(Salary, sigma=38000, conf.level=0.90, data=DS)
```
""", "images": []}

ex6["6_3b1"] = {"title": "Ex 6.3b1 — Count and proportion of female customers (DS)",
"content": """**Question.** Count female customers and compute the sample proportion.

---

**Answer.**
```r
n.females <- sum(DS$Gender=="Female"); n.females
table(DS$Gender)
nrow(DS)
n.females / 750
```
""", "images": []}

ex6["6_3b2"] = {"title": "Ex 6.3b2 — 99% CI for the female proportion",
"content": """**Question.** Build a 99% CI for the proportion of female customers; also compute the required sample size for a smaller margin.

---

**Answer.**
```r
CI.prop(Gender, success="Female", data=DS)
qnorm(0.995)
z_05  <- qnorm(0.95)
(z_05 * 5.0 / 11)^2   # required n
```
""", "images": []}

ex6["6_3c"] = {"title": "Ex 6.3c — CI for proportion difference across countries (Developers_ITA)",
"content": """**Question.** Build a CI for the difference in `Employment` proportions between GER and ITA developers.

---

**Answer.**
```r
distr.table.x(Developers_ITA$Employment, f.digits=3)
phat_GER <- 1 - 0.903
phat_ITA <- 0.15
diff.prop <- phat_GER - phat_ITA
SE.diff   <- sqrt( phat_GER*(1-phat_GER)/820 + phat_ITA*(1-phat_ITA)/802 )
ME <- qnorm(0.995) * SE.diff
c(diff.prop - ME, diff.prop + ME)
```
""", "images": []}

ex6["6_3d"] = {"title": "Ex 6.3d — Subpopulation CI: Married vs Single, Close + no Children",
"content": """**Question.** CI for mean `AmountSpent` between Married and Single, within Close-location no-children customers.

---

**Answer.**
```r
sel.M <- DS$Location=="Close" & DS$Children==0 & DS$Married=="Married"
sel.S <- DS$Location=="Close" & DS$Children==0 & DS$Married=="Single"
CI.diffmean(x=DS$AmountSpent[sel.M], y=DS$AmountSpent[sel.S],
            conf.level=0.99, digits=3)
# Alternative: by= argument
sel.sub <- DS$Location=="Close" & DS$Children==0
CI.diffmean(x=DS$AmountSpent[sel.sub], by=DS$Married[sel.sub],
            conf.level=0.99, digits=3)
```
""", "images": []}

ex6["6_4a"] = {"title": "Ex 6.4a — Pooled-variance CI for vgsales mean (NF vs F)",
"content": """**Question.** Compute the pooled-variance CI for the difference in two-group sample means.

---

**Answer.**
```r
x_bar_F  <- 87.2; x_bar_NF <- xbar  # set as appropriate
diff.bar <- x_bar_NF - x_bar_F
sd_NF <- 5.4; sd_F <- 4.8

# Pooled variance, df = n_NF + n_F - 2 = 18 here
s2_pool <- (9*sd_NF^2 + 9*sd_F^2)/18
se.diff <- sqrt(s2_pool/10 + s2_pool/10)
ME <- qt(0.975, df=18) * se.diff
c(diff.bar - ME, diff.bar + ME)

# (b) Known-variance form, with sigma_NF=5.2, sigma_F=5
SE.diff <- sqrt(5.2^2/10 + 5^2/10)
ME.k <- qnorm(0.975) * SE.diff
c(diff.bar - ME.k, diff.bar + ME.k)
```
""", "images": []}

ex6["6_6a"] = {"title": "Ex 6.6a — CI for a single proportion (point estimate + SE)",
"content": """**Question.** Estimate a proportion from a sample of 100 customers (40 successes).

---

**Answer.**
```r
phat  <- 40/100
var_p <- phat*(1-phat)/100
se    <- sqrt(var_p); se
qnorm(0.975)
ME    <- qnorm(0.975) * se
c(phat - ME, phat + ME)
```
""", "images": []}

ex6["6_6b"] = {"title": "Ex 6.6b — Same calculation with n = 1000",
"content": """**Question.** Same CI but with $n = 1000$ — comment on the precision.

---

**Answer.**
```r
se.p1000  <- sqrt(0.4*0.6/1000)
ME.p1000  <- qnorm(0.975) * se.p1000
c(0.4 - ME.p1000, 0.4 + ME.p1000)
```
""", "images": []}

ex6["6_6c"] = {"title": "Ex 6.6c — Same with 0.95 (one-sided 95% level)",
"content": """**Question.** Construct a CI at confidence level 0.95 (one-sided).

---

**Answer.**
```r
qnorm(0.95)
ME_95 <- qnorm(0.95) * se
c(0.4 - ME_95, 0.4 + ME_95)
```
""", "images": []}

ex6["6_6d"] = {"title": "Ex 6.6d — Sample size required for ME ≤ 0.04",
"content": """**Question.** What sample size gives margin-of-error 0.04 at 95% confidence?

---

**Answer.**
```r
(qnorm(0.975) * 0.5 / 0.04)^2
```
""", "images": []}

ex6["6_6e"] = {"title": "Ex 6.6e — CI for difference of proportions",
"content": """**Question.** CI for $\\hat p_A - \\hat p_B$ with $p_A=0.4, p_B=0.36, n_A=100, n_B=120$.

---

**Answer.**
```r
p_A <- 0.4; p_B <- 0.36
diff <- p_A - p_B
se.diff <- sqrt(p_A*(1-p_A)/100 + p_B*(1-p_B)/120)
ME <- qnorm(0.995) * se.diff
c(diff - ME, diff + ME)
```
""", "images": []}

ex6["6_7a"] = {"title": "Ex 6.7a — CI for proportion of DS platform games (vgsales)",
"content": """**Question.** Build a 90% CI for the proportion of DS-platform games.

---

**Answer.**
```r
CI.prop(Platform, success="DS", conf.level=0.9, digits=4, data=vgsales)
qnorm(0.95)
qnorm(0.995)
# Tail probability of z = 0.0428/0.0166:
pnorm(0.0428/0.0166)
```
""", "images": []}

ex6["6_8a1"] = {"title": "Ex 6.8 a1 — 99% CI for mean Skills.Idx (Developers_ITA)",
"content": """**Question.** Build a 99% CI for the mean `Skills.Idx` and `NrSkills` in `Developers_ITA`.

---

**Answer.**
```r
CI.mean(Skills.Idx, conf.level=0.99, data=Developers_ITA)
CI.mean(NrSkills,   conf.level=0.99, data=Developers_ITA)
```
""", "images": []}

ex6["6_8a2"] = {"title": "Ex 6.8 a2 — Pooled-variance CI: NrSkills GER vs ITA",
"content": """**Question.** Compare `NrSkills` between GER and ITA developers.

---

**Answer.**
```r
distr.summary.x(NrSkills, data=Developers_ITA)
mean.GER <- 17.6; s.GER <- 10.12
mean.ITA <- 19.22; s.ITA <- 11.33
diff.bar <- mean.ITA - mean.GER
# Equal variances
s2_pool <- (801*11.33^2 + 819*10.12^2) / (801 + 819)
se.diff <- sqrt(s2_pool/802 + s2_pool/820)
ME <- qnorm(0.995) * se.diff
c(diff.bar - ME, diff.bar + ME)
```
""", "images": []}

ex6["6_8b"] = {"title": "Ex 6.8b — Welch's SE (unequal variances)",
"content": """**Question.** Same comparison without assuming equal variances.

---

**Answer.**
```r
# Different variances (Welch)
se.diff <- sqrt(4.43^2/802 + 4.13^2/820)
ME <- qnorm(0.995) * se.diff
c(diff.bar - ME, diff.bar + ME)
```
""", "images": []}

ex6["6_8c1"] = {"title": "Ex 6.8 c1 — Paired CI for FinSkills vs Skills",
"content": """**Question.** CI for the mean difference between `FinSkills.Idx` and `Skills.Idx` (paired).

---

**Answer.**
```r
CI.diffmean(x=FinSkills.Idx, y=Skills.Idx, type="paired",
            conf.level=0.9, data=Developers_ITA)
```
""", "images": []}

ex6["6_8d"] = {"title": "Ex 6.8d — CI for Skills mean: Full-time vs Freelance",
"content": """**Question.** CI for the difference in `NrSkills` mean between Employment categories.

---

**Answer.**
```r
sel <- Developers_ITA$Employment=="Employed, full-time"
Skills.Full      <- Developers_ITA$NrSkills[sel]
Skills.Freelance <- Developers_ITA$NrSkills[Developers_ITA$Employment=="Contractor/Freelance"]
CI.diffmean(x=Skills.Full, y=Skills.Freelance)
```
""", "images": []}

ex6["6_9a"] = {"title": "Ex 6.9a — CI for proportion difference (vgsales action genre)",
"content": """**Question.** Build a 99% CI for the difference in two-sample proportions.

---

**Answer.**
```r
p_X_hat <- 23/140
p_Y_hat <- 37/159
s2_Px <- p_X_hat*(1-p_X_hat)/140
s2_Py <- p_Y_hat*(1-p_Y_hat)/159
qnorm(.995)
ME <- qnorm(.995) * sqrt(s2_Px + s2_Py)
c(p_Y_hat - p_X_hat - ME, p_Y_hat - p_X_hat + ME)
```
""", "images": []}

ex6["6_10a"] = {"title": "Ex 6.10a — CI for diff in JP sales: Strategy vs Role-Playing",
"content": """**Question.** Build a 90% CI for the JP-sales mean difference between Strategy and Role-Playing genres.

---

**Answer.**
```r
CI.diffmean(vgsales$JP_Sales[vgsales$Genre=="Strategy"],
            vgsales$JP_Sales[vgsales$Genre=="Role-Playing"],
            type="independent", conf.level=0.90, digits=4)
```
""", "images": []}

ex6["6_11a"] = {"title": "Ex 6.11a — Paired t-CI with $n=25$, given correlation",
"content": """**Question.** Build a CI for paired-data mean difference using $\\bar x_A = 85, \\bar x_B = 81.4, s_A = 5.9, s_B = 4.8, \\rho_{AB} = 0.6, n = 25$.

---

**Answer.**
```r
xbar_A <- 85; xbar_B <- 81.4
diff.BA <- xbar_B - xbar_A
s_A <- 5.9; s_B <- 4.8; r_AB <- 0.6
s2_D <- s_B^2 + s_A^2 - 2*r_AB*s_B*s_A
se_D <- sqrt(s2_D/25)
ME <- qt(0.95, df=24)*se_D
c(diff.BA - ME, diff.BA + ME)
```
""", "images": []}

ex6["6_12a"] = {"title": "Ex 6.12a — CI for Adventure-game high-sales proportion",
"content": """**Question.** Build a 99% CI for the proportion of Adventure-genre games with Global_Sales > 1.

---

**Answer.**
```r
highsales.adv <- vgsales$Global_Sales[vgsales$Genre=="Adventure"] > 1
CI.prop(highsales.adv, conf.level=0.99, digits=4)
```
""", "images": []}

ex6["6_12b"] = {"title": "Ex 6.12b — Difference: Shooter vs Racing high-sales",
"content": """**Question.** CI for the difference in proportions between Shooter and Racing genres.

---

**Answer.**
```r
qnorm(0.995); ME <- qnorm(0.995)*0.005
highsales.racing  <- vgsales$Global_Sales[vgsales$Genre=="Racing"]  > 1
highsales.shooter <- vgsales$Global_Sales[vgsales$Genre=="Shooter"] > 1
CI.diffprop(x=highsales.shooter, y=highsales.racing,
            conf.level=0.999, digits=4)
```
""", "images": []}

ex6["6_13a"] = {"title": "Ex 6.13a — CI for proportion 108/140",
"content": """**Question.** Build a 99% CI for the proportion `108/140`.

---

**Answer.**
```r
p_hat <- 108/140
ME <- qnorm(0.995)*sqrt(p_hat*(1-p_hat)/140)
c(p_hat - ME, p_hat + ME)
```
""", "images": []}

ex6["6_13d"] = {"title": "Ex 6.13d — CI for mean from grouped data (rep-style)",
"content": """**Question.** Compute the sample mean, variance and CI for a grouped distribution.

---

**Answer.**
```r
xbar    <- (43+2*14+3*10+4*13+5*8+5)/140
mean.x2 <- (43+4*14+9*10+16*13+25*8+25*13+36*5+64*5)/140
s2_x    <- (140/139)*(mean.x2 - xbar^2)
se_Xbar <- sqrt(s2_x/140)
c(xbar - qnorm(0.95)*se_Xbar, xbar + qnorm(0.95)*se_Xbar)
```
""", "images": []}

ex6["6_14a"] = {"title": "Ex 6.14a — CI for diff in proportions: EA vs Activision",
"content": """**Question.** CI for the difference in proportions of best-selling games between Electronic Arts and Activision.

---

**Answer.**
```r
EA_best_seller  <- vgsales$NA_Sales[vgsales$Publisher=="Electronic Arts"] > 1
ACT_best_seller <- vgsales$NA_Sales[vgsales$Publisher=="Activision"]      > 1
sum(EA_best_seller); sum(ACT_best_seller)
distr.summary.x(EA_best_seller, digits=4)
distr.summary.x(ACT_best_seller, digits=4)
CI.diffprop(EA_best_seller, ACT_best_seller, conf.level=0.9, digits=4)
```
""", "images": []}

ex6["6_15a"] = {"title": "Ex 6.15a — Mean difference from supplied summary stats",
"content": """**Question.** $\\bar x = 39.34, \\bar y = 49.71$ — compute difference.

---

**Answer.**
```r
xbar <- 39.34; ybar <- 49.71
diff.bar <- xbar - ybar; diff.bar
```
""", "images": []}

ex6["6_15b"] = {"title": "Ex 6.15b — Pooled vs separate t-CI",
"content": """**Question.** CI for the difference with $s^2_x=113.93, n_x=20, s^2_y=129.55, n_y=28$.

---

**Answer.**
```r
s2_x <- 113.93; n_x <- 20
s2_y <- 129.55; n_y <- 28
se_unequal <- sqrt(s2_x/n_x + s2_y/n_y); se_unequal
s2_pool    <- ((n_x-1)*s2_x + (n_y-1)*s2_y) / (n_x + n_y - 2)
se_equal   <- sqrt(s2_pool/n_x + s2_pool/n_y)
qt(0.975, 46)
c(diff.bar - qt(0.975, 46)*se_equal,
  diff.bar + qt(0.975, 46)*se_equal)
```
""", "images": []}

ex6["6_17a"] = {"title": "Ex 6.17a — Paired difference 56.8 vs 46.7",
"content": """**Question.** Build a 99% CI for the paired mean difference.

---

**Answer.**
```r
dbar <- 56.8 - 46.7
se_D <- sqrt((6.1^2 + 6.9^2 - 2*34.6)/23)
qt(0.95, 22)
c(dbar - qt(.995, 22)*se_D, dbar + qt(.995, 22)*se_D)
```
""", "images": []}

ex6["6_18b"] = {"title": "Ex 6.18b — Paired 98% CI: NA vs EU sales (Action)",
"content": """**Question.** 98% paired CI for NA vs EU sales (Action genre); compute correlation.

---

**Answer.**
```r
CI.diffmean(vgsales$NA_Sales[vgsales$Genre=="Action"],
            vgsales$EU_Sales[vgsales$Genre=="Action"],
            type="paired", conf.level=0.98, digits=4)
cor(vgsales$NA_Sales[vgsales$Genre=="Action"],
    vgsales$EU_Sales[vgsales$Genre=="Action"])
```
""", "images": []}
