/* Group number: 20*/
/* Group composition: Federico Cosenza, Simone Donoghue and Alessandro Caggia */

clear all

local user = c(username)
display "`user'"


if ("`user'" == "erick") {
    global filepath "/home/erick/TEMP/"
}

if ("`user'" == "utente") {
    global filepath "C:\Users\utente\OneDrive - Università Commerciale Luigi Bocconi\Desktop\PS II instructions\PS II data"
}

if ("`user'" == "simon") {
    global filepath "C:\Users\simon\OneDrive\Desktop\micrometrix_ps_2"
}

global output "$filepath\output"

/*
ssc install gtools, replace
ssc install bacondecomp, replace
ssc install coefplot, replace
ssc install avar, replace
ssc install eventstudyinteract
ssc install reghdfe, replace
ssc install ftools, replace
ssc install twowayfeweights
ssc install gtools, replace

*/

*** Exercise 1 ***

/* Question A 

For the purpose of our analysis, the appropriate type of weighting procedure we should use is one with analytic weights. Indeed, analytic weights consider observations as means computed from a sample of size n, with n being the weight variable. In our case, divorce rates are the number of divorces in a given state over the population of the state. By using analytic weights, we tell Stata to consider each observation (div_rate) as a mean computed over the state population (stpop), which thus serves as the weighting variable.

To provide an intuition, an effect of unilateral divorce laws will carry more weight in states with a higher population (e.g., California) compared to those with a lower one (e.g., New Hampshire).

*/



/* Question B */

/* (i) */

clear all
use "$filepath\pset_4.dta", replace

preserve

gen reform = 0
replace reform = 1 if lfdivlaw >= 1968 & lfdivlaw <= 1988

gen div_rate_rfrm = div_rate if reform == 1
gen div_rate_ctrl = div_rate if reform == 0

collapse div_rate_rfrm div_rate_ctrl [aw=stpop], by (year)
gen div_rate_diff = div_rate_rfrm - div_rate_ctrl

twoway (line div_rate_rfrm year, lcolor(black) lwidth(thick)) (line div_rate_ctrl year, lcolor(gray) lwidth(thick)) (line div_rate_diff year, lcolor(black) lp(dash)), legend(position(12) size(small) label(1 "Reform states") label(2 "Control states") label(3 "Difference in divorce rates")) title("Average divorce rate: reform states and controls") xtitle("Year") ytitle("Divorce Rate" "Divorces per 1,000 persons per year") xlabel(, grid) ylabel(0(1)7) xline(1968 1988, lcolor(blue red) lpattern(dash solid))

graph export "$output\graph_1b_i.jpg", replace
restore

/* (ii) */

preserve

gen reform = 1 if lfdivlaw >= 1969 & lfdivlaw <= 1973
replace reform = 2 if lfdivlaw == 2000

gen div_rate_rfrm = div_rate if reform == 1
gen div_rate_ctrl2 = div_rate if reform == 2

collapse div_rate_rfrm div_rate_ctrl2 [aw=stpop], by (year)
gen div_rate_diff = div_rate_rfrm - div_rate_ctrl2

twoway (line div_rate_rfrm year if year <= 1978, lcolor(black) lwidth(thick)) (line div_rate_ctrl2 year if year <= 1978, lcolor(gray) lwidth(thick)) (line div_rate_diff year if year <= 1978, lcolor(black) lp(dash)), legend(label(1 "Reform states") label(2 "Control states") label(3 "Difference in divorce rates")) title("Average divorce rate: reform states and controls") xtitle("Year") ytitle("Divorce Rate") xlabel(, grid) ylabel(0(1)7) xline(1968.5, lcolor(black) lpattern(solid)) xscale(range(1956 1978))

graph export "$output\graph_1b_ii.jpg", replace

restore

/* In both graphs, the parallel trends assumption seems to hold visually. We do not observe any considerable trend difference between reform and control states. If anything, we observe a mild increase in such difference in the period prior to the law introduction, but the magnitude doesn't seem to be significant. */


/* Question C */

use "$filepath\pset_4.dta", replace

keep if year == 1968 | year == 1978
keep if (lfdivlaw >= 1968 & lfdivlaw <= 1973) | (lfdivlaw == 2000) 

gen UNILATERAL = 0
replace UNILATERAL = 1 if lfdivlaw >= 1969 & lfdivlaw <= 1973

gen POST = 0
replace POST = 1 if year == 1978

gen POST_UNILATERAL = 0
replace POST_UNILATERAL = UNILATERAL*POST

* (i) *

reg div_rate POST_UNILATERAL POST [aweight = stpop], vce(robust)
outreg2 using "$output\table_1c.xls", replace

* (ii) *

reg div_rate POST UNILATERAL POST_UNILATERAL [aweight = stpop], vce(robust)
outreg2 using "$output\table_1c.xls", append


/* In model (i), the effect of introducing unilateral divorce law on the divorce rate is positive (β = 1.701) and statistically significant (p < 0.001). However, in model (ii), when controlling for state fixed effects, the effect becomes inexistent and loses statistical significance (p = 0.993). 

In model (ii), the difference in divorce rates between the reform and control groups is absorbed by the state fixed effects, suggesting that the treatment effect estimated in model (i) was driven by pre-existing differences between the reform and control states. This is consistent with the graphs seen in point (b), where it can be noted that reform states have, on average, higher divorce rates compared to control states in any period.

So, according to the latter model, there would be no significant effects of introducing the unilateral divorce law on divorce rates.
*/


/* Question D */

matrix M = J(3, 3, .)
matrix rownames M = "POST=1" "POST=0" "Difference 1"
matrix colnames M = "UNILATERAL=1" "UNILATERAL=0" "Difference 2"

sum div_rate [aw=stpop] if UNILATERAL == 1 & POST == 1
matrix M[1, 1] = r(mean)

sum div_rate [aw=stpop] if UNILATERAL == 1 & POST == 0
matrix M[2, 1] = r(mean)

sum div_rate [aw=stpop] if UNILATERAL == 0 & POST == 1
matrix M[1, 2] = r(mean)

sum div_rate [aw=stpop] if UNILATERAL == 0 & POST == 0
matrix M[2, 2] = r(mean)

matrix list M

matrix M[1,3] = M[1,1]-M[1,2]
matrix M[2,3] = M[2,1]-M[2,2]
matrix M[3,1] = M[1,1]-M[2,1]
matrix M[3,2] = M[1,2]-M[2,2]
matrix M[3,3] = M[1,3]-M[2,3]
matrix list M

putexcel set "$output\table_1.xlsx", replace
putexcel A1 = "" B1 = "UNILATERAL=1" C1 = "UNILATERAL=0" D1 = "Difference 2", bold
putexcel A2 = "POST=1", bold 
putexcel B2 = matrix(M[1,1]) C2 = matrix(M[1,2]) D2 = matrix(M[1,3])
putexcel A3 = "POST=0", bold
putexcel B3 = matrix(M[2,1]) C3 = matrix(M[2,2]) D3 = matrix(M[2,3])
putexcel A4 = "Difference 1", bold
putexcel B4 = matrix(M[3,1]) C4 = matrix(M[3,2]) D4 = matrix(M[3,3])


/* Question E */
use "$filepath\pset_4.dta", replace

keep if year >= 1956 & year <= 1988

encode st, gen(state)
xtset state year

gen IMP_UNILATERAL= 0
replace IMP_UNILATERAL=1 if lfdivlaw<= year

gen t = year - 1956
gen t2 = t^2
eststo clear

* Regression 1
reg div_rate IMP_UNILATERAL i.year i.state [aweight=stpop]
estadd scalar linear_trend = 0, replace
estadd scalar quadratic_trend = 0, replace
estadd scalar Adjusted_R2 = e(r2_a)
eststo m1

* Regression 2
reg div_rate IMP_UNILATERAL i.year i.state c.t#i.state [aweight=stpop]
estadd scalar linear_trend = 1, replace
estadd scalar quadratic_trend = 0, replace
estadd scalar Adjusted_R2 = e(r2_a)
eststo m2

* Regression 3
reg div_rate IMP_UNILATERAL i.year i.state c.t#i.state c.t2#i.state [aweight=stpop]
estadd scalar linear_trend = 1, replace
estadd scalar quadratic_trend = 1, replace
estadd scalar Adjusted_R2 = e(r2_a)
eststo m3
	 
esttab m1 m2 m3 using "$output\table_1e.csv", replace keep(IMP_UNILATERAL) b(%9.3f) se star(* 0.10 ** 0.05 *** 0.01) scalars("Adjusted_R2") coeflabels("Unilateral") mti("No Trends" "Linear" "Quadratic")
	
/* In the first column, we have the basic specification (without state-specific trends). Similarly to model (ii) in point (c), there is no significant effect of the introduction of unilateral law on divorce rates (β = -0.055, p = 0.276). In the second column, when we add state-specific linear trends, we get a positive and statistically significant result for the effect of the law (β = 0.477, p < 0.001). In the third column, with the addition of quadratic trends the effect still remains positive (β = 0.334, p < 0.001). This third model serves as a robustness check for the second one, confirming the need to also control for time trends in the analysis.

One reason for the results to change when adding the state-specific time trends may be that the reform and control states follow different trends after the introduction of the law. In particular, one would be tempted to say that, since the effect of the law changes from being null to being positive, the law was being introduced in states whose trend in divorces was decreasing more compared to other states. However, this is not consistent with the graph in (b), where we see a pre-trend that is actually slightly increasing more for reform states compared to control ones.

The results would have been the same under the Parallel Trends Assumption, as accounting for time trends wouldn't have captured any differences between reform and control trends.

A relevant role in this analysis is also played by the evolution of the treatment effect over time but we will see it better in point (k), since it represents the main critique made by Wolfers on Friedberg's findings.
 
*/


/* QUESTION F */

* Creates simulated observations

clear
set obs 6
gen obs = _n
gen state = floor(.9 + obs/3)
bysort state: gen year = _n
gen D = state == 1 & year == 3
replace D = 1 if state == 2 & (year == 2 | year == 3)

* Creates simulated outcomes
gen Y = 0.1 + 0.02 * (year == 2) + 0.05 * (D == 1) + uniform() / 100
gen Y2 = 0.1 + 0.02 * (year == 2) + 0.05 * (D == 1) + 0.3 * (state == 2 & year == 3) + uniform() / 100
gen Y3 = 0.1 + 0.02 * (year == 2) + 0.05 * (D == 1) + 0.4 * (state == 2 & year == 3) + uniform() / 100
gen Y4 = 0.1 + 0.02 * (year == 2) + 0.05 * (D == 1) + 0.5 * (state == 2 & year == 3) + uniform() / 100

reg Y D i.state i.year 

reg Y2 D i.state i.year 

reg Y3 D i.state i.year 

reg Y4 D i.state i.year 

/* 
In the first case, the treatment coefficient is consistently estimated and is close to 0.05, the true ATE (any negligible slight variation is only due to the random shock). However, in the cases of Y2, Y3, and Y4, we obtain negative coefficients that diverge from the real positive treatment effect. Indeed, the treatment effect when using Y2, Y3, and Y4 is non-constant over time, and thus, it is not possible to estimate the treatment coefficient consistently.

*/

* (g) 

twowayfeweights Y state year D, type(feS) 
twowayfeweights Y2 state year D, type(feTR)
twowayfeweights Y3 state year D, type(feTR)
twowayfeweights Y4 state year D, type(feTR)


/* The reason why the estimated effect changes is linked to the fact that the coefficient of this regression is a weighted sum of all the cross-group treatment effects. If the treatment effect is not constant over time, the weights of this sum might be negative, potentially yielding a negative estimate of the treatment effect even though all the ATEs are positive.

In our case, there is always one negative weight when there are non-constant treatment effects over time. When using Y as the outcome variable, the treatment effect is constant over time, and thus all weights are positive. When using Y2, Y3, or Y4, however, the treatment effect is not constant, and weights can be negative (and indeed are), reducing the magnitude of the originally positive coefficient. We also note that as we shift from Y2 to Y4, the coefficient becomes increasingly negative. This is consistent with the fact that, from Y2 to Y4, the treatment effect of state 2 is increasingly positively correlated with time, leading the regression to be more influenced by the negative weight.
*/


/* Question H */
use "$filepath\pset_4.dta", replace

keep if year >= 1956 & year <= 1988

encode st, gen(state)
xtset state year

gen IMP_UNILATERAL= 0
replace IMP_UNILATERAL=1 if lfdivlaw<= year

gen init_stpop=.
bysort st (year): replace init_stpop = stpop if year == 1956
bysort st (year): replace init_stpop = init_stpop[_n-1] if missing(init_stpop)

reg div_rate IMP_UNILATERAL i.year i.state [aweight=init_stpop]

bacondecomp div_rate IMP_UNILATERAL [aweight=init_stpop]
graph export "$output\graph_1h.jpg", replace

/* 
In the context of staggered adoption DiDs, Goodman-Bacon provides a way to break down the TWFE estimator for treatment effects. The idea behind his analysis is that the TWFE estimator can be shown to be a weighted average of all the 2x2 DD estimates across all timing groups. The major issue in this case is that when already-treated units act as controls, it is possible that some of the 2x2 DD estimates are driven by changes in the treatment effect over time of the already-treated units in the control group, and thus are biased. Practically, this translates into negative weights attached to the estimation of this kind of cross-group treatment effect. Therefore, when treatment effects are not constant over time, the TWFE estimate is biased.

Bacon proposes a way to decompose the TWFE estimate by identifying all possible 2x2 DDs and their weights in the overall treatment effect. This decomposition allows for differentiating the 2x2 DDs based on which group comparison they come from:

1) Timing groups comparison: these are DDs coming from the comparison of earlier-to later-treated groups, or vice versa, and are those that might suffer from the bias explained above and generate the issue of negative weights.

2) Timing vs. never-treated groups: these are DDs coming from the comparison between treated groups and never-treated ones (pure, unbiased DDs).

3) Timing vs. always-treated groups: these are DDs coming from the comparison between treated groups and always-treated ones.

In this way, one can observe how much of the effect comes from the timing DDs. A large share of the "Timing groups" comparison is an indication of potentially negative weights and biased results when the treatment effect is non-constant over time.

We note that "Timing groups" account for only 9% of the estimate, suggesting that there is not a major issue of negative weights in our TWFE estimate. However, we do notice that the average DD estimate across timing groups is 0.53, particularly higher compared to the estimates of the other two groups (-0.14 and -0.08). The reverse sign suggests the presence, though of low impact, of non-constant treatment effects and consequent negative weights (that make a negative estimate positive).

*/




/* Question I */
use "$filepath\pset_4.dta", replace

encode st, gen(state)
xtset state year

keep if year >= 1956 & year <= 1988

gen event_time = year - lfdivlaw if lfdivlaw != .

* Collapse tails
gen ev_m10 = event_time <= -10

* Generate event_time dummies from -9 to 14, excluding -1
foreach i in -9 -8 -7 -6 -5 -4 -3 -2 {
    gen ev_m`=abs(`i')' = (event_time == `i')
}
gen ev_p0 = (event_time == 0)

foreach i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 {
    gen ev_p`i' = (event_time == `i')
}

gen ev_p15 = event_time >= 15

forvalues k = 10(-1)2 {
    label var ev_m`k' "-`k'"
}

forvalues k = 0(1)15 {
    label var ev_p`k' "+`k'"
}

gen t = year - 1956
gen t2 = t^2

*Column 1 – Event-study regression (no trends)
reg div_rate ev_m10-ev_m2 ev_p0-ev_p15 i.state i.year [aweight=stpop]
estimates store col1

*Column 2 – Event-study regression with state-specific linear trends
reg div_rate ev_m10-ev_m2 ev_p0-ev_p15 i.state i.year c.t#i.state [aweight=stpop]
estimates store col2

*Column 3 – Event-study regression with state-specific linear and quadratic trends
reg div_rate ev_m10-ev_m2 ev_p0-ev_p15 i.state i.year c.t#i.state c.t2#i.state [aweight=stpop]
estimates store col3

esttab col1 col2 col3 using "$output\table_1i.csv", keep(ev_*) b(2) se(2) star(* 0.1 ** 0.05 *** 0.01) title("Table 2 — Dynamic Effects of Unilateral Divorce Laws") mtitles("No trends" "Linear" "Quadratic") stats(r2_a N, fmt(3 0) labels("Adjusted R^2" "Sample Size")) replace


/*
First model: Before the introduction of the law, the differences in divorce rates between the treatment and control groups seem negligible and do not show a clear pattern, as already suggested by the graph in point B. This may suggest that the parallel trends assumption holds. After the introduction of the law, the treatment effect is positive in the short run, while it decreases in the long run, becoming even negative after a decade.

Second model: The results are similar to those of model 1 regarding the coefficients of the leads. What changes is that, after the introduction of the law, the treatment effect seems stronger and more persistent for the first 8 years, declining to zero after a decade (with non-significant coefficients).

Third model: The results are still consistent with the previous models regarding the leads. The difference is that the treatment effect after the introduction of the law seems to be positive only for the first three years, then shrinks to zero (all non-significant coefficients).

In this analysis, we can see the entire response function of the divorce rate to the unilateral divorce law, observing how the difference between treated and control states evolves in the years before and after the introduction of the law. This allows the state-specific time trends to isolate only the pre-existing trends and not the evolution of the treatment effect over time, as pointed out by Wolfers when commenting on the results of Friedberg.

*/



/* Question J */

* Step 14: Reproduce Figure 3 (Dynamic Response Plot)
estimates store dyn

* Plot dynamic response with event-time dummies
coefplot col1 col2 col3, keep(ev_*) drop(_cons) vertical connect(l l l) plotlabels("No trends" "Linear" "Quadratic") yline(0, lpattern(dash)) xtitle("Years relative to introduction of law") ytitle("Effect on Divorce Rate") title("Dynamic Response to Reform: All Table 2 Specifications") 

graph export "$output\graph_1j.jpg", replace



/* Question K */

/*

In her analysis, Friedberg concludes that divorce rates in 1988 (that is, 20 years after the introducton of the law) would have been 6 percent lower if no unilateral law had been adopted in the states that actually adopted it in 1968. She also concludes that the law adoption accounts for 17% of the increase in divorce rates between 1968 and 1988, meaning that the reseffects are positive and also persistent over time. However, Wolfers argues that the dynamics were more complex and interesting. He finds that the law first led to an immediate spike in the divorce rate, which then dissipated and even declined compared to initial levels (though this latter result is less robust and more uncertain).

Econometric rationale behind the observed differences in results: According to Wolfers, the positive coefficient found by Friedberg is affected by the fact that, in her specification, state linear time trends pick up not only different pre-existing trends, but also the evolution of the treatment effect over time (which turns out to be decreasing). The coefficient estimated by Friedberg is thus partialling out the subsequent reduction in the treatment effect, resulting in an overestimation of the coefficient that is interpreted as an effect persisting over a 20-year span. Friedberg's findings are also not consistent with what is observed in the graph when testing for the parallel trends assumption (point B). By contrast, Wolfers manages to separate the pre-existing trends from the evolution of the treatment effect over time by including leads and lags and extending the time span of the dataset. In this case, the state trends purely represent pre-existing trends.

Theoretical rationale: from a more theoretical point of view, Wolfers provides an intuition for the dynamic response to the change in divorce regimes. Indeed, immediately following the reform, the spike in divorce rates may be due to pent-up demand for the new type of divorce that courts have to deal with. Further adjustments and the slow diffusion of information may keep the divorce rates high for a few years following the regime shift. In addition, Wolfers argues that the shift to a new steady state may not be immediate also because many couples may not consider the implications of a new regime for a while, sometimes even a few years, allowing for additional persistence in the high divorce rates during the first years. All of these reasons can contribute to explaining the adjustment path of the divorce rates following the introduction of the new laws.

*/



/* Question L */

*Creates dummy for people who never entered the union
gen never_divlaw = 0
replace never_divlaw = 1 if lfdivlaw > 1988

eventstudyinteract div_rate ev_* [aweight=stpop], cohort(lfdivlaw) control_cohort(never_divlaw) absorb(i.state i.year) vce(cluster state)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
coefplot matrix(C[1]), se(C[2]) keep(ev_*) vertical yline(0) xtitle("Years relative to introduction of law") ytitle("Estimated effect") title("Question 1L (No trends)") xlabel(, angle(45) alternate)

graph export "$output\graph_1l_1.jpg", replace


eventstudyinteract div_rate ev_* [aweight=stpop], cohort(lfdivlaw) control_cohort(never_divlaw) absorb(i.state i.year) covariates(c.t#i.state) vce(cluster state)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
coefplot matrix(C[1]), se(C[2]) keep(ev_*) vertical yline(0) xtitle("Years relative to introduction of law") ytitle("Estimated effect") title("Question 1L (Linear trends)") xlabel(, angle(45) alternate)

graph export "$output\graph_1l_2.jpg", replace


eventstudyinteract div_rate ev_* [aweight=stpop], cohort(lfdivlaw) control_cohort(never_divlaw) absorb(i.state i.year c.t#i.state c.t2#i.state) vce(cluster state)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
coefplot matrix(C[1]), se(C[2]) keep(ev_*) vertical yline(0) xtitle("Years relative to introduction of law") ytitle("Estimated effect") title("Question 1L (Quadratic trends)") xlabel(, angle(45) alternate)

graph export "$output\graph_1l_3.jpg", replace

/* We analyze the three models in comparison to the event study presented in the original paper by Wolfers (2006).

*First model: The treatment effect is positive only for the first two years, immediately approaching zero and then becoming negative after a decade. In Wolfers instead the treatment effect was positive and persistent for the first 8 years while becoming negative after a decade

*Second model: The treatment effect is positive only for the first two years while being not significant afterward. In wolfers the treatment effect was positive for the first 8 year then becoming not significant

*Third model: The treatment effect is not significant in the first years after the introduction of the law while becoming significantly negative after 5 years. These results completely differ from those of Wolfers where the treatment effect is positive for the first 8 years while becoming not significant afterward. However, results using quadratic trends and in particular negative effects in the long run should be interpreted cautiously 

This analysis suggests that the spike in divorce rates after the introduction of the law may have been actually shorter (just 2-3 years) compared to the one identified by Wolfers (approximately 8 years).

* Sun and Abraham show that the coefficients of each lead and lag under the parallel trends assumption and no anticipatory behaviour are a weighted average of cohort ATT (CATT) of the correspondent relative period + two sources of contamination: (1) the weighted average of CATT from other relative periods included in the specification and (2) the weighted average of CATT from relative periods excluded from the specification. Both sources are characterized by potential negative weights, that sum up to 0 in the first source and to -1 in the second source. Only under the strong assumption of homogenous treatment effects across different cohorts the first sources of contamination disappear, while the second remains. Practically, this translates into a coefficient that corresponds to a weighted average of cohort ATT (CATT) with potentially negative weights. Sun and Abraham find a way to make it a weighted average with proper weights (i.e. a convex linear combination) that represent the share of cohorts in each relative time period. 

*Particularly, the algorithm works in three steps:

*1) It runs a saturated TWFE regression with all possible interactions between relative time periods indicators and cohort indicators to estimate the CATT for each cohort

*2) It then estimates the weights simply as the sample share of each cohort in the relative time period

*3) It calculates the weighted average of the estimates of the CATT using weights of step 2

* The benefit of this estimator is that it is consistent under the assumptions of parallel trends and no anticipation of the treatment effect, yet it allows for heterogeneity in treatment effects across different cohorts and relative periods.


*/


























