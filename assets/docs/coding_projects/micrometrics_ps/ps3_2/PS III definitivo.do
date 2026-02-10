/* Group number: 20*/
/* Group composition: Federico Cosenza, Alessandro Caggia and Simone Donoghue */

/* Gets user name */
local user = c(username)
display "`user'"

/* Stores filepath conditionally */
if ("`user'" == "erick") {
    global filepath "/home/erick/TEMP/"
}
*

/*
ssc install rdrobust, replace
ssc install rddensity, replace
ssc install lpdensity, replace
*/ 

clear all
use "/Users/Alessandro/Desktop/ps3/stata_files/pset_3.dta", replace

describe




*** EXERCISE 1

************************************************************
* (a) Generate RD Plot of T (Islamic mayor 1994) against X (Islamic Vote Margin 1994)
************************************************************


rdplot T X, graph_options(title("Islamic Mayor in 1994 vs Islamic Vote Margin in 1994") ytitle("Treatment Variable: Islamic Mayor in 1994") xtitle("Running Variable: Islamic Vote Margin in 1994")legend(off))


* The rdpot allows us to observe a sharp discontinuiy in the treatment at the cutoff, hence, this is the sharp RDD framework. The treatment status is a deterministic function of the running variable, and such function is discontinuous at the cutoff. The sharp RDD framework can be used when the treatment assignment mi (islamic mayor in 1994) is determined on the basis of a cutoff score, c, on an observed forcing vairvale xi. In our case: i) the forcing variable is the win margin for the Islamic party relative to the largest non-Islamic party, and ii) the cutoff is therefore c = 0. The municipalities below the cutoff (mi = 0) are the control group, those above the cutoff (the ones receiving an islamic mayor), are the treatment group (mi = 1). The assignment follows a known deterministic rule, mi = 1{xi ≥ c}, where 1{·} is the indicator function.



************************************************************
* (b) Create macro for covariates and build Table 1 with RD estimates
************************************************************

* Define covariates and their corresponding labels for Table 1 (Balance Table)
putexcel set "/Users/Alessandro/Desktop/ps3/output/Table_1.xlsx", replace
putexcel A1 = ("Label") B1 = ("MSE-Optimal Bandwidth") C1 = ("RD Estimator") D1 = ("p-value") E1 = ("Effective Number of Observations")

local covariates hischshr1520m i89 vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk

local hischshr1520m "Share Men aged 15-20 with High School Education"
local i89 "Islamic Mayor in 1989"
local vshr_islam1994 "Islamic vote share 1994"
local partycount "Number of parties receiving votes 1994"
local lpop1994 "Log Population in 1994"
local merkezi "District center"
local merkezp "Province center"
local subbuyuk "Sub-metro center"
local buyuk "Metro center"

local row = 2
foreach z in `covariates' {
    quietly rdrobust `z' X, kernel(triangular) p(1) bwselect(mserd)
    putexcel A`row' = ("``z''") B`row' = (round(e(h_l), .001)) C`row' = (round(e(tau_cl), .001)) D`row' = (round(e(pv_cl), .001)) E`row' = (round(e(N_h_r)+e(N_h_l), .001))
    local ++row
}


* Note: Conventional standard errors from rdrobust are used, as requested by the problem set.
* I am using rdrobust `z' X, kernel(triangular) p(1) bwselect(mserd) because this is required by the problem set 



************************************************************
* (c) Generate RD plots for each covariate and combine into Graph 1
************************************************************


use "/Users/Alessandro/Desktop/ps3/stata_files/pset_3.dta", replace

* Loop with expanded labels in titles 
foreach z in `covariates' {
    rdplot `z' X, graph_options(title("``z'' discontinuity", size(small))    xtitle("Islamic Vote Margin in 1994")  legend(off))
    graph rename `z'_X, replace
}


* Combine and export
graph combine hischshr1520m_X i89_X vshr_islam1994_X partycount_X lpop1994_X merkezi_X merkezp_X subbuyuk_X buyuk_X
graph export "/Users/Alessandro/Desktop/ps3/output/Graph_1.pdf", replace



************************************************************
* (d) Create histogram plots and density plot, then combine into Graph 2
************************************************************


rdrobust Y X, kernel(triangular) p(1) bwselect(mserd)
scalar h_left = -e(h_l)
scalar h_right = e(h_r)



twoway (histogram X if X >=h_left & X < 0, freq width(1) color(red)) (histogram X if X >= 0 & X <= h_right, freq width(1) color("0 60 170")), xlabel(-30(10)30) xline(0, lpattern(dash) lcolor(black) lwidth(medium)) graphregion(color(white)) xtitle(Islamic Vote Margin in 1994) ytitle(Number of Observations) legend(off) title("Distribution of Observations")

graph rename histog, replace


* Density of  running variable
local h_l = h_left
local h_r = h_right
rddensity X, plot plot_range(`h_l' `h_r') graph_opt(legend(off)   title("Estimated Density")  xline(0, lcolor(black)) xtitle(Islamic Vote Margin in 1994) ytitle(Density))

graph rename density, replace

graph combine histog density

graph export "/Users/Alessandro/Desktop/ps3/output/Graph_2.pdf", replace




************************************************************
* (e) Formal test for density discontinuity at the cutoff
************************************************************


* We assess the validity of the Regression Discontinuity (RD) design 
* by testing whether the density of the running variable X 
* exhibits a discontinuity at the cutoff (c = 0).
* This is implemented using the rddensity procedure 
* (Cattaneo, Jansson, and Ma, 2020).

rddensity X, all

* - If p-value > 0.05: No evidence of manipulation → supports validity of RD design.
* - If p-value < 0.05: Evidence of manipulation → threatens validity.

* The rddensity test (Cattaneo et al., 2020) checks whether the density of the running variable X is discontinuous at the cutoff c. Such test, in the RD setting, is crucial because for an RD to be valid it is required that units cannot precisely manipulate  X to end up just above or below the cutoff. This is due to the fact that we need to assume units immediately before the cutoff to be a valid counterfactual for units (treated) immediately after the cutoff. If some units manipulate X to be just above the cutoff, then it could be argued such units may be different than the others. In such test, the null hypothesis is that the density of the running variable is continuous at the cutoff (no manipulation). The test reports both conventional and robust p-values. The latter adjusts for small-sample bias and is the preferred inferential method, as highlighted by Cattaneo et al. (2020).

* In our case, the manipulation test satistic is  T =  -1.3937, with a p-value of 0.1634 in the robust case, while it is T = -2.4450, with an associated p-value of 0.0145 using conventional errors. Therefore, if we consider conventional errors we have statistical evidence of manipulation of the running variables, and this would be a crucial concern for the validity of our RD estimation. However, if we consider robust standard errors, there is no statistical evidence of systematic manipulation of the running variable. Hence, when considering robust standard errors, we fail to reject the null hypothesis of density continuity. This result supports the assumption that units are as-good-as-randomly assigned around the cutoff, upholding the RD design's credibility.  We therefore proceed under the maintained assumption of a valid RD design.*



************************************************************
* (f) Test Density at alternative cutoffs (-10, -5, 5, 10)
************************************************************


* c(#) specifies the threshold or cutoff value in the support of Var, which determines the two samples (e.g., control and treatment units in RD settings).  Default is c(0)

* Density test at -10 margin. robust p-value: 0.4259, conventional p-value:  0.7964
rddensity X, all c(-10) 

* Density test at -5 margin. robust p-value:  0.0547, conventional p-value:  0.2864
rddensity X,all  c(-5) 

* Density test at +5 margin. robust p-value: 0.4518, conventional p-value: 0.4215
rddensity X,all  c(5) 

* Density test at +10 margin. robust p-value: 0.5026, conventional p-value:  0.8123
rddensity X, all c(10) 


* As an additional validation exercise, we conduct density tests at alternative cutoffs to check for  discontinuities elsewhere in the distribution of the running variable X. If no discontinuity  is found, the validity of RD design is strenghtened. 

*Regarding the conventional p-values obtained from the density tests at alternative placebo cutoffs, we find no statistically significant discontinuities. Specifically, the conventional p-values are 0.7964 at c = -10, 0.2864 at c = -5, 0.4215 at c = +5, and 0.8123 at c = +10. As all values are above the conventional significance thresholds,  we fail to reject the null hypothesis of density continuity at these placebo thresholds. This suggests no evidence of manipulation in the distribution of the running variable away from the true cutoff. However, one must consider that these conventional p-values do not correct for small-sample bias and may underestimate the true variability in finite samples. Hence, we focus now on robust p-values. 

*Focusing on the robust p-values from the density tests at alternative placebo cutoffs, we obtain the following estimates:the robust p-values are 0.4259 at c = -10, 0.0547 at c = -5, 0.4518 at c = +5, and 0.5026 at c = +10. Hence, we cannot reject, at the 10% level, the presence of a discontinuity at a -5 margin, while the remaining placebo tests show robust p-values well above conventional significance levels, indicating smooth density across these points. The discontinuity observed at the -5 margin could be due to random sampling variability. Overall, if we consider instead the 5% significance threshold, we find no statistically significant evidence of discontinuities in the distribution of the running variable away from the true cutoff. 
 

 

************************************************************
* (g) Generate a RD Plot of Y (Share Women 15-20 with HS Education) vs X
************************************************************

rdplot Y X, nbins(40) binselect(es) graph_options(ytitle("Outcome") xtitle("Running Variable") legend(off))



************************************************************
* (h) Use rdrobust to estimate effect of T on Y (Linear Polynomial) with Uniform and Triangular Kernel
************************************************************

rdrobust Y X, kernel(uniform) p(1) bwselect(mserd)
* coefficient: 3.2019, conventional p-value: 0.018, robust p-value: 0.041


rdrobust Y X, kernel(triangular) p(1) bwselect(mserd)
* coefficient: 3.0195, conventional p-value: 0.034, robust p-value: 0.076   


*Electing an Islamic mayor in 1994 increased the share of women with high school education in 2000 by approximately 3 percentage points. Using a triangular kernel, the estimated effect is 3.0195 (conventional p-value: 0.034, robust p-value: 0.076), while the uniform kernel yields a slightly larger estimate of 3.2019 (conventional p-value: 0.018, robust p-value: 0.041). Both results are statistically significant at the 5% level with the conventional p-values, and at 10% for robust p-values with similar magnitudes and overlapping confidence intervals, indicating that the positive effect is robust to the choice of the kernel.



************************************************************
* (i) Estimate the effect of T on Y using a Global Polynomial of Order 4
************************************************************

gen X_2=X^2
gen X_3=X^3
gen X_4=X^4

gen X_T=T*X
gen X_T_2=T*X_2
gen X_T_3=T*X_3
gen X_T_4=T*X_4

sum X
scalar min_X = r(min)
scalar max_X = r(max)
scalar range_X = max_X - min_X


gen weights = .
replace weights = (1-abs(X/min_X)) if X<0 & X >= min_X
replace weights = (1-abs(X/max_X)) if X>=0 & X <=  max_X
*global regression with triangular kernel 
reg Y T X X_2 X_3 X_4 X_T X_T_2 X_T_3 X_T_4 [aw = weights]
* T coefficient: 3.028359 ; p-value: 0.043


*Using a global regression approach the estimated effect of electing an Islamic mayor in 1994 on female high school attainment is +3.028 percentage points, statistically significant at the 5% level (p =  0.043 ). A global 4th-order polynomial is used to flexibly model the outcome variable Y as a function of the running variable X over the full sample, allowing for nonlinear trends that might otherwise bias the estimated treatment effect. Using a global regression implies we are working with the full sample, this is problematic because 1) this is a global optimizaion, wille we care about local errors, 2) estimates can be sensitive to the degree of the polynomial used, 3) the global regression includes also points that are far away from the discontinuity. However, in the regression above, we have addressed  point 3) as the observations are weighted with triangular kernel weights so to downweight distant observations. Such weights reduce the influence of observations that are far away on the estimation of the treatment effect at the cutoff while giving more importance to observations that are closer to the cutoff (in the EXTRA below one can find the estimate obtained through the unweighted regression). If - as in the equation above - we use the triangular kernel weights, the estimated coeffient on T aligns in magnitude with the local rdrobust estimates, confirming the positive impact of Islamic political control on women's education and supporting the robustness of the RD design findings (while we find a larger coefficient, of 3.68, with the unweighted global regression, see the EXTRA below ). 


* EXTRA: This is the regression above without the use of weights
reg Y T X X_2 X_3 X_4 X_T X_T_2 X_T_3 X_T_4
* T coefficient: 3.682873  ; T p-value: 0.024, note how in this specificatin the beta coefficient ha sincreased from 3.028 to 3.68 and the p-value has gone from 0.043 to  0.024. 


************************************************************
* (j) Estimate effect of T on Y using Local Linear Regression within Optimal Bandwidth
************************************************************


* EXTRA: PANEL A. ESTIAMTE FROM RDROBUST WITH UNIFORM KERNEL ARE EQUAL TO THE LOCAL REGRESSION
rdrobust Y X, kernel(uniform) p(1) bwselect(mserd)

* Store optimal bandwidth as requested
local opt_i = e(h_l)

* Left of cutoff
reg Y X if X < 0 & X >= -`opt_i'
matrix coef_left = e(b)
matrix var_left = e(V)
scalar intercept_left = coef_left[1, 2]

* Right of cutoff
reg Y X if X >= 0 & X <= `opt_i'
matrix coef_right = e(b)
matrix var_right = e(V)
scalar intercept_right = coef_right[1, 2]

* RD effect
scalar difference = intercept_right - intercept_left

* Variance of the difference
matrix var_conventional = var_left + var_right
scalar se_difference = sqrt(var_conventional[2,2])

* Display results
scalar list difference
scalar list se_difference

* rdrobust for comparison
rdrobust Y X, kernel(uniform) p(1) bwselect(mserd) 




* PANEL B. ESTIMATE FROM RDROBUST WITH TRIANGULA KERNEL ARE DIFFERENT TO THE LOCAL REGRESSION (DIFFERENT WEIGHTS)
clear matrix
macro drop _all
scalar drop _all
* Estimate RD effects through a local approach, using a triangular kernel
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd)

* Store optimal bandwidth as requested
local opt_i = e(h_l)

* Left of cutoff (no weights)
reg Y X if X < 0 & X >= -`opt_i'
matrix coef_left = e(b)
matrix var_left = e(V)
scalar intercept_left = coef_left[1, 2]

* Right of cutoff (no weights)
reg Y X if X >= 0 & X <= `opt_i'
matrix coef_right = e(b)
matrix var_right = e(V)
scalar intercept_right = coef_right[1, 2]

* RD effect
scalar difference = intercept_right - intercept_left

* Variance of the difference
matrix var_conventional = var_left + var_right
scalar se_difference = sqrt(var_conventional[2,2])

* Display results
scalar list difference
scalar list se_difference

* rdrobust for comparison
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd) 




* PANEL C. ESTIMATE FROM RDROBUST WITH TRIANGULA KERNEL IS EQUAL TO THE LOCAL WLS REGRESSION 
clear matrix
macro drop _all
scalar drop _all

rdrobust Y X, kernel(triangular) p(1) bwselect(mserd)

* Store optimal bandwidth as requested
local opt_i = e(h_l)

gen weights = .
replace weights = (1 - abs(X/ -`opt_i')) if X < 0 & X >= -`opt_i'
replace weights = (1 - abs(X/`opt_i')) if X >= 0 & X <= `opt_i'

reg Y X [aw = weights] if X >= -`opt_i' & X < 0
matrix coef_left = e(b)
matrix var_left = e(V)
scalar intercept_left = coef_left[1, 2]
*
reg Y X [aw = weights] if X >= 0 & X <= `opt_i'
matrix coef_right = e(b)
matrix var_right = e(V)
scalar intercept_right = coef_right[1, 2]


scalar difference = intercept_right - intercept_left

matrix var_conventional = var_left + var_right
scalar se_difference = sqrt(var_conventional[2,2])


scalar list difference
scalar list se_difference

* rdrobust for comparison
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd) 


*We estimate the effect of electing an Islamic mayor on female high school attainment using a local regression approach that restricts the sample to observations within the optimal bandwidth selected by rdrobust. 

* EXTRA (PANEL A): We first apply a uniform kernel rdrobust, which assigns equal weight to all observations inside the bandwidth, and obtain an estimated effect of +3.20 percentage points, significant at the 5% level (conventional p-value  0.018, robust p-value  0.041). This result exactly matches the rdrobust output with a uniform kernel because restricting the sample and running an unweighted OLS is  equivalent to using a uniform kernel. 

* Then, we repeat the procedure with a triangular kernel rdrobust (PANEL B). The difference in intercepts from the local unweighted regression is 3.059, while rdrobust reports  3.0195. The difference arises because rdrobust employs a triangular kernel that down-weights distant observations, giving more emphasis to the most comparable units, while the local unweighted regression does not. However, if we create triangular kernel weights and we apply them to the observations in the local regression (PANEL C), the estimate aligns perfectly with the rdrobust triangular kernel result (3.0195). 



************************************************************
* (k) Sensitivity to Bandwidths: 0.5×, 0.75×, 1.25×, 1.5×
************************************************************


* Robustness for triangular kernel
* remark: I am using a linear polynomial because i am asked to replicate point h 
* I am using conventional errors as suggested in the first page of the problem setting

* Save optimal bandwidth from (h)
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd)
scalar opt_i = e(h_l)

* Compute alternative bandwidths and estimates
matrix define R = J(5, 6, .)
local bandwidth_multipliers 0.5 0.75 1 1.25 1.5
local r = 1

foreach m of local bandwidth_multipliers {
    local k = `m' * opt_i
    rdrobust Y X, kernel(triangular) p(1) h(`k')
    matrix R[`r', 1] = `k'
    matrix R[`r', 2] = e(tau_cl)
    matrix R[`r', 3] = e(tau_bc)
    matrix R[`r', 4] = e(se_tau_cl)
    matrix R[`r', 5] = R[`r', 2] - invnormal(0.975) * R[`r', 4]
    matrix R[`r', 6] = R[`r', 2] + invnormal(0.975) * R[`r', 4]
    
    local r = `r' + 1
}

* Plot RD estimates with Confidence Intervals
local bw05 = 0.5 * opt_i
local bw075 = 0.75 * opt_i
local bw1 = 1 * opt_i
local bw125 = 1.25 * opt_i
local bw15 = 1.5 * opt_i

preserve
    clear
    svmat R
    twoway (rcap R5 R6 R1, lcolor(navy)) (scatter R2 R1, mcolor(cranberry) yline(0, lcolor(black) lpattern(dash))),graphregion(color(white)) xlabel(`bw05' `bw075' `bw1' `bw125' `bw15', format(%9.2f)) ytitle("RD Treatment Effect") xtitle("Bandwidth") title("Graph 3: Robustness to Bandwidth Choice (Triangular Kernel)") yscale(range(-5 10)) legend(off)
	
restore

graph export "/Users/Alessandro/Desktop/ps3/output/Graph_3.pdf", replace


* Robustness checks are used to ensure estimated results are not overly sensitive to reasonable methodological choices made by the researcher. As many robustness checks can be performed, a key one is to assesses the sensitivity of the estimated treatment effect to the choice of bandwidth. This is important as the local estimates can be significanlty affected by the selected neighborhood.

*The estimated effect of electing an Islamic mayor on the share of women aged 15–20 with high school education is positive and statistically significant at the optimal bandwidth (coefficient ≈ 3.02, p = 0.034). We now explore the required alternative bandwidths (from 0.5×opt_i to 1.5×opt_i): smaller bandwidths are characterized by wider confidence intervals and somewhat statistically insignificant estimates, possibly as a result of fewer obsrvationns and higher variance. In contrast, larger bandwidths are seen to reduce standard errors, improving precision and yielding significant estimates at conventional levels. Still, point estimates remain  consistently between 2.6 and 3.0 percentage points, suggesting limited sensitivity in magnitude. Hence, while small bandwidths result in imprecise estimates, increasing the bandwidth enhances statistical significance without materially altering the estimated effect size. This pattern indicates that the observed positive impact is reasonably consistent across a range of multiple bandwidths, supporting the credibility of the treatment effect while acknowledging the well-known bias-variance trade-off typical of bandwidth selection.


*see the APPENDIX for a robustness check with UNIFORM kernel (results do not differ significantly)

l


*** APPENDIX 
* Robustness for uniform kernel
* Save optimal bandwidth from (h)
rdrobust Y X, kernel(uniform) p(1) bwselect(mserd)
scalar opt_i = e(h_l)

* Compute alternative bandwidths and estimates
matrix define R = J(5, 6, .)
local bandwidth_multipliers 0.5 0.75 1 1.25 1.5
local r = 1

foreach m of local bandwidth_multipliers {
    local k = `m' * opt_i
    
    rdrobust Y X, kernel(uniform) p(1) h(`k')
    
    matrix R[`r', 1] = `k'
    matrix R[`r', 2] = e(tau_cl)
    matrix R[`r', 3] = e(tau_bc)
    matrix R[`r', 4] = e(se_tau_cl)
    matrix R[`r', 5] = R[`r', 2] - invnormal(0.975) * R[`r', 4]
    matrix R[`r', 6] = R[`r', 2] + invnormal(0.975) * R[`r', 4]
    
    local r = `r' + 1
}

*  Plot RD estimates with Confidence Intervals
local bw05 = 0.5 * opt_i
local bw075 = 0.75 * opt_i
local bw1 = 1 * opt_i
local bw125 = 1.25 * opt_i
local bw15 = 1.5 * opt_i

preserve
    clear
    svmat R
    twoway (rcap R5 R6 R1, lcolor(navy)) (scatter R2 R1, mcolor(cranberry) yline(0, lcolor(black) lpattern(dash))),graphregion(color(white)) xlabel(`bw05' `bw075' `bw1' `bw125' `bw15', format(%9.2f)) ytitle("RD Treatment Effect") xtitle("Bandwidth") title("Graph 3: Robustness to Bandwidth Choice (Uniform Kernel)") yscale(range(-5 10)) legend(off)

graph export "/Users/Alessandro/Desktop/ps3/output/Graph_3b.pdf", replace
restore




*EXERCISE 2

/* Question A */

use "$filepath\fraud_pcenter_final.dta", replace

gen running = -_dist if cov==0
replace running = _dist if cov==1

rdplot cov running, c(0) p(1) kernel(triangular) bwselect(mserd) graph_options(xtitle("distance to coverage boundary") ytitle("cell phone coverage"))

		
rdrobust cov running, c(0) p(1) kernel(triangular) bwselect(mserd) all


/* This design is a fuzzy RD. Indeed, the treatment assignment is not a 
deterministic function of the running variable. This can be seen 
in the scatterplot, as there are observations marked as cov=1 
(i.e., inside the coverage area) but with a positive distance from the coverage area, 
indicating that they are outside it according to our running variable. 
The same is true for the opposite case. Additionally, the RD estimate 
clearly indicates that the RD is not sharp, otherwise the coefficient 
would have been equal to -1. In this setting the probability of being treated 
(propensity score)simply increases from the left to the right of the threshold.

The estimates of the Sharp RD of Gonzalez (2021) are still valid as long as they are interpreted 
as an intent-to-treat effect (ITT), that is, the effect of being beyond the 
threshold, regardless of whether the treatment was actually received.

In this fuzzy RD setting, the RD estimate is obtained by using the running variable as an IV
for the the treatment. The effect found is the LATE and it is valid under the following
assumptions:

1. Continuity of potential outcomes: The outcome (e.g., fraud) must evolve smoothly at
the cutoff in the absence of treatment.

2. Discontinuity in treatment probability:There must be a jump in the probability 
of cov = 1 at running = 0. This provides a valid first stage for the fuzzy RD estimator.

3. No manipulation of the running variable: Polling centers must not have sorted 
just inside or outside coverage. Given that coverage boundaries are based on terrain 
and technical limits, this is a reasonable assumption.

4. Exclusion restriction: The running variable affects the outcome only through 
its effect on treatment. Other factors (e.g., roads, elevation, ethnicity) must 
not jump at the cutoff unless properly controlled for.

5. Monotonicity: No defiers — crossing the threshold must weakly increase the 
probability of treatment.


*/ 




/* Question B */

/* Using a proxy for the longitude might result in incorrect 
classification of voting centers, such that near the boundary, 
some voting centers might be incorrectly classified as non-covered  
while they are actually covered and vice versa. In this case, the RD design would become 
a fuzzy RD. In Gonzalez, the RD design was sharp and not fuzzy. However, 
there might be some issues with the RD specification, 
reported in the "Additional Results" section, that could make the design 
a fuzzy one. If such issues with the cell phone coverage boundary hold, then 
the specification used in this exercise and the one of Gonzalez are both 
fuzzy RD. The issues I refer to are, in particular:

1) Presence of spillover effects. This can happen when it is possible 
to cross the boundary by walking, so that voters in the non-coverage 
area can actually access coverage areas and report fraud. 

2) Presence of other mobile service providers. This refers to the fact that other 
mobile service providers were excluded from the study, so that polling centers 
outside the coverage area might instead be covered by other providers.

3) Cell tower shutdowns. The Taliban may force cell phone companies to regularly shut 
down antennas to prevent civilians from accessing the internet. If this happens during 
the election period, then polling centers marked as covered might instead be not covered.

4) Signal strength fuzziness. Due to weather conditions, the signal strength 
near the boundary might be inconsistent. Hence, some of the polling centers marked as 
covered by the cell phone towers might not actually be covered during election day, and vice versa. 
This would generate a fuzzy RD.

Points 1 and 4 are more representative of our setting, since they allow 
for both covered centers to be actually not covered and vice versa.


There can also be the case in which having a proxy for the longitude would still 
deliver a sharp RD design, so that it would be the same specification used in
Gonzalez (2021). In particulare, this happen when the cell phone coverage boundary 
is perfectly horizontal, i.e., when it varies only with latitude and not with 
longitude. In this case, the treatment assignment depends only on latitude, 
and mismeasurement in longitude has no effect on whether a polling center is 
classified as treated or not. Also, Gonzalez performed several tests to evaluate
the potential threats described above, showing that in most cases they do not affect 
sharp design. We can replicate the analysis done in Gonzalez to assess the threat
in point 4 in order to evaluate whether the noise in longitude affect results. If
this does not happen then the design remains sharp, still with an ITT interpretation
of the results.




*/



/* Question C */
use "$filepath\fraud_pcenter_final.dta", replace


********************************************
*REPLICATING UNDER FUZZY SETTING
********************************************


****************************
*Optimal bandwidth
****************************
gen running= -_dist if cov==0
replace running = _dist if cov==1

foreach var in comb comb_ind {
		rdbwselect vote_`var' running if ind_seg50==1, vce(cluster segment50)
		scalar hopt_`var'=e(h_mserd)
		forvalues r=1/2 {
			rdbwselect vote_`var' running if ind_seg50==1 & region2==`r', vce(cluster segment50)
			scalar hopt_`var'_`r'=e(h_mserd)
	}
}
*

xtset, clear
xtset segment50 pccode


********************************************************************************
* 		B. Local Linear Regression (using _dist as instrument)
********************************************************************************

gen Z = running > 0
gen cov_running = cov*running
gen Z_running = Z*running


foreach var in comb_ind comb {	
*All regions
xtivreg vote_`var' (cov cov_running = Z Z_running) running if ind_seg50==1 & abs(running)<=hopt_`var', fe vce(robust) 
est store col1_a_`var'

*Southeast
xtivreg vote_`var' (cov cov_running = Z Z_running) running if ind_seg50==1 & abs(running)<=hopt_`var'_1 & region2==1, fe vce(robust) 
est store col1_b_`var'

*Northwest
xtivreg vote_`var' (cov cov_running = Z Z_running) running if ind_seg50==1 & abs(running)<=hopt_`var'_2 & region2==2, fe vce(robust) 
est store col1_c_`var'

 }
*

*Reporting only point estimates

matrix results = J(1, 3, .)
local row = 1
local varnames

foreach var in comb_ind comb {

    * Restore estimates
    est restore col1_a_`var'
    scalar b_all = _b[cov]

    est restore col1_b_`var'
    scalar b_se = _b[cov]

    est restore col1_c_`var'
    scalar b_nw = _b[cov]

    * Fill in row
    matrix results[`row',1] = b_all
    matrix results[`row',2] = b_se
    matrix results[`row',3] = b_nw

    * Append varname
    local varnames `varnames' `var'

    * Increase row counter
    local ++row

    * If more vars left, expand matrix
    if "`var'" != "comb" {
        matrix results = results \ J(1, 3, .)
    }
}

* Assign column names
matrix colnames results = All Southeast Northwest

* Assign row names using the local macro
matrix rownames results = `varnames'

* Display labeled matrix
matrix list results



	
/*The new estimates represent the Local average treatment effect (LATE)
for the compliers, that are those whose actual treatment status coincide
with the treatment assignment. The LATE can be obtained by using
the running variable as an IV for our treatment indicator.

Compared to the results of Gonzalez(2021), the coefficient for the likelihood that
at least one statiton is in category C fraud become not significant in all regions.
There is still a difference in this outcome between southeast and northwest regions.
In the southwest the effect of the cell phone coverage is negative and significant 
at the 10% level while in the northwest there is no effect. 

The coefficient for the share of votes under category C fraud become negative more 
and significant across all regions, compared with Gonzalez. The pattern across southeast
and northeast regions remains the same as before.

*/































