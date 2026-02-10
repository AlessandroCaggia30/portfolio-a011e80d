
* Group number: 20
* Group composition: Alessandro Caggia, Federico Cosenza and Simone Donoghue

clear all

* Get user name
local user = c(username)
display "`user'"

* Store filepath conditionally
if ("`user'" == "simon") {
    global filepath "C:\Users\simon\OneDrive\Desktop\micrometrics_stata\files"
}

if ("`user'" == "Alessandro") {
    global filepath "/Users/Alessandro/Desktop/micrometrics_stata/files"
}

if ("`user'" == "utente") {
    global filepath "\Users\utente\Desktop\micrometrics_stata\files"
}

* UNCOMMENT IF NOT ALREADY INSTALLED:
* ssc install outreg2, replace
* ssc install randtreat, replace
* ssc install lassopack, replace
* ssc install pdslasso, replace
* ssc install ranktest, replace



**********************
***** QUESTION 1 *****
**********************

********* 1a *********

use "$filepath\jtrain2.dta", replace

bysort train: sum age educ black hisp nodegree re74 re75

quietly{
matrix table1 = (.,.,.,.,.,.)
local i = 1

foreach var in age educ black hisp re74 re75 nodegree {
    ttest `var', by(train) unequal
	
	matrix table1[`i',1]=r(mu_1)
	matrix table1[`i',2]=r(mu_2) 
	matrix table1[`i',3]=r(sd_1)
	matrix table1[`i',4]=r(sd_2) 
	matrix table1[`i',5]= r(mu_1)-r(mu_2)
	matrix table1[`i',6]=r(se) 
	matrix list table1
		local i=`i'+1
	if `i'<=7 matrix table1=(table1 \ .,.,.,.,.,.) 
	
}
	
matrix colnames table1= "Mean_T" "Mean_C" "StDev_T" "StDev_C" "Mean Diff" "StDev for Mean Diff"
matrix rownames table1= "Age" "Education" "Black" "Hispanic" "Real Earn 74" "Real Earn 75" "No Degree"

matrix list table1, f(%9.3f) title("Balance check")
	
	putexcel set "$filepath\Table_1.xlsx", replace
	
	
	putexcel A1=matrix(table1), names nformat(number_d2)
	putexcel (A2:A8), overwr bold border(right thick) 
	putexcel (B1:G1), overwr bold border(bottom thick)
}
	
/* COMMENT 1a: 
Out of the seven covariates analysed, two present some imbalances. The balancing table presents a mild imbalance in the proportion of the Hispanic individuals (with a mean difference of 0.05 and a standard deviation of 0.03) and a strong imbalance in the "No degree" percentage (mean difference of 0.13 and standard deviation of 0.04)
Such result may suggest a correlation between the described covariates and the treatment assignment. We were given the information that the sample we are given is a restricted subset of the original datased from Lalonde (1985)*1. It is then understnandable that the treated and controls grops are not perfectly randomized anymore, thus suggesting us an explanation for the observed imbalances. 
Note: We discarded the option of a false positive because on the one hand we are running just 7 tests and the on the other a 1% significance is hardly justifiable as a false positive. 

*1 Indeed, the original dataset from Lalonde (1986) consisted of two randomly allocated groups: 297 treated and 425 controls. Our dataset is a subset of the original sample and contains 185 treated and 260 controls.
*/



********* 1b *********

reg re78 train
outreg2 using "$filepath\Reg_1", replace dta
use "$filepath\Reg_1_dta"
export excel using "$filepath\Reg_1", replace

/* COMMENT 1b: 
Receiving the training increases individuals' earnings by 1.79$ on average; the t-statistic is 2.84, the result is significant at the 1% level(p<0.01), and the confidence interval does not include the zero (0.5505748 - 3.038111). 
*/


********* 1c *********

use "$filepath\jtrain2.dta", replace

quietly{
reg re78 train

count if train == 1
local treated = r(N)

count if train == 0
local control = r(N)

outreg2 using "$filepath\Table_2.xls", replace excel addstat(Treated, `treated', Control, `control')

reg re78 train age educ black hisp

count if train == 1
local treated = r(N)

count if train == 0
local control = r(N)

outreg2 using "$filepath\Table_2.xls", append excel addstat(Treated, `treated', Control, `control')

reg re78 train age educ black hisp re74 re75

count if train == 1
local treated = r(N)

count if train == 0
local control = r(N)

outreg2 using "$filepath\Table_2.xls", append excel addstat(Treated, `treated', Control, `control')
}

/* COMMENT 1c:
Model 1 (re78 ~ train): Training has a significant positive effect on earnings (1.794, p=0.005).
Model 2 (+ age + educ + black + hisp): The training effect remains significant and its magnitude is altered just marginally (1.686, p=0.008). 
Model 3 (+ re74 + re75): The training effect remains stable (1.680,p=0.008), even after controlling for prior earnings.

Final Conclusion:
The coefficient on the train dummy changes only marginally in magnitude and significance across models. The fact that such estimate results to be unaffecetd by the introduction of controls suggests that the randomization worked effectively in balancing the treatment and control groups. Note: since we did not include the one variable that appeared to be significantly imbalanced, that is, "No degree", such results are in line with what is observed in the balance test.
*/




********* 1d *********

use "$filepath\jtrain2.dta", replace

reg re78 train age educ black hisp re74 re75
dfbeta train
rename _dfbeta_1 influence_train

preserve
reg re78 train age educ black hisp re74 re75
outreg2 using "$filepath\TABLE_influence_train", ctitle (Standard specification) replace dta
restore

preserve
gsort influence_train
keep if _n < _N - 3  & _n > 2
reg re78 train age educ black hisp re74 re75
outreg2 using "$filepath\TABLE_influence_train", ctitle (w/o top and bottom 3) append dta
restore

preserve
gsort influence_train
keep if _n < _N - 5  & _n > 4
reg re78 train age educ black hisp re74 re75
outreg2 using "$filepath\TABLE_influence_train", ctitle (w/o top and bottom 5) append dta
restore

preserve
gsort influence_train
keep if _n < _N - 10  & _n > 9
reg re78 train age educ black hisp re74 re75
outreg2 using "$filepath\TABLE_influence_train", ctitle (w/o top and bottom 10) append dta
restore

use "$filepath\TABLE_influence_train_dta", replace
export excel using "$filepath\TABLE_influence_train", replace

/* COMMENT 1d:

We compare the baseline model ols estimated coefficient on the train dummy with the beta estimates obtained by trimming the 3, 5, and 10 observations with the lowest and largest values in influence train. 

The DfBeta analysis indicates that the estimated effect of train decreases as the most influential observations are removed, dropping from 1.680 (p<0.01) to 0.889 (p<0.1) after excluding the top and bottom 10 cases. After removing the top and bottom 3 observations, the coefficient falls to 1.156 (p<0.05). With the removal of 5 influential observations, the estimate further declines to 1.064 (p<0.05). After excluding the top and bottom 10 cases, the effect drops to 0.889 (p<0.1), showing that extreme values played a significant role in driving the initial estimate. This suggests that a few observations were pulling the treatment effect upward, though the effect remains positive and statistically significant.

*/
	   
* Appendix: Significance by Belsley, Kuh, and Welsch (1980) 

use "$filepath\jtrain2.dta", replace

quietly{
reg re78 train age educ black hisp re74 re75
dfbeta train
rename _dfbeta_1 influence_train
gen threshold = 2 / sqrt(_N)
gen influential = abs(influence_train) > threshold
list if influential == 1

clear all 
use "$filepath\jtrain2.dta", replace
reg re78 train age educ black hisp re74 re75
dfbeta train
rename _dfbeta_1 influence_train
gen threshold = (2 / sqrt(_N)) * 1.1
gen influential = abs(influence_train) > threshold
list if influential == 1
}

/* COMMENT to Appendix of 1d: 
 
With the aim of determining whether the observed reduction in beta is truly concerning, we compare our estimated influence train to a recommended cutoff that was first introduced by Belsley, Kuh, and Welsch (1980). 17 observations present a dfbeta that is above the threshold. Even allowing for a +10% margin over the cutoff still leaves 14 observations above the adjusted cutoffs, suggsesting some more nuanced analysis could be needed.

*/




**********************
***** QUESTION 2 *****
**********************

use "$filepath\jtrain3.dta", replace

********* 2a *********

quietly{
matrix table1b = (.,.,.,.,.,.)
local i = 1

foreach var in age educ black hisp re74 re75 {
    ttest `var', by(train) unequal
	
	matrix table1b[`i',1]=r(mu_1)
	matrix table1b[`i',2]=r(mu_2) 
	matrix table1b[`i',3]=r(sd_1)
	matrix table1b[`i',4]=r(sd_2) 
	matrix table1b[`i',5]= r(mu_1)-r(mu_2)
	matrix table1b[`i',6]=r(se) 
	matrix list table1b
		local i=`i'+ 1
	if `i'<=7 matrix table1b=(table1b \ .,.,.,.,.,.) 
	
}
	
matrix colnames table1b= "Mean_T 2" "Mean_C 2" "StDev_T 2" "StDev_C 2" "Mean Diff 2" "StDev for Mean Diff 2"
matrix rownames table1b= "Age" "Education" "Black" "Hispanic" "Real Earn 74" "Real Earn 75" "No Degree" 

matrix list table1b, f(%9.3f) title("Balance check")

	
putexcel set "$filepath\Table_1.xlsx", replace
	
putexcel A2=matrix(table1b), names nformat(number_d2)
putexcel (A3:A9), overwr bold border(right thick) 
putexcel (B2:G2), overwr bold 
		
putexcel H2=matrix(table1b), colnames nformat(number_d2)
putexcel (H2:N2), overwr bold
}

/* 
The balance table shows significant differences between the treatment and control groups across most covariates. Age (t=15.96), education (t=11.07), race (Black,  t=−21.18), and past earnings (Real Earn 74: t=38.61, Real Earn 75: t=48.57) exhibit high imbalances, indicating that the groups are not comparable. The Hispanic variable (t=−1.50) is moderately balanced, but overall, the differences suggest that selection into treatment was not random. In order to recover the causal effect we should add controls for the variables that are not balanced.
Differently from question 1a, in this case it was reasonably sound to expect imbalances as now we are analysing non-experimental data, while before we were considering an RCT.
*/
		 
********* 2b *********

set seed 12345
gen rand= uniform()
sort rand
gen treated = (_n <= _N/2)


********* 2c *********


* help randtreat


randtreat, gen(treated_2) misfits(global)

pwcorr treated_2 treated, sig 

* As expected, no evidence of correlation between the two (fake) treatment variables is found as both have been randomly generated.

********* 2d *********

matrix table1b = (.,.,.,.,.,.)
local i = 1

foreach var in age educ black hisp re74 re75 {
    ttest `var', by(treated) unequal
	
	matrix table1b[`i',1]=r(mu_1)
	matrix table1b[`i',2]=r(mu_2) 
	matrix table1b[`i',3]=r(sd_1)
	matrix table1b[`i',4]=r(sd_2) 
	matrix table1b[`i',5]= r(mu_1)-r(mu_2)
	matrix table1b[`i',6]=r(se) 
	matrix list table1b
		local i=`i'+ 1
	if `i'<=7 matrix table1b=(table1b \ .,.,.,.,.,.) 
	
}
	
matrix colnames table1b= "Mean_T 2" "Mean_C 2" "StDev_T 2" "StDev_C 2" "Mean Diff 2" "StDev for Mean Diff 2"
matrix rownames table1b= "Age" "Education" "Black" "Hispanic" "Real Earn 74" "Real Earn 75" "No Degree" 

matrix list table1b, f(%9.3f) title("Balance check")

	
putexcel set "$filepath\Table_1.xlsx", replace
	
putexcel A2=matrix(table1b), names nformat(number_d2)
putexcel (A3:A9), overwr bold border(right thick) 
putexcel (B2:G2), overwr bold 
		
putexcel H2=matrix(table1b), colnames nformat(number_d2)
putexcel (H2:N2), overwr bold
 
		 

/* COMMENT 2d: 
As expected, there are no significant differences across groups in any of the variables, meaning that the results are balanced. This is due to the fact that there is no possibility of self-selection or other sources of imbalances (apart from random chance) in the fake treatment randomly generated. 
*/


********* 2e *********

quietly{
reg re78 treated

count if treated == 1
local treated = r(N)

count if treated == 0
local control = r(N)

outreg2 using "$filepath\Table_2.xls", append excel addstat(Treated, `treated', Control, `control')

reg re78 treated age educ black hisp 
count if treated == 1
local treated = r(N)

count if treated == 0
local control = r(N)

outreg2 using "$filepath\Table_2.xls", append excel addstat(Treated, `treated', Control, `control')

reg re78 treated age educ black hisp re74 re75

count if treated == 1
local treated = r(N)

count if treated == 0
local control = r(N)

outreg2 using "$filepath\Table_2.xls", append excel addstat(Treated, `treated', Control, `control')
}

/* 
The first regression, estimated without controls, suggests no significant effect of the fake treatment (p=0.605). The same conclusions can be made when also adding age, education, and race (p=0.546), or, on top of them, also past earnings (p=0.390). The R² increased from 0.001 to 0.586, indicating a substantial improvement in explanatory power. The lack of significant effects, even when introducing the covariates, is very much expected, as the treatment is not real but just randomly allocated.
*/

********* 2f *********

quietly{
reg re78 train

count if train == 1
local treated = r(N)

count if train == 0
local control = r(N)

outreg2 using "$filepath\Table_2.xls", append excel addstat(Treated, `treated', Control, `control')

reg re78 train age educ black hisp

count if train == 1
local treated = r(N)

count if train == 0
local control = r(N)

outreg2 using "$filepath\Table_2.xls", append excel addstat(Treated, `treated', Control, `control')

reg re78 train age educ black hisp re74 re75

count if train == 1
local treated = r(N)

count if train == 0
local control = r(N)

outreg2 using "$filepath\Table_2.xls", append excel sortvar (train treated age educ black hisp RE74 RE75) addstat(Treated, `treated', Control, `control')
}

/* 
- Model 1 (re78 ~ train): Training has a large negative effect (−15.205, p<0.01), meaning trained individuals earned significantly less in 1978. 
- Model 2 (+ age + educ + black + hisp): The training effect weakens (−8.452, p<0.01), showing that some of the initial effect was due to demographic differences.
- Model 3 (+ re74 + re75): The training effect disappears (0.213, p=0.80). 
 
The results differ significantly from those obtained through the RCT. In this case, the coefficient on the train dummy is negative and large when no controls are included. As controls are added, the coefficient decreases in magnitude and loses significance. This aligns with our expectations, given the strong imbalances identified in the balance test. The findings suggest that treatment assignment was highly correlated with other factors. Therefore, we suggest that the effect observed in the unadjusted regression is driven by omitted variable bias (OVB). These results, in line with what is discussed in Lalonde (1986), highlight how relying on observational data can lead to unreliable estimates.
*/




**********************
***** QUESTION 3 *****
**********************

use "$filepath/jtrain2.dta", replace

********* 3a *********

rlasso re78 age educ black hisp re74 re75
lassocoef
*run a regressor on the covariates selected by rlasso (none)
reg re78 train

* COMMENT TO RESULTS

/*  We employ rlasso as our preferred data-driven regularization approach. None of the inputed covariates is selected by the rlasso command in the first step, which leads us to a second step where we regress re78 only on the train dummy. In such context, we use rlasso as a tool to select covariates to be inlcuded as controls, checking to what extent such variables have predictive power on the outcome of interest. The fact rlasso discards all the covariates implies that they are weak predictors of the outcome of interests, as was clear already in point 1a, where the regression presented a very low adjusted rsquared (3.9%). 


* Question: What are the issues of performing inference based on such a regression?

* The main problem is that regularization methods such as LASSO select variables based on the magnitude of their coefficient, that is, on how well they predict the outcome variable. The structure behind such models is the one of "approximate sparsity", that allows only a limited number of coefficients to have an impact on the outcome, that is, a coefficient different from zero. Therefeore, regularization methods based on this model, such as LASSO, shrink the coefficient with less impact of the outcome variable to zero. This feature is very usefull for variable selection and in prediction to address the issue of overfitting, however, it is problematic in terms of inference. Indeed, any variable that is highly correlated to the treatment, but that has only a small effect on the outcome, tends to be dropped, clearly leading to OVB. If we base our analysis on the controls selected by LASSO we would end up with some excluded variable that move together with the treatment and have an impact (even if modest) on the outcome variable. 
*/


********* 3b *********

clear all
use "$filepath/jtrain2.dta", replace

*** 3b.1 ***
tabulate age, generate(age_dummy)
tabulate educ, generate(educ_dummy)

pdslasso re78 train (age educ black hisp re74 re75)

rlasso re78 age educ black hisp re74 re75 

rlasso re78 train age educ black hisp re74 re75

* The double selection procedure does not select any variable. The first step is the same performed in the previous point. The second step performs an rlasso on the treatment dummy, aiming to address the discussion above on OVB. No covariate is selected in the second step either. This result is coherent with what we saw in the balancing table in point 1a: the variables included in the double selection procedure are well-balanced, therefore they do not have predictive power on the treatment. The absence of controls with predictive power on the treatment is in line with a successfull randomization. 


*** 3b2 ***
*Specification 1
* Run double selection using pdslasso
pdslasso re78 train (i.age i.educ black hisp re74 re75)

* First Lasso: Predicting outcome (re78)
rlasso re78 i.age i.educ black hisp re74 re75
lassocoef

* Second Lasso: Predicting treatment (train)
rlasso train i.age i.educ black hisp re74 re75
lassocoef

* Final OLS with selected controls from both Lasso steps
regress re78 train


*Specification 2
* Run double selection using pdslasso
pdslasso re78 train (i.age##i.educ black hisp re74 re75)

* First Lasso: Predicting outcome (re78)
rlasso re78 (i.age##i.educ black hisp re74 re75)
lassocoef

* Second Lasso: Predicting treatment (train)
rlasso train i.age##i.educ black hisp re74 re75
lassocoef

* Final OLS with selected controls from both Lasso steps
regress re78 train

/* In a first specification we add dummies for each level of education and age, and in a second specification we add interactions of such dummies as well. In the first specification just one dummy from the first rlasso is selected, while two dummies are selected from the first rlasso in the second specification. We recognize that this procedure is helpful to address the risks described in the previous section when performing inference. Indeed, it allows  to select variables based not only on how well they predict the outcome, but also on how they are correlated with the treatment, performing implicitly a balancing test. In this case, however, the second steps does not select any relevant covariate*1. Given the results obtained, we can state that the two groups were balanced in all the variables included in double selection.

*1 As a robustness check we tried catalogizing the dummies, adding more controls and interaction terms, but the model did not select any variable (see appendix)
*/



/* Appendix to point 3b */

* We also tried using other specifications for our dummies, however, no covariates were selected

quietly{
clear all
use "$filepath/jtrain2.dta", replace

* -----------------------------------------------------------------
* 1. Define Age Groups
* -----------------------------------------------------------------

gen age_group = .
replace age_group = 1 if age < 20
replace age_group = 2 if age >= 20 & age < 30
replace age_group = 3 if age >= 30

* Create dummy variables for age groups
tab age_group, gen(age_dummy)

* -----------------------------------------------------------------
* 2. Define Education Groups
* -----------------------------------------------------------------

gen educ_group = .
replace educ_group = 1 if educ <= 8
replace educ_group = 2 if educ > 8  & educ <= 12
replace educ_group = 3 if educ >= 13  

* Create dummy variables for education groups
tab educ_group, gen(educ_dummy)

* -----------------------------------------------------------------
* 3. Create New Interactions
* -----------------------------------------------------------------

* Age × Education Interaction
gen ageXeduc = age_group * educ_group

* Age × Black Interactions
foreach i of numlist 1/3 {
    gen ageXblack`i' = age_dummy`i' * black
}

* Education × Black Interactions
foreach i of numlist 1/3 {
    gen educXblack`i' = educ_dummy`i' * black
}

* Hispanic × Age Interactions
foreach i of numlist 1/3 {
    gen hispXage`i' = age_dummy`i' * hisp
}

* Hispanic × Education Interactions
foreach i of numlist 1/3 {
    gen hispXeduc`i' = educ_dummy`i' * hisp
}
* -----------------------------------------------------------------
* 4. Create re74 and re75 5-Tile (Quantiles)
* -----------------------------------------------------------------

xtile re74_20 = re74, n(4)  
xtile re75_20 = re75, n(4)  

* Create dummy variables for quantiles
tab re74_20, gen(re74_q)
tab re75_20, gen(re75_q)

* -----------------------------------------------------------------
* 5. Add More Interactions
* -----------------------------------------------------------------

* Continuous Interactions
gen ageXre74 = age * re74
gen ageXre75 = age * re75
gen educXre74 = educ * re74
gen educXre75 = educ * re75

* re74 Quantile × Age
foreach i of numlist 1/3 {
    gen re74qXage`i' = re74_q`i' * age_group
}

* re75 Quantile × Age
foreach i of numlist 1/3 {
    gen re75qXage`i' = re75_q`i' * age_group
}

* re74 Quantile × Education
foreach i of numlist 1/3 {
    gen re74qXeduc`i' = re74_q`i' * educ_group
}

* re75 Quantile × Education
foreach i of numlist 1/3 {
    gen re75qXeduc`i' = re75_q`i' * educ_group
}

* Race-Specific Quantile Interactions
foreach i of numlist 1/3 {
    gen re74qXblack`i' = re74_q`i' * black
    gen re74qXhisp`i' = re74_q`i' * hisp
    gen re75qXblack`i' = re75_q`i' * black
    gen re75qXhisp`i' = re75_q`i' * hisp
}

* Treatment-Specific Interactions
gen trainXage = train * age_group
gen trainXeduc = train * educ_group

foreach i of numlist 1/3 {
    gen trainXre74q`i' = train * re74_q`i'
    gen trainXre75q`i' = train * re75_q`i'
}

* -----------------------------------------------------------------
* 6. Run LASSO for Variable Selection
* -----------------------------------------------------------------

* First LASSO: Predicting Outcome (re78)
rlasso re78 age_dummy* educ_dummy* black hisp re74 re75 ageXeduc hispXage* hispXeduc* re74_q* re75_q* re74qXage* re75qXage* re74qXeduc* re75qXeduc* ageXre74 ageXre75 educXre74 educXre75 re74qXblack* re74qXhisp* re75qXblack* re75qXhisp*
lassocoef  

* Second LASSO: Predicting Treatment (train)
rlasso train educ_dummy*  educXblack* hispXage* hispXeduc* re74_q* re75_q* re74qXage* re75qXage* re74qXeduc* re75qXeduc* ageXre74 ageXre75 educXre74 educXre75 re74qXblack* re74qXhisp* re75qXblack* re75qXhisp* 
lassocoef  
}




**********************
***** QUESTION 4 *****
**********************


********* 4a *********

* In the Neyman approach, the estimator of the average treatment effect for the sample at hand is unbiased. However, its sampling variance provides a biased estimate of the true variance of the average treatment effect across different randomization distributions. The bias is induced by the fact that the sampling variance includes a term that cannot be observed and thus is ignored by the researchers. Such term is the sample variance of the unit-level treatment effect, clearly not observable since for each unit we only observe one of the two potential outcomes, depending on which treatment status it has been assigned to. As a result, the sample variance is biased, and consequently, so are the confidence intervals. When allowing for nonconstant (i.e. heterogeneous) treatment effects, so for a positive value of the sample variance of the unit-level treatment effect, there is one case in which the bias vanishes. This is the case when we change the target of inference, being no more the average treatment effect for the sample at hand, but the population average treatement effect from which the sample has been drawn.

********* 4b *********

use "$filepath/jtrain2.dta", replace

** Hess
ritest train _b[train], reps(10000): reg re78 train

** Code

* Define number of treated and controlled
local n_treated = 185
local n_control = 260
local n_tot = `n_treated' + `n_control'

* Define number of permutations
local n_permutations = 10000

* Create matrix  
mat M = (.,.,.) 

forval i = 1(1)`n_permutations' {
	
	* Drop variables if pre-existing
	cap drop select sample new_T
	
	* Re-randomize treatment
    gen select = runiform()
    sort select
    gen sample = (_n <= `n_tot') 
	gen new_T = 0
    replace new_T = 1 if _n <= `n_treated' & sample == 1
	
	* Regress with new assignment
	qui reg re78 new_T if sample 
	
	* Add info to matrix
    mat M = M \ (scalar(`i'), scalar(_b[new_T]), scalar(_se[new_T]))
	
	* Display number of permutation
	display "Permutation no. "`i'
}

* Clean the matrix and add column names
mat A=M[2..10000,.]
svmat A
renam A1 perm_number
rename A2 coef
rename A3 se

* Perform the test
reg re78 train
gen c = .
replace c = 0 if abs(_b[train])>abs(coef)
replace c = 1 if abs(_b[train])<abs(coef)
egen n = total(c)
gen p_value = n/10000

* Display results
display "p_value: " p_value "  n (number of c):" n


* Alternatively we might use the command "frame"

frame rename default main_data

frame main_data {
    reg re78 train 
    scalar obs_coef = _b[train]
}

frame create fs_perm
frame fs_perm {
    set obs 10000  
    gen coeff = .
}

frame main_data {
    _dots 0, title("Running Permutations") reps(10000)
    
    forvalues i = 1/10000 {
        gen randnum = runiform()
        sort randnum
        
        gen perm_treatment = 0
        replace perm_treatment = 1 in 1/185  

        reg re78 perm_treatment 

        frame fs_perm: replace coeff = _b[perm_treatment] in `i'

        drop randnum perm_treatment

        _dots `i' 0
    }
}

frame fs_perm {
    count if abs(coeff) >= abs(obs_coef)
    scalar p_value = r(N) / 10000
    di "Fisher's exact p-value: " p_value
}

* In this case we get a p-value of 0.0042

/* COMMENT TO 4b 

1. Describe fisher inference 
Fisher inference, or randomization inference, is based on the idea that in another state of the world there might have been a different random treatment assignment, with different observed treatment and control groups. The null hypothesis that is typically tested is called "sharp null hypothesis", and it is that every unit has the same potential outcome (i.e., the treatment has no effect on individuals). To test this hypothesis, one first computes the sample statistic of interest, which is usually the mean difference between the observed outcomes of the two groups. Then, one obtains the distribution of the outcome under the null by randomly reassigning the treatment and control statuses to the units in the sample n times (usually 10,000 permutations), each time recalculating the statistic. In doing so, we observe how much the outcome variable varies due to random chance in absence of a treatment effect (i.e., under the null). Finally, the Fisher p-value will be given by the fraction of recalculated statistics that are more extreme than the one observed in the original RCT.

2. Do you arrive at the same p-value? (why / why not)
Our p-value is close to the 0.0044 level reported in the reference paper. However, across multiple runs, our p-value exhibits some marginal variabilty. This is not suprising given how randomization inference works: each permutation involves a different random reassignment of treatment and control statuses, leading to slight variations in the computed test statistic (even with a large number of resampling iterations).
*/




********* 4c *********

* LaLonde considered in his paper treatment and control groups that include units coming from different sites. According to the randomization plan of the NSW program outlined in the Manpower Demonstration Research Corporation (1983), cited by LaLonde (1986), the randomization has been made at the site level (10 out of 15 sites were selected for random assignment). Therefore, Athey and Imbens, when illustrating Fisherian inference, are not precise in doing the re-randomization of treatment assignment since they are exploring assignment scenarios, under the null hypothesis, that could have never happen given the type of randomization made. In other words, the distribution under the null might be wrong, giving back a wrong p-value. Overall, the crucial point when performing randomization inference is that the resampling must respect the original treatment assignment method (Heß, 2017). For example, in cases of treatment assignment at the cluster level (ex. site, plant or village) the resampling must align with the original treatment assignment method. 



********* 4d *********

*** 4d.1 ***

* The HC1 version of the std errors is consistent but biased under homoskedasticity since it relies on a biased estimate of the variance of the individual coefficient. Indeed, its expected value under homoskedasticity differs from the actual individual variance. Being biased but consistent, this leads the HC1 version to perform poorly in small samples, while they become quite acceptable when the sample size is large enough. The HC3 version of the std error is the HC1 version corrected by rescaling each individual OLS squared residual by the square of the correspondent diagonal element of the hat matrix. This correction allows to eliminate the bias under homoskedascity and, more specifically, to give less weight to the outliers (in terms of the value of the independent variable). Indeed, the i-th element of the diagonal of the hat matrix is 1 - leverage score. Higher leverage score indicates higher distance of the i-th observation of the independent variable from the other observations of such variable.

*** 4d.2 ***

use "$filepath\jtrain2.dta", replace

reg re78 train, vce(hc3)

count if train == 1
local treated = r(N)

count if train == 0
local control = r(N)


reg re78 train age educ black hisp, vce(hc3)

count if train == 1
local treated = r(N)

count if train == 0
local control = r(N)


reg re78 train age educ black hisp re74 re75, vce(hc3)

count if train == 1
local treated = r(N)

count if train == 0
local control = r(N)



*** 4d.3 ***

use "$filepath\jtrain2.dta", replace

reg re78 train, vce(bootstrap, reps(1000))

count if train == 1
local treated = r(N)

count if train == 0
local control = r(N)


reg re78 train age educ black hisp, vce(bootstrap, reps (1000))

count if train == 1
local treated = r(N)

count if train == 0
local control = r(N)


reg re78 train age educ black hisp re74 re75, vce(bootstrap, reps (1000))

count if train == 1
local treated = r(N)

count if train == 0
local control = r(N)


/* The bootstrapping approach mimics resampling from the population by resampling with replacement multiple times from the observed data, under the assumption the sample at hand is represenative of the population. In more detail: 
- Each bootstrap sample is drawn independently with replacement, ensuring variability across samples. Replacement allows the same observation to enter the same sample multiple tiems, thus eventually assigning it more weight than other observations. 
- n resamples are then performed 
- The bootstrapped standard errors are then calculated as the std deviation of the estimate calculated across the various sample drawn. 
*/

*** 4d.4 ***

* Standard errors slightly change using HC3 compared to the initial scenario. A reason for this change, even if little, might be the presence of heteroskedasticity in the data. However, the effect of training remains significant. When comparing results using HC1 std errors vs HC3 std errors, then the reason for which our results should not change can be found in the sample size relative to the number of regressors. Indeed, for the regression with the highest number of controls included we have 445 observations over 7 regressors, for a proportion of 63 observations per regressor. The literature suggests that from a number of 50 observations per regressor the difference in accuracy for HC1 and HC3 std errors starts to vanish.























