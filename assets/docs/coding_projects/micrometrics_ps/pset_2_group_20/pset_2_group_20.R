install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("grf")
install.packages("ggplot2")
install.packages("forcasts")

library(readr)
library(dplyr)
library(tidyr)
library(grf)
library(tidyverse)
library(grf)
library(dplyr)
library(ggplot2)
library(forcats)

############################################################################################################
########################################   Question 2.A ####################################################
############################################################################################################

# ===============================================
#               I. DATA PREPARATION
# ===============================================

# Load data
data <- read_csv("/Users/Alessandro/Desktop/micrometrics_stata/files/expanded_data.csv")

# Select counties that adopted early (1969–1973) or late (2000)
df_law <- data %>%
  select(st, county_id, lfdivlaw) %>%
  distinct() %>%
  filter(lfdivlaw %in% c(1969:1973, 2000)) %>%
  mutate(treated = as.numeric(lfdivlaw %in% 1969:1973))
print(df_law)

# Compute first difference in div_rate_sim between 1968 and 1978
df_diff <- data %>%
  filter(year %in% c(1968, 1978)) %>%
  semi_join(df_law, by = c("st", "county_id")) %>%
  group_by(st, county_id, year) %>%
  summarise(div_rate_sim = mean(div_rate_sim, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = div_rate_sim, names_prefix = "div_rate_") %>%
  mutate(dv_diff = div_rate_1978 - div_rate_1968) %>%
  left_join(df_law, by = c("st", "county_id"))

# Get 1968 covariates only
pre_covariates <- data %>%
  filter(year == 1968) %>%
  semi_join(df_law, by = c("st", "county_id")) %>%
  select(st, county_id,
         education_rate, childcare_availability, unemployment_rate,
         median_income, urbanization, marriage_rate, religious_adherence,
         alcohol_consumption, domestic_violence_rate,
         women_labor_force_participation, housing_cost, crime_rate,
         social_services_spending)

# Merge everything – one observation per county
df_final <- df_diff %>%
  left_join(pre_covariates, by = c("st", "county_id")) %>%
  drop_na()

df_final <- df_final %>%
  mutate(urban_dummy = as.numeric(urbanization == "Urban"))

# Define inputs
Y <- as.numeric(df_final$dv_diff)
W <- as.numeric(df_final$treated)
X <- model.matrix(~ education_rate + childcare_availability + unemployment_rate +
                    median_income + marriage_rate + religious_adherence +
                    alcohol_consumption + domestic_violence_rate + women_labor_force_participation +
                    housing_cost + crime_rate + social_services_spending  + urban_dummy - 1,
                  data = df_final)


# ===============================================
#        TRAINING OF THE CAUSAL FOREST
# ===============================================

cf <- causal_forest(X, Y, W, num.trees = 2000, seed = 200, honesty = TRUE) 

# Predict CATEs (out-of-bag) CONDITIONAL ATE (AVG IS ATE!)
tau.hat.oob <- predict(cf)

# Plot distribution of estimated CATEs
hist(tau.hat.oob$predictions,
     main = "Distribution of Estimated CATEs",
     xlab = "Estimated Treatment Effect (CATE)",
     ylab = "Number of Counties",
     col = "skyblue", border = "white")


ate <- average_treatment_effect(cf)
catt <- average_treatment_effect(cf, target.sample = "treated")

# Print with standard errors and 95% CI
cat("Average Treatment Effect (ATE):", 
    round(ate[1], 5), 
    " (SE =", round(ate[2], 5), 
    ", 95% CI = [", round(ate[1] - 1.96 * ate[2], 5), 
    ",", round(ate[1] + 1.96 * ate[2], 5), "])\n")

cat("Conditional ATE on Treated (CATT):", 
    round(catt[1], 4), 
    " (SE =", round(catt[2], 4), 
    ", 95% CI = [", round(catt[1] - 1.96 * catt[2], 4), 
    ",", round(catt[1] + 1.96 * catt[2], 4), "])\n")


"The ATE is positive but not statistically significant. It is then consistent with the result found in
point 1.c, where, after controlling for state fixed effects we obtained not significant result. In this context the ATE is
calculated as a weighted average of all the CATEs estimated by the causal forest."




# Test calibration
test_calibration(cf)
"The p-value of the 'differential.forest.prediction' coefficient also acts as an omnibus test for the presence of heterogeneity: 
If the coefficient is significantly greater than 0, then we can reject the null of no heterogeneity. In our case we detect the 
presence of heterogeneity as the coefficient is significantly greater than 0"


################################################################################################################################################
#########                                                   Question 2.B                                                    ####################
################################################################################################################################################


# ===============================================
#                   Point (i)
# ===============================================

#  Set top-k variable count
k <- 10

#  Rank variable importance
varimp <- variable_importance(cf)
ranked.vars <- order(varimp, decreasing = TRUE)

#  Get top-k variable names (optional print)
top_k_names <- colnames(X)[ranked.vars[1:k]]
print(top_k_names)

# Compute BLP on top-k variables
X_topk <- X[, ranked.vars[1:k]]
blp <- best_linear_projection(cf, X_topk)
print (blp)

# Convert to dataframe and clean
blp_df <- as.data.frame(matrix(blp, ncol = 4))
colnames(blp_df) <- colnames(blp)
blp_df$variable <- rownames(blp)

# Rename columns and filter
blp_df <- blp_df %>%
  rename(
    coef = 'Estimate',
    se   = 'Std. Error',
    t    = 't value',
    p    = 'Pr(>|t|)'
  ) %>%
  filter(variable != "(Intercept)") %>%
  mutate(variable = fct_reorder(variable, abs(coef)))

# Fancy plot
ggplot(blp_df, aes(x = variable, y = coef)) +
  geom_point(size = 3, color = "#B22222") +
  geom_errorbar(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
                width = 0.15, color = "#003366", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  coord_flip() +
  labs(
    title = "Best Linear Projection of Conditional Treatment Effects",
    x = NULL,
    y = "Coefficient Estimate (95% CI)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )


"The BLP allows us to summarize the treatment effects of the top k variables 
in terms of heterogeneity of the treatment. It does this by running a linear 
regression model where the outcome variable is the CATE and the independent variables
are the top covariates in terms of heterogeneity of the effects that we obtain
analyzing the variable importance after having run the causal forest. 
In our results the significant coefficients are the following:
1) The CATE reduces significantly among counties with high levels of religious adherence (1% significance level) 
2) The CATE increases among counties with with higher female labor market participation (5% significance level),
3) The CATE increases among counties with with higher domestic violence rate (10% sigificance level),
For the other variables there does not seem to be a significant effect on the CATE.

Interpretation-wise, it appears reasonable to us that religion may play a role in shaping women culture, thus reducing their propensity
to resort to the newly introduced divorce law. Similarly, it seems sound that higher female labor market participation means higher (n not only 
financial) autonomy for women, and it seems reasonable that this could have correlated with unilateral divorse. Lastly, one could argue
that domestic violence may reasonable lead women to terminate unilaterealy marriage."



# =============================================================================
#                   Point (ii) Targeting Operator Characteristic (TOC)
# =============================================================================


# Train a CATE estimator on a training set.
set.seed(123) 
n <- nrow(X)
train <- sample(1:n, n / 2)
cf.cate <- causal_forest(X[train, ], Y[train], W[train]) #, honesty = FALSE


# Predict treatment effects on a held-out test set.
test <- -train
cate.hat <-  predict(cf.cate, X[test, ])$predictions

# Fit an evaluation forest for estimating the RATE.
cf.eval <- causal_forest(X[test, ], Y[test], W[test])

# Form a doubly robust RATE estimate on the held-out test set.
rate <- rank_average_treatment_effect(cf.eval, cate.hat)

# Plot the Targeting Operator Characteristic (TOC) curve.
plot(rate)

rate$estimate + 1.96*c(-1, 1)*rate$std.err

rate


"The TOC is a measure that allows us to see which is the benefit of treating only a certain fraction of
individuals that, according to our CATE estimates, are shown to be those who benefit the most from the
treatment (e.g. the TOC quantifies the incremental benefit of treating only the 20% with the largest estimated 
CATEs compared to the overall ATE). In practice, units are ranked based on their on their estimated CATE (from higher to lower) 
and the percentile of such distribution are reported on the x axis. On the y axis instead is reported the
ATE calculated across each fraction of the population listed on the x axis, to which is subtracted the overall ATE.
To be more specific, the units on the x axis belong to a test sample and are ordered based on
the CATE estimated on a training sample. The ATE reported on the Y axis is calculated directly on the test sample.
The area under the TOC curve (AUTOC) is the RATE (rank average treatment effect). The higher the AUTOC 
the more our estimated CATE does well in identifying individuals that most benefit from the treatment.
If there is barely any heterogeneity in 𝜏(𝑋𝑖) this area will be vanishingly small and in the special case where 
𝜏(𝑋𝑖) is constant, it’s zero. If our estimated rule 𝑆(𝑋𝑖) does well in identifying the individuals with 
very different benefits of treatment, we would expect this metric to be positive.
Conversely, if it does badly, or there are barely any benefits to stratifying treatment,
we would expect it to be negative or close to zero, respectively.

The TOC shows how well a prioritization rule (e.g., CATEs) ranks units by actual treatment effect.
Hence it allows me to check if my model correctly ranked observations based on how much they benefit. 
Policy relevance: You may want to target treatment (e.g. a subsidy) only to the most responsive units"

"In our case we see that the AUTOC estiamte is large and significant (0.2605612 with se equal to 0.102064). Additionally,
it displays a decreasing trend. Considering the figures, the top 20% of individuals in terms of CATE have an excess ATE of approximately + 0.6 than
the overall ATE. The TOC suggests that units ranked by the estimated CATEs benefit more than average from the program, 
and AUTOC is significant at conventional levels (0.2605612 / 0.102064)"


#EXTRAS: TOC by Specific characteristics

#A. Religious adherence

rate.religious_adherence <- rank_average_treatment_effect(
  cf,
  -1 * X[, "religious_adherence"],
  subset = !is.na(X[, "religious_adherence"])
)

plot(
  rate.religious_adherence,
  xlab = "Treated fraction",
  ylab = "Estimated CATE",
  main = "TOC by Religious Adherence"
)
rate.religious_adherence

#B. Women_labor_force_participation
women_labor_force_participation <- rank_average_treatment_effect(
  cf,
  -1 * X[, "women_labor_force_participation"],
  subset = !is.na(X[, "women_labor_force_participation"])
)

plot(
  women_labor_force_participation,
  xlab = "Treated fraction",
  ylab = "Estimated CATE",
  main = "TOC by women_labor_force_participation"
)
women_labor_force_participation

#C. Domestic_violence_rate
domestic_violence_rate <- rank_average_treatment_effect(
  cf,
  -1 * X[, "domestic_violence_rate"],
  subset = !is.na(X[, "domestic_violence_rate"])
)

plot(
  domestic_violence_rate,
  xlab = "Treated fraction",
  ylab = "Estimated CATE",
  main = "TOC by domestic_violence_rate"
)

# The TOC for these individual covariates presents AUTOC estimates that are significant (see plots). 

#==============================================================================================================================================
#          (iii) Plot the distribution of CATEs throughout the distribution of the variables you believe could drive heterogeneity
#==============================================================================================================================================

library(ggplot2)
# Create a data frame with CATEs and top 5 predictors
df_plot <- as.data.frame(X[, ranked.vars[1:3]])
df_plot$CATE <- tau.hat.oob$predictions

# Reshape to long format for faceted plotting
df_long <- df_plot %>%
  pivot_longer(cols = -CATE, names_to = "Variable", values_to = "Value")

# Plot: CATE vs each top variable
ggplot(df_long, aes(x = Value, y = CATE)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  facet_wrap(~ Variable, scales = "free_x") +
  labs(title = "CATE vs Top Predictors of Heterogeneity",
       x = "Variable Value", y = "Estimated CATE") +
  theme_minimal()


"In this analysis we see how the CATE varies conditional on the values of the variables that,
according to our analysis, we believe to be the ones that drive heterogeneity the most. Consistently
with what seen in the BLP we notice that there is high heterogeneity in the treatment effect depending
on the level of religious adherence. In particular the treatment effect tend to be higher
and positive for those countries with low levels of religious adherence. Moreover, 
the higher domestic violence rate and the higher women labor force participation the higher the effect of the treatment" 


################################################################################################################################################
#########                                                   Question 2.C                                                                #######
################################################################################################################################################


"Building on the previous points:
- The cf calibration test highlights the presence of heterogeneity: 
- The Overall TOC shows that there are heterogenous benefits
- The TOC per each variable present statistically significant heterogenous effects
- The plots of the distribution of CATEs throughout the distribution of the variables represent compelling evidence about the 
fact that these units may be driving heterogeneity.
"


################################################################################################################################################
#########                                                   Question 2.D                                                                 #######
################################################################################################################################################



cf <- causal_forest(X, Y, W, num.trees = 2000, seed = 200, honesty = FALSE) 

# Predict CATEs (out-of-bag) CONDITIONAL ATE (AVG IS ATE!)
tau.hat.oob <- predict(cf)

# Plot distribution of estimated CATEs
hist(tau.hat.oob$predictions,
     main = "Distribution of Estimated CATEs",
     xlab = "Estimated Treatment Effect (CATE)",
     ylab = "Number of Counties",
     col = "skyblue", border = "white")

ate <- average_treatment_effect(cf)


cat("Average Treatment Effect (ATE):", 
    round(ate[1], 4), 
    " (SE =", round(ate[2], 4), 
    ", 95% CI = [", round(ate[1] - 1.96 * ate[2], 4), 
    ",", round(ate[1] + 1.96 * ate[2], 4), "])\n")


"Honesty is a procedure through which we impose (1) the tree growth (i.e. the splits decisions) 
and(2) the estimation of the CATEs within each leaf to happen  across two different subsamples 
of the original one. Overall, every observation is used to perform only one of the two steps of the analysis. 
One subsample is used for splitting nodes (so to define the tree structure). One is for estimating treatment effects (CATEs) within leaves.
This is important in the estimation of CATEs since it allows us to eliminate sample-specific biases 
(overfitting) and especially to obtain valid confidence intervals,asymptotic normality and consistency of the estimator. 
When using non-honest trees, the variance of the outcome is reduced to grow the tree since it has been
used as an input to build it.
On the other hand non-honest trees use the same data to  grow the tree (decide splits) and estimate treatment effects (CATEs).
This can lead to overfit especially when we have small sample sizes, shallow forests (few trees) and small leaf size.

In our case the the ATE estimated by the dishonest model is virtually identical to that produced by the honest causal forest.
We hypothesize that this similarity arises from the model having been trained with a relatively large number of trees and with
leaves that were not excessively small, otherwise we should expect results to be different.
However,the model is clearly much more sensitive to outliers. "



#REPETITION OF THE FULL ANALYSIS WITH honesty = FALSE
# ===============================================
#        TRAINING OF THE CAUSAL FOREST
# ===============================================

cf <- causal_forest(X, Y, W, num.trees = 2000, seed = 200, honesty = FALSE) 

# Predict CATEs (out-of-bag) CONDITIONAL ATE (AVG IS ATE!)
tau.hat.oob <- predict(cf)

# Plot distribution of estimated CATEs
hist(tau.hat.oob$predictions,
     main = "Distribution of Estimated CATEs",
     xlab = "Estimated Treatment Effect (CATE)",
     ylab = "Number of Counties",
     col = "skyblue", border = "white")


ate <- average_treatment_effect(cf)
catt <- average_treatment_effect(cf, target.sample = "treated")

# Print with standard errors and 95% CI
cat("Average Treatment Effect (ATE):", 
    round(ate[1], 5), 
    " (SE =", round(ate[2], 5), 
    ", 95% CI = [", round(ate[1] - 1.96 * ate[2], 5), 
    ",", round(ate[1] + 1.96 * ate[2], 5), "])\n")

cat("Conditional ATE on Treated (CATT):", 
    round(catt[1], 4), 
    " (SE =", round(catt[2], 4), 
    ", 95% CI = [", round(catt[1] - 1.96 * catt[2], 4), 
    ",", round(catt[1] + 1.96 * catt[2], 4), "])\n")

# Test calibration
test_calibration(cf)


################################################################################################################################################
#########                                                   Question 2.B                                                    ####################
################################################################################################################################################


# ===============================================
#                   Point (i)
# ===============================================

#  Set top-k variable count
k <- 10

#  Rank variable importance
varimp <- variable_importance(cf)
ranked.vars <- order(varimp, decreasing = TRUE)

#  Get top-k variable names (optional print)
top_k_names <- colnames(X)[ranked.vars[1:k]]
print(top_k_names)

# Compute BLP on top-k variables
X_topk <- X[, ranked.vars[1:k]]
blp <- best_linear_projection(cf, X_topk)
print (blp)

# Convert to dataframe and clean
blp_df <- as.data.frame(matrix(blp, ncol = 4))
colnames(blp_df) <- colnames(blp)
blp_df$variable <- rownames(blp)

# Rename columns and filter
blp_df <- blp_df %>%
  rename(
    coef = 'Estimate',
    se   = 'Std. Error',
    t    = 't value',
    p    = 'Pr(>|t|)'
  ) %>%
  filter(variable != "(Intercept)") %>%
  mutate(variable = fct_reorder(variable, abs(coef)))

# Fancy plot
ggplot(blp_df, aes(x = variable, y = coef)) +
  geom_point(size = 3, color = "#B22222") +
  geom_errorbar(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
                width = 0.15, color = "#003366", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  coord_flip() +
  labs(
    title = "Best Linear Projection of Conditional Treatment Effects",
    x = NULL,
    y = "Coefficient Estimate (95% CI)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )


# =============================================================================
#                   Point (ii) Targeting Operator Characteristic (TOC)
# =============================================================================


# Train a CATE estimator on a training set.
set.seed(123)  
n <- nrow(X)
train <- sample(1:n, n / 2)
cf.cate <- causal_forest(X[train, ], Y[train], W[train], honesty = FALSE) #, honesty = FALSE


# Predict treatment effects on a held-out test set.
test <- -train
cate.hat <-  predict(cf.cate, X[test, ])$predictions

# Fit an evaluation forest for estimating the RATE.
cf.eval <- causal_forest(X[test, ], Y[test], W[test], honesty = FALSE)

# Form a doubly robust RATE estimate on the held-out test set.
rate <- rank_average_treatment_effect(cf.eval, cate.hat)

# Plot the Targeting Operator Characteristic (TOC) curve.
plot(rate)

rate$estimate + 1.96*c(-1, 1)*rate$std.err

rate

#EXTRAS: TOC by Specific characteristics

#A. Religious adherence

rate.religious_adherence <- rank_average_treatment_effect(
  cf,
  -1 * X[, "religious_adherence"],
  subset = !is.na(X[, "religious_adherence"])
)

plot(
  rate.religious_adherence,
  xlab = "Treated fraction",
  ylab = "Estimated CATE",
  main = "TOC by Religious Adherence"
)
rate.religious_adherence

#B. Women_labor_force_participation
women_labor_force_participation <- rank_average_treatment_effect(
  cf,
  -1 * X[, "women_labor_force_participation"],
  subset = !is.na(X[, "women_labor_force_participation"])
)

plot(
  women_labor_force_participation,
  xlab = "Treated fraction",
  ylab = "Estimated CATE",
  main = "TOC by women_labor_force_participation"
)
women_labor_force_participation

#C. Domestic_violence_rate
domestic_violence_rate <- rank_average_treatment_effect(
  cf,
  -1 * X[, "domestic_violence_rate"],
  subset = !is.na(X[, "domestic_violence_rate"])
)

plot(
  domestic_violence_rate,
  xlab = "Treated fraction",
  ylab = "Estimated CATE",
  main = "TOC by domestic_violence_rate"
)



#==============================================================================================================================================
#          (iii) Plot the distribution of CATEs throughout the distribution of the variables you believe could drive heterogeneity
#==============================================================================================================================================

library(ggplot2)
# Create a data frame with CATEs and top 5 predictors
df_plot <- as.data.frame(X[, ranked.vars[1:3]])
df_plot$CATE <- tau.hat.oob$predictions

# Reshape to long format for faceted plotting
df_long <- df_plot %>%
  pivot_longer(cols = -CATE, names_to = "Variable", values_to = "Value")

# Plot: CATE vs each top variable
ggplot(df_long, aes(x = Value, y = CATE)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  facet_wrap(~ Variable, scales = "free_x") +
  labs(title = "CATE vs Top Predictors of Heterogeneity",
       x = "Variable Value", y = "Estimated CATE") +
  theme_minimal()







############################################################################################################
#########                                      APPENDIX:                                            ########
############################################################################################################

#A. CHECK OVERLAP THROUGH PSCORE + BALANCING CHECKS

# Estimate propensity scores
ps_model <- regression_forest(X, W, num.trees = 1000)
pscore <- predict(ps_model)$predictions

# Define IPW weights
IPW <- ifelse(W == 1, 1 / pscore, 1 / (1 - pscore))

# Histogram of propensity scores
hist(pscore, breaks = 30, col = "skyblue", border = "white",
     main = "Histogram of Estimated Propensity Scores",
     xlab = "P(Treated | X)", ylab = "Number of Counties")

# Prepare data for weighted covariate plots
n <- nrow(X)
p <- ncol(X)
plot.df.cont <- data.frame(
  value = as.vector(X),
  variable = rep(colnames(X), each = n),
  W = as.factor(rep(W, times = p)),
  IPW = rep(IPW, times = p)
)

# Plot weighted densities
p1 <- ggplot(plot.df.cont, aes(x = value, weight = IPW, color = W, fill = W)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  labs(
    title = "IPW-Weighted Continuous Covariate Densities",
    x = "Value",
    y = "Weighted Density",
    fill = "Treatment",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 13)

# Urbanization covariate example
urban_df <- df_final %>%
  select(W, urbanization) %>%
  mutate(
    IPW = ifelse(W == 1, 1 / pscore, 1 / (1 - pscore)),
    W = as.factor(W),
    urbanization = as.factor(urbanization)
  )

# Show plot
p1

