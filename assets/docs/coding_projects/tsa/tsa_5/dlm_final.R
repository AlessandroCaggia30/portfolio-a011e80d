# ------------------------------------------------------------
# TIME SERIES ANALYSIS – ASSIGNMENT 5
# Dynamic Linear Models (DLMs) on GISTEMP monthly anomalies
# ------------------------------------------------------------

# ---------------------------
# 0. LOAD REQUIRED PACKAGES
# ---------------------------
library(dlm)        # Dynamic Linear Models
library(tidyverse)  # Data manipulation
library(zoo)        # Date conversion utilities
library(ggplot2)    # Visualization
library(RColorBrewer)
library(tidyr)


"Summary: The first two bullet points are affressed by the code below. 
We developed four models within the dynamic linear model (DLM) framework. As a baseline, we estimated a Random Walk Plus Noise (RWPN) model and a Locally
Linear Trend (LLT) model. We subsequently expanded the LLT by incorporating seasonal components using the dlmModSeas and dlmModTrig functions. The RWPN and
LLT models were estimated on the deseasonalized time series, while the Classical Seasonal and Fourier Seasonal models were estimated directly on the raw
series. All models were estimated by maximum likelihood using the dlmMLE function, with state estimation performed via Kalman filtering. Convergence was 
achieved for all optimization routines."


############################################################
# Index: 
# ADDRESSING BULLET POINT 1:
# A. Data Processing
# B. Defining and fitting the model
#-- I. Random Walk Plus Noise (RWPN) model definition
#-- II. Locally Linear Trend (LLT) model definition
#-- III. Polynomial + Seasonal MODEL
# C. Estimate MLE parameters
# D. Build final model with estiamted parameters 

# ADDRESSING BULLET POINT 2:
# E. Kalman Smoothing: extract latent states
# F. VISUALIZE: plot observed vs smoothed level
### I. RECONSTRUCT SMOOTHED SERIES for RWPN and LLT
### II.RECONSTRUCT SMOOTHED SERIES for DlmModSeas expansion and plot forecast
### III. EXTRA: rerun the analysis with fourier decomposition
### IV. LATENT COMPONENT EXTRACTION AND VISUALIZATION – ALL COMPONENTS, COMPREHENSIVE SYNTHESIS

# ADDRESSING BULLET POINT 3: 
# G. GLOBAL MODEL COMPARISON: RWPN vs LLT vs Classical Seasonal vs Fourier Seasonal
# H. ADDITIONAL ANALYSIS RUN WITH THE FITTED DLMSEAS MODEL

# ADDRESSING BULLET POINT 4: 
# I. COMPARISON WITH HMM MODELS

# APPENDIX: STUDY OF THE GISTEMP DATA



##############################################################
# A. PREPARE DATA: Load, reshape, deseasonalize
##############################################################

data <- read.csv("/Users/Alessandro/Desktop/gistemp.txt")


# 1. Load GISTEMP dataset and reshape wide → long
monthly_data <- data %>%
  select(Year, Jan:Dec) %>%                # Keep Year and 12 months
  pivot_longer(-Year, names_to = "Month", values_to = "Temp")  # Long format

# Map month names → numeric (Jan = 1, ..., Dec = 12)
monthly_data$Month <- match(monthly_data$Month, month.abb)

# Create complete date (YYYY-MM-DD) for each month
monthly_data <- monthly_data %>%
  arrange(Year, Month) %>%
  mutate(date = as.Date(paste(Year, Month, "01", sep = "-")))

# Convert to monthly time series: starts Jan 1880
ts_monthly <- ts(monthly_data$Temp, start = c(1880, 1), frequency = 12)


# 2. Remove Seasonal Component: prepare input for trend-only models
"In this step, we preprocess the original monthly temperature anomalies to remove seasonality before fitting
trend-only models. We apply STL decomposition (Seasonal-Trend decomposition using Loess) to decompose the time
series into trend, seasonal, and remainder components. We extract the estimated seasonal component and subtract
it from the original series to obtain a deseasonalized time series that retains only trend and irregular
fluctuations. This is necessary because models like the Random Walk Plus Noise (RWPN) and the Locally 
Linear Trend (LLT) do not explicitly model seasonal patterns; by removing the seasonal cycle in advance, we
ensure that these models focus solely on modeling the underlying trend and stochastic variation, without being
confounded by periodic seasonal effects. We use s.window = 13 to apply moderate smoothing in estimating
the seasonalpattern."

# STL decomposition (locally weighted smoothing) to extract seasonal
stl_fit <- stl(ts_monthly, s.window = 13)  # s.window = 13: smoother seasonal estimate

# Isolate seasonal component
seasonal_part <- stl_fit$time.series[, "seasonal"]

# Deseasonalized series (Y_t - seasonal_t)
ts_deseason <- ts(ts_monthly - seasonal_part, start = c(1880,1), frequency = 12)




##############################################################
# B. DEFINE AND FIT THE MODELS
##############################################################

# ------------------------------------------------------------
# I. Random Walk Plus Noise (RWPN) model definition
# ------------------------------------------------------------

"This function builds a *random walk plus noise* model (also known as **local level model**).
This is a polynomial DLM of order 1 (dlmModPoly(order=1)), modeling a latent level µ_t evolving as a random walk.
Observation equation:   Y_t = µ_t + v_t   where v_t ~ N(0, dV) [obs error]
State equation:         µ_t = µ_{t-1} + w_t   where w_t ~ N(0, dW) [state error]
Key features:
- Only one latent state (the level µ_t)
- Used when the data has no deterministic trend but exhibits local level fluctuations.
- Equivalent to ARIMA(0,1,1) under certain conditions.
The parameter `param` is a vector with:
  param[1] = observation variance (dV)
  param[2] = state variance (dW)
Initial level is set to the first observation value (m0 = ts_deseason[1])"

build_rwpn <- function(param) {
  dlmModPoly(order = 1, dV = param[1], dW = param[2], m0 = ts_deseason[1])
  # order = 1: only level; dV: obs variance; dW: state variance; m0: init level
}

# ------------------------------------------------------------
# II. Locally Linear Trend (LLT) model definition
# ------------------------------------------------------------
"This function builds a *locally linear trend* model (also known as **local linear growth model**).
This is a polynomial DLM of order 2 (dlmModPoly(order=2)), modeling:
- Level µ_t
- Slope β_t (time-varying slope)
Observation equation:   Y_t = µ_t + v_t   where v_t ~ N(0, dV)
State equations:        µ_t = µ_{t-1} + β_{t-1} + w1_t   where w1_t ~ N(0, dW[1])
                        β_t = β_{t-1} + w2_t              where w2_t ~ N(0, dW[2])
Key features:
- Captures both level and slope dynamics (useful for smoother or growing trends).
- Equivalent to ARIMA(0,2,2) under certain assumptions.
The parameter `param` is a vector with:
  param[1] = observation variance (dV)
  param[2] = level evolution variance (dW[1])
  param[3] = slope evolution variance (dW[2])"

build_llt <- function(param) {
  mod <- dlmModPoly(order = 2)     # order = 2: level + slope
  mod$V <- matrix(param[1])        # dV: observation variance
  mod$W <- diag(param[2:3])        # dW: 2×2 diagonal for level + slope noise
  return(mod)
}


# ------------------------------------------------------------
# III. Polynomial + Seasonal MODEL
# ------------------------------------------------------------

"This function defines a Dynamic Linear Model (DLM) combining a polynomial trend component (dlmModPoly) and a classical
seasonal component (dlmModSeas). The seasonal part is modeled as a seasonal factor model with a state vector of dimension
12, capturing monthly effects under the constraint that seasonal factors sum to zero. The state equation cyclically 
rotates the seasonal factors across months, ensuring the correct seasonal pattern repeats every 12 observations. 
The seasonal variance parameter controls the stochastic evolution of the seasonal component over time, allowing
the model to capture both fixed and slowly varying seasonality. This additive structure aligns with the classical 
decomposition: observed series = trend + seasonal + irregular.
p[1]: observation noise variance (Var(ε_t))
p[2]: level evolution variance (Var(η_level,t))
p[3]: slope evolution variance (Var(η_slope,t))
p[4]: seasonal component variance (Var(η_seasonal,t))
"

build_monthly_dlm_seas <- function(p) {
  dlmModPoly(order = 2, dV = p[1], dW = p[2:3]) +  # order=2: local level + slope
    dlmModSeas(frequency = 12, dV = p[4])          # classical seasonal with 12 months
}








# ------------------------------------------------------------
# C. ESTIMATE MLE PARAMETERS
# ------------------------------------------------------------

"This block estimates the unknown variance parameters of the Random Walk Plus Noise (RWPN) and Locally Linear 
Trend (LLT) models using Maximum Likelihood Estimation (MLE). The function dlmMLE numerically maximizes the 
log-likelihood of the deseasonalized time series under each model structure. For the RWPN model, we estimate
two parameters: observation variance (dV) and state variance (dW). For the LLT model, we estimate three parameters:
observation variance (dV), level variance (dW_level), and slope variance (dW_slope). Initial values are set to 100, 
and positivity constraints are imposed with lower bounds. The Hessian matrix is computed to obtain asymptotic standard
errors by inverting the observed Hessian. The final outputs are the MLE parameter estimates and their corresponding 
standard errors for each model."

# MLE for RWPN: 2 parameters (dV, dW)
mle_rwpn <- dlmMLE(
  ts_deseason,
  parm = rep(100, 2),              # Initial guess: both variances = 100
  build = build_rwpn,
  lower = c(1e-5, 0),              # lower bounds (variances ≥ 0)
  hessian = TRUE                   # compute Hessian for std errors
)

# Display results
mle_rwpn$par                      # MLE estimates
sqrt(diag(solve(mle_rwpn$hessian)))  # Asymptotic standard errors

# MLE for LLT: 3 parameters (dV, dW_level, dW_slope)
mle_llt <- dlmMLE(
  ts_deseason,
  parm = rep(100, 3),              # Initial guess
  build = build_llt,
  lower = rep(1e-5, 3),
  hessian = TRUE
)

# Display results
mle_llt$par
sqrt(diag(solve(mle_llt$hessian)))

# Initial parameter guesses (reasonable starting values)
init_p <- c(var(ts_monthly), 0.01, 0.01, 0.01)
mle_mth_seas <- dlmMLE(
  ts_monthly,
  parm = init_p,
  build = build_monthly_dlm_seas,
  lower = rep(1e-6, 4),              # ensure variances ≥ 0
  hessian = TRUE,
  control = list(maxit = 1000)
)

# Ensure convergence (convergence == 0 indicates success)
stopifnot(mle_mth_seas$convergence == 0)



# ------------------------------------------------------------
# D. BUILD FINAL MODELS WITH ESTIMATED PARAMETERS
# ------------------------------------------------------------
model_rwpn <- build_rwpn(mle_rwpn$par)
model_llt <- build_llt(mle_llt$par)
mod_mth_seas <- build_monthly_dlm_seas(mle_mth_seas$par)


# ------------------------------------------------------------
# E. KALMAN SMOOTHING: extract latent states
# ------------------------------------------------------------

"The Kalman smoother computes retrospective estimates of the unobserved state sequence, conditioning each state
θ_t on the entire dataset Y₁,…,Y_T rather than just on past data. While filtering provides E(θ_t | Y₁,…,Y_t), 
smoothing provides E(θ_t | Y₁,…,Y_T), incorporating information from both past and future observations.
The algorithm runs backward from the final filtered state, recursively updating the state estimates. It 
produces smoother, less variable estimates than filtering, as it conditions on more information. In dlmSmooth(),
the output 's' contains the smoothed means for each state at each time point. This procedure is essential for
reconstructing hidden trends (e.g., level, slope) once all data are observed."

# RWPN: state dimension = 1 (level)
smooth_rwpn <- dlmSmooth(ts_deseason, model_rwpn)
level_rwpn <- dropFirst(smooth_rwpn$s)   # remove initial filter state

# LLT: state dimension = 2 (level, slope)
smooth_llt <- dlmSmooth(ts_deseason, model_llt)
states_llt <- dropFirst(smooth_llt$s)    # T × 2 matrix
level_llt <- states_llt[, 1]             # smoothed level
slope_llt <- states_llt[, 2]             # smoothed slope

# DlmModSeas expansion
s_mth_seas <- dlmSmooth(ts_monthly, mod_mth_seas)    # smoothing step
level_mth_seas <- s_mth_seas$s[-1, 1]         # smoothed level (trend intercept)
slope_mth_seas <- s_mth_seas$s[-1, 2]         # smoothed slope (trend slope)
seasonal_vec_mth_seas <- s_mth_seas$s[-1, 3]  # smoothed seasonal (1 seasonal state retained)


# ------------------------------------------------------------
# F. VISUALIZE: plot observed vs smoothed level
# ------------------------------------------------------------
# ============================================================
# no need to reconstruct?
# ============================================================

# Prepare time axis: exclude initial filtering lag
ts_deseason_adj <- window(ts_deseason, start = time(ts_deseason)[2])

# ➤ RWPN PLOT
par(mar = c(2, 4, 1, 1) + 0.1, cex = 0.7)
plot(ts_deseason_adj, col = "darkblue", lwd = 1.2,
     ylab = "Deseasonalized Temp (°C anomaly)",
     main = "RWPN: Smoothed Level vs Observed")

lines(level_rwpn, col = "goldenrod", lwd = 1.5, lty = 2)

legend("bottomleft",
       legend = c("Observed", "Smoothed Level (RWPN)"),
       col = c("darkblue", "goldenrod"),
       lty = c(1,2), lwd = c(1.2,1.5))

# ➤ LLT PLOT
par(mar = c(2, 4, 1, 1) + 0.1, cex = 0.7)
plot(ts_deseason_adj, col = "darkblue", lwd = 1.2,
     ylab = "Deseasonalized Temp (°C anomaly)",
     main = "LLT: Smoothed Level vs Observed")

lines(level_llt, col = "goldenrod", lwd = 1.5, lty = 2)

legend("bottomleft",
       legend = c("Observed", "Smoothed Level (LLT)"),
       col = c("darkblue", "goldenrod"),
       lty = c(1,2), lwd = c(1.2,1.5))


"This visualization compares the observed deseasonalized temperature anomalies to the smoothed level estimates 
produced by the Random Walk Plus Noise (RWPN) and Locally Linear Trend (LLT) models. In both plots, the smoothed 
levels closely follow the observed data, indicating a good fit with accurate reconstruction of the underlying trend. 
The smoothed series filters out short-term noise while preserving structural changes in the level.
These smoothed estimates provide a precise latent trend estimate, useful for interpretation and for extending 
forecasts beyond the observed period. The inclusion of future prediction (forecasting step) relies on the last 
smoothed state and the model dynamics to project the trend forward, offering continuity between in-sample 
smoothing and out-of-sample prediction."


# ============================================================
# II. RECONSTRUCT SMOOTHED SERIES for DlmModSeas expansion and plot forecasr
# ============================================================

# Define time index
time_full <- time(ts_monthly)
time_full_num <- as.numeric(time_full)

par(mfrow = c(3, 1), mar = c(3, 4, 2, 2))

# A. Plot latent components
# LEVEL component plot
plot(time_full_num, level_mth_seas, type = "l", col = "darkgreen", lwd = 2,
     ylab = "Level", main = "Smoothed Level (dlmModSeas)")

# SLOPE component plot
plot(time_full_num, slope_mth_seas, type = "l", col = "purple", lwd = 2,
     ylab = "Slope", main = "Smoothed Slope (dlmModSeas)")

# SEASONAL component plot
plot(time_full_num, seasonal_vec_mth_seas, type = "l", col = "orange", lwd = 2,
     ylab = "Seasonal", main = "Smoothed Seasonal Component (dlmModSeas)")


# Sum latent components: smoothed signal = level + slope + seasonal
smoothed_mth_seas <- level_mth_seas + slope_mth_seas + seasonal_vec_mth_seas
# don't need to do this actually

# B. FORECAST: Generate forecasts from smoothed model
n_forecast <- 144  # Forecast horizon: 288 months = 24 years

f_mth_seas <- dlmFilter(ts_monthly, mod_mth_seas)

# Forecast using fitted filter
forecast_mth_seas <- dlmForecast(f_mth_seas, nAhead = n_forecast)

# Forecast mean: E[Y_t|t-1] = F * a_t
forecast_mean_mth_seas <- drop(forecast_mth_seas$a %*% t(FF(mod_mth_seas)))

# Append last observed for continuity
forecast_mean_mth_seas <- c(tail(ts_monthly, 1), forecast_mean_mth_seas)

# Time index for forecast
forecast_times <- seq(
  from = max(time_full_num) + deltat(ts_monthly),
  by = deltat(ts_monthly),
  length.out = n_forecast
)

# C. PLOT OBSERVED + SMOOTHED + FORECAST (expanded forecast horizon)
# Compute combined y-axis range to fit observed, smoothed, and forecast
ymin <- min(ts_monthly, smoothed_mth_seas, forecast_mean_mth_seas, na.rm = TRUE)
ymax <- max(ts_monthly, smoothed_mth_seas, forecast_mean_mth_seas, na.rm = TRUE)

plot(time_full_num, ts_monthly, type = "l", col = "darkblue", lwd = 1.5,
     ylab = "Value", xlab = "Time",
     main = "Forecast with Smoothed Model (dlmModSeas)",
     xlim = c(min(time_full_num), max(forecast_times)),
     ylim = c(ymin, ymax))

lines(time_full_num, smoothed_mth_seas, col = "goldenrod", lwd = 0.7)  # smoothed fit
lines(forecast_times, forecast_mean_mth_seas[-1], col = "red", lwd = 2, lty = 2)  # forecast

legend("topleft",
       legend = c("Observed", "Smoothed", "Forecast"),
       col = c("darkblue", "goldenrod", "red"),
       lty = c(1, 1, 2),
       lwd = c(1.5, 2, 2))



# ============================================================
# III. EXTRA: FOURIER SEASONAL COMPONENT 
# ============================================================

# 0. Setup: Build DLM with Polynomial + Fourier
build_monthly_dlm_trig <- function(p) {
  # p[1] = obs noise var (measurement)
  # p[2:3] = level and slope evolution variances
  # p[4] = seasonal state noise variance
  dlmModPoly(2, dV = p[1], dW = p[2:3]) + dlmModTrig(12, dV = p[4])
}

# Initial values
init_p <- c(var(ts_monthly), 0.01, 0.01, 0.01)

# Estimate via Maximum Likelihood
mle_mth_trig <- dlmMLE(ts_monthly, parm = init_p, build = build_monthly_dlm_trig,
                       lower = rep(1e-6, 4), hessian = TRUE,
                       control = list(maxit = 1000))

stopifnot(mle_mth_trig$convergence == 0)  # Make sure optimization succeeded

# Build final model with estimated parameters
mod_mth_trig <- build_monthly_dlm_trig(mle_mth_trig$par)


# 1. Filter and Smooth
# Apply Kalman Filter and Smoother
f_mth_trig <- dlmFilter(ts_monthly, mod_mth_trig)
s_mth_trig <- dlmSmooth(ts_monthly, mod_mth_trig)

# Extract components
level_mth_trig <- s_mth_trig$s[-1, 1]  # smoothed level
slope_mth_trig <- s_mth_trig$s[-1, 2]  # smoothed slope
seasonal_mat_mth_trig <- s_mth_trig$s[-1, 3:ncol(s_mth_trig$s)]  # Fourier seasonal states

# Time axes
time_full <- time(ts_monthly)
time_full_num <- as.numeric(time_full)
months_mth <- cycle(ts_monthly)

# 2. Extract Seasonal Component
extract_seasonal_effect_trig <- function(seasonal_mat, months) {
  n <- nrow(seasonal_mat)
  q <- ncol(seasonal_mat) / 2  # number of harmonics
  seasonal <- numeric(n)
  omega <- 2 * pi / 12
  for (i in 1:n) {
    t <- months[i] - 1  # months 1–12 → 0–11
    for (h in 1:q) {
      seasonal[i] <- seasonal[i] +
        seasonal_mat[i, 2*h-1] * cos(h * omega * t) +
        seasonal_mat[i, 2*h] * sin(h * omega * t)
    }
  }
  return(seasonal)
}

seasonal_mth_trig <- extract_seasonal_effect_trig(seasonal_mat_mth_trig, months_mth)

# Full Smoothed Signal
smoothed_mth_trig <- level_mth_trig + slope_mth_trig + seasonal_mth_trig

# 3. Variance Decomposition
var_total <- var(ts_monthly, na.rm = TRUE)
var_seasonal <- var(seasonal_mth_trig, na.rm = TRUE)
var_level_slope <- var(level_mth_trig + slope_mth_trig, na.rm = TRUE)

cat("Total Variance:", var_total, "\n")
cat("Variance Explained by Seasonal Component:", var_seasonal, "\n")
cat("% of Variance Seasonal:", round(100 * var_seasonal/var_total, 2), "%\n")
cat("% of Variance Level+Slope:", round(100 * var_level_slope/var_total, 2), "%\n")

# => Interpretation:
#    Seasonality explains ONLY 0.1% of the variance ⇒ extremely weak.
#    Trend (level+slope) explains 95.6% of variance ⇒ dominating.

# 4. Plot Decomposition
par(mfrow = c(3,1), mar = c(3, 4, 2, 2))

# Level
plot(time_full_num, level_mth_trig, type = "l", col = "darkgreen", lwd = 2, 
     ylab = "Level", main = "Smoothed Level")
# Slope
plot(time_full_num, slope_mth_trig, type = "l", col = "purple", lwd = 2, 
     ylab = "Slope", main = "Smoothed Slope")
# Seasonal Component
plot(time_full_num, seasonal_mth_trig, type = "l", col = "orange", lwd = 2, 
     ylab = "Seasonal", main = "Smoothed Seasonal Component")

# => Interpretation:
#    Seasonal component is almost flat, little visible structure.


##############################################
# 5. Forecast Plot
n_forecast <- 144  # forecast horizon: e.g., 24 years ahead

# Forecast from filtered state
forecast_mth_trig <- dlmForecast(f_mth_trig, nAhead = n_forecast)

# Forecast mean: E[Y_t|t-1] = F * a_t
forecast_mean_mth_trig <- drop(forecast_mth_trig$a %*% t(FF(mod_mth_trig)))

# Optional: prepend last observed to align for continuity
forecast_mean_mth_trig <- c(tail(ts_monthly, 1), forecast_mean_mth_trig)

# Time index for forecast
forecast_times <- seq(from = max(time_full_num) + deltat(ts_monthly),
                      by = deltat(ts_monthly),
                      length.out = n_forecast)

# Compute y-axis range across observed, smoothed, forecast
ymin <- min(ts_monthly, smoothed_mth_trig, forecast_mean_mth_trig, na.rm = TRUE)
ymax <- max(ts_monthly, smoothed_mth_trig, forecast_mean_mth_trig, na.rm = TRUE)

plot(time_full_num, ts_monthly, type = "l", col = "black", lwd = 1.5,
     ylab = "Value", xlab = "Time",
     main = "Forecast with Smoothed Model",
     xlim = c(min(time_full_num), max(forecast_times)),
     ylim = c(ymin, ymax))

lines(time_full_num, smoothed_mth_trig, col = "blue", lwd = 2)  # smoothed
lines(forecast_times, forecast_mean_mth_trig[-1], col = "red", lwd = 2, lty = 2)  # forecast

legend("topleft", legend = c("Actual", "Smoothed", "Forecast"),
       col = c("black", "blue", "red"), lty = c(1, 1, 2), lwd = c(1.5, 2, 2))


##############################################
# 6. RMSE Computation
residuals_mth_trig <- ts_monthly - smoothed_mth_trig
rmse <- sqrt(mean(residuals_mth_trig^2, na.rm = TRUE))
cat("Root Mean Squared Error (RMSE) of Smoothed Fit:", round(rmse, 5), "\n")

# => Interpretation:
#    Small RMSE means good fit. 
#    Here RMSE mainly reflects noise, since seasonality is negligible.



# --------------------------------------------------------------------------------------
# IV. LATENT COMPONENT EXTRACTION AND VISUALIZATION – ALL COMPONENTS, COMPREHENSIVE SYNTHESIS
# --------------------------------------------------------------------------------------

library(dlm)
library(ggplot2)
library(patchwork)

# Assumptions:
# - You already have estimated and built models:
#   model_rwpn, model_llt, mod_mth_seas, mod_mth_trig
# - You already have ts_monthly (raw) and ts_deseason (deseasonalized)

# 1. Apply smoothing
smooth_rwpn <- dlmSmooth(ts_deseason, model_rwpn)
smooth_llt <- dlmSmooth(ts_deseason, model_llt)
smooth_seas <- dlmSmooth(ts_monthly, mod_mth_seas)
smooth_trig <- dlmSmooth(ts_monthly, mod_mth_trig)

# 2. Extract latent components (drop initial filter state via dropFirst)
level_rwpn <- dropFirst(smooth_rwpn$s)            # RWPN: LEVEL
level_llt <- dropFirst(smooth_llt$s)[, 1]         # LLT: LEVEL
slope_llt <- dropFirst(smooth_llt$s)[, 2]         # LLT: SLOPE

level_seas <- dropFirst(smooth_seas$s)[, 1]       # Classical: LEVEL
slope_seas <- dropFirst(smooth_seas$s)[, 2]       # Classical: SLOPE
seasonal_seas <- dropFirst(smooth_seas$s)[, 3]    # Classical: SEASONAL

level_trig <- dropFirst(smooth_trig$s)[, 1]       # Fourier: LEVEL
slope_trig <- dropFirst(smooth_trig$s)[, 2]       # Fourier: SLOPE
seasonal_mat_trig <- dropFirst(smooth_trig$s)[, 3:ncol(smooth_trig$s)]  # Fourier: seasonal matrix

# Extract scalar seasonal from Fourier harmonics
extract_seasonal_effect_trig <- function(seasonal_mat, months) {
  n <- nrow(seasonal_mat)
  q <- ncol(seasonal_mat) / 2
  seasonal <- numeric(n)
  omega <- 2 * pi / 12
  for (i in seq_len(n)) {
    t <- months[i] - 1
    for (h in seq_len(q)) {
      seasonal[i] <- seasonal[i] +
        seasonal_mat[i, 2*h -1] * cos(h * omega * t) +
        seasonal_mat[i, 2*h] * sin(h * omega * t)
    }
  }
  return(seasonal)
}

months_mth <- cycle(ts_monthly)
seasonal_trig <- extract_seasonal_effect_trig(seasonal_mat_trig, months_mth)

# 3. Define time vector: skip initial filtering step
time_vec <- time(ts_monthly)  # drop first to match dropFirst()

# ------------------------------------------------------------
# 4. BUILD PLOTS FOR EACH MODEL
# ------------------------------------------------------------

# RWPN panel: LEVEL
p_rwpn <- ggplot(data.frame(Time = time_vec, Level = level_rwpn), aes(x = Time, y = Level)) +
  geom_line(color = "blue", size = 0.01, lwd = 0.2 ) +
  labs(title = "RWPN: Smoothed Level", y = "Level") +
  theme_minimal()

# LLT panel: LEVEL + SLOPE
p_llt1 <- ggplot(data.frame(Time = time_vec, Level = level_llt), aes(x = Time, y = Level)) +
  geom_line(color = "blue", size = 0.5) +
  labs(title = "LLT: Smoothed Level", y = "Level") +
  theme_minimal()

p_llt2 <- ggplot(data.frame(Time = time_vec, Slope = slope_llt), aes(x = Time, y = Slope)) +
  geom_line(color = "purple", size = 0.5) +
  labs(title = "LLT: Smoothed Slope", y = "Slope") +
  theme_minimal()

panel_llt <- p_llt1 / p_llt2

# Classical Seasonal panel: LEVEL + SLOPE + SEASONAL
p_seas1 <- ggplot(data.frame(Time = time_vec, Level = level_seas), aes(x = Time, y = Level)) +
  geom_line(color = "blue", size = 0.5) +
  labs(title = "Classical Seasonal: Smoothed Level", y = "Level") +
  theme_minimal()

p_seas2 <- ggplot(data.frame(Time = time_vec, Slope = slope_seas), aes(x = Time, y = Slope)) +
  geom_line(color = "purple", size = 0.5) +
  labs(title = "Classical Seasonal: Smoothed Slope", y = "Slope") +
  theme_minimal()

p_seas3 <- ggplot(data.frame(Time = time_vec, Seasonal = seasonal_seas), aes(x = Time, y = Seasonal)) +
  geom_line(color = "orange", size = 0.5) +
  labs(title = "Classical Seasonal: Smoothed Seasonal", y = "Seasonal") +
  theme_minimal()

panel_seas <- p_seas1 / p_seas2 / p_seas3

# Fourier Seasonal panel: LEVEL + SLOPE + SEASONAL
p_trig1 <- ggplot(data.frame(Time = time_vec, Level = level_trig), aes(x = Time, y = Level)) +
  geom_line(color = "blue", size = 0.5) +
  labs(title = "Fourier Seasonal: Smoothed Level", y = "Level") +
  theme_minimal()

p_trig2 <- ggplot(data.frame(Time = time_vec, Slope = slope_trig), aes(x = Time, y = Slope)) +
  geom_line(color = "purple", size = 0.5) +
  labs(title = "Fourier Seasonal: Smoothed Slope", y = "Slope") +
  theme_minimal()

p_trig3 <- ggplot(data.frame(Time = time_vec, Seasonal = seasonal_trig), aes(x = Time, y = Seasonal)) +
  geom_line(color = "orange", size = 0.5) +
  labs(title = "Fourier Seasonal: Smoothed Seasonal", y = "Seasonal") +
  theme_minimal()

panel_trig <- p_trig1 / p_trig2 / p_trig3




# ------------------------------------------------------------
# 5. COMBINE PANELS: ONE PER MODEL
# ------------------------------------------------------------
final_plot <- (p_rwpn | panel_llt | panel_seas | panel_trig) +
  plot_layout(ncol = 4)

# Show plot
final_plot



####################################################################################
# ADDRESSING BULLET POINT 3: 
# G. GLOBAL MODEL COMPARISON: RWPN vs LLT vs Classical Seasonal vs Fourier Seasonal

# Extract log-likelihoods
loglik_rwpn <- -mle_rwpn$value
loglik_llt <- -mle_llt$value
loglik_seas <- -mle_mth_seas$value
loglik_trig <- -mle_mth_trig$value

# Number of parameters
k_rwpn <- 2
k_llt <- 3
k_seas <- 4
k_trig <- 4

# Number of observations
n_obs <- length(ts_monthly)

# Compute AIC and BIC
aic_rwpn <- -2 * loglik_rwpn + 2 * k_rwpn
bic_rwpn <- -2 * loglik_rwpn + log(n_obs) * k_rwpn

aic_llt <- -2 * loglik_llt + 2 * k_llt
bic_llt <- -2 * loglik_llt + log(n_obs) * k_llt

aic_seas <- -2 * loglik_seas + 2 * k_seas
bic_seas <- -2 * loglik_seas + log(n_obs) * k_seas

aic_trig <- -2 * loglik_trig + 2 * k_trig
bic_trig <- -2 * loglik_trig + log(n_obs) * k_trig

# RMSEs already computed
rmse_rwpn <- sqrt(mean((ts_deseason_adj - level_rwpn)^2, na.rm = TRUE))
rmse_llt <- sqrt(mean((ts_deseason_adj - level_llt)^2, na.rm = TRUE))
rmse_seas <- sqrt(mean((ts_monthly - smoothed_mth_seas)^2, na.rm = TRUE))
rmse_trig <- sqrt(mean((ts_monthly - smoothed_mth_trig)^2, na.rm = TRUE))

# Create comparison table
comparison_table <- data.frame(
  Model = c("RWPN", "LLT", "Classical Seasonal", "Fourier Seasonal"),
  LogLikelihood = c(loglik_rwpn, loglik_llt, loglik_seas, loglik_trig),
  AIC = c(aic_rwpn, aic_llt, aic_seas, aic_trig),
  BIC = c(bic_rwpn, bic_llt, bic_seas, bic_trig),
  RMSE = c(rmse_rwpn, rmse_llt, rmse_seas, rmse_trig)
)

# Print table
print(comparison_table, digits = 4)

# Interpretation comment
"
Model comparison interpretation:
The Random Walk Plus Noise (RWPN) model achieves the best AIC (-6373) and BIC (-6362), meaning it provides the strongest likelihood-based fit while penalizing for simplicity. The Locally Linear Trend (LLT) model follows with slightly worse AIC/BIC, reflecting additional complexity (slope term) with only marginal improvement in fit. The Fourier Seasonal model underperforms RWPN and LLT on AIC/BIC, suggesting its harmonic seasonal structure does not capture meaningful seasonal variation in the data. The Classical Seasonal model achieves the lowest RMSE (0.00997), meaning it reconstructs the historical data most precisely in-sample, but its extremely low log-likelihood (-541) and positive AIC (+1091) indicate poor probabilistic fit.
These differences likely arise because the temperature anomaly series contains a weak seasonal signal and is predominantly driven by trend (level and slope dynamics). The Classical Seasonal model forces strong seasonal factors into the model regardless of their true contribution, leading to excellent mechanical reconstruction but poor generalization and likelihood fit. Conversely, RWPN and LLT focus on stochastic trend evolution, which better aligns with the data's structure. The low explanatory variance of seasonality (~6%) supports why models emphasizing trend outperform those emphasizing seasonal components in likelihood metrics. In short, the better statistical fit of RWPN arises because the data is trend-dominated with minimal systematic seasonality.
"


#######################################################
# H. ADDITIONAL ANALYSIS RUN WITH THE FITTED DLMSEAS MODEL
#######################################################

# ============================================================
# VARIANCE DECOMPOSITION
# ============================================================

# Compute total variance and variance explained by each component
var_total_seas <- var(ts_monthly, na.rm = TRUE)
var_seasonal_seas <- var(seasonal_vec_mth_seas, na.rm = TRUE)
var_level_slope_seas <- var(level_mth_seas + slope_mth_seas, na.rm = TRUE)

# Print variance decomposition
cat("Total Variance:", var_total_seas, "\n")
cat("Variance Explained by Seasonal Component:", var_seasonal_seas, "\n")
cat("% Variance (Seasonal):", round(100 * var_seasonal_seas / var_total_seas, 2), "%\n")
cat("% Variance (Level + Slope):", round(100 * var_level_slope_seas / var_total_seas, 2), "%\n")
"These results show that the seasonal component explains approximately 6.02% of the total variance in the
observed series, while the combined level and slope components explain about 93.77% of the variance. This
indicates that the underlying variability in the time series is predominantly driven by long-term trend and slope
dynamics, with a relatively small contribution from seasonal fluctuations. The seasonal pattern is present but 
not a dominant source of variation in the data."

"Rolling variance decomposition: this code computes, for each moving window (e.g., 120 months), the percentage of total
variance explained by the seasonal component and by the combined level + slope (trend) component. For each window,
it calculates Var(seasonal_t), Var(level_t + slope_t), and Var(total_t) over the same window and computes their ratios as percentages. This allows tracking how the relative importance of seasonality and trend change over time."

# A. Average Seasonal Effect by Month
months_mth <- cycle(ts_monthly)
avg_seasonal_by_month <- tapply(seasonal_vec_mth_seas, months_mth, mean, na.rm = TRUE)

plot(1:12, avg_seasonal_by_month, type = "o", pch = 19, col = "orange", lwd = 2,
     xaxt = "n", xlab = "Month", ylab = "Average Seasonal Effect",
     main = "Average Seasonal Effect by Month")
axis(1, at = 1:12, labels = month.abb)
abline(h = 0, lty = 2)

# ------------------------------------------
# B. Rolling Standard Deviation of Seasonal Component (10-year window)
library(zoo)

rolling_sd_seasonal <- rollapply(seasonal_vec_mth_seas, width = 120, FUN = sd, fill = NA, align = "right")

plot(time_full_num, rolling_sd_seasonal, type = "l", col = "red", lwd = 2,
     ylab = "Rolling SD", main = "Rolling Standard Deviation of Seasonal Component (10-year window)")
abline(h = mean(rolling_sd_seasonal, na.rm = TRUE), col = "blue", lty = 2)



########################################################
# I. COMPARISON WITH HIDDEN MARKOV MODELS (HMMs)
########################################################


# 1. SIMILARITIES (THEORETICAL FOUNDATIONS):
# Both Dynamic Linear Models (DLMs) and Hidden Markov Models (HMMs) are latent-state
# time series models under the state-space framework. Both aim to estimate unobserved
# states (theta_t or S_t) driving the observed process, and both apply recursive
# estimation algorithms: Kalman filter (DLM) and forward-backward algorithm (HMM).
# Both can be viewed as special cases of a general state-space model:
#    observed:   Y_t ~ f(Y_t | state_t)
#    latent:     state_t ~ g(state_t-1)
#
# 2. DIFFERENCES THAT ARE STRENGTHS OF DLM:
# - DLMs assume continuous latent states evolving under Gaussian linear dynamics.
# - DLMs directly model stochastic trend, slope, and seasonality as continuous quantities,
#   enabling smooth reconstruction of gradual processes (e.g., global temperature anomalies).
# - DLM smoothing filters out short-term fluctuations, providing robust trend estimates.
# - Latent components (level, slope, seasonal) are interpretable and evolve continuously over time.
#
# 3. DIFFERENCES THAT ARE WEAKNESSES OF DLM:
# - DLMs assume linearity and Gaussianity: standard DLMs model both the state transition
#   and observation equations as linear Gaussian systems. This limits their ability to
#   capture nonlinear dynamics, non-Gaussian shocks, or heavy-tailed noise.
#   However, these limitations can be partially addressed by extending the DLM framework:
#   (i) Nonlinear state-space models (e.g., Extended Kalman Filter, Unscented Kalman Filter),
#   (ii) Generalized Linear State-Space Models (GLSSMs) allowing non-Gaussian observation distributions,
#   or (iii) Bayesian hierarchical state-space models estimated via simulation (e.g., particle filters, MCMC).
#   Nonetheless, such extensions increase computational complexity and estimation difficulty
#   compared to the tractable closed-form inference of linear Gaussian DLMs
# - DLMs cannot detect abrupt regime shifts or discrete state transitions; instead, they smooth over them.
# - If structural breaks or latent regimes exist, DLMs may under-represent such discrete changes. Hence, 
# HMMs (with discrete latent states and a transition matrix) are better suited to model sudden regime switches.
#
# 4. INTERPRETATION AND CONCLUSION FOR THIS ANALYSIS:
# For modeling global temperature anomalies, DLMs are more suitable than HMMs because:
# - The time series exhibits a continuous, gradually evolving trend rather than abrupt structural breaks.
# Climate variability is inherently gradual, aligning with DLM’s continuous latent state assumptions.
# Therefore, while HMMs excel at detecting hidden regime switches, DLMs provide a better theoretical
# and empirical match for this trend-dominated, smoothly evolving time series.






##################################################
# APPENDIX
#################################################

# A. DESCRIPTIVE EVIDENCE ON HOW GISTEM DATA IS DERIVED
# Step 0: Prepare time information
years_full <- floor(time_full_num)   # year
months_full <- cycle(ts_monthly)     # month (1 = Jan, 12 = Dec)

# Step 1: Assign decade groups
decade_group <- cut(years_full, 
                    breaks = seq(1880, 2020, by = 10), 
                    labels = paste(seq(1880, 2010, by = 10), "-", seq(1889, 2019, by = 10)),
                    right = TRUE)

# Step 2: Create a data frame
data_temp <- data.frame(
  year = years_full,
  month = months_full,
  decade = decade_group,
  temp_anomaly = as.numeric(ts_monthly)
)

# Step 3: Compute average per month per decade
library(dplyr)

avg_temp_by_month_decade <- data_temp %>%
  group_by(decade, month) %>%
  summarise(avg_temp = mean(temp_anomaly, na.rm = TRUE)) %>%
  ungroup()

# Step 4: Reshape into a matrix (months as rows, decades as columns)
library(tidyr)

avg_temp_matrix <- pivot_wider(avg_temp_by_month_decade, names_from = decade, values_from = avg_temp)

# Step 5: Clean output
avg_temp_matrix <- as.data.frame(avg_temp_matrix)
colnames(avg_temp_matrix)[1] <- "Month"
avg_temp_matrix$Month <- month.abb[avg_temp_matrix$Month]

# View
print(avg_temp_matrix, row.names = FALSE)



# =========================================
# HEATMAP: Seasonal Anomaly by Year/Month, wrt overall
# =========================================
library(ggplot2)
library(viridis)

# Compute threshold for outliers
threshold <- 2 * sd(seasonal_vec_mth_seas, na.rm = TRUE)
outlier_flag <- abs(seasonal_vec_mth_seas) > threshold

# Build data frame
heatmap_df <- data.frame(
  year = years_full,
  month = factor(month.abb[months_full], levels = rev(month.abb)),
  seasonal = seasonal_vec_mth_seas,
  outlier = outlier_flag
)

# PLOT HEATMAP: coloring by seasonal anomaly
ggplot(heatmap_df, aes(x = year, y = month, fill = seasonal)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "C", name = "Seasonal\nAnomaly", direction = -1) +
  labs(title = "Heatmap of Seasonal Anomalies by Year and Month",
       x = "Year", y = "Month") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# OPTIONAL: overlay outliers
ggplot(heatmap_df, aes(x = year, y = month)) +
  geom_tile(aes(fill = seasonal), color = "white") +
  geom_point(data = subset(heatmap_df, outlier), color = "red", shape = 4, size = 1.5) +
  scale_fill_viridis(option = "C", name = "Seasonal\nAnomaly", direction = -1) +
  labs(title = "Heatmap of Seasonal Anomalies with Outliers Marked",
       x = "Year", y = "Month") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))






# === STEP 12:  SARIMA ===

{
  # 1. STRUCTURAL BREAKS (Multiple) ===
  bp <- breakpoints(gistemp_ts ~ 1)
  summary(bp)
  
  break_dates <- time(gistemp_ts)[bp$breakpoints]
  cat("Breaks detected at:\n")
  print(break_dates)
  
  
  # === 2. MULTIPLE DUMMY REGIMES ===
  n <- length(gistemp_ts)
  regimes <- cut(1:n, breaks = c(0, bp$breakpoints, n), labels = FALSE)
  X_regimes <- model.matrix(~ factor(regimes))[, -1]
  
  # === 3. FIT SARIMA MODEL (FULL DATA) ===
  fit_arima <- auto.arima(gistemp_ts,
                          seasonal = TRUE,
                          stepwise = FALSE,
                          approximation = FALSE,
                          xreg = X_regimes)
  summary(fit_arima)
  
  # === 4. FITTED PLOT ===
  ts.plot(gistemp_ts, col = "black", main = "GISTEMP Anomalies and SARIMA Fit with Break Dummies")
  lines(fitted(fit_arima), col = "blue")
  abline(v = break_dates, col = "red", lty = 2)
  legend("topleft", legend = c("Actual", "Fitted", "Break Points"),
         col = c("black", "blue", "red"), lty = c(1, 1, 2))
  
  # === 5. TRAIN/TEST FORECAST ===
  h <- 36
  n_total <- length(gistemp_ts)
  n_train <- n_total - h
  
  train_ts <- window(gistemp_ts, end = time(gistemp_ts)[n_train])
  test_ts  <- window(gistemp_ts, start = time(gistemp_ts)[n_train + 1])
  
  train_xreg <- X_regimes[1:n_train, , drop = FALSE]
  test_xreg  <- X_regimes[(n_train + 1):n_total, , drop = FALSE]
  
  fit_arima <- auto.arima(train_ts,
                          seasonal = TRUE,
                          stepwise = FALSE,
                          approximation = FALSE,
                          xreg = train_xreg)
  
  fc <- forecast(fit_arima, xreg = test_xreg, h = h)
  
  forecast_df <- data.frame(
    Date     = time(test_ts),
    Actual   = as.numeric(test_ts),
    Forecast = as.numeric(fc$mean),
    Lower    = as.numeric(fc$lower[, 2]),
    Upper    = as.numeric(fc$upper[, 2])
  )
  
  ggplot(forecast_df, aes(x = Date)) +
    geom_line(aes(y = Actual), color = "black") +
    geom_line(aes(y = Forecast), color = "#0072B2", linetype = "dashed") +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "#0072B2", alpha = 0.2) +
    labs(title = "GISTEMP Forecast vs Actual",
         y = "Temperature Anomaly (°C)", x = "Date") +
    theme_minimal(base_size = 13)
  
  # === 6. RESIDUAL DIAGNOSTICS ===
  res <- residuals(fit_arima)
  
  p1 <- ggplot(data.frame(Time = time(res), Residuals = res), aes(x = Time, y = Residuals)) +
    geom_line(color = "#08306B") +
    geom_smooth(method = "loess", se = FALSE, color = "#2171B5") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Residuals Over Time") +
    theme_minimal()
  
  p2 <- ggAcf(res, lag.max = 36) + labs(title = "ACF of Residuals") + theme_minimal()
  
  p3 <- ggplot(data.frame(res), aes(x = res)) +
    geom_histogram(aes(y = ..density..), fill = "#0072B2", bins = 25, alpha = 0.7) +
    stat_function(fun = dnorm, args = list(mean = mean(res), sd = sd(res)),
                  color = "red", linetype = "dashed") +
    labs(title = "Histogram of Residuals") +
    theme_minimal()
  
  p4 <- ggplot(data.frame(res), aes(sample = res)) +
    stat_qq(color = "#0072B2") +
    stat_qq_line(color = "black") +
    labs(title = "Q-Q Plot") +
    theme_minimal()
  
  (p1 | p2) / (p3 | p4) + plot_annotation(title = "SARIMA Residual Diagnostics")
  
  # === 7. BENCHMARK MODELS ===
  fit_ets <- ets(train_ts)
  fc_ets <- forecast(fit_ets, h = h)
  
  fc_naive <- naive(train_ts, h = h)
  
  accuracy(fc, test_ts)
  accuracy(fc_ets, test_ts)
  accuracy(fc_naive, test_ts)
  
  # === 8. DM TESTS ===
  dm.test(fc$mean, fc_ets$mean, alternative = "two.sided", h = 1)
  dm.test(fc$mean, fc_naive$mean, alternative = "two.sided", h = 1)
  }



