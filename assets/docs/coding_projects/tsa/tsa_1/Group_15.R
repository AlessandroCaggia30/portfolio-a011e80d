#TASK 1
# Load necessary packages
library(ggplot2)
library(ggthemes)
library(tidyr)
library(dplyr)
library(patchwork)
library(gganimate)
library(gifski)
library(transformr)
# Load dataset and inspect basic structure
data <- gistemp[,2:13]
TData <- t(data)
vectordata <- as.vector(TData)
# Create a time series object starting from 1880, with a monthly frequency
ts_data <- ts(vectordata, start=c(1880,1), frequency=12)
# Apply STL decomposition
stl_decomp <- stl(ts_data, s.window = 3)
# Apply Classical Additive Decomposition
d_additive <- decompose(ts_data, type = "additive")
# Extract components for STL and Additive Decompositions
extract_components <- function(decomp, method_name, is_stl = FALSE) {
  if (is_stl) {
    trend <- decomp[, "trend"]
    seasonal <- decomp[, "seasonal"]
    residual <- decomp[, "remainder"]
  } else {
    trend <- decomp$trend
    seasonal <- decomp$seasonal
    residual <- decomp$random
  }
  list(
    df_trend = data.frame(Year = as.numeric(time(ts_data)), Trend = as.numeric(trend)),
    df_seasonal = data.frame(Year = as.numeric(time(ts_data)), Seasonal = as.numeric(seasonal)),
    df_residual = data.frame(Year = as.numeric(time(ts_data)), Residual = as.numeric(residual)),
    method = method_name
  )
}
# Extract data for STL and Additive Decompositions
stl_data <- extract_components(stl_decomp$time.series, "STL", is_stl = TRUE)
add_data <- extract_components(d_additive, "Additive")

# Define theme and color
dark_blue <- "#08306b"
custom_theme <- theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20, margin = margin(b = 10)),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 14)
  )
create_trend_plot <- function(data, trendline_years = c(1930, 1985, 2008)) {
  df_trend <- data$df_trend
  df_actual <- data.frame(Year = as.numeric(time(ts_data)), Actual = as.numeric(ts_data)) # Actual
  data
  # Initialize empty dataframe for trendlines
  df_trendlines <- data.frame(Year = numeric(), Trend = numeric(), Group = factor())
  # Fit linear models using only the previous 30 years of data
  for (year in trendline_years) {
    data_subset <- df_trend %>% filter(Year > (year - 30) & Year <= year)
    if (nrow(data_subset) > 1) {
      model <- lm(Trend ~ Year, data = data_subset)
      # Align trendline with STL trend at start year
      start_trend_value <- df_trend %>% filter(Year == year) %>% pull(Trend)
      predicted_trend <- predict(model, newdata = data.frame(Year = seq(year, 2024, by = 1)))
      adjusted_trend <- predicted_trend - (predicted_trend[1] - start_trend_value)
      df_trendlines <- rbind(df_trendlines, data.frame(
        Year = seq(year, 2024, by = 1),
        Trend = adjusted_trend,
        Group = as.factor(year)
      ))
    }
  }
  # Create trend plot with trendlines and actual data
  p_trend <- ggplot() +
    geom_line(data = df_actual, aes(x = Year, y = Actual), color = "darkblue", linewidth = 0.3,
              linetype = "dashed") + # Actual data
    geom_line(data = df_trend, aes(x = Year, y = Trend), color = dark_blue, linewidth = 1.1) + #
    STL trend
  geom_line(data = df_trendlines, aes(x = Year, y = Trend, group = Group),
            color = "red", linewidth = 0.8, linetype = "dashed") + # Trendlines
    scale_x_continuous(breaks = seq(1880, 2024, by = 10)) +
    labs(title = paste(data$method, "Trend Component vs Actual Data"), x = "Year", y = "Trend") +
    custom_theme
  return(p_trend)
}
# Function to create seasonal comparison plot (STL vs Additive)
create_seasonal_comparison_plot <- function(stl_data, add_data) {
  p_stl_seasonal <- ggplot(stl_data$df_seasonal, aes(x = Year, y = Seasonal)) +
    geom_line(color = dark_blue, linewidth = 0.2) +
    labs(title = "STL Seasonal Component", x = "Year", y = "Seasonal Effect") +
    custom_theme
  p_add_seasonal <- ggplot(add_data$df_seasonal, aes(x = Year, y = Seasonal)) +
    geom_line(color = dark_blue, linewidth = 0.2) +
    labs(title = "Additive Seasonal Component", x = "Year", y = "Seasonal Effect") +
    custom_theme
  return(p_stl_seasonal + p_add_seasonal) # Side-by-side plot
}
# Function to create residuals comparison plot (STL vs Additive)
create_residuals_plot <- function(stl_data, add_data) {
  p_stl_residual <- ggplot(stl_data$df_residual, aes(x = Year, y = Residual)) +
    geom_line(color = dark_blue, linewidth = 0.5) +
    labs(title = "STL Residual Component", x = "Year", y = "Residual") +
    custom_theme
  p_add_residual <- ggplot(add_data$df_residual, aes(x = Year, y = Residual)) +
    geom_line(color = dark_blue, linewidth = 0.5) +
    labs(title = "Additive Residual Component", x = "Year", y = "Residual") +
    custom_theme
  return(p_stl_residual / p_add_residual) # Stack plots vertically
}
# --- Generate plots ---
stl_trend_plot <- create_trend_plot(stl_data)
seasonal_comparison_plot <- create_seasonal_comparison_plot(stl_data, add_data)
residuals_comparison_plot <- create_residuals_plot(stl_data, add_data)
# --- Save and Display STL Trend Plot ---
ggsave("/Users/Alessandro/Desktop/Plots/stl_trend_with_trendlines.png",
       stl_trend_plot, width = 16, height = 6, dpi = 300)
print(stl_trend_plot)
# --- Save and Display Seasonal Comparison (STL vs Additive) ---
ggsave("/Users/Alessandro/Desktop/Plots/seasonal_comparison.png",
       seasonal_comparison_plot, width = 16, height = 6, dpi = 300)
print(seasonal_comparison_plot)
# --- Save and Display Residuals Comparison (STL vs Additive) ---
ggsave("/Users/Alessandro/Desktop/Plots/residuals_comparison.png",
       residuals_comparison_plot, width = 16, height = 6, dpi = 300)
print(residuals_comparison_plot)
# Compute residual share for both STL and Additive
stl_data$df_residual$Residual_Share <- stl_data$df_residual$Residual / as.numeric(ts_data)
add_data$df_residual$Residual_Share <- add_data$df_residual$Residual / as.numeric(ts_data)
# Function to create residual share plot
create_residuals_share_plot <- function(stl_data, add_data) {
  p_stl_residual <- ggplot(stl_data$df_residual, aes(x = Year, y = Residual_Share)) +
    geom_line(color = dark_blue, linewidth = 0.4) +
    labs(title = "STL Residual as Share of Time Series", x = "Year", y = "Residual Share") +
    custom_theme
  p_add_residual <- ggplot(add_data$df_residual, aes(x = Year, y = Residual_Share)) +
    geom_line(color = dark_blue, linewidth = 0.4) +
    labs(title = "Additive Residual as Share of Time Series", x = "Year", y = "Residual Share") +
    custom_theme
  return(p_stl_residual | p_add_residual) # Place plots side by side
}
# Generate and save residual share plot
residuals_share_plot <- create_residuals_share_plot(stl_data, add_data)
ggsave("/Users/Alessandro/Desktop/Plots/residuals_share_comparison.png",
       residuals_share_plot, width = 16, height = 7, dpi = 300)
print(residuals_share_plot)
####################################################################
# Convert time series data into a dataframe
df_ts <- data.frame(
  Year = floor(time(ts_data)), # Extract Year
  Month = cycle(ts_data), # Extract Month (1-12)
  Value = as.numeric(ts_data) # Extract Actual Values
)
# Select 1 year out of every 10
df_reduced <- df_ts %>%
  filter(Year %% 10 == 0) # Keep only years that are multiples of 10
# Plot time series values for selected years
p_selected_years <- ggplot(df_reduced, aes(x = Month, y = Value, color = factor(Year), group = Year
)) +
  geom_line(size = 1, alpha = 0.7) +
  labs(title = "Raw Time Series Data (1 Year Every 10 Years)",
       x = "Month",
       y = "Value",
       color = "Year") +
  theme_classic()
# Save and display plot
ggsave("/Users/Alessandro/Desktop/Plots/raw_selected_years.png",
       p_selected_years, width = 10, height = 6, dpi = 300)
print(p_selected_years)
# Extract seasonality from STL decomposition
df_seasonal <- stl_data$df_seasonal
# Extract Year and Month correctly
df_seasonal$Year <- floor(df_seasonal$Year) # Convert decimal years to full years
df_seasonal$Month <- cycle(ts_data) # Extract month index (1 to 12)
# Select 1 year out of every 10
df_reduced <- df_seasonal %>%
  filter(Year %% 10 == 0)
# --- Final Static Polar Plot (Using Your Palette) ---
p_final_polar <- ggplot(df_reduced, aes(x = Month, y = Seasonal, color = factor(Year), group = Year
)) +
  geom_line(size = 1, alpha = 0.7) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) + # Show month names
  coord_polar(start = 0) + # Convert to circular layout
  labs(
    title = "STL Seasonal Component (1 Year Every 10 in Polar Plot)",
    x = "Month",
    y = "Seasonal Effect",
    color = "Year"
  ) +
  theme_classic() # Use the same theme
# Save and display the plot
ggsave("/Users/Alessandro/Desktop/Plots/final_stl_seasonal_polar_10yr.png",
       p_final_polar, width = 8, height = 8, dpi = 300)
print(p_final_polar)
# Ensure Year is numeric and remove NA values
df_seasonal <- df_seasonal %>%
  mutate(Year = as.numeric(Year)) %>%
  na.omit()
# Create animation
p_animation <- ggplot(df_seasonal, aes(x = Month, y = Seasonal, group = Year, color = factor(Year
))) +
  geom_line(alpha = 0.6) +
  labs(title = "Seasonal Pattern Over Time (Year: {frame_time})",
       x = "Month",
       y = "Seasonal Effect",
       color = "Year") +
  theme_classic() + # White background
  transition_time(Year) +
  ease_aes("linear")
# Save animation as a GIF
anim_save("/Users/Alessandro/Desktop/Plots/seasonal_animation.gif",
          p_animation, duration = 20, width = 10, height = 6, units = "in", res = 150)
p_seasonal_polar_anim <- ggplot(df_seasonal, aes(x = Month, y = Seasonal, group = Year, color =
                                                   Year)) +
  geom_line(size = 1.5, alpha = 0.8) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  coord_polar(start = 0) +
  scale_color_viridis_c(option = "C", guide = "none") + # Continuous color scale, NO legend
  labs(
    title = "Seasonal Pattern Over Time (Year: {frame_time})",
    x = "Month",
    y = "Seasonal Effect"
  ) +
  theme_minimal(base_size = 16) +
  transition_time(Year) +
  ease_aes("cubic-in-out")
# Save animation
anim_save("/Users/Alessandro/Desktop/Plots/seasonal_polar_animation.gif",
          p_seasonal_polar_anim, duration = 12, width = 8, height = 8, units = "in", res = 200)
##############################################################################
#TASK 2
# EXPONENTIAL SMOOTHING MODEL
# Simple exponential smoothing with optimized alpha
v <- HoltWinters(ts_data, seasonal = "additive")
plot(v)
v$alpha # Displays the optimized alpha value
# Create a new time series from the original dataset, limited to 1880-1930
ts_gistemp_1880_1930 <- window(ts_data, start = c(1880, 1), end = c(1930, 12))
# View the structure to confirm
str(ts_gistemp_1880_1930)
# Plot the new time series
plot(ts_gistemp_1880_1930, main="Temperature Anomalies (1880-1930)", ylab="Temperature Anomaly")
# Details on HW_ts_gistemp_1880_1930
HW_ts_gistemp_1880_1930
#why beta=FALSE
# Fit Holt-Winters with and without trend
HW_no_trend <- HoltWinters(ts_gistemp_1880_1930, beta = FALSE, gamma = FALSE)
HW_with_trend <- HoltWinters(ts_gistemp_1880_1930, beta = TRUE, gamma = FALSE)
# Plot both for comparison
par(mfrow = c(2, 1))
plot(HW_no_trend, main="Holt-Winters without Trend (beta = FALSE)")
plot(HW_with_trend, main="Holt-Winters with Trend (beta = TRUE)")
# Fit Holt-Winters with different alpha values
HW_high_alpha <- HoltWinters(ts_gistemp_1880_1930, alpha = 0.9, beta = FALSE, gamma = FALSE)
# Plot all three for comparison
par(mfrow = c(3, 1)) # Arrange plots in a single column
plot(HW_low_alpha, main="Holt-Winters Filtering (Alpha = 0.2)")
plot(HW_ts_gistemp_1880_1930, main="Holt-Winters Filtering (Default Alpha = 0.52898)")
plot(HW_high_alpha, main="Holt-Winters Filtering (Alpha = 0.9)")
# Extract fitted values
fitted_values <- HW_ts_gistemp_1880_1930$fitted[,1]
# Plot observed vs fitted values
plot(ts_gistemp_1880_1930, main="One-Step-Ahead Predictions vs Observed", col="black", ylab="
Temperature Anomaly")
lines(fitted_values, col="red", lwd=2) # Add fitted values in red
legend("topleft", legend = c("Observed", "Fitted"), col = c("black", "red"), lty = 1, cex = 0.8)\\
# ONE STEP AHEAD PREDICTIONS
# Obtain one-step-ahead fitted values
yhat1 <- fitted(v)
plot(yhat1) # Compare fitted values with observed series
# Plot the observed time series in blue
plot(ts_data, col = "blue", lwd = 2, main = "Observed and Fitted Series")
# Overlay the fitted values in red
lines(time(ts_data)[-1], yhat1[,1], col = "red", lwd = 2)
# Add a legend
legend("topleft", legend = c("Observed", "Fitted"), col = c("blue", "red"), lwd = 2)
v2 <- ts_data[-1] #for the period where the fitted values are available
# Compute Mean Absolute Percentage Error (MAPE)
# Identify indices where observed values are non-zero
nonzero <- v2 != 0
# Compute MAPE for non-zero observed values
mape_value <- mean(abs(v2[nonzero] - yhat1[nonzero]) / abs(v2[nonzero]))
print(mape_value)
install.packages("forecast")
library(forecast)
# Forecast from 1931 to 2024 (95 years = 1140 months)
forecast_HW <- forecast(HW_ts_gistemp_1880_1930, h = 1140)
# Plot forecast vs actual data
plot(forecast_HW, main="Holt-Winters Forecast (1931-2024)")
lines(ts_gistemp_monthly, col="black") # Add actual data in black
# Compute forecast errors at different time points
actual_values <- window(ts_gistemp_monthly, start = c(1931, 1), end = c(2024, 12))
errors <- actual_values - forecast_HW$mean
# Compute error statistics at different time steps
error_1day <- errors[1] # 1 day ahead (1 month)
error_1year <- mean(errors[1:12]) # 1 year ahead
error_5years <- mean(errors[1:(12*5)])
error_10years <- mean(errors[1:(12*10)])
error_50years <- mean(errors[1:(12*50)])
error_90years <- mean(errors[1:(12*90)])
# Print errors
errors_summary <- data.frame(
  Time_Horizon = c("1 Day", "1 Year", "5 Years", "10 Years", "50 Years", "90 Years"),
  Forecast_Error = c(error_1day, error_1year, error_5years, error_10years, error_50years, error_90
                     years)
)
print(errors_summary)