# List of required packages
packages <- c("Rlibeemd", "Metrics","depmixS4","sf","glue","tmap","dplyr","INLA","gifski","gganimate","mFilter", "strucchange", "WaveletComp", "tidyverse", "lubridate", "forecast", "zoo", "tsibble", "feasts", "fabletools", "multitaper",
              "ggtext","ggtext" ,"viridis","spdep","dlm","gganimate", "scales", "sp","Kendall", "patchwork","plotly", "ggridges","slider", "fable","reshape2", "gridExtra", "tidyr",  "ggplot2", "wavelets", "genlasso", "cowplot","ismev",  "evd")





# Install only missing packages
installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)
if (length(to_install) > 0) install.packages(to_install)

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))




###########################################################
# TASK 1 
##########################################################



#--------------------------------------------------------
# GHCN ANALYSIS
#--------------------------------------------------------

# === STEP 1: Import and polishing
{# === Load Daily GHCN Data ===
  ghcn_path <- "/Users/Alessandro/Desktop/final project tsa/ghcn.txt"
  
  ghcn <- read_csv(ghcn_path, col_types = cols(
    ID = col_character(),
    station = col_character(),
    latitude = col_double(),
    longitude = col_double(),
    elevation = col_double(),
    date = col_date(format = "%Y-%m-%d"),
    TMIN = col_double(),
    TMAX = col_double(),
    TAVG = col_double(),
    PRCP = col_double()
  ))
  
  # === Filter San Francisco Downtown Station ===
  sf <- ghcn %>%
    filter(str_detect(station, regex("SAN FRANCISCO DWTN", ignore_case = TRUE))) %>%
    mutate(
      TMIN = TMIN / 10,
      TMAX = TMAX / 10
    ) %>%
    dplyr::select(date, TMIN, TMAX) %>%
    arrange(date)
  
  
  # === Plot Raw Daily Series ===
  sf_long <- pivot_longer(sf, cols = c(TMIN, TMAX), names_to = "Series", values_to = "Temp")
  
  ggplot(sf_long, aes(x = date, y = Temp, color = Series)) +
    geom_line(alpha = 0.5) +
    labs(title = "Daily Min and Max Temperatures – San Francisco Downtown",
         x = "Date", y = "Temperature (°C)", color = NULL) +
    theme_minimal()
  
  # === Monthly Aggregation for Decomposition ===
  sf_monthly <- sf %>%
    mutate(
      DTR = TMAX - TMIN,
      month = floor_date(date, "month")
    ) %>%
    group_by(month) %>%
    summarise(
      TMIN = mean(TMIN, na.rm = TRUE),
      TMAX = mean(TMAX, na.rm = TRUE),
      DTR  = mean(DTR,  na.rm = TRUE),
      .groups = "drop"
    )
  
  
  #Computes the average TMIN and average TMAX, excluding missing values (na.rm = TRUE).
  
  # === Convert to Time Series ===
  start_year <- year(min(sf_monthly$month))
  start_month <- month(min(sf_monthly$month))
  
  tmin_ts <- ts(sf_monthly$TMIN, start = c(start_year, start_month), frequency = 12)
  tmax_ts <- ts(sf_monthly$TMAX, start = c(start_year, start_month), frequency = 12)
}


# Define pastel colors
pastel_colors <- c("TMIN" = "#ADD8E6",   # pastel red
                   "TMAX" = "#FFB3B3")   # pastel blue

# Plot with pastel colors
ggplot(sf_long, aes(x = date, y = Temp, color = Series)) +
  geom_line(alpha = 0.7) +
  scale_color_manual(values = pastel_colors) +
  labs(
    title = "Daily Min and Max Temperatures – San Francisco Downtown",
    x = "Date", y = "Temperature (°C)", color = NULL
  ) +
  theme_minimal()





# === STEP 2: Decomposition ===
# --- I. Pick optimal window separately for TMIN and TMAX (including "periodic")
{windows <- 1:200
  windows_ext <- c(as.character(windows), "periodic")
  
  # Helper function to get AIC from STL
  get_aic <- function(ts, w) {
    stl_mod <- stl(ts, s.window = ifelse(w == "periodic", "periodic", as.numeric(w)))
    residuals <- stl_mod$time.series[, "remainder"]
    rss <- sum(residuals^2); n <- length(residuals); k <- 2
    return(n * log(rss / n) + 2 * k)
  }
  
  # Compute AIC for TMIN
  aic_tmin <- sapply(windows_ext, function(w) get_aic(tmin_ts, w))
  best_tmin_window <- windows_ext[which.min(aic_tmin)]
  
  # Compute AIC for TMAX
  aic_tmax <- sapply(windows_ext, function(w) get_aic(tmax_ts, w))
  best_tmax_window <- windows_ext[which.min(aic_tmax)]
  
  cat("Best TMIN window:", best_tmin_window, "\n")
  cat("Best TMAX window:", best_tmax_window, "\n")
  
  tmin_stl <- stl(tmin_ts, s.window = ifelse(best_tmin_window == "periodic", "periodic", as.numeric(best_tmin_window)))
  tmax_stl <- stl(tmax_ts, s.window = ifelse(best_tmax_window == "periodic", "periodic", as.numeric(best_tmax_window)))
  
  # Tidy conversion function
  tidy_stl <- function(stl_obj, label) {
    df <- as_tibble(stl_obj$time.series)
    df$month <- as.Date(as.yearmon(time(stl_obj$time.series)))
    df$variable <- label
    return(df)
  }
  
  # Tidy data
  df_tmin <- tidy_stl(tmin_stl, "TMIN")
  df_tmax <- tidy_stl(tmax_stl, "TMAX")
  df_all <- bind_rows(df_tmin, df_tmax)
  
  # Pivot to long format and enforce correct component order
  df_long <- pivot_longer(
    df_all,
    cols = c(seasonal, trend, remainder),
    names_to = "Component",
    values_to = "Value"
  )
  df_long$Component <- factor(df_long$Component, levels = c("trend", "seasonal", "remainder"))
}


# Compute STL decompositions and store AIC/BIC
results <- lapply(windows_ext, function(w) {
  stl_mod <- tryCatch({
    stl(tmin_ts, s.window = ifelse(w == "periodic", "periodic", as.numeric(w)))
  }, error = function(e) return(NULL))
  
  if (!is.null(stl_mod)) {
    resids <- stl_mod$time.series[, "remainder"]
    rss <- sum(resids^2)
    n <- length(resids)
    k <- 2
    aic <- n * log(rss / n) + 2 * k
    bic <- n * log(rss / n) + log(n) * k
    list(window = w, model = stl_mod, AIC = aic, BIC = bic)
  } else {
    list(window = w, model = NULL, AIC = Inf, BIC = Inf)
  }
})

# Convert to data.frame and rank
model_df <- tibble(
  window = sapply(results, `[[`, "window"),
  AIC = sapply(results, `[[`, "AIC"),
  BIC = sapply(results, `[[`, "BIC")
)

top10_aic <- model_df %>% arrange(AIC) %>% slice(1:10)
top10_bic <- model_df %>% arrange(BIC) %>% slice(1:10)

# Extract seasonal components for top 10 models
top10_seasonal <- lapply(top10_aic$window, function(w) {
  mod <- results[[which(windows_ext == w)]]$model
  data.frame(
    date = as.Date(as.yearmon(time(mod$time.series))),
    seasonal = mod$time.series[, "seasonal"],
    window = as.character(w)
  )
}) %>% bind_rows()


ggplot(top10_seasonal, aes(x = date, y = seasonal, color = window)) +
  geom_line() +
  facet_wrap(~ window, scales = "free_y", ncol = 2) +
  labs(title = "Seasonal Component – Top 10 STL Models by AIC", x = "Date", y = "Seasonal") +
  theme_minimal() +
  theme(legend.position = "none")

top10_table <- model_df %>%
  arrange(AIC) %>%
  slice(1:10) %>%
  mutate(rank = row_number()) %>%
  dplyr::select(rank, window, AIC, BIC)

knitr::kable(
  top10_table,
  format = "latex",
  digits = 2,
  caption = "Top 10 STL Decompositions by AIC",
  booktabs = TRUE
)



# --- III. Final Plot


# Convert df_long to wide format by component
df_wide <- df_long %>%
  pivot_wider(names_from = Component, values_from = Value)

# Compute relative remainder: remainder / (trend + seasonal)
df_wide <- df_wide %>%
  mutate(
    raw = trend + seasonal,
    relative_remainder = remainder / raw
  )

# Pivot back to long for plotting
df_long_adj <- df_wide %>%
  dplyr::select(month, variable, trend, seasonal, relative_remainder) %>%
  pivot_longer(cols = c("trend", "seasonal", "relative_remainder"),
               names_to = "Component", values_to = "Value") %>%
  mutate(Component = recode(Component, relative_remainder = "remainder"))


{last_year_start <- as.Date("2019-01-01")
  fancy_stl_plot <- ggplot(df_long_adj, aes(x = month, y = Value, color = variable)) +
    annotate(
      "rect",
      xmin = last_year_start, xmax = max(df_long_adj$month),
      ymin = -Inf, ymax = Inf,
      fill = "grey92", alpha = 0.5
    ) +
    geom_line(linewidth = 0.8) +
    facet_grid(
      Component ~ variable,
      scales = "free_y",
      labeller = labeller(
        Component = c(
          "trend" = "Trend Component",
          "seasonal" = "Seasonal Component",
          "remainder" = "Relative Remainder (ratio)"
        ),
        variable = c(
          "TMIN" = "Minimum Temperature (TMIN)",
          "TMAX" = "Maximum Temperature (TMAX)"
        )
      )
    ) +
    scale_color_manual(values = c("TMIN" = "#1f78b4", "TMAX" = "#e31a1c")) +
    scale_x_date(breaks = scales::pretty_breaks(n = 10), date_labels = "%Y") +
    labs(
      title = "<b>STL Decomposition of Monthly Temperatures</b>",
      subtitle = "Minimum and maximum monthly temperatures for San Francisco Downtown<br><i>Shaded area: recent 5-year window</i>",
      x = "Year", y = "°C or Ratio"
    ) +
    theme_minimal(base_family = "serif", base_size = 14) +
    theme(
      plot.title = ggtext::element_markdown(size = 18, face = "bold", margin = margin(b = 6)),
      plot.subtitle = ggtext::element_markdown(size = 13, margin = margin(b = 12)),
      strip.text = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none",
      plot.margin = margin(12, 14, 10, 14)
    )
  print(fancy_stl_plot)
  }




# === STEP 3: Temperature Range Analysis (DTR) (remark) ===
{# Compute DTR
  sf_monthly <- sf %>%
    mutate(
      DTR = TMAX - TMIN,
      month = floor_date(date, "month")
    ) %>%
    group_by(month) %>%
    summarise(
      TMIN = mean(TMIN, na.rm = TRUE),
      TMAX = mean(TMAX, na.rm = TRUE),
      DTR  = mean(DTR,  na.rm = TRUE),
      .groups = "drop"
    )
  
  dtr_ts <- ts(sf_monthly$DTR, start = c(start_year, start_month), frequency = 12)
  
  # STL decomposition
  dtr_stl <- stl(dtr_ts, s.window = "periodic")
  dtr_df <- as.data.frame(dtr_stl$time.series)
  dtr_df$Raw <- as.numeric(dtr_ts)
  dtr_df$Time <- as.Date(as.yearmon(time(dtr_ts)))
  
  # Long format
  dtr_long <- pivot_longer(dtr_df, cols = c("trend", "seasonal", "remainder"),
                           names_to = "Component", values_to = "Value")
  
  # Elegant custom theme
  theme_elegant <- theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "grey85"),
          strip.text = element_text(face = "bold", size = 13),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  # Raw DTR (dark blue)
  p_raw <- ggplot(dtr_df, aes(x = Time, y = Raw)) +
    geom_line(color = "#084594", linewidth = 0.8) +
    labs(title = "Raw DTR") +
    theme_elegant
  
  # Decomposition panels (shades of blue)
  p_components <- ggplot(dtr_long, aes(x = Time, y = Value)) +
    geom_line(linewidth = 0.8, aes(color = Component)) +
    scale_color_manual(values = c(
      "trend"     = "#2171b5",  # medium blue
      "seasonal"  = "#6baed6",  # light blue
      "remainder" = "#bdd7e7"   # very light blue
    )) +
    facet_wrap(~Component, ncol = 1, scales = "free_y") +
    labs(title = "STL Decomposition") +
    theme_elegant +
    theme(legend.position = "none")
  
  # Combine panels
  p_raw / p_components + plot_layout(heights = c(1, 3)) +
    plot_annotation(title = "Diurnal Temperature Range (DTR) and STL Decomposition",
                    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))
}

# === STEP 4: Eva Diurnal ===
{sf_annual <- sf %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(max_tmax = max(TMAX, na.rm = TRUE), .groups = "drop")
  
  gev_fit <- fgev(sf_annual$max_tmax)
  summary(gev_fit)
  
  # Plot return level
  plot(gev_fit, which = 4)  # Return level plot
  
  
  sf_annual_min <- sf %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(min_tmin = min(TMIN, na.rm = TRUE), .groups = "drop")
  
  gev_fit_min <- fgev(sf_annual_min$min_tmin)
  summary(gev_fit_min)
  plot(gev_fit_min, which = 4)  # Return level plot for cold extremes
  
  
  # Eva Diurnal Temperature Range (DTR = TMAX − TMIN)
  sf <- sf %>% mutate(DTR = TMAX - TMIN)
  
  sf_annual_dtr <- sf %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(max_dtr = max(DTR, na.rm = TRUE), .groups = "drop")
  
  gev_fit_dtr <- fgev(sf_annual_dtr$max_dtr)
  summary(gev_fit_dtr)
  plot(gev_fit_dtr, which = 4)  # Return level plot for daily excursion extremes
}



# --- STEP 7: Rolling ACFs (lags 1 to 48) with correct dates

# Generate proper date sequence for ts object
date_seq <- seq(
  from = as.Date(paste(start(tmin_ts)[1], start(tmin_ts)[2], "01", sep = "-")),
  by = "month",
  length.out = length(tmin_ts)
)

# --- Function to compute rolling ACFs
roll_acf_multi <- function(ts_vals, date_seq, window = 120, max_lag = 48) {
  result <- lapply(1:max_lag, function(lag) {
    acf_vals <- rollapply(ts_vals, width = window, by = 12, FUN = function(x) {
      acf(x, lag.max = lag, plot = FALSE)$acf[lag + 1]
    }, align = "right", fill = NA)
    
    # Align valid dates with ACF values
    valid_dates <- date_seq[(length(date_seq) - length(acf_vals) + 1):length(date_seq)]
    
    tibble(Date = valid_dates, Lag = lag, ACF = acf_vals)
  })
  
  bind_rows(result) %>% filter(!is.na(ACF))
}

# --- Compute rolling ACFs for TMIN and TMAX
acf_tmin_all <- roll_acf_multi(as.numeric(tmin_ts), date_seq)
acf_tmax_all <- roll_acf_multi(as.numeric(tmax_ts), date_seq)

# --- Define color scale: red for lag 12, blue fading otherwise
get_lag_color <- function(lags) {
  sapply(lags, function(lag) {
    if (lag == 12) return("#e41a1c")  # red for lag 12
    dist <- abs(lag - 12)
    alpha <- exp(-dist / 4)           # fade with distance from 12
    rgb(70/255, 130/255, 180/255, alpha = alpha)  # steel blue with alpha
  })
}

# --- Format and apply color mapping
acf_tmin_all <- acf_tmin_all %>% mutate(Lag = factor(Lag))
acf_tmax_all <- acf_tmax_all %>% mutate(Lag = factor(Lag))

lag_colors <- get_lag_color(1:48)
names(lag_colors) <- as.character(1:48)

# --- Plot function
plot_acf_all <- function(df, varname) {
  ggplot(df, aes(x = Date, y = ACF, color = Lag, group = Lag)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = lag_colors, guide = "none") +
    labs(
      title = paste("Rolling ACFs (Lags 1–48) –", varname),
      subtitle = "Lag 12 in red; others fade with distance",
      x = "Year", y = "Autocorrelation"
    ) +
    theme_minimal(base_family = "serif", base_size = 14) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 11)
    )
}

# --- Final combined plot
plot_acf_all(acf_tmin_all, "TMIN") /
  plot_acf_all(acf_tmax_all, "TMAX")
















#  === STEP 9: Wavelet ===
# Corrected Wavelet Analysis
{
  wt1 <- analyze.wavelet(data.frame(tmin = as.numeric(tmin_ts)), my.series = "tmin")
  wt.image(wt1, main = "Wavelet Power Spectrum – TMIN")
  
  wt2 <- analyze.wavelet(data.frame(tmax = as.numeric(tmax_ts)), my.series = "tmax")
  wt.image(wt2, main = "Wavelet Power Spectrum – TMAX")
  }







# === STEP 10 Expanded Seasonality (TMIN & TMAX)

library(dplyr)
library(ggplot2)
library(gganimate)
library(patchwork)
library(viridis)

# --- Prepare data ---
prepare_df <- function(ts_data, label) {
  data.frame(
    Year = floor(time(ts_data)),
    Month = cycle(ts_data),
    Value = as.numeric(ts_data),
    Variable = label
  )
}

df_tmin <- prepare_df(tmin_ts, "TMIN")
df_tmax <- prepare_df(tmax_ts, "TMAX")
df_all  <- bind_rows(df_tmin, df_tmax) %>% drop_na()

# --- Static plots ---

# One year every 10
df_10yr <- df_all %>% filter(Year %% 10 == 0)

p1 <- ggplot(df_10yr, aes(x = Month, y = Value, color = factor(Year), group = Year)) +
  geom_line(size = 0.8, alpha = 0.8) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Monthly Values – One Year Every 10", x = "Month", y = "°C", color = "Year") +
  theme_classic()

# Decadal average
df_avg <- df_all %>%
  mutate(Decade = 10 * floor(Year / 10)) %>%
  group_by(Variable, Decade, Month) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(df_avg, aes(x = Month, y = Value, color = factor(Decade), group = Decade)) +
  geom_line(size = 0.9, alpha = 0.9) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Decadal Averages", x = "Month", y = "°C", color = "Decade") +
  theme_classic()

# Combine p1 and p2
combined_plot <- p1 / p2 + 
  plot_annotation(title = "Seasonality Patterns in TMIN and TMAX – San Francisco",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))
print(combined_plot)

# --- Animated Line Plot ---
p3 <- ggplot(df_all, aes(x = Month, y = Value, group = Variable, color = Variable)) +
  geom_line(size = 1) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Monthly Patterns (Year: {frame_time})", x = "Month", y = "°C", color = "Variable") +
  theme_classic() +
  transition_time(Year) +
  ease_aes("linear")

anim3 <- gganimate::animate(
  p3, duration = 12, width = 10, height = 5, units = "in", res = 200,
  renderer = gifski_renderer()
)
gganimate::anim_save("tmin_tmax_line_animation.gif", animation = anim3)

# --- Animated Polar Plot ---
p4 <- ggplot(df_all, aes(x = Month, y = Value, group = Variable, color = Variable)) +
  geom_line(size = 1.1, alpha = 0.85) +
  coord_polar(start = 0) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_viridis_d() +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Animated Polar Plot (Year: {frame_time})", x = NULL, y = NULL, color = "Variable") +
  theme_minimal(base_size = 16) +
  transition_time(Year) +
  ease_aes("cubic-in-out")

anim4 <- gganimate::animate(
  p4, duration = 12, width = 8, height = 8, units = "in", res = 200,
  renderer = gifski_renderer()
)
gganimate::anim_save("tmin_tmax_polar_animation.gif", animation = anim4)



# Reuse sf_long with date as x-axis
sf_long <- sf %>%
  filter(date >= as.Date("2015-01-01")) %>%
  pivot_longer(cols = c(TMIN, TMAX), names_to = "Series", values_to = "Temp")

# Define pastel colors
pastel_colors <- c("TMIN" = "#ADD8E6", "TMAX" = "#FFB3B3")


ggplot(sf_long, aes(x = date, y = Temp, color = Series)) +
  geom_line(alpha = 0.4) +
  geom_smooth(
    method = "loess",
    span = 0.01,  # very localized smoothing for daily data
    se = FALSE,
    size = 1.2
  ) +
  scale_color_manual(values = pastel_colors) +
  labs(
    title = "Daily Min and Max Temperatures – San Francisco Downtown (2000+)",
    x = "Date", y = "Temperature (°C)", color = NULL
  ) +
  theme_minimal()





#----------------------------
# GISTEMP ANALYSIS
#----------------------------


# === STEP 1: Load GISTEMP Data ===
gistemp_path <- "/Users/Alessandro/Desktop/final project tsa/gistemp.txt"
gistemp <- read_csv(gistemp_path)

# === STEP 2: Transform to Long Format & Convert to ts Object ===
{
  gistemp_monthly <- gistemp %>%
    dplyr::select(Year, Jan:Dec) %>%
    pivot_longer(-Year, names_to = "Month", values_to = "Anomaly") %>%
    mutate(
      Month = match(Month, month.abb),
      date = ymd(paste(Year, Month, "01", sep = "-"))
    ) %>%
    arrange(date) %>%
    drop_na()
  
  
  start_year <- year(min(gistemp_monthly$date))
  start_month <- month(min(gistemp_monthly$date))
  gistemp_ts <- ts(gistemp_monthly$Anomaly, start = c(start_year, start_month), frequency = 12)
}

# === I. best STL Decomposition for windows 1 to 200 ===
{# i. Define windows (1:200 + "periodic") ===
  windows <- c(as.character(1:200), "periodic")
  
  # ii. Compute RMSE and AIC for each window ===
  rmse_results <- numeric(length(windows))
  aic_results  <- numeric(length(windows))
  
  for (i in seq_along(windows)) {
    w <- windows[i]
    stl_mod <- stl(gistemp_ts, s.window = ifelse(w == "periodic", "periodic", as.numeric(w)))
    
    residuals <- stl_mod$time.series[, "remainder"]
    rmse_results[i] <- sqrt(mean(residuals^2))
    
    rss <- sum(residuals^2)
    n <- length(residuals)
    k <- 2  # trend + seasonal
    aic_results[i] <- n * log(rss / n) + 2 * k
  }
  
  # iii. Create result table and sort ===
  results <- data.frame(
    Window = windows,
    AIC = aic_results,
    RMSE = rmse_results,
    stringsAsFactors = FALSE
  )
  
  top_10_aic  <- results[order(results$AIC)[1:10], ]
  top_10_rmse <- results[order(results$RMSE)[1:10], ]
  
  cat("\nTop 10 based on AIC:\n")
  print(top_10_aic)
  
  cat("\nTop 10 based on RMSE:\n")
  print(top_10_rmse)
}

# === II. apply best STL
{# === STEP 4: Use best model (based on AIC) to extract components ===
  best_window <- top_10_aic$Window[1]
  best_stl <- stl(gistemp_ts, s.window = ifelse(best_window == "periodic", "periodic", as.numeric(best_window)))
  
  # Convert to tidy dataframe
  df_stl <- as.data.frame(best_stl$time.series)
  df_stl$Date <- as.Date(as.yearmon(time(gistemp_ts)))
  
  # Reshape for ggplot
  df_long <- df_stl %>%
    pivot_longer(cols = c("trend", "seasonal", "remainder"), names_to = "Component", values_to = "Value")
  
  # Factor order for facet layout
  df_long$Component <- factor(df_long$Component, levels = c("trend", "seasonal", "remainder"))
  
  # === Plot all components in one canvas ===
  ggplot(df_long, aes(x = Date, y = Value)) +
    geom_line(color = "darkblue", linewidth = 0.5) +
    facet_wrap(~Component, ncol = 1, scales = "free_y",
               labeller = labeller(Component = c(
                 trend = "Trend Component",
                 seasonal = "Seasonal Component",
                 remainder = "Remainder Component"
               ))) +
    labs(
      title = paste0("Best STL Decomposition GISTEMP (s.window = ", best_window, ")"),
      x = "Year", y = "Value"
    ) +
    theme_minimal(base_family = "serif", base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 13, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11),
      plot.margin = margin(10, 14, 10, 14)
    )
}


#  === STEP 3: Density Plot of Anomalies by Decade ===
{gistemp_monthly %>%
    mutate(decade = floor(year(date) / 10) * 10) %>%
    ggplot(aes(x = Anomaly, fill = as.factor(decade))) +
    geom_density(alpha = 0.5) +
    labs(title = "Density of Temperature Anomalies by Decade", x = "Anomaly (°C)", fill = "Decade") +
    theme_minimal()
}
# 1 more spread, 2 to the right: increasing


# === STEP 4: DLM analysis
#extra file just for this

# === STEP 5: Piecewise Linear Trends (Breakpoints)
{# === Estimate breakpoints
  bp <- breakpoints(gistemp_ts ~ 1)
  bp_summary <- summary(bp)
  
  # === Get break dates
  break_indices <- bp$breakpoints
  break_dates <- as.Date(as.yearmon(time(gistemp_ts)))[break_indices]
  
  # === Create full dataset with fitted values per segment
  ts_df <- tibble(
    Date = as.Date(as.yearmon(time(gistemp_ts))),
    Temp = as.numeric(gistemp_ts)
  )
  
  # Fitted values for the best breakpoint model
  fit_vals <- fitted(bp)
  ts_df <- ts_df %>% mutate(Fitted = fit_vals)
  
  # === Segment identifiers
  ts_df$Segment <- cut(seq_along(ts_df$Temp), breaks = c(0, break_indices, nrow(ts_df)), labels = FALSE)
  
  # === Fit linear models within each segment
  segment_fits <- ts_df %>%
    group_by(Segment) %>%
    do({
      model <- lm(Temp ~ Date, data = .)
      data.frame(Date = .$Date, Fitted = predict(model, newdata = .))
    })
  
  # === Master Plot
  ggplot(ts_df, aes(x = Date, y = Temp)) +
    geom_line(color = "darkblue", size = 0.4) +
    geom_line(data = segment_fits, aes(y = Fitted), color = "#d73027", size = 1) +
    geom_vline(xintercept = break_dates, linetype = "dashed", color = "#08519c", size = 0.7) +
    annotate("text", x = break_dates, y = min(ts_df$Temp) + 0.1,
             label = paste("Break", seq_along(break_dates)),
             angle = 90, vjust = -0.4, size = 2.5, color = "#08519c") +
    labs(
      title = "Piecewise Linear Trends in GISTEMP",
      subtitle = "Breakpoints estimated using Bai–Perron procedure",
      x = "Year", y = "Temperature Anomaly (°C)"
    ) +
    scale_x_date(date_labels = "%Y", breaks = pretty_breaks(n = 10)) +
    theme_minimal(base_family = "serif", base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 11),
      plot.margin = margin(12, 14, 10, 14)
    )
}


# === STEP 6: Perform Wavelet Decomposition using Haar Filter ===
{# Apply DWT with Haar filter at 4 levels
  dwt_res <- dwt(gistemp_ts, filter = "haar", n.levels = 4)
  
  # Extract Components
  approx <- dwt_res@V[[1]]  # Low-frequency (approximation at Level 1)
  details <- dwt_res@W      # High-frequency (detail levels 1–4)
  
  # Create tidy data frames
  df_orig <- tibble(Date = as.Date(as.yearmon(time(gistemp_ts))), Value = as.numeric(gistemp_ts))
  df_approx <- tibble(Date = as.Date(as.yearmon(time(approx))), Value = approx)
  
  detail_dfs <- lapply(seq_along(details), function(i) {
    tibble(Date = as.Date(as.yearmon(time(details[[i]]))), Value = details[[i]], Level = paste0("Detail L", i))
  })
  
  # Plot Original and Approximation
  p_orig <- ggplot(df_orig, aes(Date, Value)) +
    geom_line(color = "black", linewidth = 1) +
    labs(title = "Original Temperature Series", x = NULL, y = NULL) +
    theme_minimal(base_family = "serif", base_size = 14)
  
  p_approx <- ggplot(df_approx, aes(Date, Value)) +
    geom_line(color = "firebrick", linewidth = 1) +
    labs(title = "Approximation (Low Frequency) – Level 1", x = NULL, y = NULL) +
    theme_minimal(base_family = "serif", base_size = 14)
  
  # Plot Detail Components
  p_details <- map(detail_dfs, function(df) {
    ggplot(df, aes(Date, Value)) +
      geom_line(color = "#3182bd", linewidth = 1) +
      labs(title = df$Level[1], x = NULL, y = NULL) +
      theme_minimal(base_family = "serif", base_size = 14)
  })
  
  # Compose final plot
  plot_grid(p_orig, p_approx, plotlist = p_details, ncol = 1, align = "v")
}

dev.off()  # This clears the current plotting device



# === STEP 7: Rolling Local Trend (10-year LOESS) ===
{gistemp_monthly %>%
    mutate(trend_10yr = slide_dbl(Anomaly, mean, .before = 60, .complete = TRUE)) %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = Anomaly), color = "gray80", size = 0.6) +
    geom_line(aes(y = trend_10yr), color = "blue", size = 1) +
    labs(
      title = "10-Year Rolling Average – GISTEMP",
      x = "Year",
      y = "Temperature Anomaly (°C)"
    ) +
    theme_minimal(base_family = "serif", base_size = 14) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 11)
    )
}

# === STEP 8: Kendall Test ===
mk_test <- MannKendall(gistemp_monthly$Anomaly)
print(mk_test)

#he Mann-Kendall test is a non-parametric test used to assess
# the trend in a time series. It evaluates if there is a statistically significant monotonic upward or downward trend in the data over time.
#A value of 0.671 indicates a moderate positive monotonic trend in the data, meaning that there is a significant increasing 
#trend over time in your temperature anomaly data (GISTEMP).




# === STEP 9: Acf ===

acf_vals <- acf(gistemp_ts, plot = FALSE)
seasonality_strength <- acf_vals$acf[13]  # lag 12 = 1 year

cat("Seasonality strength at lag 12:", round(seasonality_strength, 3), "\n")
#Peaks at lags 12, 24, etc. indicate annual seasonality.

# === STEP 10: ACF evolution ===
library(zoo)
library(tibble)
library(dplyr)
library(ggplot2)
library(scales)

# Rolling ACF for multiple lags
roll_acf_multi <- function(ts, max_lag = 60, window = 120) {
  result <- lapply(1:max_lag, function(lag) {
    acf_vals <- rollapply(ts, width = window, by = 1, align = "right", fill = NA, FUN = function(x) {
      acf(x, lag.max = lag, plot = FALSE)$acf[lag + 1]
    })
    tibble(Date = as.Date(as.yearmon(time(ts))), Lag = lag, ACF = acf_vals)
  })
  bind_rows(result) %>% filter(!is.na(ACF))
}

# Apply on GISTEMP
acf_gistemp <- roll_acf_multi(as.numeric(gistemp_ts), max_lag = 60)

# Updated color function: darker blue, fast fading
get_lag_color <- function(lags) {
  sapply(lags, function(lag) {
    if (lag == 12) return("#e41a1c")  # bold red
    dist <- abs(lag - 12)
    alpha <- exp(-dist / 5)  # fast fade
    rgb(90/255, 140/255, 190/255, alpha = alpha)  # darker blue
  })
}


# Line width function: thick for lag 12, thin otherwise
get_lag_linewidth <- function(lags) {
  sapply(lags, function(lag) {
    if (lag == 12) return(1.4) else return(0.4)
  })
}

acf_gistemp <- acf_gistemp %>% mutate(
  Lag = factor(Lag),
  linewidth = get_lag_linewidth(as.integer(Lag))
)
lag_colors <- get_lag_color(1:60)
names(lag_colors) <- as.character(1:60)

ggplot(acf_gistemp, aes(x = Date, y = ACF, color = Lag, group = Lag, linewidth = linewidth)) +
  geom_line() +
  geom_hline(yintercept = seasonality_strength, linetype = "dashed", color = "black")+

  scale_color_manual(values = lag_colors, guide = "none") +
  scale_linewidth_identity() +
  labs(
    title = "Rolling ACFs (Lags 1–60) – GISTEMP",
    subtitle = "Lag 12 in red (thick); others fade and are thinner",
    x = "Year", y = "Autocorrelation"
  ) +
  theme_minimal(base_family = "serif", base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11)
  )




# === STEP 11:  HP Filter
{
  # Load necessary libraries
  library(mFilter)
  library(strucchange)
  library(tidyverse)
  library(zoo)
  library(reshape2)
  library(scales)
  library(patchwork)
  
  # Assume 'gistemp_ts' is your monthly time series object
  # Ensure 'gistemp_ts' is a time series object with frequency 12
  gistemp_ts <- ts(gistemp_ts, frequency = 12, start = c(start_year, start_month))
  
  # 1. Standard HP Filter
  lambda_hp <- 129600  # Recommended lambda for monthly data
  hp_standard <- hpfilter(gistemp_ts, freq = lambda_hp, type = "lambda")
  
  # Calculate variance proportions
  trend_var_std <- var(hp_standard$trend, na.rm = TRUE)
  cycle_var_std <- var(hp_standard$cycle, na.rm = TRUE)
  total_var <- var(gistemp_ts, na.rm = TRUE)
  trend_prop_std <- trend_var_std / total_var
  cycle_prop_std <- cycle_var_std / total_var
  
  # 2. Breakpoint-Enhanced HP Filter
  # Detect breakpoints in the series
  bp_model <- breakpoints(gistemp_ts ~ 1)
  breakpoints <- breakpoints(bp_model)$breakpoints
  # Define segments based on breakpoints
  segments <- c(1, breakpoints, length(gistemp_ts))
  # Initialize trend and cycle vectors
  trend_bp <- rep(NA, length(gistemp_ts))
  cycle_bp <- rep(NA, length(gistemp_ts))
  # Apply HP filter to each segment
  for (i in 1:(length(segments) - 1)) {
    idx_start <- segments[i]
    idx_end <- segments[i + 1]
    segment_ts <- window(gistemp_ts, start = time(gistemp_ts)[idx_start], end = time(gistemp_ts)[idx_end])
    hp_seg <- hpfilter(segment_ts, freq = lambda_hp, type = "lambda")
    trend_bp[idx_start:idx_end] <- hp_seg$trend
    cycle_bp[idx_start:idx_end] <- hp_seg$cycle
  }
  # Calculate variance proportions
  trend_var_bp <- var(trend_bp, na.rm = TRUE)
  cycle_var_bp <- var(cycle_bp, na.rm = TRUE)
  trend_prop_bp <- trend_var_bp / total_var
  cycle_prop_bp <- cycle_var_bp / total_var
  
  # 3. Adaptive HP Filter
  # Define a time-varying lambda (e.g., increasing over time)
  time_index <- 1:length(gistemp_ts)
  lambda_adaptive <- lambda_hp * (1 + 0.5 * sin(2 * pi * time_index / length(gistemp_ts)))
  # Apply HP filter with varying lambda
  trend_adaptive <- rep(NA, length(gistemp_ts))
  cycle_adaptive <- rep(NA, length(gistemp_ts))
  for (i in 1:length(gistemp_ts)) {
    # Define a window around the current point
    window_size <- 60  # 5-year window
    idx_start <- max(1, i - window_size)
    idx_end <- min(length(gistemp_ts), i + window_size)
    segment_ts <- gistemp_ts[idx_start:idx_end]
    lambda_i <- lambda_adaptive[i]
    hp_seg <- hpfilter(segment_ts, freq = lambda_i, type = "lambda")
    # Assign the central trend value to the current point
    center_idx <- i - idx_start + 1
    trend_adaptive[i] <- hp_seg$trend[center_idx]
    cycle_adaptive[i] <- hp_seg$cycle[center_idx]
  }
  # Calculate variance proportions
  trend_var_adaptive <- var(trend_adaptive, na.rm = TRUE)
  cycle_var_adaptive <- var(cycle_adaptive, na.rm = TRUE)
  trend_prop_adaptive <- trend_var_adaptive / total_var
  cycle_prop_adaptive <- cycle_var_adaptive / total_var
  
  # Compile variance proportions into a data frame
  variance_df <- data.frame(
    Method = c("Standard HP", "Breakpoint-Enhanced HP", "Adaptive HP"),
    Trend_Proportion = c(trend_prop_std, trend_prop_bp, trend_prop_adaptive),
    Cycle_Proportion = c(cycle_prop_std, cycle_prop_bp, cycle_prop_adaptive)
  )
  
  # Print variance proportions
  print(variance_df)
  
  # Determine the best method based on highest trend proportion
  best_method <- variance_df$Method[which.max(variance_df$Trend_Proportion)]
  cat("Best method based on trend variance proportion:", best_method, "\n")
  
  # === Use best method to generate three plots ===
  if (best_method == "Standard HP") {
    trend_best <- hp_standard$trend
    cycle_best <- hp_standard$cycle
  } else if (best_method == "Breakpoint-Enhanced HP") {
    trend_best <- trend_bp
    cycle_best <- cycle_bp
  } else {
    trend_best <- trend_adaptive
    cycle_best <- cycle_adaptive
  }
  
  # 4. Prepare plot data
  dates <- as.Date(as.yearmon(time(gistemp_ts)))
  # 4. Prepare data frame
  df <- data.frame(
    Date     = dates,
    Original = as.numeric(gistemp_ts),
    Trend    = trend_best,
    Cycle    = cycle_best
  )
  
  # 1. Left panel: Original series + Trend
  p1 <- ggplot(df, aes(x = Date)) +
    geom_line(aes(y = Original), color = "gray60") +
    geom_line(aes(y = Trend), color = "blue", size = 1) +
    labs(title = paste0("Original Series + Trend (", best_method, ")"),
         y = "Anomaly (°C)", x = NULL) +
    theme_minimal(base_family = "serif") +
    theme(panel.grid = element_blank())
  
  # 2. Left panel: Cycle
  p2 <- ggplot(df, aes(x = Date, y = Cycle)) +
    geom_line(color = "firebrick") +
    labs(title = "Cyclical Component", y = "Cycle", x = NULL) +
    theme_minimal(base_family = "serif") +
    theme(panel.grid = element_blank())
  
  # Combine vertically
  left_panel <- p1 / p2
  
  # 3. Prepare single-column variance data
  var_best <- variance_df %>%
    filter(Method == best_method) %>%
    pivot_longer(cols = c(Trend_Proportion, Cycle_Proportion),
                 names_to = "Component", values_to = "Proportion") %>%
    mutate(Component = recode(Component, 
                              Trend_Proportion = "Trend", 
                              Cycle_Proportion = "Cycle"))
  
  # 4. True vertical bar plot (y = % variance)
  p3 <- ggplot(var_best, aes(x = Component, y = Proportion, fill = Component)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = c("Trend" = "blue", "Cycle" = "firebrick")) +
    labs(title = "Variance Explained", x = NULL, y = NULL) +
    theme_minimal(base_family = "serif") +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "bold"),
      axis.text.y = element_text(size = 10),
      legend.position = "none"
    )
  
  final_plot <- (left_panel | p3) + plot_layout(widths = c(3, 0.9))
  final_plot
  
  }


# === STEP 12:   EEMD (Rlibeemd) ===
{# Step 1: Convert your time series to numeric
  x <- as.numeric(gistemp_ts)
  time_vec <- as.Date(as.yearmon(time(gistemp_ts)))
  
  # Step 2: Run EEMD decomposition
  set.seed(123)
  eemd_result <- eemd(x, noise_strength = 0.2, ensemble_size = 250)
  
  # Step 3: Convert to long data frame
  imf_df <- as.data.frame(eemd_result)
  colnames(imf_df) <- paste0("IMF", 1:(ncol(imf_df)-1))  # Last column is residue
  colnames(imf_df)[ncol(imf_df)] <- "Residue"
  imf_df$Date <- time_vec
  
  # Step 4: Pivot to long format
  imf_long <- imf_df %>%
    pivot_longer(-Date, names_to = "Component", values_to = "Value")
  
  # All IMFs (except Residue)
  plot9 <- imf_long %>%
    filter(str_detect(Component, "IMF")) %>%
    ggplot(aes(x = Date, y = Value)) +
    geom_line(color = "steelblue") +
    facet_wrap(~Component, scales = "free_y", ncol = 1) +
    labs(title = "EEMD: Intrinsic Mode Functions (IMFs)", x = NULL, y = NULL) +
    theme_minimal(base_family = "serif") +
    theme(
      axis.text.y = element_text(size = 7),                  # smaller y labels
      axis.ticks.y = element_line(size = 0.2),               # thinner ticks
      strip.text = element_text(size = 9),                   # smaller facet titles
      panel.spacing = unit(0.2, "lines")                     # tighter spacing
    ) +
    scale_y_continuous(n.breaks = 3)                         # fewer y-axis ticks
  
  
  # === Residue (interpreted as nonlinear trend)
  plot11 <- imf_long %>%
    filter(Component == "Residue") %>%
    ggplot(aes(x = Date, y = Value)) +
    geom_line(color = "firebrick", size = 1) +
    labs(title = "EEMD: Nonlinear Trend (Residue)", x = NULL, y = "Anomaly") +
    theme_minimal(base_family = "serif")
  
  # === Combine 9 + 11 in a single vertical canvas
  plot9 / plot11 + plot_layout(heights = c(3, 1))
}

library(patchwork)

# Left panel: Original + Trend / Cycle
left_panel <- p1 / p2

# Middle panel: Variance bar plot
middle_panel <- p3

# Right panel: EEMD IMFs / Residue
right_panel <- plot9 / plot11 + plot_layout(heights = c(3, 1))

# Vertical spacer styled as a separator
vline <- plot_spacer() + 
  theme(panel.background = element_rect(fill = "black"), 
        plot.margin = margin(0, 0, 0, 0)) 

# Combine horizontally with separator
final_combined_plot <- (
  left_panel | 
    middle_panel | 
    vline | 
    right_panel
) + plot_layout(widths = c(2.5, 0.7, 0.03, 2.5))  # adjust separator width if needed

# Display
final_combined_plot



#== STEP 12 SEASONAL ANALYSIS
# Build base DataFrame
df_gistemp <- data.frame(
  Year = floor(time(gistemp_ts)),
  Month = cycle(gistemp_ts),
  Value = as.numeric(gistemp_ts)
)
df_10yr <- df_gistemp %>% filter(Year %% 10 == 0)

p1 <- ggplot(df_10yr, aes(x = Month, y = Value, color = factor(Year), group = Year)) +
  geom_line(size = 0.8, alpha = 0.8) +
  labs(title = "GISTEMP – One Year Every 10", x = "Month", y = "Anomaly (°C)", color = "Year") +
  theme_classic()
df_decade <- df_gistemp %>%
  mutate(Decade = 10 * floor(Year / 10)) %>%
  group_by(Decade, Month) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(df_decade, aes(x = Month, y = Value, color = factor(Decade), group = Decade)) +
  geom_line(size = 1, alpha = 0.85) +
  labs(title = "GISTEMP – Decadal Averages", x = "Month", y = "Anomaly (°C)", color = "Decade") +
  theme_classic()

p3 <- ggplot(df_10yr, aes(x = Month, y = Value, color = factor(Year), group = Year)) +
  geom_line(size = 1, alpha = 0.85) +
  coord_polar(start = 0) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "GISTEMP – Static Polar Plot (1 in 10 Years)", x = NULL, y = NULL, color = "Year") +
  theme_classic()


# Ensure Year is numeric and no NA values
df_anim <- df_gistemp %>%
  mutate(Year = as.numeric(Year)) %>%
  drop_na(Month, Value, Year)

# Build gganimate object
p4 <- ggplot(df_anim, aes(x = Month, y = Value, group = Year, color = Year)) +
  geom_line(size = 1.2, alpha = 0.85) +
  coord_polar(start = 0) +
  scale_color_viridis_c(option = "C", guide = "none") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "GISTEMP – Animated Polar Plot (Year: {frame_time})", x = NULL, y = NULL) +
  theme_minimal(base_size = 16) +
  transition_time(Year) +
  ease_aes("cubic-in-out")

# Animate and save
anim_obj <- gganimate::animate(
  p4,
  duration = 12,
  width = 8,
  height = 8,
  units = "in",
  res = 200,
  renderer = gifski_renderer()  # ✅ Required for saving
)

gganimate::anim_save("gistemp_polar_animation.gif", animation = anim_obj)


combined_p1_p2 <- p1 | p2

# Display
print(combined_p1_p2)


#p1 and p2 detrended version
{# Extract trend from best STL
  trend_component <- best_stl$time.series[, "trend"]
  
  # Create detrended series
  detrended_values <- as.numeric(gistemp_ts) - trend_component
  
  # Rebuild dataframe with detrended values
  df_detrended <- data.frame(
    Year = floor(time(gistemp_ts)),
    Month = cycle(gistemp_ts),
    Detrended = detrended_values
  )
  
  df_10yr <- df_detrended %>% filter(Year %% 10 == 0)
  
  p1 <- ggplot(df_10yr, aes(x = Month, y = Detrended, color = factor(Year), group = Year)) +
    geom_line(size = 0.8, alpha = 0.8) +
    labs(title = "GISTEMP – Detrended (1 Year Every 10)", x = "Month", y = "Detrended Anomaly (°C)", color = "Year") +
    theme_classic()
  df_decade <- df_detrended %>%
    mutate(Decade = 10 * floor(Year / 10)) %>%
    group_by(Decade, Month) %>%
    summarise(Detrended = mean(Detrended, na.rm = TRUE), .groups = "drop")
  
  p2 <- ggplot(df_decade, aes(x = Month, y = Detrended, color = factor(Decade), group = Decade)) +
    geom_line(size = 1, alpha = 0.85) +
    labs(title = "GISTEMP – Detrended Decadal Averages", x = "Month", y = "Detrended Anomaly (°C)", color = "Decade") +
    theme_classic()
  
  p1 | p2
}


#animation
# Extract seasonal component
seasonal_component <- best_stl$time.series[, "seasonal"]

# Reconstruct proper DataFrame
df_seasonal <- data.frame(
  Date = as.numeric(time(gistemp_ts)),                # Decimal year (e.g., 1980.083)
  Month = cycle(gistemp_ts),
  Seasonal = seasonal_component
) %>%
  mutate(
    Year = floor(Date)                                # Extract integer part
  ) %>%
  filter(!is.na(Year), !is.na(Seasonal))              # Ensure no NAs


library(gganimate)


pz <- ggplot(df_seasonal, aes(x = Month, y = Seasonal, group = Year, color = Year)) +
  geom_line(alpha = 0.6) +
  scale_color_viridis_c(option = "C") +  # Continuous scale
  labs(title = "Seasonal Pattern Over Time (Year: {frame_time})",
       x = "Month", y = "Seasonal Effect", color = "Year") +
  theme_classic() +
  transition_time(Year) +
  ease_aes("linear")


anim_obj <- gganimate::animate(
  pz,
  duration = 12,
  width = 10,
  height = 5,
  units = "in",
  res = 200,
  renderer = gifski_renderer()
)

gganimate::anim_save("gistemp_seasonal_animation.gif", animation = anim_obj)













###########################################################
# TASK 2
##########################################################
# === 1. PREPARE YOUR DATA ===
# (Assume gistemp_monthly is already loaded, with columns date (Date) & Anomaly (numeric).)

# 1A: Deseasonalize via STL
ts_anom <- ts(gistemp_monthly$Anomaly,
              frequency = 12,
              start = c(year(min(gistemp_monthly$date)),
                        month(min(gistemp_monthly$date))))
stl_obj        <- stl(ts_anom, s.window = 13)
deseasonalized <- stl_obj$time.series[, "trend"] +
  stl_obj$time.series[, "remainder"]

# 1B: Add adjusted_anomaly and time_variable to the main data.frame
gistemp_monthly <- gistemp_monthly %>%
  mutate(
    adjusted_anomaly = as.numeric(deseasonalized),
    time_variable    = scale(as.numeric(date))[, 1]
  )

n_obs <- nrow(gistemp_monthly)

# 1C: Build the helper data.frames exactly as before:
transition_covariate_df <- data.frame(
  adjusted_anomaly = gistemp_monthly$adjusted_anomaly,
  time_variable    = gistemp_monthly$time_variable
)

df_E <- data.frame(
  adjusted_anomaly = gistemp_monthly$adjusted_anomaly[-1],
  lag_anomaly      = gistemp_monthly$adjusted_anomaly[-n_obs]
)

df_F <- gistemp_monthly %>%
  transmute(
    adjusted_anomaly = adjusted_anomaly,
    month            = factor(month(date))
  )

df_Z <- gistemp_monthly %>%
  mutate(
    lag_anomaly   = lag(adjusted_anomaly),
    month         = factor(month(date)),
    time_variable = time_variable
  ) %>%
  filter(!is.na(lag_anomaly))


# === 2. FIT ALL MODELS (A, C, E, F, Z, LT, NLT) ===
fit_all_models <- function(expansion, max_states = 5, n_restarts = 3) {
  models <- list()
  
  for (k in 2:max_states) {
    model_key <- paste0(expansion, "_", k)
    
    models[[model_key]] <- tryCatch({
      best_ll  <- -Inf
      best_mod <- NULL
      
      for (i in 1:n_restarts) {
        set.seed(12345 + i)
        
        mod <- switch(expansion,
                      
                      # A: constant‐mean HMM
                      "A" = depmix(adjusted_anomaly ~ 1,
                                   nstates = k,
                                   data    = data.frame(adjusted_anomaly),
                                   family  = gaussian(),
                                   ntimes  = n_obs),
                      
                      # C: constant‐mean emission, time‐varying transitions
                      "C" = depmix(adjusted_anomaly ~ 1,
                                   nstates   = k,
                                   data      = transition_covariate_df,
                                   family    = gaussian(),
                                   transition = ~ time_variable,
                                   ntimes    = n_obs),
                      
                      # E: state‐dependent AR(1) emission
                      "E" = depmix(adjusted_anomaly ~ lag_anomaly,
                                   nstates = k,
                                   data    = df_E,
                                   family  = gaussian(),
                                   ntimes  = n_obs - 1),
                      
                      # F: seasonal emission via month factor
                      "F" = depmix(adjusted_anomaly ~ month,
                                   nstates = k,
                                   data    = df_F,
                                   family  = gaussian(),
                                   ntimes  = n_obs),
                      
                      # Z: full (lag + month in emission; time‐var transition)
                      "Z" = depmix(adjusted_anomaly ~ lag_anomaly + month,
                                   nstates   = k,
                                   data      = df_Z,
                                   family    = gaussian(),
                                   transition = ~ time_variable,
                                   ntimes    = nrow(df_Z)),
                      
                      # LT: state‐dependent *linear* trend HMM
                      "LT" = depmix(adjusted_anomaly ~ time_variable,
                                    nstates = k,
                                    data    = transition_covariate_df,
                                    family  = gaussian(),
                                    ntimes  = n_obs),
                      
                      # NLT: state‐dependent *quadratic* trend HMM
                      "NLT" = depmix(adjusted_anomaly ~ time_variable + I(time_variable^2),
                                     nstates = k,
                                     data    = transition_covariate_df,
                                     family  = gaussian(),
                                     ntimes  = n_obs),
                      
                      stop("Unknown expansion code: ", expansion)
        )
        
        fitted_mod <- fit(mod, verbose = FALSE,
                          control = list(maxit = 1000, tol = 1e-8))
        ll <- logLik(fitted_mod)
        
        if (!is.na(ll) && ll > best_ll) {
          best_ll  <- ll
          best_mod <- fitted_mod
        }
      }
      best_mod
    },
    error = function(e) {
      message(paste("Model", model_key, "failed:", e$message))
      NULL
    })
  }
  
  # Remove any NULLs
  models[!sapply(models, is.null)]
}

# 2B. Call fit_all_models for each expansion
models_A   <- fit_all_models("A")
models_C   <- fit_all_models("C")
models_E   <- fit_all_models("E")
models_F   <- fit_all_models("F")
models_Z   <- fit_all_models("Z")
models_LT  <- fit_all_models("LT")   # Linear‐trend HMM
models_NLT <- fit_all_models("NLT")  # Quadratic‐trend HMM


# === 3. AIC/BIC SUMMARY ACROSS ALL EXPANSIONS ===
get_scores <- function(model_list) {
  data.frame(
    Model     = names(model_list),
    Expansion = sub("_.*", "", names(model_list)),
    States    = sapply(model_list, function(m) m@nstates),
    AIC       = sapply(model_list, AIC),
    BIC       = sapply(model_list, BIC),
    stringsAsFactors = FALSE
  )
}

scores_all <- bind_rows(
  get_scores(models_A),
  get_scores(models_C),
  get_scores(models_E),
  get_scores(models_F),
  get_scores(models_Z),
  get_scores(models_LT),
  get_scores(models_NLT)
) %>%
  arrange(AIC) %>%
  mutate(Model = factor(Model, levels = Model))


# === 4. PLOT AIC & BIC RANKINGS (ALL REMAINING MODELS) ===
expansion_colors <- c(
  "A"   = "#1b9e77",
  "C"   = "#d95f02",
  "E"   = "#e7298a",
  "F"   = "#66a61e",
  "Z"   = "#e6ab02",
  "LT"  = "#7570b3",
  "NLT" = "#a6761d"
)

theme_clean <- theme_minimal(base_size = 13) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 11),
    plot.title      = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle   = element_text(size = 11, hjust = 0.5, color = "grey30"),
    legend.title    = element_blank(),
    legend.position = "top"
  )

plot_aic <- ggplot(scores_all, aes(x = Model, y = AIC, fill = Expansion)) +
  geom_col(width = 0.65, color = "white") +
  geom_text(aes(label = round(AIC, 1)), vjust = -0.7, size = 3.3) +
  scale_fill_manual(values = expansion_colors) +
  labs(
    title    = "Model Ranking by AIC",
    subtitle = "Lower AIC, better fit",
    x        = "Model",
    y        = "AIC"
  ) +
  theme_clean

plot_bic <- ggplot(
  scores_all %>% arrange(BIC) %>% mutate(Model = factor(Model, levels = Model)),
  aes(x = Model, y = BIC, fill = Expansion)
) +
  geom_col(width = 0.65, color = "white") +
  geom_text(aes(label = round(BIC, 1)), vjust = -0.7, size = 3.3) +
  scale_fill_manual(values = expansion_colors) +
  labs(
    title    = "Model Ranking by BIC",
    subtitle = "Lower BIC, better fit w/ penalty",
    x        = "Model",
    y        = "BIC"
  ) +
  theme_clean

combined_plot <- plot_aic | plot_bic
print(combined_plot)

# Optionally save:
# ggsave("model_comparison_AIC_BIC_LT_NLT.png", combined_plot, width = 14, height = 6, dpi = 320)


# === 5. POSTERIOR‐DECODE THE BEST MODEL (BY BIC) ===
all_models     <- c(models_A, models_C, models_E, models_F, models_Z, models_LT, models_NLT)
best_model_name <- as.character(scores_all %>% arrange(BIC) %>% slice(1) %>% pull(Model))
best_model      <- all_models[[ best_model_name ]]
n_states        <- best_model@nstates
expansion_code  <- sub("_.*", "", best_model_name)

# 5A. Build the correct data.frame for that model’s training data
data_for_best <- switch(
  expansion_code,
  
  "A" = data.frame(
    date             = gistemp_monthly$date,
    adjusted_anomaly = gistemp_monthly$adjusted_anomaly
  ),
  
  "C" = data.frame(
    date             = gistemp_monthly$date,
    adjusted_anomaly = gistemp_monthly$adjusted_anomaly
  ),
  
  "E" = data.frame(
    date             = gistemp_monthly$date[-1],
    adjusted_anomaly = df_E$adjusted_anomaly
  ),
  
  "F" = data.frame(
    date             = gistemp_monthly$date,
    adjusted_anomaly = df_F$adjusted_anomaly
  ),
  
  "Z" = data.frame(
    date             = df_Z$date,
    adjusted_anomaly = df_Z$Anomaly
  ),
  
  "LT" = data.frame(
    date             = gistemp_monthly$date,
    adjusted_anomaly = gistemp_monthly$adjusted_anomaly,
    time_variable    = gistemp_monthly$time_variable
  ),
  
  "NLT" = data.frame(
    date             = gistemp_monthly$date,
    adjusted_anomaly = gistemp_monthly$adjusted_anomaly,
    time_variable    = gistemp_monthly$time_variable
  )
)

# 5B. Extract posterior states (+ state probabilities if present)
hmm_post            <- posterior(best_model)
data_for_best$state <- factor(hmm_post$state)

for (j in 1:n_states) {
  sc <- paste0("S", j)
  if (sc %in% names(hmm_post)) {
    data_for_best[[paste0("prob", j)]] <- hmm_post[[sc]]
  }
}

# 5C. Extract state‐specific parameters via getpars()
params  <- getpars(best_model)
par_mat <- matrix(params, byrow = TRUE, nrow = n_states)

if (expansion_code == "LT") {
  # LT: each row of par_mat is (intercept_j, slope_j)
  intercepts <- par_mat[, 1]
  slopes     <- par_mat[, 2]
  tvar       <- data_for_best$time_variable
  
  data_for_best$implied <- intercepts[data_for_best$state] +
    slopes[data_for_best$state] * tvar
  
} else if (expansion_code == "NLT") {
  # NLT: each row is (intercept_j, slope_j, quad_j)
  intercepts <- par_mat[, 1]
  slopes     <- par_mat[, 2]
  quads      <- par_mat[, 3]
  tvar       <- data_for_best$time_variable
  
  data_for_best$implied <- intercepts[data_for_best$state] +
    slopes[data_for_best$state] * tvar +
    quads[data_for_best$state]  * (tvar^2)
  
} else {
  # A, C, E, F, Z: constant‐mean emission (one intercept per state)
  implied_means <- sapply(best_model@response,
                          function(r) r[[1]]@parameters$coefficients)
  data_for_best$implied <- implied_means[ as.numeric(data_for_best$state) ]
}


# === 6. PLOT POSTERIOR‐DECODED STATES with “implied” fit line ===

# Force correct state levels
data_for_best$state <- factor(data_for_best$state, levels = c("1", "2", "3"))

# Ultra-contrasted, professional palette
hmm_colors <- c(
  "1" = "#E63946",  # vivid red
  "2" = "#1D3557",  # deep cyan/navy
  "3" = "#F6C48D"   # soft apricot
)

# Plot
p_state <- ggplot(data_for_best, aes(x = date, y = adjusted_anomaly)) +
  geom_line(color = "#2C3E50", linewidth = 0.6) +
  geom_point(aes(color = state), size = 1.8, alpha = 0.95) +
  scale_color_manual(values = hmm_colors) +
  labs(
    title = paste("HMM States (Model", best_model_name, ")"),
    y = "Temperature Anomaly (°C)", x = NULL
  ) +
  theme_custom

print(p_state)




# === Use C_5 explicitly ===
{model_C5 <- models_C[["C_5"]]
  n_states <- model_C5@nstates
  mu_C5 <- sapply(model_C5@response, function(r) r[[1]]@parameters$coefficients)
  
  # Posterior decoding
  hmm_C5 <- posterior(model_C5)
  gistemp_monthly$state_C5 <- factor(hmm_C5$state)
  for (j in 1:n_states) {
    gistemp_monthly[[paste0("prob_C5_", j)]] <- hmm_C5[[paste0("S", j)]]
  }
  gistemp_monthly$implied_C5 <- sapply(hmm_C5$state, function(s) mu_C5[s])
  
  # === Panel (a): HMM States (C5) ===
  p1_c5 <- ggplot(gistemp_monthly, aes(x = date, y = Anomaly)) +
    geom_line(color = "black", linewidth = 0.5) +
    geom_point(aes(color = state_C5), size = 1.3) +
    scale_color_manual(values = RColorBrewer::brewer.pal(n_states, "Dark2")) +
    labs(title = "Panel (a): HMM States (Model C_5)", y = "Anomaly", x = "") +
    theme_custom
  
  # === Panel (b): State Probabilities (C5) ===
  p2_c5 <- ggplot(gistemp_monthly, aes(x = date)) +
    labs(title = "Panel (b): State Probabilities (Model C_5)", y = "P(State)", x = "") +
    theme_custom
  
  for (j in 1:n_states) {
    p2_c5 <- p2_c5 +
      geom_line(aes_string(y = paste0("prob_C5_", j)),
                color = RColorBrewer::brewer.pal(n_states, "Set1")[j],
                linewidth = 0.5)
  }
  
  # === Panel (c): Implied vs Real Anomaly (C5) ===
  p3_c5 <- ggplot(gistemp_monthly, aes(x = date)) +
    geom_line(aes(y = Anomaly), color = "darkblue", linewidth = 0.5) +
    geom_line(aes(y = implied_C5), color = "red", linetype = "dashed", linewidth = 0.8) +
    labs(title = "Panel (c): Real vs Implied Anomaly (Model C_5)", y = "Anomaly", x = "") +
    theme_custom
  
  # === Save and Print ===
  final_plot_c5 <- p1_c5 / p2_c5
  ggsave("hmm_model_C5_panels.png", final_plot_c5, width = 10, height = 10)
  print(final_plot_c5)
  print(p3_c5)
}
#model f - The observation distribution depends on the month (i.e., seasonal fixed effects).
# model d - The emission (observation) distribution depends not only on the current hidden state but also on the previous observed value.


# === Transition matrix for Model A_5 ===
{model_A5 <- models_A[["A_5"]]
  n_states <- model_A5@nstates
  trans_objs <- model_A5@transition
  trans_matrix_A5 <- matrix(NA, nrow = n_states, ncol = n_states)
  for (i in 1:n_states) {
    trans_matrix_A5[i, ] <- getpars(trans_objs[[i]])
  }
  rownames(trans_matrix_A5) <- paste0("From_S", 1:n_states)
  colnames(trans_matrix_A5) <- paste0("To_S", 1:n_states)
  print(round(trans_matrix_A5, 4))
  
  # === Heatmap of transition matrix ===
  df_long <- melt(round(trans_matrix_A5, 4), varnames = c("From", "To"), value.name = "Probability")
  ggplot(df_long, aes(x = To, y = From, fill = Probability)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.3f", Probability)), size = 3.5, color = "black") +
    scale_fill_gradient(low = "white", high = "#08519c") +
    labs(title = "HMM Transition Matrix (Model A, 5 States)",
         x = "To State", y = "From State") +
    theme_minimal(base_size = 13) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

#===transition probability model c_5
{
  # Extract fitted model
  model_C5 <- models_C[["C_5"]]
  n_states <- model_C5@nstates
  trans_models <- model_C5@transition
  
  # Scaled time (exactly as used in fitting)
  time_variable <- scale(as.numeric(gistemp_monthly$date))[,1]
  n_time <- length(time_variable)
  
  # Softmax function
  softmax <- function(x) exp(x) / sum(exp(x))
  
  # Storage: [time, from_state, to_state]
  transition_over_time <- array(NA, dim = c(n_time, n_states, n_states))
  
  # Loop over source states
  for (i in 1:n_states) {
    # Extract coefficients (first is intercept, second is slope w.r.t. time_variable)
    coefs <- matrix(getpars(trans_models[[i]]), nrow = n_states - 1, byrow = TRUE)
    coefs <- rbind(0, coefs)  # baseline category has logit 0 (reference level)
    
    # For each time point, compute logits and softmax
    for (t in 1:n_time) {
      x_t <- time_variable[t]
      logits <- coefs[,1] + coefs[,2] * x_t
      probs <- softmax(logits)
      transition_over_time[t, i, ] <- probs
    }
  }
  
  # Reshape for plotting
  df_long <- data.frame()
  for (i in 1:n_states) {
    for (j in 1:n_states) {
      df_long <- rbind(df_long, data.frame(
        time = gistemp_monthly$date,
        From = paste0("From_S", i),
        To = paste0("To_S", j),
        Probability = transition_over_time[, i, j]
      ))
    }
  }
  
  # Plot
  ggplot(df_long, aes(x = time, y = Probability, color = To)) +
    geom_line(linewidth = 0.6) +
    facet_wrap(~ From, ncol = 1, scales = "free_y") +
    labs(title = "Transition Probabilities Over Time (Model C, 5 States)",
         x = "Time", y = "Transition Probability") +
    scale_color_brewer(palette = "Set1") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top",
          strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank())
}

#=== Animated transition matrix
{
  p <- ggplot(df_long, aes(x = To, y = From, fill = Probability)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", Probability)), size = 3) +
    scale_fill_gradient(low = "white", high = "#08519c") +
    labs(title = 'Transition Matrix Over Time (Model C, 5 States)\n{closest_state}',
         x = 'To State', y = 'From State') +
    transition_states(TimeStr, transition_length = 2, state_length = 1) +
    ease_aes('linear') +
    theme_minimal(base_size = 13) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Render in memory using gifski (no file output, no PNGs saved)
  animate(p, nframes = 100, fps = 10, renderer = gifski_renderer())
}

#=== Extra Analysis on c_5

# === Fit best C5 model (assumes gistemp_monthly exists with Anomaly + date) ===

# Custom state colors: red → blue gradient
state_colors <- c(
  "1" = "#D73027",  # red
  "2" = "#FC8D59",  # orange
  "3" = "#FEE08B",  # yellow
  "4" = "#91BFDB",  # light blue
  "5" = "#4575B4"   # blue
)

time_variable <- scale(as.numeric(gistemp_monthly$date))[, 1]

transition_covariate_df <- data.frame(
  adjusted_anomaly = gistemp_monthly$Anomaly,
  time_variable = time_variable
)

set.seed(123)
model_C5 <- depmix(
  adjusted_anomaly ~ 1,
  data = transition_covariate_df,
  nstates = 5,
  family = gaussian(),
  transition = ~ time_variable
)

fitted_C5 <- fit(model_C5, verbose = FALSE)
hmm_post <- posterior(fitted_C5)
gistemp_monthly$hmm_state <- hmm_post$state

# === 1. State Durations ===
state_durations <- table(hmm_post$state)
state_duration_df <- data.frame(
  State = factor(names(state_durations), levels = as.character(1:5)),
  Duration = as.vector(state_durations)
)

state_duration_plot <- ggplot(state_duration_df, aes(x = State, y = Duration, fill = State)) +
  geom_bar(stat = "identity") +
  labs(title = "State Durations", x = "State", y = "Duration") +
  scale_fill_manual(values = state_colors) +
  theme_minimal() +
  theme(legend.position = "none")

# === 2. Mean Anomaly by State ===
state_means <- gistemp_monthly %>%
  group_by(hmm_state) %>%
  summarise(mean_anomaly = mean(Anomaly, na.rm = TRUE))

mean_anomaly_plot <- ggplot(state_means, aes(x = factor(hmm_state), y = mean_anomaly, fill = factor(hmm_state))) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Adjusted Anomaly by State", x = "State", y = "Mean Anomaly") +
  scale_fill_manual(values = state_colors) +
  theme_minimal() +
  theme(legend.position = "none")

# === 3. Volatility (SD) by State ===
state_sd <- gistemp_monthly %>%
  group_by(hmm_state) %>%
  summarise(sd_anomaly = sd(Anomaly, na.rm = TRUE))

volatility_plot <- ggplot(state_sd, aes(x = factor(hmm_state), y = sd_anomaly, fill = factor(hmm_state))) +
  geom_bar(stat = "identity") +
  labs(title = "State-wise Volatility (SD)", x = "State", y = "Volatility") +
  scale_fill_manual(values = state_colors) +
  theme_minimal() +
  theme(legend.position = "none")

# === 4. State Transition Frequencies ===
transition_df <- data.frame(
  From = factor(hmm_post$state[-nrow(hmm_post)], levels = 1:5),
  To   = factor(hmm_post$state[-1], levels = 1:5)
)

state_transition_plot <- ggplot(transition_df, aes(x = From, fill = To)) +
  geom_bar(position = "dodge") +
  labs(title = "Transition Frequencies", x = "From State", y = "Count", fill = "To State") +
  scale_fill_manual(values = state_colors) +
  theme_minimal()

# === Combine all plots ===
combined_plot <- (state_duration_plot) /
  (mean_anomaly_plot | volatility_plot) /
  state_transition_plot +
  plot_layout(heights = c(1, 1, 1.2))

# Display and save
print(combined_plot)
ggsave("C5_HMM_Analysis_Combined.png", combined_plot, width = 16, height = 12, dpi = 300)







###########################################################
# TASK 3: DLM Forecasting for San Francisco Downtown
###########################################################

# 1. LOAD & PREPARE DAILY GHCN DATA
{
  
  # 1.1 Read raw GHCN file (adjust path)
  ghcn_path <- "/Users/Alessandro/Desktop/final project tsa/ghcn.txt"
  ghcn <- read_csv(ghcn_path, col_types = cols(
    ID        = col_character(),
    station   = col_character(),
    latitude  = col_double(),
    longitude = col_double(),
    elevation = col_double(),
    date      = col_date(format = "%Y-%m-%d"),
    TMIN      = col_double(),
    TMAX      = col_double(),
    TAVG      = col_double(),
    PRCP      = col_double()
  ))
  
  # 1.2 Keep only “San Francisco Downtown” station; convert to °C
  sf_daily <- ghcn %>%
    filter(str_detect(station, regex("SAN FRANCISCO DWTN", ignore_case = TRUE))) %>%
    mutate(
      TMIN = TMIN / 10,
      TMAX = TMAX / 10
    ) %>%
    dplyr::select(date, TMIN, TMAX) %>%
    arrange(date)
  
  # 1.3 Compute day‐of‐year index and keep only date, doy, TMIN, TMAX
  sf_daily <- sf_daily %>%
    mutate(doy = yday(date)) %>%
    dplyr::select(date, doy, TMIN, TMAX)
  
  climatology <- sf_daily %>%
    group_by(doy) %>%
    summarise(
      Smin = mean(TMIN, na.rm = TRUE),
      Smax = mean(TMAX, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 1.4 Deseasonalize via STL (keep date, doy, TMIN, TMAX, Smin, Smax, Xmin, Xmax)
  {
    yr0  <- year(min(sf_daily$date))
    doy0 <- yday(min(sf_daily$date))
    tmin_ts <- ts(sf_daily$TMIN, start = c(yr0, doy0), frequency = 365)
    tmax_ts <- ts(sf_daily$TMAX, start = c(yr0, doy0), frequency = 365)
    stl_min <- stl(tmin_ts, s.window = 3)
    stl_max <- stl(tmax_ts, s.window = 3)
    sf_daily$Smin <- as.numeric(stl_min$time.series[, "seasonal"])
    sf_daily$Smax <- as.numeric(stl_max$time.series[, "seasonal"])
    sf_daily <- sf_daily %>%
      mutate(
        Xmin = TMIN - Smin,
        Xmax = TMAX - Smax
      ) %>%
      dplyr::select(date, doy, TMIN, TMAX, Smin, Smax, Xmin, Xmax)
    }
}

# 2. SPLIT & FORM DATA MATRICES
{
  Y_daily <- sf_daily %>%
    dplyr::select(Xmin, Xmax) %>%
    as.matrix()
  n_total <- nrow(Y_daily)
  n_train <- floor(0.95 * n_total)
  Y_train <- Y_daily[1:n_train, ]
  Y_test  <- Y_daily[(n_train + 1):n_total, ]
  Smin_test <- sf_daily$Smin[(n_train + 1):n_total]
  Smax_test <- sf_daily$Smax[(n_train + 1):n_total]
  
}
dates_test <- sf_daily$date[(n_train + 1):n_total]
doy_test   <- sf_daily$doy[(n_train + 1):n_total]
H1         <- length(dates_test)


# 3. DYNAMIC LINEAR MODEL DEFINITIONS

# 3.1 Model (a): independent random walks (W, V diagonal)
build_a <- function(parm) {
  σ2_w1 <- exp(parm[1]); σ2_w2 <- exp(parm[2])
  σ2_v1 <- exp(parm[3]); σ2_v2 <- exp(parm[4])
  dlm(
    FF = diag(2),
    V  = diag(c(σ2_v1, σ2_v2)),
    GG = diag(2),
    W  = diag(c(σ2_w1, σ2_w2)),
    m0 = rep(0, 2),
    C0 = diag(1e6, 2)
  )
}
init_a <- log(c(
  var(Y_train[, 1], na.rm = TRUE) * 0.01,
  var(Y_train[, 2], na.rm = TRUE) * 0.01,
  var(Y_train[, 1], na.rm = TRUE) * 0.1,
  var(Y_train[, 2], na.rm = TRUE) * 0.1
))

# 3.2 Model (b): “SURE” random walks (V diagonal, W full)
build_b <- function(parm) {
  σ2_w11 <- exp(parm[1]); σ2_w22 <- exp(parm[2])
  ρ_w     <- tanh(parm[3])
  cov_w   <- ρ_w * sqrt(σ2_w11 * σ2_w22)
  Wmat    <- matrix(c(σ2_w11, cov_w, cov_w, σ2_w22), 2, 2)
  σ2_v1   <- exp(parm[4]); σ2_v2 <- exp(parm[5])
  Vmat    <- diag(c(σ2_v1, σ2_v2))
  dlm(
    FF = diag(2),
    V  = Vmat,
    GG = diag(2),
    W  = Wmat,
    m0 = rep(0, 2),
    C0 = diag(1e6, 2)
  )
}
init_b <- c(
  log(var(Y_train[, 1], na.rm = TRUE) * 0.01),
  log(var(Y_train[, 2], na.rm = TRUE) * 0.01),
  atanh(0),
  log(var(Y_train[, 1], na.rm = TRUE) * 0.1),
  log(var(Y_train[, 2], na.rm = TRUE) * 0.1)
)

# 3.3 Model (c): common latent factor (θ = [μ1, μ2, f_t])
build_c_fixed <- function(parm) {
  mu1    <- parm[1]; mu2    <- parm[2]; beta   <- parm[3]
  σ2_w   <- exp(parm[4]); σ2_v1  <- exp(parm[5]); σ2_v2  <- exp(parm[6])
  Fmat <- matrix(c(1, 0, beta, 0, 1, 1), nrow = 2, byrow = TRUE)
  Gmat <- diag(3)
  Wmat <- matrix(0, 3, 3); Wmat[3, 3] <- σ2_w
  Vmat <- diag(c(σ2_v1, σ2_v2))
  dlm(
    FF = Fmat,
    V  = Vmat,
    GG = Gmat,
    W  = Wmat,
    m0 = c(mu1, mu2, 0),
    C0 = diag(c(0, 0, 1e6), 3)
  )
}
init_c_fixed <- c(
  mean(Y_train[, 1]), mean(Y_train[, 2]), 1,
  log(var(Y_train[, 1], na.rm = TRUE) * 0.01),
  log(var(Y_train[, 1], na.rm = TRUE) * 0.1),
  log(var(Y_train[, 2], na.rm = TRUE) * 0.1)
)


# 4. MAXIMUM LIKELIHOOD ESTIMATION

# 4.1 Fit Model (a)
fit_a <- dlmMLE(
  y     = Y_train,
  build = build_a,
  parm  = init_a,
  lower = rep(log(1e-8), 4),
  upper = rep(log(1e2), 4)
)
mod_a <- build_a(fit_a$par)

# 4.2 Fit Model (b)
fit_b <- dlmMLE(
  y     = Y_train,
  build = build_b,
  parm  = init_b,
  lower = c(log(1e-8), log(1e-8), -5, log(1e-8), log(1e-8)),
  upper = c(log(1e2), log(1e2),  5, log(1e2), log(1e2))
)
mod_b <- build_b(fit_b$par)

# 4.3 Fit Model (c)
fit_c2 <- dlmMLE(
  y     = Y_train,
  build = build_c_fixed,
  parm  = init_c_fixed,
  lower = c(-Inf, -Inf, -Inf, log(1e-8), log(1e-8), log(1e-8)),
  upper = c( Inf,  Inf,  Inf, log(1e2),  log(1e2),  log(1e2))
)
mod_c2 <- build_c_fixed(fit_c2$par)
par_c <- fit_c2$par

# 5. ONE‐STEP‐AHEAD FORECASTS & 95% CI
# 5.1 Combine train + NA for test; filter
Y_combined <- rbind(Y_train, matrix(NA, nrow = nrow(Y_test), ncol = 2))
filt_a_all <- dlmFilter(Y_combined, mod_a)
filt_b_all <- dlmFilter(Y_combined, mod_b)
filt_c_all <- dlmFilter(rbind(Y_train, matrix(NA, nrow = nrow(Y_test), ncol = 2)), mod_c2)

# 5.2 Extract one‐step‐ahead means
fore_a_mu <- filt_a_all$f[(n_train + 1):n_total, ]  # H1 × 2
fore_b_mu <- filt_b_all$f[(n_train + 1):n_total, ]
a_c       <- filt_c_all$a[(n_train + 1):n_total, ]  # H1 × 3
fore_c_mu <- t(mod_c2$FF %*% t(a_c))                # H1 × 2

# 5.3 Define H1, pre‐allocate arrays
H1 <- nrow(Y_test)
R_a_arr <- array(0, dim = c(2, 2, H1))
R_b_arr <- array(0, dim = c(2, 2, H1))
R_c_arr <- array(0, dim = c(3, 3, H1))
fore_a_var <- matrix(0, nrow = H1, ncol = 2)
fore_b_var <- matrix(0, nrow = H1, ncol = 2)
fore_c_var <- matrix(0, nrow = H1, ncol = 2)

# 5.4 Reconstruct R_{t|t-1} via dlmSvd2var()
for (i in 1:H1) {
  idx <- n_train + i
  
  # Model A
  if (is.list(filt_a_all$U.R)) {
    U_a <- filt_a_all$U.R[[idx]]
    D_a <- if (is.list(filt_a_all$D.R)) filt_a_all$D.R[[idx]] else filt_a_all$D.R[idx, ]
  } else {
    U_a <- filt_a_all$U.R[,, idx]
    D_a <- filt_a_all$D.R[idx, ]
  }
  R_a_arr[,, i] <- dlmSvd2var(U_a, D_a)
  
  # Model B
  if (is.list(filt_b_all$U.R)) {
    U_b <- filt_b_all$U.R[[idx]]
    D_b <- if (is.list(filt_b_all$D.R)) filt_b_all$D.R[[idx]] else filt_b_all$D.R[idx, ]
  } else {
    U_b <- filt_b_all$U.R[,, idx]
    D_b <- filt_b_all$D.R[idx, ]
  }
  R_b_arr[,, i] <- dlmSvd2var(U_b, D_b)
  
  # Model C
  if (is.list(filt_c_all$U.R)) {
    U_c <- filt_c_all$U.R[[idx]]
    D_c <- if (is.list(filt_c_all$D.R)) filt_c_all$D.R[[idx]] else filt_c_all$D.R[idx, ]
  } else {
    U_c <- filt_c_all$U.R[,, idx]
    D_c <- filt_c_all$D.R[idx, ]
  }
  R_c_arr[,, i] <- dlmSvd2var(U_c, D_c)
}

# 5.5 Compute one‐step‐ahead observation variances
V_a  <- mod_a$V;   FF_a <- mod_a$FF
V_b  <- mod_b$V;   FF_b <- mod_b$FF
V_c  <- mod_c2$V;  FF_c <- mod_c2$FF

for (i in 1:H1) {
  Qa <- FF_a %*% R_a_arr[,, i] %*% t(FF_a) + V_a
  Qb <- FF_b %*% R_b_arr[,, i] %*% t(FF_b) + V_b
  Rc_h <- R_c_arr[,, i]
  Qc <- FF_c %*% Rc_h %*% t(FF_c) + V_c
  fore_a_var[i, ] <- diag(Qa)
  fore_b_var[i, ] <- diag(Qb)
  fore_c_var[i, ] <- diag(Qc)
}

# 5.6 Compute deseasonalized 95% CI bounds
z <- 1.96
fore_a_lower <- fore_a_mu - z * sqrt(fore_a_var)
fore_a_upper <- fore_a_mu + z * sqrt(fore_a_var)
fore_b_lower <- fore_b_mu - z * sqrt(fore_b_var)
fore_b_upper <- fore_b_mu + z * sqrt(fore_b_var)
fore_c_lower <- fore_c_mu - z * sqrt(fore_c_var)
fore_c_upper <- fore_c_mu + z * sqrt(fore_c_var)


# 5.7 Reconstruct raw‐scale forecasts and CI (without using future seasonal components)
{
  # Use training period seasonal component for forecasts (based on climatology)
  Smin_train <- climatology$Smin  # Seasonal component from training period
  Smax_train <- climatology$Smax  # Seasonal component from training period
  
  # Reconstruct raw forecasts by adding seasonal adjustment from the training period
  raw_a_mu <- cbind(
    TMIN = fore_a_mu[, 1] ,
    TMAX = fore_a_mu[, 2]
  )
  raw_b_mu <- cbind(
    TMIN = fore_b_mu[, 1] ,
    TMAX = fore_b_mu[, 2] 
  )
  raw_c_mu <- cbind(
    TMIN = fore_c_mu[, 1] ,
    TMAX = fore_c_mu[, 2] 
  )
  
  # Reconstruct the lower and upper bounds for the forecast confidence intervals
  raw_a_lower <- cbind(
    TMIN = fore_a_lower[, 1] ,
    TMAX = fore_a_lower[, 2] 
  )
  raw_a_upper <- cbind(
    TMIN = fore_a_upper[, 1] ,
    TMAX = fore_a_upper[, 2] 
  )
  raw_b_lower <- cbind(
    TMIN = fore_b_lower[, 1] ,
    TMAX = fore_b_lower[, 2] 
  )
  raw_b_upper <- cbind(
    TMIN = fore_b_upper[, 1] ,
    TMAX = fore_b_upper[, 2] 
  )
  raw_c_lower <- cbind(
    TMIN = fore_c_lower[, 1] ,
    TMAX = fore_c_lower[, 2] 
  )
  raw_c_upper <- cbind(
    TMIN = fore_c_upper[, 1],
    TMAX = fore_c_upper[, 2] 
  )
}


# 6. EVALUATION: RMSE & MAE ON DESEASONALIZED
{
  err_a <- Y_test - fore_a_mu
  err_b <- Y_test - fore_b_mu
  err_c <- Y_test - fore_c_mu
  rmse_a <- sqrt(colMeans(err_a^2, na.rm = TRUE))
  mae_a  <- colMeans(abs(err_a), na.rm = TRUE)
  rmse_b <- sqrt(colMeans(err_b^2, na.rm = TRUE))
  mae_b  <- colMeans(abs(err_b), na.rm = TRUE)
  rmse_c <- sqrt(colMeans(err_c^2, na.rm = TRUE))
  mae_c  <- colMeans(abs(err_c), na.rm = TRUE)
  cat("Model (a) Deseasonalized – RMSE:", round(rmse_a, 4), " MAE:", round(mae_a, 4), "\n")
  cat("Model (b) Deseasonalized – RMSE:", round(rmse_b, 4), " MAE:", round(mae_b, 4), "\n")
  cat("Model (c) Deseasonalized – RMSE:", round(rmse_c, 4), " MAE:", round(mae_c, 4), "\n")
}


# 8. EXTRACT & PRINT PARAMETER ESTIMATES
{
  W_a   <- mod_a$W;    V_a   <- mod_a$V
  σ2w_a <- diag(W_a);  σ2v_a <- diag(V_a)
  cat("Model (a) σ²_w:", round(σ2w_a, 6), " σ²_v:", round(σ2v_a, 6), "\n")
  W_b      <- mod_b$W; V_b   <- mod_b$V
  σ2w_b11  <- W_b[1, 1]; σ2w_b22 <- W_b[2, 2]
  ρw_b     <- W_b[1, 2] / sqrt(σ2w_b11 * σ2w_b22)
  σ2v_b    <- diag(V_b)
  cat("Model (b) σ²_w11:", round(σ2w_b11, 6), "σ²_w22:", round(σ2w_b22, 6),
      " ρ_w:", round(ρw_b, 4), " σ²_v:", round(σ2v_b, 6), "\n")
  α1_c    <- par_c[1]; α2_c    <- par_c[2]; β_c     <- par_c[3]
  σ2w_c   <- exp(par_c[4]); σ2v1_c  <- exp(par_c[5]); σ2v2_c  <- exp(par_c[6])
  cat("Model (c) μ1:", round(α1_c, 4), " μ2:", round(α2_c, 4),
      " β:", round(β_c, 4), " σ²_w:", round(σ2w_c, 6),
      " σ²_v1:", round(σ2v1_c, 6), " σ²_v2:", round(σ2v2_c, 6), "\n")
}

# 9. PLOT TEST‐PERIOD FORECASTS VS OBSERVATIONS (with CI ribbons)
{
  df_test <- tibble(
    date      = sf_daily$date[(n_train + 1):n_total],
    TMIN_obs  = sf_daily$TMIN[(n_train + 1):n_total],
    TMAX_obs  = sf_daily$TMAX[(n_train + 1):n_total],
    TMIN_a    = raw_a_mu[, "TMIN"],
    TMAX_a    = raw_a_mu[, "TMAX"],
    TMIN_b    = raw_b_mu[, "TMIN"],
    TMAX_b    = raw_b_mu[, "TMAX"],
    TMIN_c    = raw_c_mu[, "TMIN"],
    TMAX_c    = raw_c_mu[, "TMAX"],
    TMIN_a_lo = raw_a_lower[, "TMIN"],
    TMIN_a_hi = raw_a_upper[, "TMIN"],
    TMAX_a_lo = raw_a_lower[, "TMAX"],
    TMAX_a_hi = raw_a_upper[, "TMAX"],
    TMIN_b_lo = raw_b_lower[, "TMIN"],
    TMIN_b_hi = raw_b_upper[, "TMIN"],
    TMAX_b_lo = raw_b_lower[, "TMAX"],
    TMAX_b_hi = raw_b_upper[, "TMAX"],
    TMIN_c_lo = raw_c_lower[, "TMIN"],
    TMIN_c_hi = raw_c_upper[, "TMIN"],
    TMAX_c_lo = raw_c_lower[, "TMAX"],
    TMAX_c_hi = raw_c_upper[, "TMAX"]
  )
  {
    common_theme <- theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor    = element_blank(),
        panel.background    = element_blank(),
        plot.title          = element_text(face = "bold", size = 12, hjust = 0.5),
        axis.title.x        = element_blank(),
        axis.title.y        = element_text(size = 10),
        axis.text.x         = element_text(angle = 45, hjust = 1),
        legend.position     = "none"
      )
    
    p1 <- ggplot(df_test, aes(x = date)) +
      geom_ribbon(aes(ymin = TMIN_a_lo, ymax = TMIN_a_hi), fill = "#D55E00", alpha = 0.3) +
      geom_line(aes(y = TMIN_a), color = "#D55E00", size = 0.6) +
      geom_line(aes(y = TMIN_obs), color = "black", size = 0.1) +
      scale_x_date(date_breaks = "6 month", date_labels = "%b '%y") +
      labs(title = "TMIN – Model A", y = "Tmin (°C)") +
      common_theme
    
    p2 <- ggplot(df_test, aes(x = date)) +
      geom_ribbon(aes(ymin = TMIN_b_lo, ymax = TMIN_b_hi), fill = "#0072B2", alpha = 0.3) +
      geom_line(aes(y = TMIN_b), color = "#0072B2", size = 0.6) +
      geom_line(aes(y = TMIN_obs), color = "black", size = 0.1) +
      scale_x_date(date_breaks = "6 month", date_labels = "%b '%y") +
      labs(title = "TMIN – Model B", y = "Tmin (°C)") +
      common_theme
    
    p3 <- ggplot(df_test, aes(x = date)) +
      geom_ribbon(aes(ymin = TMIN_c_lo, ymax = TMIN_c_hi), fill = "#009E73", alpha = 0.3) +
      geom_line(aes(y = TMIN_c), color = "#009E73", size = 0.6) +
      geom_line(aes(y = TMIN_obs), color = "black", size = 0.1) +
      scale_x_date(date_breaks = "6 month", date_labels = "%b '%y") +
      labs(title = "TMIN – Model C", y = "Tmin (°C)") +
      common_theme
    
    p4 <- ggplot(df_test, aes(x = date)) +
      geom_ribbon(aes(ymin = TMAX_a_lo, ymax = TMAX_a_hi), fill = "#D55E00", alpha = 0.3) +
      geom_line(aes(y = TMAX_a), color = "#D55E00", size = 0.6) +
      geom_line(aes(y = TMAX_obs), color = "black", size = 0.1) +
      scale_x_date(date_breaks = "6 month", date_labels = "%b '%y") +
      labs(title = "TMAX – Model A", y = "Tmax (°C)") +
      common_theme
    
    p5 <- ggplot(df_test, aes(x = date)) +
      geom_ribbon(aes(ymin = TMAX_b_lo, ymax = TMAX_b_hi), fill = "#0072B2", alpha = 0.3) +
      geom_line(aes(y = TMAX_b), color = "#0072B2", size = 0.6) +
      geom_line(aes(y = TMAX_obs), color = "black", size = 0.1) +
      scale_x_date(date_breaks = "6 month", date_labels = "%b '%y") +
      labs(title = "TMAX – Model B", y = "Tmax (°C)") +
      common_theme
    
    p6 <- ggplot(df_test, aes(x = date)) +
      geom_ribbon(aes(ymin = TMAX_c_lo, ymax = TMAX_c_hi), fill = "#009E73", alpha = 0.3) +
      geom_line(aes(y = TMAX_c), color = "#009E73", size = 0.6) +
      geom_line(aes(y = TMAX_obs), color = "black", size = 0.1) +
      scale_x_date(date_breaks = "6 month", date_labels = "%b '%y") +
      labs(title = "TMAX – Model C", y = "Tmax (°C)") +
      common_theme
    
    (p1 | p4) /
      (p2 | p5) /
      (p3 | p6) +
      plot_annotation(
        caption = "Shaded area = 95% CI, colored = forecast, black = observed",
        theme   = theme(plot.caption = element_text(size = 9, hjust = 0.5, face = "italic"))
      )}
}







###########################################################################################
# TASK 3: DLM Forecasting for San Francisco Downtown vs 2: add back seasonal component
###########################################################################################


# 1.1 Subset training data
train_df <- sf_daily[1:n_train, ] %>%
  select(date, TMIN, TMAX) %>%
  mutate(
    doy = yday(date)
  )

# 1.2 Run STL separately on TMIN_train and TMAX_train
ts_tmin_train <- ts(train_df$TMIN, frequency = 365)
stl_tmin     <- stl(ts_tmin_train, s.window = 3, robust = TRUE)

ts_tmax_train <- ts(train_df$TMAX, frequency = 365)
stl_tmax     <- stl(ts_tmax_train, s.window = 3, robust = TRUE)

# 1.3 Attach seasonal components back
train_df <- train_df %>%
  mutate(
    seasonal_tmin = as.numeric(stl_tmin$time.series[, "seasonal"]),
    seasonal_tmax = as.numeric(stl_tmax$time.series[, "seasonal"])
  )

# 1.4 Build season_lookup for each DOY
season_lookup <- train_df %>%
  group_by(doy) %>%
  summarize(
    Smin_mean = mean(seasonal_tmin, na.rm = TRUE),
    Smax_mean = mean(seasonal_tmax, na.rm = TRUE)
  ) %>%
  ungroup()


# 2. SPLIT & FORM DATA MATRICES
{
  Y_daily <- sf_daily %>%
    select(Xmin, Xmax) %>%
    as.matrix()
  n_total <- nrow(Y_daily)
  n_train <- floor(0.95 * n_total)
  
  Y_train <- Y_daily[1:n_train, ]
  Y_test  <- Y_daily[(n_train + 1):n_total, ]
  
  # ──  FIX #1: define dates_test and doy_test so lengths match Y_test ──
  dates_test <- sf_daily$date[(n_train + 1):n_total]
  doy_test   <- sf_daily$doy[(n_train + 1):n_total]
  H1         <- length(dates_test)   # = n_total - n_train

}



# 5.7 Reconstruct raw‐scale forecasts and CI using TRAINING‐ONLY STL seasonality
{

  raw_a_mu    <- matrix(NA_real_, nrow = H1, ncol = 2)
  raw_b_mu    <- matrix(NA_real_, nrow = H1, ncol = 2)
  raw_c_mu    <- matrix(NA_real_, nrow = H1, ncol = 2)
  raw_a_lower <- matrix(NA_real_, nrow = H1, ncol = 2)
  raw_a_upper <- matrix(NA_real_, nrow = H1, ncol = 2)
  raw_b_lower <- matrix(NA_real_, nrow = H1, ncol = 2)
  raw_b_upper <- matrix(NA_real_, nrow = H1, ncol = 2)
  raw_c_lower <- matrix(NA_real_, nrow = H1, ncol = 2)
  raw_c_upper <- matrix(NA_real_, nrow = H1, ncol = 2)
  
  for (i in seq_len(H1)) {
    d   <- doy_test[i]
    smi <- season_lookup$Smin_mean[season_lookup$doy == d]
    sma <- season_lookup$Smax_mean[season_lookup$doy == d]
    
    # Model A (deseasonalized forecast = fore_a_mu[i, ])
    raw_a_mu[i, 1]    <- fore_a_mu[i, 1] + smi
    raw_a_mu[i, 2]    <- fore_a_mu[i, 2] + sma
    raw_a_lower[i, 1] <- fore_a_lower[i, 1] + smi
    raw_a_lower[i, 2] <- fore_a_lower[i, 2] + sma
    raw_a_upper[i, 1] <- fore_a_upper[i, 1] + smi
    raw_a_upper[i, 2] <- fore_a_upper[i, 2] + sma
    
    # Model B (deseasonalized forecast = fore_b_mu[i, ])
    raw_b_mu[i, 1]    <- fore_b_mu[i, 1] + smi
    raw_b_mu[i, 2]    <- fore_b_mu[i, 2] + sma
    raw_b_lower[i, 1] <- fore_b_lower[i, 1] + smi
    raw_b_lower[i, 2] <- fore_b_lower[i, 2] + sma
    raw_b_upper[i, 1] <- fore_b_upper[i, 1] + smi
    raw_b_upper[i, 2] <- fore_b_upper[i, 2] + sma
    
    # Model C (deseasonalized forecast = fore_c_mu[i, ])
    raw_c_mu[i, 1]    <- fore_c_mu[i, 1] + smi
    raw_c_mu[i, 2]    <- fore_c_mu[i, 2] + sma
    raw_c_lower[i, 1] <- fore_c_lower[i, 1] + smi
    raw_c_lower[i, 2] <- fore_c_lower[i, 2] + sma
    raw_c_upper[i, 1] <- fore_c_upper[i, 1] + smi
    raw_c_upper[i, 2] <- fore_c_upper[i, 2] + sma
  }
  
  colnames(raw_a_mu)    <- c("TMIN", "TMAX")
  colnames(raw_b_mu)    <- c("TMIN", "TMAX")
  colnames(raw_c_mu)    <- c("TMIN", "TMAX")
  colnames(raw_a_lower) <- c("TMIN", "TMAX")
  colnames(raw_a_upper) <- c("TMIN", "TMAX")
  colnames(raw_b_lower) <- c("TMIN", "TMAX")
  colnames(raw_b_upper) <- c("TMIN", "TMAX")
  colnames(raw_c_lower) <- c("TMIN", "TMAX")
  colnames(raw_c_upper) <- c("TMIN", "TMAX")
}

# 6. EVALUATION: RMSE & MAE ON DESEASONALIZED
{
  err_a   <- Y_test - fore_a_mu
  err_b   <- Y_test - fore_b_mu
  err_c   <- Y_test - fore_c_mu
  rmse_a  <- sqrt(colMeans(err_a^2, na.rm = TRUE))
  mae_a   <- colMeans(abs(err_a), na.rm = TRUE)
  rmse_b  <- sqrt(colMeans(err_b^2, na.rm = TRUE))
  mae_b   <- colMeans(abs(err_b), na.rm = TRUE)
  rmse_c  <- sqrt(colMeans(err_c^2, na.rm = TRUE))
  mae_c   <- colMeans(abs(err_c), na.rm = TRUE)
  cat("Model (a) Deseasonalized – RMSE:", round(rmse_a, 4),
      " MAE:", round(mae_a, 4), "\n")
  cat("Model (b) Deseasonalized – RMSE:", round(rmse_b, 4),
      " MAE:", round(mae_b, 4), "\n")
  cat("Model (c) Deseasonalized – RMSE:", round(rmse_c, 4),
      " MAE:", round(mae_c, 4), "\n")
}

# 7. RECONSTRUCT RAW DAILY FORECASTS & EVALUATE
{
  TMIN_test <- sf_daily$TMIN[(n_train + 1):n_total]
  TMAX_test <- sf_daily$TMAX[(n_train + 1):n_total]
  Yraw_test <- cbind(TMIN_test, TMAX_test)
  
  err_raw_a  <- Yraw_test - raw_a_mu
  err_raw_b  <- Yraw_test - raw_b_mu
  err_raw_c  <- Yraw_test - raw_c_mu
  rmse_raw_a <- sqrt(colMeans(err_raw_a^2, na.rm = TRUE))
  mae_raw_a  <- colMeans(abs(err_raw_a), na.rm = TRUE)
  rmse_raw_b <- sqrt(colMeans(err_raw_b^2, na.rm = TRUE))
  mae_raw_b  <- colMeans(abs(err_raw_b), na.rm = TRUE)
  rmse_raw_c <- sqrt(colMeans(err_raw_c^2, na.rm = TRUE))
  mae_raw_c  <- colMeans(abs(err_raw_c), na.rm = TRUE)
  
  cat("Model (a) Raw – RMSE:", round(rmse_raw_a, 4),
      " MAE:", round(mae_raw_a, 4), "\n")
  cat("Model (b) Raw – RMSE:", round(rmse_raw_b, 4),
      " MAE:", round(mae_raw_b, 4), "\n")
  cat("Model (c) Raw – RMSE:", round(rmse_raw_c, 4),
      " MAE:", round(mae_raw_c, 4), "\n")
}

# 8. EXTRACT & PRINT PARAMETER ESTIMATES
{
  W_a   <- mod_a$W;    V_a   <- mod_a$V
  σ2w_a <- diag(W_a);  σ2v_a <- diag(V_a)
  cat("Model (a) σ²_w:", round(σ2w_a, 6),
      " σ²_v:", round(σ2v_a, 6), "\n")
  
  W_b      <- mod_b$W; V_b   <- mod_b$V
  σ2w_b11  <- W_b[1, 1]; σ2w_b22 <- W_b[2, 2]
  ρw_b     <- W_b[1, 2] / sqrt(σ2w_b11 * σ2w_b22)
  σ2v_b    <- diag(V_b)
  cat("Model (b) σ²_w11:", round(σ2w_b11, 6),
      "σ²_w22:", round(σ2w_b22, 6),
      " ρ_w:", round(ρw_b, 4),
      " σ²_v:", round(σ2v_b, 6), "\n")
  
  α1_c    <- par_c[1]; α2_c    <- par_c[2]; β_c     <- par_c[3]
  σ2w_c   <- exp(par_c[4]); σ2v1_c  <- exp(par_c[5]); σ2v2_c  <- exp(par_c[6])
  cat("Model (c) μ1:", round(α1_c, 4),
      " μ2:", round(α2_c, 4),
      " β:", round(β_c, 4),
      " σ²_w:", round(σ2w_c, 6),
      " σ²_v1:", round(σ2v1_c, 6),
      " σ²_v2:", round(σ2v2_c, 6), "\n")
}

# 9. PLOT TEST‐PERIOD FORECASTS VS OBSERVATIONS (with CI ribbons)
{
  df_test <- tibble(
    date      = dates_test,
    TMIN_obs  = sf_daily$TMIN[(n_train + 1):n_total],
    TMAX_obs  = sf_daily$TMAX[(n_train + 1):n_total],
    TMIN_a    = raw_a_mu[, "TMIN"],
    TMAX_a    = raw_a_mu[, "TMAX"],
    TMIN_b    = raw_b_mu[, "TMIN"],
    TMAX_b    = raw_b_mu[, "TMAX"],
    TMIN_c    = raw_c_mu[, "TMIN"],
    TMAX_c    = raw_c_mu[, "TMAX"],
    TMIN_a_lo = raw_a_lower[, "TMIN"],
    TMIN_a_hi = raw_a_upper[, "TMIN"],
    TMAX_a_lo = raw_a_lower[, "TMAX"],
    TMAX_a_hi = raw_a_upper[, "TMAX"],
    TMIN_b_lo = raw_b_lower[, "TMIN"],
    TMIN_b_hi = raw_b_upper[, "TMIN"],
    TMAX_b_lo = raw_b_lower[, "TMAX"],
    TMAX_b_hi = raw_b_upper[, "TMAX"],
    TMIN_c_lo = raw_c_lower[, "TMIN"],
    TMIN_c_hi = raw_c_upper[, "TMIN"],
    TMAX_c_lo = raw_c_lower[, "TMAX"],
    TMAX_c_hi = raw_c_upper[, "TMAX"]
  )
  {
    common_theme <- theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor    = element_blank(),
        panel.background    = element_blank(),
        plot.title          = element_text(face = "bold", size = 12, hjust = 0.5),
        axis.title.x        = element_blank(),
        axis.title.y        = element_text(size = 10),
        axis.text.x         = element_text(angle = 45, hjust = 1),
        legend.position     = "none"
      )
    
    p1 <- ggplot(df_test, aes(x = date)) +
      geom_ribbon(aes(ymin = TMIN_a_lo, ymax = TMIN_a_hi),
                  fill = "#D55E00", alpha = 0.3) +
      geom_line(aes(y = TMIN_a), color = "#D55E00", size = 0.6) +
      geom_line(aes(y = TMIN_obs), color = "black", size = 0.1) +
      scale_x_date(date_breaks = "6 month", date_labels = "%b '%y") +
      labs(title = "TMIN – Model A", y = "Tmin (°C)") +
      common_theme
    
    p2 <- ggplot(df_test, aes(x = date)) +
      geom_ribbon(aes(ymin = TMIN_b_lo, ymax = TMIN_b_hi),
                  fill = "#0072B2", alpha = 0.3) +
      geom_line(aes(y = TMIN_b), color = "#0072B2", size = 0.6) +
      geom_line(aes(y = TMIN_obs), color = "black", size = 0.1) +
      scale_x_date(date_breaks = "6 month", date_labels = "%b '%y") +
      labs(title = "TMIN – Model B", y = "Tmin (°C)") +
      common_theme
    
    p3 <- ggplot(df_test, aes(x = date)) +
      geom_ribbon(aes(ymin = TMIN_c_lo, ymax = TMIN_c_hi),
                  fill = "#009E73", alpha = 0.3) +
      geom_line(aes(y = TMIN_c), color = "#009E73", size = 0.6) +
      geom_line(aes(y = TMIN_obs), color = "black", size = 0.1) +
      scale_x_date(date_breaks = "6 month", date_labels = "%b '%y") +
      labs(title = "TMIN – Model C", y = "Tmin (°C)") +
      common_theme
    
    p4 <- ggplot(df_test, aes(x = date)) +
      geom_ribbon(aes(ymin = TMAX_a_lo, ymax = TMAX_a_hi),
                  fill = "#D55E00", alpha = 0.3) +
      geom_line(aes(y = TMAX_a), color = "#D55E00", size = 0.6) +
      geom_line(aes(y = TMAX_obs), color = "black", size = 0.1) +
      scale_x_date(date_breaks = "6 month", date_labels = "%b '%y") +
      labs(title = "TMAX – Model A", y = "Tmax (°C)") +
      common_theme
    
    p5 <- ggplot(df_test, aes(x = date)) +
      geom_ribbon(aes(ymin = TMAX_b_lo, ymax = TMAX_b_hi),
                  fill = "#0072B2", alpha = 0.3) +
      geom_line(aes(y = TMAX_b), color = "#0072B2", size = 0.6) +
      geom_line(aes(y = TMAX_obs), color = "black", size = 0.1) +
      scale_x_date(date_breaks = "6 month", date_labels = "%b '%y") +
      labs(title = "TMAX – Model B", y = "Tmax (°C)") +
      common_theme
    
    p6 <- ggplot(df_test, aes(x = date)) +
      geom_ribbon(aes(ymin = TMAX_c_lo, ymax = TMAX_c_hi),
                  fill = "#009E73", alpha = 0.3) +
      geom_line(aes(y = TMAX_c), color = "#009E73", size = 0.6) +
      geom_line(aes(y = TMAX_obs), color = "black", size = 0.1) +
      scale_x_date(date_breaks = "6 month", date_labels = "%b '%y") +
      labs(title = "TMAX – Model C", y = "Tmax (°C)") +
      common_theme
    
    (p1 | p4) /
      (p2 | p5) /
      (p3 | p6) +
      plot_annotation(
        caption = "Shaded area = 95% CI, colored = forecast, black = observed",
        theme   = theme(plot.caption = element_text(size = 9, hjust = 0.5, face = "italic"))
      )
  }
}






############# TRUE FUTURE FORECAST
# --------------------------------------------------------
# APPENDIX: 4‐MONTH AHEAD FORECASTS WITH SEASON ADD‐BACK & CI
# --------------------------------------------------------

# (This assumes all objects up through ‘season_lookup’, ‘mod_a’, ‘mod_b’, ‘mod_c2’,
#  and ‘filt_a_all’, ‘filt_b_all’, ‘filt_c_all’ already exist from the earlier code.)

# 1. DEFINE FUTURE HORIZON (≈120 days ≃ 4 months)
last_date    <- max(sf_daily$date)
H2           <- 360
future_dates <- seq.Date(from = last_date + 1, by = "day", length.out = H2)
doy_future   <- yday(future_dates)

# 2. FORECAST 120 DAYS AHEAD ON DESEASONALIZED SCALE
#    (use each filtered object directly; dlmForecast will produce
#     out‐of‐sample forecasts of X_t = [Xmin, Xmax] and their covariances)

FC_a <- dlmForecast(filt_a_all, nAhead = H2)
FC_b <- dlmForecast(filt_b_all, nAhead = H2)
FC_c <- dlmForecast(filt_c_all, nAhead = H2)

# Extract the H2×2 matrix of “deseasonalized” forecasts for each model
Xf_a <- FC_a$f    # columns: [Xmin, Xmax]
Xf_b <- FC_b$f
Xf_c <- FC_c$f    # already H2×2, because filt_c_all forecasts observations via FF

# Build lists of predictive covariances for Xmin/Xmax for each horizon i
# dlmForecast returns a list element “Q” in which each Q[[i]] is the 2×2 covariance for (Xmin,Xmax)
Q_a_list <- FC_a$Q
Q_b_list <- FC_b$Q
Q_c_list <- FC_c$Q

# 3. LOOK UP TRAINING‐AVERAGE SEASONAL MEAN FOR EACH FUTURE DOY
Smin_fut <- season_lookup$Smin_mean[match(doy_future, season_lookup$doy)]
Smax_fut <- season_lookup$Smax_mean[match(doy_future, season_lookup$doy)]

# 4. CONSTRUCT RAW‐SCALE FORECASTS & 95% CI
raw_a_mu <- matrix(NA_real_, nrow = H2, ncol = 2)
raw_b_mu <- matrix(NA_real_, nrow = H2, ncol = 2)
raw_c_mu <- matrix(NA_real_, nrow = H2, ncol = 2)

raw_a_lo <- matrix(NA_real_, nrow = H2, ncol = 2)
raw_a_hi <- matrix(NA_real_, nrow = H2, ncol = 2)
raw_b_lo <- matrix(NA_real_, nrow = H2, ncol = 2)
raw_b_hi <- matrix(NA_real_, nrow = H2, ncol = 2)
raw_c_lo <- matrix(NA_real_, nrow = H2, ncol = 2)
raw_c_hi <- matrix(NA_real_, nrow = H2, ncol = 2)

for (i in seq_len(H2)) {
  # Deseasonalized forecasts at horizon i
  xa_a <- Xf_a[i, 1]; xb_a <- Xf_a[i, 2]
  xa_b <- Xf_b[i, 1]; xb_b <- Xf_b[i, 2]
  xa_c <- Xf_c[i, 1]; xb_c <- Xf_c[i, 2]
  
  # Predictive SDs on deseasonalized scale
  sd_a_min <- sqrt(Q_a_list[[i]][1,1]); sd_a_max <- sqrt(Q_a_list[[i]][2,2])
  sd_b_min <- sqrt(Q_b_list[[i]][1,1]); sd_b_max <- sqrt(Q_b_list[[i]][2,2])
  sd_c_min <- sqrt(Q_c_list[[i]][1,1]); sd_c_max <- sqrt(Q_c_list[[i]][2,2])
  
  # Seasonal mean for that day‐of‐year
  smi <- Smin_fut[i]
  sma <- Smax_fut[i]
  
  # Model A raw forecast ± CI
  raw_a_mu[i, 1] <- xa_a + smi
  raw_a_mu[i, 2] <- xb_a + sma
  raw_a_lo[i, 1] <- xa_a - 1.96 * sd_a_min + smi
  raw_a_hi[i, 1] <- xa_a + 1.96 * sd_a_min + smi
  raw_a_lo[i, 2] <- xb_a - 1.96 * sd_a_max + sma
  raw_a_hi[i, 2] <- xb_a + 1.96 * sd_a_max + sma
  
  # Model B raw forecast ± CI
  raw_b_mu[i, 1] <- xa_b + smi
  raw_b_mu[i, 2] <- xb_b + sma
  raw_b_lo[i, 1] <- xa_b - 1.96 * sd_b_min + smi
  raw_b_hi[i, 1] <- xa_b + 1.96 * sd_b_min + smi
  raw_b_lo[i, 2] <- xb_b - 1.96 * sd_b_max + sma
  raw_b_hi[i, 2] <- xb_b + 1.96 * sd_b_max + sma
  
  # Model C raw forecast ± CI
  raw_c_mu[i, 1] <- xa_c + smi
  raw_c_mu[i, 2] <- xb_c + sma
  raw_c_lo[i, 1] <- xa_c - 1.96 * sd_c_min + smi
  raw_c_hi[i, 1] <- xa_c + 1.96 * sd_c_min + smi
  raw_c_lo[i, 2] <- xb_c - 1.96 * sd_c_max + sma
  raw_c_hi[i, 2] <- xb_c + 1.96 * sd_c_max + sma
}

colnames(raw_a_mu) <- colnames(raw_b_mu) <- colnames(raw_c_mu) <- c("TMIN","TMAX")
colnames(raw_a_lo) <- colnames(raw_b_lo) <- colnames(raw_c_lo) <- c("TMIN","TMAX")
colnames(raw_a_hi) <- colnames(raw_b_hi) <- colnames(raw_c_hi) <- c("TMIN","TMAX")

# 5. ASSEMBLE FORECAST DATAFRAME FOR PLOTTING
df_future <- tibble(
  date      = future_dates,
  
  # Model A
  TMIN_a    = raw_a_mu[, "TMIN"],
  TMIN_a_lo = raw_a_lo[, "TMIN"],
  TMIN_a_hi = raw_a_hi[, "TMIN"],
  TMAX_a    = raw_a_mu[, "TMAX"],
  TMAX_a_lo = raw_a_lo[, "TMAX"],
  TMAX_a_hi = raw_a_hi[, "TMAX"],
  
  # Model B
  TMIN_b    = raw_b_mu[, "TMIN"],
  TMIN_b_lo = raw_b_lo[, "TMIN"],
  TMIN_b_hi = raw_b_hi[, "TMIN"],
  TMAX_b    = raw_b_mu[, "TMAX"],
  TMAX_b_lo = raw_b_lo[, "TMAX"],
  TMAX_b_hi = raw_b_hi[, "TMAX"],
  
  # Model C
  TMIN_c    = raw_c_mu[, "TMIN"],
  TMIN_c_lo = raw_c_lo[, "TMIN"],
  TMIN_c_hi = raw_c_hi[, "TMIN"],
  TMAX_c    = raw_c_mu[, "TMAX"],
  TMAX_c_lo = raw_c_lo[, "TMAX"],
  TMAX_c_hi = raw_c_hi[, "TMAX"]
)

# 6. PLOT 6 PANELS: TMIN & TMAX FORECASTS WITH 95% CI, 4 MONTHS HORIZON
{
  common_theme <- theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_line(color = "gray80"),
      panel.grid.minor    = element_blank(),
      panel.background    = element_blank(),
      plot.title          = element_text(face = "bold", size = 12, hjust = 0.5),
      axis.title.x        = element_blank(),
      axis.title.y        = element_text(size = 10),
      axis.text.x         = element_text(angle = 45, hjust = 1),
      legend.position     = "none"
    )
  
  # TMIN – Model A
  pA_min <- ggplot(df_future, aes(x = date)) +
    geom_ribbon(aes(ymin = TMIN_a_lo, ymax = TMIN_a_hi),
                fill = "#D55E00", alpha = 0.3) +
    geom_line(aes(y = TMIN_a), color = "#D55E00", size = 0.6) +
    labs(title = "TMIN – Model A (4-Month Forecast)", y = "Tmin (°C)") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b '%y") +
    common_theme
  
  # TMIN – Model B
  pB_min <- ggplot(df_future, aes(x = date)) +
    geom_ribbon(aes(ymin = TMIN_b_lo, ymax = TMIN_b_hi),
                fill = "#0072B2", alpha = 0.3) +
    geom_line(aes(y = TMIN_b), color = "#0072B2", size = 0.6) +
    labs(title = "TMIN – Model B (4-Month Forecast)", y = "Tmin (°C)") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b '%y") +
    common_theme
  
  # TMIN – Model C
  pC_min <- ggplot(df_future, aes(x = date)) +
    geom_ribbon(aes(ymin = TMIN_c_lo, ymax = TMIN_c_hi),
                fill = "#009E73", alpha = 0.3) +
    geom_line(aes(y = TMIN_c), color = "#009E73", size = 0.6) +
    labs(title = "TMIN – Model C (4-Month Forecast)", y = "Tmin (°C)") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b '%y") +
    common_theme
  
  # TMAX – Model A
  pA_max <- ggplot(df_future, aes(x = date)) +
    geom_ribbon(aes(ymin = TMAX_a_lo, ymax = TMAX_a_hi),
                fill = "#D55E00", alpha = 0.3) +
    geom_line(aes(y = TMAX_a), color = "#D55E00", size = 0.6) +
    labs(title = "TMAX – Model A (4-Month Forecast)", y = "Tmax (°C)") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b '%y") +
    common_theme
  
  # TMAX – Model B
  pB_max <- ggplot(df_future, aes(x = date)) +
    geom_ribbon(aes(ymin = TMAX_b_lo, ymax = TMAX_b_hi),
                fill = "#0072B2", alpha = 0.3) +
    geom_line(aes(y = TMAX_b), color = "#0072B2", size = 0.6) +
    labs(title = "TMAX – Model B (4-Month Forecast)", y = "Tmax (°C)") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b '%y") +
    common_theme
  
  # TMAX – Model C
  pC_max <- ggplot(df_future, aes(x = date)) +
    geom_ribbon(aes(ymin = TMAX_c_lo, ymax = TMAX_c_hi),
                fill = "#009E73", alpha = 0.3) +
    geom_line(aes(y = TMAX_c), color = "#009E73", size = 0.6) +
    labs(title = "TMAX – Model C (4-Month Forecast)", y = "Tmax (°C)") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b '%y") +
    common_theme
  
  # Arrange into 3×2 grid
  (pA_min | pA_max) /
    (pB_min | pB_max) /
    (pC_min | pC_max) +
    plot_annotation(
      caption = "Forecast horizon: 4 months out-of-sample; shaded = 95% CI, solid = forecast",
      theme   = theme(plot.caption = element_text(size = 9, hjust = 0.5, face = "italic"))
    )
}
#we are forecasitng flat and then adding on the seasonal copmoennt









###########################################################################
################ APPENDIX: SPATIAL ANALYSIS deseason ######################
###########################################################################

# 1. LOAD RAW DATA
ghcn_path <- "/Users/Alessandro/Desktop/final project tsa/ghcn.txt"
ghcn <- read_csv(ghcn_path, col_types = cols(
  ID        = col_character(),
  station   = col_character(),
  latitude  = col_double(),
  longitude = col_double(),
  elevation = col_double(),
  date      = col_date(format = "%Y-%m-%d"),
  TMIN      = col_double(),
  TMAX      = col_double(),
  TAVG      = col_double(),
  PRCP      = col_double()
))

# 2. CONVERT TO CELSIUS
ghcn <- ghcn %>%
  mutate(TMIN = TMIN / 10, TMAX = TMAX / 10)

# 3. DESEASONALIZE DAILY TMIN/TMAX (STL PER STATION)
ghcn_deseasonalized <- ghcn %>%
  group_by(station) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    doy = yday(date),
    TMIN_stl = {
      ts_temp <- ts(TMIN, frequency = 365)
      seasonal <- tryCatch(stl(ts_temp, s.window = "periodic")$time.series[, "seasonal"], error = function(e) rep(NA, length(TMIN)))
      TMIN - seasonal
    },
    TMAX_stl = {
      ts_temp <- ts(TMAX, frequency = 365)
      seasonal <- tryCatch(stl(ts_temp, s.window = "periodic")$time.series[, "seasonal"], error = function(e) rep(NA, length(TMAX)))
      TMAX - seasonal
    }
  ) %>%
  ungroup()

# 4. AGGREGATE TO MONTHLY (MEAN DESEASONALIZED TMIN/TMAX)
ghcn_monthly <- ghcn_deseasonalized %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(station, latitude, longitude, month) %>%
  summarise(
    TMIN = mean(TMIN_stl, na.rm = TRUE),
    TMAX = mean(TMAX_stl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(TMIN), !is.na(TMAX))

# 5. RESTRICT TO TIME WINDOW
ghcn_window <- subset(ghcn_monthly, month >= as.Date("2010-01-01"))


# === Convert SpatialPointsDataFrame to Data Frame ===
df_sites <- as.data.frame(ghcn_window)

# === Extract Unique Stations ===
sites_named <- df_sites %>%
  dplyr::select(station, longitude, latitude) %>%
  distinct()


# === Plot Locations of Stations with Labels ===
plot(sites_named[, c("longitude", "latitude")],
     pch = 19, col = 2, ylim = c(34, 49), xlim = c(-125, -65),
     xlab = "Longitude", ylab = "Latitude",
     main = "Location of Stations in GHCN Window")
text(sites_named[, c("longitude", "latitude")],
     labels = sites_named$station, pos = 1, cex = 0.5)

# === Build TMIN and TMAX matrices ===
data_df <- as.data.frame(ghcn_window)
ozone_tmin <- data_df %>%
  dplyr::select(station, month, TMIN) %>%
  pivot_wider(names_from = station, values_from = TMIN)

ozone_tmax <- data_df %>%
  dplyr::select(station, month, TMAX) %>%
  pivot_wider(names_from = station, values_from = TMAX)

tmin_matrix <- as.matrix(ozone_tmin[, -1])
tmax_matrix <- as.matrix(ozone_tmax[, -1])

tmin_matrix_t <- t(tmin_matrix)
tmax_matrix_t <- t(tmax_matrix)

month_labels <- format(ozone_tmin$month, "%Y-%m")

# 1. Choose a palette (example: diverging blue-white-red)

library(RColorBrewer)
pal <- plasma(256)
# panel-specific ranges
rng1 <- range(tmin_matrix_t, na.rm = TRUE)
rng2 <- range(tmax_matrix_t, na.rm = TRUE)



################ TMIN and TMAX correlation over time
#–– TMIN heatmap (no legend) ––
par(mar = c(5, 10, 4, 1))
image(
  tmin_matrix_t,
  col       = pal,
  zlim      = rng1,
  xaxt      = "n",
  yaxt      = "n",
  useRaster = TRUE,
  main      = "TMIN Over Time"
)
axis(
  1,
  at     = seq(0, 1, len = 12),
  labels = format(
    ozone_tmin$month[seq(1, nrow(ozone_tmin), len = 12)],
    "%Y-%m"
  ),
  las = 2
)
axis(
  2,
  at        = seq(0, 1, len = ncol(tmin_matrix)),
  labels    = colnames(tmin_matrix),
  las       = 2,
  cex.axis  = 0.6
)

#–– TMAX heatmap (no legend) ––
par(mar = c(5, 4, 4, 1))
image(
  tmax_matrix_t,
  col       = pal,
  zlim      = rng2,
  xaxt      = "n",
  yaxt      = "n",
  useRaster = TRUE,
  main      = "TMAX Over Time"
)
axis(
  1,
  at     = seq(0, 1, len = 12),
  labels = format(
    ozone_tmax$month[seq(1, nrow(ozone_tmax), len = 12)],
    "%Y-%m"
  ),
  las = 2
)


# === Spaghetti Plot All Sites ===
par(mfrow = c(2, 1), mar = c(2, 5, 4, 2), oma = c(1, 0, 0, 0))

matplot(ozone_tmin$month, tmin_matrix, type = "l", lty = 1, col = rgb(0, 0, 1, 0.2),
        ylab = "TMIN", main = "TMIN Time Series for All Sites")
lines(ozone_tmin$month, rowMeans(tmin_matrix, na.rm = TRUE), col = "red", lwd = 2)

matplot(ozone_tmax$month, tmax_matrix, type = "l", lty = 1, col = rgb(1, 0, 0, 0.2),
        ylab = "TMAX", main = "TMAX Time Series for All Sites")
lines(ozone_tmax$month, rowMeans(tmax_matrix, na.rm = TRUE), col = "blue", lwd = 2)


# === Correlation Matrix Heatmaps ===
library(RColorBrewer)


# Layout and margins# Compute correlation matrices
tmin_cor <- cor(tmin_matrix, use = "pairwise.complete.obs")
tmax_cor <- cor(tmax_matrix, use = "pairwise.complete.obs")



# STATION HEATMAP
library(fields)

wrapped_tmin <- sapply(colnames(tmin_cor),
                       function(x) paste(strwrap(x, width = 10), collapse="\n"))
wrapped_tmax <- sapply(colnames(tmax_cor),
                       function(x) paste(strwrap(x, width = 10), collapse="\n"))

par(mfrow = c(1, 2), mar = c(5, 5, 4, 6)) 
# right margin widened for legend

n_tmin <- ncol(tmin_cor)
image.plot(
  1:n_tmin, 1:n_tmin,
  tmin_cor[, n_tmin:1],
  col         = diverging_palette,
  xaxt        = "n",
  yaxt        = "n",
  xlab        = "",
  ylab        = "",
  main        = "TMIN Correlation",
  legend.width= 1.2,
  legend.mar  = 4
)
axis(
  1, at = 1:n_tmin,
  labels   = wrapped_tmin,
  las      = 2,
  cex.axis = 0.9,
  tick     = FALSE
)
axis(
  2, at = 1:n_tmin,
  labels   = rev(wrapped_tmin),
  las      = 2,
  cex.axis = 0.9,
  tick     = FALSE
)
box()
n_tmax <- ncol(tmax_cor)
image.plot(
  1:n_tmax, 1:n_tmax,
  tmax_cor[, n_tmax:1],
  col         = diverging_palette,
  xaxt        = "n",
  yaxt        = "n",
  xlab        = "",
  ylab        = "",
  main        = "TMAX Correlation",
  legend.width= 1.2,
  legend.mar  = 4
)
axis(
  1, at = 1:n_tmax,
  labels   = wrapped_tmax,
  las      = 2,
  cex.axis = 0.9,
  tick     = FALSE
)
axis(
  2, at = 1:n_tmax,
  labels   = rev(wrapped_tmax),
  las      = 2,
  cex.axis = 0.9,
  tick     = FALSE
)
box()




# ─────────────────────────────────────────────
# Load/install required packages (non‐interactive)
# ─────────────────────────────────────────────
pkgs <- c(
  "lubridate", "dplyr", "tidyr", "INLA", "sp", "spdep",
  "tmap", "sf", "rnaturalearth", "rnaturalearthdata", "ggplot2"
)
installed <- pkgs %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(pkgs[!installed], repos = "https://cloud.r-project.org")
}
lapply(pkgs, library, character.only = TRUE)

# ─────────────────────────────────────────────
# 0. Subset daily GHCN to 2010–2025 window
# ─────────────────────────────────────────────
ghcn_window <- ghcn %>%
  filter(
    date >= as.Date("2010-01-01")
  )

# ─────────────────────────────────────────────
# 1. Convert SpatialPointsDataFrame to data frame
# ─────────────────────────────────────────────
df_sites <- as.data.frame(ghcn_window)

# Extract unique station coordinates
sites_named <- df_sites %>%
  select(station, longitude, latitude) %>%
  distinct()

# Plot station locations
plot(
  sites_named$longitude, sites_named$latitude,
  pch = 19, col = 2, ylim = c(34, 49), xlim = c(-125, -65),
  xlab = "Longitude", ylab = "Latitude",
  main = "Location of Stations in GHCN Window"
)
text(
  sites_named$longitude,
  sites_named$latitude,
  labels = sites_named$station,
  pos = 1, cex = 0.5
)

# ─────────────────────────────────────────────
# 2. Build TMIN and TMAX matrices (daily)
# ─────────────────────────────────────────────
data_df <- ghcn_window

ozone_tmin <- data_df %>%
  select(station, date, TMIN) %>%
  pivot_wider(names_from = station, values_from = TMIN)

ozone_tmax <- data_df %>%
  select(station, date, TMAX) %>%
  pivot_wider(names_from = station, values_from = TMAX)

tmin_matrix <- as.matrix(ozone_tmin[, -1])
tmax_matrix <- as.matrix(ozone_tmax[, -1])
tmin_matrix_t <- t(tmin_matrix)
tmax_matrix_t <- t(tmax_matrix)
date_labels <- format(ozone_tmin$date, "%Y-%m")

# ─────────────────────────────────────────────
# 7. Prepare ghcn_window (daily) for modeling
#    - Compute id.date (temporal index)
# ─────────────────────────────────────────────
data_df <- ghcn_window %>%
  mutate(
    date    = as.Date(date),
    id.date = as.numeric(factor(date, levels = sort(unique(date))))
  ) %>%
  filter(station %in% ghcn_window$station) %>%
  as_tibble()

# Step 0: Force data frame in case of SpatialPointsDataFrame issues
data_df <- as.data.frame(ghcn_window)

# Load required packages
library(dplyr)
library(tidyr)
library(sp)

# === Step 1: Create TMIN matrix with printout ===
cat("\n===== Creating mat_tmin =====\n")
mat_tmin <- data_df %>%
  dplyr::select(date = date, station, TMIN) %>%
  pivot_wider(names_from = station, values_from = TMIN) %>%
  arrange(date)

print(head(mat_tmin, 3))  # Show first few rows
cat("\nDimensions of mat_tmin: "); print(dim(mat_tmin))

# Store matrix only (drop date)
Y_tmin <- as.matrix(mat_tmin[, -1])
cat("\nDimensions of Y_tmin: "); print(dim(Y_tmin))

# === Step 2: Coordinates matrix aligned with Y_tmin columns ===
cat("\n===== Creating S_coords =====\n")
coords <- data_df %>%
  dplyr::select(station, longitude, latitude) %>%
  distinct() %>%
  arrange(match(station, colnames(Y_tmin)))  # ensure same station order

print(coords)  # show full station list
S_coords <- as.matrix(coords[, c("longitude", "latitude")])
cat("\nDimensions of S_coords: "); print(dim(S_coords))

# === Step 3: Convert to SpatialPoints ===
cat("\n===== Creating SpatialPoints =====\n")
stations_sp <- SpatialPoints(S_coords, proj4string = CRS("+proj=longlat +datum=WGS84"))
print(stations_sp)

# === Step 4: Build 4-nearest neighbor spatial adjacency ===
library(spdep)

cat("\n===== Computing 4-Nearest Neighbor Adjacency =====\n")
nb_knn <- knn2nb(knearneigh(stations_sp, k = 4))

cat("\nList of neighbors per station:\n")
print(nb_knn)

# Convert to binary adjacency matrix
adj_matrix <- nb2mat(nb_knn, style = "B", zero.policy = TRUE)
cat("\n===== Adjacency Matrix (13 × 13) =====\n")
print(adj_matrix)


# ─────────────────────────────────────────────
# 8. Create coordinates matrix and station_sf_base
# ─────────────────────────────────────────────
coords <- data_df %>%
  select(station, longitude, latitude) %>%
  distinct() %>%
  arrange(match(station, colnames(Y_tmin)))
S_coords <- as.matrix(coords[, c("longitude", "latitude")])

station_sf_base <- coords %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# ─────────────────────────────────────────────
# 9. Load contiguous U.S. polygon
# ─────────────────────────────────────────────
us_states <- ne_states(
  country     = "United States of America",
  returnclass = "sf"
)
us_mainland <- us_states %>%
  filter(!name %in% c("Alaska", "Hawaii", "Puerto Rico"))





# ─────────────────────────────────────────────
# 10. Create SpatialPoints and adjacency
# ─────────────────────────────────────────────
stations_sp <- SpatialPoints(
  S_coords,
  proj4string = CRS("+proj=longlat +datum=WGS84")
)
rownames(stations_sp@coords) <- colnames(Y_tmin)

dist_matrix <- spDists(stations_sp, longlat = TRUE)
threshold <- 100
station.adj.mat <- (dist_matrix < threshold) | (t(dist_matrix) < threshold)
station.adj.mat[is.na(station.adj.mat)] <- 0
stopifnot(isSymmetric(station.adj.mat))

# ─────────────────────────────────────────────
# 11. Add indices to data.kh (match daily)
# ─────────────────────────────────────────────


# Compute id.date (temporal index) on data.kh directly,
# instead of joining from data_df
data.kh <- data_df %>%
  # Ensure date is Date class
  mutate(date = as.Date(date)) %>%
  # Temporal index: 1…#unique dates in data.kh
  mutate(id.date = as.numeric(factor(date, levels = sort(unique(date))))) %>%
  # id.dateu = id.date for IID term
  mutate(id.dateu = id.date) %>%
  # Spatial index: 1…#unique stations in data.kh
  mutate(
    station    = as.character(station),
    id.zone    = as.numeric(factor(station, levels = unique(station))),
    id.zone.int = id.zone
  ) %>%
  # Spatial‐temporal interaction index: unique per (zone, date)
  mutate(id.zoneu.dateu = as.numeric(interaction(id.zone.int, id.date, drop = TRUE)))

# Verify no gaps
stopifnot(
  min(data.kh$id.zone.int) == 1,
  max(data.kh$id.zone.int) == length(unique(data.kh$station)),
  min(data.kh$id.date)     == 1,
  max(data.kh$id.date)     == length(unique(data.kh$date))
)


# ─────────────────────────────────────────────
# Create all temporal and interaction indices needed
# ─────────────────────────────────────────────
data.kh <- data.kh %>%
  mutate(
    id.month       = as.numeric(format(date, "%Y%m")),
    id.monthu      = id.month,
    id.month.int   = id.month,
    id.zoneu.monthu = as.numeric(interaction(station, id.month, drop = TRUE))
  )



# ─────────────────────────────────────────────
# 13. Define KH model formulas
# ─────────────────────────────────────────────

control.inla = list(strategy = "adaptive", int.strategy = "eb")
control.mode = list(restart = TRUE)

data.kh$id.zone.int <- as.integer(factor(data.kh$id.zone))
data.kh$id.month.int <- as.integer(factor(data.kh$id.month))


formulas <- list(
  #Unstructured space × Unstructured time
  KH1 = TMIN ~ 1 +
    f(id.zone, model = "bym", graph = station.adj.mat) +
    f(id.month, model = "rw1") +
    f(id.monthu, model = "iid") +
    f(id.zoneu.monthu, model = "iid"),
  
  KH2 = TMIN ~ 1 +
    #Structured space (Besag) × Unstructured time
    f(id.zone, model = "bym", graph = station.adj.mat) +
    f(id.month, model = "rw1") +
    f(id.monthu, model = "iid") +
    f(id.month.int, model = "iid", group = id.zone.int,
      control.group = list(model = "besag", graph = station.adj.mat)),
  
  KH3 = TMIN ~ 1 +
    #Unstructured space × Structured time (RW1)
    f(id.zone, model = "bym", graph = station.adj.mat) +
    f(id.month, model = "rw1") +
    f(id.monthu, model = "iid") +
    f(
      id.zone.int,
      model = "iid",
      group = id.month.int,
      control.group = list(model = "rw1")),
  
  
  KH4 = TMIN ~ 1 +
    #Structured space (Besag) × Structured time (RW1)
    f(id.zone, model = "bym", graph = station.adj.mat) +
    f(id.month, model = "rw1") +
    f(id.monthu, model = "iid") +
    f(id.zone.int, model = "besag", graph = station.adj.mat,
      group = id.month.int,
      control.group = list(model = "rw1"))
)


# ─────────────────────────────────────────────
# 14. Fit all models
# ─────────────────────────────────────────────

run_model <- function(formula, name) {
  cat(paste0("\n[3] Running model: ", name, "\n"))
  tryCatch({
    fit <- inla(
      formula,
      data = data.kh,
      family = "gaussian",
      control.predictor = list(compute = TRUE),
      control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
    )
    cat("✔️ ", name, " — DIC: ", round(fit$dic$dic, 2),
        " | WAIC: ", round(fit$waic$waic, 2), "\n")
    return(fit)
  }, error = function(e) {
    cat("❌ ", name, " failed: ", conditionMessage(e), "\n")
    return(NULL)
  })
}

fits <- lapply(names(formulas), function(name) run_model(formulas[[name]], name))
names(fits) <- names(formulas)



# ─────────────────────────────────────────────
# 15. Compare model selection (DIC + WAIC)
# ─────────────────────────────────────────────

model_scores <- data.frame(
  Model = names(fits),
  DIC = sapply(fits, function(f) f$dic$dic),
  WAIC = sapply(fits, function(f) f$waic$waic)
)
print(model_scores)




# ─────────────────────────────────────────────
# 16. Summarize INLA results
# ─────────────────────────────────────────────
fit_KH4_daily <- fits[["KH4"]]


print(round(fit_KH4_daily$summary.fixed[, c("mean", "sd")], 3))
print(round(fit_KH4_daily$summary.hyperpar[, c("mean", "sd")], 3))

# ─────────────────────────────────────────────
# 17. Compute residuals per station
# ─────────────────────────────────────────────
df_pred <- data.frame(
  station = data.kh$station,
  obs     = data.kh$TMIN,
  fit     = fit_KH4_daily$summary.fitted.values$mean
)

residuals_daily <- df_pred %>%
  mutate(res = obs - fit) %>%
  group_by(station) %>%
  summarise(res = mean(res, na.rm = TRUE))

# ─────────────────────────────────────────────
# 18. Merge residuals into station_sf and plot
# ─────────────────────────────────────────────
station_sf <- station_sf_base %>%
  left_join(residuals_daily, by = "station")

plot.spatial <- tm_shape(us_mainland) +
  tm_polygons(
    col        = "grey95",
    border.col = "gray50",
    lwd        = 0.5
  ) +
  tm_shape(station_sf) +
  tm_symbols(
    col      = "res",
    palette  = "RdBu",
    style    = "quantile",
    midpoint = 0,
    size     = 0.8,
    shape    = 16
  ) +
  tm_text(
    text     = "station",
    size     = 0.5,
    col      = "black",
    bg.color = "white",
    bg.alpha = 0.7,
    ymod     = 0.2
  ) +
  tm_title("Stations on U.S. Map (Daily Data, Colored by Residual)") +
  tm_layout(
    legend.outside   = TRUE,
    legend.position  = c("right", "bottom"),
    frame            = FALSE,
    inner.margins    = c(0.05, 0.05, 0.05, 0.15)
  )

tmap_mode("plot")
print(plot.spatial)


# ─────────────────────────────────────────────
# 19a. Forecasting block for daily data
# ─────────────────────────────────────────────
all_dates  <- sort(unique(data.kh$date))
test_dates <- tail(all_dates, 360)
train_dates<- setdiff(all_dates, test_dates)

data_train <- data.kh %>% filter(date %in% train_dates)
data_test  <- data.kh %>% filter(date %in% test_dates)

n_train_days <- length(unique(data_train$id.date))
test_seq     <- seq_len(length(test_dates))
max_zoneud   <- max(data_train$id.zoneu.dateu)

data_test_forecast <- data_test %>%
  mutate(
    id.date        = n_train_days + test_seq[match(date, test_dates)],
    id.dateu       = id.date,
    id.zoneu.dateu = max_zoneud + seq_len(n())
  )

data_test_forecast$TMIN <- NA
data_joint <- bind_rows(data_train, data_test_forecast)

formula_KH4_forecast <- TMIN ~ 1 +
  f(
    id.zone.int, model = "besag", graph = station.adj.mat,
    group = id.date, control.group = list(model = "rw1")
  ) +
  f(id.date,   model = "rw1") +
  f(id.dateu,  model = "iid") +
  f(id.zone,   model = "iid")

fit.kh4.forecast <- inla(
  formula_KH4_forecast,
  data             = data_joint,
  family           = "gaussian",
  control.predictor = list(compute = TRUE, link = 1),
  control.compute   = list(dic = TRUE, waic = TRUE)
)

n_train <- nrow(data_train)
n_total <- nrow(data_joint)

pred_values <- fit.kh4.forecast$summary.fitted.values[(n_train + 1):n_total, ]

forecast_df <- data_test_forecast %>%
  mutate(
    forecast = pred_values$mean,
    lower    = pred_values$`0.025quant`,
    upper    = pred_values$`0.975quant`
  )

selected_zone <- unique(forecast_df$id.zone.int)[1]

print(
  ggplot(
    forecast_df %>% filter(id.zone.int == selected_zone),
    aes(x = date, y = forecast)
  ) +
    geom_line(color = "blue") +

    geom_line(
      data = data_test %>% filter(id.zone.int == selected_zone),
      aes(x = date, y = TMIN),
      color = "black"
    ) +
    labs(
      title = paste0("KH4 Forecast (24 days) for Station ", selected_zone),
      x     = "Date", y = "TMIN"
    ) +
    theme_minimal()
)

# ─────────────────────────────────────────────
# 19b Forecasting block for daily data (season added back, no CI)
# ─────────────────────────────────────────────
all_dates   <- sort(unique(data.kh$date))
test_dates  <- tail(all_dates, 360)
train_dates <- setdiff(all_dates, test_dates)

data_train <- data.kh %>% filter(date %in% train_dates)
data_test  <- data.kh %>% filter(date %in% test_dates)

n_train_days <- length(unique(data_train$id.date))
test_seq     <- seq_len(length(test_dates))
max_zoneud   <- max(data_train$id.zoneu.dateu)

data_test_forecast <- data_test %>%
  mutate(
    id.date        = n_train_days + test_seq[match(date, test_dates)],
    id.dateu       = id.date,
    id.zoneu.dateu = max_zoneud + seq_len(n())
  )

data_test_forecast$TMIN <- NA
data_joint <- bind_rows(data_train, data_test_forecast)

formula_KH4_forecast <- TMIN ~ 1 +
  f(id.zone.int, model = "besag", graph = station.adj.mat,
    group = id.date, control.group = list(model = "rw1")) +
  f(id.date,   model = "rw1") +
  f(id.dateu,  model = "iid") +
  f(id.zone,   model = "iid")

fit.kh4.forecast <- inla(
  formula_KH4_forecast,
  data             = data_joint,
  family           = "gaussian",
  control.predictor = list(compute = TRUE, link = 1),
  control.compute   = list(dic = TRUE, waic = TRUE)
)

n_train  <- nrow(data_train)
n_total  <- nrow(data_joint)

pred_values <- fit.kh4.forecast$summary.fitted.values[(n_train + 1):n_total, ]

forecast_df <- data_test_forecast %>%
  mutate(
    forecast = pred_values$mean
  )

# Add back seasonal component
doy_test  <- yday(forecast_df$date)
Smin_test <- season_lookup$Smin_mean[match(doy_test, season_lookup$doy)]

forecast_df <- forecast_df %>%
  mutate(
    forecast_raw = forecast + Smin_test
  )

# Plot forecast_raw vs. observed TMIN
selected_zone <- unique(forecast_df$id.zone.int)[1]

ggplot(
  forecast_df %>% filter(id.zone.int == selected_zone),
  aes(x = date)
) +
  geom_line(aes(y = forecast_raw), color = "#56B4E9", size = 0.6) +
  geom_line(
    data = data_test %>% filter(id.zone.int == selected_zone),
    aes(x = date, y = TMIN),
    color = "black", size = 0.2
  ) +
  labs(
    title = paste0("KH4 Forecast + Seasonality for Station ", selected_zone),
    x     = "Date",
    y     = "TMIN (°C)"
  ) +
  theme_minimal(base_size = 11)





