require(clifro)
require(data.table)
require(tidyverse)
require(gt)
require(nanoparquet)

# Functions ---------------------------------------------------------------
make_hist <- function(df, column_name) {
  num_bins <- 50
  
  ggplot(df) +
    aes_string(x = column_name) +
    geom_histogram(
      bins = num_bins,
      fill = "steelblue", color = "black") +
    theme_minimal() +
    facet_wrap(~ location) +
    labs(title = paste("Histogram of", column_name), 
         x = column_name, y = "Count")
  
}

# Load data ----------------------------------------------------------------
tidy_data <- read_parquet("data_secondary/historic_meteo_tidy_2024-10-28.csv")

# Missing data ----------------------------------------------------------------
print(paste("Percentage of historic meteorological data that is NA:",
            round(sum(is.na(tidy_data[, 7:21])) / 
                    prod(dim(tidy_data[,7:20])), 3)))

# Missing data as a table
missing_data <- tidy_data |> 
  gather(key, value, -c(location:minute)) |> 
  group_by(key) |> 
  count(na = is.na(value)) |> 
  pivot_wider(names_from = na, values_from = n, values_fill = 0) |> 
  mutate(pct_missing = (`TRUE`/sum(`TRUE`, `FALSE`))*100) |> 
  ungroup() |>
  arrange(pct_missing) 
missing_data |>
  gt()

# Missing data as a graph
(missing_data_graph <- missing_data |> 
    mutate(Present = 100 - pct_missing) |>
    gather(Key, value, 4:5) |>    
    mutate(Key = recode(Key, pct_missing = "Missing")) |>
    ggplot(aes(x = reorder(key, `TRUE`), y = value, fill = Key)) +       
    geom_col(alpha = 0.85) +
    scale_fill_manual(name = "",
                      values = c('tomato3', 'steelblue'),
                      labels = c("Missing", "Present")) +
    coord_flip() +
    labs(x = NULL, y = "Missing (%)"))

# Basic data exploration -------------------------------------------------------

# Essential summary
summary(tidy_data[, 7:21])

# Histograms
make_hist(tidy_data, "wind_speed_ms") # wind speed
make_hist(tidy_data, "wave_ht_m") # wave height, jdf only
make_hist(tidy_data, "wave_prd_sec") # wave period, jdf only

## Monthly wind speed distribution
ggplot(tidy_data, aes(factor(month), wind_speed_ms)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~ year) +
  labs(x = "Month")

# Peaks Over Threshold ----------------------------------------------------

# Peaks over threshold, wind
recent_data <- read_parquet("data_secondary/recent_meteo_tidy_2024-10-28.csv") |>
  select(location:hour, wind_speed_ms) |>
  as.data.frame()

historic_data <- tidy_data |>
  select(location:hour, wind_speed_ms) |>
  as.data.frame() 

all_data <- recent_data |> 
  rbind(historic_data) |>
  mutate(wind_speed_ms = round(wind_speed_ms, 2))

print(paste("Percentage of historic meteorological data that is NA:",
            round(sum(is.na(all_data[, 6]), na.rm = TRUE)/length(all_data[, 6]), 3)))

threshold <- 8
selected_location <- "port_townsend"

all_POT <- all_data |>
  filter(location == selected_location) |>
  drop_na() |>
  mutate(wind_speed_ms = round(wind_speed_ms, 2)) |>
  mutate(timestamp = ymd_h(paste(year, month, day, hour))) |>
  select(timestamp, location, year, month, day, hour, wind_speed_ms) |>
  mutate(above_threshold = wind_speed_ms > threshold,
         event_id = cumsum(c(0, diff(above_threshold)) == 1 & above_threshold))

exceedance_windows <- all_POT %>%
  filter(above_threshold == TRUE) %>%
  group_by(event_id) %>%
  summarise(
    max_wind_speed = max(wind_speed_ms),  # Maximum wind speed in each window
    max_time = timestamp[which.max(wind_speed_ms)]  # Timestamp of the maximum wind speed
  ) %>%
  ungroup()

exceedance_summary <- exceedance_windows %>%
  summarise(
    num_exceedances = n(),
    max_exceedance = max(wind_speed_ms),
    mean_exceedance = mean(wind_speed_ms),
    threshold_value = threshold
  )
print(exceedance_summary)

ggplot(all_POT, aes(x = timestamp, y = wind_speed_ms)) +
  geom_line(color = "gray", alpha = 0.9) +  
  geom_hline(yintercept = threshold, linetype = "dashed", color = "red", linewidth = 1) + 
  geom_point(data = exceedance_windows, aes(x = max_time, y = max_wind_speed), 
             color = "blue", size = 3, shape = 4) +  # Highlight maximum points in exceedance windows
  labs(title = "Peaks Over Threshold",
       subtitle = paste("Threshold:", threshold, "m/s, ", unique(all_POT$location)),
       x = "Time",
       y = "Wind Speed (m/s)") +
  theme_minimal()

# Peaks over threshold, waves, 
recent_data_wave <- read_parquet("data_secondary/recent_meteo_tidy_2024-10-28.csv") |>
  select(location:hour, wave_ht_m) |>
  as.data.frame()

historic_data_wave <- tidy_data |>
  select(location:hour, wave_ht_m) |>
  as.data.frame() 

all_data_wave <- recent_data_wave |> 
  rbind(historic_data_wave) |>
  mutate(wave_ht_m = round(wave_ht_m, 2)) |>
  filter(location == "juan_de_fuca")

print(paste("Percentage of historic wave data that is NA:",
            round(sum(is.na(all_data_wave[, 6]), na.rm = TRUE)/length(all_data_wave[, 6]), 3)))

threshold <- quantile(all_data_wave$wave_ht_m, 0.99, na.rm = TRUE)

all_POT_wave <- all_data_wave |>
  drop_na() |>
  mutate(wave_ht_m = round(wave_ht_m, 2)) |>
  mutate(timestamp = ymd_h(paste(year, month, day, hour))) |>
  select(timestamp, location, year, month, day, hour, wave_ht_m) |>
  mutate(above_threshold = wave_ht_m > threshold,
         event_id = cumsum(c(0, diff(above_threshold)) == 1 & above_threshold))

exceedance_windows_wave <- all_POT_wave %>%
  filter(above_threshold == TRUE) %>%
  group_by(event_id) %>%
  mutate(
    max_wave_ht = max(wave_ht_m),  # Maximum wind speed in each window
    max_time = timestamp[which.max(wave_ht_m)]  # Timestamp of the maximum wind speed
  ) %>%
  ungroup()

exceedance_summary_wave <- exceedance_windows_wave %>%
  summarise(
    num_exceedances = n(),
    max_exceedance = max(wave_ht_m),
    mean_exceedance = mean(wave_ht_m),
    threshold_value = threshold
  )
print(exceedance_summary_wave)

ggplot(all_POT_wave, aes(x = timestamp, y = wave_ht_m)) +
  geom_line(color = "gray", alpha = 0.9) +  
  geom_hline(yintercept = threshold, linetype = "dashed", color = "red", linewidth = 1) + 
  geom_point(data = exceedance_windows_wave, aes(x = max_time, y = max_wave_ht), 
             color = "blue", size = 3, shape = 4) +  # Highlight maximum points in exceedance windows
  labs(title = "Peaks Over Threshold, Wave Height",
       subtitle = paste("Threshold:", threshold, "m/s, Juan de Fuca Buoy"),
       x = "Time",
       y = "Wind Speed (m/s)") +
  theme_minimal()

## Time series of wind speed? Facet wrap by month?

## Weibull fit (define weibull)
# t <- recent_wind |> filter(wind_speed_ms != 0)
# 
# weibull_fit <- fitdistr(t$wind_speed_ms, "weibull")
# x <- seq(0, 20, .01)
# weibull_density <- tibble(x, y = dweibull(x = x, shape = weibull_fit$estimate[1], scale = weibull_fit$estimate[2]))
# ggplot(df, aes(windspeedat100mms)) +
#   geom_histogram(aes(y = ..density..), bins = 30, color = "white") +
#   geom_line(data = weibull_density, aes(x = x, y = y), color = "red") +
#   theme_minimal()
