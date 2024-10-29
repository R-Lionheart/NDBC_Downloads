require(clifro)
require(data.table)
require(tidyverse)
require(gt)
require(nanoparquet)


# Functions ---------------------------------------------------------------
make_hist <- function(df, column_name) {
  num_bins <- ceiling(sqrt(nrow(df)))
  
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
recent_tidy <- read_parquet("data_secondary/recent_meteo_tidy_2024-10-28.csv")

# Missing data ----------------------------------------------------------------
print(paste("Percentage of data that is NA:",
            round(sum(is.na(recent_tidy[, 7:20]))/prod(dim(recent_tidy[,7:20])), 2)))

# Missing data as a table
missing_data <- recent_tidy |> 
  gather(key, value, -c(location:minute, RECENT)) |> 
  group_by(key) |> 
  count(na = is.na(value)) |> 
  pivot_wider(names_from = na, values_from = n, values_fill = 0) |> 
  mutate(pct_missing = (`TRUE`/sum(`TRUE`, `FALSE`))*100) |> 
  ungroup() |>
  arrange(pct_missing)
(missing_data_table <- missing_data |> 
    select(-2, -3)|>
    gt())

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
summary(recent_tidy[, 7:20])

# Histograms
make_hist(recent_tidy, "wind_speed_ms") # wind speed
make_hist(recent_tidy, "wave_ht_m") # wave height, jdf only
make_hist(recent_tidy, "wave_prd_sec") # wave period, jdf only

## Monthly wind speed distribution
ggplot(recent_tidy, aes(factor(month), wind_speed_ms)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Month")

# Recent wind direction, windrose
recent_wind <- recent_tidy %>%
  rename(station = location) %>%
  select(station, contains("wind"))
with(recent_wind, windrose(
  speed = wind_speed_ms,
  direction = wind_dir_origin,
  station,
  n_col = 2,
  n_directions = 12,
  speed_cuts = seq(0, 16, 4),
  ggtheme = "bw",
  col_pal = "Paired",
  legend_title = "Wind Speed, m/s"
))

# Recent wave direction, windrose
recent_wave <- recent_tidy |>
  rename("station" = location) |>
  select(station, contains("wave")) |>
  filter(str_detect(station, "juan"))
with(recent_wave, windrose(
  station,
  speed = wave_ht_m,
  direction = avg_wave_dir_origin,
  ggtheme = "bw",
  col_pal = "Paired",
  legend_title = "Wave Height, m"
))

## Sig wave height over time
recent_wave_line <- recent_tidy |>
  mutate(date = make_datetime(year, month, day, hour)) |>
  select(date, wave_ht_m) |>
  drop_na()
ggplot(data = recent_wave_line, 
       aes(x = date, y = wave_ht_m)) +
  geom_line()

## Temperatures over time, all recent data
recent_temps <- recent_tidy |>
  mutate(date = make_datetime(year, month, day, hour)) |>
  select(date, contains("_c")) |>
  pivot_longer(cols = 2:4)
ggplot(data = recent_temps, 
       aes(x = date, y = value, color = name)) +
  geom_line()

# Temps grouped by month
recent_temps_box <- recent_tidy |>
  select(location, year, month, contains("_c")) |>
  group_by(location, year, month) |>
  summarise(across(contains("_c"), ~ mean(.x, na.rm = TRUE))) |>
  pivot_longer(cols = 4:6)
ggplot(recent_temps_box, aes(x = factor(month), value, fill=factor(name))) +
  geom_boxplot()


# Peaks Over Threshold ----------------------------------------------------

# Peaks over threshold
threshold <- 8
recent_POT <- recent_tidy |>
  filter(location == "port_townsend") |>
  mutate(wind_speed_ms = round(wind_speed_ms, 2)) |>
  mutate(timestamp = ymd_h(paste(year, month, day, hour))) |>
  select(timestamp, location, year, month, day, hour, wind_speed_ms) |>
  mutate(above_threshold = wind_speed_ms > threshold,
         event_id = cumsum(c(0, diff(above_threshold)) == 1 & above_threshold))

exceedance_windows <- recent_POT %>%
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

ggplot(recent_POT, aes(x = timestamp, y = wind_speed_ms)) +
  geom_line(color = "gray", alpha = 0.9) +  
  geom_hline(yintercept = threshold, linetype = "dashed", color = "red", linewidth = 1) + 
  geom_point(data = exceedance_windows, aes(x = max_time, y = max_wind_speed), 
             color = "blue", size = 3, shape = 4) +  # Highlight maximum points in exceedance windows
  labs(title = "Peaks Over Threshold",
       subtitle = paste("Threshold:", threshold, "m/s, ", unique(recent_POT$location)),
       x = "Time",
       y = "Wind Speed (m/s)") +
  theme_minimal()
