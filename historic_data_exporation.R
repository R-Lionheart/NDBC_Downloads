require(clifro)
require(data.table)
require(tidyverse)
require(gt)


## First exploratory analysis of historic meteorological data

# Functions ---------------------------------------------------------------
rename_columns <- function(df, key) {
  df_key_replace <- df %>%
    rename_with(~ ifelse(.x %in% key[[1]],
                         key[[3]][match(.x, key[[1]])], .x))
  
  df_full <- df_key_replace %>%
    rename("location" = 1, "year" = 2, "month" = 3,
           "day" = 4, "hour" = 5,"minute" = 6)
  
  return(df_full)
}

to_numeric <- function(df) {
  df %>%
    mutate(across(where(~ all(is.na(.) | !is.na(suppressWarnings(as.numeric(.))))), 
                  ~ as.numeric(.)))
}

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

# File upload -------------------------------------------------------------
hist_meteo_og <- fread("data_raw/historic_meteo_2024-10-22.csv", header = T)
#hist_wind_og <- fread("data_raw/historic_wind_JDF_2024-10-23.csv", header = T)
col_key <- read.csv("data_secondary/columns_key.csv")

# Tidying -----------------------------------------------------------------
hist_meteo_og <- hist_meteo_og |>
  mutate(column_label = sub('(.*)_\\w+', '\\1', hist_meteo_og$column_label))

# Drop non-numeric characters leftover from old headers
hist_meteo_nochar <- hist_meteo_og |>
  filter(if_all(-1, ~ str_detect(., "^[0-9.]+$") | is.na(.)))
#hist_wind_nochar <- hist_wind_og |>
#  filter(if_all(-1, ~ str_detect(., "^[0-9.]+$") | is.na(.)))

# Assign clearer column names
hist_meteo_colnames <- rename_columns(hist_meteo_nochar, col_key)
hist_wind_colnames <- rename_columns(hist_wind_nochar, col_key)

# Change columns to numeric due to different NA designation
hist_meteo_num <- to_numeric(hist_meteo_colnames)
hist_wind_num <- to_numeric(hist_wind_colnames)

# Change all missing data codes to NA
missing_codes <- c(999, 99.0, 99.00, 999.0, 9999.0)
hist_meteo_nona <- hist_meteo_num |>
  mutate(across(-c(1:6), ~ replace(., . %in% missing_codes, NA)))
hist_wind_nona <- hist_wind_num |>
  mutate(across(-c(1:6), ~ replace(., . %in% missing_codes, NA)))

# Aggregate to the hour, dropping minute data
hist_meteo_tidy <- hist_meteo_nona %>%
  group_by(location, year, month, day, hour) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)), .groups = "drop") %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

hist_wind_tidy <- hist_wind_nona %>%
  group_by(location, year, month, day, hour) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)), .groups = "drop") %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))


# Missing data ----------------------------------------------------------------
print(paste("Percentage of historic meteorological data that is NA:",
            round(sum(is.na(hist_meteo_tidy[, 7:21])) / 
                    prod(dim(hist_meteo_tidy[,7:20])), 2)))
print(paste("Percentage of historic wind only data that is NA:",
            round(sum(is.na(hist_wind_tidy[, 7:15])) / 
                    prod(dim(hist_wind_tidy[,7:15])), 2)))

# Missing data as a table
missing_data <- hist_meteo_tidy |> 
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
summary(hist_meteo_tidy[, 7:21])

# Histograms
make_hist(hist_meteo_tidy, "wind_speed_ms") # wind speed
make_hist(hist_meteo_tidy, "wave_ht_m") # wave height, jdf only
make_hist(hist_meteo_tidy, "wave_prd_sec") # wave period, jdf only

## Monthly wind speed distribution
ggplot(hist_meteo_tidy, aes(factor(month), wind_speed_ms)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~ year) +
  labs(x = "Month")


## Time series of wind speed? Facet wrap by month?

## Weibull fit (define weibull)
t <- recent_wind |> filter(wind_speed_ms != 0)

weibull_fit <- fitdistr(t$wind_speed_ms, "weibull")
x <- seq(0, 20, .01)
weibull_density <- tibble(x, y = dweibull(x = x, shape = weibull_fit$estimate[1], scale = weibull_fit$estimate[2]))
ggplot(df, aes(windspeedat100mms)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "white") +
  geom_line(data = weibull_density, aes(x = x, y = y), color = "red") +
  theme_minimal()
