require(clifro)
require(data.table)
require(tidyverse)
require(gt)
require(nanoparquet)


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

# File upload -------------------------------------------------------------
hist_meteo_og <- fread("data_raw/historic_meteo_2024-10-22.csv", header = T)
col_key <- read.csv("data_secondary/columns_key.csv")

# Tidying -----------------------------------------------------------------
hist_meteo <- hist_meteo_og |>
  mutate(year = coalesce(as.numeric(`#YY`), as.numeric(YYYY))) |>
  select(column_label, year, everything(), -c(YYYY, `#YY`))
hist_meteo$column_label <- sub("_[^_]+$", "", hist_meteo$column_label)

# Drop non-numeric characters leftover from old headers
hist_meteo_nochar <- hist_meteo |>
  filter(if_all(-1, ~ str_detect(., "^[0-9.]+$") | is.na(.)))

# Assign clearer column names
hist_meteo_colnames <- rename_columns(hist_meteo_nochar, col_key)

# Change columns to numeric due to different NA designation
hist_meteo_num <- to_numeric(hist_meteo_colnames)

# Change all missing data codes to NA
missing_codes <- c(999, 99.0, 99.00, 999.0, 9999.0)
hist_meteo_nona <- hist_meteo_num |>
  mutate(across(-c(1:6), ~ replace(., . %in% missing_codes, NA)))

# Aggregate to the hour, dropping minute data
hist_meteo_tidy <- hist_meteo_nona %>%
  group_by(location, year, month, day, hour) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)), .groups = "drop") %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .))) %>%
  mutate(location = recode(location, 
                           '46088' = 'juan_de_fuca',
                           'ptww1' = 'port_townsend',
                           "sisw1" = "smith_island"))

write_parquet(hist_meteo_tidy, 
          paste("data_secondary/historic_meteo_tidy_",
                Sys.Date(), ".csv", sep = ""),
          compression = "snappy")


# Archive for continuous wind data ----------------------------------------
#hist_wind_og <- fread("data_raw/historic_wind_JDF_2024-10-23.csv", header = T)
#hist_wind_nochar <- hist_wind_og |>
#  filter(if_all(-1, ~ str_detect(., "^[0-9.]+$") | is.na(.)))
#hist_wind_colnames <- rename_columns(hist_wind_nochar, col_key)
#hist_wind_num <- to_numeric(hist_wind_colnames)
# hist_wind_nona <- hist_wind_num |>
#   mutate(across(-c(1:6), ~ replace(., . %in% missing_codes, NA)))
# hist_wind_tidy <- hist_wind_nona %>%
#   group_by(location, year, month, day, hour) %>%
#   summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)), .groups = "drop") %>%
#   mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))