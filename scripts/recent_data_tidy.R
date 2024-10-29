require(clifro)
require(data.table)
require(tidyverse)
require(gt)


## First exploratory analysis of meteorological data

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
recent_og <- fread("data_raw/recent_meteo_2024-10-22.csv", header = T) |>
  filter(MM != "mo")
col_key <- read.csv("data_secondary/columns_key.csv")


# Tidying -----------------------------------------------------------------

# Assign clearer column names
recent_colnames <- rename_columns(recent_og, col_key)

# Change all missing data codes to NA
recent_withNA <- recent_colnames %>%
  mutate(across(where(~ !is.logical(.)), ~ na_if(., "MM")))

# Change columns with only numbers to numeric
recent_numeric <- to_numeric(recent_withNA)

# Aggregate to the hour, dropping minute data
recent_tidy <- recent_numeric %>%
  group_by(location, year, month, day, hour) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)), .groups = "drop") %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

write_parquet(recent_tidy, 
              paste("data_secondary/recent_meteo_tidy_",
                    Sys.Date(), ".csv", sep = ""),
              compression = "snappy")
