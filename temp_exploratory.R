require(data.table)
require(tidyverse)


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

to_numeric() <- function(df) {
  df %>%
    mutate(across(where(~ all(is.na(.) | !is.na(suppressWarnings(as.numeric(.))))), 
                  ~ as.numeric(.)))
}

# File upload -------------------------------------------------------------

recent_og <- fread("data_raw/recent_meteo_2024-10-22.csv", header = T) |>
  filter(MM != "mo")
# historic_og <- fread("data_raw/historic_meteo_2024-10-22.csv", header = T) |>
#   filter(MM != "mo")
col_key <- read.csv("data_secondary/columns_key.csv")

missing_data <- c(c("999", "99.0", "99.00", "999.0", "9999.0"))

# Tidying -----------------------------------------------------------------

# Assign clearer column names
recent_colnames <- rename_columns(recent_og, col_key)

# Change all missing data codes to NA
recent_addNA <- recent_colnames %>%
  mutate(across(where(~ !is.logical(.)), ~ na_if(., "MM")))

# Change columns with only numbers to numeric
recent_numeric <- to_numeric(recent_addNA)


## Check for missing values
## Define keys for missing data




# Basic shape of df
print(head(recent))
## shapes
dim()
str()
summary()


## Basic stats and missing data

check <- function(df) {
  # count NA (missing values)
  NAs <- sum(is.na(df))
  print(paste("Missing Values:", NAs))

  # count incomplete records (rows containing missing values)
  ok <- complete.cases(df)
  print(paste("Incomplete Records:", sum(!ok)))

  # Show incomplete records (if less than 100 NAs).
  if (NAs > 0 & NAs <= 100) print(df[which(!complete.cases(df)), ])

  # If more than 100, show column-wise distribution of NAs.
  if (NAs > 100) hist(which(is.na(df), arr.ind = TRUE)[, 2], xlab = "Column", freq = TRUE, breaks = 1:dim(df)[2], main = "Column-wise distribution of missing values")
}

## Show removed records
removed <- function(nrow, nrow1) {
  print(paste("number of records REMOVED:", nrow - nrow1, sep = " "))
  print(paste("number of records REMAINING:", nrow1, sep = " "))
}

## Check that missing values are actually removed
## provide some percentages, stats on how much is missing

## Check for outliers
## Histogram of speed
## Figure out visualization of wind and wave direction (rose?)
windrose(
  speed = df$windspeedat100mms,
  direction = df$winddirectionat100mdeg,
  n_directions = 12,
  speed_cuts = seq(0, 20, 4),
  ggtheme = "minimal",
  col_pal = "YlGnBu"
)

## Time stamp boxes, continuous?
## Monthly wind speed distribution
ggplot(df, aes(factor(Month), windspeedat100mms)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Month")
## Wind direction, what is the most common?
## Sig wave height over time?
## Time series of wind speed? Facet wrap by month?
## mean/max/median wind speed over time
## Weibull fit (define weibull)
weibull_fit <- fitdistr(df$windspeedat100mms, "weibull")
x <- seq(0, 20, .01)
weibull_density <- tibble(x, y = dweibull(x = x, shape = weibull_fit$estimate[1], scale = weibull_fit$estimate[2]))
ggplot(df, aes(windspeedat100mms)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "white") +
  geom_line(data = weibull_density, aes(x = x, y = y), color = "red") +
  theme_minimal()

## Visualize wind speed over time on a map, animated? Raster image?
## Aggregates?
monthly <- df %>%
  group_by(Month) %>%
  summarise(
    hours = n() / 12,
    windspeedat100mms = mean(windspeedat100mms),
    mwh = sum(kw, na.rm = T) / (1000 * 12)
  ) %>%
  mutate(ncf = percent(mwh / (hours * 1.5)))
kable(monthly, digits = 1, caption = "monthly totals", align = "c")

## Predictions?
