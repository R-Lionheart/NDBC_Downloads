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


# Missing data ----------------------------------------------------------------
print(paste("Percentage of historic meteorological data that is NA:",
            round(sum(is.na(hist_meteo_tidy[, 7:21])) / 
                    prod(dim(hist_meteo_tidy[,7:20])), 3)))
# print(paste("Percentage of historic wind only data that is NA:",
#             round(sum(is.na(hist_wind_tidy[, 7:15])) / 
#                     prod(dim(hist_wind_tidy[,7:15])), 2)))

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
