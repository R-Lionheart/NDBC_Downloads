# historic_og <- fread("data_raw/historic_meteo_2024-10-22.csv", header = T) |>
#   filter(MM != "mo")

missing_data <- c(c("999", "99.0", "99.00", "999.0", "9999.0"))