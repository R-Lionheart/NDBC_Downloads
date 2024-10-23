require(data.table)

## Download wave data
## Source of data: National Data Buoy Center

# This script downloads continuous wind data for a defined offshore buoy


# Define years and station ------------------------------------------------
station_codes <- c(juan_de_fuca = "46088") 
years <- as.character(2005:2023)


# Functions ---------------------------------------------------------------
download_historic_wind <- function(station_code, year) {
  ndbc_data <- fread(paste0("https://www.ndbc.noaa.gov/view_text_file.php?filename=",
                            station_code, "c", year, ".txt.gz&dir=data/historical/cwind/"),
                     skip = 0, header = TRUE)
}

generate_ndbc_url <- function(station_code, year) {
  paste0("https://www.ndbc.noaa.gov/view_text_file.php?filename=",
         station_code, "c", year, ".txt.gz&dir=data/historical/cwind/")
}


# Download historic wind data ---------------------------------------------

historic_wind_list <- list()

# Loop through each station code and year, download, and store in a list
for (station in station_codes) {
  for (year in years) {
    # Generate the URL
    url <- generate_ndbc_url(station, year)
    
    # Try to download the data with fread()
    try({
      ndbc_data <- fread(url, skip = 0, header = TRUE)
      
      # Add the downloaded data to the list
      historic_wind_list[[paste(station, year, sep = "_")]] <- ndbc_data
      
      # Message for tracking
      message(paste("Downloaded data for station:", station, "year:", year))
    }, silent = TRUE)
  }
}

# Mutate all columns to character, avoiding mismatching classes
historic_wind_list <- lapply(historic_wind_list, function(df) {
  df |> mutate(across(everything(), as.character))
})

historic_wind_df <- bind_rows(historic_wind_list, .id = "column_label")

write.csv(historic_wind_df, 
          paste("data_raw/historic_wind_JDF_",
                Sys.Date(), ".csv", sep = ""),
          row.names = FALSE)
