require(data.table)

## Download wind data TODO: specify this info vs buoy info
## Source of data: National Data Buoy Center

# Functions ---------------------------------------------------------------
download_recent_ndbc <- function(station_code) {
  ndbc_data <- fread(paste0("https://www.ndbc.noaa.gov/data/realtime2/",
                            toupper(station_code), ".txt"),
                     skip = 0, header = TRUE)
} 

download_historic_ndbc <- function(station_code, year) {
  ndbc_data <- fread(paste0("https://www.ndbc.noaa.gov/view_text_file.php?filename=",
                            station_code, "h", year, ".txt.gz&dir=data/historical/stdmet/"),
                     skip = 0, header = TRUE)
}

generate_ndbc_url <- function(station_code, year) {
  paste0("https://www.ndbc.noaa.gov/view_text_file.php?filename=",
         station_code, "h", year, ".txt.gz&dir=data/historical/stdmet/")
}


# Definitions -------------------------------------------------------------
## Station codes as a vector:
station_codes <- c(port_townsend = "ptww1", 
                   smith_island = "sisw1",
                   juan_de_fuca = "46088") 

## Years of interest as a range:
years <- as.character(2005:2023)

# Download recent data -----------------------------------------------------------

## Download the last 45 days of data
recent_data_list <- lapply(station_codes, download_recent_ndbc) 
recent_data_list <- mapply(cbind, recent_data_list, "RECENT"=TRUE, SIMPLIFY=F)

## Ensure that all column names are the same before combining to one frame
lapply(recent_data_list, names) %>%
  unlist() %>% 
  table() %>% 
  all(. == length(recent_data_list))

recent_data_df <- bind_rows(recent_data_list, .id = "column_label")

write.csv(recent_data_df, 
          paste("data_raw/recent_meteo_",
                Sys.Date(), ".csv", sep = ""),
          row.names = FALSE)

# Download historic data -----------------------------------------------------------
## Generate urls for each df and access it for download

historic_data_list <- list()

# Loop through each station code and year, download, and store in a list
for (station in station_codes) {
  for (year in years) {
    # Generate the URL
    url <- generate_ndbc_url(station, year)
    
    # Try to download the data with fread()
    try({
      ndbc_data <- fread(url, skip = 0, header = TRUE)
      
      # Add the downloaded data to the list
      historic_data_list[[paste(station, year, sep = "_")]] <- ndbc_data
      
      # Message for tracking
      message(paste("Downloaded data for station:", station, "year:", year))
    }, silent = TRUE)
  }
}

# Mutate all columns to character, avoiding mismatching classes
historic_data_list <- lapply(historic_data_list, function(df) {
  df |> mutate(across(everything(), as.character))
})

historic_data_df <- bind_rows(historic_data_list, .id = "column_label")

#TODO rewrite this as nanoparquet to save space
write.csv(historic_data_df, 
          paste("data_raw/historic_meteo_",
                Sys.Date(), ".csv", sep = ""),
          row.names = FALSE)
