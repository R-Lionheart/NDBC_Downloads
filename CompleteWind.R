## Complete mock script
library(clifro)
library(data.table)
library(gt)
library(tidyverse)

## Station codes as a vector:
station_codes <- c(port_townsend = "PTWW1", 
                   smith_island = "SISW1") 
                   #juan_de_fuca = "46088")

## Years of interest as a range:
years <- 2005:2023

## Source downloads script:
source("scripts/download_ndbc_data.R")

## Define keys for missing data
missing_data <- c(c("999", "99.0", "99.00", "999.0", "9999.0"))


# Read in original raw data -----------------------------------------------

## Rename columns to things that make sense
original.columns <- c("DateTimeStamp", "ATemp", "F_ATemp", "RH", "F_RH",
                      "BP", "F_BP", "WSpd", "F_WSpd", "MaxWSpd", "Wdir",
                      "F_Wdir", "SDWDir", "F_SDWDir", "TotPrcp", "F_TotPrcp",
                      "TotSoRad", "F_TotSoRad")

new.columns = c("DateTime", "AirTemp", "AirTemp_Flag", "RelativeHum",
                "RelativeHum_Flag", "BarPress", "BarPress_Flag",
                "WindSpeed", "WindSpeed_Flag", "MaxWindSpeed", "WindDir",
                "WindDir_Flag", "StdDevWindDir", "StdDevWindDir_Flag",
                "TotPrecip", "TotPrecip_Flag", "TotSolarRad", "TotSolRad_Flag")

historical.data <- read_csv("data_raw/PadillaBayFarm_HistoricalMetData/942362.csv")

## Rename the columns
tidy.historical.data <- historical.data %>%
  select(all_of(original.columns)) %>%
  rename_with(~ new.columns[which(original.columns == .x)],
              .cols = original.columns)

# Prepare data for windrose -----------------------------------------------

## Tidying steps:
# Take mean of speed and direction per hour
tidy.edits <- tidy.historical.data %>%
  separate(DateTime, into = c("month", "day", "year", "hour", "minute")) %>%
  select(year, month, day, hour, minute, direction = WindDir, speed = WindSpeed) %>%
  mutate_if(is.character, as.numeric) %>%
  group_by(year, month, day, hour) %>%
  mutate(avg_dir = round(mean(direction, na.rm = TRUE)),
         avg_speed = round(mean(speed, na.rm = TRUE), digits = 2)) %>%
  select(-c(minute, direction, speed)) %>%
  arrange(year, month, day, hour) %>%
  unique() %>%
  as.data.frame() %>%
  mutate_all(~ifelse(is.nan(.), NA, .))

# Plot windrose -----------------------------------------------

## Only columns
clayton_wind_df <- tidy.historical.data %>%
  select(wind_dirs = WindDir, wind_speeds = WindSpeed) %>%
  mutate(wind_dirs = as.numeric(wind_dirs),
         wind_speeds = as.numeric(wind_speeds)) %>%
  drop_na()

# Create custom speed bins, add a legend title, and change to a B&W theme
with(clayton_wind_df, windrose(wind_speeds, wind_dirs,
                               speed_cuts = c(1, 3, 6, 9),
                               legend_title = "Wind Speed\n(m/s)",
                               legend.title.align = .5,
                               ggtheme = "linedraw",
                               col_pal = "Spectral"))


# Check for NAs and remove ------------------------------------------------

check_NAs<-function(df){
  # count NA (missing values)
  NAs<-sum(is.na(df))
  print(paste("Missing Values:", NAs))
  
  # count incomplete records (rows containing missing values)
  ok<-complete.cases(df)
  print(paste("Incomplete Records:", sum(! ok)))
  
  # Show incomplete records (if less than 100 NAs). 
  if(NAs > 0 & NAs <= 100) print( df[which(! complete.cases(df)), ] )
  
  # If more than 100, show column-wise distribution of NAs.
  if (NAs > 100) hist(which(is.na(df), arr.ind=TRUE)[,2], xlab="Column", freq=TRUE, breaks=1:dim(df)[2], main="Column-wise distribution of missing values")
}
check_NAs(tidy.edits)
tidy.edits<-na.omit(tidy.edits) # omit rows containing missing values


# Visualize wind speed over time ------------------------------------------
t <- tidy.edits %>%
  group_by(year, month, day) %>%
  mutate(speed = mean(avg_speed))

ggplot(t, aes(x = factor(month), y = speed, group = day)) +
         geom_line()
       