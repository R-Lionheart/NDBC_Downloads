# Read the key file into a character vector
key_lines <- readLines("data_raw/key")

# Initialize an empty data frame to store the mappings
mapping_df <- data.frame(
  Acronym = character(),
  Original_Definition = character(),
  User_Definition = character(),
  stringsAsFactors = FALSE
)

# Initialize index
i <- 1

# Loop through the lines to extract acronyms and definitions
while (i <= length(key_lines)) {
  line <- trimws(key_lines[i])
  
  # Check if the line is an acronym (3 to 5 uppercase letters)
  if (grepl("^[A-Z]{3,5}$", line)) {
    acronym <- line
    i <- i + 1
    definition <- ""
    
    # Collect definition lines until empty line or next acronym
    while (i <= length(key_lines)) {
      next_line <- trimws(key_lines[i])
      if (next_line == "" || grepl("^[A-Z]{3,5}$", next_line)) {
        break
      } else {
        # Append the line to the definition
        definition <- paste(definition, next_line)
        i <- i + 1
      }
    }
    
    # Trim whitespace
    definition <- trimws(definition)
    
    # Check if acronym already exists in mapping_df
    existing_row <- mapping_df$Acronym == acronym
    if (any(existing_row)) {
      # If acronym already exists, concatenate definitions
      mapping_df$Original_Definition[existing_row] <- paste(
        mapping_df$Original_Definition[existing_row],
        "|",
        definition
      )
    } else {
      # Add new row to mapping_df
      mapping_df <- rbind(mapping_df, data.frame(
        Acronym = acronym,
        Original_Definition = definition,
        User_Definition = "",
        stringsAsFactors = FALSE
      ))
    }
  } else {
    i <- i + 1
  }
}

# Create a named vector of your definitions
user_defs <- c(
  WDIR = "wind_dir_origin",
  WSPD = "wind_speed_ms",
  GST  = "gust_speed_ms",
  WVHT = "wave_ht_m",
  DPD  = "wave_prd_sec",
  APD  = "avg_wave_prd_sec",
  MWD  = "avg_wave_dir_origin",
  PRES = "sea_level_pressure",
  ATMP = "air_temp_c",
  WTMP = "sst_c",
  DEWP = "dew_temp_c",
  VIS  = "vis_nm",
  PTDY = "pressure_dir_origin",
  TIDE = "above_mllw_ft",
  LAT  = "latitude",
  LON  = "longitude",
  GDR  = "gust_origin",
  GTIME= "gust_time"
)

# Update the User_Definition column in mapping_df
for (acronym in names(user_defs)) {
  mapping_df$User_Definition[mapping_df$Acronym == acronym] <- user_defs[acronym]
}

# View the mapping data frame
print(mapping_df)

# Save the mapping data frame
write.csv(mapping_df, "data_secondary/columns_key.csv", row.names = FALSE)
