README
================
Regina Lionheart
2024-10-22

*Overview*

This repository contains the scripts for downloading, tidying, and
exploring the meterologial data from three buoys, indicated below:

PT = Port Townsend
\#<https://www.ndbc.noaa.gov/station_page.php?station=ptww1> SI = Smith
Island \#<https://www.ndbc.noaa.gov/station_page.php?station=sisw1> JDF
= Juan de Fuca (buoy)
\#<https://www.ndbc.noaa.gov/station_page.php?station=46088>

You can recreate the analysis by running the
scripts/download_ndbc_data.R script, which will download the recent
(last 45 days) of data, as well as the complete historic dataset. That
will populate the data_raw/ subdirectory.

The recent data is downloaded separately from the historic data due to
different columns/data quality.

Headers can be a little confusing- the key is found the data_raw/
subdirectory and is applied to all datasets in the tidying scripts. All
headers in downloaded data are explained in the data_key.

The historic script contains code for downloading historic continuous
wind data from the JDF buoy, but this was not used in the analysis.

Set up github actions to update/concat the most recent data?

*Data Quality Comment*

There was one 27 m/s wind speed value that was an outlier and was
removed in the historic_tidy script.

*Questions*

Please contact Regina Lionheart for questions about this repository.
