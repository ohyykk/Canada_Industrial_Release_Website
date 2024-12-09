#### Setup ####
# load Libraries
library(lubridate)
library(dplyr)
library(readr)
library(here)

# load Data
coor_data <- read_csv(here("data", "raw_data", "NPRI-INRP_GeolocationsGeolocalisation_1993-present.csv"), 
                     locale = locale(encoding = "ISO-8859-1"),
                     show_col_types = FALSE)



#### Clean Data ####
# clean columns
map_coordinate_data <- coor_data %>%
  select(
    NPRI_id = `NPRI ID / ID INRP`,
    province = `Province / Province`,
    latitude = `Latitude / Latitude`,
    longitude= `Longitude / Longitude`
  )



#### Save By Province Data ####
write_csv(map_coordinate_data, here("data", "analysis_data", "map_coordinate_data.csv"))