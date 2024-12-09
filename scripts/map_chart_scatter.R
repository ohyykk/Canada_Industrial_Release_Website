#### Setup ####
# load Libraries
library(htmlwidgets)
library(lubridate)
library(ggplot2)
library(leaflet)
library(readr)
library(dplyr)
library(here)
library(sf)

# load Data
coor_data <- read_csv(here("data", "analysis_data", "map_coordinate_data.csv"),
                      show_col_types = FALSE)

cleaned_data <- read_csv(here("data", "analysis_data", "map_cleaned_data.csv"),
                         show_col_types = FALSE)



#### Filter Data (for developing) ####
# Filter province AB
coor_data <- coor_data %>%
  filter(province == "AB") %>%
  select(-province)

# Filter province AB
cleaned_data <- cleaned_data %>%
  filter(province == "AB") %>%
  mutate(facility = tolower(facility)) %>%
  group_by(NPRI_id, province, unit, facility, substance) %>%
  summarize(
    quantity = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  )



#### Charting ####
# Merge the datasets by NPRI_id
combined_data <- cleaned_data %>%
  inner_join(coor_data, by = "NPRI_id") %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Aggregate emissions by facility, province, latitude, and longitude, preserving substances
aggregated_data <- combined_data %>%
  group_by(facility, province, latitude, longitude) %>%
  summarise(
    substances_info = paste(
      "Substance: ", substance, "<br>",
      "Emission Quantity: ", quantity, unit, collapse = "<br><br>"
    ),
    total_quantity = sum(quantity, na.rm = TRUE),
    .groups = 'drop'
  )

# Convert the data to an sf object for spatial analysis
aggregated_data_sf <- st_as_sf(aggregated_data, coords = c("longitude", "latitude"), crs = 4326)

# Create an interactive map to visualize the facilities and their aggregated emissions
coor_map <- leaflet(aggregated_data_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = ~log(total_quantity + 1) * 0.6,
    color = "#0073C2FF",
    stroke = FALSE,
    fillOpacity = 0.6,
    clusterOptions = markerClusterOptions(
      disableClusteringAtZoom = 18
    ),
    popup = ~paste(
      "Facility: ", facility, "<br>",
      "Province: ", province, "<br><br>",
      substances_info
    )
  )



#### Save HTML ####
saveWidget(coor_map, file = here("plots", "map_scatter.html"))


