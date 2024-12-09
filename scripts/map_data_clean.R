#### Setup ####
# load Libraries
library(lubridate)
library(dplyr)
library(readr)
library(here)

# load Data
raw_data <- read_csv(here("data", "raw_data", "NPRI-INRP_ReleasesRejets_1993-present.csv"), 
                     locale = locale(encoding = "ISO-8859-1"),
                     show_col_types = FALSE)



#### Clean Data ####
# clean columns
map_cleaned_data <- raw_data %>%
  select(
    year = `Reporting_Year / Année`,
    province = PROVINCE,
    NPRI_id = `NPRI_ID / No_INRP`,
    facility = `Facility_Name / Installation`,
    substance = `Substance Name (English) / Nom de substance (Anglais)`,
    emit_way = `Group (English) / Groupe (Anglais)`,
    quantity = `Quantity / Quantité`,
    unit = `Units / Unités`
  )

# unified unit
map_cleaned_data <- map_cleaned_data %>%
  mutate(
    # Convert quantity to grams based on the unit
    quantity = case_when(
      unit == "grams" ~ quantity,
      unit == "kg" ~ quantity * 1e3,
      unit == "tonnes" ~ quantity * 1e6,
      unit == "g TEQ" ~ quantity,
      TRUE ~ NA_real_
    ),
    # Update all units to "grams"
    unit = "grams"
  )

# update emit_way values
map_cleaned_data <- map_cleaned_data %>%
  mutate(
    emit_way = case_when(
      emit_way == "Releases to Water Bodies" ~ "water",
      emit_way == "Releases to Land" ~ "land",
      emit_way == "Releases to Air" ~ "air",
      emit_way == "Sum of release to all media (<1tonne)" ~ "all",
      TRUE ~ emit_way
    )
  )

# data aggregation
map_cleaned_data <- map_cleaned_data %>%
  group_by(
    year,
    province,
    NPRI_id,
    facility,
    substance,
    emit_way,
    unit
  ) %>%
  summarize(
    quantity = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  )

#### Save Data ####
write_csv(map_cleaned_data, here("data", "analysis_data", "map_cleaned_data.csv"))



#### By Province Data ####
# summarize by province
by_province <- map_cleaned_data %>%
  group_by(province, unit) %>%
  summarize(
    total_quantity = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  )

#### Save By Province Data ####
write_csv(by_province, here("data", "analysis_data", "map_by_province_data.csv"))



#### By Substance Data ####
# summarize by substance
by_substance <- map_cleaned_data %>%
  group_by(emit_way, substance, unit) %>%
  summarize(
    total_quantity = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  )

#### Save By Province Data ####
write_csv(by_substance, here("data", "analysis_data", "map_by_substance_data.csv"))






