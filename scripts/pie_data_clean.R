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

# data aggregation
pie_chart_data <- map_cleaned_data %>%
  group_by(
    emit_way,
    unit
  ) %>%
  summarize(
    quantity = sum(quantity, na.rm = TRUE),  # Sum the quantities, ignoring NAs
    .groups = "drop"  # Ungroup after summarizing
  )


bar_chart_data <- map_cleaned_data %>%
  group_by(
    substance,
    emit_way,
    unit
  ) %>%
  summarize(
    quantity = sum(quantity, na.rm = TRUE),  # Sum the quantities, ignoring NAs
    .groups = "drop"  # Ungroup after summarizing
  )

bar_chart_data <- bar_chart_data %>%
  group_by(
    emit_way,
    unit) %>%
  top_n(5, wt = quantity) %>%
  ungroup() %>%
  arrange(emit_way, desc(quantity))

line_chart_data <- map_cleaned_data %>%
  group_by(
    year,
    substance,
    emit_way,
    unit
  ) %>%
  summarize(
    quantity = sum(quantity, na.rm = TRUE),  # Sum the quantities, ignoring NAs
    .groups = "drop"  # Ungroup after summarizing
  )

line_chart_data <- line_chart_data %>%
  group_by(
    substance,
    emit_way,
    unit) %>%
  top_n(5, wt = sum(quantity)) %>%
  ungroup() %>%
  arrange(emit_way, substance, year)

#### Save Data ####
write_csv(pie_chart_data, here("data", "analysis_data", "pie_chart_data.csv"))
write_csv(bar_chart_data, here("data", "analysis_data", "bar_chart_data.csv"))
write_csv(line_chart_data, here("data", "analysis_data", "line_chart_data.csv"))
