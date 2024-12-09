
# Load required libraries
library(dplyr)
library(readr)

# Load datasets
data1 <- read_csv("data1.csv") # Replace with the correct path to your first dataset
data2 <- read_csv("data2.csv") # Replace with the correct path to your second dataset
data3 <- read_csv("data3.csv") # Replace with the correct path to your third dataset

# Select relevant columns and clean datasets
data1_clean <- data1 %>%
  select(
    `Reporting Year / Année`,
    `Province`,
    `Substance Name (English) / Nom de substance (Anglais)`,
    `Quantity / Quantité`
  ) %>%
  rename(
    Reporting_Year = `Reporting Year / Année`,
    Province = `Province`,
    Substance_Name = `Substance Name (English) / Nom de substance (Anglais)`,
    Quantity = `Quantity / Quantité`
  ) %>%
  mutate(Source = "Data1") # Tag for source tracking

data2_clean <- data2 %>%
  select(
    `Reporting Year / Année`,
    `Province`,
    `Substance Name (English) / Nom de substance (Anglais)`,
    `Quantity / Quantité`,
    `NAICS Title EN / Titre Code SCIAN EN`,
    `Category (English) / Catégorie (Anglais)`
  ) %>%
  rename(
    Reporting_Year = `Reporting Year / Année`,
    Province = `Province`,
    Substance_Name = `Substance Name (English) / Nom de substance (Anglais)`,
    Quantity = `Quantity / Quantité`,
    Industry = `NAICS Title EN / Titre Code SCIAN EN`,
    Category = `Category (English) / Catégorie (Anglais)`
  ) %>%
  mutate(Source = "Data2") # Tag for source tracking

data3_clean <- data3 %>%
  select(
    `Reporting Year / Année`,
    `Province`,
    `Substance Name (English) / Nom de substance (Anglais)`,
    `Quantity / Quantité`,
    `Category (English) / Catégorie (Anglais)`
  ) %>%
  rename(
    Reporting_Year = `Reporting Year / Année`,
    Province = `Province`,
    Substance_Name = `Substance Name (English) / Nom de substance (Anglais)`,
    Quantity = `Quantity / Quantité`,
    Category = `Category (English) / Catégorie (Anglais)`
  ) %>%
  mutate(Source = "Data3") # Tag for source tracking

# Combine all datasets
combined_data <- bind_rows(data1_clean, data2_clean, data3_clean)

# Handle missing values (optional)
combined_data <- combined_data %>%
  filter(!is.na(Quantity) & Quantity > 0) # Remove rows with missing or zero quantities

# Save the combined data into a CSV file
write_csv(combined_data, "combined_pollution_data.csv")

# Output success message
cat("Combined data has been saved to 'combined_pollution_data.csv'")
