# Rename the dataset with "Human_exposure_to_Lead.csv" and so on since they have the same initial file name.
library(dplyr)
library(readr)

# Load the datasets
lead_data <- read.csv("Human_exposure_to_Lead.csv")
mercury_data <- read_csv("Human_exposure_to_Mercucy.csv")
cadmium_data <- read.csv("Human_exposure_to_Cadmium.csv")
bisphenol_data <- read.csv("Human_exposure_to_Bisphenol.csv")

# Rename columns
col_names <- c("Year", "3 to 5 years", "6 to 11 years", "12 to 19 years", 
               "20 to 39 years", "40 to 59 years", "60 to 79 years", "Women", "Men")
colnames(lead_data) <- col_names
colnames(mercury_data) <- col_names
colnames(cadmium_data) <- col_names
colnames(bisphenol_data) <- col_names

# List to hold datasets
datasets <- list(
  Lead = lead_data,
  Mercury = mercury_data,
  Cadmium = cadmium_data,
  Bisphenol = bisphenol_data
)

# Population weights based on the age distribution provided
population_weights <- c(
  "3 to 5 years" = 7.50 / 57.44,
  "6 to 11 years" = 7.50 / 57.44,
  "12 to 19 years" = 7.50 / 57.44,
  "20 to 39 years" = 11.37 / 57.44,
  "40 to 59 years" = 10.12 / 57.44,
  "60 to 79 years" = 7.57 / 57.44
)

# Function to calculate population-weighted average for each dataset
calculate_weighted_average <- function(data, weights) {
  # Convert numeric columns to numeric type
  numeric_cols <- c("3 to 5 years", "6 to 11 years", "12 to 19 years", 
                    "20 to 39 years", "40 to 59 years", "60 to 79 years")
  data[numeric_cols] <- lapply(data[numeric_cols], function(x) as.numeric(as.character(x)))
  
  # Calculate means for each column
  means <- sapply(data[numeric_cols], mean, na.rm = TRUE)
  
  # Calculate weighted average
  weighted_average <- sum(means * weights[names(means)], na.rm = TRUE)
  return(weighted_average)
}

# Calculate weighted averages for all toxins
weighted_averages <- sapply(datasets, calculate_weighted_average, weights = population_weights)

# Create a data frame for the results
results <- data.frame(
  Toxin = names(weighted_averages),
  Weighted_Average_Concentration = round(weighted_averages, 2)
)

# Calculate proportions
results$Proportion <- round(results$Weighted_Average_Concentration / sum(results$Weighted_Average_Concentration) * 100, 2)

# Save results to a CSV file
write.csv(results, "Human_Body_clean.csv", row.names = FALSE)
