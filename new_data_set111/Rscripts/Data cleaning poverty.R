#This Rscript explains how we cleaned the data on the file poverty

#install.packages("readxl")
#install.packages("dplyr")
#install.packages('tidyr')
#install.packages("writexl")
library(readxl)
library(dplyr)
library(writexl)

# Load the dataset
final_income_data <- read_csv("new_dataset.csv")

# Reshape the dataset using pivot_longer
long_new_dataset <- final_income_data %>%
  pivot_longer(
    cols = 3:ncol(final_income_data),
    names_to = "year",
    values_to = "Percent in poverty"
  )

# View the reshaped dataset
View(long_new_dataset)
long_new_dataset = as.data.frame(long_new_dataset)
long_new_dataset$year = substr(long_new_dataset$year, 25, 28)

substr("Three-year average 2019-2021 (2)", 25, 28)
substr("Three-year average 2017-2019 (31)", 25, 28)
substr("Three-year average 2016-2018 (32)", 25, 28)
substr("Three-year average 2013-2015 (4)", 25, 28)
substr("Three-year average 2012-2014 (33)", 25, 28)
substr("Three-year average 2011-2013 (5)", 25, 28)
substr("Three-year average 2008-2010 (6)", 25, 28)
substr("Three-year average 2004-2006 (7)", 25, 28)

# Convert all columns except the first one to numeric
data <- long_new_dataset %>%
  mutate(across(-1, ~ as.numeric(.)))

# View result
str(data)

#dataset still had unwanted rows so i will remove them
# Find rows that contain "Source:" or "Footnotes" in any column
rows_to_remove <- apply(long_new_dataset, 1, function(row) {
  any(grepl("Source:|Footnotes", row))
})

# Filter out those rows
long_new_dataset <- long_new_dataset[!rows_to_remove, ]

# Reset row names
row.names(long_new_dataset) <- NULL
#removing redundant years
years_to_remove <- c("2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008", "2007", "2006")

# Identify rows where any cell contains one of these years
rows_to_remove <- apply(long_new_dataset, 1, function(row) {
  any(row %in% years_to_remove)
})

# Filter out those rows
long_new_dataset <- long_new_dataset[!rows_to_remove, ]

# Reset row names if you want 
row.names(long_new_dataset) <- NULL

# Check the result
print(long_new_dataset)

# View the reshaped dataset
View(long_new_dataset)