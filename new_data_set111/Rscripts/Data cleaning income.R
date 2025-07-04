#This rscript shows the data cleaning on the income file
#'''{r}
library(readxl) 
income_data <- read_excel("Documents/GitHub/obesity-and-poverty/new_data_set111/income data.xlsx")

#''''

# Remove columns starting with "..."
income_data <- income_data [, !grepl("^\\.\\.\\.", names(income_data))]

# Check the result
head(income_data)
write.csv(income_data, "income.csv")

write.csv(income_data, "new_data_set111/income.csv") #last step when saving

library(readxl)
library(dplyr)

data <- read_excel("new_data_set111/income data.xlsx", sheet = "h08b", skip = 8)

# View column names
colnames(data)

# Keep only columns from 2017 onward by position
# Let's say each year has 2 columns: income and margin of error
# And the most recent year is 2023, so:
# Columns are ordered like: 2023_income, 2023_margin, 2022_income, 2022_margin, ..., 2000_income, 2000_margin
library(tidyverse)
# Remove columns for 2016 and earlier (e.g., 14 columns * 2 = 28 columns to drop)
income_data %>%
  select_if(str_detect(names(income_data), "..."))


bad_colnums = seq(from = 3, ncol(income_data), by = 2)

income_data_new = income_data[, setdiff(1:ncol(income_data), bad_colnums)]

head(data_filtered)

# remove older years
income_data_new <- income_data_new %>% select(State, `2021-2023`, `2020-2022`, `2019-2021`, `2018-2020 (41)`, `2017-2019 (40)`)
write_csv(income_data_new, "new_data_set111/final_income_data.csv")

# Load the dataset
final_income_data <- read_csv("new_data_set111/final_income_data.csv")

# Reshape the dataset using pivot_longer
long_income_data <- final_income_data %>%
  pivot_longer(
    cols = -State,
    names_to = "Period",
    values_to = "Median_Income"
  )

# View the reshaped dataset
View(long_income_data)
long_income_data = as.data.frame(long_income_data)
long_income_data[, "Period"] = substr(long_income_data[, "Period"], 6, 9)

substr("2020-2023 (40)", 6, 9)
substr("2017-2019 (40)", 6, 9)
substr("2018-2020 (41)", 6, 9)

# Convert all columns except the first one to numeric
data <- long_income_data %>%
  mutate(across(-1, ~ as.numeric(.)))

# View result
str(data)