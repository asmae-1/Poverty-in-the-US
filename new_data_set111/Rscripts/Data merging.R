#Merging the 2 datasets income and poverty

# Check column names to find common ones
names(long_new_dataset)
names(long_income_data)
#checking if year and period have the same format
str(long_new_dataset$year)
str(long_income_data$Period)

#finally cleaning both files
# Remove rows where 'State' equals the column header string or where the value column equals the header string

# For long_new_dataset
long_new_dataset_clean <- long_new_dataset %>%
  filter(State != "State", `Percent in poverty` != "Percent in poverty")

# For long_income_data
long_income_data_clean <- long_income_data %>%
  filter(State != "State", Median_Income != "Median income")
# Convert year to numeric
long_new_dataset_clean$year <- as.numeric(long_new_dataset_clean$year)
long_income_data_clean$year <- as.numeric(long_income_data_clean$year)

# Convert value columns to numeric
long_new_dataset_clean$`Percent in poverty` <- as.numeric(long_new_dataset_clean$`Percent in poverty`)
long_income_data_clean$Median_Income <- as.numeric(long_income_data_clean$Median_Income)

#merging the datasets
merged_data <- long_new_dataset_clean %>%
  inner_join(long_income_data_clean, by = c("State", "year"))
#checking merged data
head(merged_data)
View(merged_data)

#last step , removing column starting with ... in merged data
merged_data <- merged_data %>% select(-`...1`)
head(merged_data)
#putting merged data in github
write.csv(merged_data, "merged_data.csv", row.names = FALSE) 