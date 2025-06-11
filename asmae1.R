#install.packages("readxl")
#install.packages("dplyr")
#install.packages('tidyr')
#install.packages("writexl")
library(readxl)
library(dplyr)
library(writexl)

#new_data_set <- read_excel("Documents/School/Jaar 1/Periode 6/Programming for economists/new data set.xlsx", skip = 3)

# Read dataset (adjust path if needed)
new_dataset <- read_csv("new_dataset.csv")

cat("Column names:\n")
cat(names(new_dataset), sep = "\n")

# Find columns to remove by exact names or patterns matching your columns
start_col <- grep("Three-year average 2016-2018", names(new_dataset))
end_col   <- grep("Three-year average 2004-2006", names(new_dataset))

if (length(start_col) == 0 | length(end_col) == 0) {
  stop("Start or end column not found. Check the column name formatting.")
}

if (start_col > end_col) {
  tmp <- start_col
  start_col <- end_col
  end_col <- tmp
}

# Remove columns from start_col to end_col
new_dataset <- new_dataset[, -c(start_col:end_col)]

# Remove rows 5 to 57 (after column removal)
if (nrow(new_dataset) >= 57) {
  new_dataset <- new_dataset[-(5:57), ]
} else {
  warning("Dataset has fewer than 57 rows; cannot remove rows 5 to 57.")
}
new_dataset <- new_dataset[-c(54, 55), ]


print(new_dataset)
rm(cut_column, end_col, start_col, vars_to_remove)


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

# View the reshaped dataset
View(long_new_dataset)

library(ggplot2)
library(dplyr)

# Make sure you're working with the correct dataset
# We'll call it `data`, as you used in your earlier step
# This assumes it has columns: "State" and "Percent in poverty"

# Check structure
str(data)

# Summarize average poverty by state
avg_poverty_by_state <- data %>%
  group_by(State) %>%
  summarise(Average_Poverty = mean(`Percent in poverty`, na.rm = TRUE)) %>%
  arrange(desc(Average_Poverty))

# Basic barplot
p <- ggplot(avg_poverty_by_state, aes(x = reorder(State, -Average_Poverty), y = Average_Poverty)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Average Poverty Rate by State",
    x = "State",
    y = "Average % in Poverty"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Show the plot
print(p)



# Check the result
write.csv(new_dataset, "dataset.csv") #last step when saving, 