###Line plot for COVID-19

# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)

# Read the dataset
data <- read_csv("long_new_dataset.csv")

# Filter for United States data and years 2019-2023
us_data <- data %>%
  filter(State == "United States", year %in% 2019:2023) %>%
  mutate(poverty = as.numeric(`Percent in poverty`))

# Poverty Rate Line Plot
ggplot(us_data, aes(x = year, y = poverty)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 2020, y = 12, label = "COVID-19", color = "red", vjust = -0.5, fontface = "bold") +
  labs(title = "Average Poverty Rate in the United States (2019–2023)",
       x = "Year",
       y = "Poverty Rate (%)") +
  ylim(11, 12) +
  theme_minimal()