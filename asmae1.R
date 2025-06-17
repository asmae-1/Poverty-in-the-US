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

#here i am uploading my modified dataset to github
setwd("~/Documents/GitHub/Poverty-in-the-US")
write_csv(long_new_dataset, "new_data_set111/long_new_dataset.csv")
write.csv(long_new_dataset, "long_new_dataset.csv", row.names = FALSE)

#here i am loading suhani's rscript into my rstudio to continue plotting
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
download.file(
  url = "https://raw.githubusercontent.com/asmae-1/Poverty-in-the-US/refs/heads/main/suhnai.R",
  destfile = "suhnai.R",
  mode = "wb"
)
file.edit("suhnai.R")

#here i am loading in suhani's modified file into my rstudio
download.file(
  url = "https://raw.githubusercontent.com/asmae-1/Poverty-in-the-US/main/new_data_set111/long_income_data.csv",
  destfile = "long_income_data.csv"
)

long_income_data <- read.csv("long_income_data.csv")

View(long_income_data)

#here i am merging both datasets, by first checking similair column names
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
#checking the data

#final merge
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

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

#plotting
#install.packages("plotly")
#install.packages("htmlwidgets")
library(htmlwidgets)
library(tidyverse)
library(ggplot2)
library(plotly)

merged_data
#checking column names in merged data
colnames(merged_data)

library(dplyr)
library(ggplot2)

# Adding COVID period column (before vs after 2020)
merged_data <- merged_data %>%
  mutate(COVID_period = ifelse(year < 2020, "Before COVID", "After COVID"))

# Basic ggplot with color by COVID_period and text for hover
p <- ggplot(merged_data, aes(x = `Percent in poverty`, y = Median_Income, 
                             color = COVID_period,
                             text = paste("State:", State, "<br>",
                                          "Year:", year, "<br>",
                                          "Poverty %:", `Percent in poverty`, "<br>",
                                          "Income:", Median_Income))) +
  geom_point(alpha = 0.7) +
  ggtitle("Percent in Poverty vs Median Income Before and After COVID") +
  xlab("Percent in Poverty") +
  ylab("Median Income") +
  theme_minimal() +
  scale_color_manual(values = c("Before COVID" = "blue", "After COVID" = "red"))

# Convert to interactive plotly plot with hover text
ggplotly(p, tooltip = "text")


#Plot poverty overtime before and after corona
#install.packages("usmap")
library(ggplot2)
library(dplyr)
library(usmap)
library(readr)

# Load data
merged_data <- read_csv("/Users/asmaenafid/Documents/GitHub/Poverty-in-the-US/new_data_set111/merged_data.csv")

# Prepare the data: exclude national total, add COVID_period, rename for usmap
map_data <- merged_data %>%
  filter(State != "United States") %>%
  mutate(COVID_period = ifelse(year < 2020, "Before COVID", "After COVID")) %>%
  rename(state = State)

# Plot the faceted map
plot_usmap(data = map_data, regions = "states", values = "Percent in poverty") +
  facet_wrap(~COVID_period) +
  scale_fill_continuous(name = "Poverty %", low = "white", high = "darkred") +
  theme(legend.position = "right") +
  labs(title = "Spatial Variation in Poverty Rate by State",
       subtitle = "Before vs. After COVID (All Years Included)")

#plotting median income before and after covid
#reusing the same structure, just changing the value column
income_map_data <- merged_data %>%
  filter(State != "United States") %>%
  mutate(COVID_period = ifelse(year < 2020, "Before COVID", "After COVID")) %>%
  rename(state = State)

# Plot median income with spatial variation
plot_usmap(data = income_map_data, regions = "states", values = "Median_Income") +
  facet_wrap(~COVID_period) +
  scale_fill_continuous(name = "Median Income", low = "white", high = "darkgreen") +
  theme(legend.position = "right") +
  labs(title = "Spatial Variation in Median Income by State",
       subtitle = "Before vs. After COVID (All Years Included)")

#plotting combined plots in one
library(tidyr)
library(ggplot2)
library(dplyr)
library(usmap)
library(readr)
# Prepare data
combined_map_data <- merged_data %>%
  filter(State != "United States") %>%
  mutate(COVID_period = ifelse(year < 2020, "Before COVID", "After COVID")) %>%
  rename(state = State) %>%
  select(state, year, COVID_period, `Percent in poverty`, Median_Income) %>%
  pivot_longer(cols = c(`Percent in poverty`, Median_Income),
               names_to = "Indicator",
               values_to = "Value") %>%
  mutate(Indicator = recode(Indicator,
                            `Median_Income` = "Median Income",
                            `Percent in poverty` = "Percent in Poverty")) %>%
  group_by(Indicator) %>%
  mutate(Value_norm = scales::rescale(Value, to = c(0,1))) %>%
  ungroup()

# Plot normalized values with consistent color scale
plot_usmap(data = combined_map_data, values = "Value_norm") +
  facet_grid(Indicator ~ COVID_period) +
  scale_fill_continuous(name = "", low = "white", high = "darkred") +
  theme(legend.position = "right") +
  labs(title = "Spatial Variation of Median Income and Poverty Rate by State",
       subtitle = "Before vs. After COVID")

#----------------------------------------------- new dataset unemployment rates/////MIGHT NOT NEED THIS
library(readxl)
library(dplyr)

# Read the Excel file and skip the first 6 rows directly
data_unemployed <- read_excel("annavg.xlsx", skip = 6)

# Remove unwanted rows (row 59 and 60 *after* skipping 6 rows = rows 53 and 54)
data_unemployed <- data_unemployed[-c(53, 54), ]

# Identify columns to remove:
# - Columns: 2, 3
# - Every 3rd column starting from 4 to 16 (i.e., 4, 7, 10, 13, 16)
# - Columns 19 to 74 contain patterns: 
#   Rank columns: 6, 9, 12, ..., starting from col 6 to 74 by 3
#   Rate columns: 5, 8, 11, ..., also every 3rd from 5 to 74 by 3
#   Middle columns: 4, 7, 10, ..., also every 3rd from 4 to 74 by 3

# Generate the full vector of columns to remove
columns_to_remove <- sort(unique(c(
  2, 3,
  seq(4, 16, by = 3),
  seq(19, 74, by = 3),   # rank columns starting at 19
  seq(20, 75, by = 3),   # rate columns after those
  seq(21, 76, by = 3)    # following value column
)))

# Remove the identified columns
data_unemployed <- data_unemployed[, -columns_to_remove]

#removing last cells that are unwanted
data_unemployed <- data_unemployed %>% select(-any_of(c("Rank...6", "Rank...9", "Rank...12", "Rank...15", "Rank...18")))

#replacing column numbers with years and then converting to numeric
data_unemployed <- data_unemployed %>%
  rename(`2023` = `Rate...5`,
         `2022` = `Rate...8`,
         `2021` = `Rate...11`,
         `2020` = `Rate...14`,
         `2019` = `Rate...17`) %>%
  mutate(across(c(`2023`, `2022`, `2021`, `2020`, `2019`), ~ as.numeric(.)))

# View the cleaned dataset in a new tab
View(data_unemployed)

#last step, saving this file
write.csv(data_unemployed, "data_unemployed.csv", row.names = FALSE)
#----------------------------------------------------------END
library(dplyr)
library(tidyr)

# 1. Checking column names
colnames(combined_race)
colnames(long_new_dataset)

# Convert to numeric (if not already)
long_new_dataset <- long_new_dataset %>%
  mutate(
    PovertyRate = as.numeric(PovertyRate),
    Year = as.integer(Year)
  )

combined_race <- combined_race %>%
  mutate(
    Percentage = as.numeric(Percentage),
    Year = as.integer(Year)
  )

# Merge datasets by Location and Year
poverty_race <- left_join(combined_race, long_new_dataset, by = c("Location", "Year"))

#remove last unwanted column
poverty_race <- poverty_race %>% select(-`...1`)

# Check merged data
View(poverty_race)

#--------------------------- creating the plot of poverty_race
# Load required libraries
#install.packages("maps")
library(dplyr)
library(ggplot2)
library(sf)
library(maps)

# Step 1: Create a new variable to separate before and after COVID periods for whites
poverty_race <- poverty_race %>%
  mutate(
    CovidPeriod = ifelse(Year < 2020, "Before COVID", "After COVID")
  )

# Step 2: Prepare the US states map with lowercase state names
states_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  mutate(Location = tolower(ID))

# Step 3: Prepare data for plotting
map_data_ready <- poverty_race %>%
  filter(Race == "White") %>%        # Change race here if needed
  mutate(Location = tolower(Location)) %>%
  select(Location, PovertyRate, CovidPeriod)

# Step 4: Join map data with poverty data
states_map <- states_map %>%
  left_join(map_data_ready, by = "Location")

# Step 5: Plot maps with facets for Before and After COVID
ggplot(states_map) +
  geom_sf(aes(fill = PovertyRate), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  facet_wrap(~CovidPeriod) +
  labs(
    title = "Poverty Rate by State for White Population",
    fill = "Poverty Rate"
  ) +
  theme_minimal()

# For Hispanic
library(dplyr)
library(ggplot2)
library(sf)
library(maps)

# Prepare map data
states_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  mutate(Location = tolower(ID))

# Prepare poverty data for the selected race (example: Hispanic)
map_data_ready <- poverty_race %>%
  filter(Race == "Hispanic") %>%
  mutate(Location = tolower(Location),
         CovidPeriod = ifelse(Year < 2020, "Before COVID", "After COVID")) %>%
  select(Location, PovertyRate, CovidPeriod)

# Join and keep all combinations of Location and CovidPeriod for faceting
states_map_long <- states_map %>%
  inner_join(map_data_ready, by = "Location")

# Plot with facets
ggplot(states_map_long) +
  geom_sf(aes(fill = PovertyRate), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  facet_wrap(~CovidPeriod) +
  labs(
    title = "Poverty Rate by State for Hispanic Population",
    fill = "Poverty Rate"
  ) +
  theme_minimal()


# for black
map_data_ready <- poverty_race %>%
  filter(Race == "Black") %>%
  mutate(Location = tolower(Location),
         CovidPeriod = ifelse(Year < 2020, "Before COVID", "After COVID")) %>%
  select(Location, PovertyRate, CovidPeriod)

states_map_long <- states_map %>%
  left_join(map_data_ready, by = "Location")

ggplot(states_map_long) +
  geom_sf(aes(fill = PovertyRate), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  facet_wrap(~CovidPeriod) +
  labs(
    title = "Poverty Rate by State for Black Population",
    fill = "Poverty Rate"
  ) +
  theme_minimal()

#for asian
map_data_ready <- poverty_race %>%
  filter(Race == "Asian") %>%
  mutate(Location = tolower(Location),
         CovidPeriod = ifelse(Year < 2020, "Before COVID", "After COVID")) %>%
  select(Location, PovertyRate, CovidPeriod)

states_map_long <- states_map %>%
  left_join(map_data_ready, by = "Location")

ggplot(states_map_long) +
  geom_sf(aes(fill = PovertyRate), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  facet_wrap(~CovidPeriod) +
  labs(
    title = "Poverty Rate by State for Asian Population",
    fill = "Poverty Rate"
  ) +
  theme_minimal()






















