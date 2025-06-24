install.packages("renv")
renv::init() 
 

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
#@@@@@@@@@@@@@
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

View(merged_data)
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
#----------------------------------------------------
#spatial variation based on how poor each state is. THE FIRST graph. 
library(dplyr)
library(ggplot2)
library(maps)
library(sf)

# 1. Get US states map and convert to sf object
us_states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  rename(Location = ID) %>%
  mutate(Location = tolower(Location))  # ensure lowercase for matching

# 2. Prepare data: average PovertyRate per state
state_poverty <- poverty_race %>%
  select(Location, PovertyRate) %>%
  mutate(Location = tolower(Location)) %>%
  group_by(Location) %>%
  summarise(avg_poverty_rate = mean(PovertyRate, na.rm = TRUE))

# 3. Merge map data with poverty rates
map_data <- left_join(us_states, state_poverty, by = "Location")

# 4. Plot the map
ggplot(map_data) +
  geom_sf(aes(fill = avg_poverty_rate), color = "white") +
  scale_fill_gradient(
    low = "#f2e5ff", high = "#4b0082", na.value = "grey90",
    name = "Avg Poverty Rate"
  ) +
  labs(
    title = "Average Poverty Rate by U.S. State"
  ) +
  theme_minimal() 




#--------------------------------------------------------------- for the subpopulation bar plot
#1. new variable = average change in poverty rate
library(dplyr)

north_states <- c(
  "Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut",
  "New York", "New Jersey", "Pennsylvania"
)

east_states <- c("Delaware", "Maryland", "District of Columbia")

midwest_states <- c(
  "Illinois", "Indiana", "Iowa", "Michigan", "Minnesota", "Missouri", "Ohio", "Wisconsin"
)

south_states <- c(
  "Alabama", "Georgia", "Florida", "South Carolina", "North Carolina", "Virginia",
  "Kentucky", "Tennessee", "Mississippi", "Louisiana", "Arkansas",
  "West Virginia", "Oklahoma", "Texas"
)

west_states <- c(
  "California", "Oregon", "Washington", "Nevada", "Arizona", "Utah", "Colorado",
  "Idaho", "Montana", "Wyoming", "Alaska", "Hawaii", "New Mexico",
  "North Dakota", "South Dakota", "Nebraska", "Kansas"
)

poverty_race <- poverty_race %>%
  mutate(Region = case_when(
    Location %in% north_states ~ "North",
    Location %in% east_states ~ "East",
    Location %in% midwest_states ~ "Midwest",
    Location %in% south_states ~ "South",
    Location %in% west_states ~ "West",
    TRUE ~ "Other"
  ))

# Calculate the year-to-year change in poverty rate per race per state
# For each Location and Race, subtract the previous year's PovertyRate from the current year's
# change in poverty rate is calculated as ChangeInPovertyRate_t=PovertyRate_t−PovertyRate_t−1
poverty_race <- poverty_race %>%
  arrange(Location, Race, Year) %>%        # Order data by state, race, and year
  group_by(Location, Race) %>%              # Group by state and race to calculate changes within groups
  mutate(ChangeInPovertyRate = PovertyRate - lag(PovertyRate)) %>%  # Calculate difference from previous year
  ungroup()                                # Ungroup after calculation

#The average change in poverty rate per region is calculated by Grouping the data by Region 
# and Taking the mean of all ChangeInPovertyRate values within each region, ignoring missing values (NA).
library(dplyr)

avg_change_per_region <- poverty_race %>%
  group_by(Region) %>%
  summarise(avg_change = mean(ChangeInPovertyRate, na.rm = TRUE))

poverty_race <- poverty_race %>%
  left_join(avg_change_per_region, by = "Region") %>%
  rename(`Average Change Poverty Rate Per Region` = avg_change)

#removing all locations that are united states
poverty_race <- poverty_race %>%
  filter(Location != "United States")


#plotting variable 1
#---------------------------------------------------
#plotting sub population analysis for the newer graph (barplot) =Variable 1
#Barplot1: Average change in poverty rate per region
library(ggplot2)
library(dplyr)
ggplot(avg_change_per_region, aes(x = Region, y = avg_change, fill = Region)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Change in Poverty Rate per Region",
       x = "Region",
       y = "Average Change in Poverty Rate") +
  theme(legend.position = "none")




#Variable 2 = Poverty Rise Rate after COVID (%). 
## Calculate the % increase in poverty rise rate after COVID compared to before COVID per region
# Formula: ((Poverty Rise Rate After COVID - Poverty Rise Rate Before COVID) / Poverty Rise Rate Before COVID) * 100
# What: This measures how much more (or less) often poverty rates increased after COVID relative to before COVID, by region.
# note here thata. negative value is a decline in poverty
#creating the column
library(dplyr)
library(tidyr)

poverty_race <- poverty_race %>%
  group_by(Region, CovidPeriod) %>%
  summarise(AvgPovertyRate = mean(PovertyRate, na.rm = TRUE)) %>%
  pivot_wider(names_from = CovidPeriod, values_from = AvgPovertyRate) %>%
  mutate(PovertyRiseRateAfterCovid = ((`After COVID` - `Before COVID`) / `Before COVID`) * 100) %>%
  select(Region, PovertyRiseRateAfterCovid) %>%
  right_join(poverty_race, by = "Region")

#plotting barplot 2 for variable 2: Poverty Rise Rate After Covid grouped by Region:
library(ggplot2)
library(dplyr)

# Since PovertyRiseRateAfterCovid is repeated for each row in a region, 
# we just take the first value per region for plotting
plot_data <- poverty_race %>%
  group_by(Region) %>%
  summarise(PovertyRiseRateAfterCovid = first(PovertyRiseRateAfterCovid)) %>%
  ungroup()

# Plot
ggplot(plot_data, aes(x = Region, y = PovertyRiseRateAfterCovid, fill = Region)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Poverty Rise Rate After Covid by Region",
    x = "Region",
    y = "Poverty Rise Rate After Covid (%)"
  ) +
  theme(legend.position = "none")
#---------------------------------
#for these 2 variables, The Average Change shows overall trends (before, during, and after COVID).
# and The Poverty Rise Rate After Covid isolates the COVID impact specifically.

#----------------------temporal and event analyisis
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

# Read the dataset
library(readr)
merged_data <- read_csv("new_data_set111/merged_data.csv")

##now we do the line plot for median income 
# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)

##Filter for tehe US and the years 2019-2023
us_income_data <- merged_data %>%
  filter(State == "United States", year %in% 2019:2023)

# Median Income Line Plot
ggplot(us_income_data, aes(x = year, y = Median_Income)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "Median Income in the United States (2019–2023)",
       x = "Year",
       y = "Median Income (USD)") +
  scale_y_continuous(limits = c(72000, 84000), breaks = seq(72000, 84000, by = 1000)) +
  theme_minimal()





#END OF SCRIPT





#extra plots tat are not used
#--------------------------- 
#creating the plot of poverty_race of all races (these plots are not needed)
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





















