#Creating 2 new variables
library(dplyr)
library(tidyr)

# Rename 'State' to 'Location' so it matches combined_race
long_new_dataset <- long_new_dataset %>%
  rename(Location = State) %>%
  mutate(
    PovertyRate = as.numeric(PovertyRate),
    Year = as.integer(Year)
  )

combined_race <- combined_race %>%
  mutate(
    Percentage = as.numeric(Percentage),
    Year = as.integer(Year)
  )

# Join on Location and Year
poverty_race <- left_join(combined_race, long_new_dataset, by = c("Location", "Year"))

# Remove unnecessary column
poverty_race <- poverty_race %>% select(-`...1`)




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


#---------------------------------
#for these 2 variables, The Average Change shows overall trends (before, during, and after COVID).
# and The Poverty Rise Rate After Covid isolates the COVID impact specifically. 