#Plotting the plot for spatial variation
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