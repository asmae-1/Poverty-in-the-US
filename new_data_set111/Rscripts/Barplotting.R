#barplots for variably 1 and 2
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