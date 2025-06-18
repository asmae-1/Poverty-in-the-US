install.packages(c("readxl", "dplyr", "tidyr"))
library(readxl)
library(dplyr)
library(tidyr)
y
df_raw <- read_xls("data/poverty 2017- 2018 .xls", sheet = 1, skip = 5)
names(df_raw)[1:13] <- c(
  "Group", "Pop_2017", "PovNum_2017", "MoE_2017", "PovRate_2017", "MoE_Rate_2017",
  "Pop_2018", "PovNum_2018", "MoE_2018", "PovRate_2018", "MoE_Rate_2018",
  "Change_Num", "Change_Rate"
)
df_raw <- df_raw %>%
  filter(!is.na(Group))
df_long <- df_raw %>%
  select(Group, PovRate_2017, PovRate_2018) %>%
  pivot_longer(cols = starts_with("PovRate"),
               names_to = "Year",
               values_to = "PovertyRate") %>%
  mutate(Year = ifelse(Year == "PovRate_2017", 2017, 2018))


library(readxl)
poverty_2021_2022 <- read_excel("data/poverty 2021-2022.xlsx")

df_raw2 <- read_excel("data/poverty 2021-2022.xlsx", sheet = "Table 1", skip = 5)



