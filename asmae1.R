install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)

new_data_set <- read_excel("Documents/School/Jaar 1/Periode 6/Programming for economists/new data set.xlsx", 
                           skip = 3)

# Remove columns starting with "..."
new_data_set <- new_data_set[, !grepl("^\\.\\.\\.", names(new_data_set))]

# Check the result
head(new_data_set)
write.csv(new_data_set, "dataset.csv") #last step when saving

write.csv(new_data_set, "new_data_set111/new_dataset.csv") #last step when saving