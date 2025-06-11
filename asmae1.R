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

print(new_dataset)

------------------------------------
# Check the result
head(new_dataset)
write.csv(new_data_set, "dataset.csv") #last step when saving, 
write.csv(new_data_set, "new_data_set111/new_dataset.csv") #idk if this works
