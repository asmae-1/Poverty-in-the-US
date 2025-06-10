new_data_set <- read_excel("Documents/School/Jaar 1/Periode 6/Programming for economists/new data set.xlsx", 
                           skip = 3)

write.csv(new_data_set, "dataset.csv") #last step when saving