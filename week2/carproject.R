library("dplyr")
library("tidyr")

# step1 viewing the data and seeing what needs to be dropped

#dropping any missing data

cars <- na.omit(Car.Prices.Data)



names(cars)


View(cars)

#dropping any unneeded columns

cars1 <- cars %>% select(-c("MetColor", "Automatic", "Doors", "FuelType" ))

View(cars1)

str(cars1)

#now that i got what i need, i'll look tough the dataset and see what graphs i can make

