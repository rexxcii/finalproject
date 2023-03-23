library("dplyr")
library("tidyr")
#week1
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
library(ggplot2)
library (lattice)

#week2
#first gonna look though some histographs.

ggplot(cars1, aes(Price)) + geom_histogram( ) 
ggplot(cars1, aes(Age)) + geom_histogram() 
ggplot(cars1, aes(KM)) + geom_histogram( ) 
ggplot(cars1, aes(HP)) + geom_histogram( ) 
ggplot(cars1, aes(CC)) + geom_histogram( ) 
ggplot(cars1, aes(Weight)) + geom_histogram( ) 

#now i'm going to compare CC(size of the engine) to HP (horse power)
#first i'll try a scatter plot

ggplot(cars1, aes(x=CC, y=HP))+ geom_point()
#at first look, it doens't look like the size of the engine really affects the horse power

#going to try and compare how CC affects price
ggplot(cars1, aes(x=Price, y=CC))+ geom_point()
#doesn't look like CC has a that big of an impact on price

#going to see how HP affects price now
ggplot(cars1, aes(x=Price, y=HP))+ geom_point()+ coord_flip()
#looks like HP does have a small impact on the price, as the HP goes up, the range of the price does up slightly.

#now i'm going to look how age affects price
ggplot(cars1, aes(x=Price, y=Age))+ geom_point()
ggplot(cars1, aes(x=Price, y=Age))+ geom_point()+geom_smooth(method=lm)
ggplot(cars1, aes(x=Price, y=Age))+ geom_point()+geom_smooth(method=lm, se=FALSE)
ggplot(cars1, aes(x=Price, y=Age))+ geom_point()+geom_smooth()
#looks like age of the car does have a big effect on the price, where the older the car, the lower the price is

#now i'm going to look how KM affects price
ggplot(cars1, aes(x=Price, y=KM))+ geom_point()
ggplot(cars1, aes(x=Price, y=KM))+ geom_point()+geom_smooth(method=lm)
ggplot(cars1, aes(x=Price, y=KM))+ geom_point()+geom_smooth(method=lm, se=FALSE)
ggplot(cars1, aes(x=Price, y=KM))+ geom_point()+geom_smooth()
#looks like the KM does have an impact on the price, where the higher the KM the lower the price

#looks like the only ones that have a big impact on price is KM and Age, so i'll be using them for my next tests.

#week3 

#i'll be doing some data analicies and run some tests

#starting with my first hypothosis, where wether the size of the engine was affected by the horse power
library("gmodels")
#going to run an independed chi square

CrossTable(cars1$HP, cars1$CC, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#the p value is 0 so it is not significant, so best to assume that the size of the engine is not affected by the horse power

#since the horse power does not affect the size of the engine, i'll try running two different tests to see how each affect the price of the car
library("rcompanion")
library("car")
#first will be the size of the engine, i'll be running a anova test
#will need to make price a numeric value
cars1$Price <- as.numeric(cars1$Price)

#will see if the data needs to be normalized first

plotNormalHistogram(cars1$Price)
#this is positively skewed, will first try and sqrt then log

cars1$PriceSQRT <- sqrt(cars1$Price)
plotNormalHistogram(cars1$PriceSQRT)

cars1$PriceLOG <- log(cars1$Price)
plotNormalHistogram(cars1$PriceLOG)
#the LOG function looks more normalized so i wil be using it

#will need to catorize the data in CC into another colome

table(cars1['CC'])
cars1$CCgroup <- NA
cars1$CCgroup[cars1$CC ==1300] <- "1"
cars1$CCgroup[cars1$CC ==1332] <- "2"
cars1$CCgroup[cars1$CC ==1398] <- "3"
cars1$CCgroup[cars1$CC ==1400] <- "4"
cars1$CCgroup[cars1$CC ==1587] <- "5"
cars1$CCgroup[cars1$CC ==1598] <- "6"
cars1$CCgroup[cars1$CC ==1600] <- "7"
cars1$CCgroup[cars1$CC ==1800] <- "8"
cars1$CCgroup[cars1$CC ==1900] <- "9"
cars1$CCgroup[cars1$CC ==1975] <- "10"
cars1$CCgroup[cars1$CC ==1995] <- "11"
cars1$CCgroup[cars1$CC ==2000] <- "12"
#i will now run a bartlett's test
bartlett.test(PriceLOG ~ CC, data=cars1)

fligner.test(PriceLOG ~ CCgroup, data=cars1)

