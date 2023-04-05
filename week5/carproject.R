library("dplyr")
library("tidyr")
#week1
# step1 viewing the data and seeing what needs to be dropped

#dropping any missing data

cars <- na.omit(Car.Prices.Data)



names(cars)


View(cars)
str(cars)
#dropping any unneeded columns

cars1 <- cars %>% select(-c("MetColor", "Automatic", "Doors", "FuelType" ))

View(cars1)

str(cars1)

#going to make a colum with miles
cars1$Miles <- cars1$KM*0.621371
#going to drop the deimals
cars1$Miles <- round(cars1$Miles, digits = 0)

cars2 <- cars1%>% select(-c("KM"))

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
ggplot(cars1, aes(Miles)) + geom_histogram( ) 

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
## first will be the size of the engine, i'll be running a anova test
#will be using cars2 table
#will need to make price a numeric value
cars2$Price <- as.numeric(cars1$Price)

#will see if the data needs to be normalized first

plotNormalHistogram(cars2$Price)
#this is positively skewed, will first try and sqrt then log

cars2$PriceSQRT <- sqrt(cars2$Price)
plotNormalHistogram(cars1$PriceSQRT)

cars2$PriceLOG <- log(cars2$Price)
plotNormalHistogram(cars2$PriceLOG)
#the LOG function looks more normalized so i wil be using it

#will need to catorize the data in CC into another colome

table(cars2['CC'])
cars2$CCgroup <- NA
cars2$CCgroup[cars2$CC >=1300] <- "group1"
cars2$CCgroup[cars2$CC >=1400] <- "group2"
cars2$CCgroup[cars2$CC >=1500] <- "group3"
cars2$CCgroup[cars2$CC >=1600] <- "group4"
cars2$CCgroup[cars2$CC >=1800] <- "group5"
cars2$CCgroup[cars2$CC >=1900] <- "group6"
cars2$CCgroup[cars2$CC >=2000] <- "group7"
table(cars2[[9]])

#i will now run a bartlett's test
bartlett.test(PriceLOG ~ CCgroup, data=cars2)

#the p value is less then .05 which means that this has violated the assumption of homogeneity of variance.

carCCANOVA <- lm(Price ~ CCgroup, data=cars2)
Anova(carCCANOVA, Type="II", white.adjust=TRUE)

pairwise.t.test(cars1$Price, cars2$CCgroup, p.adjust="bonferroni", pool.sd = FALSE)
#after running the tests, it shows that group1 had the most significant differnces between groups 2,4,5,6, and 7

carCCMeans <- cars2 %>% group_by(CCgroup) %>% summarize(Mean = mean(Price))
carCCMeans
#after looking at the average price between the groups, group1 had the lowest, while group 5 had the highest. with these two groups being the only ones standing out, i can assome that the size of the enging did not have too big of an inpact on the price of the cars

##now i will run another anova test but on the HP.

table(cars2['HP'])
cars2$HPgroup <- NA
cars2$HPgroup[cars2$HP >=60] <- "group1"
cars2$HPgroup[cars2$HP >=70] <- "group2"
cars2$HPgroup[cars2$HP >=80] <- "group3"
cars2$HPgroup[cars2$HP >=90] <- "group4"
cars2$HPgroup[cars2$HP >=100] <- "group5"
cars2$HPgroup[cars2$HP >=110] <- "group6"
cars2$HPgroup[cars2$HP >=190] <- "group7"
table(cars2['HPgroup'])

bartlett.test(PriceLOG ~ HPgroup, data=cars2)
#the p value is less then 0.5 making it significant, which means that this has violated the assumption of homogeneity of variance.

carHPANOVA <- lm(Price ~ HPgroup, data=cars2)
Anova(carHPANOVA, Type="II", white.adjust=TRUE)

pairwise.t.test(cars1$Price, cars2$HPgroup, p.adjust="bonferroni", pool.sd = FALSE)

#after running the test, i can see that group7 has significant against all the other groups, perhaps this group as a bigger inpact on the price?

carHPMeans <- cars2 %>% group_by(HPgroup) %>% summarize(Mean = mean(Price))
carHPMeans
#group 7 does have the highest avarage price compared to the other groups, with group4 being second highest, and group6 being third highest, but not too much of a difference between the rest of the groups.
#i can assume that the HP of a car has little inpact the price of the car, but more then the CC

##now i'm going to see what has a bigger affact on the price of the car
#i'll be using a stepwise regression modle to see how it looks
#frst will be backward elimination

cars4 <- cars
cars4$Miles <- cars4$KM*0.621371
cars4$Miles <- round(cars4$Miles, digits = 0)

cars4 <- cars4%>% select(-c("KM"))

carsFitAll = lm(Price ~ ., data = cars4)
summary(carsFitAll)

step(carsFitAll, direction = 'backward')
carsfitsome= lm(Price ~ Age + FuelType + HP + Automatic + CC + Weight + 
                  Miles, data = cars4)
summary(carsfitsome)
#this modle is 86% accaret, but i will still look at the other stepwise regression modles
#nest will be forward selection

carsfitstart = lm(Price ~ 1, data = cars4)
summary(carsfitstart)

step(carsfitstart, direction = 'forward', scope = (formula(carsFitAll)))

carsfitsome2= lm(Price ~ Age + Weight + Miles + HP + CC + FuelType + Automatic, data = cars4)
summary(carsfitsome2)
#this modle is 86% accerite. same as the other modle, but will still look at one more before disiding which modle is better
#now will be doing a hybrid stepwise modle

carshybridstep = step(carsfitstart, direction="both", scope=formula(carsFitAll))
summary(carshybridstep)
#this modle is also 86% accaret. will take a closer look at the 3 modles to see which one i'll go with

summary(carsfitsome)
summary(carsfitsome2)
summary(carshybridstep)
#on closer look, they all are the same modle, so i'll be going with the hybrid modle since it is a combination of the first two
#after looking at the modle, age, weight, km, and hp all have the same p value, and will mostly like have a bigger inpact on the prise of the car.

##week4

#i'll be making some visuals that can be used for a presentation

#i'll first see how the age of the car looks compared to the price
ggplot(cars4, aes(x=Price, y=Age))+ geom_point()
ggplot(cars4, aes(x=Price, y=Age))+ geom_point()+geom_smooth(method=lm)
ggplot(cars4, aes(x=Price, y=Age))+ geom_point()+geom_smooth(method=lm, se=FALSE)
ggplot(cars4, aes(x=Price, y=Age))+ geom_point()+geom_smooth()

#next i'll see the miles on the car compated to the age
ggplot(cars4, aes(x=Price, y=Miles))+ geom_point()
ggplot(cars4, aes(x=Price, y=Miles))+ geom_point()+geom_smooth(method=lm)
ggplot(cars4, aes(x=Price, y=Miles))+ geom_point()+geom_smooth(method=lm, se=FALSE)
ggplot(cars4, aes(x=Price, y=Miles))+ geom_point()+geom_smooth()

#i'll next look at a histogram for the price of the cars
ggplot(cars4, aes(Price)) + geom_histogram( ) 
#next will the a histogram of the miles of the cars
ggplot(cars4, aes(Miles)) + geom_histogram( ) 
#next will be a histogram for the age of the cars
ggplot(cars4, aes(Age)) + geom_histogram( ) 

#i beleve the scatter plot graph will look better for showing how the price looks againt age and miles

