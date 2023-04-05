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
## first will be the size of the engine, i'll be running a anova test
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
cars1$CCgroup[cars1$CC >=1300] <- "group1"
cars1$CCgroup[cars1$CC >=1400] <- "group2"
cars1$CCgroup[cars1$CC >=1500] <- "group3"
cars1$CCgroup[cars1$CC >=1600] <- "group4"
cars1$CCgroup[cars1$CC >=1800] <- "group5"
cars1$CCgroup[cars1$CC >=1900] <- "group6"
cars1$CCgroup[cars1$CC >=2000] <- "group7"
table(cars1[[9]])

#i will now run a bartlett's test
bartlett.test(PriceLOG ~ CCgroup, data=cars1)

#the p value is less then .05 which means that this has violated the assumption of homogeneity of variance.

carCCANOVA <- lm(Price ~ CCgroup, data=cars1)
Anova(carCCANOVA, Type="II", white.adjust=TRUE)

pairwise.t.test(cars1$Price, cars1$CCgroup, p.adjust="bonferroni", pool.sd = FALSE)
#after running the tests, it shows that group1 had the most significant differnces between groups 2,4,5,6, and 7

carCCMeans <- cars1 %>% group_by(CCgroup) %>% summarize(Mean = mean(Price))
carCCMeans
#after looking at the average price between the groups, group1 had the lowest, while group 5 had the highest. with these two groups being the only ones standing out, i can assome that the size of the enging did not have too big of an inpact on the price of the cars

##now i will run another anova test but on the HP.

table(cars1['HP'])
cars1$HPgroup <- NA
cars1$HPgroup[cars1$HP >=60] <- "group1"
cars1$HPgroup[cars1$HP >=70] <- "group2"
cars1$HPgroup[cars1$HP >=80] <- "group3"
cars1$HPgroup[cars1$HP >=90] <- "group4"
cars1$HPgroup[cars1$HP >=100] <- "group5"
cars1$HPgroup[cars1$HP >=110] <- "group6"
cars1$HPgroup[cars1$HP >=190] <- "group7"
table(cars1['HPgroup'])

bartlett.test(PriceLOG ~ HPgroup, data=cars1)
#the p value is less then 0.5 making it significant, which means that this has violated the assumption of homogeneity of variance.

carHPANOVA <- lm(Price ~ HPgroup, data=cars1)
Anova(carHPANOVA, Type="II", white.adjust=TRUE)

pairwise.t.test(cars1$Price, cars1$HPgroup, p.adjust="bonferroni", pool.sd = FALSE)

#after running the test, i can see that group7 has significant against all the other groups, perhaps this group as a bigger inpact on the price?

carHPMeans <- cars1 %>% group_by(HPgroup) %>% summarize(Mean = mean(Price))
carHPMeans
#group 7 does have the highest avarage price compared to the other groups, with group4 being second highest, and group6 being third highest, but not too much of a difference between the rest of the groups.
#i can assume that the HP of a car has little inpact the price of the car, but more then the CC

##now i'm going to see what has a bigger affact on the price of the car
#i'll be using a stepwise regression modle to see how it looks
#frst will be backward elimination

carsFitAll = lm(Price ~ ., data = cars)
summary(carsFitAll)

step(carsFitAll, direction = 'backward')
carsfitsome= lm(Price ~ Age + KM + FuelType + HP + Automatic + CC + 
                     Weight, data = cars)
summary(carsfitsome)
#this modle is 86% accaret, but i will still look at the other stepwise regression modles
#nest will be forward selection

carsfitstart = lm(Price ~ 1, data = cars)
summary(carsfitstart)

step(carsfitstart, direction = 'forward', scope = (formula(carsFitAll)))

carsfitsome2= lm(Price ~ Age + Weight + KM + HP + CC + FuelType + Automatic, data = cars)
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