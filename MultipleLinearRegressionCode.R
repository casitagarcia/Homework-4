setwd("~/Desktop")

TravelData <- read.csv("MilesTraveledandTravelTime.csv", header=TRUE)

x <- TravelData$x..Miles.Traveled
y <- TravelData$y.Travel.Time..hours.

xy <- x*y
xy

sumxy <- sum(xy)
sumxy

sumx <- sum(x)
sumx

sumy <- sum(y)
sumy

xsquared <- x*x
xsquared

sumxsquared <- sum(xsquared)
sumxsquared

sumx<- sum(x)
sumx

sumofxsquared <- sumx*sumx
sumofxsquared

ysquared <- y*y
sumysquared <- sum(ysquared)
sumysquared

sumofysquared <- sumy*sumy
sumofysquared

numerator_of_intercept <- (sumy)*(sumxsquared) - (sumx)*(sumxy)
numerator_of_intercept

denominator_of_intercept <- 10*(sumxsquared)-(sumofxsquared)
denominator_of_intercept

intercept <- numerator_of_intercept/denominator_of_intercept
intercept


numerator_beta <- 10 * (sumxy) - (sumx)*(sumy)
numerator_beta

denominator_beta <- 10 * (sumxsquared)- (sumofxsquared)
denominator_beta

beta <- numerator_beta / denominator_beta
beta



##Another way to do this with modeling a simple linear regression
hist(y)

x <- TravelData$x..Miles.Traveled
y <- TravelData$y.Travel.Time..hours.



model1 <- lm(y ~ x, data=TravelData)
model1


##Another way is to perform simple linear regression in excel
#go to excel and open data



#How to do multiple linear regression in R
setwd("~/Desktop/Management379_Fall2020_Section2/Linear Regression")

TravelData2 <- read.csv("MilesTraveledandTravelTime2.csv", header=TRUE)

names(TravelData2)

y <- TravelData2$y.Travel.Time..hours.

x1 <- TravelData2$x..Miles.Traveled

x2 <- TravelData2$Number.of.Deliveries


hist(y)




model2 <- lm(y ~ x1 + x2, data=TravelData2)
model2

summary(model2)

#Assessing fit of model
plot(model2)
