housePrices <- read.csv("House_prices_-_Exercise_7.csv", header = TRUE, sep =",", dec=".", fill=TRUE)
housePrices <- housePrices[,c(3,4,5,6,8,10,11,12,15)]

fitting <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + view +condition + 
             grade +yr_built, data = housePrices)
summary(fitting)

plot(fitted(fitting), residuals(fitting))


#b)
price <- housePrices$price
hist(price, freq = FALSE)
curve(dnorm(x, mean=mean(price), sd=sqrt(var(price))), col="darkblue", from = 0, to = 8e+6, add = TRUE)

logprice <- log(price)
hist(logprice, freq = FALSE)
curve(dnorm(x, mean=mean(logprice), sd=sqrt(var(logprice))), col="darkblue", from = 11, to = 16, add = TRUE)

qqplot(price, rnorm(seq(0,8e+6, by = 10000), mean=mean(price), sd=sqrt(var(price))))
abline(a=0,b=1)

qqplot(logprice, rnorm(seq(11,16, by = 0.01), mean=mean(logprice), sd=sqrt(var(logprice))))
abline(a=0,b=1)


logfit <- update(fitting, log(price) ~ bedrooms + bathrooms + sqft_living + floors + view +condition + 
                 grade +yr_built)
summary(logfit)

qqplot(logprice, housePrices$bedrooms)
qqplot(logprice, housePrices$bathrooms)
qqplot(logprice, housePrices$sqft_living)
qqplot(logprice, housePrices$floors)
qqplot(logprice, housePrices$view)
qqplot(logprice, housePrices$condition)
qqplot(logprice, housePrices$grade)
qqplot(logprice, housePrices$yr_built)


logfitsq <- update(fitting, log(price) ~ bedrooms + bathrooms + I(sqft_living^2)+ floors + view +condition + 
               grade +I(yr_built^2))
summary(logfitsq)


#d)
set.seed(1122)
samples <- sample(nrow(housePrices), 10806)
trainingset <- housePrices[samples,]
testset <- housePrices[-samples,]

logfitpre <- lm(log(price) ~ bedrooms + bathrooms + sqft_living + floors + view +condition + 
                   grade +yr_built, data = trainingset)

logfitsqpre <- update(logfitpre, log(price) ~ bedrooms +bathrooms + I(sqft_living^2)+ floors + view +condition + 
                     grade +I(yr_built^2))

prelog <- predict(logfitpre, testset, se.fit=TRUE)
presq <- predict(logfitsqpre, testset, se.fit = TRUE)

qqplot(prelog$fit, log(testset$price))
abline(a=0,b=1)

qqplot(presq$fit, log(testset$price))
abline(a=0,b=1)

mean((prelog$fit-log(testset$price))^2)
mean((presq$fit-log(testset$price))^2)

