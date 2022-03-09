wineQuality <- read.csv("Wine_quality_data_-_Exercises_2_and_3.csv", header = TRUE, sep =";", dec=".", fill=TRUE)

goodquality <- as.integer(wineQuality$quality>5)
wineQuality$good=goodquality

m <- mean(wineQuality$pH)
std <- sqrt(var(wineQuality$pH))
hist_all <- hist(wineQuality$pH, freq = F)
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE, yaxt="n")


m_good <- mean(subset(wineQuality, good==1)$pH)
std_good <- sqrt(var(subset(wineQuality, good==1)$pH))
hist_good <- hist(subset(wineQuality, good==1)$pH, freq = F)
curve(dnorm(x, mean=m_good, sd=std_good), col="darkblue", add=TRUE)

m_bad <- mean(subset(wineQuality, good==0)$pH)
std_bad <- sqrt(var(subset(wineQuality, good==0)$pH))
hist_bad <- hist(subset(wineQuality, good==0)$pH, freq = F)
curve(dnorm(x, mean=m_bad, sd=std_bad), col="darkblue", lwd=2, add=TRUE, yaxt="n")

#q-q-plots
normal <- qnorm(seq(0.02,1,by=0.02), mean = m, sd = std)
qqplot(normal, subset(wineQuality)$pH)
abline(a=0, b=1)

normal_good <- qnorm(seq(0.02,1,by=0.02), mean = m_good, sd = std_good)
qqplot(normal, subset(wineQuality, good==1)$pH)
abline(a=0, b=1)

normal_bad <- qnorm(seq(0.02,1,by=0.02), mean = m_bad, sd = std_bad)
qqplot(normal, subset(wineQuality, good == 0)$pH)
abline(a=0, b=1)

#p-p-plots
distFun <- ecdf(wineQuality$pH)
is.stepfun(distFun)
knots(distFun)
summary(distFun)
plot(distFun)
normalDist <- pnorm(knots(distFun), mean = m, sd =std)
plot(knots(distFun), normalDist)
summary(normalDist)
plot(normalDist, distFun)
plot(distFun)

#distribution functions:
distFun <- ecdf(wineQuality$pH)
plot(distFun, verticals = TRUE, col.points = NULL,col.hor = "blue", col.vert = "blue")

distFun_good <- ecdf(subset(wineQuality, good==1)$pH)
plot(distFun_good, verticals = TRUE, col.points = NULL,col.hor = "blue", col.vert = "blue")

distFun_bad <- ecdf(subset(wineQuality, good == 0)$pH)
plot(distFun_bad, verticals = TRUE, col.points = NULL,col.hor = "blue", col.vert = "blue")
