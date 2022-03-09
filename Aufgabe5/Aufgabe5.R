flightdata <-data.frame(city=c("Los Angeles", "Phoenix", "San Diego", "San Francisco", "Seattle"),
                        Alaska_OnTime=c(497,221,212,503,1841), Alaska_Delayed=c(62,12,20,102,305), 
                        America_OnTime=c(694,4840,383,320,201), America_Delayed=c(117,415,65,129,61),
                        check.rows=TRUE, check.names=TRUE)

alpha <- 0.05
#calculate the expeced frequencies:

hjoAlaska <- c(sum(flightdata$Alaska_OnTime), sum(flightdata$Alaska_Delayed))
hojAlaska <- flightdata$Alaska_Delayed+flightdata$Alaska_OnTime
sumAlaska <- sum(hjoAlaska)

hjoAmerica <- c(sum(flightdata$America_OnTime), sum(flightdata$America_Delayed))
hojAmerica <- flightdata$America_Delayed+flightdata$America_OnTime
sumAmerica <- sum(hjoAmerica)

testStatisticAlaska <- sum((cbind(flightdata$Alaska_OnTime, flightdata$Alaska_Delayed) - hojAlaska %*% t(hjoAlaska))^2/(hojAlaska %*% t(hjoAlaska)))
testStatisticAlaska

