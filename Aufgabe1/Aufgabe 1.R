flightdata <-data.frame(city=c("Los Angeles", "Phoenix", "San Diego", "San Francisco", "Seattle"),
           Alaska_OnTime=c(497,221,212,503,1841), Alaska_Delayed=c(62,12,20,102,305), 
           America_OnTime=c(694,4840,383,320,201), America_Delayed=c(117,415,65,129,61),
           check.rows=TRUE, check.names=TRUE)



delayedPerCityAmerican <-(flightdata$America_Delayed)/(flightdata$America_Delayed+flightdata$America_OnTime)
delayedPerCityAmerican <- 100*delayedPerCityAmerican

delayedPerCityAlaska <- flightdata$Alaska_Delayed/(flightdata$Alaska_Delayed+flightdata$Alaska_OnTime)
delayedPerCityAlaska <- 100*delayedPerCityAlaska


delayedPerCity <- t(cbind(delayedPerCityAlaska, delayedPerCityAmerican))

barplot(delayedPerCity, names.arg=flightdata$city, beside=TRUE,  legend.text = c("Alaska Airlines", "America West"), args.legend=list(x=4.5, y=25))

names(delayedPerCity) <- c("San Franciso with AW", "San Franciso with AA", "San Diego with AW", "San Diego with AA",
                           "Phoenix with AW", "Phoenix with AA", "Los Angeles with AW", "Los Angeles with AA", "Seattle with AW", "Seattle with AA")
pie(delayedPerCity, names(delayedPerCity))


delayedPerAirline <- c( sum(flightdata$Alaska_Delayed)/sum(flightdata$Alaska_Delayed+flightdata$Alaska_OnTime), sum(flightdata$America_Delayed)/sum(flightdata$America_Delayed+flightdata$America_OnTime))
delayedPerAirline <- 100*delayedPerAirline
barplot(delayedPerAirline, names.arg = c("Alaska Airlines", "America West"))

names(delayedPerAirline)= c("Alaska Airlines", "American West")
pie(delayedPerAirline, names(delayedPerAirline))
