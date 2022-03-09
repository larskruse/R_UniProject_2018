wineQuality <- read.csv("Wine_quality_data_-_Exercises_2_and_3.csv", header = TRUE, sep =";", dec=".", fill=TRUE)
wineQuality

goodquality <- as.integer(wineQuality$quality>5)
wineQuality$good=goodquality

histStur_good <- hist(subset(wineQuality, good ==1)$residual.sugar, breaks = "Sturges", freq=F, main ="Histogram of the residual sugar in good wines using Sturges", xlab = "wines with a quality higher than 5")
histScott_good <-hist(subset(wineQuality, good ==1)$residual.sugar, breaks = "Scott", freq=F, main ="Histogram of the residual sugar in good wines using Scott", xlab = "wines with a quality higher than 5")
histFD_good <- hist(subset(wineQuality, good ==1)$residual.sugar, breaks = "FD", freq=F, main ="Histogram of the residual sugar in good wines using FreedmanDiaconis", xlab = "wines with a quality higher than 5")

histStur_bad <-hist(subset(wineQuality, good ==0)$residual.sugar, breaks = "Sturges", freq=F, main ="Histogram of the residual sugar in bad wines using Sturges", xlab = "wines with a quality less than 5")
histScott_bad <-hist(subset(wineQuality, good ==0)$residual.sugar, breaks = "Scott", freq=F, main = "Histogram of the residual sugar in bad wines using Scott", xlab = "wines with a quality less than 5")
histFD_bad <-hist(subset(wineQuality, good ==0)$residual.sugar, breaks = "FD", freq=F, main = "Histogram of the residual sugar in bad wines using FeedmanDiaconis", xlab = "wines with a quality less than 5")

mean_good <- mean(subset(wineQuality, good ==1)$residual.sugar)
mean_bad <- mean(subset(wineQuality, good ==0)$residual.sugar)

median_good <- median(subset(wineQuality, good ==1)$residual.sugar)
median_bad <- median(subset(wineQuality, good ==0)$residual.sugar)

sd_good <- sd(subset(wineQuality, good ==1)$residual.sugar)
sd_bad <-sd(subset(wineQuality, good ==0)$residual.sugar)

iqr_good <- IQR(subset(wineQuality, good ==1)$residual.sugar)
iqr_bad <- IQR(subset(wineQuality, good ==0)$residual.sugar)

min_good <- min(subset(wineQuality, good ==1)$residual.sugar)
max_good <- max(subset(wineQuality, good ==1)$residual.sugar)

min_bad <- min(subset(wineQuality, good ==0)$residual.sugar)
max_bad <- max(subset(wineQuality, good ==0)$residual.sugar)

statistics <- data.frame("Quality" = c(1,0), "mean"= c(mean_good, mean_bad), "median" = c(median_good, median_good), 
                         "sd" = c(sd_good, sd_bad), "IQR" = c(iqr_good, iqr_bad), "min" = c(min_good, min_bad), 
                         "max" = c(max_good, max_bad))
statistics
boxplot(subset(wineQuality, good ==1)$residual.sugar, subset(wineQuality, good ==0)$residual.sugar, names = c("good wines","bad wines"))

        
qqplot(subset(wineQuality, good ==1)$residual.sugar, subset(wineQuality, good ==0)$residual.sugar, ylim = c(0,75), xlim = c(0,75), xlab="good quality wines", ylab="bad quality wines" , main="QQ-Plot using bad and good wines")
abline(a=0, b=1, col = 2)       

distfun_good <- ecdf(subset(wineQuality, good ==1)$residual.sugar)
distfun_bad <- ecdf(subset(wineQuality, good ==0)$residual.sugar)
plot(distfun_good, main="Distribution functions for the residual sugar in good and bad wines ", verticals = TRUE, col.points = NULL,col.hor = "blue", col.vert = "blue")
plot(distfun_bad, verticals = TRUE, col.points = NULL,col.hor = "red", col.vert = "red", add=TRUE)
legend("right", inset=0.02, legend=c("good wines", "bad wines"),
       col=c("blue", "red"), lty=1:1, cex=0.8)


##Part c)
histStur_good <- hist(subset(wineQuality, good ==1)$volatile.acidity, breaks = "Sturges", freq=F, main ="Histogram of the volatile acidity in good wines using Sturges", xlab = "wines with a quality higher than 5")
histScott_good <-hist(subset(wineQuality, good ==1)$volatile.acidity, breaks = "Scott", freq=F, main ="Histogram of the volatile acidity in good wines using Scott", xlab = "wines with a quality higher than 5")
histFD_good <- hist(subset(wineQuality, good ==1)$volatile.acidity, breaks = "FD", freq=F, main ="Histogram of the volatile acidity in good wines using FeedmanDiaconis", xlab = "wines with a quality higher than 5")

histStur_bad <-hist(subset(wineQuality, good ==0)$volatile.acidity, breaks = "Sturges", freq=F, main ="Histogram of the volatile acidity in bad wines using Sturges", xlab = "wines with a quality less than 5")
histScott_bad <-hist(subset(wineQuality, good ==0)$volatile.acidity, breaks = "Scott", freq=F, main ="Histogram of the volatile acidity in bad wines using Scott", xlab = "wines with a quality less than 5")
histFD_bad <-hist(subset(wineQuality, good ==0)$volatile.acidity, breaks = "FD", freq=F, main ="Histogram of the volatile acidity in bad wines using FeedmanDiaconis", xlab = "wines with a quality less than 5")

mean_good <- mean(subset(wineQuality, good ==1)$volatile.acidity)
mean_bad <- mean(subset(wineQuality, good ==0)$volatile.acidity)

median_good <- median(subset(wineQuality, good ==1)$volatile.acidity)
median_bad <- median(subset(wineQuality, good ==0)$volatile.acidity)

sd_good <- sd(subset(wineQuality, good ==1)$volatile.acidity)
sd_bad <-sd(subset(wineQuality, good ==0)$volatile.acidity)

iqr_good <- IQR(subset(wineQuality, good ==1)$volatile.acidity)
iqr_bad <- IQR(subset(wineQuality, good ==0)$volatile.acidity)

min_good <- min(subset(wineQuality, good ==1)$volatile.acidity)
max_good <- max(subset(wineQuality, good ==1)$volatile.acidity)

min_bad <- min(subset(wineQuality, good ==0)$volatile.acidity)
max_bad <- max(subset(wineQuality, good ==0)$volatile.acidity)

statistics <- data.frame("Quality" = c(1,0), "mean"= c(mean_good, mean_bad), "median" = c(median_good, median_good), 
                         "sd" = c(sd_good, sd_bad), "IQR" = c(iqr_good, iqr_bad), "min" = c(min_good, min_bad), 
                         "max" = c(max_good, max_bad))
statistics

boxplot(subset(wineQuality, good ==1)$volatile.acidity, subset(wineQuality, good ==0)$volatile.acidity, names = c("good wines", "bad wines"))


qqplot(subset(wineQuality, good ==1)$volatile.acidity, subset(wineQuality, good ==0)$volatile.acidity, ylim = c(0,1.2), xlim = c(0,1.2), xlab="good quality wines", ylab="bad quality wines" , main="QQ-Plot using the volatile acidity of bad and good wines" )
abline(a=0, b=1, col = 2)       

distfun_good <- ecdf(subset(wineQuality, good ==1)$volatile.acidity)
distfun_bad <- ecdf(subset(wineQuality, good ==0)$volatile.acidity)
plot(distfun_good, main="Distribution functions of the volatile acidity of good and bad wines", verticals = TRUE, col.points = NULL, col.hor = "blue", col.vert = "blue")
plot(distfun_bad, verticals = TRUE, col.points = NULL,col.hor = "red", col.vert = "red", add=TRUE)
legend("right", inset = 0.02, legend=c("good wines", "bad wines"),
       col=c("blue", "red"), lty=1:1, cex=0.8)

