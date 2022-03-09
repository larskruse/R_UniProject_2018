#read in the data
presidentialRace <- read.csv("Presidential_race_2016_-_Exercise_6.csv", header = TRUE, sep =",", dec=".", fill=TRUE)

#add category vote
# 1 if majority voted for Clinton
# 0 if majority voted for Trump
votedFor <- as.integer(presidentialRace$Percent.Clinton > presidentialRace$Percent.Trump)
presidentialRace$vote=votedFor

diversityClinton <- subset(presidentialRace, presidentialRace$vote == 1)$Diversity.Index
m <- mean(diversityClinton)
std <- sqrt(var(diversityClinton))
hist(diversityClinton, freq = FALSE)
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE, yaxt="n")


diversityTrump <- subset(presidentialRace, presidentialRace$vote == 0)$Diversity.Index
m <- mean(diversityTrump)
std <- sqrt(var(diversityTrump))
hist(diversityTrump, freq = FALSE)
curve(dnorm(x, mean=m, sd=std), col="darkblue", from = 10, to = 80, add = TRUE)

            