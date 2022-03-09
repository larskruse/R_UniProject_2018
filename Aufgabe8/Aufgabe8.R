donors <- read.csv("Donors_-_Exercise_8.csv", header = TRUE, sep =",", dec=".", fill=TRUE, col.names = c('recency', 'frequency', 'amount', 'time', 'donation'))
donors

fitFre <- lm(donation ~ frequency, data = donors)
fitamo <- lm(donation ~ amount, data = donors)

summary(fitFre)
summary(fitamo)

qqplot(donors$frequency, donors$amount)

glmfitbino <- glm(donation ~ recency, data = donors, family = binomial)
glmfitgaus <- glm(donation ~ recency, data = donors, family = gaussian)
glmfitpois <- glm(donation ~ recency, data = donors, family = poisson)
glmfitquasi <- glm(donation ~ recency, data = donors, family = quasi)
glmfitquasibin <- glm(donation ~ recency, data = donors, family = quasibinomial)
glmfitquasipois <- glm(donation ~ recency, data = donors, family = quasipoisson)

summary(glmfitbino)
summary(glmfitgaus)
summary(glmfitpois)
summary(glmfitquasi)
summary(glmfitquasibin)
summary(glmfitquasipois)


set.seed(1122)
samples <- sort(sample(nrow(donors), 374))
trainingset <- donors[samples,]
testset <- donors[-samples,]


