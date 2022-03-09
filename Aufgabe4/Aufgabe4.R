#b)

laplaceVar20 <- rlaplace(2000, m=1, s=1)

q1 <- quantile(laplaceVar, probs = 0.5, type=1)
q2 <- quantile(laplaceVar, probs = 0.5, type=2)
q3 <- quantile(laplaceVar, probs = 0.5, type=3)
q4 <- quantile(laplaceVar, probs = 0.5, type=4)
q5 <- quantile(laplaceVar, probs = 0.5, type=5)
q6 <- quantile(laplaceVar, probs = 0.5, type=6)
q7 <- quantile(laplaceVar, probs = 0.5, type=7)
q8 <- quantile(laplaceVar, probs = 0.5, type=8)
q9 <- quantile(laplaceVar, probs = 0.5, type=9)

laplaceVar1000 <- rlaplace(1000, m=1, s=1)

median1000 <- median(laplaceVar1000)
median1000


#c)

y <- rlaplace(20,m=1,s=1)
z <- rlaplace(1000, m=1, s=1)
ll <- function(m, x, s) { 
    -sum(dlaplace(x, m, s,log = TRUE)) 
   } 
myMLE <- function(samples)
  optimize(f = ll, x = samples, s=1, interval = c(0,100))$minimum

myMLE20 <- myMLE(y)
myMLE1000 <- myMLE(z)
quantileMLE20 <- quantile(y, probs = 0.5)
quantileMLE1000 <- quantile(z,probs = 0.5)

myMLE20
quantileMLE20
myMLE1000
quantileMLE1000

#d)
laplaceMLEData20 <- rlaplace(100000, m=1, s=1)
laplaceMLEData20 <- matrix(laplaceMLEData20, ncol = 20)
MLECompare20 <- c(0)
for (i in 1:5000)
  MLECompare20[i] <- myMLE(laplaceMLEData20[i,])

summary(MLECompare20)


laplaceMLEData1000 <- rlaplace(5000000, m=1, s=1)
laplaceMLEData1000 <- matrix(laplaceMLEData1000, ncol = 1000)
MLECompare1000 <- c(0)
for (i in 1:5000)
  MLECompare1000[i] <- myMLE(laplaceMLEData1000[i,])

summary(MLECompare1000)

hist(MLECompare1000, freq = FALSE)
curve(dnorm(x, mean=mean(MLECompare1000), sd=sqrt(var(MLECompare1000))), col="darkblue", lwd=2, add=TRUE, yaxt="n")

hist(MLECompare20, freq = FALSE)
curve(dnorm(x, mean=mean(MLECompare20), sd=sqrt(var(MLECompare20))), col="darkblue", lwd=2, add=TRUE, yaxt="n")

normalData20 <- rnorm(seq(0,2,by = 0.1), mean=mean(MLECompare20), sd=sqrt(var(MLECompare20)) )
normalData1000 <- rnorm(seq(0.85,1.15,by = 0.003), mean=mean(MLECompare1000), sd=sqrt(var(MLECompare1000)) )

qqplot(MLECompare20, normalData20 )
abline(a=0, b=1, col = 2)  

qqplot(MLECompare1000, normalData1000 )
abline(a=0, b=1, col = 2)  
