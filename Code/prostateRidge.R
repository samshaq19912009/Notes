library(MASS)
library(DAAG)
#loading prostate data
data <- read.table('prostate.data', sep=',', header=TRUE)

names <- read.table('prostate_outcome.names')


head(data)

#x = scale(data[data$train==TRUE, 1:8])
#y = scale(data[data$train==TRUE, 9], scale=FALSE)

x =(data[, 1:8])
y =(data[, 9])

x <- as.matrix(x)

# Linear regression model
lm.1 <- lm(y~x)
summary(lm.1)

coef1 <- lm.1$coefficients

summary.lm(lm.1)
predict.lm(lm.1,data.frame(x))



lambda = seq(0.01, 1000, 1)

lm.2 <- lm.ridge(y ~ x, lambda = lambda)

d.f <- NULL
for(i in 1:length(lambda)){

d.f[i] <- sum(diag((x)%*%solve(t(x)%*%(x) + lambda[i]*diag(8))%*%t(x)))	
	
}

beta <- coef(lm.2)
matplot(d.f, beta[, 2:9], ylim=c(-0.2, 0.8), type='l', xlab=expression(df(lambda)), ylab=expression(beta))

abline(h=0, lty=2, col='gray')

