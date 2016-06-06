library(lars)

#loading prostate data
data <- read.table('prostate.dat', sep=',', header=TRUE)

head(data)

x = (scale(data[, 1:8]))
y = scale(data[, 9], scale=FALSE)

lambda = seq(0.01, 10000, 10)

lm.lasso <- lars(x, y, type='lasso')

plot(lm.lasso)


gcvres <- gcv(lm.lasso)


summary(lm.lasso)
coefficients(lm.lasso)

object <- lars(x,y,type="lasso")
### make predictions at the values in x, at each of the
### steps produced in object
fits <- predict.lars(object, x, type="fit")
### extract the coefficient vector with L1 norm=4.1
coef4.1 <- coef(object, s=4.1, mode="norm") # or
coef4.1 <- predict(object,x, s=4.1, type="fit", mode="norm")
fitted <- coef4.1$fit

larcv <- cv.lars(x,y, K = 10)
