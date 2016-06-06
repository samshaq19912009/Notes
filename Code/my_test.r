library(MASS)
library(DAAG)
library(boot)
library(pls)
library(plsdepot)


data <- read.table('prostate.dat', sep=',', header=TRUE)
prednames <- names(data)[-(9)]


Form <- formula(paste("lpsa~", paste(prednames,collapse="+"), sep=""))
attach(data)
pcr_fit <- pcr(lpsa ~., 5, data = as.data.frame(data))

plsr_fit <- pcr(lpsa ~., 5, data = as.data.frame(data))

pca.res = princomp(x)
summary(pca.res)
pca.res$loadings


head(data)

x =(data[, 1:8])
y =(data[, 9])

x <- as.matrix(x)


lm(Form, data=data[folds!=i,])

lm.1 <- lm(y~x)

y_hat <- predict.lm(lm.1,data.frame(x))

y_hat <-as.matrix(y_hat)
error_func <- function(y_pred, y) {
  error = 0
  error = error + 1
} 
i <- 1:length(y)
sum((y_hat[i]-y[i])^2)

yhat <- predict(lm.1, newdata=data.frame(x))
getrss <- function(y, yhat) mean((y-yhat)^2)

rss[1,1] <- getrss(y, yhat)

coef1 <- lm.1$coefficients

summary.lm(lm.1)
predict.lm(lm.1,data.frame(x))


K <- 10
rss <- matrix(0, nr=K, nc=3)
colnames(rss) <- c("lm","ridge","lasso")


lambda = seq(0.01, 1000, 1)
lm.2 <- lm.ridge(y ~ x, lambda = lambda)

best.lam <- which.min(lm.2$GCV)


scaledX <- sweep(x, 2, lm.2$xm)
scaledX <- sweep(scaledX, 2, lm.2$scale, "/")

beta <- lm.2$coef[,best.lam]
yhat <- scaledX %*% beta + lm.2$ym
rss[1,2] <- getrss(y, yhat)


lm.lasso <- lars(x, y, type='lasso')

best_step <- lm.lasso$df[which.min(lm.lasso$RSS)]

predictions <- predict(lm.lasso, x, s=best_step, type="fit")$fit

pls1 = plsreg1(x, y, comps = 3, crosval = TRUE)

predict(pls1,x)

