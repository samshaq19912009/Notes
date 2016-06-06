library(MASS)
library(e1071)

# Breast cancer data; Diagnosing breast cancer cases as M = malignant or B = benign

data = read.table('breastCancer.txt', sep=',', header=TRUE)
n = dim(data)[1]
p = dim(data)[2]


# select the training set
ind.tr <- sample(n, floor(2*n/3))
# select the test set
ind.te <- setdiff(seq(1, n), ind.tr)

lda.m <- lda(y~., data=data[ind.tr, ])
 

qda.m <- qda(y~., data=data[ind.tr, ])
pred.class <- predict(qda.m, data[ind.te, ])$class
act.class <- data$y[ind.te]
table(act.class, pred.class)
mean(act.class == pred.class)

nb.m <- naiveBayes(y~., data=data, subset=ind.tr)
pred.class <- predict(nb.m, newdata=data[ind.te, -11])
act.class <- data$y[ind.te]
table(act.class, pred.class)
mean(act.class == pred.class)

# Letter recognision 

data = read.table('letter.txt', sep=',', header=F)
n = dim(data)[1]
p = dim(data)[2]

ind.tr <- sample(n, floor(2*n/3))
ind.te <- setdiff(seq(1, n), ind.tr)

colnames(data)[1]<-'y'


lda.m <- lda(y~., data=data[ind.tr, ])
pred.class <- predict(lda.m, data[ind.te, ])$class
act.class <- data$y[ind.te]
mean(act.class == pred.class)

qda.m <- qda(y~., data=data[ind.tr, ])
pred.class <- predict(qda.m, data[ind.te, ])$class
act.class <- data$y[ind.te]
mean(act.class == pred.class)


nb.m <- naiveBayes(y~., data=data, subset=ind.tr)
pred.class <- predict(nb.m, newdata=data[ind.te, -1])
act.class <- data$y[ind.te]
mean(act.class == pred.class)

