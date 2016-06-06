source('BayesianRegARD.R')
library(ggplot2)

# This is a simulation to illustrate ARD

# We first randomly generate the predictors 
x <- matrix(rnorm(50000), 1000, 50)

# And we sample three types of coefficients (ignoring the first one which is the intercept)
b0 <- c(0, rnorm(10, 0, 5), rnorm(20, 0, 2), rep(0, 20))

# We set y = xb0
y = cbind(1, x)%*%b0

# Here we specify the categories 
categories <- c('A', 'B', 'C')

varCats <- c(rep('A', 10), rep('B', 20), rep('C', 20))



vars <- paste('var', seq(1, 50), sep='.')
nVar <- length(vars)


cats <- data.frame(start=NA, end=NA)
for(i in 1:length(categories)){
  ind = which(varCats==categories[i])
  cats[i, 'start'] = min(ind)
  cats[i, 'end'] = max(ind)
}
cats$categories <- categories
cats$end <- cats$end+1


# The function BayesianLinReg uses Gibbs sampling to sample from the posterior distribution of model parameters
samp <- BayesianLinReg(x, y, vars, varCats, categories, nIterations=10000)

# Posterior means of regression coefficients
b <- as.vector(rowMeans(samp$beta[2:(nVar+1), 2000:10000]))

beta <- data.frame(x = seq(1, nVar), b = b, cat=varCats)

#pdf(file='BayesRes.pdf')
ggplot(data=beta, aes(x=x, y=b)) + geom_point(cex=3) + ylab(expression(beta)) +
  #theme(plot.margin = unit(c(1, 0, 0, 0),"lines")) + 
      xlab('Variables')+scale_x_discrete(labels=vars) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size=7))+
  geom_rect(aes(NULL, NULL, xmin = start, xmax = end, fill = categories), ymin = min(beta$b)-1, ymax = max(beta$b)+1, alpha=I(1/5), data = cats)+
  geom_hline(aes(yintercept=0))
#dev.off()


# Posterior means of the three hyperparameters
tau2 <- rowMeans(samp$tau2[, 2000:10000])

tau <- data.frame(x = seq(1, length(categories)), t = as.vector(tau2))

#pdf(file='BayesianTau2.pdf')
ggplot(data=tau, aes(x=x, y=t)) + geom_point(cex=3) + ylab(expression(tau^2))  +
#  theme(plot.margin = unit(c(1, 1, 3, 1),"lines")) + 
      xlab('Variables')+scale_x_discrete(labels=categories) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size=9))+
  geom_hline(aes(yintercept=0))
#dev.off()

