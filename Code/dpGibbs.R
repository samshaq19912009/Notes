# We first simulate 10 samples from N(0, 1) and 5 samples from N(5, 1)
set.seed(123)
y <- c(rnorm(10, 0, 1), rnorm(5, 5, 1))
n = length(y)

# For now we assume that the variance is known as is set to 1
sigma2 <- 1



# G0= N(m, tau2)
m = 0
tau2 <- 10

# Number of MCMC samples
nIter = 10000


# Initilizing theta's
theta <- matrix(1, n, nIter)

# Fixing the scale parameter gamma to 1
gamma <- 0.1

# this is Algorithm 1 in Radford Neal's paper; it is based on Escobar and West (1995).
for(j in 2:nIter){
  theta[, j] <- theta[, j-1]
  for(i in 1:n){   
    # Here, q0 is proportional to the probablity of sampling a new theta and qj is proportional to the probability of assigning i to the theta_j
    # p will have the normalized probabilities; the first one is for sampling a new theta 
    p = c(q0 <- gamma*dnorm(y[i], m, sqrt(sigma2+tau2)), qj <- dnorm(y[i], theta[-i, j], sqrt(sigma2)))/sum(c(q0, qj))
    
    # These are the set of theta's we are going to sample from with probabiltiies p. 
    # The first one is simulated from the posterior distribution given y_i
    Thetas <- c( rnorm(1, (m/tau2+y[i]/sigma2)/(1/tau2+1/sigma2), sqrt(1/(1/tau2+1/sigma2)) ), theta[-i, j-1])
    
    # We now sample a new value for theta_i
    theta[i, j] <- sample(Thetas, 1, prob=p)
  }
    
}


