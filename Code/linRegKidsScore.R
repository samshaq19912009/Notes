# This is based on the children's test score example discussed by Gelman and Hill (2007).
# This is a Bayesian linear regression model with informative priors The outputs are MCMC samples for the regression parameters "beta" and sigma2, the variance of (y|x, beta). We are using a simple Gibbs sampler here. The program could be written a little more efficiently, but we are trying to keep a balance between efficiency of the code and its readability.  

# Number of MCMC iterations
nIterations = 10000;
 
# Reading in the data
data = as.matrix(read.table("kidiq.txt", sep = ",", header=TRUE));

# We choose mother's IQ and whether she is graduated from high school as predictors 
# The output is kid's test score
x = data[, 2:3];
y = data[, 1];

n = dim(x)[1]
p = dim(x)[2] 

# d is the number of regression parameters including the intercept.
d = p+1;
 
# Here, we are standardizing the predictors by subtracting their mean and deviding by their standard deviation.
# x = scale(x); 

# Adding a column of 1's for the intercept
x = cbind(rep(1, n), x); 
 
# The parameteres of Inv-chi2 prior for sigma2
nu0 = 1;
sigma02 = 0.5;
 
# The parameters of normal prior N(mu0, tau02) for beta's.
mu0 = rep(0, d);
tau02 = rep(100, d);
 
 
regSamp = function(y, x){

# The following two matrices hold the posterior samples for sigma2 and beta.
sigma2 = rep(1, nIterations);
beta = matrix(rep(0, nIterations*(p+1)), nrow = (p+1), ncol=nIterations);
 
# These are y*, x* as described in the course notes.
yStar = c(y, mu0);
xStar = rbind(x, diag(d));
  
for(i in 2:nIterations){
    
    # A contains the diagonal elements of SigmaStar^(-1/2)
    A = 1/sqrt(c(sigma2[i-1]*rep(1, n), tau02));
    # newX is SigmaStar^(-1/2)*xStar
    newX = matrix(rep(A, d), nrow=(n+d), ncol = d)*xStar;
    # newY is SigmaStar^(-1/2)*yStar
    newY = A*yStar;
    
    # Now we can get mu_n and Lambda_n as described in the course note based
    # on newX and newY
    L2 = chol(t(newX)%*%newX);
    invL2 = solve(L2);
    Lambda_n = invL2%*%t(invL2);
    mu_n = Lambda_n%*%t(newX)%*%newY;
    
    # We can now sample from the posterior distribution given sigma^2. As
    # discussed in the class, we use the Cholesky decomposition for this.
    L3 = chol(Lambda_n);
    u = rnorm(d, 0, 1);
    beta[, i] = u%*%L3 + t(mu_n);
 
	# Now, given the current beta's, we sample from the posterior distribution of 	sigma2, which is Inv-chi2(nu_n, sigma02_n). 
    eta = x%*%beta[, i];
    eps = y - eta;
    nu = sum(eps^2)/(n);
    nu_n = nu0 + n;
    sigma02_n = (nu0*sigma02+n*nu)/(nu0+n);
    
    # I sample a new value for sigma2 from Inv-chi2(nu_n, sigma02_n), to do this, I 	sample from z ~ chi2(nu_n) and then my sample from Inv-chi2 would be 	nu_n*sigma02_n/z.
    z = rchisq(1, nu_n);
    sigma2[i] = nu_n*sigma02_n/z;
    
    }

	return(list(beta = beta, sigma2 = sigma2))
	    
}


# Running the program
regRes<-regSamp(y, x);


library(coda)

beta <- mcmc(t(regRes$beta))
plot(beta)


sigma2 <- mcmc(regRes$sigma2)
plot(sigma2)

