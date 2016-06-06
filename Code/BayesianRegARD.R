BayesianLinReg <- function(x, y, vars, varCats, categories, nIterations = 10000){
  

n = dim(x)[1]
p = dim(x)[2] 

# d is the number of regression parameters including the intercept.
d = p+1;

# Number of Categories  
nCat <- length(categories)
  
# Adding a column of 1's for the intercept
x = cbind(rep(1, n), x); 
 
# The parameteres of Inv-chi2 prior for sigma2
nu0 = 1;
sigma02 = 0.5;
 
# The parameters of normal prior N(mu0, tau02) for beta's.
mu0 = rep(0, d);
tau02 = rep(100, d)

# The following two matrices hold the posterior samples for sigma2 and beta.
sigma2 = rep(1, nIterations);
beta = matrix(rep(0, nIterations*(p+1)), nrow = (p+1), ncol=nIterations);
tau2 = matrix(rep(0, nIterations*nCat), nrow = nCat, ncol=nIterations);
  
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
    
  
    for(k in 1:length(categories)){
    ind = which(varCats==categories[k])  
    temp.b <- beta[ind+1, i]
    temp.n <- length(temp.b)
    nu_n = nu0+temp.n;
	  nu = sum((temp.b)^2)/temp.n;
	  sigma02_n = (nu0*sigma02+temp.n*nu)/(nu0+temp.n);
	  z = rchisq(1, nu_n);
    tau2[k, i] <- nu_n*sigma02_n/z;
	  tau02[ind+1] = tau2[k, i]
    }

}

return(list(beta = beta, sigma2 = sigma2, tau2 = tau2))
	    
}

