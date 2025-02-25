##############################################################
### Computational statistics, Linköping University, VT2025 ###
### EM algorithm for univariate normal mixtures            ###
### (mixture of two normal distributions)                  ###
### Frank Miller                                           ###
##############################################################

emalg <- function(dat, eps=0.000001){
  n      <- length(dat)
  pi     <- rep(NA, n)         # initialize vector for prob. to belong to group 1  

  # define reasonable starting values for parameters; here based on summary statistics for total dataset
  p      <- 0.5                # starting value for mixing parameter 
  sigma1 <- sd(dat)*2/3        # starting value for standard deviation in group taken as 2/3 of total sd
  sigma2 <- sigma1
  mu1    <- mean(dat)-sigma1/2 # starting values for means, taken a bit lower and a bit higher than overall mean
  mu2    <- mean(dat)+sigma1/2
  pv     <- c(p, mu1, mu2, sigma1, sigma2)  # parameter vector

  cc     <- eps + 100          # initialize convergence criterion to avoid stopping the while-loop directly 
  while (cc>eps){
    pv1  <- pv                 # save previous parameter vector
    ### E step ###
    for (j in 1:n){
      pi1   <- p*dnorm(dat[j], mean=mu1, sd=sigma1)
      pi2   <- (1-p)*dnorm(dat[j], mean=mu2, sd=sigma2)
      pi[j] <- pi1/(pi1+pi2)
    }
    ### M step ###
    p      <- mean(pi)
    mu1    <- sum(pi*dat)/(p*n)
    mu2    <- sum((1-pi)*dat)/((1-p)*n)
    sigma1 <- sqrt(sum(pi*(dat-mu1)*(dat-mu1)/(p*n)))
    sigma2 <- sqrt(sum((1-pi)*(dat-mu2)*(dat-mu2)/((1-p)*n)))
    ######
    pv     <- c(p, mu1, mu2, sigma1, sigma2)
    cc     <- t(pv-pv1)%*%(pv-pv1)  # a convergence criterion, maybe not the best one
  }
  pv
}
# example data and run of the algorithm
data <- c(0.1, 0.5, 0.7, 1.1, 2.5, 3.4, 3.5, 3.9, 4.0)
emalg(data)


