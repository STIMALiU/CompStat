# Analysis of July-rainfall data 1786 - 2018, Stockholm
# for Computational Statistics, LiU
# assign first your path where you have saved the data
load("C:\\Users\\frami01\\Dropbox\\Teaching\\CompStat\\VT2025\\julyrain.RData")
# Data source: SMHI; vector "mrain" contains the July-rain in these 233 years

#Bootstrap analysis (own programming)
bo <- 1000                                # number of bootstrap replicates
bs <- c()                                 # to save the results for the means
for (l in 1:bo){
  x  <- sample(mrain, size=length(mrain), replace=TRUE)
  bs <- c(bs, mean(x)) 
  # Alternatively to the two lines before, one can sample indicees:
  # ind <- sample(1:length(mrain), size=length(mrain), replace=TRUE)
  # bs <- c(bs, mean(x[ind]))
}
hist(bs)                                  # Histogram of bootstrap distribution
bss  <- sort(bs)
# 95% bootstrap CI with percentile method
ci95 <- c(bss[round(bo*0.025)], bss[round(bo*0.975)])
ci95

#Bootstrap analysis (with package boot)
library(boot)
bootmean <- function(x, i)  mean(x[i])    # Definition of statistic of interest, here the mean
bss      <- boot(mrain, bootmean, R=1000) # Generate 1000 bootstrap samples with function boot
hist(bss$t)                               # Histogram of bootstrap distribution
boot.ci(bss, type="perc")                 # 95% bootstrap CI with percentile method



