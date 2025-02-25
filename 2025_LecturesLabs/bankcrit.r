##############################################################
### Computational statistics, Linköping University, VT2025 ###
### Criterion function Lab 6, Q2 (space filling design)    ###
### Frank Miller                                           ###
###                                                        ###
### We are using in this lab partial data from the         ###
### original bankdata available at                         ###
### https://archive.ics.uci.edu/ml/datasets/Bank+Marketing ###
### See also the publication:                              ###
### Sérgio Moro, P. Cortez, P. Rita (2014). A data-driven  ###
### approach to predict the success of bank telemarketing. ###
### Decision Support Systems.                              ###
##############################################################

# you need to save the following dataset at the right place and/or add/set the path where it is located
load("bankdata.Rdata")

nclients <- dim(bankdata)[1]  # number of individuals in the dataset, here 4364

# criterion function: sum of minimal distance to an element in the subset
# dat is the full dataset (here: bankdata), subs is the set of ids for the subset selected
# subs should be a vector of elements in 1, ..., 4364; for this question, it should be of length 22
# example call: crit(bankdata, 1:22), selecting the first 22 individuals
# result of this function is the criterion to be minimized
crit <- function(dat, subs){
  s <- length(subs)
  dist <- matrix(rep(NA, nclients*s), ncol=s)
  for (i in 1:s){
    dist[, i] <- sqrt((dat[,1]-dat[subs[i],1])^2+(dat[,2]-dat[subs[i],2])^2)
  }
  sum(apply(dist, 1, min))
}

# it is good to identify the individuals in the full set by their id (1, ..., 4364), 
# then we can sample from this set for the starting subset:
fullset <- 1:nclients
