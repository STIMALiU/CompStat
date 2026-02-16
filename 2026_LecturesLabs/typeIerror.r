###################################
### Computational Statistics    ###
### R-code for simulation of    ###
### type I error                ###
### Lecture 5, Spring 2026      ###
###################################

s     <- 100000  # number of simulations (repetitions)
n     <- 10      # simulated case: study with n observations

# type I error of t-test for uniform data-distribution

count <- 0
for (sim in 1:s){
  x      <- runif(n, min = -1, max = 1)
  reject <- (t.test(x, alternative = "greater")$p.value < 0.05)
  count  <- count + reject
}
rre   <- count/s


# more efficient R-code avoiding the loop
# note: there are some tricky parts in this code, especially the difference x-meanv
# (x is a matrix while meanv is a vector)

talph  <- qt(1-0.05, df = n-1)
x      <- matrix(runif(n*s, min = -1, max = 1), ncol = n)
meanv  <- rowMeans(x)
sdv    <- sqrt(rowSums((x-meanv)^2)/(n-1))
reject <- (sqrt(n)*meanv/sdv > talph)
rre    <- mean(reject) #Rejection rate estimate



