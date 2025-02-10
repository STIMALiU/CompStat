###################################
### Computational Statistics    ###
### MCMC Example 1              ###
### Simulation of values        ###
### (without plotting them)     ###
### Lecture 4, Fall 2023        ###
###################################

# example density f (bivariate normal mixture); x needs to be two-dimensional
f <- function(x)
{
  # some constants
  s1 <- 0.4
  s2 <- 0.45
  s3 <- 0.45
  s4 <- 0.5
  mu11 <- 1.0
  mu12 <- 0.7
  mu21 <- 0.3
  mu22 <- 1.5
  mu31 <- 2.3
  mu32 <- 1.7

  x1 <- x[1]
  x2 <- x[2]
  y <- (exp(-((x1+0.5)^2+(x2+0.5)^2)/(2*s1))/s1 + 
        exp(-((x1-mu11)^2+(x2-mu12)^2)/(2*s2))/s2 + 
        exp(-((x1-mu21)^2+(x2-mu22)^2)/(2*s3))/s3 +
        exp(-((x1-mu31)^2+(x2-mu32)^2)/(2*s4))/s4) / (2*3.141592)
  return(y)
}


set.seed(123)
radius <- 1
x      <- c(1, -0.5)
chain  <- x
# generate first five values (change 5 to higher number for longer chain)
for (i in 1:5)
{
  # generate first one sample from uniform distribution on circle with specific radius by rejection sampling:
  repeat
  {
    u <- runif(2, min=-radius, max=radius)
    if (u[1]^2+u[2]^2 < radius^2) break
  }

  xcand <- x + u          # canditate point
  R     <- f(xcand)/f(x)  # MH ratio
  ap    <- runif(1)
  if (ap<R)  x <- xcand 
  chain <- rbind(chain, x)
}
chain  # each row is one two-dimensional observation


