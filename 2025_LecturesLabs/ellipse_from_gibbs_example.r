###############################
### Boundary ellipse 
### for Gibbs sampling example
### from Lecture 4
### Fall 2023, by Frank Miller
###############################
w  <- 1.8
xv <- seq(-1, 1, by=0.01) * 1/sqrt(1-w^2/4)  # a range of x1-values, where the term below the root is non-negative (compare Lecture 4)
plot(xv, xv, type="n", xlab=expression(x[1]), ylab=expression(x[2]), las=1)
# ellipse
lines(xv, -(w/2)*xv-sqrt(1-(1-w^2/4)*xv^2), lwd=2, col=8)
lines(xv, -(w/2)*xv+sqrt(1-(1-w^2/4)*xv^2), lwd=2, col=8)
