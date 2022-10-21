library(mvtnorm)
vnorm.data<-rmvnorm(10000,mean=c(0,0),sigma=rbind(c(1,0.8),c(0.8,1)))
x <- y <- seq(-8, 8, len = 200)
X <- expand.grid(x = x, y = y)
X <- transform(X, z = dnorm(x, -2.5)*dnorm(y) + 0.5*dnorm(x, 2.5)*dnorm(y))
z <- matrix(X$z, nrow = 200)
pdf("bivarNorm.pdf");persp(x, y, z, col = "lightblue", border = NA,
   theta = 20, phi = 20, ticktype = "detailed", 
   ltheta = -120, shade = 0.25);dev.off()
   
