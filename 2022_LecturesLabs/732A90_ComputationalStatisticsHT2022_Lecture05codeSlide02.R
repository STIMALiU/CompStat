x<-seq(from=0,to=1,by=0.01)
y<-1-2*x+3*x^2-0.5*x^3+rnorm(length(x),mean=0,sd=0.09)

lm.lin<-lm(y~x)
lm.p3<-lm(formula=y~poly(x,3))
lm.phigh<-lm(formula=y~poly(x,20))

pred.int.lin<-predict(lm.lin,data.frame(x=x),interval="conf",level=0.95)
pred.int.p3<-predict(lm.p3,data.frame(x=x),interval="conf",level=0.95)
pred.int.phigh<-predict(lm.phigh,data.frame(x=x),interval="conf",level=0.95)

plot(x,y,pch=19,cex=0.8,col=gray(0),xlab="x",ylab=expression(1-2*x+3*x^2-0.5*x^3+epsilon),cex.axis=1.5,cex.lab=1)
lines(x,pred.int.p3[,1],col="black",lwd=4)
lines(x,pred.int.lin[,1],col="black",lwd=2)
lines(x,pred.int.phigh[,1],col="black",lwd=2)
