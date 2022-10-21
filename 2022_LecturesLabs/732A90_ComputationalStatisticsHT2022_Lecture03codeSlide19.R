fgenbeta<-function(c){
    x<-NA
    num.reject<-0
    while (is.na(x)){
	y<-runif(1)
	u<-runif(1)
	if (u<=dbeta(y,2,7)/c){x<-y}
	else{num.reject<-num.reject+1}
    }
    c(x,num.reject)
}

y<-dbeta(seq(0,2,0.001),2,7)
c<-max(y)

mbetas1<-sapply(rep(c,10000),fgenbeta)
mbetas2<-sapply(rep(4,10000),fgenbeta)

hist(mbetas1[2,],col="black",breaks=100,xlab="",ylab="",freq=FALSE,main="");hist(mbetas2[2,],col=gray(0.8),breaks=100,xlab="",ylab="",freq=FALSE,main="",add=TRUE);legend("topright",pch=19,cex=1.5,legend=c("c=3.172","c=4"),col=c("black",gray(0.8)),bty="n");
hist(mbetas1[1,],col="black",breaks=100,xlab="",ylab="",freq=FALSE,main="",ylim=c(0,3.5));hist(mbetas2[1,],col=gray(0.8),breaks=100,xlab="",ylab="",freq=FALSE,main="",add=TRUE);points(seq(0,2,0.001),y,pch=19,cex=0.3,col=gray(0.4));abline(h=3.172554,lwd=3);legend("right",pch=19,cex=1.5,legend=c("c=3.172","c=4"),col=c("black",gray(0.8)),bty="n");
