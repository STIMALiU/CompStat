library("shape")
source("732A90_ComputationalStatisticsVT2017_Lecture04codeSlide02a.R")

num.repeats<-1
Nmax<-100000
vN<-seq(from=5,to=Nmax,by=100)

vests<-t(sapply(vN,function(N,num.repeats){replicate(num.repeats,f.circArea(N))},num.repeats=num.repeats,simplify=TRUE))

plot(vN,vests,cex=0.3,pch=19,xlab="N",ylab=expression(pi),ylim=c(pi-0.1,pi+0.1),cex.lab=2,cex.axis=1.5,col="white",main="");abline(h=pi,lwd=2)
points(vN,pi+1.96*sqrt(pi/vN),col="gray",pch=19,cex=0.4)
points(vN,pi-1.96*sqrt(pi/vN),col="gray",pch=19,cex=0.4)
points(vN,vests,col="black",pch=19,cex=0.3)

N<-200
x<-runif(N)
y<-runif(N)
plot(x,y,xlab="x",ylab="y",ylim=c(-1.2,1.2),xlim=c(-1.2,1.2),main="",pch=19,col="black",cex=0.4,asp=1,cex.lab=2,cex.axis=1.5)
apply(cbind(x,y),1,function(xy){if((xy[1]^2+xy[2]^2)>1){points(xy[1],xy[2],cex=0.4,col="gray")}})
plotcircle(mid=c(0,0),r=1)
abline(v=0)
abline(h=0)
rect(-1,-1,1,1)
