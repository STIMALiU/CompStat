fCongGenGrid<-function(a,m,c,X0,N){
    X<-X0;vU<-rep(X0,N)
    for (i in 1:N){vU[i]<-X;X<-(a*X+c)%%m}
    vU<-vU/m;plot(vU[1:(N-1)],vU[2:N],pch=19,cex=0.8,main="",xlab="Xk/m",ylab="X(k+1)/m")
};fCongGenGrid(a=17,m=131,c=8,X0=4,N=200)

