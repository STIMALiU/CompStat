fsimNorm<-function(N){
    mTD<-matrix(runif(2*N),ncol=N,nrow=2)
    mTD[1,]<-mTD[1,]*2*pi
    apply(mTD,2,function(TD){Th<-TD[1];D<-TD[2];a<-sqrt(-2*log(D));c(a*sin(Th),a*cos(Th))})
}
