fthreebits<-function(k,s,L,N){
    X0<-4*s+1;a<-8*k+5;m<-2^L;X<-X0
    for (i in 1:N){
	print(c(X,rev(intToBits(X)[1:5])))
	X<-(a*X)%%m ##c=0
    }
}
