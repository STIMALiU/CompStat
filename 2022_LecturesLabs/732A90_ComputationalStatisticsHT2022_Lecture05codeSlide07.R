source("732A90_ComputationalStatisticsVT2018_Lecture05codeSlide07a.R")
hist(tsamp,breaks=50,col=gray(0.8),main="",xlab="t",ylab="",freq=FALSE,cex.axis=1.5,cex.lab=1.5)
segments(-10, 0.1, -qnorm(0.975), 0.1,lwd=3);segments(qnorm(0.975), 0.1, 10, 0.1,lwd=3)
segments(-qnorm(0.975), 0.1, -qnorm(0.975), -1,lwd=3,lty=2);segments(qnorm(0.975), 0.1, qnorm(0.975), -1,lwd=3,lty=2)
segments(-10, 0.07, -quantile(tsamp,probs=0.975), 0.07,lwd=3,col="blue");segments(quantile(tsamp,0.975), 0.07, 10, 0.07,lwd=3,col="blue")
segments(-quantile(tsamp,probs=0.975), 0.07, -quantile(tsamp,probs=0.975), -1,lwd=3,lty=2,col="blue");segments(quantile(tsamp,probs=0.975), 0.07, quantile(tsamp,probs=0.975), -1,lwd=3,lty=2,col="blue")
segments(-10, 0.05, -qt(0.975,df=9), 0.05,lwd=3,col="red");segments(qt(0.975,df=9), 0.05, 10, 0.05,lwd=3,col="red")
segments(-qt(0.975,df=9), 0.05, -qt(0.975,df=9), -1,lwd=3,lty=2,col="red");segments(qt(0.975,df=9), 0.05, qt(0.975,df=9), -1,lwd=3,lty=2,col="red")

legend("topright",col=c("black","red","blue"),pch=19,legend=c("N(0,1)","t(df=9)","MC"),bty="n",cex=2)

