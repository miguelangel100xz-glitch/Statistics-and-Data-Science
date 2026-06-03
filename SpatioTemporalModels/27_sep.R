
##modelo de medias moviles de orden 1
set.seed(25)
MA1<- arima.sim(n=100,list(order=c(0,0,1),ma=-0.8))
plot(MA1)
acf(MA1,plot=FALSE)
pacf(MA1)

#zt= at + 0.3a_t-1 - 0.4a_t-2
set.seed(25)
MA2<-(arima.sim(n=100,list(order=c(0,0,2),ma=c(.3,-0.4))))
acf(MA2)
acf(MA2,plot=F)
pacf(MA2)






