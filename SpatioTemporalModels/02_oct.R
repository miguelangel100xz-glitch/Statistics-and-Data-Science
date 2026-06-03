


set.seed(25)
ARMA11<-arima.sim(n=100,list(order=c(1,0,1),ar=0.8,ma=0.8))

par(mfrow=2)
acf(ARMA11)
pacf(ARMA11)
