
set.seed(25)
ARMA11<-arima.sim(n=200,list(order=c(1,0,1),ar=0.5,ma=-0.2))
plot(ARMA11)
acf(ARMA11)
pacf(ARMA11)
#pacf(arima.sim(n=200,list(order=c(1,0,1),ar=0.5,ma=-0.2)),lag=20)



