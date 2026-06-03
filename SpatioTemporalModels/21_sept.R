
#GENERAR un AR(1) con parametro phi=0.8 ARIMA(P,D,Q)
AR_1<-arima.sim(list(order=c(1,0,0),ar=0.8),n=100)
plot(AR_1, main=(expression(AR(1)~~~~~phi==0.8)))
abline(h=0)
acf(AR_1,main="Autocorrelación simple")
pacf(AR_1,main="Autocorrelación parcial")
AR_1d<-diff(AR_1)
plot(AR_1,AR_1d)
pacf(AR_1,main="Autocorrelacion parcial",plot=FALSE)

