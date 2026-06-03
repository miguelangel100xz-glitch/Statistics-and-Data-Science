
abs(polyroot(z=c(1,-0.4,1.2)))
abs(polyroot(z=c(1,0.5,-0.8)))  

###modelo de medias moviles de orden 1

plot(arima.sim(n=100,list(order=c(0,0,1),ma=-0.8)))
acf(arima.sim(n=100,list(order=c(0,0,1),ma=-0.8)))
pacf(arima.sim(n=100,list(order=c(0,0,1),ma=-0.8)))

