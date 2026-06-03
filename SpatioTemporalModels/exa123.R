
serie<-read.csv(header=TRUE,file.choose())


serie<-serie[c(-1:-323),c(1,11)]

TD.ts = ts(serie$X.9, start = c(1995,1),freq = 12)
plot(TD.ts)
adf.test(TD.ts)

modelo1<-arima(TD.ts,order = c(2,0,3))
modelo1

TD.ts.d<-diff(TD.ts)
serielog<-log(TD.ts)
plot(serielog)

TD.ts.d%>%acf(,main="", sub="Figura 3. Función de autocorrelación simple")
TD.ts.d%>%pacf(,main="", sub="Figura 4. Función de autocorrelación parcial")


library(forecast)
auto.arima(TD.ts)



modelo1<-arima(TD.ts,order = c(2,0,3))
modelo1
