library(tseries)

Vcarnes<-read.csv(header=TRUE,"C:\\Users\\elektra\\Downloads\\Vcarnes (1).csv")

TD.ts = ts(Vcarnes, start = c(2018,1),freq = 65)
plot(TD.ts)
adf.test(TD.ts)



#se debe convertir a que sea estacionaria
#mediante una diferenciación de la serie



TD.ts.d<-diff(TD.ts)
serielog<-log(TD.ts)
plot(serielog)



plot(TD.ts.d, xlab="Año por mes", ylab="Tasa de desempleo",
     main="Tasa de desempleo en el Distrito de Columbia", col=5,lwd=2)



adf.test(serielog)




#Aplicar los gráficos de autocorrelación a la serie diferenciada
TD.ts.d%>%acf(,main="", sub="Figura 3. Función de autocorrelación simple")
TD.ts.d%>%pacf(,main="", sub="Figura 4. Función de autocorrelación parcial")


library(forecast)
auto.arima(TD.ts)



modelo1<-arima(TD.ts,order = c(2,1,3))
modelo1
