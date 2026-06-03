#--------------------------------------------------------------------
#Pronóstico de incidentes viales promedio que requieren servicios 
#de emergencia en tres delegaciones de la CDMX del periodo 2014-2021
#-------------------------------------------------------------------
iztapalapa1<-read.csv(file.choose())
library(tseries)
TD.ts = ts(iztapalapa1$mean, start = c(2014,1),frequency = 12)
plot(TD.ts)
adf.test(TD.ts)
plot(decompose(TD.ts,type = "multiplicative"))
Dif1 = diff(TD.ts,1)
adf.test(Dif1)
par(mfrow=c(1,3))
acf(Dif1, main="Funcion de autocorrelacion simple",xlab="Retardos",lwd = 2)
pacf(Dif1,main="Funcion de autocorrelacion Parcial ",xlab="Retardos",lwd = 2)
plot(Dif1,lwd = 1.5, xlab= "Fecha",main="Comportamiento despues de diferencias")
abline(h=0,col="red",lwd = 2)
# MODELO FINAL #
fitnal=arima(TD.ts, order =c(1,1,1),seasonal=list(order=c(1,0,0), period=12))
autoplot(fitnal)
checkresiduals(fitnal, test = "LB")
autoplot(forecast(fitnal, h = 5))
plot(forecast(modelo1, h = 5), lwd = 3)
#checamos la normalidad
shapiro.test(residuals(fitnal))
#checamos raices unitarias
adf.test(residuals(fitnal))
#-------------------------------------------------
#DELEGACION GUSTAVO A MADERO 
#-------------------------------------------------
Gust1<-read.csv(file.choose())
TD.ts = ts(Gust1$mean, start = c(2014,1),frequency = 12)
plot(TD.ts)
adf.test(TD.ts)
plot(decompose(TD.ts,type = "multiplicative"))
Dif1 = diff(TD.ts,1)
par(mfrow=c(1,3))
acf(Dif1, main="Funcion de autocorrelacion simple",xlab="Retardos",lwd = 2)
pacf(Dif1,main="Funcion de autocorrelacion Parcial ",xlab="Retardos",lwd = 2)
plot(Dif1,lwd = 1.5, xlab= "Fecha",main="Comportamiento despues de diferencias")
abline(h=0,col="red",lwd = 2)
# MODELO FINAL #
fitnal=arima(TD.ts, order =c(5,1,3),seasonal=list(order=c(1,0,0), period=12))
checkresiduals(fitnal, test = "LB")
autoplot(forecast(fitnal, h = 5))
plot(forecast(fitnal, h = 5), lwd = 3)
#checamos la normalidad
shapiro.test(residuals(fitnal))
#checamos raices unitarias
adf.test(residuals(fitnal))
#---------------------------------------
#DELEGACION CUAUHTEMOC 
#---------------------------------------
CUA1<-read.csv(file.choose())
TD.ts = ts(CUA1$mean, start = c(2014,1),frequency = 12)
plot(TD.ts)
adf.test(TD.ts)
plot(decompose(TD.ts,type = "multiplicative"))
Dif1 = diff(TD.ts,1)
par(mfrow=c(1,3))
acf(Dif1, main="Funcion de autocorrelacion simple",xlab="Retardos",lwd = 2)
pacf(Dif1,main="Funcion de autocorrelacion Parcial ",xlab="Retardos",lwd = 2)
plot(Dif1,lwd = 1.5, xlab= "Fecha",main="Comportamiento despues de diferencias")
abline(h=0,col="red",lwd = 2)
# MODELO FINAL #
fitnal=arima(TD.ts, order =c(0,1,1),seasonal=list(order=c(1,0,0), period=12))
checkresiduals(fitnal, test = "LB")
autoplot(forecast(fitnal, h = 5))
plot(forecast(fitnal, h = 5), lwd = 3)
shapiro.test(residuals(fitnal))
adf.test(residuals(fitnal))














