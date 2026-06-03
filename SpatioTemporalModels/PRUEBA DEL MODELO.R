

sears<-read.csv(header=TRUE, "C:\\Users\\elektra\\Downloads\\sears.csv")
head(sears)

sears.ts<-ts(sears$ï..Yt,start=c(1955,1))
#realziar un grafico para observar el comportamiento
library(tidyverse)
#identificacion del modelo 
sears.ts%>%plot()
#analisis de autocrrelacion 
sears.ts%>%acf(main="",sub="Figura 1: Función de autocorrelación")

sears.ts%>%pacf(main="",sub="Figura 2: Función de autocorrelación")

#realizar prueba de Dickey-Fuller, para verificar si 
#la serie estacionaria.
#H0: La series es no estacionaria. Tiene raiz unitari a
#H1: La series es estacionaria.No tiene raiz unitaria


library(tseries)
adf.test(sears.ts)
#la serie es no estacionaria 
#como la estadistica es -0.4862 y el valor p
#es igual a 0.979 se concluye que la serie es no estacionaria.

#DIFERENCIAR LA SERIE 
#para convertirla en una serie estacionaria 
#se realiza la diferenciaciación de la serie. <3

#Aplicacion de una primera diferenciación

plot(diff(sears.ts))
sears.ts.d<-diff(sears.ts)

#aplicar los graficos de autocorrelacion
#A la serie diferenciada

sears.ts.d%>%diff()%>%acf(main="",sub="Figura 5: Función de autocorrelación")
sears.ts.d%>%diff()%>%pacf(main="",sub="Figura 4: Función de autocorrelación")
adf.test(diff(sears.ts.d))

#ajustar el modelo estimado
#proponer los mdoelos a ajustar ARIMA(1,2,2)
#ARIMA(2,2,2)
arima1<-arima(diff(sears.ts.d),c(1,2,2),method="ML")

#probar la significancia 
library(lmtest)
coeftest(arima1)
confint(arima1)

#ajuste del modelo 2
#ARIMA(2,2,2)
arima2<-arima(diff(sears.ts.d),c(2,2,2),method="ML")
arima2
#probar la significancia 
library(lmtest)
coeftest(arima2)
confint(arima2)

arima1$aic
arima2$aic

#validacion del modelo <3
plot(arima2$residuals,main="",sub="Residuales del modelo ARIMA(2,2,2)")
acf(arima2$residuals,main="",sub="Autocorrelaciones Residuales del modelo ARIMA(2,2,2)")

pacf(arima2$residuals,main="",sub="Autocorrelaciones Parciales Residuales del modelo ARIMA(2,2,2)")
hist(arima2$residuals,main="",sub="Gráfico de normalidad")
shapiro.test(arima2$residuals)


