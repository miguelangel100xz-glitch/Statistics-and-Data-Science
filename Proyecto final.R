
library(dplyr)
library(tidyverse)
indice<-read.csv("C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\espacio_tempo\\train.csv")

indice<-indice%>%separate(datetime,into=c("Fecha","Hora"),sep=" ")
library(lubridate)
indice<-indice%>%mutate(Fecha=ymd(Fecha))
indice<-indice%>%separate(Fecha,into=c("Año","Mes","Dia"),sep="-")
#indice<-indice%>%unite(Fecha,Año,Mes,Dia,sep="-")

indice<-indice%>%mutate(Año=as.numeric(Año))
indice<-indice%>%filter(Año>2013)
indice<-indice%>%unite(fecha,Año,Mes)


library(rstatix)
indice<-indice %>%
  group_by(fecha) %>%
  get_summary_stats(electricity_consumption)


library(tseries)

TD.ts = ts(indice$mean, start = c(2015,1),frequency = 12)
plot(TD.ts)
adf.test(TD.ts)

TD.ts%>%acf(main="", sub="Figura 3. Función de autocorrelación simple")
TD.ts%>%pacf(main="", sub="Figura 4. Función de autocorrelación parcial")

library(forecast)
plot(decompose(TD.ts,type = "multiplicative"))
plot(decompose(TD.ts, type = "additive"))
modelo1<- auto.arima(TD.ts)
autoplot(modelo1)
 ggseasonplot(TD.ts)+theme_bw()

 checkresiduals(modelo1, test = "LB")

 # MODELO FINAL #
 fitnal=arima(TD.ts, order =c(1,1,1), seasonal=list(order=c(0,0,0), period=12))
 autoplot(fitnal)
 #normalidad en residuales con test de Ljung Box
 #H0 : se distribuyen normalmente
 checkresiduals(fitnal, test = "LB")
 # COMPARACION DE PRON ´ OSTICOS # ´
 autoplot(forecast(modelo1, h = 3))
 autoplot(forecast(fitnal, h = 3))
 # GRAFICO DE PRON ´ OSTICOS # ´
 plot(forecast(modelo1, h = 5), lwd = 3)
 
 # VALIDACION DE SUPUESTOS ´
 #checamos la normalidad
 shapiro.test(residuals(fitnal))
 #checamos raices unitarias
 adf.test(residuals(fitnal))
 
 #############################################33
 #########################################
 MODEL1 <- auto.arima(TD.ts, stepwise = FALSE, approximation = FALSE, method = "ML")
 
 auto.arima(TD.ts,stationary = TRUE)
 
 fitnal=arima(TD.ts, order =c(3,0,2), seasonal=list(order=c(0,0,1), period=12))
 
 fitnal=arima(TD.ts, order =c(3,0,2), seasonal=list(order=c(1,0,1), period=12))
 
 fitnal=arima(TD.ts, order =c(2,0,1), seasonal=list(order=c(1,0,1), period=12))
 
 ##################################################3
 ##############################################
 
 
 Dif1 <- diff(TD.ts,1)
 Dif2<- diff(Dif1,12)
 
 fitnal=arima(TD.ts, order =c(1,0,0), seasonal=list(order=c(2,0,1), period=12))
 fitnal=arima(TD.ts, order =c(3,0,2), seasonal=list(order=c(1,0,1), period=12))
 fitnal=arima(TD.ts, order =c(2,0,1), seasonal=list(order=c(1,0,1), period=12))
 fitnal=arima(TD.ts, order =c(1,0,0), seasonal=list(order=c(2,1,1), period=12))
 
 
 
 
 
 
 
 