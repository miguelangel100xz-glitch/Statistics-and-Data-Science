###############3333

#Iztapalapa

iztapalapa<-viales1%>%filter(delegacion_inicio=="IZTAPALAPA")

iztapalapa<-iztapalapa%>%unite(fecha1,ano_cierre,mes_cierre)

iztapalapa<-iztapalapa[c(-53224:-54049),]

iztapalapa1<-iztapalapa%>%count(fecha_creacion)

iztapalapa1<-iztapalapa1%>%separate(fecha_creacion,into=c("Anio","Mes","Dia"))
iztapalapa1<-iztapalapa1%>%unite(fecha,Anio,Mes)

library(rstatix)
iztapalapa1<-iztapalapa1 %>%
  group_by(fecha) %>%
  get_summary_stats(n)

iztapalapa1%>%separate(fecha,into = c("Anio","Mes"))%>%
  group_by(Anio) %>%
  get_summary_stats(mean)

iztapalapa1<-iztapalapa1[-1,]

write.csv(iztapalapa1,file = "iztapalapa_inci_viales.csv")

#write.csv(d,file="descriptivo_iztapalapa.csv")
#write.csv(iztapalapa1,file = "serie_izta.csv")
#iztapalapa1<-read.csv(header=TRUE,file.choose())

iztapalapa1<-read.csv(file.choose())

library(tseries)
TD.ts = ts(iztapalapa1$mean, start = c(2014,1),frequency = 12)
plot(TD.ts)
adf.test(TD.ts)

TD.ts%>%acf(main="Delegaci�n Iztapalapa", sub="Figura 3. Funci�n de autocorrelaci�n simple")
TD.ts%>%pacf(main="", sub="Figura 4. Funci�n de autocorrelaci�n parcial")
library(forecast)
modelo1<- auto.arima(TD.ts)

plot(decompose(TD.ts,type = "multiplicative"))
plot(decompose(TD.ts, type = "additive"))

Dif1 = diff(TD.ts,1)
adf.test(Dif1)
#DIF2= diff(TD.ts,12) 

#[IRREGULAR]
par(mfrow=c(1,3))
acf(Dif1, main="Funcion de autocorrelacion simple",xlab="Retardos",lwd = 2)
pacf(Dif1,main="Funcion de autocorrelacion Parcial ",xlab="Retardos",lwd = 2)
plot(Dif1,lwd = 1.5, xlab= "Fecha",main="Comportamiento despues de diferencias")
abline(h=0,col="red",lwd = 2)
#[ESTACIONAL]
par(mfrow=c(1,3))
acf(DIF2) #AutocorrelaciÃ³n Simple
pacf(DIF2) #AutocorrelaciÃ³n Parcial
plot(DIF2) #GrÃ¡fica transformada con una diferenciaciÃ³n
abline(h=0,col="red")
library(forecast)

#[MODELO QUE PROPONE R CON AUTO.ARIMA)
MODELO.R = auto.arima(SERIE);MODELO.R
coeftest(modelo1)
autoplot(modelo1)
ggseasonplot(TD.ts)+theme_bw()


checkresiduals(modelo1, test = "LB")

# MODELO FINAL #
fitnal=arima(TD.ts, order =c(1,1,1),seasonal=list(order=c(0,0,0), period=12))
autoplot(fitnal)
#normalidad en residuales con test de Ljung Box
#H0 : se distribuyen normalmente
checkresiduals(fitnal, test = "LB")
# COMPARACION DE PRON Â´ OSTICOS # Â´
autoplot(forecast(modelo1, h = 3))
autoplot(forecast(fitnal, h = 3))
# GRAFICO DE PRON Â´ OSTICOS # Â´
plot(forecast(modelo1, h = 5), lwd = 3)

# VALIDACION DE SUPUESTOS Â´
#checamos la normalidad
shapiro.test(residuals(fitnal))
#checamos raices unitarias
adf.test(residuals(fitnal))

#encontrar transformacion de box cosx para corregir la normalidad 
################################################33



# MODELO FINAL #
fitnal=arima(TD.ts, order =c(4,1,1),seasonal=list(order=c(0,0,0), period=12))
autoplot(fitnal)
#normalidad en residuales con test de Ljung Box
#H0 : se distribuyen normalmente
checkresiduals(fitnal, test = "LB")
# COMPARACION DE PRON Â´ OSTICOS # Â´
autoplot(forecast(modelo1, h = 3))
autoplot(forecast(fitnal, h = 3))
# GRAFICO DE PRON Â´ OSTICOS # Â´
plot(forecast(modelo1, h = 5), lwd = 3)

# VALIDACION DE SUPUESTOS Â´
#checamos la normalidad
shapiro.test(residuals(fitnal))
#checamos raices unitarias
adf.test(residuals(fitnal))

#encontrar transformacion de box cosx para corregir la normalidad 
################################################33

# MODELO FINAL #
fitnal=arima(TD.ts, order =c(3,1,1),seasonal=list(order=c(0,0,0), period=12))
autoplot(fitnal)
#normalidad en residuales con test de Ljung Box
#H0 : se distribuyen normalmente
checkresiduals(fitnal, test = "LB")
# COMPARACION DE PRON Â´ OSTICOS # Â´
autoplot(forecast(modelo1, h = 3))
autoplot(forecast(fitnal, h = 3))
# GRAFICO DE PRON Â´ OSTICOS # Â´
plot(forecast(modelo1, h = 5), lwd = 3)

# VALIDACION DE SUPUESTOS Â´
#checamos la normalidad
shapiro.test(residuals(fitnal))
#checamos raices unitarias
adf.test(residuals(fitnal))

#encontrar transformacion de box cosx para corregir la normalidad 
################################################33

# MODELO FINAL #
fitnal=arima(TD.ts, order =c(4,1,2),seasonal=list(order=c(1,0,0), period=12))
autoplot(fitnal)
#normalidad en residuales con test de Ljung Box
#H0 : se distribuyen normalmente
checkresiduals(fitnal, test = "LB")
# COMPARACION DE PRON
autoplot(forecast(modelo1, h = 3))
autoplot(forecast(fitnal, h = 3))
# GRAFICO DE PRON 
plot(forecast(modelo1, h = 5), lwd = 3)

# VALIDACION DE SUPUESTOS Â´
#checamos la normalidad
shapiro.test(residuals(fitnal))
#checamos raices unitarias
adf.test(residuals(fitnal))

#encontrar transformacion de box cosx para corregir la normalidad 
################################################33

#encontrar transformacion de box cosx para corregir la normalidad 
################################################33

# MODELO FINAL #
fitnal=arima(TD.ts, order =c(1,1,1),seasonal=list(order=c(1,0,0), period=12))
autoplot(fitnal)
#normalidad en residuales con test de Ljung Box
#H0 : se distribuyen normalmente
checkresiduals(fitnal, test = "LB")
# COMPARACION DE PRON Â´ OSTICOS # Â´
autoplot(forecast(modelo1, h = 3))
autoplot(forecast(fitnal, h = 3))
# GRAFICO DE PRON Â´ OSTICOS # Â´
plot(forecast(fitnal, h = 5), lwd = 3)

# VALIDACION DE SUPUESTOS Â´
#checamos la normalidad
shapiro.test(residuals(fitnal))
#checamos raices unitarias
adf.test(residuals(fitnal))


