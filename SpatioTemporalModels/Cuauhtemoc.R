##################################333
#######Cuahtemoc
#viales%>%count(delegacion_inicio)%>%
 # ggplot(aes(delegacion_inicio,n))+
  #geom_col()+coord_flip()

CUA<-viales1%>%filter(delegacion_inicio=="CUAUHTEMOC")

CUA<-CUA%>%unite(fecha1,ano_cierre,mes_cierre)

CUA<-CUA[c(-37834:-38412),]

CUA1<-CUA%>%count(fecha_creacion)

CUA1<-CUA1%>%separate(fecha_creacion,into=c("Anio","Mes","Dia"))
CUA1<-CUA1%>%unite(fecha,Anio,Mes)

library(rstatix)
CUA1<-CUA1 %>%
  group_by(fecha) %>%
  get_summary_stats(n)

#a<-CUA1%>%separate(fecha,into = c("Anio","Mes"))%>%
 # group_by(Anio) %>%
  #get_summary_stats(mean)

#write.csv(a,file = "descriptivo_cua.csv")

#write.csv(CUA1,file = "serie_gust.csv")

#CUA11<-read.csv(header=TRUE,file.choose())
CUA1<-CUA1[-1,]
write.csv(CUA1,file = "DelegacionCuahtemoc_inc_viales.csv")

TD.ts = ts(CUA1$mean, start = c(2014,1),frequency = 12)
plot(TD.ts)
adf.test(TD.ts)

TD.ts%>%acf(main="", sub="Figura 3. FunciÃ³n de autocorrelaciÃ³n simple")
TD.ts%>%pacf(main="", sub="Figura 4. FunciÃ³n de autocorrelaciÃ³n parcial")

modelo1<- auto.arima(TD.ts)

plot(decompose(TD.ts,type = "multiplicative"))
plot(decompose(TD.ts, type = "additive"))

Dif1 = diff(TD.ts,1)
DIF2= diff(TD.ts,12) 

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
fitnal=arima(TD.ts, order =c(0,1,1),seasonal=list(order=c(1,0,0), period=12))
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
shapiro.test(residuals(modelo1))
#checamos raices unitarias
adf.test(residuals(fitnal))




# MODELO FINAL #
fitnal=arima(TD.ts, order =c(4,1,4),seasonal=list(order=c(1,0,0), period=12))
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



# MODELO FINAL #
fitnal=arima(TD.ts, order =c(0,1,1),seasonal=list(order=c(1,0,0), period=12))
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



# MODELO FINAL #
fitnal=arima(TD.ts, order =c(0,1,1),seasonal=list(order=c(1,0,0), period=12))
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



# MODELO FINAL #
fitnal=arima(TD.ts, order =c(0,1,3),seasonal=list(order=c(1,0,0), period=12))
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



# MODELO FINAL #
fitnal=arima(TD.ts, order =c(0,1,1),seasonal=list(order=c(1,0,0), period=12))
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
