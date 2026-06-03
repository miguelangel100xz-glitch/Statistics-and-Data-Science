library(dplyr)
library(tidyverse)
library(tseries)
library(ggplot2)

viales<-read.csv(header=TRUE,"C:\\Users\\elektra\\Downloads\\incidentes_viales_2014_2021oct.csv")
head(viales)

viales%>%count(incidente_c4)%>%
  ggplot(aes(incidente_c4,n))+geom_col()+coord_flip()

viales%>%count(delegacion_inicio)%>%
  ggplot(aes(delegacion_inicio,n))+
  geom_col()+coord_flip()

viales%>%
  count(clas_con_f_alarma)%>%
  ggplot(aes(clas_con_f_alarma,n))+
  geom_col()

viales1<-viales%>%filter(incidente_c4=="accidente-choque con lesionados")
viales1<-viales1%>%filter(clas_con_f_alarma=="URGENCIAS MEDICAS")
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

write.csv(d,file="descriptivo_iztapalapa.csv")

write.csv(iztapalapa1,file = "serie_izta.csv")
#iztapalapa1<-read.csv(header=TRUE,file.choose())
library(tseries)
TD.ts = ts(iztapalapa1$mean, start = c(2013,12),frequency = 12)
plot(TD.ts)
adf.test(TD.ts)

TD.ts%>%acf(main="", sub="Figura 3. Función de autocorrelación simple")
TD.ts%>%pacf(main="", sub="Figura 4. Función de autocorrelación parcial")
library(forecast)
modelo1<- auto.arima(TD.ts)

plot(decompose(TD.ts,type = "multiplicative"))
plot(decompose(TD.ts, type = "additive"))

Dif1 = diff(TD.ts,1)
#DIF2= diff(TD.ts,12) 

#[IRREGULAR]
par(mfrow=c(1,2))
acf(Dif1, main="Funcion de autocorrelacion simple",xlab="Retardos",lwd = 2)
pacf(Dif1,main="Funcion de autocorrelacion Parcial ",xlab="Retardos",lwd = 2)
plot(Dif1,lwd = 1.5, xlab= "Fecha",main="Comportamiento despues de diferencias")
abline(h=0,col="red",lwd = 2)
#[ESTACIONAL]
par(mfrow=c(1,3))
acf(DIF2) #Autocorrelación Simple
pacf(DIF2) #Autocorrelación Parcial
plot(DIF2) #Gráfica transformada con una diferenciación
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

#encontrar transformacion de box cosx para corregir la normalidad 
################################################33
####################################################
#Gustavo A madero 

Gust<-viales1%>%filter(delegacion_inicio=="GUSTAVO A. MADERO")

Gust<-Gust%>%unite(fecha1,ano_cierre,mes_cierre)

Gust<-Gust[c(-38161:-38753),]

Gust1<-Gust%>%count(fecha_creacion)

Gust1<-Gust1%>%separate(fecha_creacion,into=c("Anio","Mes","Dia"))
Gust1<-Gust1%>%unite(fecha,Anio,Mes)

library(rstatix)
Gust1<-Gust1 %>%
  group_by(fecha) %>%
  get_summary_stats(n)

c<-Gust1%>%separate(fecha,into = c("Anio","Mes"))%>%
  group_by(Anio) %>%
  get_summary_stats(mean)

write.csv(c,file = "descriptivo_gust.csv")

Gust1<-read.csv(header=TRUE,file.choose())

TD.ts = ts(Gust1$mean, start = c(2013,12),frequency = 12)
plot(TD.ts)
adf.test(TD.ts)

TD.ts%>%acf(main="", sub="Figura 3. Función de autocorrelación simple")
TD.ts%>%pacf(main="", sub="Figura 4. Función de autocorrelación parcial")

modelo1<- auto.arima(TD.ts)

plot(decompose(TD.ts,type = "multiplicative"))
plot(decompose(TD.ts, type = "additive"))

Dif1 = diff(TD.ts,1)
DIF2= diff(TD.ts,12) 

#[IRREGULAR]
par(mfrow=c(1,2))
acf(Dif1, main="Funcion de autocorrelacion simple",xlab="Retardos",lwd = 2)
pacf(Dif1,main="Funcion de autocorrelacion Parcial ",xlab="Retardos",lwd = 2)
plot(Dif1,lwd = 1.5, xlab= "Fecha",main="Comportamiento despues de diferencias")
abline(h=0,col="red",lwd = 2)
#[ESTACIONAL]
par(mfrow=c(1,3))
acf(DIF2) #Autocorrelación Simple
pacf(DIF2) #Autocorrelación Parcial
plot(DIF2) #Gráfica transformada con una diferenciación
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



##################################333
#######Cuahtemoc
viales%>%count(delegacion_inicio)%>%
  ggplot(aes(delegacion_inicio,n))+
  geom_col()+coord_flip()

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

a<-CUA1%>%separate(fecha,into = c("Anio","Mes"))%>%
  group_by(Anio) %>%
  get_summary_stats(mean)

write.csv(a,file = "descriptivo_cua.csv")

write.csv(CUA1,file = "serie_gust.csv")

CUA11<-read.csv(header=TRUE,file.choose())

TD.ts = ts(CUA1$mean, start = c(2014,1),frequency = 12)
plot(TD.ts)
adf.test(TD.ts)

TD.ts%>%acf(main="", sub="Figura 3. Función de autocorrelación simple")
TD.ts%>%pacf(main="", sub="Figura 4. Función de autocorrelación parcial")

modelo1<- auto.arima(TD.ts)

plot(decompose(TD.ts,type = "multiplicative"))
plot(decompose(TD.ts, type = "additive"))

Dif1 = diff(TD.ts,1)
DIF2= diff(TD.ts,12) 

#[IRREGULAR]
par(mfrow=c(1,2))
acf(Dif1, main="Funcion de autocorrelacion simple",xlab="Retardos",lwd = 2)
pacf(Dif1,main="Funcion de autocorrelacion Parcial ",xlab="Retardos",lwd = 2)
plot(Dif1,lwd = 1.5, xlab= "Fecha",main="Comportamiento despues de diferencias")
abline(h=0,col="red",lwd = 2)
#[ESTACIONAL]
par(mfrow=c(1,3))
acf(DIF2) #Autocorrelación Simple
pacf(DIF2) #Autocorrelación Parcial
plot(DIF2) #Gráfica transformada con una diferenciación
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





