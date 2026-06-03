

carcacha<-read.csv(header=TRUE,file.choose())
attach(carcacha)
names(carcacha)
boxplot(ï..INGRESO~WEEK_,data=carcacha)
boxplot(ï..INGRESO~DAY_,data=carcacha)

carros.ts<-ts(carcacha$ï..INGRESO)
carros.ts
plot(carros.ts)

acf(carros.ts,sub="Autocorrelograma parcial",main="")
pacf(carros.ts)

library(zoo)
library(tseries)

#H0: LA SERIE ES ESTACIONARIO 
#H1: 

adf.test(carros.ts)

#proporner el modelos SARMA
#sarma (p,d=0,q)(P,D=0,Q)s
modelo<-arima(carros.ts,order=c(7,0,7),seasonal = list(order=c(1,1,1),period=7))
library(forecast)
modelo1<-auto.arima(carros.ts)

#aplicamos la diferenciacaion 

acf(diff(carros.ts,sub="Autocorrelograma parcial",main=""))
pacf(diff(carros.ts))

modelo<-arima(carros.ts,order=c(0,1,5),seasonal = list(order=c(0,1,1),period=7))
library(forecast)
modelo1<-auto.arima(carros.ts)


################################3
################################

carros.ts<-ts(carros$Ingresos)


ma01<-rollmean(carros.ts,k=7,fill=NA)
plot(ma01)














