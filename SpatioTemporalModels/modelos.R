
library(ggplot2)
library(dplyr)
library(hrbrthemes)

indice<-read.csv("C:\\Users\\elektra\\Downloads\\dataset_tk (1).csv")
indice<-indice%>%separate(X,into=c("Fecha","Hora"),sep=" ")
library(lubridate)
indice<-indice%>%mutate(Fecha=dmy(Fecha))

library(tseries)
punjab<-ts(indice$Punjab)
adf.test(punjab
         )


descriptivos<-indice%>%pivot_longer(cols=c("Punjab","Haryana","Rajasthan","Delhi",
                             "UP","Uttarakhand"))%>%
ggplot(aes(Fecha,value))+
  geom_line()+#facet_wrap(~name,scales="free")+
  ylab("Consumo de Energia")+
  #theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2019-01-02"),as.Date("2020-12-05"))) 
  #ylim(0,1.5)

descriptivos<-indice%>%pivot_longer(cols=c("Punjab","Haryana","Rajasthan","Delhi",
                                           "UP","Uttarakhand"))

#############
TD.ts = stats::ts(indice$Punjab, start = c(2019,1),frequency = 365,end=c(2020,12))
plot(TD.ts)
adf.test(TD.ts)

library("forecast")

my_forecast <- function(x){
  model <- HoltWinters(x,beta = FALSE, seasonal = "additive")
  fcast <- forecast(model, 5) # 5 month
  return(fcast)
}

my_forecast(ts(TD.ts, start=c(2015,1), end=c(2015,7), frequency=7))


TD.ts%>%acf(main="", sub="Figura 3. Función de autocorrelación simple")
TD.ts%>%pacf(main="", sub="Figura 4. Función de autocorrelación parcial")

decompose(TD.ts,type = "multiplicative")
decompose(punjab, type = "additive")

autoplot(desc, ts.colour = "blue")+theme bw()
#gr´afico estacional
ggseasonplot(data2) + theme bw()

library(forecast)
auto.arima(TD.ts)

modelo1<-arima(TD.ts,order = c(2,0,3))
modelo1







