library(dplyr)
library(tidyverse)
library(tseries)
library(ggplot2)
library(forecast)

viales<-read.csv(header=TRUE,"C:\\Users\\elektra\\Downloads\\incidentes_viales_2014_2021oct.csv")
#head(viales)

#viales%>%count(incidente_c4)%>%
 # ggplot(aes(incidente_c4,n))+geom_col()+coord_flip()

#viales%>%count(delegacion_inicio)%>%
 # ggplot(aes(delegacion_inicio,n))+
  #geom_col()+coord_flip()

#viales%>%
 # count(clas_con_f_alarma)%>%
  #ggplot(aes(clas_con_f_alarma,n))+
  #geom_col()

viales1<-viales%>%filter(incidente_c4=="accidente-choque con lesionados")
viales1<-viales1%>%filter(clas_con_f_alarma=="URGENCIAS MEDICAS")





