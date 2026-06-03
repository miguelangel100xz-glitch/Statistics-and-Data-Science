#descriptivos modelos espacio temporales 
library(dplyr)
library(ggplot2)
library(forcats)


viales<-read.csv(header=TRUE,"C:\\Users\\elektra\\Downloads\\incidentes_viales_2014_2021oct.csv")
head(viales)

viales%>%count(incidente_c4,clas_con_f_alarma)%>%
  mutate(percent=n/nrow(viales))%>%
  ggplot(aes(x=reorder(incidente_c4,percent),y=percent))+
  geom_bar(stat = "identity")+coord_flip()+
  facet_wrap(~clas_con_f_alarma,scale="free")+
  scale_y_continuous(labels = scales::percent)


##por tipo de incidente vial 
ggplot(viales, aes(x= incidente_c4,  group=clas_con_f_alarma)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  #geom_text(aes( label = scales::percent(..prop..),
   #              y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "",x="",caption="") +
  facet_wrap(~clas_con_f_alarma,scales="free") +
  scale_y_continuous(labels = scales::percent)+coord_flip()+guides(fill=FALSE)+
  theme_bw()



#por delegación 
viales%>%count(delegacion_inicio)%>%
  filter(delegacion_inicio %in%c("ALVARO OBREGON",
                                 "AZCAPOTZALCO","BENITO JUAREZ",
                                 "COYOACAN","CUAJIMALPA","CUAUHTEMOC",
                                 "GUSTAVO A. MADERO","IZTACALCO",
                                 "IZTAPALAPA","MAGDALENA CONTRERAS",
                                 "MIGUEL HIDALGO","MILPA ALTA",
                                 "TLAHUAC","TLALPAN","VENUSTIANO CARRANZA",
                                 "XOCHIMILCO") )%>%
  mutate(percent=n/sum(n))%>%
ggplot(aes(x=reorder(delegacion_inicio,percent),y=percent,fill=delegacion_inicio))+
  geom_bar(stat = "identity",alpha=.5)+coord_flip()+
  #facet_wrap(~clas_con_f_alarma,scale="free")+
  scale_y_continuous(labels = scales::percent)+guides(fill=FALSE)+
  theme_bw()+labs(x="Porcentaje",y="Delegación")
ggsave(filename = "delegacion.jpg")

#########################################################################3
#IZTAPALAPA
viales1<-viales%>%filter(clas_con_f_alarma=="URGENCIAS MEDICAS")

viales1%>%filter(delegacion_inicio %in% c("IZTAPALAPA","GUSTAVO A. MADERO",
                                          "CUAUHTEMOC"))%>%
  ggplot(aes(x= incidente_c4,  group=delegacion_inicio)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  #geom_text(aes( label = scales::percent(..prop..),
  #              y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "",x="",caption="") +
  facet_wrap(~delegacion_inicio,scales="free") +
  scale_y_continuous(labels = scales::percent)+guides(fill=FALSE)+
  theme_bw()+guides(x =  guide_axis(angle = 20)) 

viales1<-viales1%>%filter(incidente_c4=="accidente-choque con lesionados")

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

#########################################
iztapalapa2<-iztapalapa1%>%separate(fecha,into=c("Anio","Mes"),convert = TRUE)

iztapalapa2%>%filter(Anio>2013)%>%
  mutate(Anio=as.factor(Anio))%>%
  ggplot(aes(Anio,mean,fill=Anio))+geom_boxplot(alpha=.5)+geom_jitter()+
  theme_bw()+guides(fill=FALSE)+
  labs(x="Año",y="Promedio de Incidentes Viales",
       title="")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="top")
##############################################3
#################################################
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

Gust2<-Gust1%>%separate(fecha,into=c("Anio","Mes"),convert = TRUE)

Gust2%>%filter(Anio>2013)%>%
  mutate(Anio=as.factor(Anio))%>%
  ggplot(aes(Anio,mean,fill=Anio))+geom_boxplot(alpha=.5)+geom_jitter()+
  theme_bw()+guides(fill=FALSE)+
  labs(x="Año",y="Promedio de Incidentes Viales",
       title="")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="top")
##############################################3
######################
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

CUA2<-CUA1%>%separate(fecha,into=c("Anio","Mes"),convert = TRUE)

CUA2%>%filter(Anio>2013)%>%
  mutate(Anio=as.factor(Anio))%>%
  ggplot(aes(Anio,mean,fill=Anio))+geom_boxplot(alpha=.5)+geom_jitter()+
  theme_bw()+guides(fill=FALSE)+
  labs(x="Año",y="Promedio de Incidentes Viales",
       title="")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="top")
############################















