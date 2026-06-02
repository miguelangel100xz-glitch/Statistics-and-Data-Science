

biologicos<- read_excel("C:\\Users\\W10\\Desktop\\ESTADISTICA MULTIVARIADA\\ACTIVIDADES\\PCA\\PCA_2__PARAMETROS_BIOL.xlsx")
library(tidyverse)
library(ggplot2)
library(patchwork)



ph<-ggplot(biologicos,aes(Ph))+
  geom_histogram(fill="blue",colour="black",alpha=0.2)+
  labs(title="PH")
#+scale_y_continuous(labels=scales::percent)


 temperatura<- ggplot(biologicos,aes(Temperatura))+
  geom_histogram(fill="green",colour="black",alpha=0.2)+
   labs(title="Temperatura")
 #scale_y_continuous(labels=scales::percent)


conductividad<-ggplot(biologicos,aes(Conductividad))+
  geom_histogram(fill="orange",colour="black",alpha=0.2)+
  labs(title="Conductividad")
#scale_y_continuous(labels=scales::percent)


od<-ggplot(biologicos,aes(OD))+
  geom_histogram(fill="purple",colour="black",alpha=0.2)+
  labs(title="OD")
#scale_y_continuous(labels=scales::percent)


Turbidez<-ggplot(biologicos,aes(Turbidez))+
  geom_histogram(fill="red",colour="black",alpha=0.2)+
  labs(title="Turbidez")
#scale_y_continuous(labels=scales::percent)

Dureza<-ggplot(biologicos,aes(Dureza))+
  geom_histogram(fill="pink",colour="black",alpha=0.2)+
  labs(title="Dureza")
#+cale_y_continuous(labels=scales::percent)

Sedimentos<-ggplot(biologicos,aes(Solidos_Sedimentales))+
  geom_histogram(fill="pink",colour="black",alpha=0.2)+
  labs(title="Solidos\n Sedimentales ")
#scale_y_continuous(labels=scales::percent)

TotalesS<-ggplot(biologicos,aes(Solidos_Totales))+
  geom_histogram(fill="pink",colour="black",alpha=0.2)+
  labs(title="Solidos Totales")



biologicos1<- select(biologicos,"Ph":"Solidos_Totales")

resumen <- biologicos1 %>% 
  pivot_longer("Ph":"Solidos_Totales")%>%
  #filter(name %in% c("COL","GLUC","TRIG"))%>% 
  ggplot(aes(name,value,fill=name))+
  geom_boxplot(alpha=1/4,outlier.shape = 16)+
  guides(fill=FALSE)+
  # facet_wrap(~name, scales = "free")
  #scale_x_discrete(limits=c("ICC","HEM","IMC","GLUC","COL","TRIG"))+
  labs(
    x="",y="",title="Resumen"
  )+
  coord_flip()

grafico1 <- ph + temperatura + conductividad + od 
grafico2<-Turbidez +  Dureza + Sedimentos + TotalesS

