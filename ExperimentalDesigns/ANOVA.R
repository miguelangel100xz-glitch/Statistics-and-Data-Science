library(dplyr)
library(ggplot2)
#library(readxl)
library(rstatix)
library(tidyverse)
data <- read.csv("C:\\Users\\W10\\Desktop\\PROYECTO EQUIPO\\ANALISIS_TEMPE.csv" , head=TRUE)
#head(data)

data$TIEMPO = as.factor(data$TIEMPO)
data$TRATAMIENTO = as.factor(data$TRATAMIENTO)
#str(data)

#DESCRIPTIVO EXPLORATORIO

#zona
ZONA_DESC<-data %>% group_by(TRATAMIENTO,ZONA)%>% 
  get_summary_stats(GRADOS_FAREN,type="mean_sd")
exp_zona<-select(ZONA_DESC,ZONA,TRATAMIENTO,mean,sd)
exp_zon<-ZONA_DESC %>%mutate(ZONA= fct_recode(ZONA,"ZONA2"="Z2","ZONA1"="Z1"))%>% 
  pivot_wider(names_from = ZONA,values_from=c(mean,sd))

descript_zona<- select(exp_zon,TRATAMIENTO,mean_ZONA1,sd_ZONA1,mean_ZONA2,sd_ZONA2)

#tratamiento y tiempo 
tiempo1<-data%>% mutate(TIEMPO= fct_recode(TIEMPO,"20Minutos"="20M",
                                           "40Minutos"="40M",
                                           "60Minutos"="60M"))%>%group_by(TRATAMIENTO,TIEMPO)%>%
  get_summary_stats(GRADOS_FAREN,type="mean_sd")
tiempo2<- select(tiempo1,TIEMPO,TRATAMIENTO,mean,sd)
tiempo3<-tiempo2 %>% pivot_wider(names_from=TIEMPO,values_from=c(mean,sd))

tiempo3.1 <- select(tiempo3,TRATAMIENTO,mean_20Minutos,sd_20Minutos)
tiempo3.2 <- select(tiempo3,TRATAMIENTO,mean_40Minutos,sd_40Minutos)
tiempo3.3 <- select(tiempo3,TRATAMIENTO,mean_60Minutos,sd_60Minutos)

                  


#

#SUPUESTO DE NORMALIDAD 
minutos2<- data %>% filter(TIEMPO=="20M")
minutos4<- data %>% filter(TIEMPO=="40M")
minutos6<- data %>% filter(TIEMPO=="60M")

#shapiro.test(minutos2$GRADOS_FAREN)
#shapiro.test(minutos4$GRADOS_FAREN)
#shapiro.test(minutos6$GRADOS_FAREN)

#qqnorm(minutos2$GRADOS_FAREN)
#qqnorm(minutos4$GRADOS_FAREN)
#qqnorm(minutos6$GRADOS_FAREN)

#homocedasticidad de varianza 
#bartlett.test(GRADOS_FAREN~ZONA,data=data)

library(agricolae)
#attach(data)
#model <- sp.plot(block = ZONA, 
 #                pplot = TIEMPO, 
  #               splot = TRATAMIENTO, 
   #              Y = GRADOS_FAREN)


##ANALIZIS POST HOC 
#Edf_a <- model$gl.a
#Edf_a
#Edf_b <- model$gl.b
#Edf_b
#EMS_a <- model$Ea
#EMS_a
#EMS_b <- model$Eb
#EMS_b


#ZONA GEOGRAFICA
#out1 <- LSD.test(y = GRADOS_FAREN, 
 #                trt = ZONA,
  #               DFerror = Edf_a, 
   #              MSerror = EMS_a,
    #             alpha = 0.05,
     #            p.adj = "bonferroni",
      #           group = TRUE,
       #          console = TRUE)

library(tidyverse)
library(hrbrthemes)
library(viridis)

zona_anova<-data %>%mutate(ZONA=fct_recode(ZONA,"ZONA 1"="Z1","ZONA 2"="Z2"))%>%
  ggplot( aes(x=ZONA, y=GRADOS_FAREN, fill=ZONA)) +
  geom_boxplot(alpha=1/2) +
  scale_fill_viridis(discrete = TRUE) +
  geom_jitter(color="darkblue", size=2) +
  theme_bw() +
  guides(fill=FALSE)
#TRATAMIENTOS
#out1 <- LSD.test(y = GRADOS_FAREN, 
 #                trt = TRATAMIENTO,
  #               DFerror = Edf_b, 
   #              MSerror = EMS_b,
    #             alpha = 0.05,
     #            p.adj = "bonferroni",
      #           group = TRUE,
       #          console = TRUE)

tratamientos_anova<- data %>%
  ggplot( aes(x=TRATAMIENTO, y=GRADOS_FAREN, fill=TRATAMIENTO)) +
  geom_boxplot(alpha=1/2) +
  scale_fill_viridis(discrete = TRUE) +
  geom_jitter(color="darkblue", size=2) +
  theme_bw() +
  #ggtitle("A boxplot with jitter") +
  #labs(y="Temperatura Farenheit(F°)",title="",x="",
   #    caption="Fuente: Elaboración propia.")+
  guides(fill=FALSE)


diferencia<-data %>%mutate(TIEMPO=fct_recode(TIEMPO,"20 Minutos"="20M","40 Minutos"="40M","60 Minutos"="60M"))%>%
  ggplot( aes(x=TRATAMIENTO, y=GRADOS_FAREN, fill=TIEMPO)) +
  geom_boxplot(alpha=1/2) +
  scale_fill_viridis(discrete = TRUE) +
  geom_jitter(color="darkblue", size=2) +
  theme_bw() +guides(x = guide_axis(angle = 45))

#exploratorio

T1<-data %>% filter(TRATAMIENTO %in% c("Tratamiento1","Tratamiento2"))%>%
  mutate(ZONA=fct_recode(ZONA,"Zona 1"="Z1","Zona 2"="Z2"),TIEMPO=fct_recode(TIEMPO,"20 Minutos"="20M","40 Minutos"="40M","60 Minutos"="60M"))%>%
  ggplot( aes(fill=TRATAMIENTO, y=GRADOS_FAREN, x=ZONA)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Paired") + theme_bw()+
  labs(x="",y="Temperatura Farenheit")+
  facet_wrap(TRATAMIENTO~ TIEMPO)+
  guides(fill=FALSE)

T2<-data %>% filter(TRATAMIENTO %in% c("Tratamiento3","Tratamiento4"))%>%
  mutate(ZONA=fct_recode(ZONA,"Zona 1"="Z1","Zona 2"="Z2"),TIEMPO=fct_recode(TIEMPO,"20 Minutos"="20M","40 Minutos"="40M","60 Minutos"="60M"))%>%
  ggplot( aes(fill=TRATAMIENTO, y=GRADOS_FAREN, x=ZONA)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Paired") + theme_bw()+
  labs(x="",y="Temperatura Farenheit")+
  facet_wrap(TRATAMIENTO~ TIEMPO)+
  guides(fill=FALSE)


T3<-data %>% filter(TRATAMIENTO %in% c("Tratamiento5"))%>%
  mutate(ZONA=fct_recode(ZONA,"Zona 1"="Z1","Zona 2"="Z2"),TIEMPO=fct_recode(TIEMPO,"20 Minutos"="20M","40 Minutos"="40M","60 Minutos"="60M"))%>%
  ggplot( aes(fill=TRATAMIENTO, y=GRADOS_FAREN, x=ZONA)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Paired") + theme_bw()+
  labs(x="",y="Temperatura Farenheit")+
  facet_wrap(TRATAMIENTO~ TIEMPO)+
  guides(fill=FALSE)

##############################
#############################

interact<-data %>% 
  mutate(ZONA = fct_recode(ZONA,"ZONA 1"="Z1","ZONA 2"="Z2"),TIEMPO=as.numeric(gsub("M"," ",data$TIEMPO)))%>%
  ggplot(aes(TIEMPO,GRADOS_FAREN,color=TRATAMIENTO))+
  geom_line(size=1.2)+
  facet_wrap(~ZONA)+theme_bw()

