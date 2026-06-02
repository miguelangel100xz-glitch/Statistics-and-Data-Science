library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)

datos <- read_xls("C:\\Users\\W10\\Downloads\\Copia_de_Ejercicio_factorial_laboral_1.xls")

datos%>%filter(`TIPO DE CONTRATACIÓN`<3,SEXO<3,EDAD<69,`ANTIGÜEDAD LABORAL`<60)%>%
  mutate(SEXO=as.factor(SEXO),CONTRATACION=as.factor(`TIPO DE CONTRATACIÓN`))%>%
  ggplot(aes(EDAD,`ANTIGÜEDAD LABORAL`,fill=SEXO,color=SEXO,shape=CONTRATACION))+
  geom_point()+theme_bw()

datos %>% count(`FUNCIÓN QUE DESEMPEÑA`) %>% ggplot( aes(x = n, y = reorder(`FUNCIÓN QUE DESEMPEÑA`,n))) +
  geom_segment(aes(yend = `FUNCIÓN QUE DESEMPEÑA`), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = `FUNCIÓN QUE DESEMPEÑA`)) +
  #scale_colour_brewer(palette = "Set1") +
  theme_bw() + guides(colour=FALSE)+ labs(title="Número de Trabajadores en el Hospital",x="Numero de Contratados",y="Función")


datos %>%filter(SEXO<3,`ANTIGÜEDAD LABORAL`<50)%>%
  mutate(SEXO = as.factor(SEXO),Escala = P1+P2+P3+P4+
           P5+P6+P7+P8+P9+10+P11+P12+P13+P14+P15+P16+P17
         +P18+P19+P20+P21+P22+P23+P24+P25+P26+P27+P28+P29+P30) %>% 
  ggplot(aes(`ANTIGÜEDAD LABORAL`,Escala,fill=SEXO,color=SEXO,shape=SEXO))+
  geom_point()+facet_wrap(~`FUNCIÓN QUE DESEMPEÑA`)+
  theme_bw()+labs(title="Resultados de Encuesta de Satisfacción"
                  ,x="Antiguedad Laboral\n(Años)",y="Nivel de Satisfacción")


