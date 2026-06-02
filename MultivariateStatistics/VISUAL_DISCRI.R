library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)

gorriones<- read_xlsx("C:\\Users\\W10\\Downloads\\Dos_poblaciones.xlsx")


sob1<-
gorriones%>% filter(sobrevivio==1)%>%
ggplot(aes(x1, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density()+theme_bw()+labs(x="Extensión del Ala")

sob2<-
gorriones%>% filter(sobrevivio==1)%>%mutate(x2=as.numeric(x2))%>% 
ggplot(aes(x2, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density()+theme_bw()+labs(x="Longitud de Pico y Cabeza")

sob3<-
gorriones%>% filter(sobrevivio==1)%>%mutate(x3=as.numeric(x3))%>% 
ggplot(aes(x3, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density()+theme_bw()+labs(x="Longitud del esternón")

nsov1<-
gorriones%>% filter(sobrevivio!=1)%>%
  ggplot(aes(x1, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density()+theme_bw()+labs(x="Extensión del Ala")

nsov2<-
gorriones%>% filter(sobrevivio!=1)%>%mutate(x2=as.numeric(x2))%>% 
  ggplot(aes(x2, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density()+theme_bw()+labs(x="Longitud de Pico y Cabeza")

nsov3<-
gorriones%>% filter(sobrevivio!=1)%>%mutate(x3=as.numeric(x3))%>% 
  ggplot(aes(x3, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density()+theme_bw()+labs(x="Longitud del esternón")





PLOT1<-ggarrange(sob1, sob2, sob3, nrow = 3, common.legend = TRUE)
PLOT2<-ggarrange(nsov1, nsov2, nsov3, nrow = 3, common.legend = TRUE)
