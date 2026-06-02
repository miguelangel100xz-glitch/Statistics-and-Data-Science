nuevo<- read.csv(header=TRUE,"C:\\Users\\W10\\Desktop\\ESTADISTICA MULTIVARIADA\\ACTIVIDADES\\PROYECTO ACP AF\\ACP_PROY_FINAL\\delitos_acp.csv")
delitos<- nuevo

#VISUALIZACION DE TASA POR CADA 100,000 HABITANTES

bajo_impacto_graph<-
  ggplot(delitos, aes(x = Bajo_Impacto, y = reorder(AlcaldiaHechos,Bajo_Impacto))) +
  geom_segment(aes(yend = AlcaldiaHechos), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = AlcaldiaHechos)) +
  #scale_colour_brewer(palette = "Set1") +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),   # No horizontal grid lines
    legend.position = c(1, 0.55),           # Put legend inside plot area
    legend.justification = c(1, 0.5)
  )+guides(colour=FALSE,fill=FALSE)+ labs(title="Delitos de Bajo Impacto",
                                          x="Número de  denuncias por cada 100,000 habitantes",
                                          y="Alacaldia",
                                          caption="Fuente:Portal de Datos Abiertos de la CDMX. Fiscalía General de Justicia (FGJ) de la Ciudad de México.")


violacion_graph<-
  ggplot(delitos, aes(x = Violacion, y = reorder(AlcaldiaHechos,Violacion))) +
  geom_segment(aes(yend = AlcaldiaHechos), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = AlcaldiaHechos)) +
  #scale_colour_brewer(palette = "Set1") +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),   # No horizontal grid lines
    legend.position = c(1, 0.55),           # Put legend inside plot area
    legend.justification = c(1, 0.5)
  )+guides(colour=FALSE,fill=FALSE)+ labs(title="Violación",
                                          x="Número de  denuncias por cada 100,000 habitantes",
                                          y="Alacaldia",
                                          caption="Fuente:Portal de Datos Abiertos de la CDMX. Fiscalía General de Justicia (FGJ) de la Ciudad de México.")


homicido_graph<-
  ggplot(delitos, aes(x = Homicidio_Arma_deFuego, y = reorder(AlcaldiaHechos,Homicidio_Arma_deFuego))) +
  geom_segment(aes(yend = AlcaldiaHechos), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = AlcaldiaHechos)) +
  #scale_colour_brewer(palette = "Set1") +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),   # No horizontal grid lines
    legend.position = c(1, 0.55),           # Put legend inside plot area
    legend.justification = c(1, 0.5)
  )+guides(colour=FALSE,fill=FALSE)+ labs(title="Homicidio Doloso",
                                          x="Número de  denuncias por cada 100,000 habitantes",
                                          y="Alacaldia",
                                          caption="Fuente:Portal de Datos Abiertos de la CDMX. Fiscalía General de Justicia (FGJ) de la Ciudad de México.")


#Modificar la tasa por cada 10,000 habitantes
secuestro_graf<- delitos%>% filter(Secuestro>0.09)%>%
  ggplot( aes(x = Secuestro, y = reorder(AlcaldiaHechos,Secuestro))) +
  geom_segment(aes(yend = AlcaldiaHechos), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = AlcaldiaHechos)) +
  #scale_colour_brewer(palette = "Set1") +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),   # No horizontal grid lines
    legend.position = c(1, 0.55),           # Put legend inside plot area
    legend.justification = c(1, 0.5)
  )+guides(colour=FALSE,fill=FALSE)+ labs(title="Secuestro",
                                          x="Número de  denuncias por cada 100,000 habitantes",
                                          y="Alacaldia",
                                          caption="Fuente:Portal de Datos Abiertos de la CDMX. Fiscalía General de Justicia (FGJ) de la Ciudad de México.")



robo_graph<-
  ggplot(delitos, aes(x = Robo, y = reorder(AlcaldiaHechos,Robo))) +
  geom_segment(aes(yend = AlcaldiaHechos), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = AlcaldiaHechos)) +
  #scale_colour_brewer(palette = "Set1") +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),   # No horizontal grid lines
    legend.position = c(1, 0.55),           # Put legend inside plot area
    legend.justification = c(1, 0.5)
  )+guides(colour=FALSE,fill=FALSE)+ labs(title="Robo",
                                          x="Número de  denuncias por cada 100,000 habitantes",
                                          y="Alacaldia",
                                          caption="Fuente:Portal de Datos Abiertos de la CDMX. Fiscalía General de Justicia (FGJ) de la Ciudad de México.")


#POR COMPONENTE VARIANZA ETC 

Resumen<-
  select(delitos,Bajo_Impacto:Robo)%>% 
  pivot_longer(Bajo_Impacto:Robo)%>% ggplot(aes(value))+ 
  geom_histogram(alpha=1/4,fill = "darkgreen",
                 colour = "black")+ facet_wrap(~name,scales="free")+
  labs(title="Víctimas en carpetas de investigación FGJ ",
       x="Número de  denuncias por cada 100,000 habitantes",
       y="",
       caption="Fuente:Portal de Datos Abiertos de la CDMX. Fiscalía General de Justicia (FGJ) de la Ciudad de México.")


varianzas<-
  select(delitos,Bajo_Impacto:Robo)%>% 
  pivot_longer(Bajo_Impacto:Robo)%>%
  #filter(name %in% c("COL","GLUC","TRIG"))%>% 
  ggplot(aes(name,value,fill=name))+
  geom_boxplot(alpha=2/3,outlier.shape = 16)+
  guides(fill=FALSE)+theme_bw()+
  guides(colour=FALSE,fill=FALSE)+
  coord_flip()+
  labs(title="Víctimas en carpetas de investigación FGJ ",
       x="",
       y="Número de  denuncias por cada 100,000 habitantes",
       caption="Fuente:Portal de Datos Abiertos de la CDMX. Fiscalía General de Justicia (FGJ) de la Ciudad de México.")










