#install.packages(rstatix)
library(rstatix)#correr el comando anterior si no lo tienen
#install.packages(ggplot2)
library(ggplot2)
#install.packages(ggpubr)
library(ggpubr)
#install.packages(dplyr)
library(dplyr)

##DOS FACTORES ####################
datos<-read.delim("clipboard",stringsAsFactors = TRUE)

#ESTADISTICAS DESCRIPTIVAS
datos %>% 
  group_by(FACTORA,FACTORB)%>%
  get_summary_stats(resultados,type="mean_sd")

str(datos)#verificacion de que las variables sean factores

#GRAFICOS EXPLORATORIOS 

ggplot(datos,aes(x=interaction(FACTORA,FACTORB),y=resultados,fill=FACTORB))+
  geom_boxplot()

ggplot(data = datos, aes(x = FACTORA, y = resultados, colour = FACTORB,group = FACTORB))+
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(y  =  'mean (resultados)') +
  theme_bw()

#SUPUESTOS DE ANOVA	
shapiro.test(datos$resultados)
bartlett.test(resultados ~ FACTORA, data=datos)
bartlett.test(resultados ~ FACTORB, data=datos)


#ANOVA 
anova<-datos %>% anova_test(resultados~FACTORA*FACTORB)
anova
plot(anova,1)#expandir la ventana de graficos 
plot(anova,2)

#POSTHOCTEST

#Especificamos como se calculara la suma de 
#cuadrados y los grados de libertad del error 
#agrupado 

model<- lm(resultados~FACTORA*FACTORB,data=datos)

#CALCULAMOS EL EFECTO PRINCIPAL SIMPLE 
#DEL FACTOR SIGNIFICATIVO B
datos %>% 
group_by(FACTORA)%>%
anova_test(resultados~FACTORB,error=model)

#Calculando el efecto simple por pares 

d<-datos %>% group_by(FACTORA)%>%
emmeans_test(resultados~FACTORB,p.adjust.method = "bonferroni")
d


######################################
#ANOVA TRIFACTORIAL###################
######################################
library(rstatix)
library(ggplot2)
library(ggpubr)
library(dplyr)


datos1<-read.delim("clipboard",stringsAsFactors = TRUE)

#estadisticas descvriptivas
ds<-datos1%>% group_by(T_CICLAJE,SUSPENSIO,MALLA)%>%
get_summary_stats(OBSERV,type="mean_sd")
ds

str(datos1)
#VERIFICACION DE OUTLIERS

out<-datos1 %>% 
group_by(T_CICLAJE,SUSPENSIO,MALLA)%>% 
identify_outliers(OBSERV)
out

#normalidad
library(ggpubr)
model1<- lm(OBSERV~T_CICLAJE*SUSPENSIO*MALLA,data=datos1)
ggqqplot(residuals(model1))

#Normalidad por grupos
grupo_norm<-datos1 %>%
group_by(T_CICLAJE,SUSPENSIO,MALLA)%>%
shapiro_test(OBSERV)
grupo_norm

ggqqplot(datos1, "OBSERV", ggtheme = theme_bw()) +
  facet_grid(T_CICLAJE+SUSPENSIO ~ MALLA,labeller = "label_both")

#si observamos la normalidad por tipo de temper1atur1a se aproxima a
#una normal tomar en cuenta que tambien ya son mas datos 
datos1%>% filter(T_CICLAJE=="C1")%>%
ggplot(aes(OBSERV))+geom_histogram()

datos1%>% filter(T_CICLAJE=="C2")%>%
ggplot(aes(OBSERV))+geom_histogram()

#HOMOCEDASTICIDAD
#HOMOGENEIDAD DE VARIANZAS
datos1 %>% levene_test(OBSERV~T_CICLAJE*SUSPENSIO*MALLA)

#ANOVA PARA TRES FACTORES
datos1 %>% anova_test(OBSERV~T_CICLAJE*SUSPENSIO*MALLA)

#PRUEBAS POST HOC
# aRGUMENTO DEL ANOVA PARA SUMA DE CUADRADOS Y ERROR AGRUPADO 
model1<- lm(OBSERV~T_CICLAJE*SUSPENSIO*MALLA,data=datos1)

#interaccion bidireccional 
datos1 %>% 
group_by(T_CICLAJE) %>%
anova_test(OBSERV~SUSPENSIO*MALLA,error=model1)

#Bidireccional para Abertura de Malla?
datos1 %>%
group_by(MALLA) %>%
anova_test(OBSERV~T_CICLAJE*SUSPENSIO,error=model1)

#Efectos principales simples?
rallitas<-datos1 %>% 
group_by(T_CICLAJE,MALLA)%>% 
anova_test(OBSERV~SUSPENSIO,error=model1)
rallitas

#efectos simples por pares 
simple <- datos1 %>%
group_by(T_CICLAJE,MALLA) %>%
emmeans_test(OBSERV~SUSPENSIO, p.adjust.method = "bonferroni") %>%
select(-df, -statistic, -p)
################
simple%>% filter(T_CICLAJE=="C1",MALLA=="B1")
#################
simple%>% filter(T_CICLAJE=="C2",MALLA=="B1")
#para t_ciclaje grupoc2 y malla b1
get_emmeans(simple)%>% 
filter(T_CICLAJE=="C2",MALLA=="B1")
##############
##############

simple%>% filter(T_CICLAJE=="C2",MALLA=="B2")
get_emmeans(simple)%>% 
filter(T_CICLAJE=="C2",MALLA=="B2")

###########

#VISUALIZACION GRAFICA 
ggplot(datos1,aes(T_CICLAJE,OBSERV,fill=SUSPENSIO))+
geom_boxplot()+
facet_wrap(~MALLA)+theme_bw()

