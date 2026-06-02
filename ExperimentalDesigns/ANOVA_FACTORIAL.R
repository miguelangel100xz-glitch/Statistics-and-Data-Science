
library(dplyr)
library(ggplot2)
library(tidyverse)

datos<- read.csv(header=TRUE,"C:\\Users\\W10\\Desktop\\DISEÑO DE EXP\\DISEÑO FACTORES\\FACTORES_ADMISION.csv")

datos<- read.delim("clipboard",stringAsFactors=TRUE)

datos<- datos %>% mutate(FACTORB=as.factor(FACTORB),
                         FACTORA=as.factor(FACTORA))


anova<- aov(data=datos,resultados~FACTORA*FACTORB)
###########################
datos %>% group_by(VOLUMEN_S,MALLA,T_CICLAJE)%>% shapiro_test(OBSERV)
########################

datos%>% anova_test(OBSERV~SUSPENSIO*MALLA*T_CICLAJE)


model<- lm(OBSERV~VOLUMEN_S*MALLA*T_CICLAJE,data=datos)
###########
datos%>% anova_test(OBSERV~VOLUMEN_S*MALLA*T_CICLAJE)
datos%>% group_by(VOLUMEN_S)%>% anova_test(OBSERV~MALLA*T_CICLAJE,error=model)
#########
datos%>% group_by(MALLA)%>% anova_test(OBSERV~VOLUMEN_S*T_CICLAJE,error=model)
###########
datos%>% group_by(T_CICLAJE)%>% anova_test(OBSERV~MALLA*VOLUMEN_S,error=model)
#########









