library(rstatix)
library(ggplot2)
#library(ggpubr)
library(dplyr)

##DOS FACTORES ####################
datos<- read.delim("clipboard", stringsAsFactors = TRUE)
datos<-read.csv(header=TRUE,"C:\\Users\\W10\\Desktop\\DISEÑO DE EXP\\DISEÑO FACTORES\\FACTORES_ADMISION.csv")

#Obtenemos estadisticas descriptivas
datos %>% 
  group_by(FACTORA,FACTORB)%>%
  get_summary_stats(resultados,type="mean_sd")

datos<- datos%>% mutate(FACTORA=as.factor(FACTORA),
                        FACTORB=as.factor(FACTORB))

ggplot(datos,aes(x=interaction(FACTORA,FACTORB),y=resultados))+
  geom_boxplot()

datos %>% anova_test(resultados~FACTORA*FACTORB)

#post HOC
####INTERACCIONES SIMPLES 
model<- lm(resultados~FACTORA*FACTORB,data=datos)
#suma de cuadrados y gl erro agrupado 
datos %>% 
  group_by(FACTORA)%>% 
  anova_test(resultados~FACTORB,erro=model)

datos %>% 
  group_by(FACTORB)%>% 
  anova_test(resultados~FACTORA,erro=model)
#COMPARACIONES MULTIPLES por pares de interacciones simples
library(emmeans)
d<-datos %>% group_by(FACTORA)%>% 
  emmeans_test(resultados~FACTORB,p.adjust.method = "bonferroni")

d<-datos %>% group_by(FACTORB)%>% 
  emmeans_test(resultados~FACTORA,p.adjust.method = "bonferroni")

#omparaciones por pares entre grupos 

datos %>% 
  pairwise_t_test(
    resultados~FACTORB,
    p.adjust.method = "bonferroni")

datos %>% 
  pairwise_t_test(
    resultados~FACTORA,
    p.adjust.method = "bonferroni")




#################################
################################
#TRES FACTORES

datos1<-read.csv(header=TRUE,"C:\\Users\\W10\\Desktop\\DISEÑO DE EXP\\DISEÑO FACTORES\\TRES_FACTORES.csv")
datos1<- datos1%>% mutate(T_CICLAJE=as.factor(T_CICLAJE),
                          SUSPENSIO=as.factor(SUSPENSIO),
                          MALLA=as.factor(MALLA))

ds<-datos1%>% group_by(T_CICLAJE,SUSPENSIO,MALLA)%>% 
  get_summary_stats(OBSERV,type="mean_sd")


##outliers
out<-datos1 %>% 
  group_by(T_CICLAJE,SUSPENSIO,MALLA)%>% 
  identify_outliers(OBSERV)


model1<- lm(OBSERV~T_CICLAJE*SUSPENSIO*MALLA,data=datos1)
ggqqplot(residuals(model1))
#normalidad por grupos 
grupo_norm<-datos1 %>% 
  group_by(T_CICLAJE,SUSPENSIO,MALLA)%>% 
  shapiro_test(OBSERV)
ggqqplot(datos1,"OBSERV",ggtheme=theme_bw())+
  facet_grid(T_CICLAJE+SUSPENSIO~MALLA,labeller="label_both")
##HOMOGENEIDAD DE VARIANZA 
datos1 %>% levene_test(OBSERV~T_CICLAJE*SUSPENSIO*MALLA)

############
###########
#ANOVA TRIFACTORIAL 

datos1 %>% anova_test(OBSERV~T_CICLAJE*SUSPENSIO*MALLA)

#POST_HOC_TEST

#INTERACCION DE DOS VIAS 
model1<- lm(OBSERV~T_CICLAJE*SUSPENSIO*MALLA,data=datos1)
datos1 %>% 
  group_by(T_CICLAJE) %>% 
  anova_test(OBSERV~SUSPENSIO*MALLA,error=model1)
#efectos principales simples 

datos1 %>% 
  group_by(T_CICLAJE,SUSPENSIO)%>% 
  anova_test(OBSERV~MALLA,error=model1)


ggplot(datos1,aes(T_CICLAJE,OBSERV,fill=SUSPENSIO))+
  geom_boxplot()+
  facet_wrap(~MALLA)








