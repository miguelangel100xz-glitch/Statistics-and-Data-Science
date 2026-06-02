library(rstatix)
library(ggplot2)
library(ggpubr)
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

bxp<-ggboxplot(
  datos,x="FACTORA",y="resultados",
  color="FACTORB",palette="jco"
)


ggplot(datos,aes(x=interaction(FACTORA,FACTORB),y=resultados))+
       geom_boxplot()















