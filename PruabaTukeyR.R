#################################################################
#Anova y prueba de tukey con grafico.
#################################################################
datos <- read.delim('clipboard', stringsAsFactors=TRUE)
datos
library(readxl)
datos <- read_xlsx("C:\\Users\\W10\\Desktop\\DISEÑO DE EXP\\ANOVA EN R STUDIO.xlsx")
datos
 
####################
#H0 Homogeneidad de varianzas
#H1 al menos una es diferente

bartlett.test(Contaminacion ~ Planta, data=datos)

##ANOVA DE UNA VIA 

anova<- aov(Contaminacion ~ Planta,data=datos)
par(mfrow=c(1,2))          
plot(anova, 1)           # HOMOGENEIDAD
plot(anova, 2)           # nORMALIDAD
summary(anova)

#PRUEBA DE TUKEY POST HOC 

anova = aov(Contaminacion ~ Planta, data=datos)
TukeyHSD(anova)  

plot(TukeyHSD(anova),las=1)

############################







