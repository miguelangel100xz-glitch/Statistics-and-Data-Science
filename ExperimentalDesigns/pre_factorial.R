
library(dplyr)
library(ggplot2)
library(tidyverse)

datos<- read.csv(header=TRUE,"C:\\Users\\W10\\Desktop\\DISEÑO DE EXP\\DISEÑO FACTORES\\FACTORES_ADMISION.csv")


datos<- datos %>% mutate(FACTORB=as.factor(FACTORB),
                         FACTORA=as.factor(FACTORA))


boxplot(resultados~FACTORA*FACTORB, data=datos,
         ylab="Tooth Length", main="Boxplots of Tooth Growth Data")

with(datos,
 interaction.plot(x.factor=FACTORA, trace.factor=FACTORB,response=resultados, 
fun=mean, type="b", legend=T,ylab="Tooth Length",
 main="Interaction Plot",pch=c(1,19)))

anova<- aov(data=datos,resultados~FACTORA*FACTORB)
summary(anova)

TukeyHSD(anova)#,which="")

with(data=datos, pairwise.t.test(x=resultados, g=FACTORB,p.adjust.method="bonferroni"))

with(data=datos, pairwise.t.test(x=resultados, g=FACTORA,p.adjust.method="bonferroni"))
