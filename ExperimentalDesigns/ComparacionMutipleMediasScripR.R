library(ggplot2)
library(tidyverse)
library(agricolae)


INSECTICIDAS
head(InsectSprays)
InsectSprays%>% count(spray)


aov.out = aov(count ~ spray, data=InsectSprays)
TukeyHSD(aov.out)  


plot(TukeyHSD(aov.out), las=1)

###BONFERRONI
with(InsectSprays, pairwise.t.test(x=count, g=spray, p.adjust="bonferroni"))



###########################################
#Cerdos de guinea

ToothGrowth%>% head()
ToothGrowth%>% count(supp)
ToothGrowth.bak = ToothGrowth
ToothGrowth$dose = factor(ToothGrowth$dose,
                          levels=c(0.5,1.0,2.0),
                          labels=c("low","med","high"))

ToothGrowth <- ToothGrowth%>% mutate(dose1 = case_when(dose==.5~"baja",
                                                       dose==1.0~"media",
                                                       dose==2.0~"alta")) 

summary(ToothGrowth)
##DESCRIPTIVOS

with(ToothGrowth, table(supp, dose))

aggregate(len ~ supp + dose, ToothGrowth, FUN=mean)

aov.out = aov(len ~ supp * dose, data=ToothGrowth)
model.tables(aov.out, type="means", se=T)
##########ANOVA 
aov.out = aov(len ~ supp * dose, data=ToothGrowth)
summary(aov.out)
#######TUKEY
TukeyHSD(aov.out, which="dose")

with(ToothGrowth, interaction.plot(x.factor=dose, trace.factor=supp,
                                   response=len, fun=mean, type="b", legend=T,
                                   ylab="Tooth Length", main="Interaction Plot",
                                   pch=c(1,19)))

###CONTRASTES ORTOGONALES
contrasts(ToothGrowth$supp)

contrasts(ToothGrowth$dose)

options(contrasts = c("contr.helmert","contr.poly"))
contrasts(ToothGrowth$supp)


contrasts(ToothGrowth$dose)

aov.out2 = aov(len ~ supp * dose, data=ToothGrowth)

summary(aov.out2, split=list(
  supp=list("OJ vs VC"=1),
  dose=list("low vs med"=1,"low.med vs high"=2)),
  expand.split=F
)

options(contrasts = c("contr.treatment","contr.poly"))


###########PRUEBA DE DUNCAN
library(data.table)
DBA <- fread("https://archive.org/download/byrong_DBA1/DBA1.txt",header=TRUE, sep="\t", dec=",")
head(DBA)

TRB<- factor(DBA$Material)
BLOQ<-factor(DBA$Bloque)
ALT<-as.vector(DBA$Altura)
ALT1<-as.numeric(ALT)
boxplot(split(ALT1,TRB),xlab="Clones de eucalipto", ylab="Altura en metros")

resaov<-aov(ALT1 ~ BLOQ + TRB)
library(agricolae)
outLSD <-LSD.test(resaov, "TRB",group=T,console=TRUE)

###########PRUEBA DE SCHEFFE
scheffe.test(resaov, "TRB",console=TRUE)







