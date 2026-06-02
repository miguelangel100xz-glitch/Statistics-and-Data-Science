library(readxl)
library(tidyverse)
library(dplyr)


gorriones<- read_xlsx("C:\\Users\\W10\\Downloads\\Dos_poblaciones.xlsx")

datos<-gorriones
datos <- datos %>% 
  mutate(sobrevivio=as.factor(if_else(sobrevivio==1,"V","M")))
datos <- datos[,2:5]

datos <- datos%>% mutate(x2=as.numeric(x2),
                         x3=as.numeric(x3))


library(reshape2)
library(knitr)
library(dplyr)
datos_tidy <- melt(datos, value.name = "valor")


aggregate(formula = valor ~ sobrevivio + variable, data = datos_tidy,
          FUN = function(x){shapiro.test(x)$p.value})


library(MVN)
outliers <- mvn(data = datos[,-1], mvnTest = "hz", multivariateOutlierMethod = "quan")


royston_test <- mvn(data = datos[,-1], mvnTest = "royston", multivariatePlot = "qq")


royston_test$multivariateNormality
