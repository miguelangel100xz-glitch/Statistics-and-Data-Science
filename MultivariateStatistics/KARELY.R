
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
#aqui esta el comando para la prueba de shapiro  karely espero te sirva solo tienes que sustituir 
#los nombres de la base y etc ya  sabes 
hipotesis<-aggregate(formula = valor ~ sobrevivio + variable, data = datos_tidy,
                     FUN = function(x){shapiro.test(x)$p.value})


library(MVN)
outliers <- mvn(data = datos[,-1], mvnTest = "hz", multivariateOutlierMethod = "quan")
#outliers

royston_test <- mvn(data = datos[,-1], mvnTest = "royston", multivariatePlot = "qq")
#royston_test$multivariateNormality
hz_test <- mvn(data = datos[,-1], mvnTest = "hz")
#hz_test$multivariateNormality



library(biotools)
hipo_box<-boxM(data = datos[,2:4], grouping = datos$sobrevivio)


modelo_lda <- lda(formula = sobrevivio ~ x1 + x2 + x3,
                  data = datos)

nuevas_observaciones <- data.frame(x1 = 240, x2= 30,
                                   x3 = 20)
predi<-predict(object = modelo_lda, newdata = nuevas_observaciones)


predicciones <- predict(object = modelo_lda, newdata = datos[, -1],
                        method = "predictive")
contingencia<-table(datos$sobrevivio, predicciones$class,
                    dnn = c("Clase real", "Clase predicha"))



trainig_error <- mean(datos$sobrevivio != predicciones$class) * 100
paste("trainig_error=", trainig_error, "%")




library(klaR)
#Salario+T.lote+horas,data=datos
graf_final<-partimat(sobrevivio ~ x1+x2+x3,data = datos, method = "lda", prec = 28,
                     image.colors = c("green","skyblue"),col.mean = "firebrick")

