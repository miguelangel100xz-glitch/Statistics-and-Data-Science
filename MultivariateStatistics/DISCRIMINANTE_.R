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

library(ggplot2)
library(ggpubr)


p1 <- ggplot(data = datos, aes(x1, fill = sobrevivio)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = datos, aes(x2, fill = sobrevivio)) +
  geom_histogram(position = "identity", alpha = 0.5)
p3 <- ggplot(data = datos, aes(x3, fill = sobrevivio)) +
  geom_histogram(position = "identity", alpha = 0.5)

graph1<-ggarrange(p1, p2, p3, nrow = 3, common.legend = TRUE)


graph2<-pairs(x = datos[, c("x1","x2","x3")],
      col = c("firebrick", "green3")[datos$sobrevivio], pch = 19)





library(scatterplot3d)
graph4<-scatterplot3d(datos$x1, datos$x2, datos$x3,
              color = c("firebrick", "green3")[datos$sobrevivio], pch = 19,
              grid = TRUE, xlab = "pata", ylab = "abdomen",
              zlab = "organo sexual", angle = 65, cex.axis = 0.6)
legend("topleft",
       bty = "n", cex = 0.8,
       title = "Especie",
       c("a", "b"), fill = c("firebrick", "green3"))


library(reshape2)
library(knitr)
library(dplyr)
datos_tidy <- melt(datos, value.name = "valor")
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



grafos<-
with(datos, {
  s3d <- scatterplot3d(x1, x2, x3,
                       color = c("firebrick", "green3")[datos$sobrevivio],
                       pch = 19, grid = TRUE, xlab = "pata", ylab = "abdomen",
                       zlab = "organo sexual", angle = 65, cex.axis = 0.6)
  
  s3d.coords <- s3d$xyz.convert(x1,x2,x3)
  # convierte coordenadas 3D en proyecciones 2D
  
  tresd<-text(s3d.coords$x, s3d.coords$y, # cordenadas x, y
       labels = datos$sobrevivio,     # texto
       cex = .8, pos = 4)   
  
  legend("topleft", 
         bty = "n", cex = 0.8,
         title = "Especie",
         c("a", "b"), fill = c("firebrick", "green3"))
})

library(klaR)
#Salario+T.lote+horas,data=datos
graf_final<-partimat(sobrevivio ~ x1+x2+x3,data = datos, method = "lda", prec = 28,
         image.colors = c("green","skyblue"),col.mean = "firebrick")









