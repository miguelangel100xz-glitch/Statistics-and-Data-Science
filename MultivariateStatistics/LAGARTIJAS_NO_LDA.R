######LAGARTIJAS 
#ANALISIS DISCRIMINANTE; PARA DOS POBLACIONES
#PROPIETARIOS Y NO PROPIETARIOS DE CESPED
library(readxl)
#datos<-read.delim("clipboard")
#datos<- read_xlsx("C:\\Users\\W10\\Downloads\\Copia de Roedores.xlsx")
attach(datos)
cesped<- read.csv(header=TRUE,"C:\\Users\\W10\\Downloads\\CESPED.csv")
datos <- lagartijas
datos$Género <- as.factor(datos$Género)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggpubr")
library(ggpubr)
library(plyr)
require(GGally)
##EXPLORACIÓN DE DATOS###
pairs(x = datos[,c("MASS","SVL")],
      col = c("firebrick","green3")[datos$Género],pch = 19)

#install.packages("scatterplot3d")
library(scatterplot3d)

scatterplot3d(datos$MASS, datos$svl,
              color = c("firebrick", "green3")[datos$Género], pch = 19,
              grid = TRUE, xlab = "Salario", ylab = "T.lote",
              zlab = "horas",angle = 65,cex.axis =0.6)
legend("topleft",bty = "n",cex = 0.8,title = "Grupos",
       c("Propietario","No_propietarios"), fill = c("firebrick", "green3"))


# Representación de distribución en histogramas
library(gridExtra)

p1 <- ggplot(data = datos, aes(x = MASS)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.5, 
                 aes(fill = as.factor(Género)))+
  labs(fill = "Grupos")

p2 <- ggplot(data = datos, aes(x = SVL)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.5, 
                 aes(fill = as.factor(Género)))+
  labs(fill = "Grupos")

p3 <- ggplot(data = datos, aes(x = horas)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.5, 
                 aes(fill = as.factor(Grupos)))+
  labs(fill = "Grupos")

grid.arrange(p1, p2, p3)

##NORMALIDAD UNIVARIADA###
##DISTRIBUCIÓN DE LOS PREDICTORES  DE MANERA INDIVIDUAL##
# Representación mediante Histograma de cada variable para cada especie 
par(mfrow=c(2,3))
par(mfcol = c(2, 3))
for (k in 2:3) {
  j0 <- names(datos)[k]
  #br0 <- seq(min(datos[, k]), max(datos[, k]), le = 11)
  x0 <- seq(min(datos[, k]), max(datos[, k]), le = 50)
  for (i in 2:3) {
    i0 <- levels(datos$Género)[i]
    x <- datos[datos$Género == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("Clasificación", i0), xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}
##SHAPIRO WILK
# Contraste de normalidad Shapiro-Wilk para cada variable en cada especie
library(reshape2)
library(knitr)
library(dplyr)
datos_tidy <- melt(datos, value.name = "valor")
kable(datos_tidy %>% group_by(Género, variable) %>% summarise(p_value_Shapiro.test = shapiro.test(valor)$p.value))
#PARA PROBAR NORMALIDAD MULTIVARIANTE
#install.packages("MVN")
library(MVN)
outliers <- mvn(data = datos[,-1], mvnTest = "royston", multivariateOutlierMethod = "quan")
outliers

royston_test <- mvn(data = datos[,-1], mvnTest = "royston", multivariatePlot = "qq")
royston_test$multivariateNormality


#PARA PROBAR HOMOGENEIDAD DE VARIANZAS 
#install.packages("biotools")
library(biotools) 
datos <- datos %>% mutate(Género=as.factor(Género))
boxM(data = datos[,2:3], grouping = datos[,1])

datos <- datos %>% mutate(Grupos=as.factor(Grupos))

modelo_lda<-lda(formula = Grupos ~ Salario+T.lote+horas,data = datos)
modelo_lda
plot(modelo_lda)
#PARA CLASIFICAR UN NUEVO SUJETO

nueva_observacion <- data.frame(Salario = 78,T.lote = 21 ,horas=5)

#CON BASE A LA PROBABILIDAD EL SUJETO SE CLASIFICA COMO PROPIETARIO
predict(object = modelo_lda, newdata = nueva_observacion)

#EVALUACIÓN DE ERROR DE CLASIFICACIÓN

prediccion <- predict(object = modelo_lda,newdata = datos[,-1],method = "predictive")
prediccion
RF<-data.frame(datos$Salario,datos$T.lote,datos$horas,prediccion$class,prediccion$posterior)
RF
table(datos$Grupos, prediccion$class,dnn =c("Clase real", "Clase predicha"))
#COMO SE PUEDE OBSERVAR EXISTEN 3 SUJETOS MAL CLASIFICADOS

error_entre <- mean(datos$Grupos != prediccion$class) * 100
paste("error_entre=", error_entre, "%")

#install.packages("klaR")
library(klaR)
#Salario+T.lote+horas,data=datos
partimat(Grupos ~ Salario+T.lote+horas,data = datos, method = "lda", prec = 28,
         image.colors = c("green","skyblue"),col.mean = "firebrick")





