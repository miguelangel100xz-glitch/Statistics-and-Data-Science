library(readxl)
#library(readr)
library(psy)
library(corrplot)
library(psych)
library(semPlot)
library(readr)
library(lavaan)
#data(ehd)
datos <- read_xls("C:\\Users\\W10\\Downloads\\Copia_de_Ejercicio_factorial_laboral_1.xls")
datos<- datos[,9:38]
#________________________________________
# estandarizar variable
D1=scale(datos)
#que R lo lea como una matriz de datos y no como una tabla
D2=as.data.frame(D1)
D2
#______________________________________________
#instalar paquete##
#install.packages("corrplot")
#LA MATRIZ DE ENTRADA ES DE CORRELACION 
MATRIZ_CORRELACION<-cor(D2)
#library(corrplot)
GRAFICO_CORRRELACION<-corrplot(cor(D2), method="circle",order="hclust",tl.col='black',tl.cex=.75)

#determinante
determinante<-det(cor(D2))

#KMO 
library(psych)
KMO_TEST<-KMO(cor(D2))

#Prueba de esfericidad de barlet 

esfericidad<-print(cortest.bartlett(cor(D2),nrow(datos)))

#_____________
#____________________________-
#Análisis factorial sin rotación

#factores 6 factores modelo de manera descriptiva sin rotacion
FA=factanal(D2,factors=2,rotation="none",na.action=na.comit) #modelo
#FA
LO=FA$loadings 
U<-FA$uniquenesses    #valores singulares
#________________________________________---
# Calcular el número de factores ideal
FACTOR_IDEAL<- scree.plot(datos,type = 'R') 
#Guardar factores del 1 al 6
LOF=FA$loading[,1:2]
#write.table(LOF,file="prueba.txt")
#summary(LOF)
#_____________________________
#GRAFICA FACTORES 1 Y 2
LO1=FA$loadings[,1:2]
GRAFICO_FACTORES<-plot(LO1)
text(LO1,labels = names(D),cex =.7,adj = c(0.5,1))
#___________________________________________
##ROTACIÓN VARIMAX##
FA1=factanal(D2,factors=2,rotation="varimax",na.action=na.omit,scores="Bartlett")
LO2=FA$loadings
#LO2# Calcular el número de factores ideal
VARIMAX_IDFEAL<- scree.plot(datos,type = 'R')
#scoores
#variables latentes nuevas
SC1=FA1$scores
SCF1<-SC1[,1]
HISTOGRAMA<- hist(SCF1)
CAJAS<- boxplot(SCF1)
#summary(SCF1)
#____________________________
#BIPLOT para variables con FA1
load1=FA1$loadings[,1:2]
load2=FA1$loadings[,1:2]
BIPLOT_FINAL<-biplot_graph<-plot(load1)
text(load1,labels=names(D),cex=.7,adj=c(0.5,1))

BIPLOT2<- biplot (load1,load2)
text(load1,labels=names(D),cex=.7,adj=c(0.5,1))

#biplot variables y sujetos
#scor1=SC1[,1:2]
#biplot(scor1,load1, xlab='CP1 22.29%%', ylab='CP2 17.96%%')

#install.packages("semPlot")
#install.packages("readr")
#install.packages("lavaan")
library(semPlot)
library(readr)
library(lavaan)
###Datos##
# Cargar los datos de la librería PSY##
#install.packages("psy")
library(psy)
#data(ehd)
#datos <- ehd
#datos<- read_xlsx("C:\\Users\\W10\\Desktop\\ESTADISTICA MULTIVARIADA\\ACTIVIDADES\\AF\\FA.xlsx")
# estandarizar variable
#D1=scale(datos)
#que R lo lea como una matriz de datos y no como una tabla
#D2=as.data.frame(D1)
#D2
###especificar el modleos##
modelo_confirm<-'G1=~ P4+P9+P10+P11+P12+P15+P17+P18+P19+P21+P22+P23+P24+P25+P26
G2=~ P1+P2+P5+P6+P7+P8+P14+P20+P27+P29+P30'
modelo<-cfa(modelo_confirm,data=D2)
 summary(modelo,fit.measures=TRUE)
GRAFICOS<-semPaths(modelo,what="paths",layout="circle",title=TRUE,style="LISREL")
#semPaths(modelo,what="est",layout="circle",title=TRUE,style="LISREL")

