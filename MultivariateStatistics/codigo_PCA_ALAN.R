
# Se usa la base de datos de arrestos de USA. 

delitos<-USArrests
attach(delitos)

# La base está predeterminada, los nombres de las ciudades
# son los renglones, por ello la base se guardar como archivo
# con extensión csv para extraer los nombres a una columna

write.csv(USArrests, 'USArrests.csv')

arr<-read.csv("USArrests.csv", header=TRUE)
head(delitos)
# Como se ve, los nombres de los renglones forman la primera columna
Ciudad<-arr[,1]
Ciudad
dim(delitos) #la dimensión la base
head(delitos)

# Explorar la variabilidad de los datos
boxplot(delitos)

**********************************************
  # Pruebas de normalidad, 
  
  #install.packages("MVN")
library(MVN)
Result <- mvn(delitos, mvnTest="mardia", univariatePlot = "qqplot")
Result
par(mfrow=c(1,1))
outliers <- mvn(delitos, mvnTest = "hz", multivariateOutlierMethod = "quan")
outliers
royston_test <- mvn(delitos, mvnTest = "royston", multivariatePlot = "qq")
royston_test$multivariateNormality

************************************************************
  
  # Si el determinante de la matriz de varianzas 
  # y covarianzas es det = 0 indica alta multicolinealidad entre las variables
  # Para aplicar ACP las variables deben estar correlacionadas
  MR<-round(cor(delitos),2)
MR

# Si el determinante de la matriz de correlaciones es det = 0 
# indica alta multicolinealidad entre las variables, por lo tanto no es factible
# realizar el ACP

det(MR)

install.packages('psych')
library(psych)

# Test de esferacidad de Bartlett para probar 
# H0:Matriz de correlación = Matriz de Identidad 
#   vs 
# H1: las matrices son diferentes,
# lo que interesa es rechazar H0.

print(cortest.bartlett(MR, nrow(delitos)))

# ¿Los datos son adecuados para el análisis de componentes principales?
# Se aplica el criterio de Kaiser-Meyer-Olkin (KMO). La estadística es una medida 
# de la proporción de varianza entre variables que podrían ser varianza común.
# Los resultados pueden ser:

0.00 a 0.49 inaceptable.
0.50 a 0.59 miserable.
0,60 a 0,69 mediocre.
0.70 a 0.79 medio.
0,80 a 0,89 meritorio.
0.90 a 1.00 maravilloso.

KMO(MR)

# La siguiente instrucción es para obtener los componentes principales, 
# a través de la matriz de correlación,  

compo<-princomp(delitos,cor=T)
compo

# Y para obtener los CP a través de la matriz de varianzas y covarianzas es con  

com1<- princomp(delitos)
com1

# También se puede usar el comando prcomp

acp<- prcomp(delitos) #con matriz de varianzas y covarianzas
acp #Muestra los vectores propios asociados a las componentes principales

#usando la matriz de correlación, si scale=FALSE usa la matriz de varianzas 
acpcor <- prcomp(delitos,scale =TRUE) 
acpcor
acpcor$x #muestra las puntuaciones de las componentes principales

******************************************
  ZX=scale(delitos[,1:4]) #variables estandarizadas
ZX%*%acpcor$rotation #muestra las puntuaciones de las componentes principales

*****************************************
  
  # Para ambos comandos princomp o prcomp el summary reporta 
  # en porcentaje la importancia de cada componente
  
  summary(compo)
# Muestra los pesos en vectores propios asociados a las componentes principales 
obtenidos por el comando princomp
cargas<-loadings(compo) 
pesos<-cargas[,1:4]
compo$scores  #muestra las puntuaciones de las componentes principales

******************************************
  ZX=scale(delitos) #variables estandarizadas
ZX%*%pesos #muestra las puntuaciones de las componentes principales

*****************************************
  
  #Se obtiene el gráfico de los componentes
  plot(compo)  
#Se obtiene el biplot de los dos primeros dos componentes
biplot(compo)
# Con los 2 primeros componentes se explica el 86.7% del problema
# las nuevas variables latantes se obtienen como

cpuno<-compo$loadings[,1]
NVcp1<-cpuno[1]*Murder+cpuno[2]*Assault+cpuno[3]*UrbanPop+cpuno[4]*Rape
cpdos<-compo$loadings[,2]
NVcp2<-cpdos[1]*Murder+cpdos[2]*Assault+cpdos[3]*UrbanPop+cpdos[4]*Rape

#Para obtener un Indice
#Se obtiene el primer componente y su correspondiente desviación estándar 
compo$sdev[1]
compo$loadings[,1]
#Se estandarizan los pesos del componente
cpest<-compo$loadings[,1]/compo$sdev[1]
cpest
# Se calcula el indice de delincuencia

Indice<-cpest[1]*ZX[,1]+cpest[2]*ZX[,2]+cpest[3]*ZX[,3]+cpest[4]*ZX[,4]
Indice
# Unimos la columna de Indice a la base de delitos
delitos<-cbind(delitos,Indice)
delitos

# Para graficar el Indice

Num_ciudades <- seq(1:50)
tamano <- 20 #tamaño de la burbuja
datt <- data.frame(Ciudad,Num_ciudades,Indice)
datt
# Se ordenan los datos del Indice para asignar la etiqueta
Ino<-datt[order(datt$Indice),]
Clasi<-c(rep("MB",10),rep("B",10),rep("M",10),rep("A",10),rep("MA",10))
Incla<-cbind(Ino,Clasi)
# Regresamos al orden original
dattor<-Incla[order(Incla$Num_ciudades),]

library(ggplot2)

ggplot(dattor, aes(x = Num_ciudades, y = Indice, label = Ciudad)) +
  geom_point(aes(col=Clasi, size=tamano)) + 
  geom_text(hjust = 1, size = 2) +
  labs(subtitle="Indice de delitos",
       title = "Indice, Primer Componente Principal", 
       caption = "Fuente:Elaboración propia") +
  theme_minimal()


*****************************************************************************************
  ******** SEGUNDO   EJEMPLO    ****************
  
  Europa=read.csv("http://www.instantr.com/wp-content/uploads/2013/01/europe.csv",header=TRUE)
head(Europa)
dim(Europa) 
Country<-Europa[,1]
Europa2=Europa[,2:8]
head(Europa2)
attach(Europa2)
pca1 <- prcomp(Europa2,scale=TRUE) 
pca1
summary(pca1)
plot(pca1)

#Los 3 primeros componentes se explica el 78.25%
d1<-pca1$rotation[,1]
d2<-pca1$rotation[,2]
d3<-pca1$rotation[,3]

#Las tres nuevas variables latentes son:
NV1<-d1[1]*Area+d1[2]*GDP+d1[3]*Inflation+d1[4]*Life.expect+d1[5]*Military+d1[6]*Pop.growth+d1[7]*Unemployment
NV2<-d2[1]*Area+d2[2]*GDP+d2[3]*Inflation+d2[4]*Life.expect+d2[5]*Military+d2[6]*Pop.growth+d2[7]*Unemployment
NV3<-d3[1]*Area+d3[2]*GDP+d3[3]*Inflation+d3[4]*Life.expect+d3[5]*Military+d3[6]*Pop.growth+d3[7]*Unemployment

NVarLat<-data.frame(NV1,NV2,NV3)
NVarLat

#Desviación estándar del primer componente

pca1$sdev[1]  

#cargas de cada componente NO SE ESTAN ROTANDO solo que el comando prcomp
#en el resultado presenta la palabra Rotation. 

# El biplot explica el 63%
biplot(pca1)

#Se estandarizan los componentes
pca1$rotation[,1]/pca1$sdev[1]

#Estandarizar la matriz original para obtener los índices

ZEuro<-scale(Europa2)

# Se calcula el Indice
INDI<-0.0695122*ZEuro[,1]-0.2786112*ZEuro[,2]+0.2262921*ZEuro[,3]-0.2687959*ZEuro[,4]+0.1047141*ZEuro[,5]-0.2648047*ZEuro[,6]+0.1512197*ZEuro[,7]

Num_pais <- seq(1:28)
Euro<-data.frame(Country,INDI,Num_pais)
tamano <- 25 #tamaño de la burbuja

# Se ordenan los datos del Indice para asignar la etiqueta
Eor<-Euro[order(Euro$INDI),]

# Se realizan intervalos de clase: (Dato mayor - Dato menor)/Num.intervalos 
# para asignar la clase

Clas<-c(rep("MB",1),rep("B",17),rep("M",6),rep("A",3),rep("MA",1))
Euro1<-cbind(Eor,Clas)
# Regresamos al orden original
Eurocla<-Euro1[order(Euro1$Num_pais),]

library(ggplot2)

ggplot(Eurocla, aes(x = Num_pais,y = INDI,label=Country)) +
  geom_point(aes(col=Clas, size=tamano)) + 
  geom_text(hjust = 1, size = 2) +
  labs(subtitle="Indice de Inflación y Esperanza de vida",
       title = "Indice, Primer Componente Principal", 
       caption = "Fuente:Elaboración propia") +
  theme_minimal()

# Para una interpretación más cercana al análisis necesitamos investigar
# sobre:inflación,esperanza de vida,desempleo,crecimiento de la población
# de los paises de Europa.



