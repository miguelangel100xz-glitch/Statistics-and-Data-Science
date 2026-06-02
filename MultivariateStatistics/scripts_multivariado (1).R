################################################
###############################################
###############################################
########ACP#############################

```{r message=FALSE,warning=FALSE,echo=FALSE}
library(ggplot2)
library(ggfortify)
library(knitr)
library(dplyr)
library(tidyverse)
library(patchwork)
library (corrplot)
source("VISUALIZACION_DELITOS.R")
source("ACP_DELITOS.R")
```

La sigueinte base de datos se extrajo del Portal de Datos Abiertos de la CDMX y contiene la información de las victimas de los delitos en las carpetas de la Fiscalia General de Justicia de la Ciudad de Mexico a partir de enero del 2019. Para informacion detallada de la base de datos consulte la siguiente direccion:
\url(https://datos.cdmx.gob.mx/dataset/victimas-en-carpetas-de-investigacion-fgj})

Asi tambien de la base de datos solo se analizo la informacion del año 2020 y el mes de enero del 2021 a su vez se opto por analizar solo las 16 alcaldias de la ciudad de Mexico. Por otro lado se omitieron los datos de delitos categorizados como "Hecho no Delictivo". 

De esta forma seleccionamos la información de los delitos siguientes:
-Homicidio : dentro de este delito esta homicidio y lesiones dolosas por arma de fuego
-Violacion
-Delito de Bajo Impacto
-Robo: dentro de esta categoria estan robo en via o transporte publica y robo casa habitación.
-Secuestro

Se opto por contabilizar los anteriores delitos ya que los datos tenian multicolinealidad debido a la naturaleza de los datos a analizar. 
Ejemplo: cuando se comete homicidio el delito es 
1: homicidio
2: lesion dolosa por arma de fuego

Es de esta forma que al cometer un delito se contabilizan todas las categorias del delito que estuvieran involucradas en dicho suceso. De acuerdo a lo anterior justificamos la agrupación de las categorias del delito similares.

Asi tambien se obtuvo una tasa de incidencia delictiva por cada 100,000 habitantes con la fomrula siguiente:


$$ Tasa=(\frac{EventoInteres}{Población Total})* 100,000$$
Y los datos del número de poblacion para las 16 alcaldias de la CDMX se obtuvierón del Censo Nacional del año 2020 realizado por el INEGI.
```{r tidy=TRUE, warning=FALSE,echo=FALSE,message=FALSE}
source("tasas.R")
 tasa123<- tasahabitantes %>% select(AlcaldiaHechos,Habitantes)
kable(tasa123)

```


Es asi que la base de datos para el analisis resulto de la manera siguiente.
```{r tidy=TRUE, warning=FALSE,echo=FALSE,message=FALSE}
head(nuevo1)

```
Asi tambien es importante mencionar que la columna secuestro al momento del analisis tenia 4 valores faltantes a los cuales se les asigno un cero por motivos de practicidad para el analis de componentes principales y para que no se omitieran del analisis 4 alcaldias.

```{r warning=FALSE,echo=FALSE,message=FALSE}
kable(summary(nuevo1))

```


## Analisis Descriptivo Exploratorio

```{r warning=FALSE,echo=FALSE,message=FALSE}
bajo_impacto_graph
violacion_graph
homicido_graph
secuestro_graf
robo_graph

```

```{r warning=FALSE,echo=FALSE}
Resumen
varianzas
```


## Analisis de Componentes Principales

Matriz de Varianzas y Covarianzas
```{r warning=FALSE,echo=FALSE}
kable(matriz_varianzas)

```

Matriz de Correlaciones
```{r warning=FALSE,echo=FALSE}
kable(matriz_cor)
```





```{r warning=FALSE,echo=FALSE}
S<-cor(nuevo1, use="complete.obs")
corrplot.mixed(S)
```


```{r warning=FALSE,echo=FALSE}
kable(determinante , booktabs = TRUE, align =c("l"),col.names =c("DETERMINANTE"),escape=FALSE)
```

Varianza Acumulada
```{r warning=FALSE,echo=FALSE}
source("ACP_DELITOS.R")
 acp<- prcomp(nuevo1,scale=TRUE)
summary(acp)

```
Valores Singulares
```{r warning=FALSE,echo=FALSE}
valores_propios
```


### Combinaciones Lineales 
```{r warning=FALSE,echo=FALSE}
kable(combinaciones_lineal)
```


$$PC1 = 0.51* Delito Bajo Impacto + 0.48* Violación + 0.54* Robo$$



$$PC2 = 0.45* Homicidio Arma de Fuego + 0.83* Secuestro$$



```{r warning=FALSE,echo=FALSE}
source("ACP_DELITOS.R")
library(factoextra)
fviz_pca_biplot(acp)

fviz_pca_biplot(acp, invisible ="var")
fviz_pca_biplot(acp, invisible ="ind")

```


Variables Latentes
```{r warning=FALSE,echo=FALSE}
kable(Rf)
```
#####################################
##########################################
#######discriminante###################

Definición del LDA
El Análisis Discriminante Lineal o Linear Discrimiant Analysis (LDA) es un método de clasificación supervisado de variables cualitativas en el que dos o más grupos son conocidos a priori y nuevas observaciones se clasifican en uno de ellos en función de sus características. Haciendo uso del teorema de Bayes, LDA estima la probabilidad de que una observación, dado un determinado valor de los predictores, pertenezca a cada una de las clases de la variable cualitativa, $(P(Y = k | X = x))$ . Finalmente se asigna la observación a la clase k para la que la probabilidad predicha es mayor.


Es una alternativa a la regresión logística cuando la variable cualitativa tiene más de dos niveles. Si bien existen extensiones de la regresión logística para múltiples clases, el LDA presenta una serie de ventajas:

-	Si las clases están bien separadas, los parámetros estimados en el modelo de regresión logística son inestables. El método de LDA no sufre este problema.

-	Si el número de observaciones es bajo y la distribución de los predictores es aproximadamente normal en cada una de las clases, LDA es más estable que la regresión logística.

Cuando se trata de un problema de clasificación con solo dos niveles, ambos métodos suelen llegar a resultados similares.

El proceso de un análisis discriminante puede resumirse en 6 pasos:

-	Disponer de un conjunto de datos de entrenamiento (training data) en el que se conoce a que grupo pertenece cada observación.

-	Calcular las probabilidades previas (prior probabilities): la proporción esperada de observaciones que pertenecen a cada grupo.

-	Determinar si la varianza o matriz de covarianzas es homogénea en todos los grupos. De esto dependerá que se emplee LDA o QDA.

-	Estimar los parámetros necesarios para las funciones de probabilidad condicional, verificando que se cumplen las condiciones para hacerlo.

-	Calcular el resultado de la función discriminante. El resultado de esta determina a qué grupo se asigna cada observación.

-	Utilizar validación cruzada (cross-validation) para estimar las probabilidades de clasificaciones erróneas.

Contexto

Según la Organización Mundial de la Salud (OMS), el accidente cerebrovascular es la segunda causa principal de muerte a nivel mundial, responsable de aproximadamente el 11% del total de muertes.
Este conjunto de datos se utiliza para predecir si es probable que un paciente sufra un accidente cerebrovascular en función de los parámetros de entrada como  la edad, el indice de masa corporal y el nivel de glucosa. 

Información de atributos:

-Glucosa: azúcares que se ingieren con los alimentos son transformados por el metabolismo en glucosa. Ésta se desplaza a través del torrente sanguíneo hasta alcanzar las células de diferentes tipos de tejido proporcionando la energía que necesitan para funcionar. 

-Indice de Masa Corporal (IMC): se calcula dividiendo los kilogramos de peso por el cuadrado de la estatura en metros (IMC = peso [kg]/ estatura [m2]). Una persona se considera obesa si su IMC  es mayor a 30 unidades.

-Edad: Variable continua que contabiliza el tiempo de vida de una persona en años.

-Accidente cerebrobascular: Un accidente cerebrovascular sucede cuando el flujo de sangre a una parte del cerebro se detiene. Si el flujo sanguíneo se detiene por más de pocos segundos, el cerebro no puede recibir nutrientes y oxígeno. Las células cerebrales pueden morir, lo que causa daño permanente. Un accidente cerebrovascular se presenta cuando un vaso sanguíneo en el cerebro se rompe, causando un sangrado dentro de la cabeza. 

La base que se utilizara para el analisis discriminante sera la siguiente y se muestran los primeros cinco registros de la base de datos:


```{r echo=FALSE,message=FALSE, warning=FALSE}
library(dplyr)
library(kableExtra)
stroke<- read.csv(header=TRUE,"C:\\Users\\W10\\Desktop\\ESTADISTICA MULTIVARIADA\\ACTIVIDADES\\AVANCE_DE_PROYECTO2\\LDA_STROKES.csv")

stroke1<-stroke[,-1]
stroke1<- stroke1 %>% mutate(Glucosa=as.numeric(Glucosa),Edad=as.numeric(Edad),IMC=as.numeric(IMC),Cerebrovascular=as.factor(Cerebrovascular))

#kable(head(stroke1))
head(stroke1)

```

ANALISIS EXPLORATORIO

En el siguiente gráfico distinguimos que la mayoria de los individuos que han sufrido de un accidente cerebrovascular tienen una edad superior a los 40 años y un IMC entre los 25 y 50 unidades. Sin embargo para el atributo glucosa no se identífica una separacion clara entre los individuos que han sufrido un accidente cerebrovascular (1) de los que no han sufrido un accidente cerebrovascular.


```{r echo=FALSE,message=FALSE, warning=FALSE}
library(ggplot2)

stroke1%>% mutate(Glucosa= as.factor(if_else(Glucosa<120,"<120",">120")))%>%
ggplot(aes(Edad,IMC,color=Glucosa))+geom_point(alpha=1/2)+facet_wrap(~Cerebrovascular)+theme_bw()+labs(title="Accidente Cerebrovascular, nivel de glucosa y IMC",
                                                                                                       subtitle="1- Sufrio Accidente cerebrovascular\n0-No ha sufrido accidente cerebrovascular",caption="Fuente: Elaboración propia, utilizando R-Project version 4.0.4")

```


Asi tambien en la siguiente grafica se distingue las individuos que han sufrido un accidente cerebrovascular marginalmente bien en el grupo de índice de masa corporal esto debido a la poca cantidad de casos registrados en la base de datos de individuos con accidente cerebrovascular.  


```{r echo=FALSE,message=FALSE}
library(ggpubr)


p1 <- ggplot(data = stroke1, aes(IMC, fill = Cerebrovascular)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = stroke1, aes(Edad, fill = Cerebrovascular)) +
  geom_histogram(position = "identity", alpha = 0.5)
p3 <- ggplot(data = stroke1, aes(Glucosa, fill = Cerebrovascular)) +
  geom_histogram(position = "identity", alpha = 0.5)+labs(caption=)

graph1<-ggarrange(p1, p2, p3, nrow = 3, common.legend = TRUE)
graph1

```




Asi tambien en la visualizacion siguiente se distingue marginalmente los grupos en la interaccion de glucosa y edad de los individuos. Sin embargo como se menciono anteriormente la poca cantidad de individuos con accidente cerebrovascular limita la adecuada distinción de los individuos con la característica de interes.




```{r echo=FALSE,message=FALSE, warning=FALSE}

pairs(x = stroke1[, c("Edad","IMC","Glucosa")],
      col = c("blue", "green3")[stroke1$Cerebrovascular], pch = 15)
```


Asi tambien para el siguiente gráfico se presenta el mismo fenómeno anterior del gráfico uno donde se distinguen las personas con accidente cerebrovascular con edad mayor a 40 años y glucosa mayor a 100mg/lt.


```{r echo=FALSE,message=FALSE, warning=FALSE}
library(scatterplot3d)
datos<-stroke1
scatterplot3d(datos$Edad, datos$IMC, datos$Glucosa,
              color = c("firebrick", "green3")[datos$Cerebrovascular], pch = 19,
              grid = TRUE, xlab = "Edad", ylab = "IMC",
              zlab = "Glucosa", angle = 65, cex.axis = 0.6)
legend("topleft",
       bty = "n", cex = 0.8,
       title = "Cerebrovascular",
       c("No ha sufrido", "Si ha sufrido"), fill = c("firebrick", "green3"))
```


Fuente:Elaboración propia con R-Project 4.0.5

```{r echo=FALSE,message=FALSE, warning=FALSE}

stroke2<-stroke1

```



NORMALIDAD UNIVARIADA SHAPIRO-WILK

H0: Las variables individuales se distribuyen de manera normal.

H1: Las variables individuales no se distribuyen de manera normal.


De acuerdo a la prueba de normalidad existe evidencia para rechazar H0 para los grupos de Edad, Glucosa y IMC para el grupo de individuos que no han sufrido un accidente cerebrovascular. A su vez no se rechaza H0 para IMC del grupo de individuos que han sufrido un accidente cerebrovascular.De acuerdo a  lo anterior con un intervalo de confianza al 95% se comprueba que las variables de glucosa y IMC para los dos grupos de individuos y IMC para los individuos que no han sufrido accidente cerebrovascular no siguen una distribución normal.



Pruba de Sahpiro-Wilk
```{r echo=FALSE,message=FALSE, warning=FALSE}
library(reshape2)
library(knitr)
library(dplyr)

datos_tidy <- melt(stroke2, value.name = "valor")
hipotesis<-aggregate(formula = valor ~ Cerebrovascular + variable, data = datos_tidy,
          FUN = function(x){shapiro.test(x)$p.value})
#kable(hipotesis)
hipotesis

```


```{r echo=FALSE,message=FALSE , warning=FALSE}
library(ggpubr)
nsov1<-
stroke1%>% filter(Cerebrovascular!=1)%>%
  ggplot(aes(Glucosa, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density()+theme_bw()
nsov2<-
stroke1%>% filter(Cerebrovascular!=1)%>%
  ggplot(aes(Edad, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density()+theme_bw()

nsov3<-
stroke1%>% filter(Cerebrovascular!=1)%>%
  ggplot(aes(IMC, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density()+theme_bw()
PLOT1<-ggarrange(nsov1, nsov2, nsov3, nrow = 3, common.legend = TRUE)
PLOT1

#No sobrevivientes 
nsov4<-
stroke1%>% filter(Cerebrovascular==1)%>%
  ggplot(aes(Glucosa, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density()+theme_bw()
nsov5<-
stroke1%>% filter(Cerebrovascular==1)%>%
  ggplot(aes(Edad, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density()+theme_bw()

nsov6<-
stroke1%>% filter(Cerebrovascular==1)%>%
  ggplot(aes(IMC, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density()+theme_bw()
PLOT2<-ggarrange(nsov4, nsov5, nsov6, nrow = 3, common.legend = TRUE)
PLOT2


```





NORMALIDAD MULTIVARIADA 

H0: Los datos presentan normalidad multivariada.

H1: Los datos no presentan normalidad multivariada.

De acuerdo a la prueba de Royston se obtuvo un valor p de 0.000 se concluye con un intervalo al 95% de confianza que los datos no provienen de una normal multivariada.


Prueba de Royston
```{r echo=FALSE,message=FALSE, warning=FALSE}
library(MVN)
#cat("PRUEBA DE ROYSTON")
royston_test <- mvn(data = stroke2[,-4], mvnTest = "royston")
#kable(royston_test$multivariateNormality)
hz_test <- mvn(data = stroke2[,-4], mvnTest = "hz")
#kable(hz_test$multivariateNormality)
royston_test



```


NORMALIDAD MULTIVARIADA
```{r echo=FALSE,message=FALSE, warning=FALSE, tidy=TRUE}
library(MVN)
royston_test <- mvn(data = stroke2[,-4], mvnTest = "royston", multivariatePlot = "qq")

```




Fuente:Elaboración propia con R-Project 4.0.5









PRUEBA DE HOMOGENEIDAD DE VARIANZAS
Ho: las matrices de varianzas y covarianzas son iguales.

Ha: al menos una es diferente.

De acuerdo a la prueba M de Box se presenta evidencia para rechazar la hipotesis nula de igualdad de las matrices de varianza y covarianza con un valor p de 2.08e-12. De acuerdo a lo anterior se concluye que las matrices de varianza y covarianzas no son iguales con un intervalo de confianza al 95%.


```{r echo=FALSE,message=FALSE, warning=FALSE}
library(biotools)
hipo_box<-boxM(data = stroke2[,1:3], grouping = stroke2$Cerebrovascular)

hipo_box



```




Outliers
```{r echo=FALSE,message=FALSE, warning=FALSE}
library(MVN)
outliers <- mvn(data = stroke2[,-4], mvnTest = "hz", multivariateOutlierMethod = "quan")
cat("Fuente: Elaboración propia con R-Project versión 4.0.5")
#kable(outliers)
#kable(outliers$multivariateNormality)
#kable(outliers$univariateNormality)
#kable(outliers$Descriptives)

```







De acuerdo a lo anterior se procedera con un analisis discriminante cuadratico ya que se rechazó la hipotesis de normalidad multivariada con un valor p de 0.0000 y un intervalo de confianza al 95%. Asi tambien se rechazo la hipotesis de homogeneidad de las varianzas con un valor p de 0.000 y un intervalo de confianza al 95%.






```{r echo=FALSE,message=FALSE , warning=FALSE}
library(MASS)
modelo_qda <- qda(Cerebrovascular ~ Edad + IMC+Glucosa, data = stroke2)
modelo_qda

predicciones <- predict(object = modelo_qda, newdata = stroke2)
RF<-data.frame(stroke2$Edad,stroke2$IMC,stroke2$Glucosa,predicciones$class,predicciones$posterior)
RF
table(stroke2$Cerebrovascular, predicciones$class,dnn = c("Clase real", "Clase predicha"))

trainig_error <- mean(stroke2$Cerebrovascular != predicciones$class) * 100
paste("trainig_error=",trainig_error,"%")

nuevas_observaciones <- data.frame(IMC = 40,Glucosa= 130,
                                Edad= 50)
predi<-predict(object = modelo_qda, newdata = nuevas_observaciones)

```

```{r}
library(klaR)
partimat(formula = Cerebrovascular ~ Edad +Glucosa+IMC, data = datos,
         method = "qda", prec = 400,
         image.colors = c("darkgoldenrod1", "skyblue2"),
         col.mean = "firebrick")
```

#####################################################
#########CLUSTER#####################################

```{r echo=FALSE, message=FALSE,tidy=TRUE, warning=FALSE}
library(dplyr)
melbourne<- read.csv(header=TRUE,"C:\\Users\\W10\\Desktop\\ESTADISTICA MULTIVARIADA\\ACTIVIDADES\\AVANCE_DE_PROYECTO2\\K_MEDIAS.csv")
melbourne<-na.omit(melbourne)

melbourne1<- select(melbourne,-Type,-Method,-SellerG,-Date,-Postcode,-YearBuilt,-CouncilArea,-Lattitude,-Longtitude,-X)
melbourne1<-melbourne1 %>% mutate(Direccion=Address,Recamaras=Rooms,Precio=Price,
                                  Distancia_c=Distance,Dormitorios=Bedroom2,Sanitarios=Bathroom,
                                  Cocheras=Car,Tam_propiedad=Landsize,Tam_edificio=BuildingArea,N_propiedades=Propertycount)

melbourne1 <-select(melbourne1,Regionname,Direccion,Recamaras,Precio,Distancia_c,Dormitorios,Sanitarios,Cocheras,Tam_propiedad,Tam_edificio,N_propiedades)

melbourne2<- dplyr::select(melbourne1,Regionname,Direccion,Recamaras,Precio,Distancia_c,Dormitorios,Sanitarios,Cocheras,Tam_propiedad,Tam_edificio)
kable(summary(melbourne2[,-1]))

kable(head(melbourne2))
head(melbourne1)

ggplot(melbourne2,aes(Recamaras,y=(..count..)/sum(..count..),fill=Regionname))+geom_bar()+
  scale_y_continuous(labels=scales::percent)+theme_bw()+
  labs(title="Recamaras",subtitle = "Descriptivos: \nMinimo: 1 recamara\nMediana:3 recamaras\nMaximo:8 recamaras",y="Porcentaje",x="Número de Recamaras",caption="Fuente:Elaboración propia con R 4.0.4")+guides(fill=FALSE)+facet_wrap(~Regionname)


melbourne2%>%mutate(Precio1=if_else(Precio<2000000,"Menor a $$2,000,000","Mayor a $$2,000,000"))%>%
ggplot(aes(Precio,y=(..count..)/sum(..count..),fill=Regionname))+geom_histogram()+
  scale_y_continuous(labels=scales::percent)+theme_bw()+
  labs(title="Precio de Alquiler",subtitle = "Descriptivos: \nMinimo: 280,000 dolares \nQ1:668,250 dolares\nMedia:1,105,295 dolares\nQ3:1,352,750 dolares\nMaximo: 5,600,000 dolares",y="Porcentaje",x="Precio en Dolares Australianos",caption="Fuente:Elaboración propia con R 4.0.4")+#guides(fill=FALSE)#+
  facet_wrap(~Precio1,scales = "free")+coord_flip()

ggplot(melbourne2,aes(Recamaras,y=(..count..)/sum(..count..),fill=Regionname))+geom_bar()+
  scale_y_continuous(labels=scales::percent)+theme_bw()+
  labs(title="Kilometros del Parque Central",subtitle = "Descriptivos: \nMinimo: 1.20 km recamara\nQ1:6.1 km recamaras\nPromedio:10.21 km\nQ3: 13 km\nMaximo:41 km",y="Porcentaje",x="Kilometros de Parque Central",caption="Fuente:Elaboración propia con R 4.0.4")+guides(fill=FALSE)+facet_wrap(~Regionname)

ggplot(melbourne2,aes(Sanitarios,y=(..count..)/sum(..count..),fill=Regionname))+geom_bar()+
  scale_y_continuous(labels=scales::percent)+theme_bw()+
  labs(title="Sanitarios",subtitle = "Descriptivos: \nMinimo: 1 \nMediana:2 \nMaximo:4",y="Porcentaje",x="Número de Sanitarios",caption="Fuente:Elaboración propia con R 4.0.4")+guides(fill=FALSE)+facet_wrap(~Regionname)


ggplot(melbourne2,aes(Cocheras,y=(..count..)/sum(..count..),fill=Regionname))+geom_bar()+
  scale_y_continuous(labels=scales::percent)+theme_bw()+
  labs(title="Estacionamientos",subtitle = "Descriptivos: \nMinimo: 0 \nMediana:2 \nMaximo:6",y="Porcentaje",x="Número de Estacionamientos",caption="Fuente:Elaboración propia con R 4.0.4")+guides(fill=FALSE)+facet_wrap(~Regionname)

ggplot(melbourne2,aes(Cocheras,y=(..count..)/sum(..count..),fill=Regionname))+geom_bar()+
  scale_y_continuous(labels=scales::percent)+theme_bw()+
  labs(title="Estacionamientos",subtitle = "Descriptivos: \nMinimo: 0 \nMediana:2 \nMaximo:6",y="Porcentaje",x="Número de Estacionamientos",caption="Fuente:Elaboración propia con R 4.0.4")+guides(fill=FALSE)+facet_wrap(~Regionname)


ggplot(melbourne2,aes(Tam_edificio,y=(..count..)/sum(..count..),fill=Regionname))+geom_histogram()+
  scale_y_continuous(labels=scales::percent)+theme_bw()+
  labs(title="Tamaño de Construcción en Metros Cuadrados",subtitle = "Descriptivos: Minimo: 0 m2|Q1:95.75 m2|Media:148.42 m2|Q3:176 m2|Maximo: 1022 m2",y="Porcentaje",x="Tamaño en m2",caption="Fuente:Elaboración propia con R 4.0.4")+guides(fill=FALSE)+
  facet_wrap(~Regionname)+coord_flip()



```




```{r echo=FALSE, warning=FALSE,message=FALSE}
library(ggplot2)

melbourne%>% mutate(Price=Price/1000000,Habitaciones=as.factor(Rooms),Cocheras=as.numeric(Car))%>%
ggplot(aes(Price,BuildingArea,shape=Habitaciones,color=Cocheras))+geom_point(alpha=1/2)+facet_wrap(~Regionname,nrow=4)+labs(
  x="Precio en Millones de Dolares",y="Tamaño de la Construcción m2",caption="Fuente: Elaboracion propia con software R-Project 4.0.4",title="",subtitle=""
)+theme_bw()

```




```{r echo=FALSE}
Y=scale(melbourne1[,2:10]) # Tipificamos la matriz de datos y la llamamos Y

d<-dist(Y)^2 # comando para obtener las distancias euclidias
#head(d)

```

De acuerdo a la ilustración siguiente se muestra el número optimo de cluster para el analisi de k-medias. Es asi que de acuerdo a los resultados del gráfico se procedera a aplicar el analisis de agrupación K-medias con tres clusters ya que es el número optimo con el cual se conservara la mayor aportación de varianza a el analisis de agrupación.

```{r echo=FALSE,message=FALSE,warning=FALSE}
library(cluster)
library(factoextra)
d2 <- scale(melbourne1[,2:10])
#rownames(d2) <- d2$Species
fviz_nbclust(x = d2, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(d2, method = "euclidean"), nstart = 50)


```



```{r echo=FALSE,message=FALSE,warning=FALSE}
d2f=data.frame(d2)
km_clusters <- kmeans(x = d2f, centers = 3, nstart = 50)

# Las funciones del paquete factoextra emplean el nombre de las filas del
# dataframe que contiene los datos como identificador de las observaciones.
# Esto permite añadir labels a los gráficos.
fviz_cluster(object = km_clusters, data = d2f, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE,
             pointsize=0.5,outlier.color="darkred") +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +  theme(legend.position = "none")
```


```{r echo=FALSE,message=FALSE,warning=FALSE}
set.seed(20)
k.means.fit <-kmeans(melbourne1[,2:9], 3, nstart = 10)
k.means.fit


```


```{r echo=FALSE,message=FALSE,warning=FALSE}
library(kableExtra)
kable(k.means.fit$centers)
```

```{r echo=FALSE,message=FALSE,warning=FALSE}
grupos=k.means.fit$cluster
b<-data.frame(grupos)
b

p<-merge(x = melbourne1, y = b, by = c("row.names")) 
p%>% mutate(grupos=as.factor(grupos))%>%
ggplot(aes(Precio,Tam_edificio,color=grupos,fill=grupos))+geom_point()+labs(title="Grupos Resultantes",x="Precio en Dolares Australianos",y="Tamaño de construccción m2")


```







##############################################
#############################################
################Correspondencia multiple#################
library("FactoMineR")
library("factoextra")
library("ggplot2")
library(leaps)
library(ggrepel)
library(flashClust)
library(dplyr)
datos <- read.table("C://Users//kevin//Downloads//data_MCA_Hobbies_Ana_Yusleidy__Miguel.csv", header = TRUE, sep=";")
datos
 datos[,"TV"] = as.factor(datos[,"TV"])
summary(datos[,"TV"])
res.mca <- MCA(datos,quali.sup = 19:22,quanti.sup = 23)
 res.mca$eig
eig.val <- res.mca$eig
eig.val <- get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 20))
#--- para las categor?as de las variables:
res.mca$var
res.mca$var$contrib
#--- para los individuos:
res.mca$ind
res.mca$ind$contrib
##visualizar la matriz de Burt
res.mca$call
#__
#Pesos de las columnas
res.mca$call$marge.col
##contribuciones
# Contributions of rows to dimension 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 20)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 20)
#--- para los individuos:
#--- para las categor?as de las variables:
fviz_mca_var(res.mca, repel = TRUE)
#Se pueden seleccionar s?lo algunas categorias de variables:
#--- las 10 m?s importantes
fviz_mca_var(res.mca, select.var = list(contrib = 30))
#--- y la visualizaci?n conjunta:
fviz_mca_biplot(res.mca, repel = TRUE) + theme_minimal()
##Plot de individuos y variables que muestra el v?nculo entre ellos.
plot(res.mca, autoLab = "yes")

d<-read.csv("C:\\Users\\W10\\Downloads\\data_MCA_Hobbies_Ana_Yusleidy__Miguel.csv",header = TRUE)
library(tidyverse)
d<- d %>%separate(Reading.Listening.music.Cinema.Show.Exhibition.Computer.Sport.Walking.Travelling.Playing.music.Collecting.Volunteering.Mechanic.Gardening.Knitting.Cooking.Fishing.TV.Sex.Age.Marital.status.Profession.nb.activitees,
                  into=c("leer","escuchar_musica","cine","eventos","exposiciones","computadora","deportes","caminar","viajar","tocar_musica","coleccionar","voluntarios","mecanica","jardineria","coser","cocinar","pescar","TV","sex","edad"),sep=";")

d<- d %>% separate(X,into=c("edad2","civil_status","profesion","nb_activities"),sep=";")
d<-d%>%unite("d",edad,edad2,sep="-")
#"d<-dplyr::select(d,-edad,-edad2)


library("FactoMineR")
library("factoextra")
#install.packages("ggplot2")
library("ggplot2")
library(leaps)
library(ggrepel)
library(flashClust)
library(dplyr)

# Con esto vamos a quitar la columna de numeraci?n y como 
# nombre se pondr?n a los alumnos
rownames(d) <- d$nd_activities
d
# Quitar la variable Alumnos al conjunto de datos
datos <- d[,-23]
datos

#MCA-Alumnos
res.mca <- MCA(datos, quanti.sup =FALSE, quali.sup = 3:4, graph = F)
res.mca$eig
eig.val <- res.mca$eig
eig.val <- get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 20))
#--- para las categor?as de las variables:
res.mca$var
res.mca$var$contrib
#--- para los individuos:
res.mca$ind
res.mca$ind$contrib
##visualizar la matriz de Burt
res.mca$call
#____
#Pesos de las columnas
res.mca$call$marge.col
##contribuciones
# Contributions of rows to dimension 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 20)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 20)
#--- para los individuos:
#--- para las categor?as de las variables:
fviz_mca_var(res.mca, repel = TRUE)
#Se pueden seleccionar s?lo algunas categorias de variables:
#--- las 10 m?s importantes
fviz_mca_var(res.mca, select.var = list(contrib = 30))
#--- y la visualizaci?n conjunta:
fviz_mca_biplot(res.mca, repel = TRUE) + theme_minimal()
##Plot de individuos y variables que muestra el v?nculo entre ellos.
plot(res.mca, autoLab = "yes")
#################################################
##############################################
#################################################
######FACTORIAL###############################
##################################################

```{r echo=FALSE, message=FALSE,warning=FALSE, tidy=TRUE}

library(readxl)
#library(readr)
library(psy)
library(corrplot)
library(psych)
library(semPlot)
library(readr)
library(lavaan)
library(tidyverse)
library(dplyr)
```



Depressive Mood Scale
Description
A data frame with 269 observations on the following 20 variables. Jouvent, R et al 1988 La clinique polydimensionnelle de humeur depressive. Nouvelle version echelle EHD : Polydimensional rating scale of depressive mood. Psychiatrie et Psychobiologie.

Usage
data(ehd)
Format
This data frame contains the following columns:

e1
Observed painfull sadness

e2
Emotional hyperexpressiveness

e3
Emotional instability

e4
Observed monotony

e5
Lack spontaneous expressivity

e6
Lack affective reactivity

e7
Emotional incontinence

e8
Affective hyperesthesia

e9
Observed explosive mood

e10
Worried gesture

e11
Observed anhedonia

e12
Felt sadness

e13
Situational anhedonia

e14
Felt affective indifference

e15
Hypersensibility unpleasent events

e16
Sensory anhedonia

e17
Felt affective monotony

e18
Felt hyperemotionalism

e19
Felt irritability

e20
Felt explosive mood

Source
Jouvent, R et al 1988 La clinique polydimensionnelle de humeur depressive. Nouvelle version echelle EHD : Polydimensional rating scale of depressive mood. Psychiatrie et Psychobiologie.


  Exploratorio 

Base de datos ehd 
```{r echo=FALSE, tidy=TRUE}
data(ehd)
#datos <- read_xlsx("C:\\Users\\W10\\Desktop\\ESTADISTICA MULTIVARIADA\\ACTIVIDADES\\AF\\FA.xlsx")
datos<- ehd
#________________________________________
# estandarizar variable
D1=scale(datos)
#que R lo lea como una matriz de datos y no como una tabla
D2=as.data.frame(D1)
head(D2)
```


```{r echo=FALSE, tidy=TRUE}
  D2%>% pivot_longer(e1:e12)%>% filter(value>0)%>%
  ggplot(aes(value))+geom_histogram(binwidth = 1,alpha=1/4,fill = "darkblue",
                                    colour = "black")+facet_wrap(~name)+
  labs(subtitle="",title="Escala de Estado de Animo Depresivo",x="Categoria de Respuestas",y="Frecuencia",
       caption="Fuente de los datos: Jouvent, R et al 1988 La clinique polydimensionnelle de humeur depressive.\n Nouvelle version echelle EHD : Polydimensional rating scale of depressive mood.\n Psychiatrie et Psychobiologie.")+
  theme_bw()


  D2%>% pivot_longer(e13:e20)%>% filter(value>0)%>%
  ggplot(aes(value))+geom_histogram(binwidth = 1,alpha=1/4,fill = "darkblue",
                                    colour = "black")+facet_wrap(~name,ncol=4)+
  labs(subtitle="",title="Escala de Estado de Animo Depresivo",x="Categoria de Respuestas",y="Frecuencia",
       caption="Fuente de los datos: Jouvent, R et al 1988 La clinique polydimensionnelle de humeur depressive.\n Nouvelle version echelle EHD : Polydimensional rating scale of depressive mood. \nPsychiatrie et Psychobiologie.")+
  theme_bw()



```


Matriz de correlacion

```{r echo=FALSE, tidy=TRUE}
cor(D2)
```

```{r echo=FALSE, tidy=TRUE}
corrplot(cor(D2), method="circle",order="hclust",tl.col='black',tl.cex=.75)
         
```



DETERMINANTE
```{r echo=FALSE, tidy=TRUE}
det(cor(D2))
```

KMO TEST 
```{r echo=FALSE, tidy=TRUE}
KMO(cor(D2))
```


ESFERICIDAD
```{r echo=FALSE, tidy=TRUE}
print(cortest.bartlett(cor(D2),nrow(datos)))
         
```

De acuerdo a las cargas factoriales de los items del cuestionario se opto por omitir de las variables originales las preguntas 8,9,10 y 14 ya que su carga factorial es cercano a .30 por lo que es posible omitirlas del analisis confirmatorio ya que aportan poca informacion al intrumento de medición.

```{r echo=FALSE, tidy=TRUE}
##ROTACIÓN VARIMAX##
         FA1=factanal(D2,factors=2,rotation="varimax",na.action=na.omit,scores="Bartlett")
         LO2=FA1$loadings
         #LO2# Calcular el número de factores ideal
        VARIMAX_IDFEAL<- scree.plot(datos,type = 'R')
         #scoores
         #variables latentes nuevas
         SC1=FA1$scores
         SCF1<-SC1[,1]
        HISTOGRAMA<- hist(SCF1)
        CAJAS<- boxplot(SCF1)
         #summary(SCF1)
        
        LO2
```

```{r echo=FALSE, tidy=TRUE}
 #BIPLOT para variables con FA1
         load1=FA1$loadings[,1:2]
         load2=FA1$loadings[,1:2]
         BIPLOT_FINAL<-biplot_graph<-plot(load1)
         text(load1,labels=names(D),cex=.7,adj=c(0.5,1))
         
        BIPLOT2<- biplot (load1,load2)
         text(load1,labels=names(D),cex=.7,adj=c(0.5,1))
```


MODELO CONFIRMATORIO 

En el modelo confirmatorio se puede probar que los dos grupos en los que se conformo el cuestionario depresion y ansiedad son utiles para la evaluacion del humor depresivo.Los items en seleccionados muestran una alta aportacion de informacion asi como tambien las varianzas de los items por lo cual el cuestionario es adecuado para aplicación.
```{r, tidy=TRUE}
 modelo_confirm<-'Ansiedad=~ e2 +e3+e4+e5+e6+e7+e15+e18+e19+e20
Depresion=~ e1+e11+e12+e13+e16+e17'
         modelo<-cfa(modelo_confirm,data=D2)
        summary(modelo,fit.measures=TRUE)
         GRAFICOS<-semPaths(modelo,what="paths",layout="circle",title=TRUE,style="LISREL")
         #semPaths(modelo,what="est",layout="circle",title=TRUE,style="LISREL")
         
```



Encuesta de Trabajo 
```{r echo=FALSE, tidy=TRUE}
#data(ehd)
datos <- read_xls("C:\\Users\\W10\\Downloads\\Copia_de_Ejercicio_factorial_laboral_1.xls")
```


```{r echo=FALSE, tidy=TRUE}
datos <- read_xls("C:\\Users\\W10\\Downloads\\Copia_de_Ejercicio_factorial_laboral_1.xls")
datos<- datos[,9:38]
# estandarizar variable
D1=scale(datos)
#que R lo lea como una matriz de datos y no como una tabla
D2=as.data.frame(D1)
head(D2)
#______________________________________________
#instalar paquete##
#install.packages("corrplot")
#LA MATRIZ DE ENTRADA ES DE CORRELACION 
MATRIZ_CORRELACION<-cor(D2)
```

```{r echo=FALSE, tidy=TRUE}
#library(corrplot)
GRAFICO_CORRRELACION<-corrplot(cor(D2), method="circle",order="hclust",tl.col='black',tl.cex=.75)


```

DETERMINANTE

```{r echo=FALSE, tidy=TRUE}
#determinante
determinante<-det(cor(D2))
determinante
```

KMO


```{r echo=FALSE, tidy=TRUE}
KMO_TEST<-KMO(cor(D2))

#Prueba de esfericidad de barlet 
KMO_TEST
```

ESFERICIDAD

```{r echo=FALSE, tidy=TRUE}

esfericidad<-print(cortest.bartlett(cor(D2),nrow(datos)))
esfericidad
```


CARGAS FACTORIALES 
De acuerdo a los valores de las cargas factoriales se omitieron del analisis las preguntas numero 3,13,16 y 28 ya que aportaban su apoprte a la carga factorial era menor a .30.


```{r echo=FALSE, tidy=TRUE}
FA=factanal(D2,factors=2,rotation="none",na.action=na.comit) #modelo
#FA
LO=FA$loadings 
U<-FA$uniquenesses    #valores singulares
#________________________________________---
# Calcular el número de factores ideal
FACTOR_IDEAL<- scree.plot(datos,type = 'R') 

LO
```

EIGENVALORES 
```{r echo=FALSE, tidy=TRUE}
FA=factanal(D2,factors=2,rotation="none",na.action=na.comit) #modelo
#FA
LO=FA$loadings 
U<-FA$uniquenesses    #valores singulares
#________________________________________---
# Calcular el número de factores ideal
FACTOR_IDEAL<- scree.plot(datos,type = 'R') 
U

```


grafica de factors

```{r echo=FALSE, tidy=TRUE}
LO1=FA$loadings[,1:2]
GRAFICO_FACTORES<-plot(LO1)
text(LO1,labels = names(D),cex =.7,adj = c(0.5,1))
```

VARIABLES LATENTES NUEVAS 


```{r echo=FALSE, tidy=TRUE}
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


HISTOGRAMA
CAJAS<- boxplot(SCF1)
CAJAS
```



BIPLOT 


```{r echo=FALSE, tidy=TRUE}
load1=FA1$loadings[,1:2]
load2=FA1$loadings[,1:2]
BIPLOT_FINAL<-biplot_graph<-plot(load1)
text(load1,labels=names(D),cex=.7,adj=c(0.5,1))

BIPLOT2<- biplot (load1,load2)
text(load1,labels=names(D),cex=.7,adj=c(0.5,1))

```


MODELO CONFIRMATORIO 



```{r , tidy=TRUE}
modelo_confirm<-'G1=~ P4+P9+P10+P11+P12+P15+P17+P18+P19+P21+P22+P23+P24+P25+P26
G2=~ P1+P2+P5+P6+P7+P8+P14+P20+P27+P29+P30'
modelo<-cfa(modelo_confirm,data=D2)
 summary(modelo,fit.measures=TRUE)
GRAFICOS<-semPaths(modelo,what="paths",layout="circle",title=TRUE,style="LISREL")
```

###########################################################
####################CORRELACION CANONICA##################
###########################################################
###########################################################

```{r}
require(ggplot2)
require(GGally)
require(CCA)
library(dplyr)
library(kableExtra)
library(tidyverse)


mm <- read.csv("https://stats.idre.ucla.edu/stat/data/mmreg.csv")
colnames(mm) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", 
                  "Science", "Sex")
summary(mm)

colnames(mm) <- c("Locus", "Autoconcepto", "Motivacion", "Lectura", "Escritura", "Matematicas", 
                  "Ciencias", "Sexo")
summary(mm)

#write.csv(mm,file="BASE_CANONICA_PROYECTO.CSV")
mm%>% pivot_longer(Locus:Sexo) %>%
  ggplot(aes(name,value,fill=name))+
  geom_boxplot(alpha=2/3,outlier.shape = 16)+
  guides(fill=FALSE)+theme_bw()+
  guides(colour=FALSE,fill=FALSE)+
  coord_flip()+
  labs(title="Variables psicologicas y academicas",
       caption="Fuente:Elaboración propia.",
       x="Puntuación",y="Variables")

ggduo(mm,columnsX = 1:3,columnsY = 4:8,
      types = list(continuous = "smooth_lm"),
      title = "Correlación entre variables Psicologicas y  Academicas",
      xlab = "Variables Psicológicas",
      ylab = "Academicas"
)



psych <- mm[, 1:3]
acad <- mm[, 4:8]
library(GGally)
ggpairs(psych)
ggpairs(acad)
# correlations
library(CCA)
M<-matcor(psych, acad)
cc1 <- cc(psych, acad)


cc1$cor
cc1[3:4]
cc2 <- comput(psych, acad, cc1)
cc2[3:6]

kable(cc2$corr.Y.xscores)
kable(cc2$corr.X.yscores)

kable(M$Ycor)
kable(M$Xcor)
kable(M$XYcor)


kable(cc1$scores)
#M<-matcor(x,y)  
#M 

#CC <- cc (x, y) 
#CC
# Variables canónicas
#plot(cc1)
#gala$cor
img.matcor(M, type = 2) 
barplot(cc1$cor,ylim = c(0,1)) 
plt.cc(cc1,var.label=T)
#cc2 <- cca(psych, acad)
#plot(cc2,var.label=F)


```



