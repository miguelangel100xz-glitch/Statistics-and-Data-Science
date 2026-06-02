
##datos vía clipboard##
# llamar a los datos
datos <- read.delim('clipboard', stringsAsFactors=TRUE)
datos
# Con esto vamos a quitar la columna de numeración y como 
# nombre se pondrán los nombres de los países
rownames(datos) <- datos$Alumnos
datos
# Quitar la variable Paises al conjunto de datos
datos1 <- datos[,-1]
datos1
________________________________
##DATOS EXTRAÍDOS RED R##
url <- "http://www.uv.es/imeq/files/ozovlc.txt"
ozovlc <- read.table(url, header = T)
ozovlc <- ozovlc [,c(3,1,2,4)]

names (ozovlc) <- c ("Ozono","Temp","Veloc","RSolar")
::::::::::::::::::::::::::::::::::::::::::::::::::::
# Obtener el diagrama de cajas para mirar las varianzas 
y decidir si usamos MR o MS##
boxplot(ozovlc)
cov(ozovlc)
::::::::::::::::::::::::::::::::::::::::::::::::::::
# Obtener matriz R (correlaciones)##
MR <- cor(ozovlc)
MR
#Vizualización#
#install.packages("corrplot")
library (corrplot)
S<-cor(ozovlc, use="complete.obs")
corrplot.mixed(S)
::::::::::::::::::::::::::::::::::::::::::::::::::::
# Supuestos de colinealidad para ver si es factible el analisis de componentes principales
# 1. Determinante de la matriz correlaciones
# det = 0 indica alta multicolinealidad entre las variables y entonces no es factible el análisis
det(MR)
# instalar paquete psych para obtener KMO y prueba de Bartlett.
#$install.packages('psych')
library(psych)
# Prueba de Kaiser-Meyer-Olkin (KMO)
KMO(MR)

# Test de esferacidad de Bartlett que busca H0: MR = MI vs H1: Son diferentes, lo que
# interesa es rechazar H0.
print(cortest.bartlett(MR, nrow(ozovlc)))
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  

  #___________"""""""""""""""""""""""""""""""""""""""""""""""""2222222
  #-###################################################################
#######################################################################
   Cálculo de componentes con Matriz de correlación
acp <- prcomp(ozovlc, scale=TRUE)
# imprimir el análisis de componentes principales
print(acp)
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #####################################################################
#######################################################################





___________________________________________________________________________
##Correlación de variables y componentes usando S
diag(1/sqrt(diag(cov(ozovlc)))) %*% acp$rotation %*% diag(acp$sdev)
_________________________________________________________________________
##Correlación de variables y componentes usando R
(corvar <- acp$rotation %*% diag(acp$sdev))
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
###NOTA##
# La carga del factor es la correlación existente entre una variable original
# y el componente.

# El componente se interpreta en función de las variables más correlacionadas
# con PCl. El primer factor combina la agricultura y la pone en contraposición a
# la Manufactura, la Construcción, Servicios, Seguridad Social. 

# El número de componentes a retener son todos aquellos que su eigenvalor
# sea mayor que 1.

################################################################33
##################################################################
###################################################################

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Ver varianza acumulada
summary(acp)


##Coordenadas Nuevas##
acp$x
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  ########################################################################
  ########################################################################
##########################################################################

##Gráfico de sedimentación (Codo)##
#install.packages("factoextra")
library(factoextra)
fviz_eig(acp)


###########################################################################
###########################################################################

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
##Gráfico de individuos con un perfil similar (agrupaciones)##
fviz_pca_ind(acp,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Default plot
fviz_pca_var(acp)
##Gráfica de variables. Las variables correlacionadas positivas 
apuntan al mismo lado de la gráfica. Las variables negativas
correlacionadas apuntan a lados opuestos del gráfico.##

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##Biplot
biplot(acp)
#factoextra
fviz_pca_biplot(acp)
#biplot variables
fviz_pca_biplot(acp, invisible ="ind")
#biplot individuos
fviz_pca_biplot(acp, invisible ="var")
##biplot  con  factoextra##
# Seleccionando a los indiviudos
fviz_pca_biplot(acp, label="var",
               select.ind = list(contrib = 100))
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
##########################################################################
##########################################################################
##########################################################################


# En la salida de los componentes principales cada columna representa una
# combinación lineal de las variables originales Ej.
# Zi = .5231*AGR + .001*Min - 0.34*MAN - 0.2554*ENER + ... - 0.366*TC
# De esta manera se calcula el PC1 para cada país, luego se toma el PC2, y luego PC3.

# Se calculan las componentes 
pc1 <- apply(acp$rotation[,1]*ozovlc, 1, sum)
pc2 <- apply(acp$rotation[,2]*ozovlc, 1, sum)
pc3 <- apply(acp$rotation[,3]*ozovlc, 1, sum)
________________________________________________________________________
# las integro a mi conjunto de datos
ozovlc$pc1 <- pc1
ozovlc$pc2 <- pc2
ozovlc$pc3 <- pc3
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
Rf<-data.frame(ozovlc$pc1,ozovlc$pc2)
Rf             
hist(ozovlc$pc2)
shapiro.test(ozovlc$pc2)
#######################################################################3

acp$rotation



