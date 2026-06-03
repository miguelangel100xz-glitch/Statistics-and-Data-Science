#multidimensional scaling <3

a <-read.csv( "C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\MINERIA\\ProyectoFinal\\Datasets\\possum (2).csv"
) # llamamos el archivo
a<-dplyr::select(a,-case,-Pop,-sex,-age)
#tipificar datos
datos<-scale(a[2:10])
datos<-as.data.frame(datos)


#obtener las distancias euclideas
dist_euc<-dist(datos, method="euclidean", diag=T, upper=T)
M<-as.matrix(dist_euc)
rownames(M)<-paste(a$site)
colnames(M)<-paste(a$site)
euclideas<-as.dist(M)

library(smacof)
RES<-mds(delta=euclideas, ndim=2, type="ratio")
RES

#STRESS
print(RES$stress)
#STRESS POR PUNTO
print(RES$spp)
#coordenadas
print(RES$conf)
#mapa perceptual
plot(RES, type = "p", label.conf = list(label = TRUE, col = "darkgray"), pch = 25, col = "red")

#https://www.kaggle.com/abrambeyer/openintro-possum

