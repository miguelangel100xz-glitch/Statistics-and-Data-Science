
nuevo<- read.csv(header=TRUE,"C:\\Users\\W10\\Desktop\\ESTADISTICA MULTIVARIADA\\ACTIVIDADES\\PROYECTO ACP AF\\ACP_PROY_FINAL\\delitos_acp.csv")


#delitos<- delitos[,-1]
#rownames(nuevo) <- nuevo$AlcaldiaHechos
rownames(nuevo) <- nuevo$AlcaldiaHechos
nuevo<- nuevo[,c(-1,-2)]

nuevo1<- nuevo


matriz_varianzas<-cov(nuevo)

matriz_cor<-cor(nuevo)

determinante<-det(matriz_cor)

acp<- prcomp(nuevo,scale=TRUE)
summary(acp)
eigen_values<- eigen(matriz_cor)
valores_propios<-eigen_values$values


combinaciones_lineal<- acp$rotation


library(factoextra)
#rownames(delitos) <- delitos$AlcaldiaHechos

#acp<- prcomp(delitos[,-1],scale=TRUE)


fviz_pca_biplot(acp)
################
pc1 <- apply(acp$rotation[,1]*nuevo, 1, sum)
pc2 <- apply(acp$rotation[,2]*nuevo, 1, sum)
pc3 <- apply(acp$rotation[,3]*nuevo, 1, sum)

#calculo decomponentes 
nuevo$pc1 <- pc1
nuevo$pc2 <- pc2
nuevo$pc3 <- pc3
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
Rf<-data.frame(nuevo$pc1,nuevo$pc2)
Rf 

