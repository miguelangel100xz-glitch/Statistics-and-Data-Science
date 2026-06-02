
library(readxl)


d1<-read_xlsx("C:\\Users\\W10\\Downloads\\K__means.xlsx")
d1<-d1[,-1]
d2<-scale(d1)

d2f=data.frame(d2)
km_clusters <- kmeans(x = d2f, centers = 2, nstart = 50)
 #Las funciones del paquete factoextra emplean el nombre de las filas del
 #dataframe que contiene los datos como identificador de las observaciones.
 #Esto permite añadir labels a los gráficos.
fviz_cluster(object = km_clusters, data = d2f, show.clust.cent = TRUE,
         ellipse.type = "euclid", star.plot = TRUE, repel = TRUE,
          pointsize=0.5,outlier.color="darkred") +
 labs(title = "Resultados clustering K-means") +
theme_bw() +  theme(legend.position = "none")

set.seed(20)
k.means.fit <-kmeans(melbourne1[,2:9], 3, nstart = 10)
k.means.fit 


grupos=km_clusters$cluster
b<-data.frame(grupos)
b

p<-merge(x = d1, y = b, by = c("row.names")) 
p%>% mutate(grupos=as.factor(grupos))%>%
ggplot(aes(CalifFinal,color=grupos,fill=grupos))+geom_bar()





