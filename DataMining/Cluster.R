#########################33
###CLUSTER

mamals_sleep<-read.csv("C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\MINERIA\\ProyectoFinal\\Datasets\\mammals.csv")
mamals_sleep<-na.omit(mamals_sleep)
row.names(mamals_sleep)<-mamals_sleep$species
library(dplyr)
mamals_sleep<-dplyr::select(mamals_sleep,-species)
mamals_sleep<-scale(mamals_sleep)
library(factoextra)
fviz_nbclust(x = mamals_sleep, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(mamals_sleep, method = "euclidean"), nstart = 50)

calcular_totwithins<-function(n_clusters,
                              datos,iter.max=1000,
                              nstart=50){
  cluster_kmeans<-kmeans(centers=n_clusters,
                         x=datos,iter.max = iter.max,nstart = nstart)
                         return(cluster_kmeans$tot.withinss)
}


library(purrr)

total_withins<-map_dbl(.x=1:10,
                       .f=calcular_totwithins,datos=mamals_sleep)
total_withins

data.frame(n_clusters = 1:10, suma_cuadrados_internos = total_withins) %>%
  ggplot(aes(x = n_clusters, y = suma_cuadrados_internos)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Evolución de la suma total de cuadrados intra-cluster") +
  theme_bw()

set.seed(123)
km_clusters<-kmeans(x=mamals_sleep,centers = 5,nstart = 50)

fviz_cluster(object = km_clusters,data = mamals_sleep,
             show.clust.cent = TRUE, ellipse.type = "euclid",
             star.plot=TRUE,repel = TRUE)+
  labs(title="Resultadso de clustering kmeans")+
  theme_bw()+
  theme(legend.position = "none")

###k medoids
#algoritmo pam 
#empleando la fistancia de manhatana 
library(cluster)
fviz_nbclust(x=mamals_sleep,FUNcluster = pam,method = "wss",
             k.max = 15,
             diss = dist(mamals_sleep,method = "manhattan"))

calcular_suma_dif_interna<-function(n_clusters,datos,
                                    distancia="manhattan"){
  cluster_pam<-cluster::pam(x=datos,k=n_clusters,metric = distancia)
  return(cluster_pam$objective["swap"])
}

suma_dif_interna<-map_dbl(.x=1:10,
                          .f=calcular_suma_dif_interna,
                          datos=mamals_sleep)
data.frame(n_clusters=1:10,suma_dif_interna=suma_dif_interna)%>%
  ggplot(aes(x=n_clusters,y=suma_dif_interna))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks=1:10)+
  labs(title = "evolucion de la suma total de diferencias intraclusrer")+
  theme_bw()

set.seed(123)
pam_clusters<-pam(x=mamals_sleep,k=3,metric="manhattan")
pam_clusters  

fviz_cluster(object = pam_clusters,
             data=mamals_sleep,
             ellipse.type = "t",
             repel = TRUE)+
  theme_bw()+
  labs(title = "")+
  theme(legend.position = "none")

medoids<-prcomp(mamals_sleep)$x
medoids<-medoids[row.names(pam_clusters$medoids),c("PC1","PC2")]
medoids<-as.data.frame(medoids)

colnames(medoids)<-c("x","y")

fviz_cluster(object = pam_clusters,data=mamals_sleep,ellipse.type = "t",
             repel = TRUE)+theme_bw()+
  geom_point(data = medoids,color="firebrick",size=2)+
  labs(title = "resultados clustering PAM")+
  theme(legend.position = "none")

###################################33
#######CLARA

clara_clusters<-clara(x=mamals_sleep,
                      k=5,
                      metric="manhattan",
                      stand=TRUE,
                      samples = 50,
                      pamLike = TRUE)
clara_clusters

fviz_cluster(object=clara_clusters,
             ellipse.type = "t",
             geom = "point",
             pointsize = 2.5)+
  theme_bw()+
  labs(title = "Resultados de clustering CLARA")+
  theme(legend.position = "none")

#cluster jerarquico 

mat_dist<-dist(x=mamals_sleep,method = "euclidean")
hc_euclidea_complete<-hclust(d=mat_dist,
                             method="complete")
hc_euclidea_average<-hclust(d=mat_dist,
                            method = "average")
cor(x=mat_dist,cophenetic(hc_euclidea_complete))
cor(x=mat_dist,cophenetic(hc_euclidea_average))

hc_euclidea_completo<-hclust(d=dist(x=mamals_sleep,
                                    method = "euclidean"),
                             method = "complete")

fviz_dend(x=hc_euclidea_completo,k=5,cex=0.6)+
  geom_hline(yintercept = 5.5,linetype="dashed")+
  labs(title="Herarchical clustering",
       subtitle="Distancia euclidea lincage complete k=5")

fviz_dend(x=hc_euclidea_completo,k=7,cex=0.6)+
  geom_hline(yintercept = 5.5,linetype="dashed")+
  labs(title="Herarchical clustering",
       subtitle="Distancia euclidea lincage complete k=5")

#hierarchical k-means clustering 
hc_euclidea_completo <- hclust(d = dist(x = mamals_sleep, method = "euclidean"),
                               method = "complete")
fviz_dend(x = hc_euclidea_completo, cex = 0.5, main = "Linkage completo",
          sub = "Distancia euclídea") +
  theme(plot.title =  element_text(hjust = 0.5, size = 15))

hkmeans_cluster <- hkmeans(x = mamals_sleep, hc.metric = "euclidean",
                           hc.method = "complete", k = 4)
hkmeans_cluster

fviz_cluster(object = hkmeans_cluster, pallete = "jco", repel = TRUE) +
  theme_bw() + labs(title = "Hierarchical k-means Clustering")

#fuzzzy clustering 

library(cluster)
fuzzy_cluster <- fanny(x = mamals_sleep, diss = FALSE, k = 3, metric = "euclidean",
                       stand = FALSE)

library(factoextra)
fviz_cluster(object = fuzzy_cluster, repel = TRUE, ellipse.type = "norm",
             pallete = "jco") + theme_bw() + labs(title = "Fuzzy Cluster plot")

#moidel based clustering 
library(mclust)
model_clustering<-Mclust(data=mamals_sleep,
                         G=1:10)

summary(model_clustering)

fviz_mclust(object = model_clustering, what = "BIC", pallete = "jco") +
  scale_x_discrete(limits = c(1:10))

fviz_mclust(model_clustering, what = "classification", geom = "point",
            pallete = "jco")



