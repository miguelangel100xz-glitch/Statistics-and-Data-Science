
#redes bayesianas
library(DAAG)
library(lattice)
library(ggplot2)
library(visNetwork)
library(dplyr)
library(bnlearn)

bayesHC<-read.csv("C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\MINERIA\\ProyectoFinal\\Datasets\\HepatitisCdata.csv")
bayesHC<-na.omit(bayesHC)
bayesHC<-bayesHC%>%mutate(Sex=as.factor(Sex),
                          Category=as.factor(Category),
                          Age=as.numeric(Age))
bayesHC<-dplyr::select(bayesHC,-X)

ais.sub <- bayesHC[, c("Category", "Age", "Sex", "ALB", "ALP","ALT", "AST", "BIL", "CHE", "CHOL", "CREA","GGT","PROT")]
structure <- hc(ais.sub, score = "bic-cg")
bn.mod <- bn.fit(structure, data = ais.sub)
plot.network <- function(structure, ht = "400px"){
  nodes.uniq <- unique(c(structure$arcs[,1], structure$arcs[,2]))
  nodes <- data.frame(id = nodes.uniq,
                      label = nodes.uniq,
                      color = "darkturquoise",
                      shadow = TRUE)
  edges <- data.frame(from = structure$arcs[,1],
                      to = structure$arcs[,2],
                      arrows = "to",
                      smooth = TRUE,
                      shadow = TRUE,
                      color = "black")
  return(visNetwork(nodes, edges, height = ht, width = "100%"))
}
plot.network(structure, ht = "600px")

bn.mod
#Estimaciones

#https://www.r-bloggers.com/2018/09/bayesian-network-example-with-the-bnlearn-package/


