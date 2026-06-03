
data("iris")
str(iris)

summary(iris)

set.seed(111)
ind <- sample(2, nrow(iris),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- iris[ind==1,]
testing <- iris[ind==2,]

library(psych)

pairs.panels(training[,-5],
             gap = 0,
             bg = c("red", "yellow", "blue")[training$Species],
             pch=21)

pc <- prcomp(training[,-5],
             center = TRUE,
             scale. = TRUE)
attributes(pc)


print(pc)

library(factoextra)
fviz_eig(pc)
fviz_pca_ind(pc,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pc,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pc, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

summary(pc)

pairs.panels(pc$x,
             gap=0,
             bg = c("red", "yellow", "blue")[training$Species],
             pch=21)

#library(devtools)
#install_github("vqv/ggbiplot")
#library(ggbiplot)
#g <- ggbiplot(pc,
 #             obs.scale = 1,
  #            var.scale = 1,
   #           groups = training$Species,
    #          ellipse = TRUE,
     #         circle = TRUE,
      #        ellipse.prob = 0.68)
#g <- g + scale_color_discrete(name = '')
#g <- g + theme(legend.direction = 'horizontal',
 #              legend.position = 'top')
#print(g)



trg <- predict(pc, training)

trg <- data.frame(trg, training[5])
tst <- predict(pc, testing)
tst <- data.frame(tst, testing[5])

library(nnet)
trg$Species <- relevel(trg$Species, ref = "setosa")
mymodel <- multinom(Species~PC1+PC2, data = trg)
summary(mymodel)

p <- predict(mymodel, trg)
tab <- table(p, trg$Species)
tab
1 - sum(diag(tab))/sum(tab)

p1 <- predict(mymodel, tst)
tab1 <- table(p1, tst$Species)
tab1
1 - sum(diag(tab1))/sum(tab1)




http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
  https://www.r-bloggers.com/2021/05/principal-component-analysis-pca-in-r/
  
























