library(psych)
library (corrplot)
library(ggplot2)
library(tidyverse)
library(factoextra)
library(readxl)

biologicos<- read_excel("C:\\Users\\W10\\Desktop\\ESTADISTICA MULTIVARIADA\\ACTIVIDADES\\PCA\\PCA_2__PARAMETROS_BIOL.xlsx")


biologicos1<- select(biologicos,-1)

#PREPROCESAMIENTO DE LA DATA
# matriz de varianzas y covarianzas 
 #d <- d %>% filter(TRIG < 500)



 cov_biologicos<- biologicos%>% select(-1)%>% cov()
 
 #correlacion
 
 corr_biologicos<-  cor(biologicos1)
 #MR
 
 
 #Vizualizaci贸n#
 #install.packages("corrplot")
 library (corrplot)
 visual_biol<-cor(biologicos1, use="complete.obs")
 visual_biol1<-corrplot.mixed(visual_biol)
 
 
 #kable(S , booktabs = TRUE, align =c("l","c","r","r","c"),col.names =c("IMC","ICC","HEM", "GLUC","COL","TRIG"),escape=FALSE)
 
 o_biologicos<-det(corr_biologicos)
 #print("Determinante")
 #print(o)
 
 
 #######################################3
 #########################################
 #ACP 
 
 # C谩lculo de componentes con Matriz de correlaci贸n
 acp_biologicos <- prcomp(biologicos1, scale. = TRUE)
 
 #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 # imprimir el an谩lisis de componentes principales
 
 #kable(acp , booktabs = TRUE, align =c("l","c","r","r","c"),row.names =c("IMC","ICC","HEM", "GLUC","COL","TRIG"),escape=FALSE)
 
 
 
 #print(acp)
 
 
 k_biologicos<-acp_biologicos$rotation
 
 #Ver varianza acumulada
 
 p_BIOLOGICOS<- summary(acp_biologicos)
 q_BIOLOGICOS <- eigen(corr_biologicos)
 
 tb01_biologicos =data.frame(stringsAsFactors=FALSE,Posiciones =c("Eigenvalores","Varianza","Varianza Acumulada"),PC1 = c(1.8002,0.300,0.300),
                  PC2 = c(1.3310,0.2218,0.5219),
                  PC3 = c(1.0589,0.1765,0.6984), 
                  PC4=c(0.7833,0.1306,0.8289) ,
                  PC5=c(0.5681,0.0947,0.9236),
                  PC6=c(0.4582,0.07637,1.00000)
 )
 
 
 
 ###############################################################
 
 ##Correlaci贸n de variables y componentes usando R
 corvar_biologicos <- acp_biologicos$rotation %*% diag(acp_biologicos$sdev)
 
 
 
 #############################
 ###########################3
 ############################333
 ##Coordenadas Nuevas##
 f_biologicos<-acp_biologicos$x
 
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 ##Gr谩fico de sedimentaci贸n (Codo)##
 #install.packages("factoextra")

 sedimentacion_biologicos<-fviz_eig(acp_biologicos)
 
 biplot_biologicos<-fviz_pca_biplot(acp_biologicos, label="var",
                 select.ind = list(contrib = 100))
 
 #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 # En la salida de los componentes principales cada columna representa una
 # combinaci贸n lineal de las variables originales Ej.
 # Zi = .5231*AGR + .
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ##Gr醘ico de individuos con un perfil similar (agrupaciones)##
 fviz_pca_ind(acp_biologicos,
              col.ind = "cos2", # Color by the quality of representation
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE     # Avoid text overlapping
 )
 ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         # Default plot
         fviz_pca_var(acp_biologicos)
 ##Gr醘ica de variables. Las variables correlacionadas positivas 
 apuntan al mismo lado de la gr醘ica. Las variables negativas
 correlacionadas apuntan a lados opuestos del gr醘ico.##
 
 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         ##Biplot
         biplot(acp_biologicos)
 #factoextra
 fviz_pca_biplot(acp_biologicos)
 #biplot variables
 fviz_pca_biplot(acp_biologicos, invisible ="ind")
 #biplot individuos
 fviz_pca_biplot(acp_biologicos, invisible ="var")
 ##biplot  con  factoextra##
 # Seleccionando a los indiviudos
 fviz_pca_biplot(acp_biologicos, label="var",
                 select.ind = list(contrib = 100))
 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         
 
 
 ############################333
 ###############################33
 #################################3
 #variables latentes 
 
 # Se calculan las componentes 
 pc1_biologicos <- apply(acp_biologicos$rotation[,1]*biologicos1, 1, sum)
 pc2_biologicos <- apply(acp_biologicos$rotation[,2]*biologicos1, 1, sum)
 pc3_biologicos <- apply(acp_biologicos$rotation[,3]*biologicos1, 1, sum)
 #________________________________________________________________________
 # las integro a mi conjunto de datos
 biologicos1$pc1_biologicos <- pc1_biologicos
 biologicos1$pc2_biologicos <- pc2_biologicos
 biologicos1$pc3_biologicos <- pc3_biologicos
 #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 Rf_biologicos<-data.frame(biologicos1$pc1_biologicos,biologicos1$pc2_biologicos)
 
 
 k<-Rf_biologicos %>% mutate(Contaminacion = case_when(biologicos1.pc2_biologicos>30~"Contaminado",
                                                    biologicos1.pc2_biologicos<30~"No Contaminado"))
 
 k<- k %>% mutate(n=1:100)
 
 ggplot(k,aes(biologicos1.pc1_biologicos,biologicos1.pc2_biologicos,shape=Contaminacion,colour=Contaminacion))+
         geom_point()+
         labs(title="Contaminaci髇 de\n Transectos",x="Componente 1",y="Componente 2")

 
 
 
 
 mejor_de_su_clase <- k %>%
         group_by() %>%
         filter(row_number((biologicos1.pc2_biologicos)) >60 )
 
 ggplot(k, aes(biologicos1.pc1_biologicos,biologicos1.pc2_biologicos,colour=Contaminacion)) +
         geom_point(aes(colour = Contaminacion)) +
         geom_text(aes(label = n), data = mejor_de_su_clase)

 
 ggplot(k, aes(biologicos1.pc1_biologicos,biologicos1.pc2_biologicos)) +
         geom_point(aes(colour=Contaminacion)) +
         geom_point(size = 3, shape = 1, data = mejor_de_su_clase) +
         ggrepel::geom_label_repel(aes(label = n), data = mejor_de_su_clase)+
         labs(title="Transectos Contaminados",x="Componente1",y="Componente 2")
 
 
 ggplot(k, aes(biologicos1.pc1_biologicos,biologicos1.pc2_biologicos)) +
         geom_point(aes(colour = Contaminacion)) +
         geom_label(aes(label = n), data = mejor_de_su_clase, nudge_y = 2, alpha = 0.5)
 
 
 mejor_de_su_clases <- k %>%
         group_by() %>%
         filter(row_number((biologicos1.pc2_biologicos)) <40 )
 
 ggplot(k, aes(biologicos1.pc1_biologicos,biologicos1.pc2_biologicos)) +
         geom_point(aes(colour=Contaminacion),alpha=1/2) +
         geom_point(size = 3, shape = 1, data = mejor_de_su_clase) +
         ggrepel::geom_label_repel(aes(label = n), data = mejor_de_su_clases)+
         labs(title="Transectos No Contaminados",x="Componente1",y="Componente 2")
 
 
  
 
 