#tortugas 
library(ggplot2)
library(tidyverse)
library(patchwork)
library(factoextra)

tortugas <- read_excel("C:\\Users\\W10\\Desktop\\ESTADISTICA MULTIVARIADA\\ACTIVIDADES\\PCA\\PCA_3__TORTUGAS.xlsx")

lon_cap<-ggplot(tortugas,aes(Longitud_del_caparazon))+
  geom_histogram(fill="pink",colour="black",alpha=0.2)+
  labs(title="Longitud")

Anch_cap<-ggplot(tortugas,aes(Ancho_del_caparazon))+
  geom_histogram(fill="pink",colour="black",alpha=0.2)+
  labs(title="Ancho")

Alt_cap<-ggplot(tortugas,aes(Altura_del_caparazon))+
  geom_histogram(fill="pink",colour="black",alpha=0.2)+
  labs(title="Alto")




tortugas1<- select(tortugas,"Longitud_del_caparazon":"Altura_del_caparazon")

resumen <- tortugas1 %>% 
  pivot_longer("Longitud_del_caparazon":"Altura_del_caparazon")%>%
  #filter(name %in% c("COL","GLUC","TRIG"))%>% 
  ggplot(aes(name,value,fill=name))+
  geom_boxplot(alpha=1/4,outlier.shape = 16)+
  guides(fill=FALSE)+
  # facet_wrap(~name, scales = "free")
  #scale_x_discrete(limits=c("ICC","HEM","IMC","GLUC","COL","TRIG"))+
  labs(
    x="",y="",title="Especie Chrysemys  Picta  Marginata"
  )+
  coord_flip()

tortugas_vi<- lon_cap+Anch_cap+Alt_cap+resumen

cov_tort<- cov(tortugas1)

#correlacion

corr_tort<-  cor(tortugas1)
#MR


#VizualizaciÃ³n#
#install.packages("corrplot")
library (corrplot)
#visual_biol<-cor(tortugas1, use="complete.obs")
#visual_biol1<-corrplot.mixed(visual_biol)


#kable(S , booktabs = TRUE, align =c("l","c","r","r","c"),col.names =c("IMC","ICC","HEM", "GLUC","COL","TRIG"),escape=FALSE)

o_tort<-det(corr_tort)
#print("Determinante")
#print(o)





