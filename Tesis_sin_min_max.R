
library(dplyr)
library(spatstat)
library(ggplot2)


tuxtlas<-read.csv("C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\----\\Escritorio\\Escritorio\\LTVF.csv")
tuxtlas1.0<-dplyr::select(tuxtlas,x,y) #se seleccionan las variables de x e y

cap3_tux1.1<-tuxtlas1.0

cap3_tux1.2<-cap3_tux1.1%>%#se transforma a escala kilometros
  mutate(x=x/1000)%>%
  mutate(y=y/1000)

cap3_tux1.3<-cap3_tux1.2

cap3_tux1.3<-cap3_tux1.2


#Orientacion hacia los ejes de maxima variabilidad-----------------------

library(factoextra)

cap3_cova_tux<-cor(cap3_tux1.3)#calculo de la matriz de covarianzas
eigen <- eigen(cap3_cova_tux)#calculo de eigenvalores y eigenvectores
det(cap3_cova_tux)#determinante de la matriz de covarianzas

cap3_pc_tux<-prcomp(cap3_tux1.3,scale. = TRUE)#calculo de acp y biplot 



a<-fviz_pca_var(cap3_pc_tux,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)+
  guides(color=FALSE)+
  labs(title="")+
  theme_bw()


cap_3_tux_res.ind <- get_pca_ind(cap3_pc_tux)#extraccion de coordenadas

cap3_rotac_tux1.1<-as.data.frame(cap_3_tux_res.ind$coord)#convertir coordenadas a data frame

cap3_rotac_tux1.2<-cap3_rotac_tux1.1%>%#guardado en una base de datos nueva
  mutate(x=Dim.1,
         y=Dim.2)%>%dplyr::select(x,y)


cap3_rotac_tux_graf<-cap3_rotac_tux1.2

cap3_model0_tux<-cap3_rotac_tux_graf#guardado en otra base de datos

cap3_modelo_tux_nppparam<-cap3_model0_tux%>%
  mutate(x=x,
         y=y)


cap3_modelo_tux_nppp <- ppp(cap3_modelo_tux_nppparam$x,#ajuste del patron puntual
                            cap3_modelo_tux_nppparam$y,
                            c(-2.8,3.2),
                            c(-1.8,1.6)
                            )



cap3_fit_tux_polinom<- #ajuste de un modelo de segundo grado 
  ppm(cap3_modelo_tux_nppp~polynom(x,y,2))

m1<-step(cap3_fit_tux_polinom)

m1

#########################################################3
#-----------------------------------------------------------



chichinautzin<-read.csv("C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\----\\Escritorio\\Escritorio\\CENTRO DE CIENCIAS CAMPOS VOLCANICOS\\DOS CAMPOS\\volcanes_SCVF_PARA_KDE_MNG_UTM.csv")

#Aplicacion de patrones puntuales espaciales 

cap3_chintzn1.1<-chichinautzin

#Conversion a kilometros campo 
#volcanico chichinautzin

cap3_chintzn1.2<-cap3_chintzn1.1%>%
  mutate(x=x/1000)%>%
  mutate(y=y/1000)

#Transformacion min max
#campo volcanico chichinautzin

cap3_chintzn1.3<-cap3_chintzn1.2


#rotacion hacia los ejes de maxima variabilidad 
#-----------------------------------------------------------------------
#con componentes principales chichinautzin

library(factoextra)

cap3_chntzn_cova<-cor(cap3_chintzn1.3)#matriz de covarianzas 

eigens<-eigen(cap3_chntzn_cova)#calculo de eigenvalores y eigenvectores
det(cap3_chntzn_cova)#determinante de la matriz de covarianzas

cap3_pc_chtzn<-prcomp(cap3_chintzn1.3,scale. = TRUE)#calculo de componentes


b<-fviz_pca_var(cap3_pc_chtzn,#grafico de biplot
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)+
  guides(color=FALSE)+
  labs(title="")+
  theme_bw()

#se extraen las coordenadas de cada evento para posterior analisis 

cap_3_chtzn_res.ind <- get_pca_ind(cap3_pc_chtzn)#extraccion de cordenadas

cap3_rotac_chntz<-as.data.frame(cap_3_chtzn_res.ind$coord)#guardado base nueva

cap3_rotac_chntz1.1<-cap3_rotac_chntz%>%#limpieza y acomodado nueva base
  mutate(x=Dim.1,
         y=Dim.2)%>%dplyr::select(x,y)


#------------------------------------------------------------------------
#grafico de rotacion de coordenadas hacia los ejes de 
#maxima variabilidad 
#cap3_rotacion_de_ejes min max

cap3_rotac_chntz_graf<-cap3_rotac_chntz1.1

cap3_modelo_chtzn_nparam<-cap3_rotac_chntz_graf#guardado base nueva

cap3_modelo_chtzn_nparam1.1<-cap3_modelo_chtzn_nparam%>%#ajuste de valores
  mutate(x=x,
         y=y)

#se ajusta el patron puntual 
#se amplia el tamaño de ancho de ventana 
cap3_modelo_chtzn_nppp <- ppp(cap3_modelo_chtzn_nparam1.1$x,#ajuste de patron puntual
                              cap3_modelo_chtzn_nparam1.1$y,
                              c(-4.3,3.7),
                              c(-2.4,2.7))


cap3_modelo_chtzn_nppp1.0<-ppm(cap3_modelo_chtzn_nppp~polynom(x,y,2))#ajuste de modelo de patron puntual 2do grado
cap3_modelo_chtzn_nppp1.1<-step(cap3_modelo_chtzn_nppp1.0)#eliminacion hacia atras 

#---------------------------------------------------------
#Prueba de permutaciones 
#----------------------------------------------------------

library(dplyr)
library(spatstat)
library(ggplot2)


tuxtlas<-read.csv("C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\----\\Escritorio\\Escritorio\\LTVF.csv")
tuxtlas1.0<-dplyr::select(tuxtlas,x,y) #se seleccionan las variables de x e y

cap3_tux1.1<-tuxtlas1.0

cap3_tux1.2<-cap3_tux1.1%>%#se transforma a escala kilometros
  mutate(x=x/1000)%>%
  mutate(y=y/1000)

cap3_tux1.3<-cap3_tux1.2


#Orientacion hacia los ejes de maxima variabilidad-----------------------

library(factoextra)

cap3_cova_tux<-cor(cap3_tux1.3)#calculo de la matriz de covarianzas
eigen <- eigen(cap3_cova_tux)#calculo de eigenvalores y eigenvectores
det(cap3_cova_tux)#determinante de la matriz de covarianzas

cap3_pc_tux<-prcomp(cap3_tux1.3,scale. = TRUE)#calculo de acp y biplot 

fviz_pca_var(cap3_pc_tux,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)+
  guides(color=FALSE)+
  labs(title="")+
  theme_bw()


cap_3_tux_res.ind <- get_pca_ind(cap3_pc_tux)#extraccion de coordenadas

cap3_rotac_tux1.1<-as.data.frame(cap_3_tux_res.ind$coord)#convertir coordenadas a data frame

cap3_rotac_tux1.2<-cap3_rotac_tux1.1%>%#guardado en una base de datos nueva
  mutate(x=Dim.1,
         y=Dim.2)%>%dplyr::select(x,y)

cap3_rotac_tux_graf<-cap3_rotac_tux1.2

cap3_model0_tux<-cap3_rotac_tux_graf#guardado en otra base de datos

cap3_modelo_tux_nppparam<-cap3_model0_tux%>%
  mutate(x=x,
         y=y)


cap3_modelo_tux_nppp <- ppp(cap3_modelo_tux_nppparam$x,#ajuste del patron puntual
                            cap3_modelo_tux_nppparam$y,
                            c(-2.8,3.2),
                            c(-1.8,1.6))

#------------------------------------------------------------------------

#Campo Volcanico Chichinautzin

#------------------------------------------------------------------------

chichinautzin<-read.csv("C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\----\\Escritorio\\Escritorio\\CENTRO DE CIENCIAS CAMPOS VOLCANICOS\\DOS CAMPOS\\volcanes_SCVF_PARA_KDE_MNG_UTM.csv")

#Aplicacion de patrones puntuales espaciales 

cap3_chintzn1.1<-chichinautzin

#Conversion a kilometros campo 
#volcanico chichinautzin

cap3_chintzn1.2<-cap3_chintzn1.1%>%
  mutate(x=x/1000)%>%
  mutate(y=y/1000)

#Transformacion min max
#campo volcanico chichinautzin

cap3_chintzn1.3<-cap3_chintzn1.2


#rotacion hacia los ejes de maxima variabilidad 
#-----------------------------------------------------------------------
#con componentes principales chichinautzin

library(factoextra)

cap3_chntzn_cova<-cor(cap3_chintzn1.3)#matriz de covarianzas 

eigens<-eigen(cap3_chntzn_cova)#calculo de eigenvalores y eigenvectores
det(cap3_chntzn_cova)#determinante de la matriz de covarianzas

cap3_pc_chtzn<-prcomp(cap3_chintzn1.3,scale. = TRUE)#calculo de componentes


fviz_pca_var(cap3_pc_chtzn,#grafico de biplot
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)+
  guides(color=FALSE)+
  labs(title="")+
  theme_bw()

#se extraen las coordenadas de cada evento para posterior analisis 

cap_3_chtzn_res.ind <- get_pca_ind(cap3_pc_chtzn)#extraccion de cordenadas

cap3_rotac_chntz<-as.data.frame(cap_3_chtzn_res.ind$coord)#guardado base nueva

cap3_rotac_chntz1.1<-cap3_rotac_chntz%>%#limpieza y acomodado nueva base
  mutate(x=Dim.1,
         y=Dim.2)%>%dplyr::select(x,y)


#------------------------------------------------------------------------
#grafico de rotacion de coordenadas hacia los ejes de 
#maxima variabilidad 
#cap3_rotacion_de_ejes min max

cap3_rotac_chntz_graf<-cap3_rotac_chntz1.1

cap3_modelo_chtzn_nparam<-cap3_rotac_chntz_graf#guardado base nueva

cap3_modelo_chtzn_nparam1.1<-cap3_modelo_chtzn_nparam%>%#ajuste de valores
  mutate(x=x,
         y=y)

#se ajusta el patron puntual 
#se amplia el tamaño de ancho de ventana 
cap3_modelo_chtzn_nppp <- ppp(cap3_modelo_chtzn_nparam1.1$x,#ajuste de patron puntual
                              cap3_modelo_chtzn_nparam1.1$y,
                              c(-4.3,3.7),
                              c(-2.4,2.7))

#------------------------------------------------------------------------
#Prueba de Permutaciones 
#Campo volcanico chichinautzin

cap3_modelo_chtzn_nppp1.0<-ppm(cap3_modelo_chtzn_nppp~polynom(x,y,2))#ajuste de modelo de patron puntual 2do grado
cap3_modelo_chtzn_nppp1.1<-step(cap3_modelo_chtzn_nppp1.0)#eliminacion hacia atras 

#cap3_modelo_chtzn_nppp1.2<-ppm(cap3_modelo_chtzn_nppp,~(x+y+I(x^2)+I(y^2)))#seleccion del modelo ajustado 

d<-as.vector(coef(cap3_modelo_chtzn_nppp1.1))#extraccion de coeficientes en vector

x_chtzn<-c(cap3_modelo_chtzn_nparam1.1$x)#guardado de variables x para calculo de verosiml
y_chtzn<-c(cap3_modelo_chtzn_nparam1.1$y)#guardado de base y 

L_chtzn<-as.vector(NULL)#vector donde se guardaran los resultados 

#Verosimilitud del campo volcanico chichinautzin-------------------------

for(i in 1:356){#ajuste de ciclo d pertence al vector de los coeficientes
  #x_chtzn representa la variable x guardada en lista
  #y_chtzn representa la variable y 356 es por el numero de volcanes
  
  L_chtzn[i]= ((d[1]+
                  d[2]*(x_chtzn[i]^2)+
                  d[3]*(y_chtzn[i]^2))*356)
  
  -(exp(d[1]+
          d[2]*(x_chtzn[i]^2)+
          d[3]*(y_chtzn[i]^2))
  )
  
  
  
}

Verosimilitud_chtzn<-sum(L_chtzn) #calculo de la verosimilitud 

#Campo volcanico los tuxtlas 

cap3_fit_tux_polinom<- #ajuste de un modelo de segundo grado 
  ppm(cap3_modelo_tux_nppp~polynom(x,y,2))

cap3_fit_tux_polinom1.1<-step(cap3_fit_tux_polinom)#eliminacion hacia atras 
d<-as.vector(coef(cap3_fit_tux_polinom1.1))#guardado de los coeficientes del modelo ajustadp

x_tux<-c(cap3_modelo_tux_nppparam$x)#guardado de las variable de x de tuxtlas
y_tux<-c(cap3_modelo_tux_nppparam$y)#guardado de la variable y de tuxtlas 

L_tux<-as.vector(NULL)

#Verosimilitud del campo volcanico de los tuxtlas------------------------ 

for(i in 1:368){#368 es por el numero de volcanes individuales en los tuxtlas
  
  L_tux[i]= (d[1]+# d representa el vector de coeficientes del modelo ajustado
               d[2]*(x_tux[i]^2)+
               d[3]*(y_tux[i]^2))*368
  
  -(exp(d[1]+
          d[2]*(x_tux[i]^2)+
          d[3]*(y_tux[i]^2))
  )
  
  
  
}


Verosimilitud_tux<-sum(L_tux)#calculo de la verosimilitud 


#Verosimilitud de H0--------------------------------------------------
#para el calculo de la verosimilitud se unieron las bases de 
#chichinaautzin y los tuxtlas 

H0<-rbind(cap3_modelo_chtzn_nparam1.1,cap3_modelo_tux_nppparam)

cap3_modelo_H0 <- ppp(H0$x,H0$y,#ajuste del patron puntual para H0
                      c(-4.3,3.7),
                      c(-2.4,2.7))


cap3_modelo_H01.0<-ppm(cap3_modelo_H0~polynom(x,y,2))#ajuste del polinomio de 2do grado
cap3_modelo_H01.1<-step(cap3_modelo_H01.0)#elimincacion hacia atras 

cap3_modelo_H0_nppp1.2<-ppm(cap3_modelo_H0,~(I(x^2)+I(y^2)))#seleccion del modelo 

d<-as.vector(coef(cap3_modelo_H0_nppp1.2))#se guardan los coeficientes del modelo en un vector

x_H0<-c(H0$x)#se guardan las variables de x de H0
y_H0<-c(H0$y)#variable de y de HO

L_H0<-as.vector(NULL)#vector donde se guardaran los valores calculados 

#Verosimilitud del campo volcanico chichinautzin-------------------------

for(i in 1:724){#724 representa el numero total de datos 
  
  L_H0[i]= (d[1]+#d representa el vector de coeficientes del modelo 
              d[2]*(x_H0[i]^2)+
              d[3]*(y_H0[i]^2))*724
  
  -(exp(d[1]+
          d[2]*(x_H0[i]^2)+
          d[3]*(y_H0[i]^2)))
  
  
  
  
}

Verosimilitud_H0<-sum(L_H0)#se calcula la verosimilitud 

H1<-log(Verosimilitud_chtzn)+log(Verosimilitud_tux) #Se calcula la verosimilitud para H1

G<- -2*log(Verosimilitud_H0)-((-2*(H1)))# se calcula el estadistico de prueba

#######prueba de permutaciones#-----------------------------------------

#para la prueba de permutaciones se juntan las dos bases de datos en 
#la base llamada perm 

perm<-rbind(cap3_modelo_chtzn_nparam1.1,cap3_modelo_tux_nppparam)

G_perm<-as.vector(NULL)#se crea un vector donde se guardaran los estadisticos de 
#prueba calculados 

for (i in 1:1009) {#se consideran 1000 iteraciones 
  
  muestra<-sample(1:nrow(perm),size=356,replace = FALSE)#se crea un sistema para extraccion de muestras
  perm1<-perm[muestra,]#tamaño de muestra 356
  perm2<-perm[-muestra,]#tamaño de muestra 368
  
  #----------------------------------------------------
  #verosimilitud de perm
  
  cap3_modelo_perm <- ppp(perm1$x,#se ajusta el patron puntual para perm1 
                          perm1$y,
                          c(-4.3,3.7),
                          c(-2.4,2.7))
  
  cap3_modelo_perm1.2<-ppm(cap3_modelo_perm,~(I(x^2)+I(y^2)))#se ajusta el modelo a perm1
  
  d_perm1<-as.vector(coef(cap3_modelo_perm1.2))#se extraen los coeficientes del modelo ajustado de perm1 
  
  x_perm1<-c(perm1$x)#se extraen las variables de x de perm1
  y_perm1<-c(perm1$y)#se extraen las variables de y de perm1
  
  L_perm1<-as.vector(NULL)#se crea el vector donde se guardara el estadistico calculado
  
  for(j in 1:356){# se construye el ciclo para el calculo de la verosimilitud de perm1
    
    L_perm1[j]= (d_perm1[1]+#d_perm1 representa el vector de coeficientes del modelo
                   d_perm1[2]*(x_perm1[j]^2)+
                   d_perm1[3]*(y_perm1[j]^2))*356
    
    -(exp(d_perm1[1]+
            d_perm1[2]*(x_perm1[j]^2)+
            d_perm1[3]*(y_perm1[j]^2))
    )
    
    
    
  }
  
  Verosimilitud_perm1<-sum(L_perm1)#se calcula la verosimilitud de perm1
  
  
  #verosimilitud de perm2
  
  cap3_modelo_perm2 <- ppp(perm2$x,#se crea un patron puntual espacial para perm2
                           perm2$y,
                           c(-4.3,3.7),
                           c(-2.4,2.7))
  
  cap3_modelo_perm2.22<-ppm(cap3_modelo_perm2,~(I(x^2)+I(y^2)))#se ajusta el modelo a perm2
  
  d_perm2<-as.vector(coef(cap3_modelo_perm2.22))#se guardan los coeficientes del modelo
  
  x_perm2<-c(perm2$x)#se guarda la variable de x de perm2
  y_perm2<-c(perm2$y)#se guarda la variable de y de perm2
  
  L_perm2<-as.vector(NULL)#se crea el vector en donde se guardaran los calculos
  
  for(l in 1:368){#se inicia el ciclo para el calculo de la verosimilitud de perm2
    
    L_perm2[l]= (d_perm2[1]+#d se refiere a el vector de coeficientes del modelo ajustado de perm2
                   d_perm2[2]*(x_perm2[l]^2)+
                   d_perm2[3]*(y_perm2[l]^2))*368
    
    -(exp(d_perm2[1]+
            d_perm2[2]*(x_perm2[l]^2)+
            d_perm2[3]*(y_perm2[l]^2))
    )
    
    
  }
  
  Verosimilitud_perm2<-sum(L_perm2)#se calcula la verosimilitud de perm2
  
  H1_perm<-log(Verosimilitud_perm2)+log(Verosimilitud_perm1)#se calcula la verosimilitud de H1
  
  G_perm[i]<- -2*log(Verosimilitud_H0)-(-2*(H1_perm))#se calcula la razon de verosimilitudes 
  #el calculo de H0  viene de antes de la creacion de la prueba y se guarda cada 
  #repeticion par aposterior construccion de un grafico.
  
}

hist(G_perm)#grafico de la distribucion del estadistico de prueba
((sum(G_perm>=G))+1)/(1009+1)# p valor< 0.05

as.data.frame(G_perm)%>%
  ggplot(aes(x=(G_perm)))+
  geom_histogram(aes(y=..density..),position="identity",
                 #fill="midnightblue",
                 color="white",alpha=.4)+
  geom_density()+#size=1.1,#color="darkgreen",
  #linetype="")+
  labs(x="Distribución de la prueba de permutaciones",
       y="Densidad")+
  scale_y_continuous(labels = scales::percent_format(scale = .1))+
  theme_bw()+
  geom_vline(xintercept = (G), 
             color = "red",
             size=1.5,linetype="dashed",alpha=.8)
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())











