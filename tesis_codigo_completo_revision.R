#SCRIPT TESIS 

#Bases de datos 
#Los datos fueron proporcionados por la 
#Dra. Katrin Sieron investigadora del Centro de
#Ciencias de la Tierra de la Universidad 
#Veracruzana y presentados en:  
#Katrin et al. (2022).


#Se cargan las bases de datos 
#El campo volcanico chichinautzin posee una cantidad de 356 volcanes individuales
#El campo volcanico de los Tuxtlas poseen 368 volcanes individuales 
#Los dos campos poseen las variables de latitud y longitud en coordenadas UTM

library(dplyr)
library(spatstat)
library(ggplot2)


chichinautzin<-read.csv("C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\----\\Escritorio\\Escritorio\\CENTRO DE CIENCIAS CAMPOS VOLCANICOS\\DOS CAMPOS\\volcanes_SCVF_PARA_KDE_MNG_UTM.csv")
tuxtlas<-read.csv("C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\----\\Escritorio\\Escritorio\\LTVF.csv")
tuxtlas<-dplyr::select(tuxtlas,x,y)

#Graficos capitulo uno 
#Donde se busca contestar a la pregunta de si las intensidades de los campos volcanicos son iguales 

cap1_ch<-chichinautzin%>%
  mutate(clase="CV Chichinauztin")

cap1_tux<-tuxtlas%>%
  mutate(clase="CV Los Tuxtlas")


cap1_union<-rbind(cap1_tux,cap1_ch)

ggplot(cap1_union,aes(x/1000,y/1000))+
  geom_point()+
  facet_wrap(~clase,scales = "free")+
  labs(x="Latitud en Kilométros",
       y="Longitud en Kilométros")+
  theme_bw()

#Graficos capitulo 2 
#Herramientas de modelación.
set.seed(2023)


#Proceso Poisson Homogeneo

cap_2X<- runifpoint(200)
cap_2X<-as.data.frame(cap_2X)

cap_2X<-cap_2X%>%mutate(clase="Aleatorio")

ggplot(data=cap_2X, mapping=aes(x=x, y=y)) +
  geom_point()+ theme_bw()



#Proceso puntual Regular 

cap2_r<- rSSI(0.05, 200)

cap2_r<-as.data.frame(cap2_r)

cap2_r<-cap2_r%>%mutate(clase="Regular")

ggplot(data=cap2_r, mapping=aes(x=x, y=y)) +
  geom_point()+ theme_bw()

#Proceso Puntual agrupado


# Proceso de Poisson no homeg´eneo (lambda depende de x)
lambda <- 200 # Tasa de ocurrencia de los sucesos
npoints <- rpois(n = 1, lambda = lambda) # Generamos el n´umero de ocurrencias
# Generamos los puntos como variables normales independientes
x <- rnorm(npoints,mean = .5,sd=.5)
y <- rnorm(npoints,mean = .5,sd=.5)
cap2_df <- data.frame(x = x, y = y) # Dataframe con los resultados
cap_2df<-cap2_df%>%
  mutate(x=(x-min(x))/(max(x)-min(x)))%>%
  mutate(y=(y-min(y))/(max(y)-min(y)))

cap_2df<-cap_2df%>%mutate(clase="Agrupado")

# Representaci´on gr´afica
ggplot(data=cap2_df, mapping=aes(x=x, y=y)) +
  geom_point()+
  theme_minimal()



#Union de graficos capitulo 2

cap2_tpatrones<-rbind(cap_2df,
                      cap_2X,
                      cap2_r)


ggplot(cap2_tpatrones,aes(x,y))+
  geom_point()+
  facet_wrap(~clase)+
  theme_bw()


#Realizacion de proceso puntuales poisson no homogeneos 
#Bajo aleatoriedad espacial completa
set.seed(2023)
cap_21<- runifpoint(200)
cap_21<-as.data.frame(cap_21)

cap_211<-cap_21%>%mutate(clase="1")

cap_22<-cap_21%>%mutate(clase="2")

cap_23<-cap_21%>%mutate(clase="3")

cap2_real<-rbind(cap_211,cap_22,
                 cap_23)

ggplot(data=cap2_real, mapping=aes(x=x, y=y)) +
  geom_point()+ theme_bw()+
  facet_wrap(~clase)



#Graficos y pruebas Capitulo 3 
#Aplicacion de patrones puntuales espaciales 

cap3_chintzn<-chichinautzin
cap3_tux<-tuxtlas

#Conversion a kilometros campo 
#volcanico chichinautzin
#Transformacion min max
#campo volcanico chichinautzin

  cap3_chintzn<-cap3_chintzn%>%
  mutate(x=x/1000)%>%
  mutate(y=y/1000)
  
  cap3_chintzn<-cap3_chintzn%>%
  mutate(x=(x-min(x))/(max(x)-min(x)))%>%
  mutate(y=(y-min(y))/(max(y)-min(y)))


  #Conversion a kilometros 
  #y trasnformacion min max
  #Campo volcanicos los tuxtlas

  
  cap3_tux<-cap3_tux%>%
    mutate(x=x/1000)%>%
    mutate(y=y/1000)
  
  cap3_tux<-cap3_tux%>%
    mutate(x=(x-min(x))/(max(x)-min(x)))%>%
    mutate(y=(y-min(y))/(max(y)-min(y)))

  
  #grafico transformacion min max
  
  cap3_chintzn_clase<-cap3_chintzn%>%
    mutate(clase="CV Chichinauztin")

  cap3_tux_clase<-cap3_tux%>%
    mutate(clase="CV Los Tuxtlas")
  
  cap3_union<-rbind(cap3_tux_clase,cap3_chintzn_clase)
  
  ggplot(cap3_union,aes(x,y))+
    geom_point()+
    facet_wrap(~clase,scales = "free")+
    labs(x="x",
         y="y")+
    theme_bw()
  
  #rotacion hacia los ejes de maxima variabilidad 
#---------------------------------------------------------
  #con componentes principales chichinautzin
  
  library(factoextra)
  
  cova<-cov(cap3_chintzn)
  det(cova)
  
  cap3_pc_chtzn<-prcomp(cap3_chintzn)
    
  
  fviz_pca_var(cap3_pc_chtzn,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE     # Avoid text overlapping
  )+guides(color=FALSE)+
    labs(title="")+
    theme_bw()
  
  
  cap_3_chtzn_res.ind <- get_pca_ind(cap3_pc_chtzn)
  
  cap3_rotac_chntz<-as.data.frame(cap_3_chtzn_res.ind$coord)
  
  cap3_rotac_chntz<-cap3_rotac_chntz%>%
    mutate(x=Dim.1,
           y=Dim.2)%>%dplyr::select(x,y)

#---------------------------------------------------
  #rotacion hacia los ejes de maxima variabilidad 
  #con componentes principales Los Tuxtlas
  
  library(factoextra)
  
  cova<-cov(cap3_tux)
  det(cova)
  
  cap3_pc_tux<-prcomp(cap3_tux)
  
  
  fviz_pca_var(cap3_pc_tux,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE     # Avoid text overlapping
  )+guides(color=FALSE)+
    labs(title="")+
    theme_bw()
  
  
  cap_3_tux_res.ind <- get_pca_ind(cap3_pc_tux)
  
  cap3_rotac_tux<-as.data.frame(cap_3_tux_res.ind$coord)
  
  cap3_rotac_tux<-cap3_rotac_tux%>%
    mutate(x=Dim.1,
           y=Dim.2)%>%dplyr::select(x,y)

#--------------------------------------------
  #grafico de rotacion de coordenadas hacia los ejes de 
  #maxima variabilidad 
  #cap3_rotacion_de_ejes min max
  
  cap3_rotac_chntz_graf<-cap3_rotac_chntz%>%
    mutate(x=(x-min(x))/(max(x)-min(x)),
           y=(y-min(y))/(max(y)-min(y)),
           clase="CV Chichinautzin")

    
  cap3_rotac_tux_graf<-cap3_rotac_tux%>%
    mutate(x=(x-min(x))/(max(x)-min(x)),
           y=(y-min(y))/(max(y)-min(y)),
           clase="CV Los Tuxtlas") 

  
  cap3_graf_rot_eje<-rbind(cap3_rotac_chntz_graf,
                           cap3_rotac_tux_graf)  

  
  ggplot(cap3_graf_rot_eje,aes(x,y))+
    geom_point()+
    facet_wrap(~clase)+
    theme_bw()
  

  #----------------------------------------------
  #Ajuste de los modelos
  #----------------------------------------------
  
  #Ajuste no parametrico
  
  
  #Ajuste de los modelos poisson no homogeneos
  #En este apartado se incluye el ajuste noparametrico 
  #A traves de la metodologia planteada en 
  #Baddeley et al. (2012)
  
  cap3_modelo_chtzn<-cap3_rotac_chntz_graf
  
  
  cap3_modelo_chtzn_patronp <- ppp(cap3_modelo_chtzn$x,
                   cap3_modelo_chtzn$y,
                   c(0,1),
                   c(0,1))
  
  
  
  
  cap3_fit_chtzn_npm<-ppm(cap3_modelo_chtzn_patronp,~polynom(x,y,2))
                          
  cap3_chtzn_nopm<-rho2hat(cap3_fit_chtzn_npm
                           ,"x","y",
                           method = "ratio")
  
  plot(cap3_chtzn_nopm)
  points(cap3_modelo_chtzn)
  
  #Este modelo ajustado representa un ajuste no 
  #parametrico 
  #no sirve para la modelacion ya que el ajuste del 
  #modelo parametrico da estas predicciones 
  
  plot(cap3_fit_chtzn_npm)
  
  
  #Las predicciones del modelo 
  #lineal son completamente fuera de rango 
  #lo mismo pasa para el campo volcanico los tuxtlas
  
  cap3_modelo_tux<-cap3_rotac_tux_graf
  
  cap3_modelo_tux_patronp <- ppp(cap3_modelo_tux$x,
                                   cap3_modelo_tux$y,
                                   c(0,1),
                                   c(0,1))
  
  cap3_fit_tux_npm<-ppm(cap3_modelo_tux_patronp,polynom(x,y,2))
  
  cap3_tux_nopm<-rho2hat(cap3_fit_tux_npm
                           ,"x","y",
                           method = "ratio")
  
  plot(cap3_tux_nopm)
  points(cap3_modelo_tux)
  
  #Las predicciones del modelo 
  #lineal son completamente fuera de rango 
  #lo mismo pasa para el campo volcanico los tuxtlas
  cap3_fit_tux_npm
  
  plot(cap3_fit_tux_npm)
  
  
  #Sin embargo las estimaciones no parametricas 
  #con el kernel me dieron una idea del modelo 
  #parametrico que tenia que buscar para que
  #se presentara un buen ajuste a los datos 
  
  
  #-------------------------------------------------
  #Modelo parametrico chichinautzin
  #-------------------------------------------------
  
  #Ajuste parametrico en este caso 
  #el modelo que se aproximaba mejor 
  #a las estimaciones no parametricas 
  #fue un modelo polinomial de tercer grado 
  #aqui estan las fuentes que utilice para ajustar el
  #modelo y ya no andarme equivocando 
  #https://spatstat.org/SSAI2017/slides/slides.pdf
  
  cap3_modelo_chtzn_param<-cap3_rotac_chntz_graf
  
  #Ampliamos el tamaño de la ventana ya 
  #que al estar entre cero y uno toma a todos los datos 
  #como ventana de interes 
  #toma a tod el conjunto como una unidad 
  #agrandamos el tamaño de la ventana y por ende 
  #tambien los datos en tamaño por 100 
  
  cap3_modelo_chtzn_param<-cap3_modelo_chtzn_param%>%
    mutate(x=x*100,
           y=y*100)
  
  cap3_modelo_chtzn_polinom <- ppp(cap3_modelo_chtzn_param$x,
                                 cap3_modelo_chtzn_param$y,
                                 c(0,100),
                                 c(0,100))
  
  cap3_fit_chtzn_polinom<-
    ppm(cap3_modelo_chtzn_polinom~polynom(x,y,2))
  
  
  plot(cap3_fit_chtzn_polinom)
  
  cap3_fit_chtzn_polinom
  
  diagnose.ppm(cap3_fit_chtzn_polinom)
  
  #Comparaciones de modelos de 
  #menor grado 
  
  cap3_fit_chtzn_polinom1<-
    ppm(cap3_modelo_chtzn_polinom,~x+y)
  
  cap3_fit_chtzn_polinom2<-
    ppm(cap3_modelo_chtzn_polinom,~polynom(x,y,2))
  
  anova(cap3_fit_chtzn_polinom1,
        cap3_fit_chtzn_polinom,
        test = "Chi")
  
  anova(cap3_fit_chtzn_polinom2,
        cap3_fit_chtzn_polinom,
        test = "Chi")
  
  #Se extraen los graficos de estimaciones de intensidad 
  #y se extrae los graficos de los errores de estimacion 
  
  plot(cap3_fit_chtzn_polinom)
  
  plot(predict(cap3_fit_chtzn_polinom))
  contour(predict(cap3_fit_chtzn_polinom),add = TRUE)
  
  diagnose.ppm(cap3_fit_chtzn_polinom)
  #se obtiene realizaciones del modelo 
  #estocastico ajustado 
  
  plot(simulate(cap3_fit_chtzn_polinom))
  
  #------------------------------------
  #Modelo parametrico los tuxtlas
  #------------------------------------
  
  cap3_modelo_tux_param<-cap3_rotac_tux_graf
  
  cap3_modelo_tux_param<-cap3_modelo_tux_param%>%
    mutate(x=x*100,
           y=y*100)
  
  cap3_modelo_tux_polinom <- ppp(cap3_modelo_tux_param$x,
                                   cap3_modelo_tux_param$y,
                                   c(0,100),
                                   c(0,100))
  
  cap3_fit_tux_polinom<-
    ppm(cap3_modelo_tux_polinom~polynom(x,y,2))
  
  plot(cap3_fit_tux_polinom)
  
  cap3_fit_tux_polinom
  
  diagnose.ppm(cap3_fit_tux_polinom)
  
  plot(predict(cap3_fit_tux_polinom))
  contour(predict(cap3_fit_tux_polinom),add = TRUE)
  
  plot(simulate(cap3_fit_tux_polinom))
  
  #Comparaciones de modelos de 
  #menor grado 
  
  cap3_fit_tux_polinom1<-
    ppm(cap3_modelo_tux_polinom,~x+y)
  
  cap3_fit_tux_polinom2<-
    ppm(cap3_modelo_tux_polinom,~polynom(x,y,2))
  
  anova(cap3_fit_tux_polinom1,
        cap3_fit_tux_polinom,
        test = "Chi")
  
  anova(cap3_fit_tux_polinom2,
        cap3_fit_tux_polinom,
        test = "Chi")
  
  
  #---------------------------------------------------
  
  
  
  
  
  #---------------------------------------------------
  
  
  cap3_modelo_chtzn_param<-cap3_rotac_tux
  
  cap3_modelo_chtzn_param<-cap3_modelo_chtzn_param%>%
    mutate(x=x*100,
           y=y*100)
  
  cap3_modelo_chtzn_polinom <- ppp(cap3_modelo_chtzn_param$x,
                                   cap3_modelo_chtzn_param$y,
                                   c(0,100),
                                   c(0,100))
  
  plot(density(cap3_modelo_chtzn_polinom,
               sigma=bw.CvL(cap3_modelo_chtzn_polinom),
               diggle = TRUE))
  
  chtzn_density <- density(cap3_modelo_chtzn_polinom,
                         sigma=bw.CvL(cap3_modelo_chtzn_polinom),
                         at="points",diggle = TRUE
                         ,leaveoneout = FALSE,se=TRUE)
  
  mean(chtzn_density$estimate)
  chtzn_estim<-as.data.frame(chtzn_density$estimate)
  
  ggplot(chtzn_estim,aes(`chtzn_density$estimate`))+
    geom_histogram(fill="midnightblue",
                   alpha=.8)+
    labs(y="Frecuencia",
      x="Estimación de intensidad no parametrica para cada evento")+
    theme_bw()

  #---------------------------------------------
  
  cap3_model0<-cap3_rotac_tux_graf
  
  cap3_modelo_tux_nppparam<-cap3_model0%>%
    mutate(x=x*100,
           y=y*100)
  
  
  cap3_modelo_tux_nppp <- ppp(cap3_modelo_tux_nppparam$x,
                              cap3_modelo_tux_nppparam$y,
                                 c(0,100),
                                 c(0,100))
  
  tux_density <- density(cap3_modelo_tux_nppp,
                         sigma=bw.CvL(cap3_modelo_tux_nppp),
                         at="points",diggle = TRUE
                         ,leaveoneout = FALSE,se=TRUE)
  
  mean(tux_density$estimate)
  
  tux_estim<-as.data.frame(tux_density$estimate)
  
  ggplot(tux_estim,aes(tux_estim$`tux_density$estimate`))+
    geom_histogram(fill="midnightblue",
                   alpha=.8)+
    labs(y="Frecuencia",
         x="Estimación de intensidad no parametrica para cada evento")+
    theme_bw()
  
  #----------------------------------------------------------------
#----------------------------------------------------------------  
  #----------------------------------
  #Prueba de permutaciones 
  #----------------------------------
  
  
  H0_log<-function(a,b,c,d,e,f,g,h,i,j){
    ((a+b+c+d+e+f+g+h+i+j)*(724))-(exp(a+b+c+d+e+f+g+h+i+j))
    
  }
  
  H1_log<-function(a,b,c,d,e,f,g,h,i,j,n){
    ((a+b+c+d+e+f+g+h+i+j)*(n))-(exp(a+b+c+d+e+f+g+h+i+j))
    
  }
  
  #Bajo H0
  
  cap3_H0_join<-dplyr::select(cap3_graf_rot_eje,
                                   x,
                                   y)
  
  cap3_H0_join<-cap3_H0_join%>%
    mutate(x=x*100,
           y=y*100)
  
    
  #convertir en patron puntual espacial 
  
  cap3_H0_ChtznTux <- ppp(cap3_H0_join$x,
                          cap3_H0_join$y,
                    c(0,100),
                    c(0,100))
  
  fit_H0_chtztux<-ppm(cap3_H0_ChtznTux~polynom(x,y,3))

  cap3_H0_coef_obs<-fit_H0_chtztux$coef
    
  cap3_H0_coef_obs<-as.list(cap3_H0_coef_obs)
  
  
  
  cap3_H0<-H0_log(cap3_H0_coef_obs$x,
             cap3_H0_coef_obs$y,
             cap3_H0_coef_obs$`I(x^2)`,
             cap3_H0_coef_obs$`I(x * y)`,
             cap3_H0_coef_obs$`I(y^2)`,
             cap3_H0_coef_obs$`I(x^3)`,
             cap3_H0_coef_obs$`I(x^2 * y)`,
             cap3_H0_coef_obs$`I(x * y^2)`,
             cap3_H0_coef_obs$`I(y^3)`,
             cap3_H0_coef_obs$`(Intercept)`)
  
  
  #Verosimilitud de Chichinautzin
  
  cap3_rotac_chntz_perm<-dplyr::select(cap3_rotac_chntz_graf,
                x,
                y)
  
  cap3_rotac_chntz_perm<-cap3_rotac_chntz_perm%>%
    mutate(x=x*100,
           y=y*100)
  
  cap3_rotac_chntz_perm_ppm <- ppp(cap3_rotac_chntz_perm$x,
                                   cap3_rotac_chntz_perm$y,
                          c(0,100),
                          c(0,100))
  
  
  cap3_L_chitzn<-ppm(cap3_rotac_chntz_perm_ppm~polynom(x,y,3))
  
  cap3_coef_obs_chtzn_l<-cap3_L_chitzn$coef
  
  cap3_coef_obs_chtzn_l<-as.list(cap3_coef_obs_chtzn_l)
  
  cap3_L_chitzn_t<-H1_log(cap3_coef_obs_chtzn_l$x,
                     cap3_coef_obs_chtzn_l$y,
                     cap3_coef_obs_chtzn_l$`I(x^2)`,
                     cap3_coef_obs_chtzn_l$`I(x * y)`,
                     cap3_coef_obs_chtzn_l$`I(y^2)`,
                     cap3_coef_obs_chtzn_l$`I(x^3)`,
                     cap3_coef_obs_chtzn_l$`I(x^2 * y)`,
                     cap3_coef_obs_chtzn_l$`I(x * y^2)`,
                     cap3_coef_obs_chtzn_l$`I(y^3)`,
                     cap3_coef_obs_chtzn_l$`(Intercept)`,
                     n=356)
  

    
  #Verosimilitud de Tuxtlas
  cap3_rotac_tux_perm<-
    dplyr::select(cap3_rotac_tux_graf,
                  x,
                  y)
  
  
  cap3_rotac_tux_perm<-cap3_rotac_tux_perm%>%
    mutate(x=x*100,
           y=y*100)
  
  cap3_rotac_tux_perm_ppm <- ppp(cap3_rotac_tux_perm$x,
                                 cap3_rotac_tux_perm$y,
                                   c(0,100),
                                   c(0,100))
  
  
  cap3_L_tux<-ppm(cap3_rotac_tux_perm_ppm~polynom(x,y,3))
  
  cap3_coef_obs_tux_l<-cap3_L_tux$coef
  
  cap3_coef_obs_tux_l<-as.list(cap3_coef_obs_tux_l)
  
  cap3_L_tux_t<-H1_log(cap3_coef_obs_tux_l$x,
                          cap3_coef_obs_tux_l$y,
                          cap3_coef_obs_tux_l$`I(x^2)`,
                          cap3_coef_obs_tux_l$`I(x * y)`,
                          cap3_coef_obs_tux_l$`I(y^2)`,
                          cap3_coef_obs_tux_l$`I(x^3)`,
                          cap3_coef_obs_tux_l$`I(x^2 * y)`,
                          cap3_coef_obs_tux_l$`I(x * y^2)`,
                          cap3_coef_obs_tux_l$`I(y^3)`,
                          cap3_coef_obs_tux_l$`(Intercept)`,
                          n=368)

  
  H1<-cap3_L_tux_t+cap3_L_chitzn_t
  
  
  T_test<-2*(cap3_H0-H1)
  #=5647.102
  
  #----------------------
  #Ciclo para la prueba de permutaciones
  #
  
  #-----------------------------------------------------
  
  #paso 1 se juntan las bases en un mismo 
  #dataframe 
  perm<-dplyr::select(cap3_graf_rot_eje,
                                    x,
                                    y)
  
  
  #se crea un vector de parametros en donde se guardaran las 
  #realizaciones del modelo
  
  T_test_perm<-as.vector(NULL)
  
  
  #se guarda una semilla para 
  #para que sea repetir los resultados
  
  set.seed(1234)
  
  for (i in 1:500) {
    muestra<-sample(1:nrow(perm),size=356,replace = FALSE)
    perm1<-perm[muestra,]
    perm2<-perm[-muestra,]
    
    perm1<-ppp(perm1$x,
               perm1$y,
               c(0,100),
               c(0,100))
    
    fit1<-ppm(perm1~polynom(x,y,2))
    
    coef_obs_perm1<-fit1$coef
    
    coef_obs_perm1<-as.list(coef_obs_perm1)
    
    L_perm1_t<-H1_log(coef_obs_perm1$x,
                      coef_obs_perm1$y,
                      coef_obs_perm1$`I(x^2)`,
                      coef_obs_perm1$`I(x * y)`,
                      coef_obs_perm1$`I(y^2)`,
                      coef_obs_perm1$`I(x^3)`,
                      coef_obs_perm1$`I(x^2 * y)`,
                      coef_obs_perm1$`I(x * y^2)`,
                      coef_obs_perm1$`I(y^3)`,
                      coef_obs_perm1$`(Intercept)`,
                      n=356)
    
    perm2<-ppp(perm2$x,
               perm2$y,
               c(0,100),
               c(0,100))
    
    fit2<-ppm(perm2~polynom(x,y,3))
    
    coef_obs_perm2<-fit2$coef
    
    coef_obs_perm2<-as.list(coef_obs_perm2)
    
    L_perm2_t<-H1_log(coef_obs_perm2$x,
                      coef_obs_perm2$y,
                      coef_obs_perm2$`I(x^2)`,
                      coef_obs_perm2$`I(x * y)`,
                      coef_obs_perm2$`I(y^2)`,
                      coef_obs_perm2$`I(x^3)`,
                      coef_obs_perm2$`I(x^2 * y)`,
                      coef_obs_perm2$`I(x * y^2)`,
                      coef_obs_perm2$`I(y^3)`,
                      coef_obs_perm2$`(Intercept)`
                      ,n=368)
    
    H1<-L_perm1_t+L_perm2_t
    
    
    T_test_perm[i]<-2*(T_test-H1)
    
  }
  
  