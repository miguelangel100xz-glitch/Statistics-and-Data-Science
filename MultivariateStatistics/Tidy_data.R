
library(dplyr)
library(tidyverse)

#Abandonese toda esperanza si desea revisar el codigo ya que 
#los caracteresespeciales se modificaron al guardar automaticamente por la 
#computadora :c
#PRESTE ATENCION CIDADO A EL MOMENTO DE LA TASA POR CADA 100,000 HABITANTES 
#LOS ULTIMOS CUATRO NOMBRES DE ROBO---.....ETC

victimas <- read.csv(header=TRUE,"C:\\Users\\W10\\Desktop\\--\\R FOR DATA SCIENCE\\VICTIMAS\\victimas_completa_2019_2020.csv")

victimas <- victimas %>% filter(A�.o_hecho>2019)
victimas1 <- victimas %>% mutate(
  Delito_Bajo_Impacto = ifelse(Categoria== "DELITO DE BAJO IMPACTO","TRUE","FALSE"),
  HECHO_NO_DELICTIVO = ifelse(Categoria== "HECHO NO DELICTIVO","TRUE","FALSE"),
  LESIONES_DOLOSAS_ARMA_FUEGO = ifelse(Categoria== "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO","TRUE","FALSE"),
  ROBO_CASA_HABITACION_VIOLENCIA = ifelse(Categoria== "ROBO A CASA HABITACIÓN CON VIOLENCIA","TRUE","FALSE"),
  ROBO_CAJERO_VIOLENCIA = ifelse(Categoria== "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA ","TRUE","FALSE"),
  ROBO_NEGOCIO_VIOLENCIA = ifelse(Categoria== "ROBO A NEGOCIO CON VIOLENCIA","TRUE","FALSE"),
  ROBO_MICROBUS = ifelse(Categoria== "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA","TRUE","FALSE"),
  ROBO_PASAJERO_TAXI = ifelse(Categoria== "ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA","TRUE","FALSE"),
  ROBO_METRO      = ifelse(Categoria== "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA","TRUE","FALSE"),
  ROBO_REPARTIDOR = ifelse(Categoria== "ROBO A REPARTIDOR CON Y SIN VIOLENCIA","TRUE","FALSE"),
  ROBO_VIA_PUBLICA = ifelse(Categoria== "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA","TRUE","FALSE"),
  ROBO_VEHICULO = ifelse(Categoria== "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA","TRUE","FALSE"),
  SECUESTRO = ifelse(Categoria== "SECUESTRO","TRUE","FALSE"),
  VIOLACION = ifelse(Categoria== "VIOLACIÓN","TRUE","FALSE")
)

acp<-victimas %>% select(Categoria,AlcaldiaHechos)
acp<- na.omit(acp)
challenge1<-acp%>% group_by(AlcaldiaHechos,Categoria) %>% count()
challenge2<-challenge1 %>% pivot_wider(names_from=Categoria,values_from=n)
challenge3<- na.omit(challenge2)


challenge4<-challenge2 %>% filter(AlcaldiaHechos=="ALVARO OBREGON"|AlcaldiaHechos=="AZCAPOTZALCO"|
                                    AlcaldiaHechos=="BENITO JUAREZ"|AlcaldiaHechos=="COYOACAN"|
                                    AlcaldiaHechos=="CUAJIMALPA DE MORELOS"|AlcaldiaHechos=="CUAUHTEMOC"|
                                    AlcaldiaHechos=="GUSTAVO A MADERO"|AlcaldiaHechos=="IZTACALCO"|
                                    AlcaldiaHechos=="IZTAPALAPA"|AlcaldiaHechos=="LA MAGDALENA CONTRERAS"|
                                    AlcaldiaHechos=="MIGUEL HIDALGO"|AlcaldiaHechos=="MILPA ALTA"|AlcaldiaHechos=="TLAHUAC"
                                  |AlcaldiaHechos=="TLALPAN"|
                                    AlcaldiaHechos=="VENUSTIANO CARRANZA"|AlcaldiaHechos=="XOCHIMILCO")


challenge4[is.na(challenge4)]<- 0

tasahabitantes<- cbind(challenge4, habitantes=c(759137,432205,434153,614447,217686,545884,
                                                1173351,404695,1835486,247622,414470,152685,
                                                392313,699928,443704,442178 ))

tasas<- tasahabitantes %>% mutate(Bajo_Impacto= (`DELITO DE BAJO IMPACTO`/habitantes)*100000,
                                  No_Delictivo= (`HECHO NO DELICTIVO`/habitantes)*100000,
                                  Robo_Casa=(`ROBO A CASA HABITACIÃ“N CON VIOLENCIA`/habitantes)*100000,
                                  Robo_Vehiculo= (`ROBO DE VEHÃCULO CON Y SIN VIOLENCIA`/habitantes)*100000,
                                  Violacion= (`VIOLACIÃ“N`/habitantes)*100000,
                                  Homicidio=(`HOMICIDIO DOLOSO`/habitantes)*100000,
                                  Lesion_Arma_deFuego=(`LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO`/habitantes)*100000,
                                  Robo_Saliendo_Cajero=(`ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA`/habitantes)*100000,
                                  Robo_Negocio=(`ROBO A NEGOCIO CON VIOLENCIA`/habitantes)*100000,
                                  Robo_Pasajero_Microbus=(`ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA`/habitantes)*100000,
                                  Robo_Pasajero_Taxi=(`ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA`/habitantes)*100000,
                                  Robo_Pasajero_Metro=(`ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA`/habitantes)*100000,
                                  Robo_Repartidor=(`ROBO A REPARTIDOR CON Y SIN VIOLENCIA`/habitantes)*100000,
                                  Robo_Transeunte_ViaPublica=(`ROBO A TRANSEUNTE EN VÃA PÃšBLICA CON Y SIN VIOLENCIA`/habitantes)*100000,
                                  Robo_Transportista=(`ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA`/habitantes)*100000,
                                  Secuestro=(`SECUESTRO`/habitantes)* 100000 )



por_cienmil<- tasas%>% select(AlcaldiaHechos,Bajo_Impacto:Secuestro)

colineales<-por_cienmil%>% mutate(Robo_ViaPublica=Robo_Vehiculo+
                                    Robo_Saliendo_Cajero+Robo_Pasajero_Microbus+
                                    Robo_Pasajero_Taxi+
                                    Robo_Pasajero_Metro+
                                    Robo_Repartidor+Robo_Transeunte_ViaPublica+
                                    Robo_Transportista,
                                  Robo_Domicilio=Robo_Casa+Robo_Negocio)


nuevo<- select(colineales,AlcaldiaHechos,
               Bajo_Impacto,Violacion,Homicidio,
               Lesion_Arma_deFuego,Secuestro,
               Robo_ViaPublica,Robo_Domicilio)

nuevo <- nuevo%>% mutate(Robo=Robo_ViaPublica+Robo_Domicilio)

nuevo<- select(nuevo,AlcaldiaHechos,
               Bajo_Impacto,Violacion,Homicidio,
               Lesion_Arma_deFuego,Secuestro,
               Robo)

nuevo <- nuevo %>% mutate(Homicidio_Arma_deFuego = Lesion_Arma_deFuego+Homicidio)

nuevo<- select(nuevo,AlcaldiaHechos,
               Bajo_Impacto,Violacion,Homicidio_Arma_deFuego,Secuestro,
               Robo)

#write.csv(nuevo,file="delitos_acp.csv")
###############################################################################
#https://datos.cdmx.gob.mx/dataset/victimas-en-carpetas-de-investigacion-fgj


#write.csv(nuevo,file="delitos_acp.csv")
