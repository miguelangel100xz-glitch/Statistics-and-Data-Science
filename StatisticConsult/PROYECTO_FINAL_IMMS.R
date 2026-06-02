library(dplyr)
library(tidyverse)

nacimientos2008<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\sinac2008DatosAbiertos.csv")
nacimientos2008<-nacimientos2008%>%filter(madre_sobrevivio_al_parto=="NO",afiliacion_serv_salud=="IMSS")
nacimientos2008<-nacimientos2008%>%mutate(NSOBREVIVIOPARTO=TRUE,ENT_CERT=entidad_certifico)
nacimientos2008<-nacimientos2008%>%mutate(id=row_number())
nacimientos2008<-nacimientos2008%>%select(id,ENT_CERT,NSOBREVIVIOPARTO)
nacimientos2008<-nacimientos2008%>%pivot_wider(names_from = ENT_CERT,values_from=NSOBREVIVIOPARTO,values_fill = FALSE)
nacimientos2008<-nacimientos2008%>%mutate(Anio=2008)

#++++++++++++++++++++++++++++++++++++++++++++++++++

nacimientos2009<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\sinac2009DatosAbiertos.csv")
nacimientos2009<-nacimientos2009%>%filter(madre_sobrevivio_al_parto=="NO",afiliacion_serv_salud=="IMSS")
nacimientos2009<-nacimientos2009%>%mutate(NSOBREVIVIOPARTO=TRUE,ENT_CERT=entidad_certifico)
nacimientos2009<-nacimientos2009%>%mutate(id=row_number())
nacimientos2009<-nacimientos2009%>%select(id,ENT_CERT,NSOBREVIVIOPARTO)
nacimientos2009<-nacimientos2009%>%pivot_wider(names_from = ENT_CERT,values_from=NSOBREVIVIOPARTO,values_fill = FALSE)
nacimientos2009<-nacimientos2009%>%mutate(Anio=2009)
nacimientos2009<-nacimientos2009%>%mutate(AGUASCALIENTES=FALSE,
                                          CAMPECHE=FALSE,
                                          TABASCO=FALSE)


#++++++++++++++++++++++++++++++++++++++++++++++++++

nacimientos2010<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\sinac2010DatosAbiertos.csv")
nacimientos2010<-nacimientos2010%>%filter(madre_sobrevivio_al_parto=="NO",afiliacion_serv_salud=="IMSS")
nacimientos2010<-nacimientos2010%>%mutate(NSOBREVIVIOPARTO=TRUE,ENT_CERT=entidad_certifico)
nacimientos2010<-nacimientos2010%>%mutate(id=row_number())
nacimientos2010<-nacimientos2010%>%select(id,ENT_CERT,NSOBREVIVIOPARTO)
nacimientos2010<-nacimientos2010%>%pivot_wider(names_from = ENT_CERT,values_from=NSOBREVIVIOPARTO,values_fill = FALSE)
nacimientos2010<-nacimientos2010%>%mutate(Anio=2010)
nacimientos2010<-nacimientos2010%>%mutate(AGUASCALIENTES=FALSE,
                                          "BAJA CALIFORNIA SUR" = FALSE,
                                          "SAN LUIS POTOSI" =FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++

nacimientos2011<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\sinac2011DatosAbiertos.csv")
nacimientos2011<-nacimientos2011%>%filter(madre_sobrevivio_al_parto=="NO",afiliacion_serv_salud=="IMSS")
nacimientos2011<-nacimientos2011%>%mutate(NSOBREVIVIOPARTO=TRUE,ENT_CERT=entidad_certifico)
nacimientos2011<-nacimientos2011%>%mutate(id=row_number())
nacimientos2011<-nacimientos2011%>%select(id,ENT_CERT,NSOBREVIVIOPARTO)
nacimientos2011<-nacimientos2011%>%pivot_wider(names_from = ENT_CERT,values_from=NSOBREVIVIOPARTO,values_fill = FALSE)
nacimientos2011<-nacimientos2011%>%mutate(Anio=2011)
nacimientos2011<-nacimientos2011%>%mutate("BAJA CALIFORNIA SUR"=FALSE,
                                          COLIMA=FALSE,
                                          TAMAULIPAS=FALSE)

#+++++++++++++++++++++++++++++++++++++++++++++++++++

nacimientos2012<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\sinac2012DatosAbiertos.csv")
nacimientos2012<-nacimientos2012%>%filter(madre_sobrevivio_al_parto=="NO")
nacimientos2012<-nacimientos2012%>%mutate(NSOBREVIVIOPARTO=TRUE,ENT_CERT=entidad_certifico)
nacimientos2012<-nacimientos2012%>%mutate(id=row_number())
nacimientos2012<-nacimientos2012%>%select(id,ENT_CERT,NSOBREVIVIOPARTO)
nacimientos2012<-nacimientos2012%>%pivot_wider(names_from = ENT_CERT,values_from=NSOBREVIVIOPARTO,values_fill = FALSE)
nacimientos2012<-nacimientos2012%>%mutate(Anio=2012)
nacimientos2012<-nacimientos2012%>%mutate("BAJA CALIFORNIA SUR"=FALSE,
                                          AGUASCALIENTES=FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++
nacimientos2013<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\sinac2013DatosAbiertos.csv")
nacimientos2013<-nacimientos2013%>%filter(madre_sobrevivio_al_parto=="NO")
nacimientos2013<-nacimientos2013%>%mutate(NSOBREVIVIOPARTO=TRUE,ENT_CERT=entidad_certifico)
nacimientos2013<-nacimientos2013%>%mutate(id=row_number())
nacimientos2013<-nacimientos2013%>%select(id,ENT_CERT,NSOBREVIVIOPARTO)
nacimientos2013<-nacimientos2013%>%pivot_wider(names_from = ENT_CERT,values_from=NSOBREVIVIOPARTO,values_fill = FALSE)
nacimientos2013<-nacimientos2013%>%mutate(Anio=2013)
nacimientos2013<-nacimientos2013%>%mutate(AGUASCALIENTES=FALSE,
                                          "BAJA CALIFORNIA SUR"=FALSE,
                                          COLIMA=FALSE,
                                          "SAN LUIS POTOSI"=FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++
nacimientos2014<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\sinac2014DatosAbiertos.csv")
nacimientos2014<-nacimientos2014%>%filter(madre_sobrevivio_al_parto=="NO")
nacimientos2014<-nacimientos2014%>%mutate(NSOBREVIVIOPARTO=TRUE,ENT_CERT=entidad_certifico)
nacimientos2014<-nacimientos2014%>%mutate(id=row_number())
nacimientos2014<-nacimientos2014%>%select(id,ENT_CERT,NSOBREVIVIOPARTO)
nacimientos2014<-nacimientos2014%>%pivot_wider(names_from = ENT_CERT,values_from=NSOBREVIVIOPARTO,values_fill = FALSE)
nacimientos2014<-nacimientos2014%>%mutate(Anio=2014)
nacimientos2014<-nacimientos2014%>%mutate(YUCATAN=FALSE,
                                          ZACATECAS=FALSE,
                                          TLAXCALA=FALSE,
                                          TABASCO=FALSE,
                                          "QUINTANA ROO"=FALSE,
                                          "NUEVO LEON"=FALSE,
                                          COLIMA=FALSE,
                                          "BAJA CALIFORNIA SUR"=FALSE,
                                          CAMPECHE=FALSE,
                                          "COAHUILA DE ZARAGOZA"=FALSE)

#+++++++++++++++++++++++++++++++++++++++++++++
nacimientos2015<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\sinac2015DatosAbiertos.csv")
nacimientos2015<-nacimientos2015%>%filter(madre_sobrevivio_al_parto=="NO")
nacimientos2015<-nacimientos2015%>%mutate(NSOBREVIVIOPARTO=TRUE,ENT_CERT=entidad_certifico)
nacimientos2015<-nacimientos2015%>%mutate(id=row_number())
nacimientos2015<-nacimientos2015%>%select(id,ENT_CERT,NSOBREVIVIOPARTO)
nacimientos2015<-nacimientos2015%>%pivot_wider(names_from = ENT_CERT,values_from=NSOBREVIVIOPARTO,values_fill = FALSE)
nacimientos2015<-nacimientos2015%>%mutate(Anio=2015)
nacimientos2015<-nacimientos2015%>%mutate(YUCATAN=FALSE,
                                          TLAXCALA=FALSE,
                                          TABASCO=FALSE,
                                          "SAN LUIS POTOSI"=FALSE,
                                          "QUINTANA ROO"=FALSE,
                                          "QUERETARO  DE ARTEAGA"=FALSE,
                                          DURANGO=FALSE,
                                          COLIMA=FALSE,
                                          "COAHUILA DE ZARAGOZA"=FALSE,
                                          "BAJA CALIFORNIA"=FALSE,
                                          "BAJA CALIFORNIA SUR"=FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nacimientos2016<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\sinac2016DatosAbiertos.csv")
nacimientos2016<-nacimientos2016%>%filter(madre_sobrevivio_al_parto=="NO")
nacimientos2016<-nacimientos2016%>%mutate(NSOBREVIVIOPARTO=TRUE,ENT_CERT=entidad_certifico)
nacimientos2016<-nacimientos2016%>%mutate(id=row_number())
nacimientos2016<-nacimientos2016%>%select(id,ENT_CERT,NSOBREVIVIOPARTO)
nacimientos2016<-nacimientos2016%>%pivot_wider(names_from = ENT_CERT,values_from=NSOBREVIVIOPARTO,values_fill = FALSE)
nacimientos2016<-nacimientos2016%>%mutate(Anio=2016)
nacimientos2016<-nacimientos2016%>%mutate(TLAXCALA=FALSE,
                                          TABASCO=FALSE,
                                          "SAN LUIS POTOSI"=FALSE,
                                          CAMPECHE=FALSE,
                                          "BAJA CALIFORNIA SUR"=FALSE,
                                          AGUASCALIENTES=FALSE,
                                          "QUERETARO  DE ARTEAGA"=FALSE,
                                          "DISTRITO FEDERAL"=FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nacimientos2017<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\sinac2017DatosAbiertos.csv")
nacimientos2017<-nacimientos2017%>%filter(SOB_PARTO==2)
nacimientos2017<-nacimientos2017%>%mutate(NSOBREVIVIOPARTO=TRUE)
nacimientos2017<-nacimientos2017%>%mutate(id=row_number())

nacimientos2017<-nacimientos2017%>%mutate(ENT_CERT=case_when(ENT_CERT=="1"~"AGUASCALIENTES",
                                                             ENT_CERT=="2"~"BAJA CALIFORNIA",
                                                             ENT_CERT=="3"~"BAJA CALIFORNIA SUR",
                                                             ENT_CERT=="4"~"CAMPECHE",
                                                             ENT_CERT=="5"~"COAHUILA DE ZARAGOZA",
                                                             ENT_CERT=="6"~"COLIMA",
                                                             ENT_CERT=="7"~"CHIAPAS",
                                                             ENT_CERT=="8"~"CHIHUAHUA",
                                                             ENT_CERT=="9"~"DISTRITO FEDERAL",
                                                             ENT_CERT=="10"~"DURANGO",
                                                             ENT_CERT=="11"~"GUANAJUATO",
                                                             ENT_CERT=="12"~"GUERRERO",
                                                             ENT_CERT=="13"~"HIDALGO",
                                                             ENT_CERT=="14"~"JALISCO",
                                                             ENT_CERT=="15"~"MEXICO",
                                                             ENT_CERT=="16"~"MICHOACAN DE OCAMPO",
                                                             ENT_CERT=="17"~"MORELOS",
                                                             ENT_CERT=="18"~"NAYARIT",
                                                             ENT_CERT=="19"~"NUEVO LEON",
                                                             ENT_CERT=="20"~"OAXACA",
                                                             ENT_CERT=="21"~"PUEBLA",
                                                             ENT_CERT=="22"~"QUERETARO  DE ARTEAGA",
                                                             ENT_CERT=="23"~"QUINTANA ROO",
                                                             ENT_CERT=="24"~"SAN LUIS POTOSI",
                                                             ENT_CERT=="25"~"SINALOA",
                                                             ENT_CERT=="26"~"SONORA",
                                                             ENT_CERT=="27"~"TABASCO",
                                                             ENT_CERT=="28"~"TAMAULIPAS",
                                                             ENT_CERT=="29"~"TLAXCALA",
                                                             ENT_CERT=="30"~"VERACRUZ DE IGNACIO DE LA LLAVE",
                                                             ENT_CERT=="31"~"YUCATAN",
                                                             ENT_CERT=="32"~"ZACATECAS"))


nacimientos2017<-nacimientos2017%>%select(id,ENT_CERT,NSOBREVIVIOPARTO)
nacimientos2017<-nacimientos2017%>%pivot_wider(names_from = ENT_CERT,values_from=NSOBREVIVIOPARTO,values_fill = FALSE)
nacimientos2017<-nacimientos2017%>%mutate(Anio=2017)
nacimientos2017<-nacimientos2017%>%mutate("BAJA CALIFORNIA SUR"=FALSE,
                                          COLIMA=FALSE,
                                          "QUERETARO  DE ARTEAGA"=FALSE,
                                          "SAN LUIS POTOSI"=FALSE,
                                          TLAXCALA=FALSE,
                                          YUCATAN=FALSE,
                                          ZACATECAS=FALSE)




#muestra 
ENT_CERT==1~"AGUASCALIENTES",
ENT_CERT==2~"BAJA CALIFORNIA",
ENT_CERT==3~"BAJA CALIFORNIA SUR",
ENT_CERT==4~"CAMPECHE",
ENT_CERT==5~"COAHUILA DE ZARAGOZA",
ENT_CERT==6~"COLIMA",
ENT_CERT==7~"CHIAPAS",
ENT_CERT==8~"CHIHUAHUA",
ENT_CERT==9~"DISTRITO FEDERAL",
ENT_CERT==10~"DURANGO",
ENT_CERT==11~"GUANAJUATO",
ENT_CERT==12~"GUERRERO",
ENT_CERT==13~"HIDALGO",
ENT_CERT==14~"JALISCO",
ENT_CERT==15~"MEXICO",
ENT_CERT==16~"MICHOACAN DE OCAMPO",
ENT_CERT==17~"MORELOS",
ENT_CERT==18~"NAYARIT",
ENT_CERT==19~"NUEVO LEON",
ENT_CERT==20~"OAXACA",
ENT_CERT==21~"PUEBLA",
ENT_CERT==22~"QUERETARO  DE ARTEAGA",
ENT_CERT==23~"QUINTANA ROO",
ENT_CERT==24~"SAN LUIS POTOSI",
ENT_CERT==25~"SINALOA",
ENT_CERT==26~"SONORA",
ENT_CERT==27~"TABASCO",
ENT_CERT==28~"TAMAULIPAS",
ENT_CERT==29~"TLAXCALA",
ENT_CERT==30~"VERACRUZ DE IGNACIO DE LA LLAVE",
ENT_cERT==31~"YUCATAN",
ENT_CERT==32~"ZACATECAS"










#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
nacimientos2018<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\sinac2018DatosAbiertos.csv")
nacimientos2018<-nacimientos2018%>%filter(SOB_PARTO==2)
nacimientos2018<-nacimientos2018%>%mutate(NSOBREVIVIOPARTO=TRUE)
nacimientos2018<-nacimientos2018%>%mutate(id=row_number())

nacimientos2018<-nacimientos2018%>%mutate(ENT_CERT=case_when(ENT_CERT=="1"~"AGUASCALIENTES",
                                                             ENT_CERT=="2"~"BAJA CALIFORNIA",
                                                             ENT_CERT=="3"~"BAJA CALIFORNIA SUR",
                                                             ENT_CERT=="4"~"CAMPECHE",
                                                             ENT_CERT=="5"~"COAHUILA DE ZARAGOZA",
                                                             ENT_CERT=="6"~"COLIMA",
                                                             ENT_CERT=="7"~"CHIAPAS",
                                                             ENT_CERT=="8"~"CHIHUAHUA",
                                                             ENT_CERT=="9"~"DISTRITO FEDERAL",
                                                             ENT_CERT=="10"~"DURANGO",
                                                             ENT_CERT=="11"~"GUANAJUATO",
                                                             ENT_CERT=="12"~"GUERRERO",
                                                             ENT_CERT=="13"~"HIDALGO",
                                                             ENT_CERT=="14"~"JALISCO",
                                                             ENT_CERT=="15"~"MEXICO",
                                                             ENT_CERT=="16"~"MICHOACAN DE OCAMPO",
                                                             ENT_CERT=="17"~"MORELOS",
                                                             ENT_CERT=="18"~"NAYARIT",
                                                             ENT_CERT=="19"~"NUEVO LEON",
                                                             ENT_CERT=="20"~"OAXACA",
                                                             ENT_CERT=="21"~"PUEBLA",
                                                             ENT_CERT=="22"~"QUERETARO  DE ARTEAGA",
                                                             ENT_CERT=="23"~"QUINTANA ROO",
                                                             ENT_CERT=="24"~"SAN LUIS POTOSI",
                                                             ENT_CERT=="25"~"SINALOA",
                                                             ENT_CERT=="26"~"SONORA",
                                                             ENT_CERT=="27"~"TABASCO",
                                                             ENT_CERT=="28"~"TAMAULIPAS",
                                                             ENT_CERT=="29"~"TLAXCALA",
                                                             ENT_CERT=="30"~"VERACRUZ DE IGNACIO DE LA LLAVE",
                                                             ENT_CERT=="31"~"YUCATAN",
                                                             ENT_CERT=="32"~"ZACATECAS"))



nacimientos2018<-nacimientos2018%>%select(id,ENT_CERT,NSOBREVIVIOPARTO)
nacimientos2018<-nacimientos2018%>%pivot_wider(names_from = ENT_CERT,values_from=NSOBREVIVIOPARTO,values_fill = FALSE)
nacimientos2018<-nacimientos2018%>%mutate(Anio=2018)
nacimientos2018<-nacimientos2018%>%mutate(AGUASCALIENTES=FALSE,
                                          COLIMA=FALSE,
                                          HIDALGO=FALSE,
                                          MORELOS=FALSE,
                                          "SAN LUIS POTOSI"=FALSE,
                                          TLAXCALA=FALSE)


#muestra 
ENT_CERT==1~"AGUASCALIENTES",
ENT_CERT==2~"BAJA CALIFORNIA",
ENT_CERT==3~"BAJA CALIFORNIA SUR",
ENT_CERT==4~"CAMPECHE",
ENT_CERT==5~"COAHUILA DE ZARAGOZA",
ENT_CERT==6~"COLIMA",
ENT_CERT==7~"CHIAPAS",
ENT_CERT==8~"CHIHUAHUA",
ENT_CERT==9~"DISTRITO FEDERAL",
ENT_CERT==10~"DURANGO",
ENT_CERT==11~"GUANAJUATO",
ENT_CERT==12~"GUERRERO",
ENT_CERT==13~"HIDALGO",
ENT_CERT==14~"JALISCO",
ENT_CERT==15~"MEXICO",
ENT_CERT==16~"MICHOACAN DE OCAMPO",
ENT_CERT==17~"MORELOS",
ENT_CERT==18~"NAYARIT",
ENT_CERT==19~"NUEVO LEON",
ENT_CERT==20~"OAXACA",
ENT_CERT==21~"PUEBLA",
ENT_CERT==22~"QUERETARO  DE ARTEAGA",
ENT_CERT==23~"QUINTANA ROO",
ENT_CERT==24~"SAN LUIS POTOSI",
ENT_CERT==25~"SINALOA",
ENT_CERT==26~"SONORA",
ENT_CERT==27~"TABASCO",
ENT_CERT==28~"TAMAULIPAS",
ENT_CERT==29~"TLAXCALA",
ENT_CERT==30~"VERACRUZ DE IGNACIO DE LA LLAVE",
ENT_cERT==31~"YUCATAN",
ENT_CERT==32~"ZACATECAS"
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nacimientos2019<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\sinac2019DatosAbiertos.csv")
nacimientos2019<-nacimientos2019%>%filter(SOB_PARTO==2)
nacimientos2019<-nacimientos2019%>%mutate(NSOBREVIVIOPARTO=TRUE)
nacimientos2019<-nacimientos2019%>%mutate(id=row_number())

nacimientos2019<-nacimientos2019%>%mutate(ENT_CERT=case_when(ENT_CERT=="1"~"AGUASCALIENTES",
                                                             ENT_CERT=="2"~"BAJA CALIFORNIA",
                                                             ENT_CERT=="3"~"BAJA CALIFORNIA SUR",
                                                             ENT_CERT=="4"~"CAMPECHE",
                                                             ENT_CERT=="5"~"COAHUILA DE ZARAGOZA",
                                                             ENT_CERT=="6"~"COLIMA",
                                                             ENT_CERT=="7"~"CHIAPAS",
                                                             ENT_CERT=="8"~"CHIHUAHUA",
                                                             ENT_CERT=="9"~"DISTRITO FEDERAL",
                                                             ENT_CERT=="10"~"DURANGO",
                                                             ENT_CERT=="11"~"GUANAJUATO",
                                                             ENT_CERT=="12"~"GUERRERO",
                                                             ENT_CERT=="13"~"HIDALGO",
                                                             ENT_CERT=="14"~"JALISCO",
                                                             ENT_CERT=="15"~"MEXICO",
                                                             ENT_CERT=="16"~"MICHOACAN DE OCAMPO",
                                                             ENT_CERT=="17"~"MORELOS",
                                                             ENT_CERT=="18"~"NAYARIT",
                                                             ENT_CERT=="19"~"NUEVO LEON",
                                                             ENT_CERT=="20"~"OAXACA",
                                                             ENT_CERT=="21"~"PUEBLA",
                                                             ENT_CERT=="22"~"QUERETARO  DE ARTEAGA",
                                                             ENT_CERT=="23"~"QUINTANA ROO",
                                                             ENT_CERT=="24"~"SAN LUIS POTOSI",
                                                             ENT_CERT=="25"~"SINALOA",
                                                             ENT_CERT=="26"~"SONORA",
                                                             ENT_CERT=="27"~"TABASCO",
                                                             ENT_CERT=="28"~"TAMAULIPAS",
                                                             ENT_CERT=="29"~"TLAXCALA",
                                                             ENT_CERT=="30"~"VERACRUZ DE IGNACIO DE LA LLAVE",
                                                             ENT_CERT=="31"~"YUCATAN",
                                                             ENT_CERT=="32"~"ZACATECAS"))



nacimientos2019<-nacimientos2019%>%select(id,ENT_CERT,NSOBREVIVIOPARTO)
nacimientos2019<-nacimientos2019%>%pivot_wider(names_from = ENT_CERT,values_from=NSOBREVIVIOPARTO,values_fill = FALSE)
nacimientos2019<-nacimientos2019%>%mutate(Anio=2019)
nacimientos2019<-nacimientos2019%>%mutate("BAJA CALIFORNIA"=FALSE,
                                          DURANGO=FALSE,
                                          GUANAJUATO=FALSE,
                                          MORELOS=FALSE,
                                          NAYARIT=FALSE,
                                          "QUERETARO  DE ARTEAGA"=FALSE,
                                          TAMAULIPAS=FALSE,
                                          TLAXCALA=FALSE)


#muestra 
ENT_CERT==1~"AGUASCALIENTES",
ENT_CERT==2~"BAJA CALIFORNIA",
ENT_CERT==3~"BAJA CALIFORNIA SUR",
ENT_CERT==4~"CAMPECHE",
ENT_CERT==5~"COAHUILA DE ZARAGOZA",
ENT_CERT==6~"COLIMA",
ENT_CERT==7~"CHIAPAS",
ENT_CERT==8~"CHIHUAHUA",
ENT_CERT==9~"DISTRITO FEDERAL",
ENT_CERT==10~"DURANGO",
ENT_CERT==11~"GUANAJUATO",
ENT_CERT==12~"GUERRERO",
ENT_CERT==13~"HIDALGO",
ENT_CERT==14~"JALISCO",
ENT_CERT==15~"MEXICO",
ENT_CERT==16~"MICHOACAN DE OCAMPO",
ENT_CERT==17~"MORELOS",
ENT_CERT==18~"NAYARIT",
ENT_CERT==19~"NUEVO LEON",
ENT_CERT==20~"OAXACA",
ENT_CERT==21~"PUEBLA",
ENT_CERT==22~"QUERETARO  DE ARTEAGA",
ENT_CERT==23~"QUINTANA ROO",
ENT_CERT==24~"SAN LUIS POTOSI",
ENT_CERT==25~"SINALOA",
ENT_CERT==26~"SONORA",
ENT_CERT==27~"TABASCO",
ENT_CERT==28~"TAMAULIPAS",
ENT_CERT==29~"TLAXCALA",
ENT_CERT==30~"VERACRUZ DE IGNACIO DE LA LLAVE",
ENT_cERT==31~"YUCATAN",
ENT_CERT==32~"ZACATECAS"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
nacimientos2020<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\sinac_2020.csv")
nacimientos2020<-nacimientos2020%>%filter(SOBREVIVIOPARTO==0)
nacimientos2020<-nacimientos2020%>%mutate(NSOBREVIVIOPARTO=TRUE)
nacimientos2020<-nacimientos2020%>%mutate(id=row_number())
nacimientos2020<-nacimientos2020%>%mutate(ENT_CERT=ENTIDADFEDERATIVACERTIFICA)
nacimientos2020<-nacimientos2020%>%mutate(ENT_CERT=case_when(ENT_CERT=="1"~"AGUASCALIENTES",
                                                             ENT_CERT=="2"~"BAJA CALIFORNIA",
                                                             ENT_CERT=="3"~"BAJA CALIFORNIA SUR",
                                                             ENT_CERT=="4"~"CAMPECHE",
                                                             ENT_CERT=="5"~"COAHUILA DE ZARAGOZA",
                                                             ENT_CERT=="6"~"COLIMA",
                                                             ENT_CERT=="7"~"CHIAPAS",
                                                             ENT_CERT=="8"~"CHIHUAHUA",
                                                             ENT_CERT=="9"~"DISTRITO FEDERAL",
                                                             ENT_CERT=="10"~"DURANGO",
                                                             ENT_CERT=="11"~"GUANAJUATO",
                                                             ENT_CERT=="12"~"GUERRERO",
                                                             ENT_CERT=="13"~"HIDALGO",
                                                             ENT_CERT=="14"~"JALISCO",
                                                             ENT_CERT=="15"~"MEXICO",
                                                             ENT_CERT=="16"~"MICHOACAN DE OCAMPO",
                                                             ENT_CERT=="17"~"MORELOS",
                                                             ENT_CERT=="18"~"NAYARIT",
                                                             ENT_CERT=="19"~"NUEVO LEON",
                                                             ENT_CERT=="20"~"OAXACA",
                                                             ENT_CERT=="21"~"PUEBLA",
                                                             ENT_CERT=="22"~"QUERETARO  DE ARTEAGA",
                                                             ENT_CERT=="23"~"QUINTANA ROO",
                                                             ENT_CERT=="24"~"SAN LUIS POTOSI",
                                                             ENT_CERT=="25"~"SINALOA",
                                                             ENT_CERT=="26"~"SONORA",
                                                             ENT_CERT=="27"~"TABASCO",
                                                             ENT_CERT=="28"~"TAMAULIPAS",
                                                             ENT_CERT=="29"~"TLAXCALA",
                                                             ENT_CERT=="30"~"VERACRUZ DE IGNACIO DE LA LLAVE",
                                                             ENT_CERT=="31"~"YUCATAN",
                                                             ENT_CERT=="32"~"ZACATECAS"))



nacimientos2020<-nacimientos2020%>%select(id,ENT_CERT,NSOBREVIVIOPARTO)
nacimientos2020<-nacimientos2020%>%pivot_wider(names_from = ENT_CERT,values_from=NSOBREVIVIOPARTO,values_fill = FALSE)
nacimientos2020<-nacimientos2020%>%mutate(Anio=2020)
nacimientos2020<-nacimientos2020%>%mutate(TLAXCALA=FALSE)

##############################3
################################
#prueba1 n2016
n_2017<-rbind(nacimientos2008,nacimientos2009,nacimientos2010,nacimientos2011,nacimientos2012,nacimientos2013,nacimientos2014,nacimientos2015,nacimientos2016,
              nacimientos2017,nacimientos2018,nacimientos2019,nacimientos2020)

#write.csv(n_2017,file="BOOPSTRAP_NACIMIENTOS_2008_2020.csv")
###################################################333
#########################################################3
############################################################
############################################################





n_2017<-n_2017%>%mutate(BAJA_CALIFONIA=`BAJA CALIFORNIA`,
                        BAJA_CALIFORNIA_SUR=`BAJA CALIFORNIA SUR`,
                        COAHUILA=`COAHUILA DE ZARAGOZA`,
                        DF=`DISTRITO FEDERAL`,
                        MICHOACAN=`MICHOACAN DE OCAMPO`,
                        NUEVO_LEON=`NUEVO LEON`,
                        QUERETARO=`QUERETARO  DE ARTEAGA`,
                        QUINTANA_ROO=`QUINTANA ROO`,
                        SAN_LUIS_POTOSI=`SAN LUIS POTOSI`,
                        VERACRUZ=`VERACRUZ DE IGNACIO DE LA LLAVE`)

n_2017<-dplyr::select(n_2017,-`VERACRUZ DE IGNACIO DE LA LLAVE`,
                      -`SAN LUIS POTOSI`,-`QUINTANA ROO`,-`QUERETARO  DE ARTEAGA`,
                      -`NUEVO LEON`,-`MICHOACAN DE OCAMPO`,-`DISTRITO FEDERAL`,
                      -`COAHUILA DE ZARAGOZA`,-`BAJA CALIFORNIA SUR`  ,-`BAJA CALIFORNIA`)


n_2017<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\PROYECTO FINAL\\BOOPSTRAP_NACIMIENTOS_2008_2020.csv")

str(n_2017)

library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)

library(GGally)

n_2017<-n_2017%>%mutate(BAJA_CALIFORNIA=BAJA_CALIFONIA)
n_2017<-dplyr::select(n_2017,-BAJA_CALIFONIA)

AGC_DF<-
  n_2017 %>%
  dplyr::select(Anio, AGUASCALIENTES:BAJA_CALIFORNIA) %>%
  pivot_longer(AGUASCALIENTES:BAJA_CALIFORNIA) %>%
  group_by(Anio, name) %>%
  summarise(prop = mean(value)) %>%
  ungroup() %>%
  filter(name %in% c("AGUASCALIENTES","BAJA_CALIFORNIA","BAJA_CALIFORNIA_SUR","CAMPECHE","CHIAPAS","CHIHUAHUA","COAHUILA","COLIMA","DF"          ))%>%
  ggplot(aes(as.numeric(Anio), prop, color = name)) +
  geom_line(size = 1.2, show.legend = FALSE) +
  facet_wrap(vars(name),scales="free") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Año", y = "% de muertes maternas",title="Comportamiento de la Mortalida Materna en México por Entidad Federativa",
       
       subtitle="Periodo 2008-2020",
       caption="Fuente: Elaboración propia.\nDatos recuperados de: http://www.dgis.salud.gob.mx/contenidos/basesdedatos/da_nacimientos_gobmx.html"
  )+
  xlim(2007,2020)+theme_bw()

#ggsave(AGC_DF,file="AGC_DF.pdf")  


#######################
######################
DUR_NAY<-n_2017 %>%
  dplyr::select(Anio, AGUASCALIENTES:BAJA_CALIFORNIA) %>%
  pivot_longer(AGUASCALIENTES:BAJA_CALIFORNIA) %>%
  group_by(Anio, name) %>%
  summarise(prop = mean(value)) %>%
  ungroup() %>%
  filter(name %in% c("DURANGO","GUANAJUATO","GUERRERO","HIDALGO","JALISCO",
                     "MEXICO","MICHOACAN","MORELOS","NAYARIT"
  ))%>%
  ggplot(aes(as.numeric(Anio), prop, color = name)) +
  geom_line(size = 1.2, show.legend = FALSE) +
  facet_wrap(vars(name),scales="free") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Año", y = "% de muertes maternas",title="Comportamiento de la Mortalida Materna en México por Entidad Federativa",
       
       subtitle="Periodo 2008-2020",
       caption="Fuente: Elaboración propia.\nDatos recuperados de: http://www.dgis.salud.gob.mx/contenidos/basesdedatos/da_nacimientos_gobmx.html"
  )+
  xlim(2007,2020)+theme_bw()
##############################33
##############################
NL_TAB<-n_2017 %>%
  dplyr::select(Anio, AGUASCALIENTES:BAJA_CALIFORNIA) %>%
  pivot_longer(AGUASCALIENTES:BAJA_CALIFORNIA) %>%
  group_by(Anio, name) %>%
  summarise(prop = mean(value)) %>%
  ungroup() %>%
  filter(name %in% c("NUEVO_LEON","OAXACA","PUEBLA","QUERETARO","QUINTANA_ROO","SAN_LUIS_POTOSI",
                     "SINALOA","SONORA" ,"TABASCO"
  ))%>%
  ggplot(aes(as.numeric(Anio), prop, color = name)) +
  geom_line(size = 1.2, show.legend = FALSE) +
  facet_wrap(vars(name),scales="free") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Año", y = "% de muertes maternas",title="Comportamiento de la Mortalida Materna en México por Entidad Federativa",
       
       subtitle="Periodo 2008-2020",
       caption="Fuente: Elaboración propia.\nDatos recuperados de: http://www.dgis.salud.gob.mx/contenidos/basesdedatos/da_nacimientos_gobmx.html"
  )+
  xlim(2007,2020)+theme_bw()
#########################3333
###########################
TAM_ZAC<-n_2017 %>%
  dplyr::select(Anio, AGUASCALIENTES:BAJA_CALIFORNIA) %>%
  pivot_longer(AGUASCALIENTES:BAJA_CALIFORNIA) %>%
  group_by(Anio, name) %>%
  summarise(prop = mean(value)) %>%
  ungroup() %>%
  filter(name %in% c("TAMAULIPAS","TLAXCALA","VERACRUZ","YUCATAN","ZACATECAS"
  ))%>%
  ggplot(aes(as.numeric(Anio), prop, color = name)) +
  geom_line(size = 1.2, show.legend = FALSE) +
  facet_wrap(vars(name),scales="free") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Año", y = "% de muertes maternas",title="Comportamiento de la Mortalida Materna en México por Entidad Federativa",
       
       subtitle="Periodo 2008-2020",
       caption="Fuente: Elaboración propia.\nDatos recuperados de: http://www.dgis.salud.gob.mx/contenidos/basesdedatos/da_nacimientos_gobmx.html"
  )+
  xlim(2007,2020)+theme_bw() 


n_20171<-dplyr::select(n_2017,-id,-X)
simple_mod <- lm(Anio ~ .,
                 data = n_20171
)


library(rsample)
bootstraps(n_20171, times = 1e3)

set.seed(123)
youtube_intervals <- reg_intervals(Anio ~ AGUASCALIENTES+
                                     BAJA_CALIFORNIA+
                                     BAJA_CALIFORNIA_SUR+
                                     CAMPECHE+
                                     COAHUILA+
                                     COLIMA+
                                     CHIAPAS+
                                     CHIHUAHUA+
                                     DF+
                                     DURANGO+
                                     GUANAJUATO+
                                     GUERRERO+
                                     HIDALGO+
                                     JALISCO+
                                     MEXICO+
                                     MICHOACAN+
                                     MORELOS+
                                     NAYARIT+
                                     NUEVO_LEON+
                                     OAXACA+
                                     PUEBLA+
                                     QUERETARO+
                                     QUINTANA_ROO+
                                     SAN_LUIS_POTOSI+
                                     SINALOA+
                                     SONORA+
                                     TABASCO+
                                     TAMAULIPAS+
                                     TLAXCALA+
                                     VERACRUZ+
                                     YUCATAN
                                   #ZACATECAS
                                   ,data = n_20171,
                                   type = "percentile",
                                   keep_reps = TRUE
)

#youtube_intervals
youtube_intervals1<-youtube_intervals%>%select(term:.upper)
write.csv(youtube_intervals1,file="Intervalos_Confianza_PEntidad_F.csv")

###################################################333
######################################################
######################################################
youtube_intervals %>%
  
  
  #filter(term %in% c("AGUASCALIENTESTRUE","BAJA_CALIFORNIATRUE","BAJA_CALIFORNIA_SURTRUE",
  #                  "CAMPECHETRUE","CHIAPASTRUE","CHIHUAHUATRUE","COAHUILATRUE","COLIMATRUE","DFTRUE"
  #))%>%
  mutate(
    term = str_remove(term, "TRUE"),
    term = fct_reorder(term, .estimate)
  ) %>%
  ggplot(aes(.estimate, term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray80") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper),
                 size = 1.5, alpha = 0.5, color = "midnightblue"
  ) +
  geom_point(size = 3, color = "midnightblue") +
  labs(title="Intervalos de Confianza Bootstrap para Mortalidad Materna en Mexico",
       subtitle="Alpha=0.05\nPeriodo 2008-2020 ",
       caption="Fuente: Elaboración propia.\nDatos recuperados de: http://www.dgis.salud.gob.mx/contenidos/basesdedatos/da_nacimientos_gobmx.html",
       x = "Estimaciones de Mortalidad Materna",
       y = NULL
  )+theme_bw()

youtube_intervals %>%
  filter(term %in% c("AGUASCALIENTESTRUE","BAJA_CALIFORNIATRUE","BAJA_CALIFORNIA_SURTRUE",
                     "CAMPECHETRUE","CHIAPASTRUE","CHIHUAHUATRUE","COAHUILATRUE","COLIMATRUE","DFTRUE"
  ))%>%
  mutate(
    term = str_remove(term, "TRUE"),
    term = fct_reorder(term, .estimate)
  ) %>%
  unnest(.replicates) %>%
  ggplot(aes(estimate, fill = term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray50") +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(vars(term),scales = "free")+
  labs(x = "Estimaciones", y = "Replicas",title="Intervalos de Confianza de la Mortalida Materna en México por Entidad Federativa",
       
       subtitle="Alpha = 0.05",
       caption="Fuente: Elaboración propia.\nDatos recuperados de: http://www.dgis.salud.gob.mx/contenidos/basesdedatos/da_nacimientos_gobmx.html"
  )+theme_bw()

###################################################333
######################################################
######################################################

youtube_intervals %>%
  filter(term %in% c("DURANGOTRUE","GUANAJUATOTRUE","GUERREROTRUE","HIDALGOTRUE","JALISCOTRUE",
                     "MICHOACANTRUE","MORELOSTRUE","NAYARITTRUE","NUEVO_LEONTRUE"
  ))%>%
  mutate(
    term = str_remove(term, "TRUE"),
    term = fct_reorder(term, .estimate)
  ) %>%
  ggplot(aes(.estimate, term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray80") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper),
                 size = 1.5, alpha = 0.5, color = "midnightblue"
  ) +
  geom_point(size = 3, color = "midnightblue") +
  labs(
    x = "Incremento de las muertes maternas",
    y = NULL
  )

youtube_intervals %>%
  filter(term %in% c("DURANGOTRUE","GUANAJUATOTRUE","GUERREROTRUE","HIDALGOTRUE","JALISCOTRUE",
                     "MICHOACANTRUE","MORELOSTRUE","NAYARITTRUE","NUEVO_LEONTRUE"
  ))%>%
  mutate(
    term = str_remove(term, "TRUE"),
    term = fct_reorder(term, .estimate)
  ) %>%
  unnest(.replicates) %>%
  ggplot(aes(estimate, fill = term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray50") +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(vars(term),scales="free")+
  labs(x = "Estimaciones", y = "Replicas",title="Intervalos de Confianza de la Mortalida Materna en México por Entidad Federativa",
       
       subtitle="Alpha = 0.05",
       caption="Fuente: Elaboración propia.\nDatos recuperados de: http://www.dgis.salud.gob.mx/contenidos/basesdedatos/da_nacimientos_gobmx.html"
  )+theme_bw()


###########################################
###########################################
###################################################333
######################################################
######################################################

youtube_intervals %>%
  filter(term %in% c("DURANGOTRUE","GUANAJUATOTRUE","GUERREROTRUE","HIDALGOTRUE","JALISCOTRUE",
                     "MICHOACANTRUE","MORELOSTRUE","NAYARITTRUE","NUEVO_LEONTRUE"
  ))%>%
  mutate(
    term = str_remove(term, "TRUE"),
    term = fct_reorder(term, .estimate)
  ) %>%
  ggplot(aes(.estimate, term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray80") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper),
                 size = 1.5, alpha = 0.5, color = "midnightblue"
  ) +
  geom_point(size = 3, color = "midnightblue") +
  labs(
    x = "Incremento de las muertes maternas",
    y = NULL
  )

youtube_intervals %>%
  filter(term %in% c("DURANGOTRUE","GUANAJUATOTRUE","GUERREROTRUE","HIDALGOTRUE","JALISCOTRUE",
                     "MICHOACANTRUE","MORELOSTRUE","NAYARITTRUE","NUEVO_LEONTRUE"
  ))%>%
  mutate(
    term = str_remove(term, "TRUE"),
    term = fct_reorder(term, .estimate)
  ) %>%
  unnest(.replicates) %>%
  ggplot(aes(estimate, fill = term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray50") +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(vars(term),scales="free")+
  labs(x = "Estimaciones", y = "Replicas",title="Intervalos de Confianza de la Mortalida Materna en México por Entidad Federativa",
       
       subtitle="Alpha = 0.05",
       caption="Fuente: Elaboración propia.\nDatos recuperados de: http://www.dgis.salud.gob.mx/contenidos/basesdedatos/da_nacimientos_gobmx.html"
  )+theme_bw()

###########################################
###########################################
###################################################333
######################################################
######################################################

youtube_intervals %>%
  filter(term %in% c("OAXACATRUE","PUEBLATRUE","QUERETAROTRUE","QUINTANA_ROOTRUE","SAN_LUIS_POTOSITRUE",
                     "SINALOATRUE","SONORATRUE","TABASCOTRUE","TAMAULIPASTRUE"
  ))%>%
  mutate(
    term = str_remove(term, "TRUE"),
    term = fct_reorder(term, .estimate)
  ) %>%
  ggplot(aes(.estimate, term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray80") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper),
                 size = 1.5, alpha = 0.5, color = "midnightblue"
  ) +
  geom_point(size = 3, color = "midnightblue") +
  labs(
    x = "Incremento de las muertes maternas",
    y = NULL
  )

youtube_intervals %>%
  filter(term %in% c("OAXACATRUE","PUEBLATRUE","QUERETAROTRUE","QUINTANA_ROOTRUE","SAN_LUIS_POTOSITRUE",
                     "SINALOATRUE","SONORATRUE","TABASCOTRUE","TAMAULIPASTRUE"
  ))%>%
  mutate(
    term = str_remove(term, "TRUE"),
    term = fct_reorder(term, .estimate)
  ) %>%
  unnest(.replicates) %>%
  ggplot(aes(estimate, fill = term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray50") +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(vars(term),scales="free")+
  labs(x = "Estimaciones", y = "Replicas",title="Intervalos de Confianza de la Mortalida Materna en México por Entidad Federativa",
       
       subtitle="Alpha = 0.05",
       caption="Fuente: Elaboración propia.\nDatos recuperados de: http://www.dgis.salud.gob.mx/contenidos/basesdedatos/da_nacimientos_gobmx.html"
  )+theme_bw()

######################################################
######################################################

youtube_intervals %>%
  filter(term %in% c("TLAXCALATRUE","VERACRUZTRUE","YUCATANTRUE"
  ))%>%
  mutate(
    term = str_remove(term, "TRUE"),
    term = fct_reorder(term, .estimate)
  ) %>%
  ggplot(aes(.estimate, term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray80") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper),
                 size = 1.5, alpha = 0.5, color = "midnightblue"
  ) +
  geom_point(size = 3, color = "midnightblue") +
  labs(
    x = "Incremento de las muertes maternas",
    y = NULL
  )

youtube_intervals %>%
  filter(term %in% c("TLAXCALATRUE","VERACRUZTRUE","YUCATANTRUE","MEXICOTRUE"
  ))%>%
  mutate(
    term = str_remove(term, "TRUE"),
    term = fct_reorder(term, .estimate)
  ) %>%
  unnest(.replicates) %>%
  ggplot(aes(estimate, fill = term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray50") +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(vars(term),scales="free")+
  labs(x = "Estimaciones", y = "Replicas",title="Intervalos de Confianza de la Mortalida Materna en México por Entidad Federativa",
       
       subtitle="Alpha = 0.05",
       caption="Fuente: Elaboración propia.\nDatos recuperados de: http://www.dgis.salud.gob.mx/contenidos/basesdedatos/da_nacimientos_gobmx.html"
  )+theme_bw()

