

##Base de datos 2018
##LAS BASES DE DATOS DE HOGAR2018 Y USUARIO 2018 
##COINCIDEN EN NUMERO DIPONIBLE PARA ANALISIS CHI CUADRADO
##coinciden hogar y usuario en veracruz_xxxxx_2018 mismo numero de casos 
# usario tiene variable sexo al final 


hogar2018<-read.csv("C:/Users/W10/Desktop/PROYECTO CULTURA ESTADISTICA/Base de datos Tics/2018/conjunto_de_datos/tr_endutih_hogar_anual_2018.csv",header=TRUE)
dim(hogar2018)
names(hogar2018)
data.frame(hogar2018)
class(hogar2018)
##
veracruz_hogar_2018<- hogar2018[hogar2018$ENT == 30,]
dim(veracruz_hogar_2018)
View(veracruz_hogar_2018)
sapply(veracruz_hogar_2018, function(x) sum(is.na(x)))
##
rural_hogar_2018<-veracruz_hogar_2018[is.na(veracruz_hogar_2018$CD_ENDUTIH),]
dim(rural_hogar_2018)


residente2018<-read.csv("C:/Users/W10/Desktop/PROYECTO CULTURA ESTADISTICA/Base de datos Tics/2018/conjunto_de_datos/tr_endutih_residente_anual_2018.csv",header=TRUE)
dim(residente2018)
names(residente2018)
data.frame(residente2018)
class(residente2018)
##
veracruz_residente_2018<-residente2018[residente2018$ENT==30,]
dim(veracruz_residente_2018)
sapply(veracruz_residente_2018, function(x) sum(is.na(x)))
##
rural_residente_2018<-veracruz_residente_2018[is.na(veracruz_residente_2018$CD_ENDUTIH),]
dim(rural_residente_2018)


usuario2018<-read.csv("C:/Users/W10/Desktop/PROYECTO CULTURA ESTADISTICA/Base de datos Tics/2018/conjunto_de_datos/tr_endutih_usuario_anual_2018.csv",header=TRUE)
dim(usuario2018)
names(usuario2018)
data.frame(usuario2018)
##
veracruz_usuario_2018<-usuario2018[usuario2018$ENT==30,]
dim(veracruz_usuario_2018)
sapply(veracruz_usuario_2018, function(x) sum(is.na(x)))
##
rural_usuario_2018<-veracruz_usuario_2018[is.na(veracruz_usuario_2018$CD_ENDUTIH),]


##Base de datos 2018
##LAS BASES DE DATOS DE HOGAR2018 Y USUARIO 2018 
##COINCIDEN EN NUMERO DIPONIBLE PARA ANALISIS CHI CUADRADO
##coinciden hogar y usuario en veracruz_xxxxx_2018 mismo numero de casos 
# usario tiene variable sexo al final 


hogar2018<-read.csv("C:/Users/W10/Desktop/PROYECTO CULTURA ESTADISTICA/Base de datos Tics/2018/conjunto_de_datos/tr_endutih_hogar_anual_2018.csv",header=TRUE)
dim(hogar2018)
names(hogar2018)
data.frame(hogar2018)
class(hogar2018)
##
veracruz_hogar_2018<- hogar2018[hogar2018$ENT == 30,]
dim(veracruz_hogar_2018)
View(veracruz_hogar_2018)
sapply(veracruz_hogar_2018, function(x) sum(is.na(x)))
##
rural_hogar_2018<-veracruz_hogar_2018[is.na(veracruz_hogar_2018$CD_ENDUTIH),]
dim(rural_hogar_2018)


residente2018<-read.csv("C:/Users/W10/Desktop/PROYECTO CULTURA ESTADISTICA/Base de datos Tics/2018/conjunto_de_datos/tr_endutih_residente_anual_2018.csv",header=TRUE)
dim(residente2018)
names(residente2018)
data.frame(residente2018)
class(residente2018)
##
veracruz_residente_2018<-residente2018[residente2018$ENT==30,]
dim(veracruz_residente_2018)
sapply(veracruz_residente_2018, function(x) sum(is.na(x)))
##
rural_residente_2018<-veracruz_residente_2018[is.na(veracruz_residente_2018$CD_ENDUTIH),]
dim(rural_residente_2018)


usuario2018<-read.csv("C:/Users/W10/Desktop/PROYECTO CULTURA ESTADISTICA/Base de datos Tics/2018/conjunto_de_datos/tr_endutih_usuario_anual_2018.csv",header=TRUE)
dim(usuario2018)
names(usuario2018)
data.frame(usuario2018)
##
veracruz_usuario_2018<-usuario2018[usuario2018$ENT==30,]
dim(veracruz_usuario_2018)
sapply(veracruz_usuario_2018, function(x) sum(is.na(x)))
##
rural_usuario_2018<-veracruz_usuario_2018[is.na(veracruz_usuario_2018$CD_ENDUTIH),]
dim(rural_usuario_2018)

###analisis chi cuadrado
##coinciden hogar y usuario en veracruz_xxxxx_2018 mismo numero de casos 
# usario tiene variable sexo al final 

names(rural_usuario_2018)


##Gráfico de pastel  
frecuencia1<-table(base$Sexo)
frecuencia1
pocentaje1<-round(prop.table(table(base$Sexo))*100,digits=2)
pocentaje1
#round(pocentaje1, 2)

names(pocentaje)
etiquetas1<-paste(labels=c("Hombre","Mujer"," No identificado"),pocentaje1, "%", sep=" ")
etiquetas1
pie(pocentaje1, etiquetas1, col=c("purple","blue","red"))

#labels=c("F","M")
 #shadow=FALSE,edges=200,radius=0.8)

#Gráfico de barras en porcentaje
barplot(pocentaje1)
#Gráfico de barras en frecuencias 
plot(base$Sexo, main="Grado máximo de estudios de los docentes",xlab="Grado máximo de estudios",ylab="Frecuencia",col=c("royalblue","seagreen","purple"))

#Tabla de contigencia con valores observados (frecuencias)
tabla<-table(rural_hogar_2018$ESTRATO, rural_hogar_2018$P4_5_1)
tabla
#para realizar la gráfica por frecuencia
barplot(tabla)

#para realizar tabla de porcentaje total  
graficapro<-prop.table(tabla)
graficapro
#para pasar en términos de porcentaje 
graficap<-round(prop.table(tabla)*100,digits=3)
graficap
barplot(graficapro, main="Sexo por ocupación", xlab="Tipo de ocupación",ylab="Proporción",col=c("royalblue","grey","red"))
barplot(graficap, main="Sexo por ocupación", xlab="Tipo de ocupación",ylab="Proporción",col=c("royalblue","grey","red"))
#Grafico de barras para dos variables
n.grup<-nrow(graficap)
n.grup
barplot(graficap,legend=rownames(graficap), beside=T,axis.lty=2, col=heat.colors(n.grup))
#Para ver los niveles de la variable es para un vector tipo factor  
unique(rural_usuario_2018$SEXO)
legend(x="topright",legend=c("Mujer", "Hombre","No identificado"),fill=c("royalblue","grey","red"),title="Loan")

#Prueba chi-cuadrada  
chisq.test(tabla)
fisher.test(tabla)

 

