
##BASE DE DATOS 2017 
#LAS BASES DE DATOS DE HOGAR Y USUARIO COINCIDEN INICIALMENTE 
#TODAS DEL AÑO 2017 
# las bases de datos de  veracruz_xxxxxx_2017 coinciden en casos

hogar2017<-read.csv("C:/Users/W10/Desktop/PROYECTO CULTURA ESTADISTICA/Base de datos Tics/2017/conjunto_de_datos/tr_hogar.csv",header=TRUE)
dim(hogar2017)
data.frame(hogar2017)
##
veracruz_hogar_2017<- hogar2017[hogar2017$ENT == 30,]
View(veracruz_hogar_2017)
dim(veracruz_hogar_2017)
sapply(veracruz_hogar_2017, function(x) sum(is.na(x)))
##
rural_hogar_2017<-veracruz_hogar_2017[is.na(veracruz_hogar_2017$CD_ENDUTIH),]
dim(rural_hogar_2017)


#C:\Users\W10\Desktop\PROYECTO CULTURA ESTADISTICA\Base de datos Tics\2017\conjunto_de_datos
residente2017<-read.csv("C:/Users/W10/Desktop/PROYECTO CULTURA ESTADISTICA/Base de datos Tics/2017/conjunto_de_datos/tr_residente.csv",header=TRUE)
dim(residente2017)
data.frame(residente2017)
##
veracruz_residente_2017<-residente2017[residente2017$ENT==30,]
dim(veracruz_residente_2017)
sapply(veracruz_residente_2017, function(x) sum(is.na(x)))
##
rural_residente_2017<-veracruz_residente_2017[is.na(veracruz_residente_2017$CD_ENDUTIH),]
dim(rural_residente_2017)


usuario2017<-read.csv("C:/Users/W10/Desktop/PROYECTO CULTURA ESTADISTICA/Base de datos Tics/2017/conjunto_de_datos/tr_usuario.csv",header=TRUE)
dim(usuario2017)
data.frame(usuario2017)
##
veracruz_usuario_2017<-usuario2017[usuario2017$ENT==30,]
dim(veracruz_usuario_2017)
sapply(veracruz_usuario_2017, function(x) sum(is.na(x)))
##
rural_usuario_2017<-veracruz_usuario_2017[is.na(veracruz_usuario_2017$CD_ENDUTIH),]
dim(rural_usuario_2017)


#####

#Gráfico de barras en porcentaje
barplot(pocentaje1)
#Gráfico de barras en frecuencias 
plot(base$Sexo, main="Grado máximo de estudios de los docentes",xlab="Grado máximo de estudios",ylab="Frecuencia",col=c("royalblue","seagreen","purple"))

#Tabla de contigencia con valores observados (frecuencias)
tabla<-table(rural_residente_2017$P3_9_2, rural_residente_2017$P3_12)
tabla
#para realizar la gráfica por frecuencia
barplot(tabla,sub="NIVEL EDUCATIVO
",main="FRECUENCIA OCUPACIÓN-USO DE INTERNET AÑO 2017 ",ylab="NÚMERO DE CASOS",col=c("royalblue","seagreen","purple"))

#para realizar tabla de porcentaje total  
graficapro<-prop.table(tabla)
graficapro
#para pasar en términos de porcentaje 
graficap<-round(prop.table(tabla)*100,digits=3)
graficap
barplot(graficapro, main="UTILIZACION DE CELULAR EN CUANTO A GÉNERO AÑO 2017", xlab="Tipo de ocupación",ylab="Proporción",col=c("royalblue","grey","red"))
barplot(graficap, main="UTILIZACION DE CELULAR EN CUANTO A GÉNERO AÑO 2017", xlab="GÉNERO 1-MASCULINO 2-FEMENINO",ylab="Proporción",col=c("royalblue","grey","red"))
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
mosaicplot(tabla,sub="RESPUESTAS 1-SI 2-NO 
",main="FRECUENCIA OCUPACION-USO DE INTERNET AÑO 2017 ",ylab="NIVEL EDUCATIVO",col=c("royalblue","seagreen","purple"))


