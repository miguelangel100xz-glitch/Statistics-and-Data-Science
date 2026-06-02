

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


#####

#Gráfico de barras en porcentaje
barplot(pocentaje1)
#Gráfico de barras en frecuencias 
plot(base$Sexo, main="Grado máximo de estudios de los docentes",xlab="Grado máximo de estudios",ylab="Frecuencia",col=c("royalblue","grey","red"))

#Tabla de contigencia con valores observados (frecuencias)
tabla<-table(rural_residente_2017$SEXO, rural_residente_2017$P3_9_2)
tabla
#para realizar la gráfica por frecuencia
barplot(tabla)

#para realizar tabla de porcentaje total  
graficapro<-prop.table(tabla)
graficapro
#para pasar en términos de porcentaje 
graficap<-round(prop.table(tabla)*100,digits=3)
graficap
#barplot(graficapro, main="NIVEL EDUCAATIVO y USO DE CELULAR", xlab="GENERO",ylab="Proporción",col=c("royalblue","grey","red"))
#barplot(graficap, main="Gráfico exploratorio uso de celular ", xlab="RESPUESTA SI-NO",ylab="PARENTESCO",col=c("royalblue","grey","red"))
##Grafico de barras para dos variables
n.grup<-nrow(graficap)
n.grup
barplot(ylab="PORCENTAJE",xlab="GÉNERO 1-MASCULINO 2-FEMENINO",main="Gráfico exploratorio uso de celular 2017",graficap,legend=rownames(graficap), beside=T,axis.lty=2, col=heat.colors(n.grup))
#Para ver los niveles de la variable es para un vector tipo factor  
unique(rural_usuario_2018$SEXO)
legend(x="topright",legend=c("Mujer", "Hombre","No identificado"),fill=c("yellow","red","red"),title="Simbologia")

#Prueba chi-cuadrada  
chisq.test(tabla)
fisher.test(tabla)
mosaicplot(tabla, color=TRUE,colnames(tabla),main="PARENTESCO Y USO DE CELULAR ",ylab="RESPUESTAS NO-SI",xlab="PARENTESCO")
####

