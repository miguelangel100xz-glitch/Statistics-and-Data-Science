
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
tabla<-table(rural_residente_2018$PAREN, rural_residente_2018$P3_9_3)
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
barplot(ylab="PORCENTAJE",xlab="TIPO DE RESPUESTA 1-SI 2-NO 3-NO CONTESTO",main="Gráfica Multivariable-Parentesco y Uso de Celular 2018",graficap,legend=rownames(graficap), beside=T,axis.lty=2, col=heat.colors(n.grup))
#Para ver los niveles de la variable es para un vector tipo factor  
unique(base$Sexo)
legend(x="topright",legend=c("Mujer", "Hombre","No identificado"),fill=c("royalblue","grey","red"),title="Loan")

#Prueba chi-cuadrada  
chisq.test(tabla)
IMOITIDO VARIABLE P3_9_1 NO RELACION ENTRE VARIABLES 


#Tabla de contigencia con valores observados (frecuencias)
tabla<-table(rural_residente_2018$P3_7, rural_residente_2018$P3_9_1)
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
unique(base$Sexo)
legend(x="topright",legend=c("Mujer", "Hombre","No identificado"),fill=c("royalblue","grey","red"),title="Loan")

#Prueba chi-cuadrada  
chisq.test(tabla)

mosaicplot(tabla, color=TRUE, main="ASISTE A LA ESCUELA-USO DE COMPUTADORA")


#####


#Tabla de contigencia con valores observados (frecuencias)
tabla<-table(rural_residente_2018$P3_7, rural_residente_2018$P3_9_3)
tabla
#para realizar la gráfica por frecuencia
barplot(tabla)

#para realizar tabla de porcentaje total  
graficapro<-prop.table(tabla)
graficapro
#para pasar en términos de porcentaje 
graficap<-round(prop.table(tabla)*100,digits=3)
graficap
barplot(graficapro, main="ASISTE A LA ESCUELA-USO DE CELULAR", xlab="Tipo de respuesta si/no/no contesto",ylab="Proporción",col=c("royalblue","grey","red"))
barplot(graficap, main="ASISTE A LA ESCUELA-USO DE CELULAR", xlab="Tipo de respuesta si/no/no contesto ",ylab="Proporción",col=c("royalblue","grey","red"))
#Grafico de barras para dos variables
n.grup<-nrow(graficap)
n.grup
barplot(graficap,legend=rownames(graficap), beside=T,axis.lty=2, col=heat.colors(n.grup))
#Para ver los niveles de la variable es para un vector tipo factor  
unique(base$Sexo)
legend(x="topright",legend=c("Mujer", "Hombre","No identificado"),fill=c("royalblue","grey","red"),title="Loan")

#Prueba chi-cuadrada  
chisq.test(tabla)

mosaicplot(tabla, color=TRUE, main="ASISTE A LA ESCUELA-USO DE COMPUTADORA")

###
#####


#Tabla de contigencia con valores observados (frecuencias)
tabla<-table(rural_residente_2018$NIVEL, rural_residente_2018$P3_9_1)
tabla
#para realizar la gráfica por frecuencia
barplot(tabla)

#para realizar tabla de porcentaje total  
graficapro<-prop.table(tabla)
graficapro
#para pasar en términos de porcentaje 
graficap<-round(prop.table(tabla)*100,digits=3)
graficap
barplot(graficapro, main="NIVEL DE ESTUDIO-USO DE COMPUTADORA", xlab="Tipo de respuesta si/no/no contesto",ylab="Proporción",col=c("royalblue","grey","red"))
barplot(graficap, main="NIVEL DE ESTUDIO_USO DE COMPUTADORA", xlab="Tipo de respuesta si/no/no contesto ",ylab="Proporción",col=c("royalblue","grey","red"))
#Grafico de barras para dos variables
n.grup<-nrow(graficap)
n.grup
barplot(graficap,legend=rownames(graficap), beside=T,axis.lty=2, col=heat.colors(n.grup))
#Para ver los niveles de la variable es para un vector tipo factor  
unique(base$Sexo)
legend(x="topright",legend=c("Mujer", "Hombre","No identificado"),fill=c("royalblue","grey","red"),title="Loan")

#Prueba chi-cuadrada  
chisq.test(tabla)

mosaicplot(tabla, color=TRUE, main="NIVEL DE ESTUDIO-USO DE COMPUTADORA",ylab="RESPUESTA SI/NO/NO CONTESTO",xlab="NIVEL EDUCATIVO")
###


#Tabla de contigencia con valores observados (frecuencias)
tabla<-table(rural_residente_2018$P3_12, rural_residente_2018$P3_9_3)
tabla
#para realizar la gráfica por frecuencia
barplot(tabla)

#para realizar tabla de porcentaje total  
graficapro<-prop.table(tabla)
graficapro
#para pasar en términos de porcentaje 
graficap<-round(prop.table(tabla)*100,digits=3)
graficap
barplot(graficapro, main="OCUPACION-USO DE COMPUT", xlab="Tipo de respuesta si/no/no contesto",ylab="Proporción",col=c("royalblue","grey","red"))
barplot(graficap, main="OCUPACION_USO DE CELULAR", xlab="Tipo de respuesta si/no/no contesto ",ylab="Proporción",col=c("royalblue","grey","red"))
#Grafico de barras para dos variables
n.grup<-nrow(graficap)
n.grup
barplot(graficap,legend=rownames(graficap), beside=T,axis.lty=2, col=heat.colors(n.grup))
#Para ver los niveles de la variable es para un vector tipo factor  
unique(base$Sexo)
legend(x="topright",legend=c("Mujer", "Hombre","No identificado"),fill=c("royalblue","grey","red"),title="Loan")

#Prueba chi-cuadrada  
chisq.test(tabla)

mosaicplot(tabla, color=TRUE, main="OCUPACION-USO DE CELULAR",ylab="RESPUESTA SI/NO/NO CONTESTO",xlab="OCUPACION")


######














