	
##BASE DE DATOS 2019
#LAS GRAFICAS DE USUARIO Y HOGAR COINCIDEN EN RURALES 


hogar2019<-read.csv("C:/Users/W10/Desktop/PROYECTO CULTURA ESTADISTICA/Base de datos Tics/2019/conjunto_de_datos/tr_endutih_hogar_anual_2019.csv",header=TRUE)
dim(hogar2019)
names(hogar2019)
data.frame(hogar2019)
class(hogar2019)
###
rural_hogar_2019<- hogar2019[hogar2019$TLOC== 4,]
dim(rural_hogar_2019)

residente2019<- read.csv("C:/Users/W10/Desktop/PROYECTO CULTURA ESTADISTICA/Base de datos Tics/2019/conjunto_de_datos/tr_endutih_residente_anual_2019.csv",header=TRUE)
dim(residente2019)
names(residente2019)
data.frame(residente2019)
class(residente2019)
###
rural_residente_2019<- residente2019[residente2019$TLOC== 4,]
dim(rural_residente_2019)

usuario2019<-read.csv("C:/Users/W10/Desktop/PROYECTO CULTURA ESTADISTICA/Base de datos Tics/2019/conjunto_de_datos/tr_endutih_usuario_anual_2019.csv",header=TRUE)
dim(usuario2019)
names(usuario2019)
data.frame(usuario2019)
class(usuario2019)
###
rural_usuario_2019<- usuario2019[usuario2019$TLOC== 4,]
dim(rural_usuario_2019)name
#####$$$$$$$######

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
tabla<-table(rural_residente_2019$SEXO, rural_residente_2019$P3_9_2)
tabla
#para realizar la gráfica por frecuencia
barplot(tabla)

#para realizar tabla de porcentaje total  
graficapro<-prop.table(tabla)
graficapro
#para pasar en términos de porcentaje 
graficap<-round(prop.table(tabla)*100,digits=3)
graficap
barplot(graficapro, main="Estrato y computadora de escritorio", xlab="Tipo de ocupación",ylab="Proporción",col=c("royalblue","grey","red"))
barplot(graficap, main="ESTRATO Y USO DE COMPUTADORA", xlab="Tipo de respuesta si/no/no contesto",ylab="Proporción",col=c("royalblue","grey","red"))
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
mosaicplot(tabla, color=TRUE,colnames(tabla),main="ESTRATO- USO DE COMPUTADORA",xlab=" ESTRATO",ylab="TIPO DE RESPUESTA SI-NO-NO CONTESTO")

 
###relacion computadora portatil

