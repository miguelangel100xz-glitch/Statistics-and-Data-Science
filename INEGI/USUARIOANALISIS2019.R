############%%%%%%%%%%%########

usuario2019<-read.csv("C:/Users/W10/Desktop/PROYECTO CULTURA ESTADISTICA/Base de datos Tics/2019/conjunto_de_datos/tr_endutih_usuario_anual_2019.csv",header=TRUE)
dim(usuario2019)
names(usuario2019)
data.frame(usuario2019)
class(usuario2019)
###
rural_usuario_2019<- usuario2019[usuario2019$TLOC== 4,]
dim(rural_usuario_2019)


#Gráfico de barras en porcentaje
barplot(pocentaje1)
#Gráfico de barras en frecuencias 
plot(base$Sexo, main="Grado máximo de estudios de los docentes",xlab="Grado máximo de estudios",ylab="Frecuencia",col=c("royalblue","seagreen","purple"))

#Tabla de contigencia con valores observados (frecuencias)
tabla<-table(rural_usuario_2019$SEXO, rural_usuario_2019$P6_1)
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
barplot(graficap, main="PARENTESCO Y DISPONE DE CELULAR", xlab="Tipo de respuesta si/no/no contesto",ylab="Proporción",col=c("royalblue","grey","red"))
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
mosaicplot(tabla, color=TRUE,colnames(tabla),main="PARENTESCO- DISPONE DE CELULAR ",xlab=" ESTRATO",ylab="TIPO DE RESPUESTA SI-NO-NO CONTESTO")

 
