
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
names(rural_hogar_2017)



#Gráfico de barras en porcentaje
barplot(pocentaje1)
#Gráfico de barras en frecuencias 
plot(base$Sexo, main="Grado máximo de estudios de los docentes",xlab="Grado máximo de estudios",ylab="Frecuencia",col=c("royalblue","seagreen","purple"))

#Tabla de contigencia con valores observados (frecuencias)
tabla<-table(rural_hogar_2018$ESTRATO, rural_hogar_2018$P4_2_1)
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
mosaicplot(tabla, color=TRUE,colnames(tabla),main="Estrato-Computadora de escritorio")


