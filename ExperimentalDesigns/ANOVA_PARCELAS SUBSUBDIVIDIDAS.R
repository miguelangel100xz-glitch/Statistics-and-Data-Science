library(agricolae)

# Statistical procedures for agricultural research, pag 143
# Grain Yields of Three Rice Varieties Grown under 
#Three Management practices and Five Nitrogen levels; in a
#split-split-plot design with nitrogen as main-plot, 
#management practice as subplot, and variety as sub-subplot 
#factores, with three replications.
library(agricolae)
data<-read.csv("C:\\Users\\W10\\Desktop\\PROYECTO EQUIPO\\ANALISIS_TEMPE.csv",header=TRUE)
model<-with(data,ssp.plot(REPETICION,ZONA,TIEMPO,TRATAMIENTO,GRADOS_FAREN))
gla<-model$gl.a; glb<-model$gl.b; glc<-model$gl.c
Ea<-model$Ea; Eb<-model$Eb; Ec<-model$Ec
op<-par(mfrow=c(1,3),cex=0.6)
out1<-with(data,LSD.test(GRADOS_FAREN,ZONA,gla,Ea,console=TRUE))
out2<-with(data,LSD.test(GRADOS_FAREN,TIEMPO,glb,Eb,console=TRUE))
out3<-with(data,LSD.test(GRADOS_FAREN,TRATAMIENTO,glc,Ec,console=TRUE))
plot(out1,xlab="ZONA",las=1,variation="IQR")
plot(out2,xlab="TIEMPO",variation="IQR")
plot(out3,xlab="TRATAMIENTO",variation="IQR")
# with aov
AOV<-aov(GRADOS_FAREN ~  REPETICION + ZONA*TIEMPO*TRATAMIENTO + Error(REPETICION/ZONA/TIEMPO),data=data)
summary(AOV)
par(op)
