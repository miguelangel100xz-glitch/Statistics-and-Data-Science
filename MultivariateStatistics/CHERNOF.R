library(aplpack)
library(readxl)

empleo<- read_xlsx("C:\\Users\\W10\\Desktop\\ESTADISTICA MULTIVARIADA\\R\\POR_EMPLEO.xlsx")
Caras<-faces(empleo[,2:9] )
Caras



