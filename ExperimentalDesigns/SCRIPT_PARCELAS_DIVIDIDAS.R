#Se obtuvo de un experimento 
#de parcela dividida con dos factores 
#experimentales: tres métodos de labranza 
#(arado poco profundo, arado profundo y
#labranza mínima) y dos métodos de control
#de malezas (total y parcial, lo que
#significa que el herbicida se roció al
#voleo o solo a lo largo de las hileras de
#cultivo). . Los métodos de labranza se 
#asignaron a las parcelas principales, 
#mientras que los métodos de control 
#de malezas se asignaron a las subparcelas
#y el experimento se diseñó en cuatro 
#bloques completos.l




dataset<- read.delim("clipboard", stringsAsFactors = TRUE)
mod.aov <- aov(Produccion ~ Arado*C_Maleza +
                 Error(Bloque/Arado), data = dataset)
summary(mod.aov)

mod.aov2 <- aov(Produccion ~ Bloque + Arado*C_Maleza +
                  Error(Bloque:Arado), data = dataset)
summary(mod.aov2)


