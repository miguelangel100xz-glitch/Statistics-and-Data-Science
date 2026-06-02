data<-read.delim("clipboard",stringsAsFactors = TRUE)
head(data)

data$var = as.factor(data$var)
data$age = as.factor(data$age)
str(data)

library(agricolae)
attach(data)

model <- sp.plot(block = block, 
                 pplot = var, 
                 splot = age, 
                 Y = heading)


##ANALIZIS POST HOC 
Edf_a <- model$gl.a
Edf_a
Edf_b <- model$gl.b
Edf_b
EMS_a <- model$Ea
EMS_a
EMS_b <- model$Eb
EMS_b
#variedades
out1 <- LSD.test(y = heading, 
                 trt = var,
                 DFerror = Edf_a, 
                 MSerror = EMS_a,
                 alpha = 0.05,
                 p.adj = "bonferroni",
                 group = TRUE,
                 console = TRUE)

plot(out1, 
     xlab = "varieties",
     ylab = "heading (days)",
     las = 1, 
     variation = "IQR")
#edad de la planta 
out2 <- LSD.test(y = heading, 
                 trt = age,
                 DFerror = Edf_b, 
                 MSerror = EMS_b,
                 alpha = 0.05,
                 p.adj = "bonferroni",
                 group = TRUE,
                 console = TRUE)

plot(out2, 
     xlab = "Siembra",
     ylab = "Maduracion (dias)",
     las = 1, 
     variation = "IQR")

#INTERACCION 
out3 <- LSD.test(y = heading, 
                 trt = var:age,
                 DFerror = Edf_b, 
                 MSerror = EMS_b,
                 alpha = 0.05,
                 p.adj = "bonferroni",
                 group = TRUE,
                 console = TRUE)


plot(out3, 
     xlab = "var:age",
     ylab = "heading (days)",
     las = 1, 
     variation = "IQR")






