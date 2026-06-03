
 data(Boston,package="MASS") 
 b <- Boston 
 head(b)
 attach(b)
 library(locfit)
 # Regresion local
 #rm numero de cuartos en promedio por vivienda
 #lstat porcentaje de población con estatus social en la categoría inferior 
 plot(lstat,rm, xlab='Porcentaje de población', ylab='Numero de cuartos')
 w<-lm(rm~lstat)
abline(w) 
a<-b$rm
d<-b$lstat
cuarto<-as.data.frame(cbind(d,a))

plot(cuarto, main = "Lowess(Cuartos por vivienda)")
lines(lowess(cuarto), col = 2)
lines(lowess(cuarto, f = 0.2), col = 3)

modelo1 <- loess(a ~ d, cuarto, span = 0.75, degree = 1)
modelo2 <- loess(a ~ d, cuarto, span = 0.20, degree = 1)

plot(cuarto$d, cuarto$a, xlab = "Por_personas", ylab = "Cuartos por vivienda", main = "Loess(Cuartos)", degree = 1)
lines(cuarto$d, modelo1$fit, col = 2)
lines(cuarto$d, modelo2$fit, col = 3)
legend(5, 120, c(paste("span = ", c("0.75", "0.20"))), lty = 1, col = 2:3)


modelo11 <- loess(a ~ d, cuarto, span = 0.75, degree = 2)
modelo22 <- loess(a ~ d, cuarto, span = 0.20, degree = 2)
#####
plot(cuarto$d, cuarto$a, xlab = "Por_personas", ylab = "Cuartos por vivienda", main = "Loess(Cuartos)", degree = 1)
lines(cuarto$d, modelo11$fit, col = 2)
lines(cuarto$d, modelo22$fit, col = 3)
legend(5, 120, c(paste("span = ", c("0.75","0.20"))), lty = 1, col = 2:3)  







 
ggplot() + geom_point(data = nltest, aes(x = x, y = y), size = 0.9) + 
  geom_line(aes( x = nltest$x, y = y_poly_test_predict), color = "red") +
  xlab("Variable Independiente") + 
  ylab("Variable Dependiente") + 
  ggtitle("Curva de Ajuste sobre Conjunto de Validación (nltest)") 
