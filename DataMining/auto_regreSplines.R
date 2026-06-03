 
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(purrr)
 
  
 library(ISLR)
str(Auto)
library(ggplot2)
ggplot(data = Auto, aes(x = displacement, y = mpg)) +
geom_point(col = "darkgrey") +
ggtitle("mpg vs displacement") +
theme_bw() +
theme(panel.background = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
theme(plot.title = element_text(hjust = 0.5))   

set.seed(1)
# Índice observaciones de entrenamiento
train <- sample(nrow(Auto), 0.8*nrow(Auto), replace = FALSE)
datosA.train <- Auto[train, ]
datosA.test <- Auto[-train, ]

nrow(datosA.train) + nrow(datosA.test)



library(boot)
# Vector para almacenar el error de validación de cada polinomio
cv.error <- rep(NA, 10)
# Vector para almacenar el RSS de cada polinomio
rss <- rep (NA, 10)
for (i in 1:10){
modelo.poli <- glm(mpg ~ poly(displacement, i), data = datosA.train)
set.seed(2)
cv.error[i] <- cv.glm(datosA.train, modelo.poli, K = 10)$delta[1]
rss[i] <- sum(modelo.poli$residuals^2)
}

ggplot(data = data.frame(polinomio = 1:10, cv.error = cv.error), 
       aes(x = polinomio, y = cv.error)) +
geom_point(color = "orangered2") +
geom_path() +
scale_x_continuous(breaks = 0:10) +
labs(title = "cv.MSE  ~ Grado de polinomio") +
theme_bw() +
theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
theme(plot.title = element_text(hjust = 0.5))


ggplot(data = data.frame(polinomio = 1:10, RSS = rss), aes(x = polinomio, 
                                                           y = RSS)) +
geom_point(color = "orangered2") +
geom_path() +
scale_x_continuous(breaks=0:10) +
labs(title = "RSS  ~ Grado de polinomio") +
theme_bw() +
theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
theme(plot.title = element_text(hjust = 0.5))

# Polinomio con menor error de validación
which.min(cv.error)

modelo.poli <- lm(mpg ~ poly(displacement, 2), data = datosA.train)
summary(modelo.poli)

ggplot(data = datosA.train, aes(x = displacement, y = mpg)) +
geom_point(col = "darkgrey") +
geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue", se = TRUE, 
            level = 0.95) +
labs(title = "Polinomio grado 2: mpg ~ displacement") +
theme_bw() + theme(plot.title = element_text(hjust = 0.5))

#Comparación de modelos
modelo1 <- lm(mpg ~ displacement, data = datosA.train)
modelo2 <- lm(mpg ~ poly(displacement, 2), data = datosA.train)
modelo3 <- lm(mpg ~ poly(displacement, 3), data = datosA.train)

anova(modelo1, modelo2, modelo3)

#Evaluación de modelos
pred.modelo.poli <-  predict(modelo.poli, datosA.test)
test.error.poli <- mean((pred.modelo.poli - datosA.test$mpg)^2)
test.error.poli

Spline de regresión

• bs() -> Genera la matriz base para un spline polinómico. 
  El grado por defecto es 3 (cubic spline).
• ns() -> Genera la matriz base para un spline cúbico.
• smooth.spline() -> Ajusta un spline de suavizado cúbico.


library(splines)
# Vector donde se almacenarán los errores de validación
cv.error <- rep(NA, 16)
# K-fold cross validation para cada valor de df
for(i in 2:16){
modelo.spline <- glm(mpg ~ bs(displacement, degree = 2, df = i), 
                     data = datosA.train)
set.seed(3)
cv.error[i] = cv.glm(datosA.train, modelo.spline, K = 10)$delta[1]
}

# El primer valor del vector cv.error son NA ya que no se ha evaluado en el for loop el valor 1 
head(cv.error)


ggplot(data = data.frame(grados_libertad = 2:16, cv.error = cv.error[-1]), 
       aes(x = grados_libertad, y = cv.error)) +
geom_point(color = "orangered2") +
geom_path() +
scale_x_continuous(breaks=3:16) +
labs(title = "cv.MSE  ~ Grados de libertad") +
theme_bw() +
theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
theme(plot.title = element_text(hjust = 0.5))

which.min(cv.error)

attr(x = bs(datosA.train$displacement, degree = 2, df = 6), which = "knots")

# Modelo spline
modelo.spline <- lm(mpg ~ bs(displacement, degree = 2, df = 6), 
                    data = datosA.train)
summary(modelo.spline)

# SPLINE
ggplot(data = datosA.train, aes(x = displacement, y = mpg)) +
geom_point(col = "darkgrey") +
geom_smooth(method = "lm", formula = y ~ bs(x, degree = 2, df = 6), 
            color = "red", se = TRUE, level = 0.95) +
labs(title = "Spline con df = 6: mpg ~ displacement") +
theme_bw() + theme(plot.title = element_text(hjust = 0.5))

# SPLINE NATURAL (por defecto, cúbico) 
ggplot(data = datosA.train, aes(x = displacement, y = mpg)) +
geom_point(col = "darkgrey") +
geom_smooth(method = "lm", formula = y ~ ns(x, df = 6), color = "black", 
            se = TRUE, level = 0.95) +
labs(title = "Natural spline con df = 6: mpg ~ displacement") +
theme_bw() + theme(plot.title = element_text(hjust = 0.5))

# SPLINE DE SUAVIZADO
modelo.spline.s <- smooth.spline(x = datosA.train$displacement, 
                                 y = datosA.train$mpg, cv = TRUE)

# Grados de libertad y lambda correspondientes al modelo ajustado
modelo.spline.s$df

modelo.spline.s$lambda

plot(mpg ~ displacement, data = Auto, col = "darkgrey")
title("Smoothing spline (df = 4,65)")
lines(modelo.spline.s, col = "darkgreen", lwd = 2)

pred.modelo.spline <-  predict(modelo.spline, datosA.test)
test.error.spline <- mean((pred.modelo.spline - datosA.test$mpg)^2)
test.error.spline

# REGRESIÓN LOCAL
modelo.local <- loess(mpg ~ displacement, span = 0.7, data = datosA.train)

ggplot(data = datosA.train, aes(x = displacement, y = mpg)) +
geom_point(col = "darkgrey") +
geom_smooth(method = "loess", formula = y ~ x, span = 0.2, 
            color = "orange", se = F) +
geom_smooth(method = "loess", formula = y ~ x, span = 0.7, 
            color = "brown", se = F) +
labs(title = "Regresión local: mpg ~ displacement") +
theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
geom_text(aes(label = "span = 0.2", x = 400, y = 40), size = 5, 
          colour = "orange") +
geom_text(aes(label = "span = 0.7", x = 400, y = 35), size = 5, 
          colour = "brown")

#evaluación del modelo de regresion local  con span = 0,7
pred.modelo.local <-  predict(modelo.local, datosA.test)
test.error.local <- mean((pred.modelo.local - datosA.test$mpg)^2)
test.error.local

cv.error <- rep(NA, 7)

for (i in 2:7) {
datosA.train$dis.cut <- cut(datosA.train$displacement, i )
modelo.step <- glm(mpg ~ dis.cut, data = datosA.train)
set.seed(4)
cv.error[i] <- cv.glm(datosA.train, modelo.step, K = 10)$delta[1]
}

cv.error

plot(2:7, cv.error[-1], xlab="Número de cortes", ylab="CV error", type="l", 
     pch=20, lwd=2)
points(which.min(cv.error), cv.error[which.min(cv.error)], col = "red", cex = 2, 
       pch = 20)







