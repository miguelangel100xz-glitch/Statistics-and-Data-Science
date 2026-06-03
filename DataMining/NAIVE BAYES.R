 ###############################################3
#NAYVE BAYES 
#Funte de los datos :https://www.kaggle.com/zhaoyingzhu/heartcsv
heart<-read.csv("C:\\Users\\elektra\\Downloads\\heart.csv")
heart<-na.omit(heart)
heart<-heart%>%mutate(HeartDisease=as.character(HeartDisease))
x<-dplyr::select(heart,Age,RestingBP,Cholesterol,MaxHR)

set.seed(123)

training.samples <- heart$HeartDisease %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- heart[training.samples, ]
test.data <- heart[-training.samples, ]

library(naivebayes)
library("klaR")
# Fit the model
heart<-dplyr::select(heart,HeartDisease,Age,RestingBP,Cholesterol,MaxHR)
heart<-heart%>%mutate(HeartDisease=as.factor(HeartDisease))
model <- NaiveBayes(HeartDisease ~Age+RestingBP+Cholesterol+MaxHR, data = heart)
model
# Make predictions
predictions <- model %>% predict(test.data)
# Model accuracy
mean(predictions$class == test.data$HeartDisease)
library(klaR)
# Build the model
set.seed(123)
model <- train(HeartDisease ~Age+RestingBP+Cholesterol+MaxHR, data = train.data, method = "nb", 
               trControl = trainControl("cv", number = 10))
model
# Make predictions
predicted.classes <- model %>% predict(test.data)
# Model n accuracy
mean(predicted.classes == test.data$HeartDisease)
#Nuestro modelo presenta un 70% de precision[(redondeado :)]
#Verifacion de graficos de modelo 
#se presenta dos formas de grafico la forma gaussiana que es la mas comun
#y la forma no parametrica 
#cerificamos normalidad en nuestros datos 
shapiro.test(heart$Age)
shapiro.test(heart$RestingBP)
shapiro.test(heart$Cholesterol)
shapiro.test(heart$MaxHR)

###############################################3
#CALCULANDO PROBABILIDADES CONDICIONALES 
library(e1071)
model <- naiveBayes(HeartDisease ~ ., data = heart)
class(model)
summary(model)
print(model)

tbl_list <- sapply(heart[-1], table, heart[ , 1])
tbl_list <- lapply(tbl_list, t)
cond_probs <- sapply(tbl_list, function(x) { 
  apply(x, 1, function(x) { 
    x / sum(x) }) })
cond_probs <- lapply(cond_probs, t)
print(cond_probs)

preds <- predict(model, newdata = heart)
conf_matrix <- table(preds, heart$HeartDisease)
conf_matrix
#precision del 71% <3

#No se cumple normalidad ára ninguna de las variables dentro del modelo 
#v: si es el caso de que sus variables presentarana normalidad 
#se presenta de la siguiente forma. 
x<-dplyr::select(heart,Age,RestingBP,Cholesterol,MaxHR)

y <- heart[[1]]
M <- as.matrix(x)
### nayvebayes
gnb <- nonparametric_naive_bayes(x = M, y = y)
#Visualizando el ajuste no parametrico de naive bayes 
plot(gnb, which = 1)
# Visualizandoi cada variable analizada de la probabilidad condicional 
plot(gnb, which = 1, prob = "conditional")
plot(gnb, which = 2, prob = "conditional")
plot(gnb, which = 3, prob = "conditional")
plot(gnb, which = 4, prob = "conditional")

#Verificando las secciones de datos categoricos para clasificacion con nayve bayes 
heart<-read.csv("C:\\Users\\elektra\\Downloads\\heart.csv")
heart<-na.omit(heart)
heart<-heart%>%mutate(HeartDisease=as.factor(HeartDisease))

nb.model <- naive_bayes(HeartDisease ~ ST_Slope + ExerciseAngina + RestingECG + 
                          ChestPainType+Sex,
                        data = heart, laplace = 1)
nb.model
plot(nb.model)

#generand predicciones para los nuevos datos 
#categoricos <3 generando el data frame<3
new.data <- data.frame(Sex = "M",
                       ChestPainType = "ASY",
                       RestingECG = "LVH",
                       ExerciseAngina = "Y",
                       ST_Slope="Up"
                       )
predict(nb.model, new.data)
pred <- t(predict(nb.model, new.data, type = "prob"))
dotchart(pred[order(pred[,1], decreasing = T),][1:2])
#De acuerdo al analisis de naive bayes es mas posible que el individuo con
#caracteristicas :
#Sex = "M",
#ChestPainType = "ASY",
#RestingECG = "LVH",
#ExerciseAngina = "Y",
#ST_Slope="Up"
#Presente una enfermedad del corazon <3 probabilidad del
#90% tomando en cuenta el algoritmo nayve bayes


#FUENTES
#https://www.r-bloggers.com/2018/01/understanding-naive-bayes-classifier-using-r/
#https://rdrr.io/cran/naivebayes/man/nonparametric_naive_bayes.html
#https://www.r-bloggers.com/2017/02/naive-bayes-classification-in-r-part-2/
#https://www.r-bloggers.com/2020/03/predicting-the-video-game-hype-train-playing-around-with-naive-bayesian-learning/
#https://www.r-bloggers.com/2017/02/naive-bayes-classification-in-r-part-1/










