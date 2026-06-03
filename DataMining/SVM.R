


#machine vector support 

machinesv<-read.csv("C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\MINERIA\\ProyectoFinal\\Datasets\\heart.csv")
machinesv<-machinesv%>%filter(Cholesterol>0)%>%
  mutate(HeartDisease=as.factor(HeartDisease))
machinesv%>%
  ggplot(aes(Cholesterol,RestingBP,color=HeartDisease))+
  geom_point()

library(tidyverse)    # data manipulation and visualization
library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology
library(ISLR)         # contains example data set "Khan"
library(RColorBrewer)
machinesv1<-dplyr::select(machinesv,Cholesterol,RestingBP,HeartDisease)
# set pseudorandom number generator
set.seed(123)
# sample training data and fit model
train <- base::sample(746,373, replace = FALSE)
svmfit <- svm(HeartDisease~Cholesterol+RestingBP, data = machinesv1[train,], kernel = "radial", gamma = 1, cost = 1)
# plot classifier

plot(svmfit, machinesv1)

tune.out <- tune(svm, HeartDisease~Cholesterol+RestingBP, data = machinesv1[train,], kernel = "radial",
                 ranges = list(cost = c(0.1,1,10,100,1000),
                               gamma = c(0.5,1,2,3,4)))
# show best model
tune.out$best.model


(valid <- table(true = machinesv1[-train,"HeartDisease"], pred = predict(tune.out$best.model,
                                                       newx = machinesv1[-train,])))


#######################55% de precision :c
#http://www.key2stats.com/data-set/view/1626

prueba2<-read.csv("C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\MINERIA\\ProyectoFinal\\Datasets\\Speed__gender__and_height_of_1325_students_1626 (1).csv")
prueba2<-prueba2%>%mutate(gender=ifelse(gender=="female","1","0"))
ggplot(prueba2%>%filter(height<4000&speed>0),aes(speed,height,shape=gender,
                   color=gender))+
  geom_point()

prueba2<-prueba2%>%filter(height<4000&speed>0)
prueba2<-prueba2%>%#mutate(site=ifelse(site<4,"1","2"))%>%
  mutate(gender=as.factor(gender))


machinesv1<-dplyr::select(prueba2,speed,gender,height)
#
train <- base::sample(1278,639, replace = FALSE)
svmfit <- svm(gender~speed+height, data = machinesv1[train,], kernel = "radial", gamma = 1, cost = 1)
# plo

plot(svmfit, machinesv1)

tune.out <- tune(svm, gender~speed+height, data = machinesv1[train,], kernel = "linear",
                 ranges = list(cost = c(0.1,1,10,100,1000),
                               gamma = c(0.5,1,2,3,4)))

ggplot(data = tune.out$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de clasificación vs hiperparámetro C") +
  theme_bw()

# show best model
tune.out$best.model


(valid <- table(true = machinesv1[-train,"gender"], pred = predict(tune.out$best.model,
                                                                         newx = machinesv1[-train,])))
modelo_svm <- tune.out$best.model

# Aciertos del modelo con los datos de entrenamiento
paste("Error de entrenamiento:", 100*mean(prueba2$gender != modelo_svm$fitted), "%")

#https://rpubs.com/Joaquin_AR/267926
#https://www.cienciadedatos.net/documentos/34_maquinas_de_vector_soporte_support_vector_machines
#https://rpubs.com/Joaquin_AR/267926




