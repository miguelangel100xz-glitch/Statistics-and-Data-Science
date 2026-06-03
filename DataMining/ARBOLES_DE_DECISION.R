
#Arboles de decisión 
#Las principales ventajas de este método son su interpretabilidad, pues nos da un conjunto de reglas a partir de las cuales se pueden tomar decisiones. Este es un algoritmo que no es demandante en poder de cómputo comparado con procedimientos más sofisticados y, a pesar de ello, que tiende a dar buenos resultados
#de predicción para muchos tipos de datos.
#Sus principales desventajas son que este en tipo de
#clasificación “débil”, pues sus resultados pueden variar 
#mucho dependiendo de la muestra de datos usados para entrenar 
#un modelo. Además es fácil sobre ajustar los modelos, esto es, 
#hacerlos excelentes para clasificar datos que conocemos, 
#pero deficientes para datos conocidos.
library(gmodels) # for CrossTable
library(ggplot2)
#install.packages("C50") # for DT algorithm
library(C50)
library(caret)
library(lattice)

#seleccionamos la base de datos y transformamos la variable de respuesta a factor 
arboles<-read.csv(header=TRUE,
                  "C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\MINERIA\\ProyectoFinal\\Datasets\\heart.csv")
arboles<-arboles%>%mutate(HeartDisease=as.factor(HeartDisease))

#Context
#Cardiovascular diseases (CVDs) are the number 1 cause of death globally, taking an estimated 17.9 million lives each year, which accounts for 31% of all deaths worldwide. Four out of 5CVD deaths are due to heart attacks and strokes, and one-third of these deaths occur prematurely in people under 70 years of age. Heart failure is a common event caused by CVDs and this dataset contains 11 features that can be used to predict a possible heart disease.
#People with cardiovascular disease or who are at high cardiovascular risk (due to the presence of one or more risk factors such as hypertension, diabetes, hyperlipidaemia or already established disease) need early detection and management wherein a machine learning model can be of great help.
#Attribute Information
#Age: age of the patient [years]
#Sex: sex of the patient [M: Male, F: Female]
#ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]
#RestingBP: resting blood pressure [mm Hg]
#Cholesterol: serum cholesterol [mm/dl]
#FastingBS: fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise]
#RestingECG: resting electrocardiogram results [Normal: Normal, ST: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV), LVH: showing probable or definite left ventricular hypertrophy by Estes' criteria]
#MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]
#ExerciseAngina: exercise-induced angina [Y: Yes, N: No]
#Oldpeak: oldpeak = ST [Numeric value measured in depression]
#ST_Slope: the slope of the peak exercise ST segment [Up: upsloping, Flat: flat, Down: downsloping]
#HeartDisease: output class [1: heart disease, 0: Normal]
#Source
#This dataset was created by combining different datasets already available independently but not combined before. In this dataset, 5 heart datasets are combined over 11 common features which makes it the largest heart disease dataset available so far for research purposes. The five datasets used for its curation are:

#Cleveland: 303 observations
#Hungarian: 294 observations
#Switzerland: 123 observations
#Long Beach VA: 200 observations
#Stalog (Heart) Data Set: 270 observations
#Total: 1190 observations
#Duplicated: 272 observations

#Final dataset: 918 observations

#Every dataset used can be found under the Index of heart disease datasets from UCI Machine Learning Repository on the following link: https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/
#link: https://www.kaggle.com/fedesoriano/heart-failure-prediction

#seleccionamos un tamaño de muestra de entrenamiento y de prueba
train_sample <- sample(918, 818) # get 900 randomly selected numbers, each between 0 and 1000
str(train_sample)

arboles_train <- arboles[train_sample, ]
arboles_test <- arboles[-train_sample,]

#analisamos los datos con el algortimo c50 
arboles_model <- C5.0(arboles_train[-12], arboles_train$HeartDisease)
arboles_model

summary(arboles_model)

arboles_predict <- predict(arboles_model, arboles_test)

CrossTable(arboles_test$HeartDisease, arboles_predict, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = TRUE, dnn = c('actual defualt','predicted default')) # prop.c is for proportionaliy calculation per column

credit_boost10 <- C5.0(arboles_train[-12], arboles_train$HeartDisease, trials = 10)
credit_boost10

credit_boost_pred10 <- predict(credit_boost10, arboles_test)
CrossTable(arboles_test$HeartDisease, credit_boost_pred10, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = TRUE, dnn = c('actual defualt','predicted default'))

#Asignando pesos de error a los arboles de clasificacion
#verificando que los falsos positivos son mas costosos que los positivos negativos
#generamos una matriz de costos para forzar el algoritmo a 
#analizar la base de datos evitando los falsos positivos esto reducira la calidad de precisison
#del modelo pero evitara los falsos positivos en el algoritmo de clasificacion.
#C5.0 puede crear un modelo de árbol inicial y luego descomponer la estructura
#del árbol en un conjunto de reglas mutuamente excluyentes.
#Estas reglas pueden luego podarse y modificarse en un conjunto 
#más pequeño de reglas potencialmente superpuestas. 
#Las reglas se pueden crear usando la opción de: rules
matrix_dimentions <- list(c("0", "1"), c("0", "1"))
names(matrix_dimentions) <- c("predicted", "actual")
matrix_dimentions

error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimentions) ## causes problem (trees won't grow), had to do it without dimnames!
error_cost2 <- matrix(c(0, 1, 4, 0), nrow = 2)

error_cost
error_cost2

arboles_cost <- C5.0(arboles_train[-12], arboles_train$HeartDisease, costs = error_cost2)

arboles_cost_pred <- predict(arboles_cost, arboles_test)
CrossTable(arboles_test$HeartDisease, arboles_cost_pred, prop.r = FALSE, prop.c = FALSE, prop.chisq = FALSE, dnn = c('actual default', 'predicted default'))

arboles_model_rules <- C5.0(arboles_train[-12], arboles_train$HeartDisease, rules = TRUE)
arboles_model_rules

summary(arboles_model_rules)

probs <- predict(credit_boost10, arboles_test, type = "prob")
# plot ROC curve
library(pROC)
library(dplyr)
ROC <- roc(predictor=probs[,1], 
           response=arboles_test$HeartDisease,
           levels=rev(levels(arboles_test$HeartDisease)))
plot(ROC)

ROC$auc

probs <- predict(credit_boost10, arboles_test, type = "prob")
# plot ROC curve
pred <- ROCR::prediction(probs[, 2], arboles_test$HeartDisease)
perf_dt_10 <- ROCR::performance(pred,  'tpr',  'fpr')
#plot(perf_dt_10) #complains about coersing s4 into numeric, so we did it manually
plot(perf_dt_10@x.values[[1]], perf_dt_10@y.values[[1]], xlab = perf_dt_10@x.name[[1]], ylab = perf_dt_10@y.name[[1]], type = "l")

ROCR::performance(pred, 'auc')

data.frame(predicted=probs, actual=arboles_test$HeartDisease) %>% ggplot(data=., aes(x=predicted.0)) +
  geom_density(aes(fill=arboles_test$HeartDisease), alpha=0.5) +
  xlab('Predicted probability of NO') +
  scale_fill_discrete(name="Actual label") +
  theme(legend.position=c(0.8,0.8))

arboles_predict <- predict(arboles_model, arboles_test)

probs_1 <- predict(arboles_model, arboles_test, type = "prob")
# plot ROC curve
pred_1 <- ROCR::prediction(probs_1[, 2], arboles_test$HeartDisease)
perf_dt_1 <- ROCR::performance(pred_1,  'tpr',  'fpr')
#plot(perf_dt_1)
plot(perf_dt_1@x.values[[1]], perf_dt_1@y.values[[1]],  xlab = perf_dt_10@x.name[[1]], ylab = perf_dt_10@y.name[[1]], type = "l" )

ROCR::performance(pred_1, 'auc')

arboles_cost_pred <- predict(arboles_cost, arboles_test)

roc_dt_1   <- data.frame(fpr = unlist(perf_dt_1@x.values), tpr = unlist(perf_dt_1@y.values))
roc_dt_1$method <- "DT 1"
roc_dt_10 <- data.frame(fpr = unlist(perf_dt_10@x.values), tpr = unlist(perf_dt_10@y.values))
roc_dt_10$method <- "DT 10"
rbind(roc_dt_1, roc_dt_10) %>%
  ggplot(data = ., aes(x = fpr, y = tpr, linetype = method, color = method)) + 
  geom_line() +
  geom_abline(a = 1, b = 0, linetype = 2) +
  scale_x_continuous(labels = scales::percent, lim = c(0,1)) +
  scale_y_continuous(labels = scales::percent, lim = c(0,1)) +
  theme(legend.position = c(0.8,0.2), legend.title = element_blank())




arboles_classifier3 <- train(arboles_train[-12], arboles_train$HeartDisease , method = "C5.0", verbose = FALSE)
# we can do our own grid 
#grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )
#credit_classifier3<- train(credit_train[-21], credit_train$default , method = "C5.0", verbose = FALSE, tuneGrid = grid)

arboles_classifier3

arboles_test_pred3 <- predict(arboles_classifier3, arboles_test)
CrossTable(arboles_test$HeartDisease, arboles_test_pred3, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = TRUE, dnn = c('actual defualt','predicted default')) # prop.c is for proportionaliy calculation per column

ctrl <- trainControl(method = "cv",   
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     allowParallel = FALSE)
arboles_train<-arboles_train%>%mutate(HeartDisease=if_else(HeartDisease==1,"YES","No"))

m_cv_ROC <- train(arboles_train[-12], arboles_train$HeartDisease,
                  method = "C5.0",
                  metric = "ROC",
                  trControl = ctrl)

arboles_test<-arboles_test%>%mutate(HeartDisease=ifelse(HeartDisease==1,"YES","No"))
probs_cv_ROC <- predict(m_cv_ROC, arboles_test, type = "prob")
# plot ROC curve
pred_cv_ROC <- ROCR::prediction(probs_cv_ROC[, 2], arboles_test$HeartDisease)
perf_dt_cv_ROC <- ROCR::performance(pred_cv_ROC,  'tpr',  'fpr')
#plot(perf_dt_1)
plot(perf_dt_cv_ROC@x.values[[1]], perf_dt_cv_ROC@y.values[[1]],  xlab = perf_dt_cv_ROC@x.name[[1]], ylab = perf_dt_cv_ROC@y.name[[1]], type = "l" )

# plot ROC for each method
roc_dt_1   <- data.frame(fpr = unlist(perf_dt_1@x.values), tpr = unlist(perf_dt_1@y.values))
roc_dt_1$method <- "DT 1"
roc_dt_10 <- data.frame(fpr = unlist(perf_dt_10@x.values), tpr = unlist(perf_dt_10@y.values))
roc_dt_10$method <- "DT 10"
roc_dt_cv_ROC <- data.frame(fpr = unlist(perf_dt_cv_ROC@x.values), tpr = unlist(perf_dt_cv_ROC@y.values))
roc_dt_cv_ROC$method <- "DT CV ROC"

rbind(roc_dt_1, roc_dt_10, roc_dt_cv_ROC) %>%
  ggplot(data = ., aes(x = fpr, y = tpr, linetype = method, color = method)) + 
  geom_line() +
  geom_abline(a = 1, b = 0, linetype = 2) +
  scale_x_continuous(labels = scales::percent, lim = c(0,1)) +
  scale_y_continuous(labels = scales::percent, lim = c(0,1)) +
  theme(legend.position = c(0.8,0.2), legend.title = element_blank())

library("ipred")
set.seed(300)
arboles<-arboles%>%mutate(HeartDisease=(ifelse(HeartDisease==1,"YES","No")))
arboles<-arboles%>%mutate(HeartDisease=as.factor(HeartDisease))
mybag <- bagging(HeartDisease ~ . , data = arboles, nbagg = 25)
arboles_pred <- predict(mybag, arboles)
table(arboles_pred, arboles$HeartDisease)

library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number  = 10)
train(HeartDisease ~ ., data = arboles, method = "treebag", trControl = ctrl)

#Fuentes 
#https://rpubs.com/jboscomendoza/arboles_decision_clasificacion#:~:text=Los%20%C3%A1rboles%20de%20decisi%C3%B3n%20son,disciplinas%20como%20modelo%20de%20predicci%C3%B3n.&text=Tenemos%20una%20variable%20objetivo%20(dependiente,variable%20objetivo%20para%20casos%20desconocidos.
#https://github.com/jboscomendoza/rpubs/tree/master/arboles/
#https://github.com/mzakariaCERN/FunWithR/blob/master/3_DT/DT_BankLoans.md




