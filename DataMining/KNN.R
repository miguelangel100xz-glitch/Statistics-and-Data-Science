#######################33
##
library(dplyr)

mba<-read.csv("C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\MINERIA\\ProyectoFinal\\Datasets\\Admission (1).csv")

str(mba)
mba<-mba%>%mutate(Decision=ifelse(Decision=="admit","Yes","No"))
mba<-mba%>%mutate(Decision=as.factor(Decision))


library(caret)
library(pROC)
library(mlbench)

set.seed(1234)
ind <- sample(2, nrow(mba), replace = T, prob = c(0.7, 0.3))
training <- mba[ind == 1,]
test <- mba[ind == 2,]
str(training)

trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

set.seed(222)
fit <- train(Decision ~ .,
             data = training,
             method = 'knn',
             tuneLength = 20,
             trControl = trControl,
             preProc = c("center", "scale"),
             metric = "ROC",
             tuneGrid = expand.grid(k = 1:60))
fit
plot(fit)
fit$bestTune
varImp(fit)


pred <- predict(fit, newdata = test)
confusionMatrix(pred, test$Decision)

bicycles<-read.csv("C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\MINERIA\\ProyectoFinal\\Datasets\\day.csv")
bicycles<-dplyr::select(bicycles,season:cnt)

set.seed(1234)
ind <- sample(2, nrow(bicycles), replace = T, prob = c(0.7, 0.3))
training <- bicycles[ind == 1,]
test <- bicycles[ind == 2,]

trControl <- trainControl(method = 'repeatedcv', 
                          number = 10,   
                          repeats = 3) 
set.seed(333)

fit <- train(cnt ~.,
             data = training,
             tuneGrid = expand.grid(k=1:70),
             method = 'knn',
             metric = 'Rsquared',
             trControl = trControl,
             preProc = c('center', 'scale'))
fit
plot(fit)

varImp(fit)

pred <- predict(fit, newdata = test)
RMSE(pred, test$cnt)

plot(pred ~ test$cnt)

#https://www.r-bloggers.com/2021/04/knn-algorithm-machine-learning/
#https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset



