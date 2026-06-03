
# load tidyverse and tidymodels packages
library(tidyverse)
library(broom)
library(yardstick)
library(dplyr)
# load cowplot to change plot theme
library(cowplot)

# get `biopsy` dataset from `MASS`
heart<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\MINERIA\\ProyectoFinal\\Datasets\\heart.csv")

# change column names from `V1`, `V2`, etc. to informative variable names
heart<-dplyr::select(heart,Age,RestingBP,Cholesterol,MaxHR,
                     HeartDisease)
heart<-heart%>%mutate(HeartDisease=as.factor(HeartDisease))
# fit a logistic regression model to predict tumor types
glm_out1 <- glm(
  formula = HeartDisease ~ .,
  family = binomial,
  data = heart
) %>%
  augment() %>%
  mutate(model = "m1") # name the model

# fit a different logistic regression model to predict tumor types
glm_out2 <- glm(HeartDisease ~ Age+Cholesterol+MaxHR,
                family = binomial,
                data = heart
) %>%
  augment() %>%
  mutate(model = "m2") # name the model

# combine the two datasets to make an ROC curve for each model
glm_out <- bind_rows(glm_out1, glm_out2)
# plot ROC curves

glm_out %>%
  group_by(model) %>% # group to get individual ROC curve for each model
  roc_curve(event_level = 'second', truth = HeartDisease, .fitted) %>% # get values to plot an ROC curve
  ggplot(
    aes(
      x = 1 - specificity, 
      y = sensitivity, 
      color = model
    )
  ) + # plot with 2 ROC curves for each model
  geom_line(size = 1.1) +
  geom_abline(slope = 1, intercept = 0, size = 0.4) +
  scale_color_manual(values = c("#48466D", "#3D84A8")) +
  coord_fixed() +
  theme_cowplot()
# calculate AUCs
glm_out %>%
  group_by(model) %>% # group to get individual AUC value for each model
  roc_auc(event_level = 'second', truth = HeartDisease, .fitted)

###########################################################33
#############################################################
#Curvas roc para random forest 
#https://www.kaggle.com/fedesoriano/hepatitis-c-dataset
#Context
#The data set contains laboratory values of blood donors and Hepatitis C patients and demographic values like age. The data was obtained from UCI Machine Learning Repository: https://archive.ics.uci.edu/ml/datasets/HCV+data
#Content
#All attributes except Category and Sex are numerical.
#Attributes 1 to 4 refer to the data of the patient:
 # 1) X (Patient ID/No.)
#2) Category (diagnosis) (values: '0=Blood Donor', '0s=suspect Blood Donor', '1=Hepatitis', '2=Fibrosis', '3=Cirrhosis')
#3) Age (in years)
#4) Sex (f,m)
#Attributes 5 to 14 refer to laboratory data:
#  5) ALB
#6) ALP
#7) ALT
#8) AST
#9) BIL
#10) CHE
#11) CHOL
#12) CREA
#13) GGT
#14) PROT
#The target attribute for classification is Category (2): blood donors vs. Hepatitis C patients (including its progress ('just' Hepatitis C, Fibrosis, Cirrhosis).
                                                                                         Acknowledgements

#                                                                                                Creators: Ralf Lichtinghagen, Frank Klawonn, Georg Hoffmann
 #                                                                                               Donor: Ralf Lichtinghagen: Institute of Clinical Chemistry; Medical University Hannover (MHH); Hannover, Germany; lichtinghagen.ralf '@' mh-hannover.de
  #                                                                                              Donor: Frank Klawonn; Helmholtz Centre for Infection Research; Braunschweig, Germany; frank.klawonn '@' helmholtz-hzi.de
 #                                                                                               Donor: Georg Hoffmann; Trillium GmbH; Grafrath, Germany; georg.hoffmann '@' trillium.de
                                                                                                
  #                                                                                              Relevant Papers
   #                                                                                             Lichtinghagen R et al. J Hepatol 2013; 59: 236-42
    #                                                                                            Hoffmann G et al. Using machine learning techniques to generate laboratory diagnostic pathways â???" a case study. J Lab Precis Med 2018; 3: 58-67
                                                                                                
     #                                                                                           Other Datasets
      #                                                                                          Stroke Prediction Dataset: LINK

library(tidymodels)
library(ggthemes)
library(ranger)
library(randomForest)
library(dplyr)

HC<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\MINERIA\\ProyectoFinal\\Datasets\\HepatitisCdata.csv")
HC<-na.omit(HC)
HC<-dplyr::select(HC,-Sex,-X)
#HC<-HC%>%mutate(Category=case_when(Category=="0=Blood Donor"~"Blood_Donor",
 #                                  Category=="0s=suspect Blood Donor"~"Suspect_Blood_Donor",
  #                                 Category=="1=Hepatitis"~"Hepatitis",
   #                                Category=="2=Fibrosis"~"Fibrosis",
    #                               Category=="3=Cirrhosis"~"Cirrosis"))
HC<-HC%>%mutate(Category=case_when(Category=="0=Blood Donor"~"1",
                                   Category=="0s=suspect Blood Donor"~"2",
                                   Category=="1=Hepatitis"~"3",
                                   Category=="2=Fibrosis"~"4",
                                   Category=="3=Cirrhosis"~"5"))
HC<-HC%>%mutate(Category=as.factor(Category))
#HC<-HC%>%filter(Category!="Fibrosis")
graph<-HC%>%pivot_longer(Age:PROT)
#View(graph)
ggplot(graph,aes(value,fill=Category))+geom_density()+facet_wrap(~name,scales = "free")
ggplot(graph,aes(value,fill=Category))+geom_density(alpha=.5)+facet_wrap(~name,scales="free")

HC_split <- initial_split(HC, prop = 0.7)
HC_split

HC_split %>%
  training()  

HC_recipe <- training(HC_split) %>%
  recipe(Category ~.) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

HC_recipe

HC_training <- juice(HC_recipe)
HC_training

HC_testing <- HC_recipe %>%
  bake(testing(HC_split)) 

HC_testing

HC_ranger <- rand_forest(trees = 100, mode = "classification") %>%
  set_engine("ranger") %>%
  fit(Category ~ ., data = HC_training)

HC_ranger

HC_rf <-  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest") %>%
  fit(Category ~ ., data = HC)

HC_rf

HC_noengine <- rand_forest(trees = 100, mode = "classification") %>%
  fit(Category ~ ., data = HC_training)

predict(HC_ranger,HC_testing)

HC_ranger %>%
  predict(HC_testing) %>%
  bind_cols(HC_testing)  

HC_ranger %>%
  predict(HC_testing) %>%
  bind_cols(HC_testing) %>%
  metrics(truth = Category, estimate = .pred_class)

HC_rf %>%
  predict(HC_testing) %>%
  bind_cols(HC_testing) %>%
  metrics(truth = Category, estimate = .pred_class)

HC_ranger %>%
  predict(HC_testing, type = "prob")  

HC_probs <- HC_ranger %>%
  predict(HC_testing, type = "prob") %>%
  bind_cols(HC_testing)

HC_probs

#HC_probs<-dplyr::select(HC_probs,-.pred_Fibrosis)
#HC_probs<-HC_probs%>%filter(Category!="Fibrosis")
HC_probs%>%
  roc_curve(Category, .pred_1:.pred_5)

HC_probs%>%
  roc_curve(Category, .pred_1:.pred_5) %>%
  autoplot()+
  ggthemes::theme_fivethirtyeight()+
  labs(title = 'Hepatitis C' , subtitle="1-Blood Donor\n2-Supect Blood Donor\n3-Hepatitis \n4-Fibrosis \n5-Cirrosis" )

#Category=="0=Blood Donor"~"1",
#Category=="0s=suspect Blood Donor"~"2",
#Category=="1=Hepatitis"~"3",
#Category=="2=Fibrosis"~"4",
#Category=="3=Cirrhosis"~"5"



#predict(HC_ranger, HC_testing, type = "prob") %>%
 # bind_cols(predict(HC_ranger, HC_testing)) %>%
  #bind_cols(select(HC_testing, Category))%>%
  #glimpse()


#predict(vino_ranger, vino_testing, type = "prob") %>%
 # bind_cols(predict(vino_ranger, vino_testing)) %>%
  #bind_cols(select(vino_testing, Category)) %>%
  #metrics(truth=quality, .pred_3:.pred_9, estimate = .pred_class)


#https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/






