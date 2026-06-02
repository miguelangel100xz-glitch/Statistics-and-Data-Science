

#########################3
library(readxl)
library(readr)
library(ggplot2)
library(tidyverse)
#PROYECTO DE PCA estadistica multivariada 

hemoglobina <- read.csv(head=TRUE,"C:\\Users\\W10\\Desktop\\ESTADISTICA MULTIVARIADA\\ACTIVIDADES\\PCA\\PCA_1_HEMOGLOBI.csv")

ggplot(hemoglobina,aes(IMC))+
  geom_histogram(fill="blue",colour="black",alpha=0.2)#+
  scale_y_continuous(labels=scales::percent)
  
  hemoglobina%>% 
  filter(ICC< 10)%>%
  ggplot(aes(ICC))+
    geom_histogram(fill="green",colour="black",alpha=0.2)#+
  scale_y_continuous(labels=scales::percent)
  
  
  ggplot(hemoglobina,aes(HEM))+
    geom_histogram(fill="orange",colour="black",alpha=0.2)#+
  scale_y_continuous(labels=scales::percent)
  
  
  ggplot(hemoglobina,aes(GLUC))+
    geom_histogram(fill="purple",colour="black",alpha=0.2)#+
  scale_y_continuous(labels=scales::percent)
  
  
  ggplot(hemoglobina,aes(COL))+
    geom_histogram(fill="red",colour="black",alpha=0.2)#+
  scale_y_continuous(labels=scales::percent)
  
  ggplot(hemoglobina,aes(TRIG))+
    geom_histogram(fill="pink",colour="black",alpha=0.2)#+
  scale_y_continuous(labels=scales::percent)
  
  
  select(hemoglobina,IMC:TRIG)%>% 
    pivot_longer(IMC:TRIG)%>%
    #filter(name %in% c("COL","GLUC","TRIG"))%>% 
    ggplot(aes(name,value,fill=name))+
    geom_boxplot(alpha=1/4,outlier.shape = 16)+
    guides(fill=FALSE)+
   # facet_wrap(~name, scales = "free")
    scale_x_discrete(limits=c("ICC","HEM","IMC","GLUC","COL","TRIG"))+
    labs(
      x="",y="",title="TITULO"
    )

   # select(hemoglobina,IMC:TRIG)%>% 
  #boxplot(.)
    
   d<- select(hemoglobina,IMC:TRIG)
 c<- cov(d)
  
  MR <- cor(d)
  MR
  
  S<-cor(d, use="complete.obs")
  corrplot.mixed(S)
  
  det(MR)
  
  library(psych)
  # Prueba de Kaiser-Meyer-Olkin (KMO)
  KMO(MR)
  
  print(cortest.bartlett(MR, nrow(d)))
  
  acp <- prcomp(d, scale=TRUE)
  acp
  
  summary(acp)
  
  
  
biologicos<- read_excel("C:\\Users\\W10\\Desktop\\ESTADISTICA MULTIVARIADA\\ACTIVIDADES\\PCA\\PCA_2__PARAMETROS_BIOL.xlsx")

tortugas <- read_excel("C:\\Users\\W10\\Desktop\\ESTADISTICA MULTIVARIADA\\ACTIVIDADES\\PCA\\PCA_3__TORTUGAS.xlsx")

