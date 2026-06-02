require(ggplot2)
require(GGally)
require(CCA)


mm <- read.csv("https://stats.idre.ucla.edu/stat/data/mmreg.csv")
colnames(mm) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", 
                  "Science", "Sex")
summary(mm)

colnames(mm) <- c("Locus", "Autoconcepto", "Motivacion", "Lectura", "Escritura", "Matematicas", 
                  "Ciencias", "Sexo")
summary(mm)

#write.csv(mm,file="BASE_CANONICA_PROYECTO.CSV")
mm%>% pivot_longer(Locus:Sexo) %>%
  ggplot(aes(name,value,fill=name))+
  geom_boxplot(alpha=2/3,outlier.shape = 16)+
  guides(fill=FALSE)+theme_bw()+
  guides(colour=FALSE,fill=FALSE)+
  coord_flip()+
  labs(title="Variables psicologicas y academicas",
       caption="Fuente:Elaboración propia.",
       x="Puntuación",y="Variables")

ggduo(mm,columnsX = 1:3,columnsY = 4:8,
      types = list(continuous = "smooth_lm"),
      title = "Correlación entre variables Psicologicas y  Academicas",
      xlab = "Variables Psicológicas",
      ylab = "Academicas"
)



psych <- mm[, 1:3]
acad <- mm[, 4:8]
library(GGally)
ggpairs(psych)
ggpairs(acad)
# correlations
library(CCA)
M<-matcor(psych, acad)
cc1 <- cc(psych, acad)

cc1$cor
cc1[3:4]
cc2 <- comput(psych, acad, cc1)
cc2[3:6]


#M<-matcor(x,y)  
#M 

#CC <- cc (x, y) 
#CC
# Variables canónicas
#plot(cc1)
#gala$cor
img.matcor(M, type = 2) 
barplot(cc1$cor,ylim = c(0,1)) 
plt.cc(cc1,var.label=T)
#cc2 <- cca(psych, acad)
#plot(cc2,var.label=F)

