data<-read.delim("clipboard",stringsAsFactors = TRUE)

head(data)

#------
# Format variables
#------
str(data)

data$varieties = as.factor(data$varieties)
data$fertilizer = as.factor(data$fertilizer)
str(data)

#------
# Fit analysis of variance model for strip plot
#------

block<- data$block
varieties<-data$varieties
fertilizer<- data$fertilizer
yield<- data$yield
library(agricolae)

model = strip.plot(block,
                   varieties,
                   fertilizer,
                   yield)
#------
# Apply mean comparison test
#------

gla<-model$gl.a
glb<-model$gl.b
glc<-model$gl.c

Ea<-model$Ea
Eb<-model$Eb
Ec<-model$Ec

#------
# LSD mean comparison test
#------
# Main effects

library(agricolae)

# Main effects
library(agricolae)
# First factor variable (varieties)
LSD_A = LSD.test(y = yield,
                 trt = varieties,
                 DFerror = model$gl.a,
                 MSerror = model$Ea,
                 alpha = 0.05,
                 p.adj = "bonferroni",
                 group = TRUE,
                 console = TRUE)

# Second factor variable (fertilizer)
LSD_B = LSD.test(y = yield,
                 trt = fertilizer,
                 DFerror = model$gl.b,
                 MSerror = model$Eb,
                 alpha = 0.05,
                 p.adj = "bonferroni",
                 group = TRUE,
                 console = TRUE,main = TRUE)

# Interaction (varieties:fertilizer)
LSD_AB = LSD.test(y = yield,
                  trt = varieties:fertilizer,
                  DFerror = model$gl.c,
                  MSerror = model$Ec,
                  alpha = 0.05,
                  p.adj = "bonferroni",
                  group = TRUE,
                  console = TRUE)
###################analisis grafico

library(ggplot2)
# Variedades 
p1 = ggplot(MeanSE_A, aes(x = varieties,
                          y = avg_A))
plotA = p1 + 
  geom_bar(stat = "identity",
           color = "black",
           position = position_dodge(width=0.9))
plotB = plotA +
  geom_errorbar(aes(ymax = avg_A + se,
                    ymin = avg_A - se), 
                position = position_dodge(width=0.9), 
                width = 0.25)
plotC = plotB + 
  labs(title = "",
       x = "varieties",
       y = "yield")

#NIVELES DE FERTILIZANTE

p2 = ggplot(MeanSE_B, aes(x = fertilizer,
                          y = avg_B))
plotA = p2 + 
  geom_bar(stat = "identity",
           color = "black",
           position = position_dodge(width=0.9))
plotB = plotA +
  geom_errorbar(aes(ymax = avg_B + se,
                    ymin = avg_B - se), 
                position = position_dodge(width=0.9), 
                width = 0.25)

plotC = plotB + 
  labs(title = "",
       x = "fertilizer",
       y = "yield")


#interaccion nivel de fertilizante y tipo de fertilizante

p3 = ggplot(MeanSE_AB, aes(x = fertilizer,
                           y = avg_AB,
                           fill = factor(varieties)))
plotA = p3 + 
  geom_bar(stat = "identity",
           color = "black",
           position = position_dodge(width=0.9))
plotB = plotA + 
  scale_fill_manual(values = gray(1:3/3),
                    labels = c("Super", "Shaheen", ("Basmati-15")))
plotC = plotB + 
  geom_errorbar(aes(ymax = avg_AB + se,
                    ymin = avg_AB - se), 
                position = position_dodge(width=0.9), 
                width = 0.25)
plotD = plotC + 
  labs(title = "",
       x = "fertilizer",
       y = "yield",
       fill = "varieties")






