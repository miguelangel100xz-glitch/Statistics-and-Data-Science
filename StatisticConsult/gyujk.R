
library(readxl)
library(ggplot2)
library(ggstatsplot)

froze<-read_excel(file.choose())

ggbarstats(
  data = froze,
  x = Gruppe_Luteal,
  y = Embryoglue
) +
  labs(caption = NULL) # remove caption






