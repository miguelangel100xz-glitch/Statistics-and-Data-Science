
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)

froze<-read_excel("C:\\Users\\elektra\\Downloads\\KiWu_Draft_05_STATA_labels_AVI 21.06.2021 Anonym 1.2  .xlsx")
#############################################
embryoglue<-table(froze$Embryoglue,froze$Gruppe_Luteal)
fisher_test(embryoglue, detailed = TRUE)
pairwise_fisher_test(embryoglue)

#############################################

hashat<-table(froze$Ass_Hatching,froze$Gruppe_Luteal)
fisher_test(hashat, detailed = TRUE)
pairwise_fisher_test(hashat)

#############################################

anz<-table(froze$Anz_Embryonen,froze$Gruppe_Luteal)
fisher_test(anz, detailed = TRUE)
pairwise_fisher_test(anz)

#########################################
biochem<-table(froze$Biochem_SS,froze$Gruppe_Luteal)
fisher_test(biochem, detailed = TRUE)
pairwise_fisher_test(biochem)
#########################################
HCG<-table(froze$HCG_gt_100,froze$Gruppe_Luteal)
fisher_test(HCG, detailed = TRUE)
pairwise_fisher_test(HCG)
###########################################3
ANZ<-table(froze$Anz_Fruchtblase_init,froze$Gruppe_Luteal)
fisher_test(ANZ, detailed = TRUE)
pairwise_fisher_test(ANZ)
#################################################
ANZ_2<-table(froze$Anz_Fruchtblase_SSW12,froze$Gruppe_Luteal)
fisher_test(ANZ_2, detailed = TRUE)
pairwise_fisher_test(ANZ_2)
#############################################33
################################################
fet_red<-table(froze$Fetale_Reduktion,froze$Gruppe_Luteal)
fisher_test(fet_red, detailed = TRUE)
pairwise_fisher_test(fet_red)
##########################################333
abort_lt12<-table(froze$Abort_lt_12,froze$Gruppe_Luteal)
fisher_test(abort_lt12, detailed = TRUE)
pairwise_fisher_test(abort_lt12)
################################################
EUGG<-table(froze$EUG,froze$Gruppe_Luteal)
fisher_test(EUGG, detailed = TRUE)
pairwise_fisher_test(EUGG)
############################333
blutung<-table(froze$Blutung_1_Trim,froze$Gruppe_Luteal)
fisher_test(blutung, detailed = TRUE)
pairwise_fisher_test(blutung)
#################################33
blutung2<-table(froze$Blutung_2_Trim,froze$Gruppe_Luteal)
fisher_test(blutung2, detailed = TRUE)
pairwise_fisher_test(blutung)
#########################################
blutung3<-table(froze$Blutung_3_Trim,froze$Gruppe_Luteal)
fisher_test(blutung, detailed = TRUE)
pairwise_fisher_test(blutung3)
#########################################3
gdmm<-table(froze$GDM,froze$Gruppe_Luteal)
fisher_test(gdmm, detailed = TRUE)
pairwise_fisher_test(gdmm)
##############################
preklam<-table(froze$Praeklampsie,froze$Gruppe_Luteal)
fisher_test(preklam, detailed = TRUE)
pairwise_fisher_test(preklam)
#####################################3
IUUU<-table(froze$IUWR,froze$Gruppe_Luteal)
fisher_test(IUUU, detailed = TRUE)
pairwise_fisher_test(blutung)
#######################################33
hosp2<-table(froze$Hospital_2_T,froze$Gruppe_Luteal)
fisher_test(hosp2, detailed = TRUE)
pairwise_fisher_test(hosp2)
#########################################
hosp3<-table(froze$Hospital_3_T,froze$Gruppe_Luteal)
fisher_test(hosp3, detailed = TRUE)
pairwise_fisher_test(hosp2)
############################################
kinder<-table(froze$Kinder_gesund,froze$Gruppe_Luteal)
fisher_test(kinder, detailed = TRUE)
pairwise_fisher_test(kinder)
#######################################333
kindertot<-table(froze$Kinder_tot,froze$Gruppe_Luteal)
fisher_test(kindertot, detailed = TRUE)
pairwise_fisher_test(hosp2)
#####################################3333
########################################






