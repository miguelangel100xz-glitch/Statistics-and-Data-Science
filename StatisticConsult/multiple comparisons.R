

library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(MASS)
library(rstatix)
froze<-read_excel("C:\\Users\\elektra\\Downloads\\KiWu_Draft_05_STATA_labels_AVI 21.06.2021 Anonym 1.2  .xlsx")
#############################################

froze<-froze%>%mutate(Gruppe_Luteal=case_when(Gruppe_Luteal=="Crinone"~"Vaginal progesterone gel",
                                              Gruppe_Luteal=="Crinone + Duphaston"~"Oral dydrogesterone + vaginal progesterone gel",
                                              Gruppe_Luteal=="Duphaston"~"Oral dydrogesterone",
                                              Gruppe_Luteal=="Utrogestan"~"Vaginal progesterone capsules",
                                              Gruppe_Luteal=="Prolutex"~"Subcutaneous injection"))



print("Clinical Pregnancy")
ANZ<-table(froze$Anz_Fruchtblase_init,froze$Gruppe_Luteal)
c<-fisher_test(ANZ, detailed = TRUE)

d<-pairwise_fisher_test(ANZ,p.adjust.method = "holm")
library(openxlsx)
write.xlsx(c,"Clinical_pregnancytest_exact_fisher.xlsx")

print("Ongoing Pregnancy")
ANZ_2<-table(froze$Anz_Fruchtblase_SSW12,froze$Gruppe_Luteal)
a<-fisher_test(ANZ_2, detailed = TRUE)
b<-pairwise_fisher_test(ANZ_2,p.adjust.method = "holm")

write.xlsx(e,"Livebirts_test_exact_fisher.xlsx")

print("Live Births")
kinder<-table(froze$Kinder_gesund,froze$Gruppe_Luteal)
e<-fisher_test(kinder, detailed = TRUE)
f<-pairwise_fisher_test(kinder)

froze%>%group_by(Gruppe_Luteal,Endometrium_gt_7,Abbruch,Embryoglue,Ass_Hatching,
                 Biochem_SS,HCG_gt_100,Anz_Fruchtblase_init,
                 Anz_Fruchtblase_SSW12,Fetale_Reduktion,Abort_lt_12,Spaetabort,
                 EUG,Heterotope_EUG,Abort_Induktion,
                 Blutung_1_Trim,Blutung_2_Trim,
                 Blutung_3_Trim,GDM,
                 Vorztg_Wehen_Cerklage,Vorztg_Wehen_2_T,
                 Vorztg_Wehen_3_T,Plazenta_Praevia,
                 Isolierte_Hypertonie,Praeklampsie,
                 Eklampsie,Psychol_Probleme,IUWR,Hospital_1_T,
                 Hospital_2_T,Hospital_3_T,Geburt,Cholestase)%>%count()

count%>%pivot_longer(cols = c(`Endometrium_gt_7`,`Abbruch`,
  `Embryoglue`, `Ass_Hatching`,`Biochem_SS`,`HCG_gt_100`,
  `Anz_Fruchtblase_init`,`Anz_Fruchtblase_SSW12`,
  `Fetale_Reduktion`,`Abort_lt_12`,
  `Spaetabort`,`EUG`,`Heterotope_EUG`,
  `Abort_Induktion`,`Blutung_1_Trim`,
  `Blutung_2_Trim`,`Blutung_3_Trim`,
  `GDM`,`Vorztg_Wehen_Cerklage`,
  `Vorztg_Wehen_2_T`,`Vorztg_Wehen_3_T`,
  `Plazenta_Praevia`,`Isolierte_Hypertonie`,
  `Praeklampsie`,`Eklampsie`,
  `Psychol_Probleme`,`IUWR`,
  `Hospital_1_T`,`Hospital_2_T`,
  `Hospital_3_T`,`Geburt`,
  `Cholestase`),names_to = "jjjle",values_to = "casos")











