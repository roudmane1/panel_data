library("lme4")
library("leaps")
library("tidyverse")
setwd("C:/Users/roudmane/Desktop/cours/M1/S2/analyse_données_panels")
##Explortation des donnÃ©es 
name<-"data/Table_Intersection_BV_IRIS_2012_Pres_All3.csv"
data<-read.csv2(name,header=TRUE,sep = ";")
n=nrow(data)
View(data)
colnames(data)
data_modifier<-data [ ,!colnames(data)%in%c("P12_H1564","P12_H1524","P12_H2554","P12_H5564","P12_HACT1564","P12_HACT1524","P12_HACT2554","P12_HACT5564","P12_HACTOCC1564","P12_HACTOCC1524","P12_HACTOCC2554","P12_HACTOCC5564","P12_POPH"
                                          ,"P12_H0014","P12_H1529","P12_H3044","P12_H4559","P12_H6074","P12_H75P","P12_H0019","P12_H2064","P12_H65P","P12_HNSCOL15P"," P12_HNSCOL15P_DIPL0","
                                          P12_HNSCOL15P_CEP","P12_HNSCOL15P_BEPC","P12_HNSCOL15P_CAPBEP","P12_HNSCOL15P_BAC","P12_HNSCOL15P_BACP2","
                                          P12_HNSCOL15P_SUP","P12_HNSAL15P","P12_FNSAL15P","P12_ACTOCC15P_TP","P12_SAL15P_TP"," P12_HSAL15P_TP","P12_FSAL15P_TP","P12_NSAL15P_TP","P12_SAL15P_CDI","
                                          P12_SAL15P_CDD","P12_SAL15P_INTERIM","P12_SAL15P_EMPAID","P12_SAL15P_APPR","P12_NSAL15P_INDEP","
                                          P12_NSAL15P_EMPLOY","P12_NSAL15P_AIDFAM","P12_ACTOCC15P_ILT1","P12_ACTOCC15P_ILT2P","P12_ACTOCC15P_ILT2","
                                          P12_ACTOCC15P_ILT3","P12_ACTOCC15P_ILT4","P12_ACTOCC15P_ILT5","C12_ACTOCC15P"," C12_ACTOCC15P_PAS"," C12_ACTOCC15P_MAR","
                                          C12_ACTOCC15P_DROU"," C12_ACTOCC15P_VOIT","C12_ACTOCC15P_TCOM","C12_MEN","C12_MENPSEUL","C12_MENHSEUL"," C12_MENFSEUL","C12_MENSFAM","C12_MENFAM"," C12_MENCOUPSENF","
                                          C12_MENCOUPAENF","
                                          C12_MENFAMMONO","
                                          C12_PMEN","
                                          C12_PMEN_MENPSEUL","
                                          C12_PMEN_MENHSEUL","
                                          C12_PMEN_MENFSEUL","
                                          C12_PMEN_MENSFAM","
                                          C12_PMEN_MENFAM","
                                          C12_PMEN_MENCOUPSENF","
                                          C12_PMEN_MENCOUPAENF","
                                          C12_PMEN_MENFAMMONO","
                                          P12_POP15P","
                                          P12_POP1524","
                                          P12_POP2554","
                                          P12_POP5579","
                                          P12_POP80P","
                                          P12_POPMEN15P","
                                          P12_POPMEN1524","
                                          P12_POPMEN2554","
                                          P12_POPMEN5579","
                                          P12_POPMEN80P","
                                          P12_POP15P_PSEUL","
                                          P12_POP1524_PSEUL","
                                          P12_POP2554_PSEUL","
                                          P12_POP5579_PSEUL","
                                          P12_POP80P_PSEUL","
                                          P12_POP15P_MARIE","
                                          P12_POP15P_CELIB","
                                          P12_POP15P_VEUF","
                                          P12_POP15P_DIVOR","
                                          C12_MEN_CS1","
                                          C12_MEN_CS2","
                                          C12_MEN_CS3","
                                          C12_MEN_CS4","
                                          C12_MEN_CS5","
                                          C12_MEN_CS6","
                                          C12_MEN_CS7","
                                          C12_MEN_CS8","
                                          C12_PMEN_CS1","
                                          C12_PMEN_CS2","
                                          C12_PMEN_CS3","
                                          C12_PMEN_CS4","
                                          C12_PMEN_CS5","
                                          C12_PMEN_CS6","
                                          C12_PMEN_CS7","
                                          C12_PMEN_CS8","
                                          C12_FAM","
                                          C12_COUPAENF","
                                          C12_FAMMONO","
                                          C12_COUPSENF","
                                          C12_NE24F0","
                                          C12_NE24F1","
                                          C12_NE24F2","
                                          C12_NE24F3","
                                          C12_NE24F4P","
                                          P12_POP0205","
                                          P12_POP0610","
                                          P12_POP1114","
                                          P12_POP1517","
                                          P12_POP1824","
                                          P12_POP2529","
                                          P12_POP30P","
                                          P12_SCOL0205","
                                          P12_SCOL0610","
                                          P12_SCOL1114","
                                          P12_SCOL1517","
                                          P12_SCOL1824","
                                          P12_SCOL2529","
                                          P12_SCOL30P","
                                          P12_NSCOL15P","
                                          P12_NSCOL15P_DIPL0","
                                          P12_NSCOL15P_CEP","
                                          P12_NSCOL15P_BEPC","
                                          P12_NSCOL15P_CAPBEP","
                                          P12_NSCOL15P_BAC","
                                          P12_NSCOL15P_BACP2","
                                          P12_NSCOL15P_SUP","
                                          P12_HNSCOL15P","P12_HNSCOL15P_DIPL0","
                                          P12_HNSCOL15P_CEP","P12_HNSCOL15P_BEPC","P12_HNSCOL15P_CAPBEP","P12_HNSCOL15P_BAC","P12_HNSCOL15P_BACP2","P12_HNSCOL15P_SUP","P12_HCHOM1564","P12_FCHOM1564"," P12_INACT1564","P12_HINACT1564","P12_FINACT1564","P12_ETUD1564","P12_HETUD1564","P12_FETUD1564","P12_RETR1564","P12_HRETR1564","P12_FRETR1564"," P12_AINACT1564","P12_HAINACT1564","P12_FAINACT1564","
                                          C12_ACT1564","C12_ACT1564_CS1"," C12_ACT1564_CS2","C12_ACT1564_CS3","C12_ACT1564_CS4","C12_ACT1564_CS5","C12_ACT1564_CS6","
                                          C12_ACTOCC1564"," C12_ACTOCC1564_CS1","C12_ACTOCC1564_CS2","C12_ACTOCC1564_CS3","C12_ACTOCC1564_CS4","C12_ACTOCC1564_CS5","C12_ACTOCC1564_CS6","P12_ACTOCC15P"," P12_HACTOCC15P","P12_FACTOCC15P","P12_SAL15P","P12_HSAL15P"
                                         ,"P12_F0019","P12_F2064","P12_F65P"
                                           )]
data_modifier2<-data_modifier[ ,!colnames(data_modifier)%in%c("Nbre_Votants_Intersection" ,"Bureau_de_vote")]#enlever la colonne de bureau de vote et la variable réponse
str(data_modifier2)
lm(data_modifier$Nbre_Votants_Intersection ~ .,data=data_modifier2)
#faire une boucle pour voire les var les plus sinificative en regardant les pvalues


attach(data_modifier2)#on dit a R que l'on a travaillé sur le jeu de donnée
#Voir la corrélation entre notre notre variable de réponse et le variables explicatives 
for(i in colnames(data_modifier2)){

  A[i]=summary(lm(data_modifier$Nbre_Votants_Intersection ~data_modifier2[,i] ))[[4]][8]#recuperer la pvalues
}

# on peut voir l'existance de plusieurs varibales explicatives qui n'influence pas sur la variation de notre variable réponse 
#nous allons faire le lmer en prenant en compte la variable bureau de vote en tand que variable à effet aléatoire 




a=lm(data_modifier$Nbre_Votants_Intersection ~ data_modifier2$Surface_BV+data_modifier2$Surface_Intersection)
summary(lm(Nbre_Votants_Intersection ~ Surface_BV))[[4]][8]#recuperation du pvalue 
summary(a)[[4]][1]

summary(lmer(data$Nbre_Votants_Intersection~data_modifier2$Tour+(1|data_modifier$Bureau_de_vote),REML = FALSE))

cor(x = data$Nbre_Votants_Intersection,y=data_modifier2$Surface_BV)
C=array()
for(i in colnames(data_modifier2)){
  
  C[i]=cor(x=data_modifier$Nbre_Votants_Intersection ,matrix(data_modifier2$i) )
}


###########
lm(Nbre_Votants_Intersection~colnames(data_modifier[,2]))

lmer(Nbre_Votants_Intersection ~ P12_POP1564
     +P12_POP1524
     +P12_POP2554
     +P12_POP5564+(1|Bureau_de_vote),data=data_modifier,REML = FALSE)
a=lm(Nbre_Votants_Intersection~P12_F1564)
summary(a)
lmer(Nbre_Votants_Intersection ~P12_F1564
     +P12_F1524
     +P12_F2554
     +P12_F5564
     +P12_ACT1564
     +P12_ACT1524
     +P12_ACT2554
     +P12_ACT5564
     +(1|Bureau_de_vote),data=data_modifier,REML = FALSE)

regsubsets( P12_F1564
           +P12_F1524
           +P12_F2554
           +P12_F5564
           +P12_ACT1564
           +P12_ACT1524
           +P12_ACT2554
           +P12_ACT5564,Nbre_Votants_Intersection,data=data_modifier)
lm(Nbre_Votants_Intersection~typ_iris)
a=regsubsets(data_modifier[,5:10],Nbre_Votants_Intersection,really.big=T)
#Sépration de données en train et test
sample <- sample.int(n = nrow(data_modifier), size = round(nrow(data)*70/100,0), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]