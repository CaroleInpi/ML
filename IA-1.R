
  ### "IA2008-2017"

#import fichier
library(readxl)
IA2008_2017 <- suppressWarnings(read_excel("./IA2008-2017.xlsx", skip = 1))

#summary(IA2008_2017)
#str(IA2008_2017)

#choix colonnes

iapart1<-IA2008_2017[,c(1, 3, 10, 13, 16, 20,21, 24, 26, 33, 34,35,36,46, 47,51,77,75, 78, 137, 143, 145, 146 )]
#Publication Number, Title Terms - DWPI, Claims Count, Assignee - Standardized, Assignee - Original - Country
#Assignee Count, Inventor, Inventor - w/address, Inventor Count, Publication Country Code, Publication Kind Code
#Publication Date, Estimated Expiration Date, Estimated Remaining Life, Dead/Alive, Publication Year
#IPC Section, IPC Class, IPC Subclass, Language of Publication, DWPI Count of Family Members, DWPI Count of Family Countries
#Relevancy)

#str(iapart1)
sum(is.na(iapart1))


#renommage des colonnes```
library(tidyverse)
iapart2 <- iapart1 %>% rename (
  PN="Publication Number",
  MotsTitres="Title Terms - DWPI",
  nClaims = "Claims Count",
  Deposant ="Assignee - Standardized",
  PaysDeposant = "Assignee - Original - Country",
  nDeposant= "Assignee Count",
  Inventeur= "Inventor",
  AdresseInventeur = "Inventor - w/address",
  nInventeur = "Inventor Count",
  KindCode= "Publication Kind Code",
  DatePublication="Publication Date",
  PaysPublication= "Publication Country Code",
  DateExpirationPrevue= "Estimated Expiration Date",
  DureeRestantePrevue= "Estimated Remaining Life",
  DeadOrAlive = "Dead/Alive",
  AnneePublication="Publication Year",
  SectionIPC = "IPC Section",
  ClasseIPC = "IPC Class",
  SousclasseIPC = "IPC Subclass",
  Langage = "Language of Publication",
  nFamily= "DWPI Count of Family Members",
  nPays="DWPI Count of Family Countries",
  Pertinence= "Relevancy"
)
#str(iapart2)
summary(iapart2)


#changement de catégorie
iapart3 <- iapart2 %>% mutate(
  nClaims = as.numeric(nClaims),
  nDeposant= as.numeric(nDeposant),
  nInventeur = as.numeric(nInventeur),
  KindCode= factor(KindCode),
  DatePublication=as.Date(DatePublication) ,
  PaysPublication=factor(PaysPublication),
  DateExpirationPrevue= as.Date(DateExpirationPrevue),
  DeadOrAlive = factor(DeadOrAlive),
  AnneePublication=factor(AnneePublication),
  SectionIPC = factor(SectionIPC),
  ClasseIPC = factor(ClasseIPC),
  SousclasseIPC = factor(SousclasseIPC),
  Langage = factor(Langage),
  nFamily= as.numeric(nFamily),
  nPays=as.numeric(nPays),
  Pertinence= factor(Pertinence)
)
#PN en row.names
iapart4<-column_to_rownames(iapart3, var="PN")
str(iapart4)


#traitement des NAs
sapply(iapart4, class)
summary(iapart4) 
#15 donnees manquantes dans nClaims
vide<-which(is.na(iapart4$nClaims))
iapart4$nClaims[vide]<-median(iapart4$nClaims, na.rm=T) #remplissage avec mediane
IPCvide<-which(is.na(iapart4$SectionIPC))
IPCvide2<-which(is.na(iapart4$ClasseIPC))  
IPCvide3<-which(is.na(iapart4$SousclasseIPC)) 
iapart4[IPCvide,c(16,17,18)]<-c("G", "G06", "G06N")

#évaluation des colonnes restantes avec données manquantes 
sapply(iapart4, function(x) sum(is.na(x)))

#pas de traitement des colonnes comme DateExpirationPrevue: 4457 NA, etc. 

#ACP 
library(FactoMineR)
iapart5<-iapart4[,c(2,5,8,21,22)]

ia_pca<-PCA(X = iapart5, quali.sup = 5 )
ia_pca$var$contrib

#dendogramme se crée mais ne s'affiche pas
library(dendextend)
iapart6 <- iapart5[,-5]
ia.hca <- hclust(dist(iapart6))
ia.dend <- as.dendrogram(ia.hca)

#essai knn+validation croisée

table(iapart5$Pertinence)
library(class)
pertinence = iapart5$Pertinence
ia.results = knn.cv(iapart6, pertinence, 5) 
levels(ia.results) = levels(pertinence)
table(ia.results, pertinence)

library(caret)
confusionMatrix(ia.results, pertinence)
```


