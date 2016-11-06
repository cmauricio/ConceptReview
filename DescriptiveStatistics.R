#This code uses the package Plyr to generate the desciptive statistics for the sample
#of texts according to the database generated for the analysis.

library(plyr)
library(ggplot2)

wDir<-"C:/Users/Mauro/Dropbox/AuditConceptualRevision/Analyses"

setwd(wDir)

Review<-read.table("160705-ReviewShort.csv",header=T,sep=";",stringsAsFactors = T)



##### Descriptives #####

DescInter<-count (Review, vars=c("KeywordUsed","ConceptualInterchange"))
write.table(DescInter,"DescriptivesInterchange.csv",sep=";")

DescJournals<-count (Review, vars="Journal")
DescBooks<-count (Review, vars="Book")
write.table(DescJournals,"DescriptivesJournals.csv",sep=";")
write.table(DescBooks,"DescriptivesBooks.csv",sep=";")

DescKW<-count (Review, vars="KeywordUsed")
write.table(DescKW,"DescriptivesKeywordUsed.csv",sep=";")

DescDatabases<-count (Review, vars="Database")
write.table(DescDatabases,"DescriptivesDatabases.csv",sep=";")

DescMfield<-count (Review, vars="MainField")
DescSfield<-count (Review, vars="SecondaryField")
write.table(DescMfield,"DescriptivesMainfield.csv",sep=";")
write.table(DescSfield,"DescriptivesSecondaryfield.csv",sep=";")

Desc1Lev<-count (Review, vars="ResearchType")
Desc2Lev<-count (Review, vars="ResearchDesign")
Desc3Lev<-count (Review, vars="ResearchLevel")
Desc4Lev<-count (Review, vars="ResearchSublevel")
write.table(Desc1Lev,"DescriptivesRType.csv",sep=";")
write.table(Desc2Lev,"DescriptivesRDesign.csv",sep=";")
write.table(Desc3Lev,"DescriptivesRLevel.csv",sep=";")
write.table(Desc4Lev,"DescriptivesRsublevel.csv",sep=";")

FieldIntersection<-count (Review, vars=c("MainField","SecondaryField"))
write.table(FieldIntersection,"FieldIntersection.csv",sep=";")

AtoIntersection<-count (Review, vars=c("ResearchType","ResearchDesign","ResearchLevel","ResearchSublevel"))
write.table(AtoIntersection,"AtoIntersection.csv",sep=";")


##### Plots #####

p<-ggplot(a,aes(KeywordUsed,ConceptualInterchange, size=as.factor(freq)))

p+geom_point()


##### Experimental studies and Teleology #####

DicVarsTele<-read.table("160705-DicVarsTele.csv",header=T,sep=";",stringsAsFactors = T)

NDTeleology<-count (DicVarsTele, vars=c("ResearchType","ResearchDesign","ResearchLevel","ResearchSublevel","ND"))
write.table(NDTeleology,"NDTeleology.csv",sep=";")

##### Descriptives for Characteristics, teleologies, and complementary concepts #####

DicVarsChars<-read.table("160705-DicVarsChars.csv",header=T,sep=";",stringsAsFactors = T)
DescCharacteristics<-colSums(DicVarsChars[,18:length(DicVarsChars)])
write.table(DescCharacteristics,"DescriptivesCharacteristics.csv",sep=";")

DicVarsTele<-read.table("160705-DicVarsTele.csv",header=T,sep=";",stringsAsFactors = T)
DescTeleologies<-colSums(DicVarsTele[,18:length(DicVarsTele)])
write.table(DescTeleologies,"DescriptivesTeleologies.csv",sep=";")

DicVarsComp<-read.table("160705-DicVarsComp.csv",header=T,sep=";",stringsAsFactors = T)
DescComplementary<-colSums(DicVarsComp[,18:length(DicVarsComp)])
write.table(DescComplementary,"DescriptivesComplementary.csv",sep=";")

##### Variable operationalisation question #####

Review2<-read.table("160706-ReviewShort.csv",header=T,sep=";",stringsAsFactors = T)

OperationalisationFail<-count (Review2, vars=c("MainField","ResearchType","VariablesOperationalised"))
write.table(OperationalisationFail,"OperationalisationFail.csv",sep=";")


##### Not description chars #####

Review2<-read.table("160706-ReviewShort.csv",header=T,sep=";",stringsAsFactors = T)

CharacterisationFail<-count (Review2, vars=c("MainField","ResearchType","ConceptCharacteristics"))
write.table(CharacterisationFail,"CharacterisationFail.csv",sep=";")

##### Not description tele #####

Review2<-read.table("160706-ReviewShort.csv",header=T,sep=";",stringsAsFactors = T)

TeleologyFail<-count (Review2, vars=c("MainField","ResearchType","Teleology"))
write.table(TeleologyFail,"TeleologyFail.csv",sep=";")
