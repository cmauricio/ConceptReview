#This piece of code uses the package 'gmodels' to carry chi analysis for comparing
#the differences between different definitions found in the variables analysed
#for the concept review.

library(gmodels)

wDir<-"C:/Users/Mauro/Dropbox/AuditConceptualRevision/Analyses"

setwd(wDir)

Matrix<-read.table("160704-ReviewShort.csv", sep=";", header = T, stringsAsFactors = F) ## Edges

SwitchTest<-cbind(Matrix$KeywordUsed,Matrix$ConceptualInterchange)
colnames(SwitchTest)<-c("KeywordUsed","ConceptualInterchange")

Cont<-xtabs(~ConceptualInterchange+KeywordUsed, data=SwitchTest)

CrossTable(Matrix$KeywordUsed, Matrix$ConceptualInterchange,
           digits=3,chisq = T, expected = T,
           sresid=T, format = c("SAS","SPSS"))

#####


Chars.Field.Test<-cbind(Matrix$MainField,Matrix$ConceptCharacteristics)
colnames(Chars.Field.Test)<-c("MainField","ConceptCharacteristics")

for (i in 1:nrow(Chars.Field.Test)){
     if (Chars.Field.Test[i,2]!="ExternalDefinition"& Chars.Field.Test[i,2]!="ND"){
          Chars.Field.Test[i,2]<-"Defined"
     }
     if(Chars.Field.Test[i,1]!="Education"&Chars.Field.Test[i,1]!="Philosophy"){
          Chars.Field.Test[i,1]<-NA
     }
}

Chars.Field.Test<-na.omit(Chars.Field.Test)


CrossTable(Chars.Field.Test[,1], Chars.Field.Test[,2],
           digits=3,chisq = T, expected = T,
           sresid=T, format = c("SAS","SPSS"))

#####

Chars.Research.Test<-cbind(Matrix$ResearchType,Matrix$ConceptCharacteristics)
colnames(Chars.Research.Test)<-c("ResearchType","ConceptCharacteristics")

for (i in 1:nrow(Chars.Research.Test)){
     if (Chars.Research.Test[i,2]!="ExternalDefinition"& Chars.Research.Test[i,2]!="ND"){
          Chars.Research.Test[i,2]<-"Defined"
     }
     if(Chars.Research.Test[i,1]!="Empiric"&Chars.Research.Test[i,1]!="Theoretical"){
          Chars.Research.Test[i,1]<-NA
     }
}

Chars.Research.Test<-na.omit(Chars.Research.Test)


CrossTable(Chars.Research.Test[,1], Chars.Research.Test[,2],
           digits=3,chisq = T, expected = T,
           sresid=T, format = c("SAS","SPSS"))


#####

Tele.Research.Test<-cbind(Matrix$ResearchType,Matrix$Teleology)
colnames(Tele.Research.Test)<-c("ResearchType","Teleology")

for (i in 1:nrow(Tele.Research.Test)){
     if (Tele.Research.Test[i,2]!="ND"){
          Tele.Research.Test[i,2]<-"Defined"
     }
     if(Tele.Research.Test[i,1]!="Empiric"& Tele.Research.Test[i,1]!="Theoretical"){
          Tele.Research.Test[i,1]<-NA
     }
}

Tele.Research.Test<-na.omit(Tele.Research.Test)


CrossTable(Tele.Research.Test[,1], Tele.Research.Test[,2],
           digits=3,chisq = T, expected = T,
           sresid=T, format = c("SAS","SPSS"))
