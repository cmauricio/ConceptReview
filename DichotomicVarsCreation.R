#This code uses the packade 'plyr' to automatic generate dichotomic variables
#from the categorical data found in the concept review database.

wDir<-"C:/Users/Mauro/Dropbox/AuditConceptualRevision/Analyses"

setwd(wDir)

library(plyr)

Review<-read.table("160704-ReviewShort.csv",header=T,sep=";",stringsAsFactors = F)

##### Creating Dichotomic Variables for Complementary Concepts #####
CharacteristicLevels <- unique(unlist(strsplit(Review$ConceptCharacteristics, ",")))

Review$id <- 1:nrow(Review)

DicCharsLevs<-ddply(Review, .(id), function(x)
     table(factor(unlist(strsplit(paste(x$ConceptCharacteristics), ",")),
                  levels = CharacteristicLevels)))

##### Creating Dichotomic variables for Teleologies #####
TeleologyLevels <- unique(unlist(strsplit(Review$Teleology, ",")))

Review$id <- 1:nrow(Review)

DicTeleLevs<-ddply(Review, .(id), function(x)
     table(factor(unlist(strsplit(paste(x$Teleology), ",")),
                  levels = TeleologyLevels)))

##### Creating Dichotomic variables for Teleologies #####
ComplementaryLevels <- unique(unlist(strsplit(Review$ComplementaryConcepts, ",")))

Review$id <- 1:nrow(Review)

DicCompLevs<-ddply(Review, .(id), function(x)
     table(factor(unlist(strsplit(paste(x$ComplementaryConcepts), ",")),
                  levels = ComplementaryLevels)))



##### Creating Databases for each of the variables #####

VarsChars<-cbind(Review,DicCharsLevs)
VarsComp<-cbind(Review,DicCompLevs)
VarsTele<-cbind(Review,DicTeleLevs)

barplot(colSums(VarsChars[20:ncol(VarsChars)]))
barplot(colSums(VarsComp[20:ncol(VarsComp)]))
barplot(colSums(VarsTele[20:ncol(VarsTele)]))

write.table(VarsChars,"VarsChars.csv",sep=";")
write.table(VarsComp,"VarsComp.csv",sep=";")
write.table(VarsTele,"VarsTele.csv",sep=";")

