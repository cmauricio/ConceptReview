# This code uses igraph to create the network analyses for the different
# variables of the concept analyses. Characteristics, Teleologies, and 
# associated concepts

#Loading the libraries
library(igraph)
library(network)


#setting up the working directory
wDir<-"C:/Users/Mauro/Dropbox/AuditConceptualRevision/Analyses"

setwd(wDir)


#importing the data from the different databases (characteristics, teleologies and associated concepts)
#and separating the files so that only dichotomic values are included for the analyses
DicVarsChars<-read.table("160705-DicVarsChars.csv",header=T,sep=";",stringsAsFactors = T)
DicVarsChars<-DicVarsChars[,-c((1:6),(8:17))]

DicVarsTele<-read.table("160705-DicVarsTele.csv",header=T,sep=";",stringsAsFactors = T)
DicVarsTele<-DicVarsTele[,-c((1:6),(8:17))]

DicVarsComp<-read.table("160705-DicVarsComp.csv",header=T,sep=";",stringsAsFactors = T)
DicVarsComp<-DicVarsComp[,-c((1:6),(8:17))]

##### Do not use, it is generating an error that has to be corrected by hand
##### 
#MatrixChars<-aggregate(. ~ KeywordUsed, DicVarsChars, sum)
#write.table(MatrixChars,"MatrixChars.csv",sep=";")
#
#MatrixTele<-aggregate(. ~ KeywordUsed, DicVarsTele, sum)
#write.table(MatrixTele,"MatrixTele.csv",sep=";")
#
#MatrixComp<-aggregate(. ~ KeywordUsed, DicVarsComp, sum)
#write.table(MatrixComp,"MatrixComp.csv",sep=";")

#This part of the code imports the names to be assigned to the edges of the network

MatrixChars<-read.table("MatrixChars.csv", sep=";", header = T, stringsAsFactors = T) ## Edges

MatrixTele<-read.table("MatrixTele.csv", sep=";", header = T, stringsAsFactors = T)

MatrixComp<-read.table("MatrixComp.csv", sep=";", header = T, stringsAsFactors = T)



##### Transforming the edge matrix in a weighted bipartite matrix #####

MatrixChars2<-read.table("MatrixChars.csv", sep=";", stringsAsFactors = F)

MatrixTele2<-read.table("MatrixTele.csv", sep=";", stringsAsFactors = F)

MatrixComp2<-read.table("MatrixComp.csv", sep=";", stringsAsFactors = F)


#Nodes are created with this code     
CharsBip<-matrixToThreeRow(MatrixChars2) ## Nodes
CharsBip<-subset(CharsBip,Weight!=0)
write.table(CharsBip,"CharsBip.csv", sep=";")

TeleBip<-matrixToThreeRow(MatrixTele2) ## Nodes
TeleBip<-subset(TeleBip,Weight!=0)
write.table(TeleBip,"TeleBip.csv", sep=";")

CompBip<-matrixToThreeRow(MatrixComp2) ## Nodes
CompBip<-subset(CompBip,Weight!=0)
write.table(CompBip,"CompBip.csv", sep=";")




#Codes for the edges are assigned with this code
CharsCodesEdges<-codesForEdges("CH","KW",MatrixChars2) 
write.table(CharsCodesEdges,"CharsCodesEdges.csv", sep=";")

TeleCodesEdges<-codesForEdges("TL","KW",MatrixTele2)
write.table(TeleCodesEdges,"TeleCodesEdges.csv", sep=";")

CompCodesEdges<-codesForEdges("CM","KW",MatrixComp2)
write.table(CompCodesEdges,"CompCodesEdges.csv", sep=";")



NodesChars<-nodeMtxFromEdge("CH","KW",MatrixChars2)
write.table(NodesChars,"NodesChars.csv", sep=";")

NodesTele<-nodeMtxFromEdge("TL","KW",MatrixTele2)
write.table(NodesTele,"NodesTele.csv", sep=";")

NodesComp<-nodeMtxFromEdge("CM","KW",MatrixComp2)
write.table(NodesComp,"NodesComp.csv", sep=";")


##### Using iGraph #####


##### Characteristics #####

#Read the previously prepared dtabases
CharBip<-read.table("CharsBip.csv", sep=";", header = T, stringsAsFactors = F)
CharCodes<-read.table("CharsCodesEdges.csv", sep=";", header = T, stringsAsFactors = F)
NodesChars<-read.table("NodesChars.csv", sep=";", header = T, stringsAsFactors = F)

row.names(CharCodes)<-CharCodes[,1]
CharCodes<-CharCodes[,-1]

#creating the igraph object
net1<-graph.incidence(CharCodes, directed=T)

#Assigning values to the layout of the network (igraph object)
#l <- layout.kamada.kawai(net1)
l <- layout.fruchterman.reingold (net1)
l <- layout.norm(l, ymin=-2, ymax=2, xmin=-2, xmax=2)


V(net1)$color <- c("steel blue", "orange")[V(net1)$type+1]
V(net1)$shape <- c("square", "circle")[V(net1)$type+1]
V(net1)$label.cex=.7

#Exporting the network plot to a PNG file
png(filename = "CharacteristicsNet.png",
    width = 10200, height = 10000, units = "px", pointsize = 12,
    bg = "white", res = 1200)

#Plotting the network
plot(net1, vertex.label.color="black", 
     edge.arrow.size=.1,
     vertex.label=NodesChars$var.name,
     vertex.size=CharBip$Weight,
     
     mark.groups=list(c(64,33,47,89,50,49,68,10,11,35,59,30,92,75,21,20,86,19,51,34,12,46,60), 
                      c(32,22,95,74,81,71,37,91,9,94,96,102,26,31,27,62,84)), #This part of the codes mark the groups, it has to be coded by hand as you cannot get the numeric separation
     mark.col=c("#C5E5E7","#ECD89A"), 
     mark.border=NA,
     
     #vertex.size=CharBip$Weight,
     rescale=T,
     layout=l*1.5)


dev.off()



##### Teleologies #####

TeleBip<-read.table("TeleBip.csv", sep=";", header = T, stringsAsFactors = F)
TeleCodes<-read.table("TeleCodesEdges.csv", sep=";", header = T, stringsAsFactors = F)
NodesTele<-read.table("NodesTele.csv", sep=";", header = T, stringsAsFactors = F)

row.names(TeleCodes)<-TeleCodes[,1]
TeleCodes<-TeleCodes[,-1]

net2<-graph.incidence(TeleCodes, directed=T)

l <- layout.fruchterman.reingold(net2)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)


V(net2)$color <- c("steel blue", "orange")[V(net2)$type+1]
V(net2)$shape <- c("square", "circle")[V(net2)$type+1]
V(net2)$label.cex=.7

png(filename = "TeleologyNet.png",
    width = 10200, height = 10000, units = "px", pointsize = 12,
    bg = "white", res = 1200)

plot(net2, vertex.label.color="black", 
     edge.arrow.size=.1,
     vertex.label=NodesTele$var.name,
     vertex.size=10,
     #
     mark.groups=list(c(30,25,18,51,15,23,5,19,24,3,22), 
                      c(40,6,43,52,48,14,16,41,44,50,42,31,4,28,9,12,27,17,7)),
     mark.col=c("#C5E5E7","#ECD89A"), 
     mark.border=NA,
     #
     #vertex.size=CharBip$Weight,
     rescale=T,
     layout=l*1.5)

dev.off()



##### Associated Concepts #####

CompBip<-read.table("CompBip.csv", sep=";", header = T, stringsAsFactors = F)
CompCodes<-read.table("CompCodesEdges.csv", sep=";", header = T, stringsAsFactors = F)
NodesComp<-read.table("NodesComp.csv", sep=";", header = T, stringsAsFactors = F)


row.names(CompCodes)<-CompCodes[,1]
CompCodes<-CompCodes[,-1]

net3<-graph.incidence(CompCodes, directed=T)

l <- layout.fruchterman.reingold(net3)
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1)


V(net3)$color <- c("steel blue", "orange")[V(net3)$type+1]
V(net3)$shape <- c("square", "circle")[V(net3)$type+1]
V(net3)$label.cex=.7

png(filename = "ComplementNet.png",
    width = 10200, height = 10000, units = "px", pointsize = 12,
    bg = "white", res = 1200)

plot(net3, vertex.label.color="black", 
     edge.arrow.size=.1,
     vertex.label=NodesComp$var.name,
     vertex.size=10,
     
     mark.groups=list(c(42,34,32,28,18,52,26,27,37,23,11,6), 
                      c(31,10,25,53,39,12,21,15,51,36,3,50,40,17,22,19,35)),
     mark.col=c("#C5E5E7","#ECD89A"), 
     mark.border=NA,
     
     #vertex.size=CharBip$Weight,
     rescale=T,
     layout=l*1.5)

dev.off()

