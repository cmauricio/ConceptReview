##### This file develops a series of functions to prepare files for igraph #####

##### Function to transform non-squared edge matrixes into bipartite igraph matrixes #####

matrixToThreeRow <- function (mtx) {
     
     NewData<-data.frame(from=0,to=0,Weight=0)
     i<-2
     j<-2
     n<-1
     x<-((nrow(mtx)-1)*(ncol(mtx)-1))
     
     while(n<=x){
          for(i in 2:nrow(mtx)){
               for(j in 2:ncol(mtx)){
                    
                    NewData[n,1]<-as.character(mtx[i,1])
                    NewData[n,2]<-as.character(mtx[1,j])
                    NewData[n,3]<-as.character(mtx[i,j])
                    n<-n+1
                    
               }
          }
     }
     
     return(NewData)
}

##### Create codes for the EDGE matrix #####

codesForEdges<-function(nameCols,nameRows,mtx) {
     
     cols<-ncol(mtx)
     rows<-nrow(mtx)
     
     nameVC<-0
     for (i in 1:cols-1) {
          nameVC[i]<-paste(nameCols,i, sep="")
     }
     
     nameVR<-0
     for (i in 1:rows-1) {
          nameVR[i]<-paste(nameRows,i, sep="")
     }
     
     for (i in 2:cols){
          mtx[1,i]<-nameVC[i-1]
     }
     
     for (i in 2:rows){
          mtx[i,1]<-nameVR[i-1]
     }
     
     mtx[1,1]<-""
     
     
     return (mtx)
}

##### Code association the NODE matrix #####

nodeMtxFromEdge<-function(nameCols,nameRows,mtx) {
     
     cols<-ncol(mtx)
     rows<-nrow(mtx)
     
     nameVC<-0
     nameC<-0
     i<-1
     
     a<-0
     b<-0
     
     for (i in 1:cols-1) {
          nameVC[i]<-paste(nameCols,i, sep="")
     }
     
     for (i in 1:cols) {
          nameC[i]<-mtx[1,i]
     }
     
     nameC<-nameC[-1]

     nameVR<-0
     nameR<-0
     for (i in 1:rows-1) {
          nameVR[i]<-paste(nameRows,i, sep="")
     }
     
     for (i in 1:rows) {
          nameR[i]<-mtx[i,1]
     }
     
     nameR<-nameR[-1]
     
     a<-c(nameVR,nameVC)
     b<-c(nameR,nameC)
     
     c<-matrix(c("id","var.name"),nrow=1,ncol=2)
     
     c<-cbind(a,b)
     
     outcome<-as.data.frame(c)
     
     colnames(outcome)<-c("id","var.name")
     
     return (outcome)
}



