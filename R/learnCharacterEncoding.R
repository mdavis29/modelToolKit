#' @title Lean Character Encoding
#' @param mydata a data frame containing factors
#' @param factCols a character of columns in mydata
#' @param maxLevs maximum number of factor levels to encode
#' @param minFreq minimum frequency to spawn an encoded col
#' @param verbose pring debugging
#' @export 

learnCharacterEncoding<-function(mydata, factCols = NULL, maxLevs = 50, minFreq = 1, verbose = FALSE){
  haveCols<-colnames(mydata)
  if(is.null(factCols)){factCols<-haveCols[lapply(mydata, class) == 'factor']
    if(verbose)print('using all factor columns since factCols is NULL')
  }
  n.facts<-length(factCols)
  factLevs<-list()
  newFactLevs<-list()
  droppedLevs<-list()
  for ( i in 1:n.facts ){
    tempFactName<-factCols[i]
    if(verbose)print(paste('finding', tempFactName))
    tempData<-mydata[,tempFactName]
    tempTable<-table(tempData)
    tempTable<-tempTable[order(tempTable, decreasing = TRUE)]
    tempTable<-head(tempTable, maxLevs)
    factLevs[[i]]<-names(tempTable[tempTable >= minFreq])
    droppedLevs[[i]]<-names(tempTable[tempTable < minFreq])
    newFactLevs[[i]]<-make.names(factLevs[[i]])
  }
  names(newFactLevs)<-factCols
  names(droppedLevs)<-factCols
  names(factLevs)<-factCols
  output<-list(colNames = factCols,
             oldlevels = factLevs,
             newLevels = newFactLevs,
             maxLevs = maxLevs,
             minFreq = minFreq,
             ignoredCols = setdiff(factCols, haveCols),
             droppedLevs = droppedLevs,
             originalCols = haveCols,
             newColNames = make.names(haveCols))
  class(output)<-append('catagoryEncoder', class(output))
  return(output)
}

#' @title predict method for catagorical encoder object
#' @param catEncoder catagoryEncoder object
#' @param newData a new Data frame  
#' @param sparse return a sparse matrix
#' @export

predict.catagoryEncoder<-function(catEncoder, newData, sparse = TRUE){
  newCols<-colnames(newData)
  catCols<-names(catEncoder$oldlevels)[lapply(catEncoder$oldlevels, length)>0]
  haveCols<-newCols[newCols %in% catCols]
  missingCols<-catCols[!catCols %in% newCols]
  nc<-length(haveCols)
  cores<-parallel::detectCores()  
  cl<-makeCluster(cores)
  registerDoParallel(cl)
  outMat<-foreach(i =  1:nc, .combine = 'cbind', .packages=c("foreach", 'Matrix')) %dopar% {
    tempCol<- haveCols[i]
    tempLevs<-catEncoder$oldlevels[[tempCol]]
    tempData<-as.factor(newData[,tempCol])
    nl<-length(tempLevs)
      tempMat<- foreach(j = 1:nl, .combine = 'cbind', .packages = 'Matrix' ) %do% {
        if(!sparse){x=as.matrix(ifelse(tempData %in% tempLevs[j],1,0), ncol = 1)}
        if(sparse){x=Matrix(ifelse(tempData %in% tempLevs[j],1,0), ncol = 1, sparse = TRUE)}
        x
      } ## end inner foreach 
    colnames(tempMat)<-make.names(paste(tempCol, tempLevs, sep = '.'))
    tempMat
  }## end outer foreach
  stopCluster(cl)
return(outMat)
}


