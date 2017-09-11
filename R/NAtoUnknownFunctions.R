#' @title NA To Value for Dataframe
#' @param mydata a dataframe
#' @param numCols a char vector of numeric column names to apply the na to zero 
#' @param factCols a char vector of factor column names
#' @param numVal a numeric value to replace NA in numeric columns
#' @param newLevName new level Name for NA levels
#' @description maps all numeric or only specified numeric columns in a data frame to val
#' @return the mydata data frame with num Nas mapped to numVal and fact Nas mapped to newLevName
#' @details if numVal or newLevName is null, then no action is taken that type of column
#' @export


NAtoValueDf<-function(mydata, numCols = NULL, factCols = NULL, numVal = NULL, newLevName = 'unKnown'){
  output<-NULL
  if(is.null(numCols)){
    numCols<-colnames(mydata)[lapply(mydata,class) %in% c('integer', 'numeric')]
  }
  if(is.null(factCols)){
    factCols<-colnames(mydata)[lapply(mydata,class) == 'factor']
  }
  missingFacts<-factCols[!factCols %in% colnames(mydata)]
  missingNums<-numCols[!numCols %in% colnames(mydata)]
  if(length(missingFacts) >= 1 ){stop(paste('missing', missingFacts))} 
  if(length(missingNums) >= 1 ){stop(paste('missing', missingNums))}
  nf<-length(factCols)
  nn<-length(numCols)
  if(nn > 0  & !is.null(numVal)){
    for(i in 1:nn){
      tempData<-mydata[, numCols[i]]
      tempData[is.na(tempData)]<-numVal 
      mydata[, numCols[i]] <-tempData
    }
  }
  if(nf > 0 &  !is.null(newLevName)){
    for(i in 1:nf){
      tempData<-mydata[, factCols[i]]
      levels(tempData)<-c(levels(tempData), newLevName)
      tempData[is.na(tempData)]<-newLevName
      mydata[, factCols[i]] <-tempData
    }
  }
  return(mydata)  
}   
    