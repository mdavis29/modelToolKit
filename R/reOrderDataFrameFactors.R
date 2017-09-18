#' @title Re Order Data Frame Factors
#' @param mydata a data frame
#' @param factCols a character vector of factor columns (optional)
#' @return a data frame with factors levels so most frequent is first
#' @author Matthew Davis
#' @return a data frame or factor with the the most frequent level first for each factor
#' @export
#' @details only reoders factor levels
#' @description  this function takes a data frame and returns it with reordered levels
reOrderAll <- function(mydata, factCols = NULL){
  if(is.factor(mydata)){
    tempTable<-table(tempData)
    newLevs<-names(tempTable)[order(tempTable, decreasing = TRUE)]
    tempData<-factor(as.character(tempData), levels = newLevs)
    mydata<-tempData
  }
  if(is.data.frame(mydata)){
    if(is.null(factCols)){
      factCols<-colnames(mydata)[lapply(mydata, class) == 'factor']
    }
    newData<-foreach(i = 1:(length(factCols)), .inorder = TRUE ) %do% {
      tempData<-mydata[, factCols[i]]
      tempTable<-table(tempData)
      newLevs<-names(tempTable)[order(tempTable, decreasing = TRUE)]
      tempData<-factor(as.character(tempData), levels = newLevs)
      tempData
    }
    newData<-do.call(cbind.data.frame, newData)
    colnames(newData)<-factCols
    mydata[, factCols]<-newData
  }
  return(mydata)
}
