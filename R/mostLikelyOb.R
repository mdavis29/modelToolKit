#' @title  Most Likely Factor
#' @author Matthew Davis
#' @param mydata a data frame
#' @param useMedian calc median for numerics, if FALSE calculates mode
#' @return a vector of frequencies of the most frequent Classes
#' @description finds the most frequent factor, or if number finds the mode
#' @export
mostLikelyOb<-function(mydata, useMedian = TRUE){
  nc<-ncol(mydata)
  output<-matrix(ncol = 0, nrow  = 1)
  for (i in 1:nc){
    tempData<-mydata[,i]
    if(class(tempData) %in% c('factor')){
      tempOut<-names(mostLikelyFact(tempData))
    }
    if(class(tempData) %in% c('numeric', 'integer') & useMedian ){
      tempOut<-median(tempData, na.rm = TRUE)
    }
    if(class(tempData) %in% c('numeric', 'integer') & !useMedian ){
      tempOut<-names(mostLikelyFact(tempData))
    }
    output<-data.frame(output, tempOut)
  }
  colnames(output)<-colnames(mydata)
  return(output)
}



