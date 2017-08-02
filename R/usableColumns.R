#' @title Usable Columns
#' @param mydata a data frame
#' @param maxUnique maxim number of unique values in a factor column
#' @param seed max Percentage of the majority class 
#' @param verbose print troubleshouting information  
#' @return a vector of column names
#' @author Matthew Davis
#' @description returns a vector of columns names usable for machine learning models 
#' @details This predict UHC Work RVUS from Work RVUs
#' @export

usableColumns<-function(mydata, maxUnique = 100, maxPrecentOneClass = .99, verbose = FALSE){
  mydata<-as.data.frame(mydata)
  nc<-ncol(mydata)
  library(doParallel)
  cl<-makeCluster(parallel::detectCores())
  registerDoParallel(cl)
  if(verbose)print(cl)
  output<- foreach(i = 1:(nc)) %dopar% {
    tempData <- mydata[,i]
    tempOut<-colnames(mydata)[i]
    tempData_factor<-as.factor(tempData)
    numUnique<-length(unique(levels(tempData_factor)))
    tempDataClass<-class(tempData)
    if(numUnique < 2 )tempOut<-NULL
    if(numUnique > maxUnique & 
       tempDataClass %in% c('factor',"character" ))tempOut<-NULL
    if(!tempDataClass %in% c('factor', 'character',  'numeric',  'integer'))tempOut<-NULL
    tempTab<-table(tempData)
    if(max(tempTab)/sum(tempTab) > maxPrecentOneClass)tempOut<-NULL
    tempOut
  }
  output<-unlist(output)
  return(output)
}
