#' @title Level Matcher 
#' @param newData data frame
#' @param oldData data frame 
#' @param verbose print debugging output
#' @description applies one set of levels from a data frame to another data frame
#' @return new data frame, where NA is used when an unknow level is present 
#' @export
#' 
levelMatcher<-function(dmv = NULL, 
                       newData, 
                       oldData = NULL, 
                       verbose = TRUE){
  if(!is.null(dmv)){
    colNames<- names(dmv$lvls)
    levList<-dmv$lvls
    nLevs<-length(dmv$lvls)
    }
  if(is.null(dmv) & !is.null(oldData)){
    colNames<-colnames(oldData)[lapply(oldData, class) == 'factor']
    levList<- lapply(oldData[,colNames], levels)
    nLevs<-length(colNames)
  }
  if(verbose)print(paste('num levs', nLevs))
  if(verbose)print(levList)
  for(i in 1:nLevs){
    tempData<-newData[, colNames[i]]
    levels(tempData)[!levels(tempData)%in% levList[[colNames[i]]]]<-NA
    newData[, colNames[i]]<-tempData
  }
  return(newData)
}
  
  

  
  
