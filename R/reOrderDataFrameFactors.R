#' @title Re Order Data Frame Factors
#' @param mydata a data frame
#' @return a data frame with factors levels so most frequent is first
#' @author James Zeng
#' @export
#' @details only reoders factor levels
#' @description  this function takes a data frame and returns it with reordered levels
reOrderDataFrameFactors <- function(mydata){
  for(i in 1:(ncol(mydata))){
    if(class(mydata[,i]) == 'factor'){
      o<-table(mydata[,i])[order(table(mydata[,i]), decreasing = TRUE)]
      levels(mydata[,i])<-names(o)
    }}
  return(mydata)
}
