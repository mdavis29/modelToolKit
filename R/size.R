#' @title size
#' @param mydata an r object
#' @param units units in the format(object.size) method
#' @return a data frame of preformance metrics for all the models in the enviroments
#' @description usefull if you save a bunch models in an enviroment and want to test them all
#' @author Matthew Davis
#' @details this function is for choosing the best model based on a test set
#' @export
size<-function(mydata,units = 'Mb'){
  format(object.size(mydata), units =units)
}
