#' @title Test models in an enviroment
#' @param model.env enviroment where 'train' class models are stroed
#' @param testSet a data frame to test the model against
#' @param treatment a 'teatment' class object from vtreat package (if this was used as a pre proc step on input data)
#' @return vector or performance metrics for the model
#' @description usefull if you save a bunch models in an enviroment and want to test them all
#' @author Matthew Davis
#' @details This predict UHC Work RVUS from Work RVUs
#' @export
#'
fitTest.env<-function(model.env, testSet = toTest, treatment = NULL ){
  output<-t(data.frame(eapply(model.env, fitTest, testSet = testSet, Treatment = treatment)))
  output<-output[order(output[,1]),]
  return(output)
}
