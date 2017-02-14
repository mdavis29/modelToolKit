#' @title Test a set of models in an enviroment
#' @param model a model of class 'train' from caret
#' @param testSet a data frame to test the model against
#' @param className optional class Name (target Class), if not provided, uses colnames(testSet)-coefNames
#' @param Treatment a 'teatment' class object from vtreat package (if this was used as a pre proc step on input data)
#' @param p psign (Vtreat parameter that trims off insigicant varibles, set to 1 for no trimming)
#' @return vector or performance metrics for the model
#' @description tests a model with a vtreatment and a test set
#' @author Matthew Davis
#' @details This predict UHC Work RVUS from Work RVUs
#' @export

fitTest<-function(model, testSet, Treatment = NULL,className = NULL, p = 1){
      output<-NA
    if(!is.null(Treatment) & is.null(className)){
            testSet<-prepare(Treatment,testSet, pruneSig = p )}
    if(is.null(className)){
        className <- colnames(testSet)[!colnames(testSet) %in% model$coefnames] }
    if(length(className)>1)print('unsure of class column')

    o<-testSet[,className[1]]
    p<-predict(model,testSet)
    output<-postResample(pred = p, obs = o)
    return(output)
    }
