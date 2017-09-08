#' @title  Logistic Difference for Data Frames 
#' @author Matthew Davis
#' @param model a model of class glm, rpart or ranger
#' @param testObs  a data frame with one row to predict with the model
#' @param mydata a data frame with columns included in model
#' @param refOb a data frame with one row for a reference observation
#' @param verbose print debugging output
#' @description a function that interates through model predictors to estimate impact
#' @return a vector of estimated absolute influences 
#' @export

logisticDifferenceDf<-function(model,testObs, refOb = NULL, mydata = NULL, verbose = TRUE){
  n<-nrow(testObs)
  if(is.null(refOb) & !is.null(mydata)){
    refOb<-mostLikelyOb(mydata)
  } 
  if(n == 1){
    output<-logisticDifference(model = model,
                               testOb = testObs,
                               refOb = refOb, 
                               mydata = mydata,
                               verbose = verbose)
  }
  if(n > 1){
    cores<-parallel::detectCores()
    cores<-min(c(n, cores))
    cl<-makeCluster(cores)
    registerDoParallel(cl)
    output<-foreach(i = 1:n, .combine = 'rbind', .inorder = TRUE, .packages = 'modelToolKit') %dopar%{
      logisticDifference(model = model,
                         testOb = testObs[i,],
                         refOb = refOb, 
                         mydata = mydata,
                         verbose = verbose)
    }
  }
  return(output)
}
