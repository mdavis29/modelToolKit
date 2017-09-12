#' @title  Logistic Difference for Data Frames 
#' @author Matthew Davis
#' @param model a model of class glm, rpart or ranger
#' @param testObs  a data frame with one row to predict with the model
#' @param refData a data frame with columns included in model
#' @param className  name of the class column used for stratitifed resampling 
#' @param p percent of the refData to keep in the sample
#' @param verbose print debugging output
#' @description a function that interates through model predictors to estimate impact
#' @return a vector of estimated absolute influences 
#' @export

logisticDifferenceDf<-function(model,testObs, refData, className = NULL,p = .1,seed = 2012,  verbose = TRUE){
  n<-nrow(testObs)
  if(n == 1){
    output<-logisticDifference(model = model,
                               testOb = testObs,
                               refOb = refData, 
                               mydata = mydata,
                               verbose = verbose, 
                               seed = 2012)
  }
  n.samples <- 1
  if(!is.null(nrow(refData))){
    set.seed(seed)
    if(is.null(className)){
      className<-'dummyClassName'
      refData[,className]<-sample(1:2, nrow(refData), replace = TRUE)
      if(verbose)print('no class name provided, random sampeling')
    }
    sampleIndex<-caret::createDataPartition(refData[, className], p = p,list = FALSE )
    reSampledData<-refData[sampleIndex, ]
    n.samples<-nrow(reSampledData)
  }
  if(is.null(nrow(refData))){
    reSampledData<-as.data.frame(refData)
  }
  if(verbose)print(paste('num samples', n.samples))
  if(n > 1){
    cores<-parallel::detectCores()
    cores<-min(c(n, cores))
    cl<-makeCluster(cores)
    registerDoParallel(cl)
    output<-foreach(i = 1:n, .combine = 'rbind', .inorder = TRUE, .packages = 'modelToolKit') %dopar%{
      tempTestOb<-testObs[i,]
      
      tempOutput<-foreach(j = 1:n.samples, .combine = 'rbind')%do%{
        tempRefOb<-reSampledData[j,]
        logisticDifference(model = model,
                         testOb = tempTestOb,
                         refOb = tempRefOb, 
                         verbose = verbose)
        }## end inner foreach looping through all the samples 
      apply(tempOutput,2,mean, na.rm = TRUE)  
    }## end outer foreach looping through all the test obs
  }## end if n>1
  return(output)
}
