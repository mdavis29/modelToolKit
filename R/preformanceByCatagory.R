#' @title Preformance Test By Catagory
#' @param preds a vector of predictions
#' @param obs a vector of observations
#' @param cats a vector of catagories
#' @param classify logical use classification
#' @param minFreq minium frequency of classes
#' @param verbose whether to print debugging output 
#' @description test model preformance by a catagory
#' @export
preformanceByCatagory<-function(preds, obs, cats, classify = FALSE, minFreq = 10, verbose = FALSE){
  if(length(preds) != length(obs) | length(preds) != length(cats))stop('preds obs and cats are different lengths')
  if(class(preds) == 'factor' | class(obs) == 'factor'){
    classify <-TRUE
    preds<-as.factor(preds)
    obs<-as.factor(obs)
  }
  if(verbose)print(paste('classify :', classify))
  cats <- as.factor(cats)
  catLevs <- levels(cats)
  n <- length(catLevs)
  cores <- parallel::detectCores()
  cores <- min(c(n, cores))
  cl <- makeCluster(cores)
  if(verbose)print(paste('using cores : ', cores))
  registerDoParallel(cl)
    output<-foreach(i = 1:n, .combine = 'rbind') %dopar%{
      tempName<-catLevs[i]
      keepIndex<-cats == tempName
      tempOut<-data.frame(out1 = NA , out2 = NA, Levelname = tempName, n = sum(keepIndex))
      if(length(keepIndex)> minFreq ){
        tempObs <-obs[keepIndex] 
        tempPreds <-preds[keepIndex]
        metrics<-caret::postResample(tempPreds, tempObs)
        tempOut$out1<-metrics[1]
        tempOut$out2<-metrics[2]
      }
      tempOut
    }
    stopCluster(cl)
    if(verbose)print(dim(output))
    if(classify){colnames(output)[1:2]<-c('Accuracy', 'Kappa')
      output<-output[order(output[,1], decreasing = TRUE),]}
    if(!classify){colnames(output)[1:2]<-c('RMSE', 'Rsquared')
      output<-output[order(output[,1]),]}
  return(output)
}