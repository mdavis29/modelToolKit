#' @title Preformance Test By Catagory
#' @param preds a vector of predictions
#' @param obs a vector of observations
#' @param cats a vector of catagories
#' @param classify logical use classification
#' @param minFreq minium frequency of classes
#' @param verbose whether to print debugging output 
#' @description test model preformance by a catagory
#' @export
preformanceByCatagory<-function(preds, obs, cats, classify = FALSE, minFreq = 10,catName = NULL, verbose = FALSE){
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
    output<-output[order(output$n), ]
    class(output)<-append( 'preformByCat', class(output))
    attr(output, 'outputType')<-ifelse(colnames(output)[1] == 'RMSE', 'reg', 'class')
    attr(output, 'catName')<-catName
  return(output)
}


#' @title Plot method for preformance by category data frame
#' @param obj data frame output from preformanceByCatagory 
#' @param p integet show plot 1 or 2 (different preformance metric)
#' @param verbose print output
#' @description Treemap plot of performance by catagory, size is num of obs with that cat
#' @export

plot.preformByCat<-function(obj, p = 1, verbose =FALSE, labLen = 20){
  obj$label<-paste(
              paste(substr(as.character(obj[,3]), 1,labLen), obj[,4], sep = ' n:'), 
                round(obj[,p],2), sep = '\n')
  varName <- colnames(obj)[p]
  pal<-"RdYlBu"
  titleName<-paste('Model Performance by Catagory', attr(obj, 'catName'), sep = ': ')
  obj<-obj[!is.na(obj[,varName]),]
  if(varName[1] == 'RMSE'){
    pal<-"YlOrRd"}
  if(verbose)print(varName)
  if(verbose)print(titleName)
  if(verbose)print(pal)
 p1<- invisible(treemap(dtf =  obj, 
              index = c("label" ), 
              vSize = 'n', 
              vColor = varName ,
              palette = pal,
              title = titleName,
              type = "value",
              draw = TRUE,
              drop.unused.levels = TRUE,
              inflate.labels = TRUE))
  }
  
  
  
  
