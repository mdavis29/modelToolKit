#' @title  Logistic Difference for Data Frames 
#' @author Matthew Davis
#' @param model a model of class glm, rpart or ranger
#' @param testObs  a data frame with one row to predict with the model
#' @param refData a data frame with columns included in model
#' @param className  name of the class column used for stratitifed resampling 
#' @param p percent of the refData to keep in the sample
#' @param seed random sampleing seed
#' @param verbose print debugging output
#' @description a function that interates through model predictors to estimate impact
#' @return a vector of estimated absolute influences 
#' @export

logisticDifferenceDf<-function(model,
                               testObs, 
                               refData, 
                               className = NULL,
                               p = .1,
                               seed = 2012,  
                               verbose = TRUE){
  n <- nrow(testObs)
  varImp<-NULL
  if(n == 1){
    output<-logisticDifference(model = model,
                               testOb = testObs,
                               refOb = refData, 
                               verbose = verbose)
  }
  modelClass<-class(model)[1]
  packagesToLoad<-c()
  if( modelClass %in% 'ranger'){
    preds<-predict(model, refData)$predictions[,2]
    varImp<-try(model$variable.importance[order(abs(model$variable.importance), decreasing = TRUE)])
    packagesToLoad<-'ranger'
  }  
  if( modelClass %in% 'rpart'){
    preds<-predict(model, refData)[,2]
    varImp<-try(model$variable.importance[order(abs(model$variable.importance), decreasing = TRUE)])
    packagesToLoad<-'rpart'
  }
  if( modelClass %in% 'glm'){
    preds<-predict(model, refData, type = 'response')
    varImp<-caret::varImp(model)[,1]
    names(varImp)<-rownames(caret::varImp(model))
    varImp<-varImp[order(varImp, decreasing = TRUE)]
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
    packagesToLoad<-c(packagesToLoad, 'modelToolKit')
    cores<-parallel::detectCores()
    cores<-min(c(n, cores))
    cl<-makeCluster(cores)
    registerDoParallel(cl)
    
    output<-foreach(i = 1:n, .combine = 'rbind', .inorder = TRUE, .packages = packagesToLoad) %dopar%{
      tempTestOb<-testObs[i,]
      tempOutput<-foreach(j = 1:n.samples, .combine = 'rbind', .packages = packagesToLoad)%do%{
        tempRefOb<-reSampledData[j,]
        logisticDifference(model = model,
                         testOb = tempTestOb,
                         refOb = tempRefOb, 
                         verbose = verbose)
        }## end inner foreach looping through all the samples 
      apply(tempOutput,2,mean, na.rm = TRUE)  
    }## end outer foreach looping through all the test obs
    stopCluster(cl)
  }## end if n>1
  
  outputList<-list(output = output,
                   refData =refData,
                   testObs = testObs, 
                   className = className,
                   preds = preds,
                   p = p,
                   varImp = varImp,
                   class = class(model))
  class(outputList) <- append(class(outputList),"logisticDiff")
  return( outputList)
}

#' @title Plot method for class logisticDiff
#' @param obj an object of class logisticDiff
#' @param n.obs number of obs to be used
#' @param n.vars number of variables to plot
#' @param bins number of bins on histogram plot
#' @param indvidualPlot to individual observation influence
#' @param verbose print debugging
#' @description A plot method of variable inflence objects
#' @export
#' 

plot.logisticDiff<-function(obj , n.obs = 50,n.vars = 10,  indvidualPlot = FALSE, bins = 50, verbose = FALSE ){
  n.vars<-min(n.vars, ncol(obj$output)-2)
  output<-head(data.frame(obj$output), n.obs)
  if(verbose)print('only uses first 50 obs')
  output$testObNumber<-as.factor(paste('test Ob', 1:(nrow(output))))
  numCols<-colnames(output)[lapply(output, class) %in%c('numeric', 'integer')]
  if(!indvidualPlot){
    p <- ggplot(data.frame(preds = preds),  aes(x=preds)) +
        geom_histogram(bins = bins, show.legend = TRUE) +
        geom_vline(data = output,
               aes(xintercept = testPred , color=testObNumber) ,
               show.legend = TRUE )
    print(p)
  }
  if(indvidualPlot){
    dat.m<-melt(output, id = 'testObNumber')
    dat.m1<-dat.m[dat.m$variable %in% c('referencePred', 'testPred'),]
    dat.m2<-dat.m[!dat.m$variable %in% c('referencePred', 'testPred'),]
    agg<-aggregate(value~variable, data=dat.m2, FUN = mean)
    agg<-agg[order(abs(agg$value), decreasing = TRUE),]
    keepVars<-agg$variable[1:n.vars]
    dat.m2<-dat.m2[dat.m2$variable %in% keepVars, ]
    dat.m2<-dat.m2[order(dat.m2$value, decreasing = TRUE),]
    
    dat.m<-rbind(dat.m1, dat.m2)

    p <- ggplot(dat.m, aes(y = value))+ 
                      geom_col(aes(x = variable, colour = value, fill = value), show.legend = FALSE)+ 
                      geom_text(aes(label = round(value,3), x = variable, y = value/2 ), show.legend = FALSE)+
                      coord_flip()+
                      
                      ggtitle('Varible Importance') + 
                      xlab('variable Name') + 
                      ylab('contribution') +   
                      scale_fill_gradient2(low = 'darkred', mid = 'gray', high= 'lightblue', midpoint = 0) + 
                      facet_wrap(~ testObNumber)

    print(p)
  }
}
  