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
  if(class(model)[1] %in% 'ranger')preds<-predict(model, refData)$predictions[,2]
  if(class(model)[1] %in% 'rpart')preds<-predict(model, refData)[,2]
  if(class(model)[1] %in% 'glm')preds<-predict(model, refData, type = 'response')
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
    stopCluster(cl)
  }## end if n>1
  
  outputList<-list(output = output,
                   refData =refData,
                   testObs = testObs, 
                   className = className,
                   preds = preds,
                   p = p)
  class(outputList) <- append(class(outputList),"logisticDiff")
  return( outputList)
}




#' @title Plot method for class logistucDiff

#' 
logisticDiff.plot<-function(obj , n = 50 ){
  output<-head(data.frame(obj$output), n)
  print('only uses first 50 obs')
  output$testObNumber<-as.factor(paste('test Ob', 1:(nrow(output))))
 
  ## density plot of observations 
  p1<- ggplot(data.frame(preds = preds),  aes(x=preds)) +
      geom_histogram(bins = 50, show.legend = TRUE) +
      geom_vline(data = output,
             aes(xintercept = testPred , color=testObNumber) ,
             show.legend = TRUE )
 plotList<-list()
 plotList<-append(plotList, p1)
 nr<-nrow(output)
 nr<-min(nr, n)
 for(i in 1:nr){
  tempDat<-c(output[i, !colnames(output)%in% c('testPred','testObNumber', 'referencePred')])
  dat<-data.frame(vals = unlist(tempDat), names = names(tempDat))
  dat<-dat[order(abs(dat$vals), decreasing = TRUE), ]
  testPred<-round(output[i, 'testPred'],3)
  refPred<-round(output[i,  'referencePred'],3) 
  plotTitle<-paste( 'Varible Importance',
                    paste(
                      paste('test pred', testPred, sep =':'),
                      paste('reference pred', refPred, sep =':')), sep = ' --- ')
  
  dat<-head(dat,n)
  tempPlot<- ggplot(dat, aes(x = reorder(names,abs(vals)) ,  y = vals, fill = vals))+ 
            geom_bar(stat="identity") + 
            scale_fill_gradient2(low = 'darkred', mid = 'gray', high= 'blue', midpoint = 0) +
            ylim(-max(abs(dat$vals)), max(abs(dat$vals)))+
            coord_flip()+ 
            geom_hline(yintercept = 0 ,color="black") +
            ggtitle(  plotTitle) + 
            xlab('variable Name') + 
            ylab('contribution')

    plotList<-append(plotList, tempPlot)
 }
 do.call(grid.arrange, plotList)
}
  