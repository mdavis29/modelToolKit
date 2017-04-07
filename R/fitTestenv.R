#' @title Test Enviroment 2 class
#' @param mydata data frame to use as test data
#' @param e enviroment with models of class train are stored
#' @param classCol name of the class column
#' @param predVars column of predictor varaibles (optional), guess of the first model if NULL
#' @param levs levels of the class column
#' @param referenceLevNum  of the reference level to consider success (ie typically 1 from 0/1 class)
#' @return a data frame of preformance metrics for all the models in the enviroments
#' @description usefull if you save a bunch models in an enviroment and want to test them all
#' @author Matthew Davis
#' @details this function is for choosing the best model based on a test set
#' @export

testEnv2class<-function(mydata, e,
                        classCol = 'Class',
                        predVars = NULL,
                        levs = c('notReAdmitted','reAdmitted'),
                        referenceLevNum = 2 ){
  output<-data.frame()
  modelNames<-ls(e)
  if(!is.null(predVars)){mydata.predVars<-mydata[, predVars] }

  for ( i in 1:(length(modelNames))){
    fit.temp<-get(modelNames[i], envir = e)
    if(is.null(predVars)){
      print(paste('guessing preVar Names from', modelNames[i]))
      mydata.predVars<-mydata[,fit.temp$finalModel$xNames]
      }
    print(paste('now analyzing', modelNames[i] ))
    predsr<-predict(fit.temp, mydata.predVars,type = 'raw')
    predsp<-predict(fit.temp, mydata.predVars,type = 'prob')
    preds<-data.frame(obs = mydata[, classCol] ,pred = predsr, predsp)
    twoCSum<-caret::twoClassSummary(preds, lev = levs, model = modelNames[i] )
    ll<-caret::mnLogLoss(preds, lev = levs, model = modelNames[i] )
    r<-pROC::roc(preds$obs, preds[, levs[referenceLevNum]], levels =levs )
    threshB<-pROC::coords(r, x = 'best', best.method = 'closest.topleft')
    threshY<-pROC::coords(r, x = 'best', best.method = "youden")
    cm<-ModelMetrics::confusionMatrix(ifelse(preds$obs == levs[referenceLevNum],1,0),
                                        preds[, levs[referenceLevNum]],
                                        cutoff = threshB[1] )
      tp<-cm[1,1]
      tn<-cm[2,2]
      fp<-cm[2,1]
      fn<-cm[1,2]
      x<-c(ll,twoCSum, as.numeric(r$auc),threshB,threshY)
      names(x)<-c(names(ll),
                  paste('twoClassSummary',names(twoCSum),sep = ''),
                  'AUCofROC',
                  paste('threshB',names(threshB),sep = ''),
                  paste('threshY',names(threshY),sep = ''))
      temp<-data.frame(t(x), modelNames = modelNames[i])
      temp$accuracyBestThresh<-(tp+tn)/(tp+tn+fp+fn)
      temp$precisionBestThresh<-tp/(tp+fp)
      temp$recallBestThresh<-tp/(tp+fn)
      output<-rbind(output, temp)
    }
  output<-output[order(output$logLoss),]
  return(output)
  }
