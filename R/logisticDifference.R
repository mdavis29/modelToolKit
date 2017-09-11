#' @title  Logistic Difference
#' @author Matthew Davis
#' @param model a model of class glm, rpart or ranger
#' @param testOb  a data frame with one row to predict with the model
#' @param mydata a data frame with columns included in model
#' @param refOb a data frame with one row for a reference observation
#' @param verbose print debugging output
#' @description a function that interates through model predictors to estimate impact
#' @return a vector of estimated absolute influences 
#' @export

logisticDifference<-function(model, testOb, 
                             mydata = NULL, 
                             refOb = NULL, 
                             verbose = FALSE){
  if(is.null(refOb) & !is.null(mydata)){
    refOb <- mostLikelyOb(rd)
  }
  if(verbose)print(refOb[1,])
  predVars <- intersect(colnames(testOb), colnames(refOb))
  if(verbose)print(predVars)
  if(class(model)[1] %in% 'glm'){
    ref<-predict(model, refOb, type = 'response')
    output<-c()
    for(i in 1:(length(predVars))){
      tempOb<-refOb[, predVars]
      tempOb[,predVars[i]]<-testOb[, predVars[i]]
      pred<-predict(model, tempOb, type = 'response')
      output<-append(output,pred-ref )
    }
  }
  if(class(model)[1] %in% 'ranger'){
    output<-c()
    ref<-predict(model, refOb)$predictions[,2]
    for(i in 1:(length(predVars))){
      tempOb<-refOb[, predVars]
      tempOb[,predVars[i]]<-testOb[, predVars[i]]
      pred<-predict(model, tempOb, type = 'response')$predictions[,2]
      output<-append(output,pred-ref )
    }
  }
  if(class(model)[1] %in% 'rpart'){
    ref<-predict(model, refOb  )
    output<-c()
    for(i in 1:(length(predVars))){
      tempOb<-refOb[, predVars]
      tempOb[,predVars[i]]<-testOb[, predVars[i]]
      pred<-predict(model, tempOb,'prob')[,2]
      output<-append(output,pred-ref )
    }
  }
  names(output)<-predVars  
  return(output)
}
