#' @title Test models in an enviroment
#' @param fit a glm model
#' @param nfits number of models to fit
#' @param p probablity cut off if binomial is use used
#' @return a dataframe of performance metrics based on removing influential points
#' @description This function finds the optimal number of influential points to remove from a linear model
#' @author Matthew Davis
#' @export
#' @details the CooksDistance Cut is the point at which to remove any points with greater distance
cooksDistanceOptimizer<-function(fit, nfits = 10, p = .5){
  cd<-cooks.distance(fit)
  cd.order<-order(cd, decreasing = TRUE)
  cd.reorderd<-cd[cd.order]
  if (fit$family$family == "binomial"){
    aucs<-c()
    arcs<-c()
    ll<-c()
    for ( i in 0:nfits){
      keep <-cd.order > i
      fit.temp<-glm(fit$formula, data = fit$data[keep,],family =fit$family )
      preds<-predict(fit.temp, fit$data)
      cfm<-table(fit$y, preds > p )
      arcs<-append(arcs, (cfm[1]+cfm[4])/sum(cfm))
      aucs<-append(aucs, auc(fit$y, preds))
      ll<-append(ll, logLoss(fit$y, predict(fit.temp, fit$data)))
      }
    output<-data.frame(accuracy = arcs, auc = aucs, nRemoved = 0:nfits, logLoss = ll)
    output<-cbind(output,CooksDistanceCut = append(NA, cd.reorderd[1:nfits]))
    output<-output[order(output$logLoss),]

  }

  if(fit$family$family %in% c('gaussian', 'poisson')){
    r2<-c()
    rmses<-c()
    for ( i in 0:nfits){
      keep <-cd.order > i
      fit.temp<-glm(fit$formula, data = fit$data[keep,],family =fit$family )
      preds<-predict(fit.temp, fit$data)
      r2<-append(r2, cor(fit$y, preds, use = 'complete')^2)
      rmses<-append(rmses,rmse(fit$y, preds))
      }
    output<-data.frame(r2=r2, rmse = rmses, nRemoved = 0:nfits)
    output<-cbind(output,CooksDistanceCut = append(NA, cd.reorderd[1:nfits]))
    output<-output[order(output$r2, decreasing = TRUE),]
  }
  return(output)
}

