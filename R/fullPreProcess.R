#' @title full Pre Process
#' @param mydata a data frame to pre process
#' @param dmv dummies from the dummyVars function
#' @param pp preProces from the caret::preProcess
#' @param treat treatment from vtreat designTreatment
#' @description looks up zip codes 2014 IRS tax data aggregated
#' @details for use to create a preprocessed dataset for predictive modeling
#' @return a data frame of preprocessed data
#' @author Matthew Davis
#' @export
#'
fullPreProcess<-function(mydata, dmv= NULL,pp = NULL,treat = NULL){
  temp<-mydata
  if(!is.null(dmv)){
    print('prediction Dummy variables')
    temp<-predict(dmv, temp)
  }
  if(!is.null(treat)){
    print('vtreating variables')
    temp<-vtreat::prepare(treat, temp, pruneSig = 1)
  }
  if(!is.null(pp)){
    print('caret PreProcessing')
    temp<-predict(pp, newdata = temp)
  }
return(temp)
}
