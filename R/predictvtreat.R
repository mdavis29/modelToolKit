#' @title Predict with vtreat
#' @param fit a model of class 'train' from caret
#' @param mydata a data frame to test the model against
#' @param treatment a 'teatment' class object from vtreat package (if this was used as a pre proc step on input data)
#' @param p psign (Vtreat parameter that trims off insigicant varibles, set to 1 for no trimming)
#' @return vector or performance metrics for the model
#' @author Matthew Davis
#' @details This predict UHC Work RVUS from Work RVUs
#' @description this creates a prediction method with a treatment using vtreat
#' @export
##prediction function including the preProc
predictVtreat<-function(fit, mydata, treatment = NULL, p = 1 ){
        nameTest<-all(fit$coefnames %in% colnames(mydata))
        if(!is.null(treatment) & nameTest == FALSE){
            mydata<-prepare(treatment, mydata, pruneSig = p)}
        p<-predict(fit, mydata)
      return(p)}
