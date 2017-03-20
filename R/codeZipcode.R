#' @title Find the Best single hidden layer Autoencoder
#' @param zipcodes a vector of zipcodes
#' @description looks up zip codes 2014 IRS tax data aggregated
#' @author Matthew Davis
#' @export
#' @details \itemize{
#'  \item AGI 1 = $1 under $25,000 :
#'  \item AGI 2 = $25,000 under $50,000:
#'  \item AGI 3 = $50,000 under $75,000:
#'  \item AGI 4 = $75,000 under $100,000:
#'  \item AGI  5 = $100,000 under $200,000:
#'  \item AGI  6 = $200,000 or more}

zipCodeDems<-function(zipcodes, pca = FALSE){
  getZicode<-function(zc){
    if(nchar(zc) == 5){
        dems<-irs[irs[, "zipCode" ] == zc,
                !colnames(irs) %in% c('The.State.Federal.Information.Processing.System..FIPS..code',
                                      'stateCode', 'zipCode')]
        dems<-sapply(dems, mean, na.rm = TRUE)
        }
    if(nchar(zc) == 2){
        dems<-irs[irs[, "stateCode" ] == zc,
                !colnames(irs) %in% c('The.State.Federal.Information.Processing.System..FIPS..code',
                                      'stateCode', 'zipCode')]
        dems<-sapply(dems, mean, na.rm = TRUE)
        }
      return(dems)
      }
  output<-t(sapply(zipcodes, getZicode))
  if( pca == TRUE){
    pcs<-predict(preProcZipcodes, output)  
    colnames(pcs)<-c('zipCodeComp1', 'zipCodeComp2')
    output<-cbind(zipCode = zipcodes, pcs)
    
    stateTest<-round(mean(sapply(zipcodes, nchar), na.rm = TRUE),0) == 2
    if(stateTest == TRUE){
      colnames(output)[1]<-"stateCode" 
      }
  }
  return(output)
}


