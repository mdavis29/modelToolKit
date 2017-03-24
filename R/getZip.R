#' @title get Zip
#' @param zipCodes a vector of vip codes
#' @param pca logical, whether or not to return principal components
#' @description looks up zip codes 2014 IRS tax data aggregated
#' @details returns a dataframe with demographics for each zipcode,
#' If the zipcode is not found, it returns the mean of all zipcodes
#' @author Matthew Davis
#' @export
getZip<-function(zipCodes, pca = FALSE){
  zc<-as.numeric(as.character(zipCodes))
  demCols<-colnames(irs)[!colnames(irs) %in%  c('zipCode', 'stateCode')]
  validTest<-!zc %in% c(0,00000,99999) &
    nchar(as.character(zc)) == 5 &
    !is.na(zc)
  avg<-sapply(irs[,demCols],
              mean,
              na.rm = TRUE)
  uniqueInputZips<-unique(zc[validTest])
  uniqueIrsZips<-unique(irs$zipCode)
  keepZips<-intersect(uniqueInputZips,uniqueIrsZips )

  irs.temp<-irs[irs$zipCode %in% keepZips, ]
  output<-NULL
  output.temp<-NULL
  for ( i in 1:(length(zc))){
    if(validTest[i] == TRUE){
    output.temp<-irs.temp[irs.temp$zipCode == zc[i],demCols][1,]
      }
    if(validTest[i] == FALSE | is.null(output.temp) ){
    output.temp<-avg
      }
    output<-rbind(output, output.temp)
    output.temp<-NULL
    }
  if(pca == TRUE){
    pc<-predict(preProc, output)
    colnames(pc)<-paste('zipCode', colnames(pc), sep = '')
    output<-cbind(output, pc)
    }
  return(output)
}
