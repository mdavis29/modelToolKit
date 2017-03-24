#' @title get Zip
#' @param zipCodes a vector of vip codes
#' @param pca logical, whether or not to return principal components
#' @description looks up zip codes 2014 IRS tax data aggregated
#' @details returns a dataframe with demographics for each zipcode,
#' If the zipcode is not found, it returns the mean of all zipcodes
#' @author Matthew Davis
#' @export
getZip<-function(zipCodes, pca = FALSE){
  numCols <- colnames(irs)[!colnames(irs) %in% c('stateCode', 'zipCode')]
  avg<-sapply(irs[,numCols ], median)
  ucodes<-unique(as.numeric(as.character(zipCodes)))
  vcodes<-ucodes[!ucodes %in% c(0,99999) & !is.na(ucodes) ]
  lookupCodes<-vcodes[vcodes %in% irs$zipCode ]
  trimmedIrs<-irs[irs$zipCode %in% lookupCodes, ]
  tempDF<-NULL
  for (i in 1:(length(lookupCodes))){
    temp <- head(trimmedIrs[trimmedIrs$zipCode %in%  lookupCodes[i],],1)
    tempDF <-rbind(tempDF, temp)
    }
  lookupDF<-data.frame(zipCode = zipCodes )
  output<-merge(lookupDF,  tempDF, all.x = TRUE)
  k <- 0
  for ( j in 1:(nrow(output))){
    if (sum(is.na(output[j,]))>2){
        output[j,numCols]<-avg
        k<-k+1
        }
    }
  if(pca == TRUE){
    pc <- predict(preProc,output)
    colnames(pc)<-paste('zc', colnames(pc), sep = '')
    output<-cbind(output, pc)
    }

  print(paste(k,'misses'))
  return(output)
}
