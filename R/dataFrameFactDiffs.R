#' @title data frame Factor Differences
#' @param df1 a data frame
#' @param df2 a data frame to compair with
#' @param verbose print debugging output
#' @description takes two data frames and finds differences in the factor distrobutions
#' @return a data frame with pvals from McNemars test, xMean, yMean is the % that a level appears
#' @export
dataFrameFactDiffs<-function(df1,df2, verbose = FALSE){
  missingFactorCols<-NULL
  df1FactCols<-colnames(df1)[lapply(df1, class) == 'factor']
  df2FactCols<-colnames(df2)[lapply(df2, class) == 'factor']
  commonFactorLevs<-union( df1FactCols, df2FactCols)
  missingCols<-setdiff(intersect( df1FactCols, df2FactCols) , commonFactorLevs)
  if(verbose)print(paste('commonLevs', paste(commonFactorLevs, collapse = ','),sep = ':'))
  if(verbose)print(paste('MissingLevs', paste( missingCols, collapse = ','),sep = ':'))
  n<-length(commonFactorLevs)
  output<-NULL
  for ( i in 1:n){
    if(verbose)print(paste('now getting',commonFactorLevs[i] ))
    tempData<-factorDiffs(df1[, commonFactorLevs[i]],
                          df2[,commonFactorLevs[i]])
    tempData$colName<-commonFactorLevs[i]
    output<-rbind(output, tempData)
  }
  return(output)
}

