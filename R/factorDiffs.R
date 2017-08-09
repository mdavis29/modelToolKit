#' @title factor Differences
#' @param x a factor vector
#' @param y a factor vector
#' @param reOrder whether to re order by the p value 
#' @param verbose print debugging output
#' @description takes two factor vectors and preforms McNemar test and returns a p value for every factor level
#' @return a vector of p values for each factor levels
#' @export
factorDiffs<-function(x, y, reOrder = TRUE, verbose = FALSE){
  missingLevels<-NULL
  commonLevels<-union(levels(y), levels(x))
  missingLevels<-setdiff(intersect(levels(x), levels(y)) , commonLevels)
  n<-length(commonLevels)
  if(verbose)print(paste('commonLevs', paste(commonLevels,collapse = ',' ), ':'))
  if(verbose)print(paste('missingLevels', paste(missingLevels,collapse = ',' ), ':'))
  if(verbose & reOrder & length(missingLevels) == 0 )print('warning cannot re order do to missing fact levels')
  cores<-parallel::detectCores()
  cores<-min(c(cores, n))
  if(verbose)print(paste('using cores:', cores))
  cl<-makeCluster(cores)
  registerDoParallel(cl)
  output<-foreach(i = 1:n,.combine = 'rbind', .packages = 'stats') %dopar%{
   tempMatrix<-matrix(NA, ncol = 2, nrow = 2)
   colnames(tempMatrix)<-c('present', 'absent')
   rownames(tempMatrix)<-c('present', 'absent')
   tempX<- x == commonLevels[i]
   tempY<- y == commonLevels[i]
   tempMatrix[1,1] <-sum(tempX & tempY)
   tempMatrix[1,2]<-sum(tempX & !tempY)
   tempMatrix[2,1]<-sum(!tempX & tempY)
   tempMatrix[2,2]<-sum(!tempX & !tempY)
   test<-mcnemar.test(tempMatrix)
   tempOutput<-data.frame(lev =commonLevels[i],
                          pval = as.numeric(test$p.value),
                          xMean = sum(tempX)/length(tempX),
                          yMean = sum(tempY)/length(tempY))

   tempOutput
  }
  stopCluster(cl)
  if(reOrder){
    if(verbose)print('re Ordering')
    output<-output[order(output[, 'pval']),]
  }
  return(output)
}



