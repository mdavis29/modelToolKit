#' @title Find the Optimum Kmeans Cluster Model
#' @param df1 a data frame
#' @param df2 a data frame 
#' @param verbose print debugging output
#' @return a data frame of ss errors by algorythim types
#' @author Matthew Davis
#' @description builds a bunch of cluster models to find the optimum number of clusters
#' @details This predict UHC Work RVUS from Work RVUs
#' @export
find2dfDiffs<-function(df1, df2, verbose = TRUE){
  useCols1<-colnames(df1)[lapply(df1, class) %in% c('numeric', 'integer')]
  useCols2<-colnames(df2)[lapply(df2, class) %in% c('numeric', 'integer')]
  keepCols<-intersect(useCols1, useCols2)
  if(verbose)print(keepCols)
  n<-length(keepCols)
  ##set up parallel frame work
  cores<-parallel::detectCores()    
  cores<-min(n, cores)
  cl<-makeCluster(cores, type = 'SOCK')
  registerDoParallel(cl)
  if(verbose)print(paste('using cores : ', cores))
  output <- foreach( i = 1:n, .inorder = TRUE,.combine = 'rbind') %dopar%{
      tempOut<-data.frame(pval = NA, 
                          meanDf1 = NA, 
                          meanDf2 = NA,
                          normDf1 = NA, 
                          normDf2 = NA )
      temp1<-na.omit(df1[, keepCols[i]])
      if(verbose)print(keepCols[i])
      if(verbose)print(head(temp1))
      temp2<-na.omit(df2[, keepCols[i]])
      if(verbose)print(head(temp1))
      if(length(unique(temp1)) > 1 & 
          length(unique(temp2)) > 1 & 
          length(temp1) > 3 &
          length(temp2) > 4){
        test<-try(t.test(temp1, temp2))
        if(class(test) == 'htest'){
          tempOut$pval = test$p.value
          tempOut$meanDf1<-test$estimate[1]
          tempOut$meanDf2<-test$estimate[2]
          if(length(unique(temp1))>3 ){
            tempOut$normDf1<-shapiro.test(temp1)$p.value
          }
          if(length(unique(temp2))>3 ){
            tempOut$normDf2<-shapiro.test(temp2)$p.value
          }
        }
      }
      tempOut
  }
  stopCluster(cl)    
  if(verbose)print(paste('dim output',paste( dim(output), collapse = ',')))
  rownames(output)<-keepCols  
  return(output)
}  