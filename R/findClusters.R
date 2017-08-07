#' @title Find the Optimum Kmeans Cluster Model
#' @param mydata list of models
#' @param n a data frame to test the model against
#' @param seed set seed for randomization
#' @param plotIt whether to plot solution 
#' @param verbose whether to print debugging output
#' @return a data frame of ss errors by algorythim types
#' @author Matthew Davis
#' @description builds a bunch of cluster models to find the optimum number of clusters
#' @details This predict UHC Work RVUS from Work RVUs
#' @export

findClusters<-function(mydata, n=10, seed = 2012,plotIt = TRUE, verbose = FALSE){
  set.seed(seed)
  mydata<-as.matrix(mydata[,apply(mydata, c(2), class) %in% c('integer', 'numeric')])
  cores<-parallel::detectCores()    
  cores<-if(cores>n)cores<-n
  cl<-makeCluster(cores, type = 'SOCK')
  registerDoParallel(cl)
  output <- foreach( i = 1:n, .inorder = TRUE,.combine = 'rbind') %dopar%{
      fit<-kmeans(mydata, i)
      tempOut<-list()
      tempOut$within<-fit$tot.withinss
      tempOut$between<-fit$betweenss
      tempOut$k<-nrow(fit$centers)
      unlist(tempOut)
  }
  stopCluster(cl)
  rownames(output)<-paste('NumClusters', 1:n, sep = '')
  if(verbose)print(head(output))
  if(plotIt){
    plot( y = output[,1],
          x = output[,3], type = 'l', 
          ylab = 'scaled error', 
          xlab = 'Cluster of Numbers',
          main = 'Optimum Number of Clusters')
    text( y = output[,1],x = output[,3], labels = output[,3])
    lines(y = output[,2], x = output[,3], col = 'blue')
    text( y = output[,2],x = output[,3], labels = output[,3], col ='blue')
    legend('right', fill = c('black', 'blue'), 
           legend = c('SSE Within Clusters', 'SSE Between Clusters'))
    
    
  }
  return(output)}

