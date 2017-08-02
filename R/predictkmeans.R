#' @title predict method for Kmeans
#' @param fit kmeans class object
#' @param newData data frame of new data
#' @return a data frame of ss errors per cluster, cluster classification and totalSSE
#' @author Matthew Davis
#' @description builds a bunch of cluster models to find the optimum number of clusters
#' @details cluster reported back as a factor in the output data frame
#' @export

predict.kmeans<-function(fit, newData){
  cols<-colnames(fit$centers)
  if(!all(cols %in% colnames( newData))){
    stop(paste(cols[!cols %in% colnames(mydata)], 'missing from mydata'))
  }
  mydata<-as.matrix(newData[,cols])
  centers<-fit$centers
  n<-nrow(centers)
  cores<-parallel::detectCores()    
  cores<-min(c(n, cores))
  cl<-makeCluster(cores, type = 'SOCK')
  registerDoParallel(cl)
  output<-foreach(i =1:n,
                  .packages = 'stats', 
                  .combine = 'cbind' ) %dopar%{
    apply(mydata,c(1), function(x)sum(x-centers[i,])^2 )
                  }
  stopCluster(cl)
  output<-as.data.frame(output)
  colnames(output)<-paste(paste('cluster', rownames(centers), sep = ''), 'SSE',sep  = '.')
  output$cluster<-as.factor(apply(output, 1, which.min))
  output$totalSSE<-apply(output,1, sum)
  return(output)}


