#' @title predict method for Kmeans
#' @return a data frame of ss errors per cluster, cluster classification and totalSSE
#' @author Matthew Davis
#' @import stats
#' @param fit kmeans model
#' @param newdata data frame with new data to assign clusters to
#' @param verbose print debugging output
#' @description builds a bunch of cluster models to find the optimum number of clusters
#' @details cluster reported back as a factor in the output data frame
#' @export
#' @exportClass predict 
predictKmeans<- function(fit, newdata, verbose = FALSE){
    cols<-colnames(fit$centers)
    if(!all(cols %in% colnames( newdata))){
      stop(paste(cols[!cols %in% colnames(newdata)], 'missing from newdata'))
    }
    newdata.m<-as.matrix(newdata[,cols])
    centers<-fit$centers
    if(verbose)print(centers)
    n<-nrow(centers)
    cores<-parallel::detectCores()    
    cores<-min(c(n, cores))
    cl<-makeCluster(cores, type = 'SOCK')
    registerDoParallel(cl)
    output<-foreach(i =1:n,
                    .packages = 'stats', 
                    .combine = 'cbind' ) %dopar%{
      as.matrix(apply(newdata.m,c(1), function(x)sum(x-centers[i,], na.rm = TRUE )^2 ), ncol = 1)
                    }
    stopCluster(cl)
    if(verbose)print(str(output))
    output<-as.data.frame(output)
    colnames(output)<-paste(paste('cluster', rownames(centers), sep = ''), 'SSE',sep  = '.')
    output$cluster<-apply(output, 1, which.min)
    if(verbose)print(head(output))
    output$totalSSE<-apply(output[, !colnames(output) %in% 'cluster'],1, sum)
    return(output)}