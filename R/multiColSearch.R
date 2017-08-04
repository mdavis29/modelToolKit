#' @title Multiple Column Search 
#' @param mydata a data frame
#' @param val a character vector of values to search for in columns
#' @param cols a character vector of column names in mydata to search
#' @param returnMat logical return a matrix of logical columns
#' @param all logical whether to return TRUE for all matches , if FALSE than any Matchs (ignored when returnMat)
#' @return a  logical vector or matrix of logical vectors
#' @author Matthew Davis
#' @description searches for values in multiple columns 
#' @details This predict UHC Work RVUS from Work RVUs
#' @export

multiColSearch<-function(mydata, val, cols = NULL, returnMat = FALSE, all = TRUE){
  if(is.null(cols)){cols<-colnames(mydata)}
  if(!all(cols %in% colnames(mydata))){
    stop(paste(cols[!cols %in% colnames(mydata)], 'missing from mydata'))
  }
  n<-length(cols)
  cores<-parallel::detectCores()
  cores<-min(c(n, cores))
  cl<-makeCluster(cores)
  registerDoParallel(cl)
  output<-foreach(i = 1:n, .combine = 'cbind') %dopar% {
    as.character(mydata[,cols[i]]) %in% as.character(val)
  } 
  stopCluster(cl)
  if(!is.null(ncol(output))){
    colnames(output)<-cols
    if(!returnMat & all ){output<-apply(output, c(1), all)}
    if(!returnMat & !all){output<-apply(output, c(1), any)}
  }
    return(output) 
}
