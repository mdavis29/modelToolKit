#' @title APR DRG coder
#' @param mydata A vector of APR DRG Numners or Names
#' @param Severity an optional vector of integer APR serverity, same length of mydata
#' @return a data frame with aprDrgWeight, ALOS and CostOutThresh
#' @description recodes an APRDRG to APR wieght, mean Length of Stay and Cost outlier thresh
#' @author Matthew Davis
#' @export
#' @details APR DRG 2014 table
#'
codeAPRDRG<-function(mydata,Severity = NULL){
    output<-data.frame()
    test1 <- 3 >= round(mean(sapply(as.character(mydata), nchar), na.rm = TRUE),1)
    if(test1 == TRUE & is.null(Severity)){
      print('warning, no Severity given, so means of APR DRG returned')
      for(i in 1:(length(mydata))){
        temp<-apr[apr[,'APR.DRG'] == mydata[i],c(4:6)]
        m<-sapply(temp, mean, na.rm = TRUE)
        temp<-data.frame(aprDrgWeight = m[1], ALOS =m[2], CostOutThresh = m[3])
        output<-rbind(temp, output)
      }}
    if(test1 == TRUE & is.null(Severity) == FALSE ){
      for(i in 1:(length(mydata))){
        temp<-apr[apr[,'APR.DRG'] == mydata[i] &
                  apr[,'Severity'] == Severity[i],
                  c(4:6)]
        temp<-as.numeric(temp)
        temp<-data.frame(aprDrgWeight = temp[1], ALOS =temp[2], CostOutThresh = temp[3])
        output<-rbind(temp, output)
      }}
    if(test1 == FALSE & is.null(Severity)){
      print('warning, no Severity given, so means of APR DRG returned')
      for(i in 1:(length(mydata))){
        temp<-apr[apr[,'APR.DRG.Description'] == mydata[i],c(4:6)]
        m<-sapply(temp, mean, na.rm = TRUE)
        temp<-data.frame(aprDrgWeight = m[1], ALOS =m[2], CostOutThresh = m[3])
        output<-rbind(temp, output)
      }}
    if(test1 == FALSE & is.null(Severity) == FALSE ){
      for(i in 1:(length(mydata))){
        temp<-apr[apr[,'APR.DRG.Description'] == mydata[i] &
                    apr[,'Severity'] == Severity[i],
                    c(4:6)]
        temp<-as.numeric(temp)
        temp<-data.frame(aprDrgWeight = temp[1], ALOS =temp[2], CostOutThresh = temp[3])
        output<-rbind(temp, output)
      }}
    rownames(output)<-NULL
    return(output)}
