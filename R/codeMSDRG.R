#' @title MSDRG to LOS and Weight
#
#' @param mydata a vector of MSDRG Names or MSDRG Numbers
#' @return a data frame with msDRG weight, GMLOS, and AMLOS
#' @description recodes an msDRG to the CMS-1655-P-FY-2017-TABLE-5-_Weight_File_FR
#' @author Matthew Davis
#' @export
#' @details GMLOS = geometric length of stay, AMLOS arithemtic length of stay
#'
codeMSDRG<-function(mydata){
  output<-data.frame()

test1 <- 3 >= round(mean(sapply(as.character(mydata), nchar), na.rm = TRUE),1)
  if (test1 == FALSE){
    for ( i in 1:(length(mydata))){
      temp<-ms[ms[,"MS.DRG.Title"] == mydata[i], ][1,c(7,8,9)]
      colnames(temp)<-c('msDrgWeight', 'gmlos', 'amlos')
      output<-rbind(output, temp)
      }
  }
  if (test1 == TRUE){
    for ( i in 1:(length(mydata))){
      temp<-ms[ms[,"MS.DRG"] == mydata[i], ][1,c(7,8,9)]
      colnames(temp)<-c('msDrgWeight', 'gmlos', 'amlos')
      output<-rbind(output, temp)
    }
  }
      return(output)
  }

