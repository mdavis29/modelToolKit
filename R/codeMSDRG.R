#' @title MSDRG to LOS and Weight
#' @param mydata a vector of MSDRG Names or MSDRG Numbers
#' @return a data frame with msDRG weight, GMLOS, and AMLOS
#' @description recodes an msDRG to the CMS-1655-P-FY-2017-TABLE-5-_Weight_File_FR
#' @author Matthew Davis
#' @export
#' @details GMLOS = geometric length of stay, AMLOS arithemtic length of stay
#'
codeMSDRG<-function(mydata){
  avg<-sapply(ms[, 7:9], median, na.rm = TRUE)
  lookupDF<-NULL
  uniqueDRG<-unique(mydata)
  haveDRGs<-as.character(ms[,'MS.DRG.Title'])
  lookupDRGs<-uniqueDRG[uniqueDRG %in% haveDRGs ]
  trimedTable<-ms[haveDRGs %in% lookupDRGs, ]
  trimedHaveDRG<-as.character(trimedTable[,'MS.DRG.Title' ])
  for ( i in 1:(length( lookupDRGs))){
    temp<-head(trimedTable[trimedHaveDRG == lookupDRGs[i],6:9],1)
    lookupDF<-rbind(lookupDF, temp)
    }
  
  mydataDF<-data.frame(MS.DRG.Title = mydata)
  output<-merge(mydataDF,lookupDF,all.x = TRUE  )

  print(paste( sum(is.na(output$Weights)), 'misses'))
  output$Weights[is.na(output$Weights)]<-avg[1]
  output$Geometric.mean.LOS [is.na(output$Geometric.mean.LOS)]<-avg[2]
  output$Arithmetic.mean.LOS[is.na(output$Arithmetic.mean.LOS)]<-avg[3]
  return(output)
  }


