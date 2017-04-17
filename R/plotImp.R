#' @title plotImp
#' @param fit a genaric model that caret varImp works on
#' @param plotIt Whether or not to plot the varImp
#' @description looks up zip codes 2014 IRS tax data aggregated
#' @details returns a dataframe with demographics for each zipcode,
#' If the zipcode is not found, it returns the mean of all zipcodes
#' @author Matthew Davis
#' @export

plotImp<-function(fit, plotIt = TRUE){
  imp<-data.frame(caret::varImp(fit))
  newImp<-imp[order(imp$Overall),]
  names(newImp) <- rownames(imp)[order(imp$Overall, decreasing = TRUE)]
  if(plotIt == TRUE){
    par(mar = c(2,8,2,2))
    barplot(newImp, horiz = TRUE, cex.names = .8, las = 1)
    }
  return(newImp)  
  }