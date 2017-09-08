#' @title  Most Likely Factor
#' @author Matthew Davis
#' @param factorVar a factor variable
#' @param nFacts number of of factors to return
#' @return a vector of frequencies of the most frequent Classes
#' @description finds the most frequent factor, or if number finds the mode
#' 
mostLikelyFact<-function(factorVar, nFacts = 1){
  factorVar<-as.factor(factorVar)
  nFacts<-min(nFacts, length(levels(factorVar)))
  output<-table(factorVar)[order(table(factorVar), decreasing = TRUE)]
  return(head(output, nFacts))
}