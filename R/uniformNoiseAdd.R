#' @title Unifrom Noise Adder
#' @param mydata a dataframe or vector
#' @param percent pecent of noise to add from 0 to 1
#' @return vector or performance metrics for the model
#' @description used for adding an amount of noise to a data frame
#' @author Matthew Davis
#' @details uses min*percet and max*percent to run runif
#' @export
uniformNoiseAdd<-function(mydata,percent = 0.1 ,seed = 2016){
  set.seed(seed)
  if(class(mydata) %in% c('numeric', 'integer')){
      maxVal<-max(mydata, na.rm = TRUE)*percent
      minVal<-min(mydata, na.rm = TRUE)*percent
      x<-sample(c(-1,1),length(mydata),replace = TRUE)
      mydata<-mydata + x*runif(length(mydata), min = minVal, max = maxVal)
      }
  if(class(mydata) %in% c('data.frame', 'matrix')){
    for(i in 1:(ncol(mydata))){
        x<-sample(c(-1,1),length(mydata[,i]),replace = TRUE)
        if (class(mydata[,i]) %in% c('numeric', 'integer')){
          maxVal<-max(mydata[,i], na.rm = TRUE)*percent
          minVal<-min(mydata[,i], na.rm = TRUE)*percent
          mydata[,i]<-mydata[,i] + x*runif(length(mydata[,i]), min = minVal, max = maxVal)
            }
          }
        }
    return(mydata)
  }