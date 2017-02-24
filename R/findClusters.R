#' @title Find the Optimum Kmeans Cluster Model
#' @param mydata list of models
#' @param n a data frame to test the model against
#' @param vtreatIt whether to scale data
#' @param seed set seed for randomization
#' @return a data frame of ss errors by algorythim types
#' @author Matthew Davis
#' @description builds a bunch of cluster models to find the optimum number of clusters
#' @details This predict UHC Work RVUS from Work RVUs
#' @export

findClusters<-function(mydata, n=10, seed = 2012, vtreatIt = FALSE){

      if(vtreatIt == TRUE){
        treatment<-designTreatmentsZ(mydata, colnames(mydata))
        mydata.treated<-prepare(treatment, mydata, pruneSig = 1)
        mydata.treated<-na.omit(mydata.treated)}

      if(vtreatIt == FALSE){
        mydata.treated <-mydata[,lapply(mydata, class) %in% c('numeric', 'integer')]}

      sse.Hartigan.Wong<-c()
      sse.Lloyd<-c()
      sse.Forgy<-c()
      sse.MacQueen<-c()
      set.seed(seed)
     for( i in 1:n){
          fit<-kmeans(mydata.treated, i, algorithm  = "Hartigan-Wong")
            sse.Hartigan.Wong<-append(sse.Hartigan.Wong, fit$tot.withinss)
          fit<-kmeans(mydata.treated, i, algorithm  = "Lloyd")
            sse.Lloyd<-append(sse.Lloyd, fit$tot.withinss)
          fit<-kmeans(mydata.treated, i, algorithm  = "Forgy")
            sse.Forgy<-append(sse.Forgy, fit$tot.withinss)
          fit<-kmeans(mydata.treated, i, algorithm  = "MacQueen")
            sse.MacQueen<-append(sse.MacQueen, fit$tot.withinss)
                  }
      plot(x = 1:n, y = sse.Hartigan.Wong,
              type = 'both',
              xlab = 'number of clusters',
              ylab = 'sum of squares error',
              log ='y',
              title = 'Kmeans Cluster Models')
          lines(x = 1:n, y = sse.Lloyd, type = 'both', col = 'orange')
          lines(x = 1:n, y = sse.Forgy, type = 'both', col = 'red')
          lines(x = 1:n, y = sse.MacQueen, type = 'both', col = 'blue')
          legend('topright', fill = c('black', 'orange', 'red','blue' ),
                    legend = c('Hartigan-Wong','Lloyd','Forgy','MacQueen'),
                    title = 'Cluster Model')
          output<-data.frame( NumClusters = 1:n,
                              Hartigan.Wong = sse.Hartigan.Wong,
                              Lloyd = sse.Lloyd,
                              Forgy = sse.Forgy,
                              MaQueen =sse.MacQueen)
            print(paste('seed',seed))
            return(output)}



