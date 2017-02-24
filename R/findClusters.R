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
      sseb.Hartigan.Wong<-c()
      sseb.Lloyd<-c()
      sseb.Forgy<-c()
      sseb.MacQueen<-c()
      set.seed(seed)

      for( i in 1:n){
          fit<-kmeans(mydata.treated, i, algorithm  = "Hartigan-Wong")
            sse.Hartigan.Wong<-append(sse.Hartigan.Wong, fit$tot.withinss)
            sseb.Hartigan.Wong<-append(sseb.Hartigan.Wong, fit$betweenss)
          fit<-kmeans(mydata.treated, i, algorithm  = "Lloyd")
            sse.Lloyd<-append(sse.Lloyd, fit$tot.withinss)
            sseb.Lloyd<-append(sseb.Lloyd, fit$betweenss)
          fit<-kmeans(mydata.treated, i, algorithm  = "Forgy")
            sse.Forgy<-append(sse.Forgy, fit$tot.withinss)
            sseb.Forgy<-append(sseb.Forgy, fit$betweenss)
          fit<-kmeans(mydata.treated, i, algorithm  = "MacQueen")
            sse.MacQueen<-append(sse.MacQueen, fit$tot.withinss)
            sseb.MacQueen<-append(sseb.MacQueen, fit$betweenss)
                  }
      par(mfrow =c(1,2), mar = c(2,1,1,1))
      plot(x = 1:n, y = sse.Hartigan.Wong,
              type = 'b',
              xlab = 'number of clusters',
              ylab = 'sum of squares error')
          lines(x = 1:n, y = sse.Lloyd, type = 'b', col = 'orange')
          lines(x = 1:n, y = sse.Forgy, type = 'b', col = 'red')
          lines(x = 1:n, y = sse.MacQueen, type = 'b', col = 'blue')
          legend('topright', fill = c('black', 'orange', 'red','blue' ),cex = .8,
                    legend = c('Hartigan-Wong','Lloyd','Forgy','MacQueen'),
                    title = 'SS Error within Clusters')

      plot(x = 1:n, y = sseb.Hartigan.Wong,
               type = 'b',
               xlab = 'number of clusters',
               ylab = 'sum of squares error')

          lines(x = 1:n, y = sseb.Lloyd, type = 'b', col = 'orange')
          lines(x = 1:n, y = sseb.Forgy, type = 'b', col = 'red')
          lines(x = 1:n, y = sseb.MacQueen, type = 'b', col = 'blue')
          legend('bottomright', fill = c('black', 'orange', 'red','blue' ),cex = .8,
                 legend = c('Hartigan-Wong','Lloyd','Forgy','MacQueen'),
                 title = 'SS Error Between Clusters')

          output<-data.frame( NumClusters = 1:n,
                              Hartigan.Wong = sse.Hartigan.Wong,
                              Lloyd = sse.Lloyd,
                              Forgy = sse.Forgy,
                              MaQueen =sse.MacQueen)
            print(paste('seed',seed))
            return(output)}
findClusters(mtcars)
