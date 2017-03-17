#' @title Find the Best single hidden layer Autoencoder
#' @param trainData a data frame or matrix with numeric columns
#' @param maxHiddenLayers currently not implimented
#' @param maxNodes max number of nodes in hidden layer
#' @param max.int max number of iterations
#' @param n.models number of models to build
#' @param rescale logical rescales the data
#' @description this creates a number of autoencoders and compairs sse to find optimum
#' @return \itemize{
#'      \item model is the best fit model i
#'      \item a.grid the grid used to build models that were compaired
#'      \item sse vector of sum squared erros
#'      \item BestParams df of the best tuning parameters for the auto encoder
#'      \item colNames used to for the autoencoder (any character cols are filtered out)}
#' @author Matthew Davis
#' @details finds the best single hidden layer autoencoder
#' @export
findBestAutoEncoder<-function(trainData,
                              maxHiddenLayers = 1,
                              maxNodes = 2,
                              n.models = 40,
                              rescale = TRUE,
                              max.int = 1000){
      td<-trainData
      td<-td[, apply(td, 2, class) %in% c('numeric', 'integer')]
      td<-as.matrix( td)

          a.grid<-data.frame(expand.grid(
              hidenNodes  = seq(1,maxNodes),
              rhos = seq(0.01, .6, by = 0.05) ,
              epsilons = seq(0.01, .2, by = 0.02),
              opt.meths =  c("BFGS"),
              unit.types = c("logistic", "tanh"),
              lambdas = seq(1e-20, 1e-2, by= 1e-3),
              betas = seq(1e-20, 1e-2, by= 1e-3)))

          si<-sample(1:(nrow(a.grid)), n.models,replace = FALSE)
          sse<-c()
            for( i in 1:n.models){
              print(i)
              fit<-autoencode(X.train = td,
                    X.test = NULL,
                    nl = 3,
                    N.hidden = a.grid$hidenNodes[si[i]],
                    epsilon = a.grid$epsilons[si[i]],
                    optim.method = as.character(a.grid$opt.meths[si[i]]),
                    unit.type = as.character(a.grid$unit.types[si[i]]),
                    lambda = a.grid$lambdas[si[i]],
                    beta = a.grid$betas[si[i]],
                    max.iterations = 2000,
                    rho = a.grid$rhos[si[i]],
                    rescale.flag =rescale )
            sse<-append(sse, fit$mean.error.training.set)
                                  }
                bestIndex<-si[sse == min(sse)][1]
                print('now building best model')
                fit<-autoencode(X.train = td,
                                X.test = NULL,
                                nl = 3,
                                N.hidden = a.grid$hidenNodes[bestIndex],
                                epsilon = a.grid$epsilons[bestIndex],
                                optim.method = as.character(a.grid$opt.meths[bestIndex]),
                                unit.type = as.character(a.grid$unit.types[bestIndex]),
                                lambda = a.grid$lambdas[bestIndex],
                                beta = a.grid$betas[bestIndex],
                                max.iterations = max.int,
                                rho = a.grid$rhos[bestIndex],
                                rescale.flag =rescale )
                  returnList<-list()
                  returnList$model<-fit
                  returnList$a.grid<-a.grid[si,]
                  returnList$sse<-sse
                  returnList$BestParams<-a.grid[bestIndex,]
                  returnList$colNames<-colnames(td)
                    return(returnList)}

