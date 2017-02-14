#' @title  modelToolsMUSC
#' @description This is package of tools to help with basic functions such as finding a good autoencoder. The package provides:
#'  \itemize{
#'    \item predictVtreat predicted a model of class 'train' with vtreat preprocessing step
#'    \item fit.test preforms a postResample on a model (class 'train') on a new data, with vtreat
#'    \item fitTest.env runs a postResample test on a group of models saved in an enviroment
#'    \item findBestAutoEncoder finds the best single layer autoencoder
#'    \item findClusters finds the optimum number of clusters for a cluster model
#'    \item cooksDistanceOptimizer finds the bets glm model by removing points by their cooks distance
#'   }
#' @docType package
#' @name modelToolsMUSC
NULL

