#' Similarity matrix generator
#'
#' Generate a similarit matrix used by Shrinkage Clustering
#'
#' @param data A data matrix (n*k) with n examples and k dimensions
#' @return \item{S}{The similarity matrix (n*n) of the input data set}
#' @references Hu, Chenyue W., Hanyang Li, and Amina A. Qutub. “Shrinkage Clustering: A Fast and Size-Constrained Clustering Algorithm for Biomedical Applications.” BMC Bioinformatics 19 (2018): 19. PMC. Web. 7 May 2018.
#'
#' @export
#'
#' @example SuperCluster_example.R



simiMatrix <- function(data) {
  # compute distance matrix
  d = as.matrix(dist(data,upper=T,diag=T))
  # compute parameter beta and similarity matrix
  beta = 1/(mean(d/sd(d))^2)
  S = exp(-beta*(d^2/sd(d)^2))
  return(S)
}

