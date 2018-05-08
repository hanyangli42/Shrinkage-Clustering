#' Shrinkage Clustering membership plot
#'
#' Display a clustering assignment result  on a principal component analysis (PCA) plot
#'
#' @param data A data matrix (n*k) with n examples and k dimensions
#' @param clust_membership A list (n*1) that indicate the cluster membership of each example
#' @details The function uses library ggbiplot to generate the clustering assignment PCA plot. It takes the original data matrix and a clustering result as input. A pdf file of the plot will be generated.
#' @references Hu, Chenyue W., Hanyang Li, and Amina A. Qutub. “Shrinkage Clustering: A Fast and Size-Constrained Clustering Algorithm for Biomedical Applications.” BMC Bioinformatics 19 (2018): 19. PMC. Web. 7 May 2018.
#'
#' @export
#'
#' @example SuperCluster_example.R

scplot <- function(data, clust_membership) {
  library(ggbiplot)
  pc<-prcomp(data,scale=T)
  pdf('clustering_result_pca.pdf',width=5,height=4)
  out<-ggbiplot(pc,groups=factor(clust_membership), ellipse=T,var.axes = F)+theme_bw()
  print(out)
  dev.off()
}
