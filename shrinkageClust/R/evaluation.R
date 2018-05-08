
#' Clustering result evaluation
#'
#' Quantitatively evaluate a clustering result
#'
#' @param clust A list (n*1) that indicate the predicted cluster membership of each example
#' @param label A list (n*1) that indicate the ground-truth cluster membership of each example
#' @return \item{NMI}{Normalized mutual information}
#' @return \item{RI}{Rand index}
#' @return \item{F1}{F1 score}
#' @details This function quantitatively assess the accuracy of a clustering assignment by calculating three metrics, which are normalized mutual information (NMI), Rand index, and F1 score. Note, To calculate NMI, the R package "NMI" is required to be installed first.
#' @references Hu, Chenyue W., Hanyang Li, and Amina A. Qutub. “Shrinkage Clustering: A Fast and Size-Constrained Clustering Algorithm for Biomedical Applications.” BMC Bioinformatics 19 (2018): 19. PMC. Web. 7 May 2018.
#' @references Manning CD, Raghavan P, Schütze H, et al. Introduction to information retrieval, vol. 1. Cambridge: Cambridge university press; 2008.
#' @export
#'
#' @example SuperCluster_example.R




evaluation <- function(clust, label) {
  library(NMI)
  id = 1:length(label)
  classes_id = cbind(id, label)
  clusters_id = cbind(id,clust)
  #NMI
  NMI = NMI(clusters_id,classes_id)

  t<-table(label,clust)
  #find column sum of confusion matrix
  cols<-as.numeric(colSums(t))
  TPFP_i = 0;
  for(i in 1:length(cols)) {
    TPFP_i[i] = choose(cols[i],2)
  }
  TPFP<-sum(TPFP_i)
  #position of all entries > 1 (in confusion table)
  pos <- which(t > 1)
  TP_pair<-t[pos]
  TP_i = 0;
  for(i in 1:length(TP_pair)) {
    TP_i[i] = choose(TP_pair[i],2)
  }
  TP<-sum(TP_i)#TP
  FP<-TPFP-TP#FP

  rows<-as.numeric(rowSums(t))
  TPFN_i = 0;
  for(i in 1:length(rows)) {
    TPFN_i[i] = choose(rows[i],2)
  }
  TPFN<-sum(TPFN_i)

  FN<-TPFN-TP#FN
  TN<-choose(sum(t),2)-TP-FP-FN#TN


  #Rand Index(RI)
  RI <- (TP+TN)/(TP+FP+FN+TN)
  #F1 Score
  P <- TP/(TP+FP)
  R <- TP/(TP+FN)
  F1 <- (2*P*R)/(P+R)



  return(list(NMI=NMI,RI=RI,F1=F1))


}
