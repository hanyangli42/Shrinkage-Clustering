#' Breast Cancer Wisconsin Dataset (BCWD)
#'
#' This dataset is a dataframe of size 569 x 31. Each row is an observation. Column 1 to 30 are features of the observations. Colum 31 is the ground-truth label of each observation.
#'
#' @name bcwd_data
#' @docType data
#' @details The BCWD dataset is a preprocessed dataset originally from a study of breast cancer diagnostics (Street et al., 1993 and Mangasarian et al., 1995). It is used as a case study in the original paper of the Shrinkage Clustering algorithm (Hu et al., 2018).  The BCWD dataset contains 569 breast cancer samples (357 benign and 212 malignant) with 30 characteristic features computed from a digitized image of a fine needle aspirate (FNA) of a breast mass. The dataset is available on the UCI machine learning repository (Bache and Lichman, 2013) and is one of the most popularly tested dataset for clustering and classification.
#' @references Street WN, Wolberg WH, Mangasarian OL. Nuclear feature extraction for breast tumor diagnosis. In: IS&T/SPIE’s Symposium on Electronic Imaging: Science and Technology. San Jose: International Society for Optics and Photonics; 1993. p. 861–70.
#' @references Mangasarian OL, Street WN, Wolberg WH. Breast cancer diagnosis and prognosis via linear programming. Oper Res. 1995;43(4):570–7.
#' @references Hu, Chenyue W., Hanyang Li, and Amina A. Qutub. “Shrinkage Clustering: A Fast and Size-Constrained Clustering Algorithm for Biomedical Applications.” BMC Bioinformatics 19 (2018): 19. PMC. Web. 7 May 2018.
#' @references 5.	Bache K, Lichman M. UCI Machine Learning Repository: University of California, Irvine, School of Information and Computer Sciences; 2013. http://archive.ics.uci.edu/ml.
#' @keywords data
#' @examples
#' data("bcwd_data")
#' label = data$label
#' data = data[, c(1:30)]

NULL
