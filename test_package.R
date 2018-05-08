# install shrinkageClust pakckage and test it on BCWD dataset

path_to_file = '/Users/hanyangli/Desktop/Qutub\ Lab/Shrinkage\ Clustering/R\ Package/shrinkageClust_Package/shrinkageClust_0.1.0.tar.gz'
install.packages(path_to_file, repos = NULL, type="source")

# load BCWD dataset (569 samples, 30 dimensions, 2 clusters)
data("bcwd_data")
label = data$label
data = data[, c(1:30)]

# compute similarity matrix
S = simiMatrix(data)

# view distribution of similarity scores in S
hist(S)

# run shrinkage clustering
set.seed(10)
clust = SuperCluster(s=S,w=100,k=20,iter=500,random=1)
clust_membership = clust$c[,1]

# quantitatively assess the accuracy of the clustering result
eval_scores = evaluation(clust_membership, label)
eval_scores$NMI # Normalized mutual information
eval_scores$RI # Rand index
eval_scores$F1 #F1 score

# generate a PCA plot to visualize the clustering result
scplot(data, clust_membership)

## Note: scplot is based on ggbiplot package. Use the command below to install ggbiplot
# library(devtools)
# install_github('ggbiplot','vqv')
# library(ggbiplot)
