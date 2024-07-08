# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("cluster", "FactoMineR", "factoextra", "pheatmap")

install_and_load(packages)
survey_df<-read.csv('E:\\JESIN\\DOCUMENTS\\scma\\A4\\Survey.csv',header=TRUE)
sur_int=survey_df[,18:46] 


# Cluster analysis to characterize respondents based on background variables. 
library(cluster) 
library(factoextra) 
show(sur_int)

# Determining Optimal Number of Clusters with Gap Statistic
fviz_nbclust(sur_int,kmeans,method = "gap_stat")

# Performing k-means Clustering
set.seed(123) 
km.res<-kmeans(sur_int,4,nstart = 25) 

# Visualizing k-means Clustering Results
fviz_cluster(km.res,data=sur_int,palette="jco", ggtheme = theme_minimal()) 

# Hierarchical Clustering (Dendrogram)
res.hc <- hclust(dist(sur_int), method = "ward.D2") 
fviz_dend(res.hc,cex=0.5,k=4,palette = "jco")

#  Heatmap of Clustered Data
library(pheatmap) 
pheatmap(t(sur_int),cutree_cols = 4) 