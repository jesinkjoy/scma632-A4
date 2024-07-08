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
packages <- c("dplyr", "psych", "tidyr", "GPArotation", "FactoMineR", "factoextra", "pheatmap")

# Call the function
install_and_load(packages)

survey_df <- read.csv('E:\\JESIN\\DOCUMENTS\\scma\\A4\\Survey.csv', header = TRUE) 
dim(survey_df) 
names(survey_df) 
head(survey_df) 
str(survey_df)

#is.na(survey_df) 
sum(is.na(survey_df)) 
sur_int = survey_df[, 18:46] 
str(sur_int) 
dim(sur_int) 

# Performing PCA using GPArotation
library(GPArotation) 
pca_1 <- principal(sur_int, 5, n.obs = 70, rotate = "promax") 
print(pca_1)

# Performing PCA using FactoMineR
library(FactoMineR)
pca_2 <- PCA(sur_int, scale.unit = TRUE) 
summary(pca_2)

# Using factoextra to plot the PCA biplot
library(factoextra)
fviz_pca_biplot(pca_2, repel = TRUE)

# Factor Analysis 
factor_analysis<-fa(sur_int,nfactors = 4,rotate = "varimax") 
names(factor_analysis) 
print(factor_analysis$loadings,reorder=TRUE) 
fa.diagram(factor_analysis) 
print(factor_analysis$communality) 
print(factor_analysis$scores) 
