# Load required libraries
library(MASS)  
library(ggplot2)  

# Load the dataset 
icecream <- read.csv('E:\\JESIN\\DOCUMENTS\\scma\\A4\\icecream.csv')

# Display the dataset to confirm it's loaded correctly
head(icecream)

# Extract the attributes for MDS (excluding the Brand column)
icecream_mds <- icecream[, -1]  

# Perform Multidimensional Scaling (MDS)
mds <- cmdscale(dist(icecream_mds))

# Plot the MDS results
plot_data <- data.frame(
  x = mds[, 1],  # X-axis coordinates
  y = mds[, 2],  # Y-axis coordinates
  brand = icecream$Brand  # Brand names
)

ggplot(plot_data, aes(x, y, label = brand)) +
  geom_point() +
  geom_text(size = 3, hjust = 0, vjust = 0) +
  labs(title = "Multidimensional Scaling (MDS) Plot of Ice Cream Brands") +
  theme_minimal()
