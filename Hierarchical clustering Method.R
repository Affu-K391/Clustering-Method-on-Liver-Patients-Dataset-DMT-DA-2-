library(ggplot2)
library(dendextend)

# Define the data
Age <- c(65, 62, 58, 72, 46, 26, 29, 17, 55, 57, 72, 64, 74, 61, 25, 38, 33, 40, 40, 51, 51, 62, 40, 63, 34, 34, 34, 20, 84, 57)
Gender <- c("Female", "Male", "Male", "Male", "Male", "Male", "Female", "Female", "Male", "Male", "Male", "Male", "Male", "Female", "Male", "Male", "Male", "Male", "Female", "Female", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Female", "Male", "Male")
Total_Protiens <- c(6.8, 7.5, 6.8, 7.3, 7.6, 7, 6.7, 7.4, 6.8, 5.9, 7.4, 7, 8.1, 5.8, 5.5, 7.6, 7.3, 6.8, 6.8, 7.3, 7, 6.4, 4.3, 6, 5, 7.2, 7.2, 5.5, 6, 6)
Albumin <- c(3.3, 3.2, 3.4, 2.4, 4.4, 3.5, 3.6, 4.1, 3.4, 2.7, 3, 3.4, 4.1, 2.7, 2.3, 4.4, 3.5, 3.1, 3.1, 2.6, 2.4, 3.1, 1.6, 3.9, 2.7, 2.7, 4, 1.9, 3.2, 2.5)

# Combine the data into a dataframe
data <- data.frame(Age = Age, Gender = Gender, Total_Protiens = Total_Protiens, Albumin = Albumin)

# Perform hierarchical clustering
set.seed(123)
hc <- hclust(dist(data), method = "ward.D2")

# Plot the dendrogram
plot(hc, hang = -1, labels = FALSE)

# Determine number of clusters
barplot(diff(hc$height))

# Cut the dendrogram to get clusters
cluster <- cutree(hc, k = 3)
table(cluster)
# Plot the dendrogram with enhanced visuals
par(mar = c(5, 10, 4, 2))
plot(hc, hang = -1, labels = FALSE, main = "Dendrogram for Hierarchical Clustering", xlab = "Patients", ylab = "Distance")

# Determine number of clusters and plot barplot
barplot(diff(hc$height), main = "Barplot of Height Differences", xlab = "Clusters", ylab = "Height Difference")

# Cut the dendrogram to get clusters and plot dendrogram with clusters
cluster <- cutree(hc, k = 3)
hc_col <- color_branches(as.dendrogram(hc), k = 3)
plot(hc_col, main = "Dendrogram with Clusters", xlab = "Patients", ylab = "Distance")
rect.hclust(hc, k = 3, border = "blue")

# Add cluster information to data
data$cluster <- as.factor(cluster)

# Plot scatter plot with clusters
ggplot(data, aes(x = Age, y = Total_Protiens, col = cluster)) + 
  geom_point() +
  labs(title = "Scatter Plot with Clusters", x = "Age", y = "Total Proteins", col = "Cluster") +
  theme_minimal()

