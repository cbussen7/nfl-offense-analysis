library(tidyverse)

data <- read_csv("/Users/christopherbussen/Documents/School/UDS2024/MTH208/final/nfl_updated.csv")

# don't use team and location - categorical - drop rows w NA
data <- select(data, -c(team, city, state))

# check which columns have NA
colSums(is.na(data))

# drop columns w NAs - mov and Avg attendance
data <- data[, colSums(is.na(data)) == 0]

# scale data due to different scales
data_scaled <- scale(data)

# perform pca on scaled data
pca_results <- prcomp(data_scaled, center = TRUE, scale. = TRUE)
summary(pca_results)

# View loadings (rotation matrix)
(loadings <- pca_results$rotation)

# plot first two dimensions
plot(pca_results$x[,1:2], xlab = "PC1", ylab = "PC2", main = "PCA of NFL Data")

# 9 PCs capture 90% of variance
pcs <- pca_results$x[, 1:9]

# perform hierarchical clustering
d <- dist(pcs)
hc <- hclust(d, method = "complete")

# plot dendrogram
plot(hc, labels = FALSE, hang = -1, main = "Hierarchical Clustering Dendrogram of NFL Data")

# cut tree and plot clusters
clusterCut <- cutree(hc, 2)
plot(pca_results$x[,1:2], col = clusterCut, 
     xlab = "PC1", ylab = "PC2", main = "Hierarchichal Clustering of NFL Data")

# perform kmeans
set.seed(2024)
(kmeans_result <- kmeans(pcs, centers = 2))

# plot clusters
plot(pca_results$x[,1:2], col = kmeans_result$cluster, 
     xlab = "PC1", ylab = "PC2", main = "K-means Clustering of PCA of NFL Data")


# methods to decide clusters
# silhouette scores to see how well data is clustered
silhouette_scores <- silhouette(kmeans_result$cluster, d)
plot(silhouette_scores, col = 1:3, border = NA)


# elbow method
wss <- sapply(1:10, function(k) {
   kmeans(data_scaled, centers = k, nstart = 25)$tot.withinss
})

# plot elbow
k_values <- 1:10  # Range of k values used
plot(k_values, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K", ylab = "Total within-cluster sum of squares")

