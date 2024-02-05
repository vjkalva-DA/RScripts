library(dplyr)
library(NbClust)
fb <- read.csv("Song_Football.csv")
str(fb)
colSums(is.na(fb))
fb.c <- fb[,2:8]

# Preparing data for clustering

# scaling the data to normalize the values

fb.sc <- scale(fb.c,center = TRUE, scale = TRUE)

# adding weight to specific variables

tackle <- fb.sc[,1]*3
fb.new <- cbind(fb.sc,tackle)
fb.cluster <- fb.new[,2:8]

# clustring the data

set.seed(2)
results <- kmeans(fb.cluster,3)
results$cluster
results$size
cluster <- results$cluster
fb_data_clustered <- cbind(fb,cluster)

strength <- results$betweenss/results$tot.withinss
results$tot.withinss/results$betweenss

# finding the optimal no of clusters

NO_cluster <- NbClust(fb.cluster,min.nc = 2, max.nc = 20, method = "kmeans", index = "all")


# Hirarchical clustering

dist <- dist(fb.cluster,method = "euclidean", p=2)
fb.hclust <- hclust(dist, method = "average")
plclust(fb.hclust)
