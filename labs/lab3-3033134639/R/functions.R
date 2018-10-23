## Correlation similarity measure ---------------------------------------------
clusterCor <- function(cl.1, cl.2) {
  #
  # This function takes two clusters and calculates their correlation. 
  #
  # Arguments:
  #   cl.1: cluster 1
  #   cl.2: cluster 2
  #
  # Returns (under default args):
  #   a number, the correlation between the two clusters.
  
  joint.dist = table(cl.1, cl.2)
  
  cor.raw = sum(joint.dist ^ 2) /
    sqrt(sum(rowSums(joint.dist) ^ 2)) /
    sqrt(sum(colSums(joint.dist) ^ 2))
  
  adj = sqrt((1 / nrow(joint.dist) / ncol(joint.dist)))
  
  (cor.raw - adj)/(1 - adj)
}


## Clustering similarities in k-means -----------------------------------------
clusterSimKmeans <- function(data, sub.samp.percent, num.centers, 
                             sim.measure = clusterCor) {
  # This function runs the algorithm outlined in Ben-Hur et. al. It takes
  # subsamples of a dataframe, runs k-means on those samples, and computers
  # a similarity measure on the resulting clusters.
  #
  # Arguments:
  #   data: the dataframe of interest. 
  #         
  #   sub.samp.percent: the size of the sub sample. Number must be between 0-1.
  #
  #   num.centers: the number of clusters for k-means.
  #
  #   sim.measure: a similarity measure, a few of which are defined in Ben-Hur
  #                et. al. Default is set to correlation.
  #
  # Returns (under default args):
  #   a number, the correlation of the clusters of two subsamples.
  
  # Two subsamples
  sub.1 <- sample_frac(data, size = sub.samp.percent)
  sub.2 <- sample_frac(data, size = sub.samp.percent)
  
  # K-means on subsamples
  cl.1 <- kmeans(sub.1, centers = num.centers)$cluster
  cl.2 <- kmeans(sub.2, centers = num.centers)$cluster
  
  # Grabbing which points appear in both samples
  intersect = sub.1 & sub.2
  cl.1.intersect <- cl.1 %in% intersect
  cl.2.intersect <- cl.2 %in% intersect
  
  # Take similarity measure
  sim.measure(cl.1.intersect, cl.2.intersect)
}
