## Correlation similarity measure ---------------------------------------------
clusterCor <- function(cl.1, cl.2) {
  # Come back and add documentation.
  #
  #
  # Calculate the table function only once.
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
  # Add documentation
  #
  #
  #
  
  #
  sub.1 <- sample_frac(data, size = sub.samp.percent)
  sub.2 <- sample_frac(data, size = sub.samp.percent)
  
  #
  cl.1 <- kmeans(sub.1, centers = num.centers)$cluster
  cl.2 <- kmeans(sub.2, centers = num.centers)$cluster
  
  #
  intersect = sub.1 & sub.2
  cl.1.intersect <- cl.1 %in% intersect
  cl.2.intersect <- cl.2 %in% intersect
  
  # Take similarity measure
  sim.measure(cl.1.intersect, cl.2.intersect)
}
