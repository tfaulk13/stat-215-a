library(dendextend)

plotHclust <- function(dat, y, dist.metric = "euclidean", linkage = "complete",
                       text.size = 0.5, show.plt = F,
                       save = F, save.filename) {
  ## plotHclust: plot dendrogram from hierarchical clustering
  ## 
  ## Inputs:
  ##  -dat = data matrix or data.frame
  ##  -y = class labels or outcome (optional)
  ##  -dist.metric = distance metric (see stats::dist)
  ##  -linkage = type of linkage (see stats::hclust)
  ##  -text.size = size of text for leaves
  ##  -title = string for plot title name
  ##  -show.plt = logical (default = T)
  ##  -save = logical (default = F)
  ##  -save.filename = string ending in .rds (only needed if save = T)
  ## 
  ## Outputs: 
  ##  -hdend_plt = hierarchical clustering dendrogram
  
  dat <- as.matrix(dat)
  
  if (sum(is.na(dat)) > 0) {
    stop("NAs found in dat")
  }
  
  # distance matrix
  Dmat <- dist(dat, method = dist.metric)
  
  # hierarchical clustering
  my_hclust <- hclust(Dmat, method = linkage)
  my_hclust_dend <- as.dendrogram(my_hclust)
  
  if (missing(y)) { # if don't provide y, don't annotate tree
  }else if (is.factor(y)) { # categorical y
    n_colors <- length(unique(y))
    labels_colors(my_hclust_dend) <- c(1:n_colors)[y][order.dendrogram(my_hclust_dend)]
    labels(my_hclust_dend) <- as.character(y)[order.dendrogram(my_hclust_dend)]
    my_hclust_dend <- hang.dendrogram(my_hclust_dend, hang_height = 0.1)
    my_hclust_dend <- assign_values_to_leaves_nodePar(my_hclust_dend, 
                                                      text.size, "lab.cex")
  }else { # continuous y
    my_colors <- colorNumeric(palette = "viridis", domain = c(min(y), max(y)))
    labels_colors(my_hclust_dend) <- my_colors(y)[order.dendrogram(my_hclust_dend)]
    # labels(my_hclust_dend) <- as.character(y)[order.dendrogram(my_hclust_dend)]
    labels(my_hclust_dend) <- "------"
    my_hclust_dend <- hang.dendrogram(my_hclust_dend, hang_height = 0.1)
    my_hclust_dend <- assign_values_to_leaves_nodePar(my_hclust_dend, 
                                                      text.size, "lab.cex")
  }
  
  title <- paste0("Hierarchical Clustering: \n", 
                  linkage, " Linkage, ", dist.metric, " Distance")
  
  if (show.plt == TRUE) {
    plot(my_hclust_dend, main = title, horiz = FALSE)
  }
  
  if (save) { # save figure to file
    if (!missing(save.filename)) {
      saveRDS(my_hclust_dend, paste0("./", save.filename))
    }else {
      saveRDS(my_hclust_dend, paste0("./",title,".rds"))
    }
  }
  
  return(hdend_plt = my_hclust_dend)
  
}
