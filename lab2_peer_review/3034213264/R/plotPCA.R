library(dplyr)
library(irlba)
library(GGally)

plotPCA <- function(dat, npcs, pcs, y, y_additional, ylab = "y", is.discrete = T,
                    var = F, center.pca = T, scale.pca = F,
                    title = "", show.plt = T, 
                    subsample = 1, alpha = .5, point.size = .5, 
                    save = F, save.filename) {
  ## plotPCA: plot pc pairs plot given data dat and labels y
  ## 
  ## Inputs: (must specify either npcs or which.pcs)
  ##  -dat = data matrix
  ##  -npcs = max number of pcs to show
  ##  -pcs = vector of which pcs to show (optional; only needed if npcs missing)
  ##  -y = class labels or outcome (optional)
  ##  -y_additional = another set of class labels to plot (optional)
  ##  -ylab = label for legend title
  ##  -is.discrete = logical; whether or not y is discrete
  ##  -center.pca = logical; whether or not to center data for pca
  ##  -scale.pca = logical; whether or not to scale data for pca
  ##  -var = logical (default = F); whether or not to compute proportion of 
  ##         variance explained
  ##  -title = string for plot title name
  ##  -alpha = alpha for plotting points (default = .5)
  ##  -point.size = size of points to plot (default = .5)
  ##  -subsample = proportion of points to subsample and plot 
  ##  -show.plt = logical (default = T)
  ##  -save = logical (default = F)
  ##  -save.filename = string ending in .rds (only needed if save = T)
  ## 
  ## Outputs: list of
  ##  -pca_plt = PCA pairs plot
  ##  -pca_scores = PCA scores
  ##  -pca_loadings = PCA loadings
  ##  -prop_var_explained = proportion of variance explained
  
  dat <- as.matrix(dat)
  
  if (sum(is.na(dat)) > 0) {
    stop("NAs found in dat")
  }
  
  if (missing(npcs)) {
    npcs <- max(pcs)
  }else {
    pcs <- 1:npcs
  }
  
  # center and scale data if wanted
  dat <- scale(dat, center = center.pca, scale = scale.pca)
  
  pairs.plt <- NULL
  
  if (npcs / min(nrow(dat), ncol(dat)) > .25) {
    dat.svd <- svd(dat) # just do full svd
  }else {
    # don't need to compute full svd for pca (computationally efficient)
    dat.svd <- dat %>% irlba(nu = npcs, nv = npcs) 
  }
  
  if (var == T) {
    # frobenius norm squared = total variation
    total_var <- norm(dat, "F")^2 
    # compute (marginal) proportion of variance explained
    prop_var_explained <- dat.svd$d^2 / total_var
  }
  
  # if npcs = 2, don't use ggpairs. Only need one plot
  if (npcs == 2) {
    
    if (is.factor(y)) {
      dat.svd$u %>%
        as.data.frame() %>%
        tbl_df() %>%
        mutate(y = y) -> plot.frame
    }else {
      dat.svd$u %>%
        as.data.frame() %>%
        tbl_df() %>%
        mutate(y = y) %>%
        filter(y > -125) -> plot.frame
    }
    
    if (subsample != 1) { # subsample points to plot
      rand_samp <- sample(1:nrow(plot.frame), size = subsample * nrow(plot.frame),
                          replace = F)
      plot.frame <- plot.frame[rand_samp,]
    }
    
    pc_plt <- ggplot(plot.frame) +
      aes(x = V1, y = V2, color = y) +
      geom_point(alpha = alpha, size = point.size) +
      myGGplotTheme() +
      myGGplotColor(discrete = is.discrete) +
      labs(x = paste0("PC1 (", round(prop_var_explained[1], 3), ")"),
           y = paste0("PC2 (", round(prop_var_explained[2], 3), ")"),
           title = title,
           color = ylab)
    
    return(list(pca_plt = pc_plt, 
                pca_scores = dat.svd$u, pca_loadings = dat.svd$v,
                prop_var_explained = prop_var_explained))
  }
  
  if (!missing(y) & missing(y_additional)) {# given 1 known set of class labels
    
    if (is.factor(y)) {
      dat.svd$u %>%
        as.data.frame() %>%
        tbl_df() %>%
        mutate(y = y) -> plot.frame
    }else {
      dat.svd$u %>%
        as.data.frame() %>%
        tbl_df() %>%
        mutate(y = y) %>%
        filter(y > -125) -> plot.frame
    }
  
    # change color scheme
    discrete <- is.factor(y)
    ggplot <- function(...) return(ggplot2::ggplot(...) + 
                                     myGGplotColor(discrete = discrete))
    unlockBinding("ggplot", parent.env(asNamespace("GGally")))
    assign("ggplot", ggplot, parent.env(asNamespace("GGally")))
    
    pairs.plt <- ggpairs(plot.frame,
                         columns = pcs,
                         mapping = aes(color = y,
                                       alpha = alpha),
                         columnLabels = paste0('PC ', pcs, " (", 
                                               round(prop_var_explained[pcs], 3), ")"),
                         lower = list(continuous = wrap("points", 
                                                        size = point.size)),
                         upper = 'blank',
                         title = title)

  }else if (!missing(y) & !missing(y_additional)) { # given two set of class labels to plot
    dat.svd$u %>%
      as.data.frame() %>%
      tbl_df() %>%
      mutate(y = y, y_additional = y_additional) %>%
      # don't plot HI and AK because it skews colors
      filter(y > -125) -> plot.frame # plot data frame
    
    # make lower scatter plots and color by y for the pc pairs plots
    my_lower_plts <- function(data, mapping, ...) {
      x_var <- as.character(mapping$x[2])
      y_var <- as.character(mapping$y[2])
      x_idx <- which(colnames(data) == x_var)
      y_idx <- which(colnames(data) == y_var)
      colnames(data)[x_idx] <- "x_var" 
      colnames(data)[y_idx] <- "y_var"
      
      p <- ggplot(data = data) +
        aes(x = x_var, y = y_var, color = y, alpha = alpha) +
        geom_point(size = point.size) +
        scale_color_viridis_c(option = "C")
      return(p)
    }
    
    # make lower scatter plots and color by y_additional for the pc pairs plots
    my_upper_plts <- function(data, mapping, ...) {
      x_var <- as.character(mapping$x[2])
      y_var <- as.character(mapping$y[2])
      x_idx <- which(colnames(data) == x_var)
      y_idx <- which(colnames(data) == y_var)
      colnames(data)[x_idx] <- "x_var" 
      colnames(data)[y_idx] <- "y_var"
      
      p <- ggplot(data = data) +
        aes(x = x_var, y = y_var, color = y_additional, alpha = alpha) +
        geom_point(size = point.size) +
        scale_color_viridis_c(option = "D")
      return(p)
    }
    
    if (subsample != 1) { # subsample points to plot
      rand_samp <- sample(1:nrow(plot.frame), size = subsample * nrow(plot.frame),
                          replace = F)
      plot.frame <- plot.frame[rand_samp,]
    }
    
    pairs.plt <- ggpairs(plot.frame,
                         columns = pcs,
                         mapping = aes(color = y, alpha = alpha),
                         columnLabels = paste0('PC ', pcs, " (", 
                                               round(prop_var_explained[pcs], 3), ")"),
                         lower = list(continuous = my_lower_plts),
                         upper = list(continuous = my_upper_plts),
                         title = title)
    
  }else { # if class labels/outcomes are unknown
    dat.svd$u %>%
      as.data.frame() %>%
      tbl_df() -> plot.frame
    
    pairs.plt <- ggpairs(plot.frame,
                         columns = pcs,
                         mapping = aes(alpha = alpha),
                         columnLabels = paste0('PC ', pcs, " (", 
                                               round(prop_var_explained[pcs], 3), ")"),
                         lower = list(continuous = wrap("points",
                                                        size = point.size)),
                         upper = 'blank',
                         title = title)
  }
  
  pairs.plt <- pairs.plt + myGGplotTheme(axis_text_size = rel(.4), 
                                         strip_text_size = rel(.8))
  
  if (show.plt == TRUE) {
    print(pairs.plt)
  }
  
  if (save) { # save figure to file
    if (!missing(save.filename)) {
      saveRDS(pairs.plt, paste0("./", save.filename))
    }else {
      saveRDS(pairs.plt, paste0("./",title,".rds"))
    }
  }
  
  return(list(pca_plt = pairs.plt, 
              pca_scores = dat.svd$u, pca_loadings = dat.svd$v,
              prop_var_explained = prop_var_explained))
  
}
