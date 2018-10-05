plotMapClusters <- function(loc, y, ylab = "y", point.size, alpha,
                            viridis_color = "plasma", title = "", 
                            show.plt = F, save = F, save.filename) {
  ## plotMapClusters: plot clusters on US map
  ## 
  ## Inputs:
  ##  -loc = data matrix or data.frame with the columns "lat" and "long"
  ##         indicating latitude and longitude locations of y
  ##  -y = class labels or outcome
  ##  -ylab = label for legend
  ##  -alpha = alpha for points in ggplot
  ##  -point.size = size of point in ggplot
  ##  -viridis_color = name of viridis color theme
  ##  -title = title of the plot
  ##  -show.plt = logical (default = T)
  ##  -save = logical (default = F)
  ##  -save.filename = string ending in .rds (only needed if save = T)
  ## 
  ## Outputs: 
  ##  -map_plt = plot of US map, colored by y
  
  plt_df <- data.frame(loc, y = y)
  state_df <- map_data("state")
  
  if (is.factor(y)) {
    map_plt <- ggplot(plt_df %>% filter(long > -125)) + # don't plot AK and HI
      # geom_tile(aes(x = long, y = lat, fill = y)) +
      geom_point(aes(x = long, y = lat, color = y), 
                 size = point.size, alpha = alpha) +
      # scale_color_viridis_c(option = viridis_color) +
      geom_polygon(aes(x = long, y = lat, group = group),
                   data = state_df, color = "black", fill = NA) +
      myGGplotMapTheme() +
      scale_color_viridis_d(option = viridis_color) +
      labs(title = title, color = ylab) +
      coord_fixed(1.3)
  }else {
    map_plt <- ggplot(plt_df %>% filter(long > -125)) + # don't plot AK and HI
      # geom_tile(aes(x = long, y = lat, fill = y)) +
      geom_point(aes(x = long, y = lat, color = y), 
                 size = point.size, alpha = alpha) +
      # scale_color_viridis_c(option = viridis_color) +
      geom_polygon(aes(x = long, y = lat, group = group),
                   data = state_df, color = "black", fill = NA) +
      myGGplotMapTheme() +
      scale_color_viridis_c(option = viridis_color) +
      labs(title = title, color = ylab) +
      coord_fixed(1.3)
  }
  
  
  if (show.plt == TRUE) {
    print(map_plt)
  }
  
  if (save) { # save figure to file
    if (!missing(save.filename)) {
      saveRDS(map_plt, paste0("./", save.filename))
    }else {
      saveRDS(map_plt, paste0("./",title,".rds"))
    }
  }
  
  return(map_plt = map_plt)
  
}
