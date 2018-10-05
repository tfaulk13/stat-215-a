plotCounties <- function(county_df, y, viridis_color = "plasma", 
                         ylab, title = "", county.line.size = .0025, 
                         county.line.color = "lightgrey",
                         show.plt = F, save = F, save.filename) {
  ## plotCounties: plot counties colored by clusters on US map
  ## 
  ## Inputs:
  ##  -county_df = matrix or data.frame with the columns "state" and "county"
  ##         (as factors) corresponding to y
  ##  -y = class labels or outcome
  ##  -viridis_color = name of viridis color theme
  ##  -ylab = legend title
  ##  -title = title of the plot
  ##  -county.line.size = width of county lines
  ##  -county.line.color = color of county lines
  ##  -show.plt = logical (default = T)
  ##  -save = logical (default = F)
  ##  -save.filename = string ending in .rds (only needed if save = T)
  ## 
  ## Outputs: 
  ##  -map_plt = plot of US map, colored by y
  
  # merge with counties data to make ggplot
  counties <- map_data("county")
  counties <- counties %>% mutate_if(is.character, as.factor)
  plt_df <- data.frame(county_df, y = y)
  plt_df <- merge(x = counties, y = plt_df,
                  by.x = c("region", "subregion"), 
                  by.y = c("state", "county"))
  
  # fill values for the county map
  data <- data.frame(county_df, y = y)
  
  # merge lat/long from counties with data
  plt_df <- counties %>%
    left_join(., data, by = c("region" = "state", "subregion" = "county"))

  # plot counties, colored by y
  if (is.factor(y)) {
    map_plt <- ggplot(plt_df %>% filter(long > -125)) + # don't plot AK and HI
      geom_polygon(aes(x = long, y = lat, group = group, fill = y)) +
      geom_path(aes(x = long, y = lat, group = group), 
                color = county.line.color, size = county.line.size) +
      myGGplotMapTheme() +
      scale_fill_viridis_d(option = viridis_color, na.value = "lightgrey") +
      labs(title = title) +
      coord_fixed(1.3)
  }else {
    map_plt <- ggplot(plt_df %>% filter(long > -125)) + # don't plot AK and HI
      geom_polygon(aes(x = long, y = lat, group = group, fill = y)) +
      geom_path(aes(x = long, y = lat, group = group), 
                color = county.line.color, size = county.line.size) + 
      myGGplotMapTheme() +
      scale_fill_continuous(na.value = "lightgrey") +
      scale_fill_viridis(option = viridis_color) +
      labs(title = title) +
      coord_fixed(1.3)
  }
  
  if (!missing(ylab)) {
    map_plt <- map_plt + labs(fill = ylab)
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
