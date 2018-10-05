library(ggplot2)
library(viridis)
library(gridExtra)
library(RColorBrewer)
library(grid)

myGGplotTheme <- function(font = "Helvetica",
                          axis_title_size = rel(1), axis_text_size = rel(1),
                          panel_grid_major_color = "grey90",
                          panel_background_color = "grey98",
                          strip_background_color = "#2c3e50",
                          strip_text_size = rel(1), 
                          legend_title_size = rel(1), legend_text_size = rel(1),
                          title_size = rel(1.5), ...) {
  # creates my own ggplot theme
  #
  # inputs:
  # -font = font family for ggplot text
  # -axis_title_size = font size of axis title
  # -axis_text_size = font size of axis text
  # -panel_grid_major_color = color of panel grid major axes
  # -panel_background_color = color for plot background
  # -strip_background_color = color for strip background (for facet_grid/wrap)
  # -strip_text_size = font size of strip text
  # -legend_title_size = font size of legend title
  # -legend_text_size = font size of legend text/key
  # -title_size = font size of plot title
  #
  # output: 
  # -ggplot theme object
  #
  # example usage: 
  # ggplot(iris) + aes(x = Sepal.Length, y = Sepal.Width) + 
  #   geom_point() + myGGplotTheme()
  
  my_theme <- theme(
    axis.title = element_text(family = font, size = axis_title_size,
                              face = "bold"),
    axis.text = element_text(family = font, size = axis_text_size),
    axis.line = element_line(size = 1, color = "black"),
    axis.ticks = element_line(size = rel(1), colour="black"),
    panel.grid.major = element_line(colour = panel_grid_major_color, 
                                    size = rel(0.5)), 
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = panel_background_color),
    strip.background = element_rect(fill = strip_background_color, 
                                    color = strip_background_color),
    strip.text = element_text(color = "white", face = "bold", 
                              size = strip_text_size),
    legend.key = element_rect(fill = "grey98"), 
    legend.text = element_text(family = font, size = legend_text_size), 
    legend.title = element_text(family = font, face = "bold", 
                                size = legend_title_size),
    plot.title = element_text(family = font, face = "bold", size = title_size),
    ...)
  return(my_theme)
}

myGGplotColor <- function(discrete, ...) {
  # changes color theme for ggplot
  # 
  # input:
  # -discrete = T/F logical; whether or not data points are discrete
  # 
  # ouptut:
  # -ggplot color theme
  # 
  # example usage: 
  # ggplot(iris) + aes(x = Sepal.Length, y = Sepal.Width, color = Species) + 
  #   geom_point() + myGGplotTheme() + myGGplotColor(discrete = T)
  
  if (discrete) {
    my_palette <- brewer.pal(n = 8, name = "Dark2")
    # reorder colors
    my_palette[2] <- my_palette[1]
    my_palette[1] <- "#FF9300"
    my_color <- scale_color_manual(values = my_palette, ...)
  }else {
    my_color <- scale_colour_viridis(discrete = F, option = "plasma", 
                                     begin = 0, end = 0.95, ...)
  }
  return(my_color)
}


myGGplotFill <- function(discrete, ...) {
  # changes fill theme for ggplot
  # 
  # input:
  # -discrete = T/F logical; whether or not data points are discrete
  # 
  # ouptut:
  # -ggplot fill theme
  
  if (discrete) {
    my_palette <- brewer.pal(n = 8, name = "Dark2")
    # reorder colors
    my_palette[2] <- my_palette[1]
    my_palette[1] <- "#FF9300"
    my_fill <- scale_fill_manual(values = my_palette, ...)
  }else {
    my_fill <- scale_fill_viridis(discrete = F, option = "plasma", 
                                  begin = 0, end = 0.95, ...)
  }
  return(my_fill)
}

# nice ggplot theme for map
myGGplotMapTheme <- function(...) {
  my_theme <- theme_void() + theme(...)
  return(my_theme)
}
