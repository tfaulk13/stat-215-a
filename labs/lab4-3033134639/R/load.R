## Loading packages -----------------------------------------------------------
library(tidyverse)
library(utils)

## Function to load cloud data -----------------------------------------------
loadCloudData <- function(path = 'data', file = 'image1.txt') {
  #
  # This function loads the cloud data into R. As long as other cloud data
  # has the same column structure, this function can be used to load that too.
  #
  # Arguments:
  #   path: the folder path to the folder that contains the file.
  #   file: the name of the file to be loaded.
  #
  # Returns:
  #   a data.frame with the columns: 'X', 'Y', 'EXP', 'NDAI', 'SD', 'CORR',       #  'RadDF', 'RadCF', 'RadBF', 'RadAF', 'RadAN'
  
  cloud_path <- file.path(path, file)

  cloud_df <- read.table(cloud_path, header = F)
  
  col_names <- c('y', 'x', 'label', 'NDAI', 'SD', 'CORR', 'DF', 'CF', 'BF', 'AF', 'AN')
  
  colnames(cloud_df) <- col_names
  
  return(cloud_df)
}

## Loading image data and creating dataframe for various operations -----------
image_1 <- loadCloudData()
image_2 <- loadCloudData(file = 'image2.txt')
image_3 <- loadCloudData(file = 'image3.txt')

images <- rbind(image_1, image_2, image_3)

image_1_filter <- image_1 %>%
  filter(label != 0)

images_filter <- images %>%
  filter(label != 0)

## Loading basic ggplot theme for EDA
gg_theme <- theme(plot.title = element_text(face = 'bold', 
                                            size = 12, 
                                            hjust = 0.5),
                  legend.position = 'none',
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_blank(),
                  axis.title = element_text(face = 'bold')) 

