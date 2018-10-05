library(tidyverse)
library(maps)
library(factoextra)

# Loading in linguistic  data -------------------------------------------------
ling_data <- read.delim('data/lingData.txt', sep = " ")
ling_location <- read.delim('data/lingLocation.txt', sep = " ")

# Loading in question data ----------------------------------------------------
load('data/question_data.RData')

# Loading in state data -------------------------------------------------------
map_df <- map_data('state')


