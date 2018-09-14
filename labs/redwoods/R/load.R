library(tidyverse)

## Reading in data of interest ------------------------------------------------
motes <- read_delim('data/mote-location-data.txt', delim = '\t', col_names = c('ID', 'Height', 'Direc', 'Dist', 'Tree'), skip = 1)
data <- read_csv('data/sonoma-data-all.csv')
sonoma_dates <- read_lines('data/sonoma-dates')

