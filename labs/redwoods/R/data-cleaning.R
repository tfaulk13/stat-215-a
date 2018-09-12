library(tidyverse)

## Reading in data of interest ------------------------------------------------
motes <- read_delim('labs/lab1/data/mote-location-data.txt', delim = '\t', col_names = c('ID', 'Height', 'Direc', 'Dist', 'Tree'), skip = 1)
dates <- read_csv('labs/lab1/data/sonoma-data-all.csv')


## Merging data ---------------------------------------------------------------
all_data <- left_join(dates, motes, by = c('nodeid' = 'ID'))