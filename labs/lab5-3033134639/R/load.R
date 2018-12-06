library(readr)

#### Not working rn

## Function to load R data ----------------------------------------------------
loadRData <- function(path = 'data', file = 'fMRIdata.Rdata') {
  #
  # This function loads the .Rdata for this lab. Arguments can be generalized
  # to load in any .Rdata file.
  #
  # Arguments:
  #   path: the path indicating the location of the .Rdata file. 
  #   file: the name of the .Rdata file to be loaded.
  #
  # Returns (under default args):
  #   a data.frame with the columns: ID, CITY, STATE, ZIP, lat, long, and 467     #   columns of binary encoded data.
  
  file_to_load <- file.path(path, file)
  load(file_to_load)
}

fMRI <- loadRData()

## Just use this to load for time being

fMRI <- load('data/fMRIdata.Rdata')

## From Google Drive, not sure why useful yet

fit_stim <- read_csv('data/fit_stim.csv')

real_wav <- read_csv('data/real_wav.csv')
