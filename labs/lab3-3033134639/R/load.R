## Function to load binary data -----------------------------------------------
loadRData <- function(path = 'data/', file = 'lingBinary.Rdata') {
  #
  # This function loads the .Rdata for this lab. Arguments can be generalized
  # to load in any .Rdata file.
  #
  # Arguments:
  #   path: the path indicating the location of the `lingBinary` .Rdata file; 
  #         path should be relative to the lab3 project file.
  #   file: the name of the .Rdata file to be loaded.
  #
  # Returns (under default args):
  #   a data.frame with the columns: ID, CITY, STATE, ZIP, lat, long, and 467     #   columns of binary encoded data.
  
  file_to_load <- paste0(path, file)
  get(load(file_to_load))
}
