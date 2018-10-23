library(dplyr)

## Dropping non-numerical columns from ling.binary to do kmeans ---------------
ling.data <- ling.binary %>%
  dplyr::select(7:474)

