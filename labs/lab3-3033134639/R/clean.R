## Dropping non-numerical columns from ling.binary to do kmeans ---------------
ling.data <- ling.binary %>%
  select(7:474)

