source('./R/load.R')
source('./R/functions.R')
 
library(foreach)
library(parallel)
library(doParallel)
library(dplyr)
library(readr)

working.directory <- file.path('./data')

load(file.path(working.directory, 'lingBinary.Rdata'))

ling.data <- lingBinary %>%
  select(7:474)

nCores <- 2
registerDoParallel(nCores)
repetitions <- 3


results.list <- foreach(i = 2:3) %dopar% {
  results <- foreach(j = 1:repetitions) %do% {
    clusterSimKmeans(data = ling.data, 
                     sub.samp.percent = .5, 
                     num.centers = i)
  }
  unlist(results)
}

result.col.names <- c('k2means', 'k3means') #, 'k4means', 'k5means', 'k6means', 'k7means', 'k8means', 'k9means', 'k10means')

results.df <- as.data.frame(results.list, col.names = result.col.names)


write_csv(results.df, './results.csv')



