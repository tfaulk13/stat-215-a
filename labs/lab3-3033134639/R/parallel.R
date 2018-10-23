source('R/load.R')
source('R/functions.R')
 
library(foreach)
library(parallel)
library(doParallel)
library(dplyr)
library(readr)

load('data/lingBinary.Rdata')

ling.data <- lingBinary %>%
  select(7:474)

nCores <- 4
registerDoParallel(nCores)
repetitions <- 100


results.list <- foreach(i = 2:10) %dopar% {
  results <- foreach(j = 1:repetitions) %do% {
    clusterSimKmeans(data = ling.data, 
                     sub.samp.percent = .2, 
                     num.centers = i)
  }
  unlist(results)
}

result.col.names <- c('k02means', 'k03means', 'k04means', 'k05means', 'k06means', 'k07means', 'k08means', 'k09means', 'k10means')

results.df <- as.data.frame(results.list, col.names = result.col.names)


write_csv(results.df, 'data/results.csv')



