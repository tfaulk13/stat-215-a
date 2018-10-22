source('R/load.R')
ling.binary <- loadRData()

source('R/clean.R')

nCores <- 9
registerDoParallel(nCores)
repetitions = 100


results.list <- foreach(i = 2:3) %dopar% {
  results <- foreach(j = 1:repetitions) %do% {
    clusterSimKmeans(data = ling.data, 
                     sub.samp.percent = .1, 
                     num.centers = i)
  }
  unlist(results)
}

result.col.names <- c('k2means', 'k3means', 'k4means', 'k5means', 'k6means', 'k7means', 'k8means', 'k9means', 'k10means')

results.df <- as.data.frame(results.list, col.names = result.col.names)


write.csv(results.df, 'results.csv')



