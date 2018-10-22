nCores <- 2
registerDoParallel(nCores)
repetitions = 3


results.list <- foreach(i = 2:3) %dopar% {
  results <- foreach(j = 1:repetitions) %do% {
    clusterSimKmeans(data = ling.data, 
                     sub.samp.percent = .1, 
                     num.centers = i)
  }
  unlist(results)
}

result.col.names <- c('k2means', 'k3means')

results.df <- as.data.frame(results_list, col.names = result.col.names)



