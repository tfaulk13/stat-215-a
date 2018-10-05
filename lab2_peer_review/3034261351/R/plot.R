# define a function for plotting
plotExperiment <- function(KernelFun, width = 1) {
  # Plot density eitimates with different Kernel functions and bandwidth
  # Args:
  #   kernel: kernel function as a parameter in kernel smoothers
  #   width: bandwidth as another paramter in kernel smoothers 
  
  density <- EstimateDensity(redwood$humid_temp, KernelFun, width, resolution = 100)
  p <- ggplot(density, aes(x, f.hat)) +
    # add a line layer 
    geom_line() +
    xlab("Temperature over Redwood Data") +
    ylab("Est Density") +
    theme_nice
  return(p)
}
