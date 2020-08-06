corr <- function(directory, threshold = 0) {
  
  out <- 0
  
  comp <- complete(directory)
  comp <- subset(comp, nobs > threshold)
  
  if (length(comp) > 0) out = comp$cor
  
  out
  
}