pollutantmean <- function(directory, pollutant, id = 1:332) {

  out <- c()
  
  for (i in 1:length(id)) {
    fileName <- formatC(id[i], width = 3, format = "d", flag = "0")
    fileData <- read.csv(paste(directory, "/", fileName, ".csv", sep = ""))
    measurements <- fileData[[pollutant]]
    out <- c(out, measurements[!is.na(measurements)])
  }
  
  mean(out)
  
}