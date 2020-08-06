complete <- function(directory, id = 1:332) {
  
  out <- data.frame()
  
  for (i in 1:length(id)) {
    fileName <- formatC(id[i], width = 3, format = "d", flag = "0")
    fileData <- read.csv(paste(directory, "/", fileName, ".csv", sep = ""))
    fileData <- subset(fileData, complete.cases(fileData))
    out <- rbind(out, list(id[i], length(fileData[[1]]), cor(fileData$sulfate, fileData$nitrate)))
  }
  
  colnames(out) <- c("id", "nobs", "cor")
  out
  
}