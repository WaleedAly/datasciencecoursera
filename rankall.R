rankall <- function(outcome, num = "best") {

  ## Read outcome data
  outcomes <- read.csv('c2a3/outcome-of-care-measures.csv')
  
  #Data preps 
  outcomes[,11] <- as.numeric(outcomes[,11]) ## Heart Attack
  outcomes[,17] <- as.numeric(outcomes[,17]) ## Heart Failure
  outcomes[,23] <- as.numeric(outcomes[,23]) ## Pneumonia
  
  ## Check that outcome is valid
  validOutcomes <- data.frame(id = c(11, 17, 23), desc = c("heart attack", "heart failure", "pneumonia"))
  if (!any(outcome == validOutcomes$desc)) stop("invalid outcome")
  else outcomeCol <- subset(validOutcomes, desc == outcome)$id
  if (!is.numeric(num) && num != "worst") num <- "best"
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the (hospital) names and the (state) name
  states <- levels(factor(outcomes$State))
  hospitals <- as.character(lapply(states, rankhospital, outcome, num))
  return(data.frame(state = states, hospital = hospitals))
  
}