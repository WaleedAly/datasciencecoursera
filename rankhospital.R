rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  outcomes <- read.csv('c2a3/outcome-of-care-measures.csv')
  
  #Data preps 
  outcomes[,11] <- as.numeric(outcomes[,11]) ## Heart Attack
  outcomes[,17] <- as.numeric(outcomes[,17]) ## Heart Failure
  outcomes[,23] <- as.numeric(outcomes[,23]) ## Pneumonia
  
  ## Check that state and outcome are valid
  validStates <- outcomes$State
  validOutcomes <- data.frame(id = c(11, 17, 23), desc = c("heart attack", "heart failure", "pneumonia"))
  if (!any(state == validStates)) stop("invalid state")
  if (!any(outcome == validOutcomes$desc)) stop("invalid outcome")
  else outcomeCol <- subset(validOutcomes, desc == outcome)$id
  if (!is.numeric(num) && num != "worst") num <- "best"
  
  ## Return hospital name in that state with the given rank 30-day death rate
  outcomesForState <- split(outcomes, outcomes$State)[state][[1]]
  outcomesForState <- outcomesForState[c(2, outcomeCol)]
  outcomesForState <- outcomesForState[!is.na(outcomesForState[2]),]
  outcomesForState <- outcomesForState[order(outcomesForState[,2], outcomesForState[,1]),]
  
  if(num == "best") return(head(outcomesForState, 1)[[1]])
  else if(num == "worst") return(tail(outcomesForState, 1)[[1]])
  else if(num > nrow(outcomesForState) || num < 1) return(NA)
  
  return(outcomesForState[num,][[1]])
  
}