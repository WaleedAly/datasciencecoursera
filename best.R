best <- function(state, outcome) {
  
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
  
  ## Return hospital name in that state with lowest 30-day death rate
  outcomesForState <- split(outcomes, outcomes$State)[state][[1]]
  bestOutcome <- min(outcomesForState[[outcomeCol]], na.rm = TRUE)
  bestHospitals <- outcomesForState[outcomesForState[outcomeCol] == bestOutcome,][2]
  return(sort(bestHospitals[[1]])[1])
  
}