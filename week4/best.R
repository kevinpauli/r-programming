best <- function(state, outcome) {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  if (!(state %in% outcomes$State)) {
    stop("invalid state")
  }
  validOutcomes = data.frame(
    outcome = c("heart attack", "heart failure", "pneumonia"), 
    colname = c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"),
    stringsAsFactors = FALSE
  )
  if (!(outcome %in% validOutcomes$outcome)) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  colname = validOutcomes$colname[which(validOutcomes$outcome == outcome)]
  stateOutcomes <- outcomes[outcomes$State == state, c('Hospital.Name', colname)]
  stateOutcomes[,colname] <- as.numeric(stateOutcomes[,colname])
  lowestDeathRate <- min(stateOutcomes[,colname], na.rm = TRUE)
  bestHospitals = stateOutcomes$Hospital.Name[which(stateOutcomes[,colname] == lowestDeathRate)]
  min(bestHospitals)
}