rankhospital <- function(state, outcome, num = "best") {
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  colname = validOutcomes$colname[which(validOutcomes$outcome == outcome)]
  stateOutcomes <- outcomes[outcomes$State == state, c('Hospital.Name', colname)]
  stateOutcomes[,colname] <- as.numeric(stateOutcomes[,colname])
  stateOutcomes <- na.omit(stateOutcomes)
  rankedStateOutcomes <- stateOutcomes[order(stateOutcomes[,colname], stateOutcomes$Hospital.Name),]
  num <- if (is.numeric(num)) {
    num
  } else if (num == "best") {
    1
  } else if (num == "worst") {
    nrow(rankedStateOutcomes)
  }
  if (num <= nrow(rankedStateOutcomes)) {
    rankedStateOutcomes$Hospital.Name[num]
  } else {
    NA
  }
}