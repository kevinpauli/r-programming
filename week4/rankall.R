rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that outcome is valid
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
  
  ## For each state, find the hospital of the given rank
  colname = validOutcomes$colname[which(validOutcomes$outcome == outcome)]
  outcomes[,colname] <- as.numeric(outcomes[,colname])
  dataOfInterest <- outcomes[!is.na(outcomes[,colname]), c('State', 'Hospital.Name', colname)]
  dataOfInterestByState <- split(dataOfInterest, dataOfInterest$State) 
  dataOfInterestByStateAndSorted <- lapply(dataOfInterestByState, function(stateData) { stateData[order(stateData[,colname], stateData[,'Hospital.Name']),] })
  hospitalList <- lapply(dataOfInterestByStateAndSorted, 
                        function(sortedStateData) { 
                          num <- if (is.numeric(num)) {
                            num
                          } else if (num == "best") {
                            1
                          } else if (num == "worst") {
                            nrow(sortedStateData)
                          }
                          sortedStateData$Hospital.Name[num] 
                        } 
  )
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  stateVector <- names(hospitalList)
  hospitalVector <- unlist(hospitalList, use.names = FALSE)
  data.frame(hospital = hospitalVector, state = stateVector, row.names = stateVector)
}