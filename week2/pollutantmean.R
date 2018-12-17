pollutantmean <- function(directory, pollutant, id = 1:332) {
  allMeasurements <- c()
  for (i in id) {
    zeropad <- if (i > 99) {
      ''
    } else if (i > 9) {
      '0'
    } else {
      '00'
    }
    filename = paste0(directory, '/', zeropad, toString(i), '.csv')
    df <- read.csv(filename)
    thisMeasurements <- df[,pollutant]
    allMeasurements <- c(allMeasurements, thisMeasurements)
  }
  mean(allMeasurements, na.rm = TRUE)
}