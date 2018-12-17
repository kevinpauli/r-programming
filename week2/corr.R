corr <- function(directory, threshold = 0) {
  complete <- complete(directory, 1:332)
  acceptable <- subset(complete, nobs > threshold)
  id <- acceptable[,'id']
  results <- c()
  for (fileNum in id) {
    zeropad <- if (fileNum > 99) {
      ''
    } else if (fileNum > 9) {
      '0'
    } else {
      '00'
    }
    filename = paste0(directory, '/', zeropad, toString(fileNum), '.csv')
    df <- read.csv(filename)
    dfcomplete <- df[complete.cases(df),]
    sulfate <- dfcomplete[,'sulfate']
    nitrate <- dfcomplete[,'nitrate']
    result <- cor(sulfate, nitrate)
    results = append(results, result)
  }
  results
}