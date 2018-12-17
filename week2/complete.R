complete <- function(directory, id = 1:332) {
  nobs = c()
  for (i in 1:length(id)) {
    fileNum = id[i]
    zeropad <- if (fileNum > 99) {
      ''
    } else if (fileNum > 9) {
      '0'
    } else {
      '00'
    }
    filename = paste0(directory, '/', zeropad, toString(fileNum), '.csv')
    df <- read.csv(filename)
    thisNobs <- length(which(complete.cases(df)))
    nobs[i] <- thisNobs
  }
  data.frame(id, nobs)
}