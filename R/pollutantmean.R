#rm(list = ls())
#setwd("./mapApi")
#pollutantmean("specdata", "sulfate", 1:1)
pollutantmean <- function(workdir, colName, monitorid = 1:332) {
  ## 'workdir' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'colName' is a character vector of length 1 indicating
  ## mean; either "sulfate" or "nitrate"

  ## 'monitorid' is an integer vector indicating the monitor ID numbers
  ## to be used

  ## Return the mean of the pollmean across all monitors list
  ## in the 'monitorid' vector (ignoring NA values)
  ## NOTE: Do not round the result
  #' @export
  means <- c()
  for (i in monitorid) {
    path <- paste(getwd(),"/", workdir, "/", sprintf("%03d", i), ".csv", sep = "")
    data <- read.csv(path)
    withoutNA <- data[!is.na(data[, colName]), colName]

    means<- c(means,withoutNA)
  }

  mean(means)
}
