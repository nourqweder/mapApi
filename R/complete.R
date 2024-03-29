#'title complete
#'
#'param workdir is a character vector of length 1 indicating
#'  the location of the CSV files
#'param monitorid is an integer vector indicating the monitor ID numbers
#'   to be used
#' return a data frame of the from:
#'  id nobs
#'  1  117
#'  2  1041
#'  where nobs is: number of complete cases
#'  and id is: mointor id
#'
#' @export
complete <- function(workdir, monitorId =1:332) {

  result <- data.frame(id=numeric(0), nobs=numeric(0))
  for (item in monitorId) {
    #print(monitorId)
    path <- paste(getwd(), "/", workdir,"/",sprintf("%03d",item),".csv",sep = "")
    #print(path)
    myData <- read.csv(path)
    myData<- myData [!is.na(myData$sulfate),]
    #print(head(myData))
    myData<- myData [!is.na(myData$nitrate),]
    nobs <- nrow(myData)
    #print(nobs)
    result <- rbind(result, data.frame(id =item, nobs=nobs) )

  }
result
}
