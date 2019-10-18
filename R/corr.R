#cr <- corr("specdata", 150)
#head(cr)
corr <- function(workdir, num = 0)
{
  #workdir <- "specdata"
  corResult <- numeric(0)
  comp <- complete(workdir)
  comp <- comp[comp$nobs >= num,]
  #item <- 1:3
  for (item in comp$id ) {
   # item <- 1
    path<- paste(getwd(),"/",workdir,"/",sprintf("%03d",item),".csv",sep = "")
   # print(path)
    data <-  read.csv(path)
    corData <- data[!is.na(data$sulfate),]
    corData <- data[!is.na(data$nitrate),]
    sul_data <- corData$sulfate
    nitrate_data <- corData$nitrate
  corResult <- cbind(corResult, cor(x=sul_data, y = nitrate_data))
  }
  corResult

}
#cr <- corr("specdata", 150)
#head(cr)
