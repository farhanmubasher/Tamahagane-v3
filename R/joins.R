library(jsonlite)
library(plyr)
tamahagane.apply.join<- function(inputFilePath, parameters, outputFilePath)
{
  inputFilePath <- fromJSON(inputFilePath)

  x <- read.csv(inputFilePath[1], header = TRUE,
                na.string = c("", "NA"),
                stringsAsFactors = TRUE,
                encoding = "Latin-1")


  y <- read.csv(inputFilePath[2], header = TRUE,
                na.string = c("", "NA"),
                stringsAsFactors = TRUE,
                encoding = "Latin-1")

  parameters <- fromJSON(parameters)
  joinType <- parameters[["select"]]
  result <- join(x, y, type= joinType)
  result[is.na(result)] <- "?"
  if(nrow(result) == 0){

    return( write.table( "No Records Found !!", file = outputFilePath, quote = FALSE, row.names = FALSE, col.names = FALSE))
  }
  else{
    return(write.csv(result, file = outputFilePath, quote = FALSE, row.names = FALSE) )
  }
}
