library(jsonlite)
library(lubridate)

tamahagane.apply.sort<- function(inputFilePath, parameters, outputFilePath)
{
  inputFilePath <- fromJSON(inputFilePath)
  Dataset <- read.csv(inputFilePath[1], header = TRUE,
                      na.string = c("", "NA"),
                      stringsAsFactors = TRUE,
                      encoding = "Latin-1")

  parameters <- fromJSON(parameters)
  columName <-  parameters[["readingheader"]]
  columName<- gsub(" ", ".", columName)
  sortType <-  parameters[["select"]]

  check  <- tryCatch({
    !all(is.na(as.Date(Dataset[, columName],format="%d/%m/%Y")))
  }, error=function(e){NULL})

  if(!is.null(check) && check!= FALSE)
  {
    Dataset[columName] <- dmy(Dataset[, columName])
    count.misssing.values  <- colSums(is.na(Dataset[columName]))
    if(count.misssing.values >= 1)
    {
      return( write.table( "Date Formate is Not Correct !!", file = outputFilePath, quote = FALSE, row.names = FALSE, col.names = FALSE))
    }
    else
    {

      if(sortType=="ASC" || sortType=="")
      {
        return (write.csv(Dataset[order(Dataset[, columName]) ,], file = outputFilePath, quote = FALSE, row.names = FALSE) )
      }
      else(sortType=="DESC")
      {

        return (write.csv(Dataset[rev(order(Dataset[, columName]))  ,] , file = outputFilePath, quote = FALSE, row.names = FALSE))
      }

    }

  }

  else if (is.null(check) || check== FALSE){

    if(sortType=="ASC" || sortType=="")
    {

      return (write.csv(Dataset[order(Dataset[, columName]) ,] , file = outputFilePath, quote = FALSE, row.names = FALSE))
    }
    else(sortType=="DESC")
    {

      Dataset <- Dataset[rev(order(Dataset[, columName]))  ,]

      return (write.csv(Dataset[rev(order(Dataset[, columName]))  ,], file = outputFilePath, quote = FALSE, row.names = FALSE))
    }
  }


}
