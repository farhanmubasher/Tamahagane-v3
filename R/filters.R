
library(jsonlite)

tamahagane.apply.filter <- function(inputFilePath, parameters, outputFilePath)
{
  inputFilePath <- fromJSON(inputFilePath)
  converted.Dataset <- read.csv(inputFilePath[1], header = TRUE,
                                na.string = c("", "NA"),
                                stringsAsFactors = TRUE,
                                encoding = "Latin-1")

  parameters <- fromJSON(parameters)
  cols <- parameters[["readingheader"]]
  cols<- gsub(" ", ".", cols)
  converted.filterType <- parameters[["select"]]
  converted.Values <- parameters[["textfield"]]

  j<-1
  while(j<=length(converted.Values))
  {
    if(converted.filterType[j] == "STARTWITH" | converted.filterType[j] == "ENDWITH")
    {
      obj.start <- paste("^", converted.Values[j] , sep = "")
      obj.end <- paste(converted.Values[j] , "$", sep = "")
    }

    j<- j+1
  }

  i<- 1
  while(i <= length(converted.Values) )
  {
    if(converted.filterType[i] == "EQUAL"){
      converted.Dataset <-  subset(converted.Dataset, converted.Dataset[cols[i]] == converted.Values[i])
    }
    else if(converted.filterType[i] == "NOTEQUAL"){
      converted.Dataset <-  subset(converted.Dataset, converted.Dataset[cols[i]] != converted.Values[i])
    }
    else if(converted.filterType[i] == "ISIN"){
      converted.Dataset <-  subset(converted.Dataset, converted.Dataset[cols[i]] == converted.Values[i])
    }
    else if(converted.filterType[i] == "ISNOTIN"){
      converted.Dataset <-  subset(converted.Dataset, converted.Dataset[cols[i]] != converted.Values[i])
    }
    else if(converted.filterType[i] == "LESSTHAN"){
      converted.Dataset <-  subset(converted.Dataset, converted.Dataset[cols[i]] < converted.Values[i])
    }
    else if(converted.filterType[i] == "GREATERTHAN"){
      converted.Dataset <-  subset(converted.Dataset, converted.Dataset[cols[i]] > converted.Values[i])
    }
    else if(converted.filterType[i] == "STARTWITH"){
      converted.Dataset <- converted.Dataset[grep(obj.start, converted.Dataset[,cols[i]]) ,]
    }
    else if(converted.filterType[i] == "ENDWITH"){
      converted.Dataset <- converted.Dataset[grep(obj.end, converted.Dataset[,cols[i]]) ,]
    }

    i<- i+1
  }


  if(nrow(converted.Dataset) == 0){

    return( write.table( "No Records Found !!", file = outputFilePath, quote = FALSE, row.names = FALSE, col.names = FALSE))
  }
  else{
    return(write.csv(converted.Dataset, file = outputFilePath , quote = FALSE, row.names = FALSE))
  }

}

