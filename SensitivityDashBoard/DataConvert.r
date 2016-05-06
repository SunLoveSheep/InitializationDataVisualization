#This is the code to clean the input data.

DataConvert <- function(rawFile)
{
  #assign row names
  rownames(rawFile) <- rawFile[,1]
  
  #remove colume not in use
  rawFile <- rawFile[,-1]
  rawFile <- rawFile[,-12]
  
  #assign column names
  colnames(rawFile) <- NumArray
  
  return(rawFile)
}
