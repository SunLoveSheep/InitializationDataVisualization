#R function to make ggplot given the input data.

PlotCurve <- function(FileName, FuncNum)
{
  TarRange <- c(0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001)
  NumArray <- c(0:10)
  RegionArray <- c(1:21)
  #x_label <- as.double(c('5','6.295','7.924','9.976','12.559','15.811','19.905','25.059','31.548','39.716','50','62.946','79.245','99.763','125.59','158.114','199.054','250.594','315.479','397.164','500'))
  NumMovedMatrix <- as.data.frame(matrix(NA, nrow = as.integer(nrow(FileName)), ncol = as.integer(length(TarRange))))
  
  if (FuncNum < 15){
    Optimal <- -1400 + 100*(FuncNum - 1)
    
    for (t in 1:length(TarRange))
    {
      for (r in 1:nrow(FileName))
      {
        for (column in 1:ncol(FileName))
        {
          #check NA
          if (is.na(FileName[r,column] < (1-TarRange[t])*Optimal) == TRUE){}
          else if (FileName[r,column] < (1-TarRange[t])*Optimal){
            NumMovedMatrix[r,t] <- NumArray[column]
            break
          }
        }
      }
    }
    
  }
  else{
    Optimal <- 100*(FuncNum - 14)
    
    for (t in 1:length(TarRange))
    {
      for (r in 1:nrow(FileName))
      {
        for (column in 1:ncol(FileName))
        {
          #check NA
          if (is.na(FileName[r,column] < (1+TarRange[t])*Optimal) == TRUE){}
          else if (FileName[r,column] < (1+TarRange[t])*Optimal){
            NumMovedMatrix[r,t] <- NumArray[column]
            break
          }
        }
      }
    }
    
  }
  
  StackedNumMoved <- with(NumMovedMatrix,
                          data.frame(value = c(V1,V2,V3,V4,V5,V6,V7,V8),
                                     TargetAccuracy = factor(rep(TarRange,
                                                           each = NROW(NumMovedMatrix))),
                                     Regions = rep(1:21, 1)))
  p <- ggplot(na.omit(StackedNumMoved), aes(Regions, value, colour = TargetAccuracy)) + geom_point(size = 3) + geom_line()
  #p <- ggplot(StackedNumMoved, aes(Regions, value, colour = TargetAccuracy)) + geom_point(size = 3) + geom_line()
  p <- p +  xlab("Region size") + ylab("Number of Point Moved") + scale_y_continuous(breaks = c(0,2,4,6,8,10))
  
  return(p)
}
