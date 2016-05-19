library(ggplot2)
library(grid)
library(gridExtra)
library(stringr)
library(reshape)

Num9_Region_neg22_neg12_RowNames <- c("RightNum9_Mean","RightNum9_STD","Num9-1_Mean","Num9-1_STD",
                                      "Num9-3_Mean","Num9-3_STD","Num9-6_Mean","Num9-6_STD",
                                      "Num9-11_Mean","Num9-11_STD","Num9-14_Mean","Num9-14_STD",
                                      "NP10_Extra0_Mean","NP10_Extra0_STD","NP19_Extra9_Mean","NP19_Extra9_STD",
                                      "NP11_Extra1_Mean","NP11_Extra1_STD","NP13_Extra3_Mean","NP13_Extra3_STD",
                                      "NP16_Extra6_Mean","NP16_Extra6_STD","NP21_Extra11_Mean","NP21_Extra11_STD",
                                      "NP24_Extra14_Mean","NP24_Extra14_STD")

#To check characters in strings to get row names and number of rows:
#grepl("Right",rownames(CEC13_F1_FE50_Num9)) == TRUE #for the right case
#grepl("Num9-",rownames(CEC13_F1_FE50_Num9)) == TRUE #for cases with wrong guiding number
#grepl("Extra",rownames(CEC13_F1_FE50_Num9)) == TRUE #for cases with no guidance

ConfidenceLevel <- 0.95
N <- 25*51
FuncNum <- "F28"
NP <- 10
FEperD <- 50
D <- 10

#--------------------------------------------------------------------------
#some examples
PlottoFile(FuncNum, NP, FEperD, D)
#plot FE50 D30 NP50:
PlottoFile(FuncNum = "F1", NP = 50, FEperD = 50, D = 30)
#plot FE50 D30 DReduce NP20:
PlotAllCases(FuncNum = "F1", NP = 20, FEperD = 50, D = 30)
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
#read all .csv files from a given path and do things:
#plot to a given folder:
FuncNum <- "F19"
PlotAllCSVtoFolder(funcnum = FuncNum)
FuncNum <- "F19"
Plot3CasestoFolder(funcnum = FuncNum)

#function to plot to a given folder, given a list of .csv files
PlotAllCSVtoFolder <- function(funcnum = "F1")
{
  inputpath <- paste("C:/Users/MikeSUN/Dropbox/Programmings/Initialization_GuidedCornerBased/results/DeterReduce/",funcnum,"/",sep = "")
  outputpath <- paste("C:/Users/MikeSUN/Dropbox/Programmings/Initialization_GuidedCornerBased/results/Plots/",funcnum,"/",sep = "")
  
  files <- list.files(path=inputpath, pattern="*.csv")
  for(file in files)
  {
    #perpos <- which(strsplit(file, "")[[1]]==".")
    #assign(
    #  gsub(" ","",substr(file, 1, perpos-1)), 
    #  read.csv(paste(inputpath,file,sep=""),header = FALSE))
    inputfile <- read.csv(paste(inputpath,file,sep=""), header = FALSE)
    
    title <- FindPlotTitle(file)
    jpgpath <- paste(outputpath,title,".jpg",sep = "")
    jpeg(jpgpath, width = 1600, height = 900, res = 120)
    PlotAllCases(inputfile, title)
    dev.off()
  }
}

#extract data from cases: RightNum, 0Extra, 9Extra and construct new data frame.
#Calculating mean and confidence interval
DataProcess_CompareCases <- function(funcnum)
{
  inputpath <- paste("C:/Users/MikeSUN/Dropbox/Programmings/Initialization_GuidedCornerBased/results/DeterReduce/",funcnum,"/",sep = "")
  outputpath <- paste("C:/Users/MikeSUN/Dropbox/Programmings/Initialization_GuidedCornerBased/results/Plots/",funcnum,"/",sep = "")
  
  #-----------------------------------
  #read in files, extract and combine targeted data
  #column 1-25: optimal inside region
  #column 26-50: optimal outside region
  files <- list.files(path=inputpath, pattern="*.csv")
  combinedinput <- c()
  for(file in files)
  {
    #read in .csv file
    inputfile <- read.csv(paste(inputpath,file,sep=""), header = FALSE)
    
    #find the title of the file and extract RightNum, 0Extra, 9Extra data
    title <- Find3CasesTitle(file)
    inputfile <- DataProcess_Right0Extra9ExtraOnly(inputfile)
    
    tmprownames <- rownames(inputfile)
    for (i in 1:length(tmprownames))
    {
      tmprownames[i] <- paste(title,tmprownames[i],sep = "_")
    }
    rownames(inputfile) <- tmprownames
    
    #combine all rows into one output file
    combinedinput <- rbind(combinedinput, inputfile)
  }
  #-----------------------------------
  
  #-----------------------------------
  #Calculate the mean and std, or other metric on the target data:
  output <- data.frame(c(rep(0.0, nrow(combinedinput))),c(rep(0.0, nrow(combinedinput))))
  colnames(output) <- c("Optimal Inside Region","Optimal Outside Region")
  #find means for optimal inside cases
  for (i in 1:nrow(combinedinput))
  {
    output[i,1] <- rowMeans(combinedinput[i,1:25])
    output[i,2] <- rowMeans(combinedinput[i,26:50])
  }
  rownames(output) <- rownames(combinedinput)
  
  #calculating differences
  output_delta <- data.frame(c(rep(0.0, nrow(combinedinput)/3)), c(rep(0.0, nrow(combinedinput)/3)))
  colnames(output_delta) <- c("Optimal Inside Region","Optimal Outside Region")
  rownames_delta <- c(rep("",nrow(combinedinput)/3))
  for (i in seq(1,nrow(output_delta),2))
  {
    output_delta[i,1] <- (output[i*3-2,1] - output[i*3,1])/abs(output[i*3,1])
    output_delta[i+1,1] <- (output[i*3+2,1] - output[i*3,1])/abs(output[i*3,1])
    output_delta[i,2] <- (output[i*3-2,2] - output[i*3,2])/abs(output[i*3,2])
    output_delta[i+1,2] <- (output[i*3+2,2] - output[i*3,2])/abs(output[i*3,2])
    
    rownames_delta[i] <- paste(rownames(output[i*3-2,]),"_vs_0Extra",sep = "")
    rownames_delta[i+1] <- paste(rownames(output[i*3+2,]),"_vs_0Extra",sep = "")
  }
  rownames(output_delta) <- rownames_delta
  #-----------------------------------
  return(output_delta)
}

#function to plot 3 cases comparisons given a certain function data to a given folder
Plot3CasestoFolder <- function(funcnum = "F1")
{
  #read data from .csv files and reshape data.
  CasesData <- DataProcess_CompareCases(funcnum)
  
  XLabels <- data.frame(V1 = rownames(CasesData))
  XLabels$V1 <- factor(XLabels$V1, levels = XLabels$V1)
  
  PlotMatrix <- data.frame(Cases = XLabels$V1,
                           Difference_Inside = CasesData[,1],
                           Difference_Outside = CasesData[,2])
  PlotMatrix <- melt(PlotMatrix, id.vars = 'Cases', variable.name = 'InsideOutside')
  
  title <- paste(funcnum," comparisons among RightNum9, Extra9, and Extra0 cases", sep = "")
  p <- ggplot() + 
    geom_bar(data = PlotMatrix, aes(x = Cases, y = value, fill = variable), stat = "identity", position = "dodge") +
    #geom_bar(data = PlotMatrix, aes(x = Cases, y = Difference_Outside, fill = Difference_Outside), stat = "identity", position = "dodge") +
    coord_flip() + theme(text = element_text(size=16)) + ggtitle(title)
  
  jpgpath <- paste("C:/Users/MikeSUN/Dropbox/Programmings/Initialization_GuidedCornerBased/results/3CasesComparison/",funcnum,"_3CasesComparison.jpeg",sep = "")
  jpeg(jpgpath, width = 1600, height = 900, res = 120)
  
  print(p)#need to add this "print" for ggplot, otherwise the figure will not be saved in the output image.
  dev.off()
}
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
#given FuncNum, NP, FEperD, D, read .csv data, plot and output to .jpg file
PlottoFile <- function(FuncNum, NP, FEperD, D)
{
  FileName <- paste("CEC13_A1_Tech1_",FuncNum,"_NP",NP,"_FE",FEperD,"_D",D,"_Region-22_-12.csv", sep = "")
  inputpath=file.path("C:", "Users", "MikeSUN", "Dropbox", "Programmings", "Initialization_GuidedCornerBased", "results", "DeterReduce", FuncNum, FileName)
  InputFile <- read.csv(inputpath, header = FALSE)
  
  title <- FindPlotTitle(FileName)
  jpg_name <- paste(title,".jpg",sep = "")
  mypath=file.path("C:", "Users", "MikeSUN", "Dropbox", "Programmings", "Initialization_GuidedCornerBased", "results", "Plots", jpg_name)
  jpeg(mypath, width = 1600, height = 900, res = 120)
  PlotAllCases(InputFile, title)
  dev.off()
}
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
#plot with grid.arrange
PlotAllCases <- function(input, input_title)
{
  ProcessedInput <- DataProcess(input)
  
  plot.new()
  p1<-PlotMeanSTD(as.data.frame(ProcessedInput[1]), ConfidenceLevel, N) + ggtitle("Optimal Inside Target: Guided vs Guided_WrongNumPoint")
  p2<-PlotMeanSTD(as.data.frame(ProcessedInput[2]), ConfidenceLevel, N) + ggtitle("Optimal Inside Target: Guided vs NotGuided_DifferentNP")
  p3<-PlotMeanSTD(as.data.frame(ProcessedInput[3]), ConfidenceLevel, N) + ggtitle("Optimal Outside Target: Guided vs Guided_WrongNumPoint")
  p4<-PlotMeanSTD(as.data.frame(ProcessedInput[4]), ConfidenceLevel, N) + ggtitle("Optimal Outside Target: Guided vs NotGuided_DifferentNP")
  grid.arrange(p1,p2,p3,p4, top = input_title, nrow = 4)
}
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
#build plots
PlotMeanSTD <- function(input, con_level = 0.95, n = N)#input matrix, con_level: confidence level, n: total sample size
{
  #input should contains all instances for inside or outside cases with means and stds
  
  #confidence z (or t)
  z <- 1.96 #default
  if (con_level == 0.95)
  {
    z <- 1.96
  }
  
  #calculate means for all rows in the input
  MeanofRows <- rowMeans(input)
  
  #to record the Means of means and stds, as well as the confidence minimum and maximum
  Meanofmeans <- c(rep(0.0,nrow(input)/2))
  Meanofstds <- c(rep(0.0,nrow(input)/2))
  ConfidenceMin <- c(rep(0.0,nrow(input)/2))
  ConfidenceMax <- c(rep(0.0,nrow(input)/2))
  XLabels <- c(rep("",nrow(input)/2))
  
  #read from input matrix, every two rows are iteratively the mean and the std for one case
  i <- 1
  while (i<=(nrow(input)/2))
  {
    Meanofmeans[i] <- MeanofRows[i*2-1]
    Meanofstds[i] <- MeanofRows[i*2]
    
    ConfidenceMin[i] <- Meanofmeans[i] - z*Meanofstds[i]/sqrt(n)
    ConfidenceMax[i] <- Meanofmeans[i] + z*Meanofstds[i]/sqrt(n)
    
    XLabels[i] <- rownames(input[i*2-1,])
    
    i <- i+1
  }
  
  #factorize the XLabel:
  XLabels <- data.frame(V1 = XLabels)
  XLabels$V1 <- factor(XLabels$V1, levels = XLabels$V1)
  
  PlotMatrix <- data.frame(x = XLabels$V1,
                          Mean = Meanofmeans,
                          Con_Min = ConfidenceMin,
                          Con_Max = ConfidenceMax)
  
  p <- ggplot(PlotMatrix, aes(x = x, y = Mean)) + xlab("Cases") + ylab("Objective values") +
    geom_point(size = 4) +
    geom_errorbar(aes(ymax = Con_Max, ymin = Con_Min)) +
    theme(axis.text.x = element_text(face = "bold", size = 10))
  
  return(p)
}
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
#Reshape data
DataProcess <- function(input)
{
  output <- input[,-1] #delete the first column
  colnames(output) <- input[1,-1]
  i <- nrow(input)
  while (i > 0)
  {
    j <- i-2
    output <- output[-j,]
    i <- i-3
  }
  
  rownames(output) <- Num9_Region_neg22_neg12_RowNames
  
  output_InsideInstances_Guided <- output[1:12,1:25]
  output_InsideInstances_NotGuided <- rbind(output[1:2,1:25],output[13:26,1:25])
  output_OutsideInstances_Guided <- output[1:12,26:50]
  output_OutsideInstances_NotGuided <- rbind(output[1:2,26:50],output[13:26,26:50])
  
  output <- list(output_InsideInstances_Guided, output_InsideInstances_NotGuided, output_OutsideInstances_Guided, output_OutsideInstances_NotGuided)
  
  return(output)
}

#record only the RightNum case
DataProcess_Right0Extra9ExtraOnly <- function(input)
{
  output <- input[,-1] #delete the first column
  colnames(output) <- input[1,-1]
  i <- nrow(input)
  while (i > 0)
  {
    j <- i-2
    output <- output[-j,]
    i <- i-3
  }
  
  rownames(output) <- Num9_Region_neg22_neg12_RowNames
  
  #output <- output[1:2,1:25]
  output <- output[c("RightNum9_Mean","RightNum9_STD","NP10_Extra0_Mean","NP10_Extra0_STD","NP19_Extra9_Mean","NP19_Extra9_STD"), 1:50]
  return(output)
}
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
#find plot title:
FindPlotTitle <- function(input_string)
{
  n <- length(str_split(input_string,"_")[[1]])
  test <- c()
  for (i in 1:n)
  {
    if (grepl("Region+",sapply(str_split(input_string,"_"), "[",i))==TRUE)
      break;
    
    if (i==1)
      test <- paste(test, sapply(str_split(input_string,"_"), "[",i), sep = "")
    else
      test <- paste(test, "_", sapply(str_split(input_string,"_"), "[",i), sep = "")
  }
  #sapply(str_split(GetObjectName(CEC13_F1_FE50_Num9),"_"), "[",2)
  #test <- paste(sapply(str_split(GetObjectName(CEC13_F1_FE50_Num9),"_"), "[",2),"_",sapply(str_split(GetObjectName(CEC13_F1_FE50_Num9),"_"), "[",3), sep = "")
  
  return(test)
}

#find title without CEC, A, T, and Region...
Find3CasesTitle <- function(input_string)
{
  n <- length(str_split(input_string,"_")[[1]])
  test <- c()
  for (i in 1:n)
  {
    if (grepl("CEC+", sapply(str_split(input_string, "_"), "[",i))==TRUE)
      next;
    if (grepl("A+", sapply(str_split(input_string, "_"), "[",i))==TRUE)
      next;
    if (grepl("Tech+", sapply(str_split(input_string, "_"), "[",i))==TRUE)
      next;
    if (grepl("Region+",sapply(str_split(input_string,"_"), "[",i))==TRUE)
      break;
    
    if (length(test) == 0)
      test <- paste(test, sapply(str_split(input_string,"_"), "[",i), sep = "")
    else
      test <- paste(test, "_", sapply(str_split(input_string,"_"), "[",i), sep = "")
  }
  #sapply(str_split(GetObjectName(CEC13_F1_FE50_Num9),"_"), "[",2)
  #test <- paste(sapply(str_split(GetObjectName(CEC13_F1_FE50_Num9),"_"), "[",2),"_",sapply(str_split(GetObjectName(CEC13_F1_FE50_Num9),"_"), "[",3), sep = "")
  
  return(test)
}
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
#get name of object:
GetObjectName <- function(input)
{
  #deparse(expr, ...): turns unevaluated expressions into character strings
  #substitute(expr, env): returns the parse tree for the (unevaluated) expression expr, substituting any variables bound in env.
  #quote(expr): simply returns its argument. The argument is not evaluated and can be any R expression.
  output <- deparse(substitute(input))
  
  return(output)
}
#--------------------------------------------------------------------------
