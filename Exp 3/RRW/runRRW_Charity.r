
require(dplyr)
library(RRW)
library(smartGridSearch)
library(chutils)

test <- FALSE
#use multicore
useMultiCore <- TRUE
#fit free parameters
runSmartGridSearch <- TRUE
#Only run new models that have not been run before?
skipModelsWithExistingDirs <- TRUE

multicorePackages = c('RRW')

mainDir <- getwd()

############## smartGridSearch and simulation parameters
#"BIC" "AIC" "R_Square"
minimizeStat <- "BIC"
#do you want to equalize the influence of RT and pHit on the minimization statistic (TRUE) or have the influence be a function of the number of observations (FALSE)
equalizeRTandPhit <- TRUE
#the minimum number of trials per overlapRound to include in the analysis.
minN <- 40
#number of intervals to split each bounded parameter into in order to get the random values between the hi and low values
numGridIntervals <- 100
#the number of times to loop with grid search before looking for the top 10 best fits.
numGridLoops <- ifelse (test, 10, 2500)
#the number of loops for each RW step.
loopsPerRWstep <- ifelse (test, 20, 500)
#paramtable confirmation loops
optBoundLoops <- ifelse (test, 4, 10)
#The number of best fit runs from which to resize the parameter bounds.
#Here, I set it to the sqrt(total number of runs per grid loop) or 10 at minimum.
  #optParamListN <- ifelse ( trunc(sqrt(numGridLoops)) < 10, 10, trunc(sqrt(numGridLoops)) )
optParamListN <- 10
#the number of simulations to average using the final parameters.
numSimsToAverage <- 40
######################################################

mainDir <- getwd()
inputDataFile <- "../analysisReadyData.sn.txt"
analysisReadyData<-read.table(inputDataFile, header=T, sep="\t")


#create an effect code for in group/out group. (HVI-LVI) UNCW-UNCW==0; Other-other == 0; UNCW-Other = 1; Other-UNCW = -1
# analysisReadyData$HVI<-ifelse(analysisReadyData$HigherItem=="UNCW", 1, 0)
# analysisReadyData$LVI<-ifelse(analysisReadyData$LowerItem=="UNCW", -1, 0)
# analysisReadyData$inGroupEffect<-(analysisReadyData$HVI + analysisReadyData$LVI)
#analysisReadyData$inGroupConsistency<-ifelse(analysisReadyData$inGroupEffect==0, "HVOsame-LVOsame", ifelse(analysisReadyData$inGroupEffect==1, "HVOin-LVOout", "HVOout-LVOin"))

analysisReadyData$HVO<-ifelse(analysisReadyData$HigherItem=="UNCW", "in", "out")
analysisReadyData$LVO<-ifelse(analysisReadyData$LowerItem=="UNCW", "in", "out")
analysisReadyData$inGroupConsistency<- paste("HVO", analysisReadyData$HVO,"-", "LVO", analysisReadyData$LVO, sep="")
analysisReadyData$inGroupConsistency<-ifelse(analysisReadyData$inGroupConsistency == "HVOout-LVOout" | analysisReadyData$inGroupConsistency == "HVOin-LVOin", "HVOsame-LVOsame", analysisReadyData$inGroupConsistency)

tmp.df<-analysisReadyData

### direct.xvy = -1 means the lower valued object is on the left
### direct.xvy = 1 means the higher valued object is on the left
tmp.df$leftItem <- ifelse(tmp.df$direct.xVy < 0 , "leftLVO","leftHVO")

source("modelspecs_CharityIngroup.r")
modelNames <- names(allModels)

### this loop runs the smartGridSearch to optimize free parameters
if(runSmartGridSearch) {
  for (i in modelNames) {
    subDirExists <- ch.newDir (mainDir, i)
    #if the sub directory does NOT exist or you do NOT want to skip models, then run the analysis
    if(subDirExists == FALSE | skipModelsWithExistingDirs == FALSE) {
      modDir <- getwd()
      setwd(modDir)

      tmpModelList <- allModels[[i]]

      #create a file tag that will be used to save files.  it will be time and date stamped
      fileTagShortName <- paste("RespBias", i, sep="_")
      fileTag <- paste(format(Sys.time(), "%b_%d_%Y_%H-%M"), fileTagShortName, sep="_")

      df.fitted <- rrwRunSmartGridSearch(tmp.df, tmpModelList, minN = minN, dataOverlapCol = "overlapRound", RwSamplesCol = "Q50", dataRtCol = "res.RT", correctCol = "correct01", correctVals = c(1,0), loopsPerRWstep = loopsPerRWstep, minimizeStat = minimizeStat, equalizeRTandPhit = equalizeRTandPhit, numLoops = numGridLoops, numIntervals = numGridIntervals, optParamListN = optParamListN, optBoundLoops = optBoundLoops, multicore = useMultiCore, multicorePackages = multicorePackages, numSimsToAverage = numSimsToAverage, fileTag = fileTag)

      rrwPlotSGSoutput(df.fitted, tmpModelList, dataRtCol = "rt", dataPhitCol = "pHit", rtFitCol = "rtFit", pHitFitCol = "pCross", correctCol = "correct", overlapCol = "overlap", fileTag = fileTag,numSimsToPlot = numSimsToAverage)
    }
    setwd(mainDir)
  }
}

### this loop runs the fits fixed parameters from previous runs
if(!is.null(allFixedModels)) {
  modelNames <- names(allFixedModels)

  for (i in modelNames) {
    subDirExists <- ch.newDir (mainDir, i)
    #if the sub directory does NOT exist or you do NOT want to skip models, then run the analysis
    if(subDirExists == FALSE | skipModelsWithExistingDirs == FALSE) {
      modDir <- getwd()
      setwd(modDir)

      tmpModelList <- allFixedModels[[i]]

      #create a file tag that will be used to save files.  it will be time and date stamped
      fileTagShortName <- paste("terrorist", i, sep="_")
      fileTag <- paste(format(Sys.time(), "%b_%d_%Y_%H-%M"), fileTagShortName, sep="_")

      #do this to run a set of fixed parameters.  Remember, the rrwModelList should have the upperBound==lowerBound
      df.fitted <- rrwRunWithFixedParameters(tmp.df, tmpModelList, minN = minN, dataOverlapCol = "overlapRound", RwSamplesCol = "Q50", dataRtCol = "res.RT", correctCol = "correct01", correctVals = c(1,0), loopsPerRWstep = loopsPerRWstep, minimizeStat = minimizeStat, equalizeRTandPhit = equalizeRTandPhit, numSimsToAverage = numSimsToAverage, fileTag = fileTag)

      rrwPlotSGSoutput(df.fitted, tmpModelList, dataRtCol = "rt", dataPhitCol = "pHit", rtFitCol = "rtFit", pHitFitCol = "pCross", correctCol = "correct", overlapCol = "overlap", fileTag = fileTag,numSimsToPlot = numSimsToAverage)
    }
    setwd(mainDir)
  }
}
