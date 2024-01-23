
library(RRW)
library(chutils)

############## smartGridSearch and simulation parameters
#the number of simulations to average using the final parameters.
numSimsToAverage <- 40
######################################################

mainDir <- getwd()

source("modelspecs_CharityIngroup.r")

if(!is.null(allModels)) {
  modelNames <- names(allModels)
    for (i in modelNames) {
      subDirExists <- ch.newDir (mainDir, i)
      #if the sub directory does NOT exist or you do NOT want to skip models, then run the analysis
      if(subDirExists == TRUE) {
        modDir <- getwd()
        setwd(modDir)

        datFileName <- list.files(modDir, full.names=TRUE, pattern="Fitted.txt", all.files=T)

        tmpModelList <- allModels[[i]]

        #create a file tag that will be used to save files.  it will be time and date stamped
        fileTagShortName <- paste("Plot", i, sep="_")
        fileTag <- paste(format(Sys.time(), "%b_%d_%Y_%H-%M"), fileTagShortName, sep="_")

        df.fitted <- read.table(datFileName, header=T, sep="\t")

        rrwPlotSGSoutput(df.fitted, tmpModelList, dataRtCol = "rt", dataPhitCol = "pHit", rtFitCol = "rtFit", pHitFitCol = "pCross", correctCol = "correct", overlapCol = "overlap", fileTag = fileTag,numSimsToPlot = numSimsToAverage)
      }
      setwd(mainDir)
  }
}

# ### this loop runs the fits fixed parameters from previous runs
if(!is.null(allFixedModels)) {
  modelNames <- names(allFixedModels)

  for (i in modelNames) {
    subDirExists <- ch.newDir (mainDir, i)
    #if the sub directory does NOT exist or you do NOT want to skip models, then run the analysis
    if(subDirExists == TRUE) {
      modDir <- getwd()
      setwd(modDir)

      datFileName <- list.files(modDir, full.names=TRUE, pattern="Fitted.txt", all.files=T)

      tmpModelList <- allFixedModels[[i]]

      #create a file tag that will be used to save files.  it will be time and date stamped
      fileTagShortName <- paste("terrorist", i, sep="_")
      fileTag <- paste(format(Sys.time(), "%b_%d_%Y_%H-%M"), fileTagShortName, sep="_")

      #do this to run a set of fixed parameters.  Remember, the rrwModelList should have the upperBound==lowerBound
      df.fitted <- read.table(datFileName, header=T, sep="\t")

      rrwPlotSGSoutput(df.fitted, tmpModelList, dataRtCol = "rt", dataPhitCol = "pHit", rtFitCol = "rtFit", pHitFitCol = "pCross", correctCol = "correct", overlapCol = "overlap", fileTag = fileTag,numSimsToPlot = numSimsToAverage)
    }
    setwd(mainDir)
  }
}
