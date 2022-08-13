library(chMorals)
library(chutils)
library(RRW)

#set up the new RT variables
fitCol<-"fit.RT"
resCol<-"res.RT"
useTwoParameterModel <- FALSE
respChoiceVal <- c("Donate", "Keep")

# read in parameters
params<-ch.readMoralsDBfile("charityYouDBfile.txt")

#set up the group and item directories
mainDir <- getwd()
ch.newDir (mainDir, params$gpSubDir)
gpDir <- getwd()
setwd(mainDir)

ch.newDir (mainDir, params$itemSubDir)
itemDir <- getwd()
setwd(mainDir)

statsOutputFile <- file.path(mainDir,paste(params$dt.set, params$statsOutputFilePrefix))
sink(statsOutputFile, append = F)
  cat("\n***** New Run ****\n\n")
sink(NULL)


### read in data
data.raw <- read.table(params$moralsTaskDataFile, header = T, sep="\t")
data.ovrlp <- read.table(params$valueOverlapDataFile, header=T, sep="\t", quote="\"")

######_____REMOVE PRACTICE TRIALS _____######
data.raw<-data.raw[data.raw$trial_type>=1, ]
data.raw$ref_holder<-"refdist"

### do Prep analysis
processedData <- ch.moralsDataPrep(data.raw, "sn", "keybRT", "overlap", "direction", "trial", "keyDef", respChoiceVal = c("Donate", "Keep"), item1cols = c("Item"), item2cols = c("ref_holder"), overlapItem1cols = c("IA1"), overlapItem2cols = c("IB1"), statsOutputFile = statsOutputFile, params = params)

### Filter data
analysisReadyData <-ch.moralsFilterDataQ(processedData, "sn", "keybRT", "overlapRound", "correct", c(1,0), statsOutputFile = statsOutputFile, params = params)

### Do RT and p(Hit Analysis on Group Data - remove learning effects for the group)
analysisReadyData.gp <-ch.moralsGrpRTpHit(analysisReadyData, "trial", "keybRT", fitCol, resCol, "overlapRound", "keyDef", c("Donate", "Keep"), "correct", c(1,0), useTwoParameterModel = useTwoParameterModel, params = params)
write.table(analysisReadyData.gp, file="analysisReadyData.gp.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

### Do RT and p(Hit Analysis on individual subject Data - remove learning effects for each subject)
analysisReadyData.sn <- ch.moralsSnRTpHit(analysisReadyData, "sn", "trial", "keybRT", fitCol, resCol, "overlap", "correct", c(1,0), useTwoParameterModel = useTwoParameterModel, params = params)
write.table(analysisReadyData.sn, file="analysisReadyData.sn.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

#Do d'analysis as a group, but use the data whereby the learning effects were removed by subject
df.dPrime <- ch.moralsDprimeAnalysis(analysisReadyData.sn, "overlapRound", "correct", c(1,0), "targetPresent", c(TRUE,FALSE), resCol, params = params, filenameID = "gp")

#Do an item analysis on the data.  Doesn't matter whether use group or sn data - no rt analysis is done
itemAnalDat <- ch.moralsItemChoiceAnalysis(analysisReadyData.gp, "Item", "you_holder", "overlapRound", "dirOverlap","keyDef", respChoiceVal = c("Donate", "Keep"), params = params, saveFigures = T, comparisonItemName = "you")

analysisReadyData.sn$refValue<-ifelse(analysisReadyData.sn$dirOverlap < 0, "refHVO", "refLVO")
grpFitModels <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.sn, c("refValue"), resCol, "overlapRound", "keyDef", yesNoVal = respChoiceVals, "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = TRUE, params = params, minNperOverlap = params$minOverlapN)
### and plot the data
setwd(gpDir)
ch.moralsPlotFitsByGrps(grpFitModels, c("refValue"), "overlapRound", analysisReadyData.sn, filenameID = params$dt.set)
setwd(mainDir)

grpFitModels <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.sn, c("typeOfScen"), resCol, "overlapRound", "keyDef", yesNoVal = respChoiceVals, "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = TRUE, params = params, minNperOverlap = params$minOverlapN)
### and plot the data
setwd(gpDir)
ch.moralsPlotFitsByGrps(grpFitModels, c("typeOfScen"), "overlapRound", analysisReadyData.sn, filenameID = params$dt.set)
setwd(mainDir)

grpFitModels <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.sn, c("refValue","typeOfScen"), resCol, "overlapRound", "keyDef", yesNoVal = respChoiceVals, "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = TRUE, params = params, minNperOverlap = params$minOverlapN)
### and plot the data
setwd(gpDir)
ch.moralsPlotFitsByGrps(grpFitModels, c("refValue","typeOfScen"), "overlapRound", analysisReadyData.sn, filenameID = params$dt.set)
setwd(mainDir)

setwd(mainDir)
