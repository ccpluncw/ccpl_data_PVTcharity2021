library(chMorals)
library(chutils)

#set up the new RT variables
fitCol <- "fit.RT"
resCol <- "res.RT"
useTwoParameterModel <- FALSE

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
data.raw <-read.table(params$moralsTaskDataFile, header=T, sep="\t")
data.ovrlp <-read.table(params$valueOverlapDataFile, header=T, sep="\t", quote="\"")

######_____REMOVE PRACTICE TRIALS _____######
data.raw <- data.raw[data.raw$trial_type >=1, ]
head(data.raw)

### do Prep analysis
processedData<-ch.moralsDataPrep(data.raw, "sn", "keybRT", "overlap", "direction", "trial", "keyDef", respChoiceVal = c("Left Item", "Right Item"), item1cols = c("Item1"), item2cols = c("Item2"), overlapItem1cols = c("IA1"), overlapItem2cols = c("IB1"), statsOutputFile = statsOutputFile, params = params)

### if Item1 is the higher-valued item and is from UNCW, then print out UNCW. if Item2 is the higher-valued item and is from UNCW, then print out UNCW
processedData$HigherItem<-ifelse( (processedData$direct.xVy==1 & processedData$Group1=="UNC  Wilmington alumnus") |(processedData$direct.xVy==-1 & processedData$Group2=="UNC  Wilmington alumnus"), "UNCW", "other")

##if the lower-valued item is on the right and is from UNCW
processedData$LowerItem<-ifelse( (processedData$direct.xVy==-1 & processedData$Group1=="UNC  Wilmington alumnus")| (processedData$direct.xVy==1 & processedData$Group2 == "UNC  Wilmington alumnus"), "UNCW", "other")

analysisReadyData <- ch.moralsFilterDataQ(processedData, "sn", "keybRT", "overlapRound", "correct",c(1,0), statsOutputFile = statsOutputFile, params = params)

analysisReadyData.gp <- ch.moralsGrpRTpHit(analysisReadyData, "trial", "keybRT", fitCol, resCol, "overlapRound", "keyDef",c("Left Item", "Right Item"), "correct",c(1,0), useTwoParameterModel = useTwoParameterModel, params = params)
write.table(analysisReadyData.gp, file="analysisReadyData.gp.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

### Do RT and p(Hit Analysis on individual subject Data - remove learning effects for each subject)
analysisReadyData.sn <- ch.moralsSnRTpHit(analysisReadyData, "sn", "trial", "keybRT", fitCol, resCol, "overlap", "correct", c(1,0), useTwoParameterModel = useTwoParameterModel, params = params)
write.table(analysisReadyData.sn, file="analysisReadyData.sn.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)


#Do d'analysis as a group, but use the data whereby the learning effects were removed by subject
df.dPrime <- ch.moralsDprimeAnalysis(analysisReadyData.sn, "overlapRound", "correct", c(1,0), "targetPresent", c(TRUE,FALSE), resCol, params = params, filenameID = "gp")

#Do an item analysis on the data.  Doesn't matter whether use group or sn data - no rt analysis is done
itemAnalDat <- ch.moralsItemChoiceAnalysis(analysisReadyData.gp, "Item1", "Item2", "overlapRound", "dirOverlap","keyDef", respChoiceVal = c("Left Item", "Right Item"), params = params, saveFigures = T)

#### For experiments with catagory variable manipulations (e.g., different groups), do an analysis
#### by group
    grpFitModels <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.gp, c("HigherItem", "LowerItem"), resCol, "overlapRound", "keyDef", yesNoVal = c("Left Item", "Right Item"), "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = useTwoParameterModel, params = params, minNperOverlap = params$minOverlapN)
    ### and plot the data
    setwd(gpDir)
    ch.moralsPlotFitsByGrps(grpFitModels, c("HigherItem", "LowerItem"), "overlapRound", analysisReadyData.gp, filenameID = params$dt.set)
    setwd(mainDir)

write.table(df.dPrime, file="df.dPrime.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
