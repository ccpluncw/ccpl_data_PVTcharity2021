library(qualtRics)
library(chutils)
library(dplyr)
library(Hmisc)

surveyFile <- "CWJenkins - pq_April 4, 2023_07.17.csv"
demoFile <- "prolific_export_6419835f478c9e89c4f2bdda.csv"
itemCodeFile <- "itemCode.txt"
cwLoadingFile <- "cwFactorLoadings.txt"

sliderBuffer <- 5

data1.raw <- data.frame(read_survey(surveyFile))
cwLoadings <- read.table(cwLoadingFile, header=T, sep="\t")
df.itemCode <- read.table(itemCodeFile, header=T, sep="\t")
df.demo <- read.csv(demoFile, header=T)

##### remove non-prolific test subjects
data1.1 <- data1.raw[!is.na(data1.raw$prolific_id), ]

numberOfRawSubjects <- nrow(data1.1)
#### get Demographics
meanAge.raw <- mean(data1.1$age, na.rm=T)
sdAge.raw <- sd(data1.1$age, na.rm=T)

gender.table.raw <- with(data1.1, table(gender)) %>% prop.table

#######
### attention check
  data1.1$passCenterAtt <- ifelse(data1.1$sadistic_50 < (50-sliderBuffer) | data1.1$sadistic_50 > (50+sliderBuffer),FALSE, TRUE)
  data1.1$passEndAtt <- ifelse(data1.1$sadistic_49 < (100-sliderBuffer),FALSE, TRUE)

  badIDs <- data1.1[data1.1$passCenterAtt == FALSE & data1.1$passEndAtt == FALSE, "prolific_id"]

  ### put code here to count the number of bad badIDs
  numBadSs <- length(badIDs)

  data1.2 <- data1.1[data1.1$passCenterAtt & data1.1$passEndAtt, ]

  #### get Demographics
  meanAge.final <- mean(data1.2$age, na.rm=T)
  sdAge.final <- sd(data1.2$age, na.rm=T)

  gender.table.final <- with(data1.2, table(gender)) %>% prop.table

##### Now caluculate Competence and Warmth

data1.m <- data.frame(t(colMeans(data1.2[,c(which( colnames(data1.2)=="sincere_2" ):which( colnames(data1.2)=="competence_47" ))], na.rm=T)))
items <- c('2', '3', '4', '5', '6', '7', '9', '10', '11', '12', '13', '14', '15','16', '17', '37', '48', '38','39', '40', '41', '42', '43', '44', '45', '46', '47')
traits <- c('sincere', 'tolerant', 'good_natured', 'trustworthy', 'friendly', 'helpful', 'moral', 'understanding', 'intelligent', 'efficient', 'skilled', 'confident', 'creative','capable', 'foresighted', 'clever', 'hunger', 'fear', 'pain', 'rage', 'desire', 'pleasure', 'pride', 'joy', 'embarrassment','communication', 'emotion_rec', 'memory', 'right_wrong', 'planning', 'self_control')

df.cw <- NULL
for(it in items) {
  tmp.c <- 0
  tmp.w <- 0
  for (tr in traits) {
    colName.tmp <- paste(tr,it,sep="_")
    tmp.c <- data1.m[1,colName.tmp] * cwLoadings[cwLoadings$traitVar == tr, "Competance"] + tmp.c
    tmp.w <- data1.m[1,colName.tmp] * cwLoadings[cwLoadings$traitVar == tr, "Warmth"] + tmp.w
  }
  cName.tmp <- paste("competence",it,sep="_")
  wName.tmp <- paste("warmth",it,sep="_")
  df.tmp <- data.frame(itemNum = it, compF = tmp.c, warmF = tmp.w, comp = data1.m[1,cName.tmp], warm = data1.m[1,wName.tmp])
  df.cw <- ch.rbind(df.cw, df.tmp)
}
  df.cw$zCompF <- scale(df.cw$compF)
  df.cw$zWarmF <- scale(df.cw$warmF)
  df.cw$zComp <- scale(df.cw$comp)
  df.cw$zWarm <- scale(df.cw$warm)

  df.cw <- merge(df.cw, df.itemCode)

  corr.table <-rcorr(as.matrix(df.cw[,6:9]),type="pearson")

  write.table(df.cw, file="CompetenceWarmthOut.txt", quote=F, sep = "\t", row.names=F)

pdf("compWamthGraphs.pdf")
  with(df.cw, plot(zCompF ~ zWarmF,bty="n", ylim=c(-3,3), xlim=c(-2,2), main = "factor combinations"))
  with(df.cw, text(zWarmF, zCompF-.1, labels=item, cex=0.5))
  with(df.cw, plot(zComp ~ zWarm,bty="n", ylim=c(-3,3), xlim=c(-2,2), main = "items"))
  with(df.cw, text(zWarm, zComp-.1, labels=item, cex=0.5))
dev.off()

sink("CWoutStats.txt")
  cat("Number of Raw Subjects:", numberOfRawSubjects, "\n")
  cat("Age of Raw Subjects: Mean: ", meanAge.raw, " SD: ", sdAge.raw, "\n")
  cat("Gender of Raw Subjects:\n")
  print(gender.table.raw)
  cat("\n\nNumber of Subjects Removed for Attention Check:", numBadSs, "\n")
  cat("\n\nTotal Number of Subjects in Analysis:",numberOfRawSubjects- numBadSs, "\n")
  cat("Age of Raw Subjects: Mean: ", meanAge.final, " SD: ", sdAge.final, "\n")
  cat("Gender of Raw Subjects:\n")
  print(gender.table.final)
  cat("\n\nCorrelation between Competence and Warmth:\n")
  print(corr.table)
sink(NULL)

write.table(badIDs, file="CWbadSubs.txt", quote=F, sep = "\t", row.names=F, col.names=F)
