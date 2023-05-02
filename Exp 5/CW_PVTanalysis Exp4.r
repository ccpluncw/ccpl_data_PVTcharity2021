library(chMorals)
library(chutils)
library(dplyr)
library(effectsize)


#set up the group and item directories
mainDir <- getwd()
setwd(file.path(mainDir, "exp4"))

data.sn <- read.table("analysisReadyData.sn.txt", header=T, sep="\t")
data.sn$donate <- ifelse(data.sn$keyDef == "Donate", 1, 0)

df.distance <- read.table("charityDistanceMeasures.txt", header=T, sep="\t")
data.cw <- read.table("../CompetenceWarmthOut.txt", header=T, sep="\t")

sink("../CW_PVT.stat_Exp4.txt")
  cat("\n\n ******* Distance by pDonate Logistic *******\n\n")
  cat("\n\n************************* Preregistered Analysis *************************n\n")

sink(NULL)

  ### summarize Data
  df.summary.item <- data.frame(data.sn %>% group_by(Item) %>% summarise (mRT = mean(res.RT, na.rm = T), pDonate = mean(donate, na.rm=T)))
  ### merge summary with PVT distance information
  df.summary.item <- merge(df.summary.item,df.distance, by.x = "Item", by.y="IA1")
  ### merge summary with CW distance information
  df.summary.item <- merge(df.summary.item,data.cw, by.x = "Item", by.y="item")
  #### create CW prediction variable - but note that same results if only use zWarmF
  df.summary.item$CWpred <- coef(cw.lm)["wDiff"] * df.summary.item$zWarmF + coef(cw.lm)["cwDiff"] * (df.summary.item$zWarmF*df.summary.item$zCompF)

  pdf("distanceBypDonatePVT.pdf")
  ### do PVT assessment
    df.summary.item <- df.summary.item[order(df.summary.item$DO),]
    lFit.pvt <- with(df.summary.item, ch.logistic (DO, pDonate, parameters = c(bottom = 0, slope = 5), fixedMaxX = 1))

    df.summary.item$PVTfit <- predict(lFit.pvt$fit)
    df.summary.item <- df.summary.item[order(df.summary.item$DO),]
    with(df.summary.item, plot(pDonate~DO, ylim = c(0,1), xlim = c(-1,1), main = "PVT All Data", bty="n"))
    with(df.summary.item, text(DO, pDonate-0.02, labels=Item, cex=0.5))
    with(df.summary.item, lines(PVTfit~DO))

    ### do CW assessment
    df.summary.item <- df.summary.item[order(df.summary.item$CWpred),]
    lFit.cw <- with(df.summary.item, ch.logistic (CWpred, pDonate, parameters = c(bottom = 0, slope = 5)))

    df.summary.item$CWfit <- predict(lFit.cw$fit)
    df.summary.item <- df.summary.item[order(df.summary.item$zWarmF),]
    with(df.summary.item, plot(pDonate~CWpred, ylim = c(0,1), xlim = c(-0.5,0.5), main = "CW All Data", bty="n"))
    with(df.summary.item, text(CWpred, pDonate-0.02, labels=Item, cex=0.5))
    with(df.summary.item, lines(CWfit~CWpred))

  sink("../CW_PVT.stat_Exp4.txt", append=T)
    cat("\n\n*** OVERALL data  ***\n\n")
    cat("\n\n*** CW BIC & R2 for Choice   ***\n")
      print(summary(lFit.cw$fit))
      cat("\n BIC =", BIC(lFit.cw$fit))
      cat("\n R Square =", lFit.cw$r2)
    cat("\n\n*** PVT BIC & R2 for Choice   ***\n")
      print(summary(lFit.pvt$fit))
      cat("\n BIC =", BIC(lFit.pvt$fit))
      cat("\n R Square =", lFit.pvt$r2)
  sink(NULL)


types <- unique(data.sn$typeOfScen)
for(tp in types) {
  #select appropriate subset
  df.tmp <- data.sn[data.sn$typeOfScen == tp,]

  ### summarize Data
  df.summary.item <- data.frame(df.tmp %>% group_by(Item) %>% summarise (mRT = mean(res.RT, na.rm = T), pDonate = mean(donate, na.rm=T)))
  ### merge summary with PVT distance information
  df.summary.item <- merge(df.summary.item,df.distance, by.x = "Item", by.y="IA1")
  ### merge summary with CW distance information
  df.summary.item <- merge(df.summary.item,data.cw, by.x = "Item", by.y="item")
  #### create CW prediction variable - but note that same results if only use zWarmF
  df.summary.item$CWpred <- coef(cw.lm)["wDiff"] * df.summary.item$zWarmF + coef(cw.lm)["cwDiff"] * (df.summary.item$zWarmF*df.summary.item$zCompF)
  ### do PVT assessment
    df.summary.item <- df.summary.item[order(df.summary.item$DO),]
    lFit.pvt <- with(df.summary.item, ch.logistic (DO, pDonate, parameters = c(bottom = 0, slope = 5), fixedMaxX = 1))

    df.summary.item$PVTfit <- predict(lFit.pvt$fit)
    with(df.summary.item, plot(pDonate~DO, ylim = c(0,1), xlim = c(-1,1), main = paste("PVT",tp), bty="n"))
    with(df.summary.item, text(DO, pDonate-0.02, labels=Item, cex=0.5))
    with(df.summary.item, lines(PVTfit~DO))

    ### do CW assessment
    df.summary.item <- df.summary.item[order(df.summary.item$CWpred),]
    lFit.cw <- with(df.summary.item, ch.logistic (CWpred, pDonate, parameters = c(bottom = 0, slope = 5)))

    df.summary.item$CWfit <- predict(lFit.cw$fit)
    with(df.summary.item, plot(pDonate~CWpred, ylim = c(0,1), xlim = c(-0.5,0.5), main = paste("CW",tp), bty="n"))
    with(df.summary.item, text(CWpred, pDonate-0.02, labels=Item, cex=0.5))
    with(df.summary.item, lines(CWfit~CWpred))

  sink("../CW_PVT.stat_Exp4.txt", append=T)
    cat("\n\n ******* Condition:", tp, " *******")
    cat("\n\n*** CW BIC & R2 for Choice   ***\n")
      print(summary(lFit.cw$fit))
      cat("\n BIC =", BIC(lFit.cw$fit))
      cat("\n R Square =", lFit.cw$r2)
    cat("\n\n*** PVT BIC & R2 for Choice   ***\n")
      print(summary(lFit.pvt$fit))
      cat("\n BIC =", BIC(lFit.pvt$fit))
      cat("\n R Square =", lFit.pvt$r2)
  sink(NULL)
}
dev.off()
setwd(mainDir)
