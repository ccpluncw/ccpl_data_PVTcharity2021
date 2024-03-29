library(chMorals)
library(chutils)
library(dplyr)
library(chValues)
library(effectsize)

#set up the new RT variables
useTwoParameterModel <- TRUE

#set up the group and item directories
mainDir <- getwd()
setwd(file.path(mainDir, "exp1"))

#read in the data
data.sn <- read.table("analysisReadyData.sn.txt", header=T, sep="\t")
### combine data so that items do not show up on both left and right
  data.sn.1 <- NULL
  Item1 <- unique(data.sn$Item1)
  for(i in 1:(length(Item1)-1)) {
    for(j in (i+1):length(Item1)) {
      df.tmp.1 <- data.sn[data.sn$Item1 == Item1[i] & data.sn$Item2 == Item1[j], ]
      #### if Item1[i] is in Item2, switch item sides and response choice to remove effect of side
      df.tmp.r <- data.sn[data.sn$Item1 == Item1[j] & data.sn$Item2 == Item1[i], ]
      df.tmp.r$Item1 <- Item1[i]
      df.tmp.r$Item2 <- Item1[j]
      df.tmp.r$keyDef <- ifelse(df.tmp.r$keyDef == "Left Item", "Right Item", "Left Item")
      df.tmp.r$direct.xVy <- ifelse(df.tmp.r$direct.xVy == -1, 1, -1)
      df.tmp.r$dirOverlap <- df.tmp.r$dirOverlap * -1

      data.sn.1 <- ch.rbind(data.sn.1, df.tmp.r)
      data.sn.1 <- ch.rbind(data.sn.1, df.tmp.1)
    }
  }
#now we can calculate the probability they choose the left item - which is item[i,j] specific
data.sn.1$chooseLeft <- ifelse(data.sn.1$keyDef == "Left Item", 1, 0)

## summarize the data by item[i,j]
data.sn.sum <- data.frame(data.sn.1 %>% group_by(Item1, Item2) %>% summarise(aveRT = mean(res.RT, na.rm=T), medianRT = median(res.RT, na.rm=T), pItem1 = mean(chooseLeft, na.rm=T), pHVO = mean(correct, na.rm=T), n =sum(!is.na(chooseLeft)), overlap = mean(overlap, na.rm=T) ) )

## now read in the competence and warmth data
data.cw <- read.table("../CompetenceWarmthOut.txt", header=T, sep="\t")
data.cw <- data.cw[,6:10]

#create all combinations of the items to be used for the predictions
cw.predict <- ch.getCombinations(data.cw$item, minNumPerSide = 1, maxNumPerSide = 1, sample = "all", nStimPerGroup = 0, includeReverseOrder = TRUE)
cw.predict <- cw.predict[,1:2]
names(cw.predict) <- c("Item1", "Item2")
## now merge with the competence and warmth data
cw.predict <- merge(cw.predict, data.cw, by.x=c("Item1"), by.y=("item"))
cw.predict <- cw.predict %>% rename (
  zCompF.I1 = zCompF,
  zWarmF.I1 = zWarmF,
  zComp.I1 = zComp,
  zWarm.I1 = zWarm
)
cw.predict <- merge(cw.predict, data.cw, by.x=c("Item2"), by.y=("item"))
cw.predict <- cw.predict %>% rename (
  zCompF.I2 = zCompF,
  zWarmF.I2 = zWarmF,
  zComp.I2 = zComp,
  zWarm.I2 = zWarm
)

## calculate the predictor variables
cw.predict$wDiff <- (cw.predict$zWarmF.I1 - cw.predict$zWarmF.I2)
cw.predict$cDiff <- (cw.predict$zCompF.I1 - cw.predict$zCompF.I2)
cw.predict$cwDiff <- (cw.predict$zWarmF.I1 * cw.predict$zCompF.I1) - (cw.predict$zWarmF.I2 * cw.predict$zCompF.I2)

data.sn.2 <- merge(data.sn.sum, cw.predict, by=c("Item1", "Item2"))

#predict pItem1 with full model
fullCW.lm <- with(data.sn.2, lm(pItem1 ~ wDiff + cDiff + cwDiff))
#predict pItem1 with only warmth
w.lm <- with(data.sn.2, lm(pItem1 ~ wDiff))
#predict pItem1 with only warmth and interaction
cw.lm <- with(data.sn.2, lm(pItem1 ~ wDiff + cwDiff))

############  Now Save the CW predictions

    ### create prediction dataset from fit for this dataset
    data.sn.2$CWpred <- (coef(cw.lm)["wDiff"] * data.sn.2$wDiff + coef(cw.lm)["cwDiff"] * data.sn.2$cwDiff)
    ### identify correct answer based on CW
    data.sn.2$CWdirection <- ifelse(data.sn.2$CWpred > 0, "Left Item", "Right Item")
    ### make CW predict independent of side (right or left) in preparation for next steps
    data.sn.2$CWpred <- abs(data.sn.2$CWpred)


    ### create prediction dataset from fit for all combinations for future data
    cw.predict$CWpred <- (coef(cw.lm)["wDiff"] * cw.predict$wDiff + coef(cw.lm)["cwDiff"] * cw.predict$cwDiff)
    ### identify correct answer based on CW
    cw.predict$CWdirection <- ifelse(cw.predict$CWpred > 0, "Left Item", "Right Item")
    ### make CW predict independent of side (right or left) in preparation for next steps
    cw.predict$CWpred <- abs(cw.predict$CWpred)

    corr.cw.w <- with(cw.predict, cor(CWpred, abs(wDiff)))

    ### retain only important columns
    data.sn.3 <- cw.predict[,c("Item1", "Item2", "CWpred", "CWdirection")]
    write.table(data.sn.3, file="../CompetenceWarmthPredictors.txt", quote=F, sep = "\t", row.names=F)

    data.sn.m1 <- merge(data.sn.1, data.sn.3, by=c("Item1", "Item2"))
    #identify whether the participant gave the correct answer as predicted by the CW model
    data.sn.m1$correctCW <- ifelse(data.sn.m1$CWdirection == data.sn.m1$keyDef, 1, 0)
    ## summarize the data by item[i,j]
    data.sn.sum.m <- data.frame(data.sn.m1 %>% group_by(Item1, Item2) %>% summarise(aveRT = mean(res.RT, na.rm=T), medianRT = median(res.RT, na.rm=T), pHVO = mean(correct, na.rm=T), n =sum(!is.na(correctCW)), overlap = mean(overlap, na.rm=T), mCWpred = mean(CWpred, na.rm = T), pCW = mean(correctCW, na.rm = T) ) )

###### do overall analysis
    cw.choice.lm <- with(data.sn.sum.m, lm(pCW ~ mCWpred))
    cw.rt.lm <- with(data.sn.sum.m, lm(aveRT ~ mCWpred))
    pvt.choice <- with(data.sn.sum.m, ch.pHVOfit(overlap, pHVO, useTwoParameterModel = useTwoParameterModel))
    pvt.rt.lm <- with(data.sn.sum.m, lm(aveRT ~ overlap))
    #### Plot overall analysis
    data.sn.sum.m <- data.sn.sum.m[order(data.sn.sum.m$mCWpred),]

    op <- par(mfrow=c(2,1),bty="n", font=1, family='serif', mar=c(2,5,2,5), oma=c(3,0,3,0), cex=1.25, las=1)

    fileName <- "allData_pHitRTcw.pdf"

      ch.plot.lm(data.sn.sum.m$mCWpred, data.sn.sum.m$pCW, cex1 = 1.25, yLabel  = "p(CW)", printR2 = F)
      ch.plot.lm(data.sn.sum.m$mCWpred, data.sn.sum.m$aveRT, cex1 = 1.25,  yLabel  = "RT", printR2 = F)
      mtext("All Data", outer = TRUE, cex = 1.25)
    dev.copy(pdf, fileName, width=6, height=9)
    dev.off();

    data.sn.sum.m <- data.sn.sum.m[order(data.sn.sum.m$overlap),]
    fileName <- "allData_pHitRTpvt.pdf"

      ch.plot.pHit(data.sn.sum.m$overlap, data.sn.sum.m$pHVO, useTwoParameterModel = useTwoParameterModel,  cex1 = 1, printR2 = F,yLabel="p(HVO)")
      ch.plot.lm(data.sn.sum.m$overlap, data.sn.sum.m$aveRT, cex1 = 1.25,  yLabel  = "RT", printR2 = F)
      mtext("All Data", outer = TRUE, cex = 1.25)
    dev.copy(pdf, fileName, width=6, height=9)
    dev.off();

    par(op)
##### Now calculate how well CW vs PVT predicts choice for each item paired with each other item - by item.
##### Get BIC and R2
df.out <- NULL
op <- par(mfrow=c(2,1),bty="n", font=1, family='serif', mar=c(2,5,2,5), oma=c(3,0,3,0), cex=1.25, las=1)
for(pr in Item1) {
  df.tmp <- data.sn.sum.m %>% filter_at(vars(c("Item1", "Item2")), any_vars(. == pr))
  fileName <- paste(pr,"pHitRTcw.pdf")

  y1Fit <- ch.plot.lm(df.tmp$mCWpred, df.tmp$pCW, cex1 = 1.25, yLabel  = "p(CW)", printR2 = F)
  y2Fit <- ch.plot.lm(df.tmp$mCWpred, df.tmp$aveRT, cex1 = 1.25,  yLabel  = "RT", printR2 = F)
  mtext(pr, outer = TRUE, cex = 1.25)
  dev.copy(pdf, fileName, width=6, height=9)
  dev.off();

  df.tmp<- df.tmp[order(df.tmp$overlap),]
  fileName <- paste(pr,"pHitRTpvt.pdf")

    y3Fit <- ch.plot.pHit(df.tmp$overlap, df.tmp$pHVO, useTwoParameterModel = useTwoParameterModel,  cex1 = 1, printR2 = F, yLabel="p(HVO)")
    y4Fit <- ch.plot.lm(df.tmp$overlap, df.tmp$aveRT, cex1 = 1.25, yLabel  = "RT", printR2 = F)
    mtext(pr, outer = TRUE, cex = 1.25)
    dev.copy(pdf, fileName, width=6, height=9)
    dev.off();


    df.tmp.out <- data.frame(probe = pr, CWchoiceR2 = summary(y1Fit)$r.squared, CWrtR2 = summary(y2Fit)$r.squared, CWbicChoice = BIC(y1Fit), CWbicRt = BIC(y2Fit), PVTchoiceR2 = y3Fit$r2, PVTrtR2 = summary(y4Fit)$r.squared, PVTbicChoice =BIC(y3Fit$nlsObject) , PVTbicRt = BIC(y4Fit))

  df.out <- ch.rbind(df.out, df.tmp.out)
}
par(op)

#get means and SDs
fitMeans.l <- sapply(df.out[,2:9], mean)
fitSDs.l <- sapply(df.out[,2:9], sd)

### Now calculate whether CW or PVT predicted more accurately Using the BIC (primary measure)
BICttestChoice.l <- with(df.out, t.test(CWbicChoice, PVTbicChoice, paired = T))
BICttestRT.l <- with(df.out, t.test(CWbicRt, PVTbicRt, paired = T))
### Now calculate whether CW or PVT predicted more accurately Using the R2 for fun
r2ttestChoice.l <- with(df.out, t.test(CWchoiceR2, PVTchoiceR2, paired = T))
r2ttestRT.l <- with(df.out, t.test(CWrtR2, PVTrtR2, paired = T))

### Now calculate effect sizes
BICcohensdChoice.l <- t_to_d(BICttestChoice.l$statistic, BICttestChoice.l$parameter, paired = TRUE, ci = 0.95, alternative = "two.sided")$d
BICcohensdRT.l <- t_to_d(BICttestRT.l$statistic, BICttestRT.l$parameter, paired = TRUE, ci = 0.95, alternative = "two.sided")$d
### Now calculate whether CW or PVT predicted more accurately Using the R2 for fun
r2cohensdChoice.l <- t_to_d(r2ttestChoice.l$statistic, r2ttestChoice.l$parameter, paired = TRUE, ci = 0.95, alternative = "two.sided")$d
r2cohensdRT.l <- t_to_d(r2ttestRT.l$statistic, r2ttestRT.l$parameter, paired = TRUE, ci = 0.95, alternative = "two.sided")$d

sink("../CW_PVT.stat_Exp1.txt")
  cat("\n*** Competence and Warmth predicting pItem1 with full model  ***\n")
  print(summary(fullCW.lm))
  print(BIC(fullCW.lm))
  cat("\n\n*** Warmth alone predicting pItem1  ***\n")
  print(summary(w.lm))
  print(BIC(w.lm))
  cat("\n\n*** Warmth and interaction predicting pItem1  ***\n")
  print(summary(cw.lm))
  print(BIC(cw.lm))
  cat("\n\n*** Correlation between CW prediction and abs(Warmth_i - Warmth_j) ***\n")
  print(corr.cw.w)
  cat("\n\n************************* Preregistered Analysis *************************n\n")
  cat("\n\n*** OVERALL data  ***\n\n")
  cat("\n\n*** CW BIC & R2 for Choice   ***\n")
  print(BIC(cw.choice.lm))
  print(summary(cw.choice.lm)$r.squared)
  cat("\n\n*** PVT BIC & R2 for Choice  ***\n")
  print(BIC(pvt.choice$nlsObject))
  print(pvt.choice$r2)
  cat("\n\n*** CW BIC & R2 for RT   ***\n")
  print(BIC(cw.rt.lm))
  print(summary(cw.rt.lm)$r.squared)
  cat("\n\n*** PVT BIC & R2 for RT  ***\n")
  print(BIC(pvt.rt.lm))
  print(summary(pvt.rt.lm)$r.squared)

  cat("\n\n*** BY ITEM   ***\n\n")
  cat("\n*** Fit Stats Means   ***\n")
  print(fitMeans.l)
  cat("\n*** Fit Stats SDs   ***\n")
  print(fitSDs.l)
  cat("\n*** BIC t.test for Choice   ***\n")
  print(BICttestChoice.l)
  cat("\nCohen's D:", BICcohensdChoice.l,"\n")
  cat("\n\n*** BIC t.test for RT   ***\n")
  print(BICttestRT.l)
  cat("\nCohen's D:", BICcohensdRT.l,"\n")
  cat("\n\n*** R2 t.test for Choice  ***\n")
  print(r2ttestChoice.l)
  cat("\nCohen's D:", r2cohensdChoice.l,"\n")
  cat("\n\n*** R2 t.test for RT  ***\n")
  print(r2ttestRT.l)
  cat("\nCohen's D:", r2cohensdRT.l,"\n")

sink(NULL)


############  Now Fit with Power Function

    cw.choice.nls <- with(data.sn.sum.m, nls(pCW ~ a*mCWpred^b, start = list(a=1,b=1)))
    cw.choice.r2 <- ch.R2( data.sn.sum.m$pCW, fitY= fitted(cw.choice.nls))

    #### Plot overall analysis
    data.sn.sum.m <- data.sn.sum.m[order(data.sn.sum.m$mCWpred),]
    fileName <- "allData_pHitRTcwPower.pdf"

    op <- par(mfrow=c(2,1),bty="n", font=1, family='serif', mar=c(2,5,2,5), oma=c(3,0,3,0), cex=1.25, las=1)

      y1Fit <- with(data.sn.sum.m, nls(pCW ~ a*mCWpred^b, start = list(a=1,b=1)))
        with(data.sn.sum.m, plot(mCWpred, pCW, xlab= "p(CW)", ylab=NA, pch=16, ylim = c(0,1)))
        mtext(side=2,"p(CW)", line=3, cex = 1.25)
        with(data.sn.sum.m, lines(mCWpred, predict(y1Fit), col="black", lwd=3))
        r2.choice <- round( ch.R2(data.sn.sum.m$pCW, fitY= fitted(y1Fit)), d=2)
#        mtext(side=2, bquote(r^2==.(r2.choice)), line=0, at = -.2, cex = .8*1.25)

      ch.plot.lm(data.sn.sum.m$mCWpred, data.sn.sum.m$aveRT, cex1 = 1.25, yLabel  = "RT", printR2 = F)
        mtext("All Data", outer = TRUE, cex = 1.25)
      dev.copy(pdf, fileName, width=6, height=9)
      dev.off();
    par(op)
    ##### Now calculate how well Warmth predicts choice for each item paired with each other item - by item.
    ##### Get BIC and R2
    df.out2 <- NULL
    op <- par(mfrow=c(2,1),bty="n", font=1, family='serif', mar=c(2,5,2,5), oma=c(3,0,3,0), cex=1.25, las=1)
    for(pr in Item1) {
      df.tmp <- data.sn.sum.m %>% filter_at(vars(c("Item1", "Item2")), any_vars(. == pr))
      df.tmp <- df.tmp[order(df.tmp$mCWpred),]
      fileName <- paste(pr,"pHitRTcwPower.pdf")

        y1Fit <- with(df.tmp, nls(pCW ~ a*mCWpred^b, start = list(a=1,b=1)))
          with(df.tmp, plot(mCWpred, pCW, xlab= "p(CW)", ylab=NA, pch=16, ylim = c(0,1)))
      		mtext(side=2,"p(CW)", line=3, cex = 1.25)
      		with(df.tmp, lines(mCWpred, predict(y1Fit), col="black", lwd=3))
      		r2.choice <- round( ch.R2(df.tmp$pCW, fitY= fitted(y1Fit)), d=2)
#      		mtext(side=2, bquote(r^2==.(r2.choice)), line=0, at = -.2, cex = .8*1.25)

        y2Fit <- ch.plot.lm(df.tmp$mCWpred, df.tmp$aveRT, cex1 = 1.25, yLabel  = "RT", printR2 = F)
          mtext(pr, outer = TRUE, cex = 1.25)
        dev.copy(pdf, fileName, width=6, height=9)
        dev.off();

      fileName <- paste(pr,"pHitRTpvt.pdf")
      df.tmp <- df.tmp[order(df.tmp$overlap),]

        y3Fit <- ch.plot.pHit(df.tmp$overlap, df.tmp$pHVO, useTwoParameterModel = useTwoParameterModel,  cex1 = 1, printR2 = F,yLabel="p(HVO)")
        y4Fit <- ch.plot.lm(df.tmp$overlap, df.tmp$aveRT, cex1 = 1.25, yLabel  = "RT", printR2 = F)
          mtext(pr, outer = TRUE, cex = 1.25)
        dev.copy(pdf, fileName, width=6, height=9)
        dev.off();

      df.tmp.out <- data.frame(probe = pr, CWchoiceR2 = r2.choice, CWrtR2 = summary(y2Fit)$r.squared, CWbicChoice = BIC(y1Fit), CWbicRt = BIC(y2Fit), PVTchoiceR2 = y3Fit$r2, PVTrtR2 = summary(y4Fit)$r.squared, PVTbicChoice =BIC(y3Fit$nlsObject) , PVTbicRt = BIC(y4Fit))

      df.out2 <- ch.rbind(df.out2, df.tmp.out)
    }

    fitMeans <- sapply(df.out2[,2:9], mean)
    fitSDs <- sapply(df.out2[,2:9], sd)

    ### Now calculate whether CW or PVT predicted more accurately Using the BIC (primary measure)
    BICttestChoice <- with(df.out2, t.test(CWbicChoice, PVTbicChoice, paired = T))
    ### Now calculate whether CW or PVT predicted more accurately Using the R2 for fun
    r2ttestChoice <- with(df.out2, t.test(CWchoiceR2, PVTchoiceR2, paired = T))

    ### Now calculate effect sizes
    BICcohensdChoice <- t_to_d(BICttestChoice$statistic, BICttestChoice$parameter, paired = TRUE, ci = 0.95, alternative = "two.sided")$d
    ### Now calculate whether CW or PVT predicted more accurately Using the R2 for fun
    r2cohensdChoice <- t_to_d(r2ttestChoice$statistic, r2ttestChoice$parameter, paired = TRUE, ci = 0.95, alternative = "two.sided")$d

    sink("../CW_PVT.stat_Exp1.txt", append = T)
      cat("\n\n************************* Power Fits Analysis *************************n\n")
      cat("\n\n*** OVERALL data  ***\n\n")
      cat("\n\n*** CW BIC & R2 for Choice   ***\n")
      print(BIC(cw.choice.nls))
      print(cw.choice.r2)

      cat("\n\n*** BY ITEM   ***\n\n")
      cat("\n*** Fit Stats Means   ***\n")
      print(fitMeans)
      cat("\n*** Fit Stats SDs   ***\n")
      print(fitSDs)
      cat("\n*** BIC t.test for Choice   ***\n")
      print(BICttestChoice)
      cat("\nCohen's D:", BICcohensdChoice,"\n")
      cat("\n\n*** R2 t.test for Choice  ***\n")
      print(r2ttestChoice)
      cat("\nCohen's D:", r2cohensdChoice,"\n")

    sink(NULL)


  op <- par(mfrow=c(2,2),bty="n", font=1, family='serif', mar=c(2,1,2,1), oma=c(2,2,2,2))
    with(df.out2, boxplot(CWbicChoice, PVTbicChoice, names = c("CW", "PVT"), main = "BIC Choice", frame = F))
    with(df.out2, boxplot(CWchoiceR2, PVTchoiceR2, names = c("CW", "PVT"), main = "R Square Choice", ylim=c(0,1), frame = F))
    with(df.out, boxplot(CWbicRt, PVTbicRt, names = c("CW", "PVT"), main = "BIC RT", frame = F))
    with(df.out, boxplot(CWrtR2, PVTrtR2, names = c("CW", "PVT"), main = "R Square RT", ylim=c(0,1), frame = F))
    dev.copy(pdf,"fitStatPlots.pdf", width=7, height=7)
    dev.off()
  par(op)

  setwd(mainDir)
  source("CW_PVTanalysis Exp2.r")
  source("CW_PVTanalysis Exp4.r")
