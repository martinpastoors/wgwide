library(r4ss)
library(data.table)
library(FLCore)
library(ggplot2)
library(tidyr)
library(data.table)


## Directory where SS3 stock assessment run is stored:
dirSS3output <- "C:/Users/PC09/Documents/Work/WGs/2018/WGWIDE/SS_3.0.1/"

# ----------------------
# FINAL RUN - WGWIDE 2018
# ----------------------
# 1.2) BASE RUN - NO SPAIN ALK & 1 SEX SETTING (Exaclty the same results than 2 sex settings)
BC <- SS_output(dir=paste(dirSS3output, "FOR_ASSESSMENT/BaseCase", sep=""),covar=T, verbose=F, forecast=TRUE)
SS_plots(BC, uncertainty=T, png=T, pdf=F) 

run1 <- SS_output(dir=paste(dirSS3output, "FOR_ASSESSMENT/Run1_WrongPELACUS", sep=""),covar=T, verbose=F, forecast=TRUE)
SS_plots(run1, uncertainty=T, png=T, pdf=F) 

run2 <- SS_output(dir=paste(dirSS3output, "FOR_ASSESSMENT/Run2_noIBTS", sep=""),covar=T, verbose=F, forecast=TRUE)
SS_plots(run2, uncertainty=T, png=T, pdf=F) 

run3 <- SS_output(dir=paste(dirSS3output, "FOR_ASSESSMENT/Run3_NoALK17", sep=""),covar=T, verbose=F, forecast=TRUE)
SS_plots(run3, uncertainty=T, png=T, pdf=F) 

run4 <- SS_output(dir=paste(dirSS3output, "FOR_ASSESSMENT/Run4_NoLFD17", sep=""),covar=T, verbose=F, forecast=TRUE)
SS_plots(run4, uncertainty=T, png=T, pdf=F) 

run5 <- SS_output(dir=paste(dirSS3output, "FOR_ASSESSMENT/Run5_wDisc_InCatch", sep=""),covar=T, verbose=F, forecast=TRUE)
SS_plots(run5, uncertainty=T, png=T, pdf=F) 

run6 <- SS_output(dir=paste(dirSS3output, "FOR_ASSESSMENT/Run6_wDisc_Separated", sep=""),covar=T, verbose=F, forecast=TRUE)
SS_plots(run6, uncertainty=T, png=T, pdf=F) 

run7 <- SS_output(dir=paste(dirSS3output, "FOR_ASSESSMENT/Run7_NoLFD&IBTS17", sep=""),covar=T, verbose=F, forecast=TRUE)
SS_plots(run7, uncertainty=T, png=T, pdf=F) 

run8 <- SS_output(dir=paste(dirSS3output, "FOR_ASSESSMENT/Run8_NoPELACUS", sep=""),covar=T, verbose=F, forecast=TRUE)
SS_plots(run8, uncertainty=T, png=T, pdf=F) 

run9 <- SS_output(dir=paste(dirSS3output, "FOR_ASSESSMENT/Run9_OnlyCatch", sep=""),covar=T, verbose=F, forecast=TRUE)
SS_plots(run9, uncertainty=T, png=T, pdf=F) 

run10 <- SS_output(dir=paste(dirSS3output, "FOR_ASSESSMENT/Run10_OnlyCatch&Survey", sep=""),covar=T, verbose=F, forecast=TRUE)
SS_plots(run10, uncertainty=T, png=T, pdf=F) 

run11 <- SS_output(dir=paste(dirSS3output, "FOR_ASSESSMENT/Run11_NoLFD15", sep=""),covar=T, verbose=F, forecast=TRUE)
SS_plots(run11, uncertainty=T, png=T, pdf=F) 

run12 <- SS_output(dir=paste(dirSS3output, "FOR_ASSESSMENT/Run12_NoALK16", sep=""),covar=T, verbose=F, forecast=TRUE)
SS_plots(run12, uncertainty=T, png=T, pdf=F) 

run13 <- SS_output(dir=paste(dirSS3output, "FOR_ASSESSMENT/Run13_NoALK-LFD16", sep=""),covar=T, verbose=F, forecast=TRUE)
SS_plots(run13, uncertainty=T, png=T, pdf=F) 




# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ------------------------------------
# COMPARISONS 
# ------------------------------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Get the outputs to be compared
comp <- SSgetoutput(dirvec = c(paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase/")))
comp1 <- SSgetoutput(dirvec = c(paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase/"), paste0(dirSS3output,"FOR_ASSESSMENT/Run1_WrongPELACUS/")))
comp2 <- SSgetoutput(dirvec = c(paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase"), paste0(dirSS3output,"FOR_ASSESSMENT/Run2_noIBTS/")))
comp3 <- SSgetoutput(dirvec = c(paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase/"), paste0(dirSS3output,"FOR_ASSESSMENT/Run3_NoALK17/")))
comp4 <- SSgetoutput(dirvec = c(paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase/"), paste0(dirSS3output,"FOR_ASSESSMENT/Run4_NoLFD17/")))
comp5 <- SSgetoutput(dirvec = c(paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase/"), paste0(dirSS3output,"FOR_ASSESSMENT/Run5_wDisc_InCatch/")))
comp6 <- SSgetoutput(dirvec = c(paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase/"), paste0(dirSS3output,"FOR_ASSESSMENT/Run6_wDisc_Separated/")))
comp7 <- SSgetoutput(dirvec = c(paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase/"), paste0(dirSS3output,"FOR_ASSESSMENT/Run7_NoLFD&IBTS17/")))
comp8 <- SSgetoutput(dirvec = c(paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase/"), paste0(dirSS3output,"FOR_ASSESSMENT/Run8_NoPELACUS/")))
comp8b <- SSgetoutput(dirvec = c("C:/Users/PC09/Documents/Work/WGs/2016/WHOM_Benchmark/SS/FINAL_RUN/NO_PELGAS_RecDev_FixedFec", paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/2018_Assessment/"), paste0(dirSS3output,"FOR_ASSESSMENT/Run8_NoPELACUS/")))
comp9 <- SSgetoutput(dirvec = c(paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase/"), paste0(dirSS3output,"FOR_ASSESSMENT/Run9_OnlyCatch/")))
comp10 <- SSgetoutput(dirvec = c(paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase/"), paste0(dirSS3output,"FOR_ASSESSMENT/Run10_OnlyCatch&Survey/")))
comp11 <- SSgetoutput(dirvec = c(paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase/"), paste0(dirSS3output,"FOR_ASSESSMENT/Run11_NoLFD15/")))
comp12 <- SSgetoutput(dirvec = c(paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase/"), paste0(dirSS3output,"FOR_ASSESSMENT/Run12_NoALK16/")))
comp13 <- SSgetoutput(dirvec = c(paste0(dirSS3output,"converted/"), paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase/"), paste0(dirSS3output,"FOR_ASSESSMENT/Run13_NoALK-LFD16/")))

# Create summary comparison
summaryComp <- SSsummarize(comp)
summaryComp1 <- SSsummarize(comp1)
summaryComp2 <- SSsummarize(comp2)
summaryComp3 <- SSsummarize(comp3)
summaryComp4 <- SSsummarize(comp4)
summaryComp5 <- SSsummarize(comp5)
summaryComp6 <- SSsummarize(comp6)
summaryComp7 <- SSsummarize(comp7)
summaryComp8 <- SSsummarize(comp8)
summaryComp8b <- SSsummarize(comp8b)
summaryComp9 <- SSsummarize(comp9)
summaryComp10 <- SSsummarize(comp10)
summaryComp11 <- SSsummarize(comp11)
summaryComp12 <- SSsummarize(comp12)
summaryComp13 <- SSsummarize(comp13)

# Plot comparison
SSplotComparisons(summaryComp, subplots=11:12,indexfleets = 5, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/BaseCase/comparison/"), 
                  legendlabels=c("Assessment2017", "Assessment2018_BaseCase"))

SSplotComparisons(summaryComp1, indexfleets = 3, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/Run1_WrongPELACUS/comparison/"), 
                  legendlabels=c("Assessment2017", "Assessment2018_BaseCase", "Assessment2018_Disc"))

SSplotComparisons(summaryComp2, indexfleets = 3, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/Run2_noIBTS/comparison/"), 
                  legendlabels=c("Assessment2017", "Assessment2018_BaseCase", "Assessment2018_noIBTS17"))

SSplotComparisons(summaryComp3,subplots=11:12,  indexfleets = 2, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/Run3_NoALK17/comparison/"), 
                  legendlabels=c("Assessment2017", "Assessment2018_BaseCase", "Assessment2018_noALK17"))

SSplotComparisons(summaryComp4, subplots=11:12, indexfleets = 3, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/Run4_NoLFD17/comparison/"), 
                  legendlabels=c("Assessment2017", "Assessment2018_BaseCase", "Assessment2018_noLFD17"))

SSplotComparisons(summaryComp5, subplots=11:12, indexfleets = 2, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/Run5_wDisc_InCatch/comparison/"), 
                  legendlabels=c("Assessment2017", "Assessment2018_BaseCase", "Assessment2018_DiscInCatch"))

SSplotComparisons(summaryComp6,  subplots=11:12, indexfleets = 3, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/Run6_wDisc_Separated/comparison/"), 
                  legendlabels=c("Assessment2017", "Assessment2018_BaseCase", "Assessment2018_DiscSeparated"))


SSplotComparisons(summaryComp7, subplots=11:12, indexfleets = 3, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/Run7_NoLFD&IBTS17/comparison/"), 
                  legendlabels=c("Assessment2017", "Assessment2018_BaseCase", "Assessment2018_PELACUSfix"))

SSplotComparisons(summaryComp8, subplots=11:12, indexfleets = 3, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/Run8_NoPELACUS/comparison/"), 
                  legendlabels=c("Assessment2017", "Assessment2018_BaseCase", "Assessment2018_NoPELACUS"))

SSplotComparisons(summaryComp8b, indexfleets = 2, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/Run8_NoPELACUS/comparison/"), 
                  legendlabels=c("Benchmark","Assessment2017", "Assessment2018_BaseCase", "Assessment2018_NoPELACUS"))

SSplotComparisons(summaryComp9, indexfleets = 3, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/Run9_OnlyCatch/comparison/"), 
                  legendlabels=c("Assessment2017", "Assessment2018_BaseCase", "Assessment2018_OnlyCatch"))

SSplotComparisons(summaryComp10,subplots=11:12, indexfleets = 3, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/Run10_OnlyCatch&Survey/comparison/"), 
                  legendlabels=c("Assessment2017", "Assessment2018_BaseCase", "Assessment2018_OnlyCatch&Survey"))

SSplotComparisons(summaryComp11, indexfleets = 2, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/Run11_NoLFD15/comparison/"), 
                  legendlabels=c("Assessment2017", "Assessment2018_BaseCase", "Assessment2018_NoLFD15"))

SSplotComparisons(summaryComp12, indexfleets = 2, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/Run12_NoALK16/comparison/"), 
                  legendlabels=c("Assessment2017", "Assessment2018_BaseCase", "Assessment2018_NoALK16"))

SSplotComparisons(summaryComp13, indexfleets = 2, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = paste0(dirSS3output,"FOR_ASSESSMENT/Run13_NoALK-LFD16/comparison/"), 
                  legendlabels=c("Assessment2017", "Assessment2018_BaseCase", "Assessment2018_NoALK-LFD16"))





# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ------------------------------------
# RETROSPECTIVE 
# ------------------------------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Run retro
retroDir <- paste0(dirSS3output, "FOR_ASSESSMENT/BaseCase")
SS_doRetro(masterdir=dirSS3output, oldsubdir = "FOR_ASSESSMENT/BaseCase", years=0:-10)

# Get output
retroModels <- SSgetoutput(dirvec=paste0(dirSS3output, "FOR_ASSESSMENT/BaseCase/retrospectives",paste("/retro",0:-10,sep="")))
# Summarize
retroSummary <- SSsummarize(retroModels)
endyrvec <- retroSummary$endyrs + 0:-10
# Plot Retro
SSplotComparisons(retroSummary, endyrvec=endyrvec, subplots=11:12, indexfleets = 5, indexUncertainty = TRUE, legendlabels=paste("Data",0:-10,"years"), print=TRUE,
                  plotdir = paste(retroDir, "/retrospectives/retroComparison", sep=""))

retroDir <- getwd()
SS_doRetro(masterdir=retroDir, oldsubdir = "2017_Assessment_NoSP_ALK_1sex", years=0:-10)





######################################
# CREATE FLStock
######################################
# 
require(FLCore)
# source function
source("C:/Users/PC09/Documents/Work/R_codes/SS3toFLStock_v3.30.r")

stockname <- "WHOM"   # SS3 dynamics, 1 morph per year, quarterly time step (Rec in Q1)

## Directory where SS3 stock assessment run is stored:
dirSS3output <- "C:/Users/PC09/Documents/Work/WGs/2018/WGWIDE/SS_3.0.1/FOR_ASSESSMENT/BaseCase/"

fbar_amin <- 1
fbar_amax <- 10

## First time must be run with "creatFLStock" set to "TRUE", so that it creates the FLStock needed by EqSim
## All subsequent runs set "createFLStock" to "FALSE", since the FLStock object has already been created and can simply be loaded into the session
createFLStock <-  TRUE

if(createFLStock){ 
  library(r4ss)
  #source("SS3toFLStock.r")
  SS3toFLStock(dirSS3output = dirSS3output, stockname = stockname, fbar_amin=fbar_amin, fbar_amax=fbar_amax)
}else{
  load(paste(dirSS3output,"/",stockname,"_SS3results.rdata",sep="")) 
}

