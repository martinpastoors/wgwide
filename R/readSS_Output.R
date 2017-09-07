# ----------------------------------------------------------------------------------------------
# readSS_Output.r
# 
# From: Piera Carpi
# 
# 03/09/2017 small modifications (MP)
# ----------------------------------------------------------------------------------------------

rm(list=ls())

library(r4ss)
library(FLCore)
library(tidyverse)
library(data.table)

# which methods in this package
# typeof(lsf.str("package:r4ss"))

# Find whether the select statement is in multiple packages
# find("select")

path       <- "D:/WGWIDE/2017/06. Data/hom-west/2017_Assessment_NoSP_ALK_1sex/" 
setwd(path)

sourcepath <- "D:/WGWIDE/2017/08. Personal folders/Martin/r/"
source(paste(sourcepath, "SS3toFLStock_function.r", sep=""))

source("D:/GIT/mptools/R/my_utils.R")
source("D:/GIT/fishvise/R/converter.R")

#-----------------------------------------------------------------------
# Creat and read FLStock object(s)
#-----------------------------------------------------------------------

# stockname
stockname <- "WHmackerel"   # SS3 dynamics, 1 morph per year, quarterly time step (Rec in Q1)

## Directory where SS3 stock assessment run is stored:
dirSS3output <- path
dirSS3bench  <- "D:/WKWIDE/06. Data/HOM west/FINAL RUN/"

# fbar ages
fbar_amin <- 1; fbar_amax <- 10

# Create an FLStock RData file and write to the standard path
SS3toFLStock(dirSS3output = dirSS3output, stockname = stockname, fbar_amin=fbar_amin, fbar_amax=fbar_amax)

# Also do this for the benchmark results
SS3toFLStock(dirSS3output = dirSS3output, stockname = stockname, fbar_amin=fbar_amin, fbar_amax=fbar_amax)
SS3toFLStock(dirSS3output = dirSS3bench , stockname = "WHOM_bench", fbar_amin=fbar_amin, fbar_amax=fbar_amax)

# Load the data
WHOM_2017  <- get(load("WHmackerel_SS3results.rdata"))
WHOM_bench <- get(load(paste(dirSS3bench, "WHOM_bench_SS3results.rdata",sep="")))


comp <- FLStocks(new_assessment = WHOM_2017, benchmark = WHOM_bench)
plot(comp)

str(WHOM_2017,2)


# create data.frames
WHOM2017df  <- FLStock2rbya(WHOM_2017) %>% mutate(assess="WHB2017")
WHOMbenchdf <- FLStock2rbya(WHOM_bench) %>% mutate(assess="WHBbench")

summary(WHOM_2017@stock.n)

str(WHOM_2017)
rbya <- rbind(WHOM2017df, WHOMbenchdf)

# plot of N
rbya %>% 
  ggplot(aes(year, n)) +
  theme_publication() +
  geom_line(aes(colour=assess)) +
  facet_wrap(~age, scale="free_y")

# plot of SSB
rbya %>% 
  mutate(ssb = n * ssbW * mat) %>% 
  group_by(assess, year) %>% 
  summarise(ssb = sum(ssb, na.rm=TRUE)) %>% 
  
  ggplot(aes(year, ssb)) +
  theme_publication() +
  geom_line(aes(colour=assess)) 

# plot of F
rbya %>% 
  filter(age >= 1, age <= 10) %>% 
  group_by(assess, year) %>% 
  summarise(f = mean(f)) %>% 

  ggplot(aes(year, f)) +
  theme_publication() +
  geom_line(aes(colour=assess)) 


#-----------------------------------------------------------------------
# GET OUTPUT
#-----------------------------------------------------------------------


# waa <- data.table(R1.sad$wtatage)
# waa <- waa[ waa$fleet==0, c(1,7:27), with=FALSE]
# waa <- waa[!duplicated(waa$yr),]
# waa$yr <- waa$yr*-1
# waa1 <- melt(waa, id.vars =  "yr")
# waa1[,variable:=as.numeric(as.character(variable))]
# ggplot(waa1, aes(variable,value)) + geom_line() + geom_smooth() + facet_wrap(~yr)
# smooth <- loess(value~variable*yr, data=waa1)
# 
# swaa <- data.table(cbind(rep(1982:2015, 21), rep(0:20, each=34), predict(smooth)))
# ggplot(swaa, aes(V2,V3)) + geom_line() + facet_wrap(~V1)
# swaa1 <- spread(swaa, V2, V3)

# 0) BENCHMARK
R0.bench <- SS_output(dir="Benchmark_2017_prova2/", covar=T, verbose=F, forecast=TRUE)
SS_plots(R0.bench, plot = 1:24, uncertainty=T, png=T)

# 1) BASE RUN
out_BR <- SS_output(dir="2017_Assessment/",covar=T, verbose=F, forecast=TRUE)
SS_plots(out_BR, uncertainty=T, png=T)#, plot=c(2:24)) 

# 1.1) BASE RUN - NO SPAIN ALK
out_BR_noSP <- SS_output(dir="2017_Assessment_NoSP_ALK/",covar=T, verbose=F, forecast=TRUE)
SS_plots(out_BR_noSP, uncertainty=T, png=F, pdf=T)#, plot=c(2:24)) 

# ----------------------
# FINAL RUN - WGWIDE 2017
# ----------------------
# 1.2) BASE RUN - NO SPAIN ALK & 1 SEX SETTING (Exaclty the same results than 2 sex settings)
out_BR_noSP_1s <- SS_output(dir="2017_Assessment_NoSP_ALK_1sex/",covar=T, verbose=F, forecast=TRUE)
SS_plots(out_BR_noSP_1s, uncertainty=T, png=T, pdf=F) 

# ------------------------------------
# SENSITIVITIES
# ------------------------------------
# 2) BASE RUN - NO SPAIN ALK and OLD VALUE for 2007 EGG SURVEY
out_BR_egg <- SS_output(dir="2017_Assessment_NoSP_ALK_EggSurvey/",covar=T, verbose=F, forecast=TRUE)
SS_plots(out_BR_egg, uncertainty=T, png=T) 

# 3) BASE RUN - NO SPAIN ALK and REMOVED LT 51 cm
out_BR_LT <- SS_output(dir="2017_Assessment_NoSP_ALK_Bis/",covar=T, verbose=F, forecast=TRUE)
SS_plots(out_BR_LT, uncertainty=T, png=T) 

# 4) BASE RUN - NO 2016 ALK 
out_BR_ALK2016 <- SS_output(dir="2017_Assessment_No2016_ALK/",covar=T, verbose=F, forecast=TRUE)
SS_plots(out_BR_ALK2016, uncertainty=T, png=T)#, plot=c(2:24)) 

# 5) BASE RUN - NO 2016 LFD
out_BR_2016LFD <- SS_output(dir="2017_Assessment_No2016_LFD/",covar=T, verbose=F, forecast=TRUE)
SS_plots(out_BR_2016LFD, uncertainty=T, png=F,pdf=T) 


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ------------------------------------
# RETROSPECTIVE 
# ------------------------------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Run retro
retroDir <- getwd()
SS_doRetro(masterdir=retroDir, oldsubdir = "2017_Assessment_NoSP_ALK_1sex", years=0:-5)

# Get output
retroModels <- SSgetoutput(dirvec=file.path("retrospectives",paste("retro",0:-5,sep="")))
# Summarize
retroSummary <- SSsummarize(retroModels)
endyrvec <- retroSummary$endyrs + 0:-5
# Plot Retro
SSplotComparisons(retroSummary, endyrvec=endyrvec, indexfleets = 1, indexUncertainty = TRUE, legendlabels=paste("Data",0:-5,"years"), print=TRUE,
                  plotdir = paste(retroDir, "/retrospectives/RunComparison", sep="/"))


SS_doFbar_wSD(masterdir=retroDir, oldsubdir = "2017_Assessment_NoSP_ALK_1sex", ages=1:10)
Fbar1to10 <- extract_Fsd(retroDir)

ggplot(Fbar, aes(year, Fbar)) + geom_line() +
  geom_errorbar(aes(ymin=Fbar-CI, ymax=Fbar+CI)) + theme_bw(14) 


# Estimate Fbar:
myF <- list()
for(i in 1:5){
  myF[[i]] <- rowMeans(retroModels[[i]]$Z_at_age[which(retroModels[[i]]$Z_at_age$Gender==1),5:14]-0.15)
}
myF_df <- do.call(cbind.data.frame, myF)
names(myF_df) <- c("Retro1", "Retro2", "Retro3", "Retro4", "Retro5")
myF_df$Year <- 1982:2017
myFr <- melt(myF_df, id.var="Year")

ggplot(myFr, aes(Year, value, col=variable)) + geom_line()




# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ------------------------------------
# COMPARISONS 
# ------------------------------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Get the outputs to be compared
SpainALK <- SSgetoutput(dirvec = c("./2017_Assessment/", "./2017_Assessment_NoSP_ALK/"))
BenchVsNew <- SSgetoutput(dirvec = c("./Benchmark_2017/", "./2017_Assessment_NoSP_ALK/", "./2017_Assessment_NoSP_ALK_EggSurvey/", "./2017_Assessment_NoSP_ALK_Bis/"))
BenchVsNo2016ALK <- SSgetoutput(dirvec = c("./Benchmark_2017/", "./2017_Assessment_NoSP_ALK/", "./2017_Assessment_No2016_ALK/"))
BenchVsNo2016LFD <- SSgetoutput(dirvec = c("./Benchmark_2017/", "./2017_Assessment_NoSP_ALK/", "./2017_Assessment_No2016_LFD/"))
BenchVsNoSpALK_1sex <- SSgetoutput(dirvec = c("./Benchmark_2017/", "./2017_Assessment_NoSP_ALK/", "./2017_Assessment_NoSP_ALK_1sex/"))
BenchVs1sex_bench <- SSgetoutput(dirvec = c("./Benchmark_2017_prova2/", "./2017_Assessment_NoSP_ALK/", "./2017_Assessment_NoSP_ALK_1sex/"))

# Create summary comparison
summary_ALK <- SSsummarize(SpainALK)
summary_bench <- SSsummarize(BenchVsNew)
summary_no2016ALK <- SSsummarize(BenchVsNo2016ALK)
summary_no2016LFD <- SSsummarize(BenchVsNo2016LFD)
summary_no2016ALK_1sex <- SSsummarize(BenchVsNoSpALK_1sex)
summary_1sex_benchProva <- SSsummarize(BenchVs1sex_bench)

# Plot comparison
SSplotComparisons(summary_bench, indexfleets = 2, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = "./2017_Assessment/RunComparison/4_Benchmark_Egg_LT/", 
                  legendlabels=c("Benchmark", "2017_Assessment", "2017_Assessment_oldEgg", "2017_Assessment_no51LT"))

SSplotComparisons(summary_no2016ALK, indexfleets = 2, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = "./2017_Assessment/RunComparison/5_Benchmark_New_no2016ALK/", 
                  legendlabels=c("Benchmark", "2017_Assessment", "2017_Assessment_no2016ALK"))

SSplotComparisons(summary_no2016ALK_1sex, indexfleets = 2, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = "./2017_Assessment_NoSP_ALK/RunComparison_1sex/",
                  legendlabels=c("Benchmark", "2017_Assessment_noSP_ALK", "2017_Assessment_noSP_ALK_1sex"))

SSplotComparisons(summary_no2016LFD, indexfleets = 2, indexUncertainty = TRUE, print=TRUE, png=FALSE, pdf=TRUE,
                  plotdir = "./2017_Assessment/RunComparison/6_Benchmark_New_no2016LFD/",
                  legendlabels=c("Benchmark", "2017_Assessment", "2017_Assessment_no2016LFD"))

SSplotComparisons(summary_ALK, indexfleets = 1, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = "./2017_Assessment/RunComparison/",
                  legendlabels=c("ALL ALK", "NO SPANISH ALK"))

SSplotComparisons(summary_1sex_benchProva, indexfleets = 1, indexUncertainty = TRUE, print=TRUE, png=TRUE, pdf=FALSE,
                  plotdir = "./Benchmark_2017_prova/RunComparison/",
                  legendlabels=c("Bench_Prova", "ALK_NO_SP", "ALK_NO_SP_1sex"))


