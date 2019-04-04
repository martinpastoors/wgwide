# ============================================================================
# UPload Assessment Data in SAG
#
# 21/03/2018 First coding of adding historical data to standardgraphs
# 27/04/2018 Change to Historical in the Purpose Field
# 15/11/2018 Change just the comments of an assessment (trial)
# 19/03/2019 Updated for HAWG 2019
# 04/04/2019 Updated for IBPNEAMac 2019
# ============================================================================

rm(list=ls())

library(FLCore)
library(FLSAM)
library(icesSAG)  # devtools::install_github("ices-tools-prod/icesSAG")
library(tidyverse)

# use token
options(icesSAG.use_token = TRUE)

setwd("./mac.27.nea")

# load the fit object
load("//community.ices.dk/DavWWWRoot/ExpertGroups/benchmarks/2019/IBPNEAMac/2019 Meeting docs/07. Software/final run/VarObsCatches2FvarNEW.RData")

# Get the assessment FLStock data
load("results/MAC FLStock.RData")

# rename the FLStock and FLSAM objects
STK     <- Mac
# STK.sam <- NSH.sam

# Set years and ranges
FiY   <- as.numeric(min(dimnames(STK)$year))
DtY   <- as.numeric(max(dimnames(STK)$year))
LaY   <- as.numeric(max(dimnames(STK)$year))
# nyrs  <- ((DtY)-(FiY))+1
nyrs  <- ((DtY)-(FiY))
nyrs2 <- ((LaY)-(FiY))+1

# assessment summary
ssb <- 
  as.data.frame(stockassessment::ssbtable(fit.new)) %>% 
  rownames_to_column() 

tsb <- 
  as.data.frame(stockassessment::tsbtable(fit.new)) %>% 
  rownames_to_column() 

fbar <-
  as.data.frame(stockassessment::fbartable(fit.new)) %>% 
  rownames_to_column() 

rec <-
  as.data.frame(stockassessment::rectable(fit.new)) %>% 
  rownames_to_column() 

catch <-
  as.data.frame(stockassessment::catchtable(fit.new)) %>%
  rownames_to_column() 

# Meta information
stockkeylabel  <- "mac.27.nea"
assessmentyear <- 2019
contactperson  <- "mpastoors@pelagicfish.eu"

# SSB in intermediate year 
# SSBint <- 4160000

# Create the input data for uploading  
info     <- stockInfo(
  StockCode      = stockkeylabel, 
  AssessmentYear = assessmentyear, 
  ContactPerson  = contactperson)

info$StockCategory             <- "1"
info$MSYBtrigger               <- 2500000
info$Blim                      <- 1990000
info$Bpa                       <- 2500000
info$Flim                      <- 0.46
info$Fpa                       <- 0.37
info$FMSY                      <- 0.23
info$Fage                      <- "4-8" 
info$RecruitmentAge            <- 0
info$CatchesLandingsUnits      <- "t"
info$RecruitmentDescription    <- "age"
info$RecruitmentUnits          <- "NE3" 
info$FishingPressureDescription<- "F"
info$FishingPressureUnits      <- NA 
info$StockSizeDescription      <- "SSB"
info$StockSizeUnits            <- "t"
info$Purpose                   <- "Bench"
info$CustomSeriesName1         <- "model catch"
info$CustomSeriesName2         <- "model catch low"
info$CustomSeriesName3         <- "model catch high"
info$CustomSeriesUnits1        <- "t"
info$CustomSeriesUnits2        <- "t"
info$CustomSeriesUnits3        <- "t"
info$ModelName                 <- "SAM"
info$ModelType                 <- "A"

# Create the fish data
fishdata                          <- stockFishdata(FiY:LaY)

fishdata$Catches[1:nyrs]          <- an(STK@landings)[1:nyrs]

fishdata$Low_Recruitment[1:nyrs]  <- rec$Low[1:nyrs]
fishdata$Recruitment[1:nyrs]      <- rec$Estimate[1:nyrs]
fishdata$High_Recruitment[1:nyrs] <- rec$High[1:nyrs]

fishdata$Low_StockSize            <- ssb$Low
fishdata$StockSize                <- ssb$Estimate
fishdata$High_StockSize           <- ssb$High

fishdata$Low_TBiomass             <- tsb$Low
fishdata$TBiomass                 <- tsb$Estimate
fishdata$High_TBiomass            <- tsb$High

fishdata$Low_FishingPressure[1:nyrs] <- fbar$Low[1:nyrs]
fishdata$FishingPressure[1:nyrs]     <- fbar$Estimate[1:nyrs]
fishdata$High_FishingPressure[1:nyrs]<- fbar$High[1:nyrs]

fishdata$CustomSeries1[1:nyrs]    <- catch$Low[1:nyrs]
fishdata$CustomSeries2[1:nyrs]    <- catch$Estimate[1:nyrs]
fishdata$CustomSeries3[1:nyrs]    <- catch$High[1:nyrs]

# upload to SAG
key <- icesSAG::uploadStock(info, fishdata)

# Get SAG settings
# getSAGSettingsForAStock(assessmentKey=key) %>% View()

# Add comment to SAG settings
setSAGSettingForAStock(assessmentKey=key,
                       chartKey=0,
                       settingKey=21,
                       settingValue="IBPNEAMac 2019",
                       copyNextYear=FALSE)

# Get SAG settings
getSAGSettingsForAStock(assessmentKey=key) %>% View()

# t1  <- unlist(getStockDownloadData(key))

# plot F's
# fishdata %>% 
#   dplyr::select(Year, FishingPressure, CustomSeries4, CustomSeries5) %>% 
#   setNames(c("year", "F26", "F01", "F78")) %>% 
#   gather(key=F, value=value, F26:F78) %>% 
#   filter(year >= 1980) %>% 
#   
#   ggplot(aes(x=year, y=value, group=F)) +
#   theme_bw() +
#   geom_line(aes(colour=F), size=1)

