# ============================================================================
# Upload Summaryto SAG
#
# Martin Pastoors
#
# 21/03/2018 First coding of adding historical data to standardgraphs
# 02/09/2018 Adapted during WGWIDE 2018
# ============================================================================

rm(list=ls())

# library(devtools)
# devtools::install_github("ices-tools-prod/icesSAG")

library(icesSAG)
library(tidyverse)
library(readxl)

# use token
options(icesSAG.use_token = TRUE)

# Load utils code
source("../mptools/r/my_utils.r")

file  <- "D:/WGWIDE/2018 Meeting Docs/06. Data/hom.27.2a4a5b6a7a-ce-k8/sag/StandardGraphs_Template.xlsx"
sheet <- "FishData"

# generic
stockname      <- as.character(readxl::read_excel(path = file, sheet = "FishData", range="A2", col_types = "text", col_names = FALSE) )
assessmentyear <- as.numeric  (readxl::read_excel(path = file, sheet = "FishData", range="B2", col_types = "text", col_names = FALSE) )
modeltype      <- as.character(readxl::read_excel(path = file, sheet = "FishData", range="C2", col_types = "text", col_names = FALSE) )
contactperson  <- as.character(readxl::read_excel(path = file, sheet = "FishData", range="D2", col_types = "text", col_names = FALSE) )

stockcategory  <- as.integer  (readxl::read_excel(path = file, sheet = "FishData", range="A4", col_types = "text", col_names = FALSE) )
purpose        <- as.character(readxl::read_excel(path = file, sheet = "FishData", range="B4", col_types = "text", col_names = FALSE) )
modelname      <- as.character(readxl::read_excel(path = file, sheet = "FishData", range="C4", col_types = "text", col_names = FALSE) )

# reference points
msybtrigger <- as.numeric(readxl::read_excel(path = file, sheet = "FishData", range="C9", col_types = "text", col_names = FALSE) )
fmsy        <- as.numeric(readxl::read_excel(path = file, sheet = "FishData", range="C10", col_types = "text", col_names = FALSE) )
blim        <- as.numeric(readxl::read_excel(path = file, sheet = "FishData", range="C11", col_types = "text", col_names = FALSE) )
bpa         <- as.numeric(readxl::read_excel(path = file, sheet = "FishData", range="C12", col_types = "text", col_names = FALSE) )
flim        <- as.numeric(readxl::read_excel(path = file, sheet = "FishData", range="C13", col_types = "text", col_names = FALSE) )
fpa         <- as.numeric(readxl::read_excel(path = file, sheet = "FishData", range="C14", col_types = "text", col_names = FALSE) )

recage      <- as.integer  (readxl::read_excel(path = file, sheet = "FishData", range="B33", col_types = "text", col_names = FALSE) )
fbarrange   <- as.character(readxl::read_excel(path = file, sheet = "FishData", range="B34", col_types = "text", col_names = FALSE) )

# descriptions  
recdescription <- as.character(readxl::read_excel(path = file, sheet = "FishData", range="D45", col_types = "text", col_names = FALSE) )
recunits       <- as.character(readxl::read_excel(path = file, sheet = "FishData", range="D46", col_types = "text", col_names = FALSE))

stocksizedescription <- as.character(readxl::read_excel(path = file, sheet = "FishData", range="J45", col_types = "text", col_names = FALSE) )
stocksizeunits       <- as.character(readxl::read_excel(path = file, sheet = "FishData", range="J46", col_types = "text", col_names = FALSE))

fdescription <- as.character(readxl::read_excel(path = file, sheet = "FishData", range="S45", col_types = "text", col_names = FALSE) )
funits       <- as.character(readxl::read_excel(path = file, sheet = "FishData", range="S46", col_types = "text", col_names = FALSE))

catchunits <- as.character(readxl::read_excel(path = file, sheet = "FishData", range="N46", col_types = "text", col_names = FALSE))

# read the summary data
x <- 
  read_excel(path = file, sheet = sheet, range="A49:X100", col_names = FALSE, col_types = "text") %>% 
  setNames(c("year",
             "rec_low","rec","rec_upp",
             "tsb_low","tsb","tsb_upp",
             "ssb_low","ssb","ssb_upp",
             "catches","landings","landingsbms","discards","logbookdiscards","ibc","unallocated",
             "f_low","f","f_upp","f_landings","f_discards","f_ibc","f_unallocated"
             )) %>% 
  filter(!is.na(year))


# Upload stock assessment data to SAG database
info     <- 
  stockInfo(StockCode      = stockname, 
            AssessmentYear = assessmentyear, 
            ContactPerson = "piera.carpi@cefas.co.uk") 

info$StockCategory             <- stockcategory
info$MSYBtrigger               <- msybtrigger
info$Blim                      <- blim
info$Bpa                       <- bpa
info$Flim                      <- flim
info$Fpa                       <- fpa 
info$FMSY                      <- fmsy
info$Fage                      <- fbarrange
info$RecruitmentAge            <- recage
info$CatchesLandingsUnits              <- catchunits
info$RecruitmentDescription    <- recdescription
info$RecruitmentUnits          <- recunits
info$FishingPressureDescription<- fdescription
info$FishingPressureUnits      <- funits
info$StockSizeDescription      <- stocksizedescription
info$StockSizeUnits            <- stocksizeunits
info$Purpose                   <- purpose

# summary(info)

# fishdata
FiY       <- min(x$year) 
LaY       <- max(x$year)

fishdata  <- stockFishdata(FiY:LaY)

fishdata$Low_Recruitment       <- x$rec_low
fishdata$Recruitment           <- x$rec
fishdata$High_Recruitment      <- x$rec_upp

fishdata$Low_StockSize         <- x$ssb_low
fishdata$StockSize             <- x$ssb
fishdata$High_StockSize        <- x$ssb_upp

fishdata$Low_FishingPressure   <- x$f_low
fishdata$FishingPressure       <- x$f
fishdata$High_FishingPressure  <- x$f_upp

fishdata$Landings              <- x$landings
fishdata$Catches               <- x$catches
fishdata$Unallocated_Removals  <- x$unallocated
fishdata$Discards              <- x$discards

# summary(fishdata)
# glimpse(fishdata)

# upload to SAG
key <- icesSAG::uploadStock(info, fishdata)

# now add a comment to the upload of an assessment
setSAGSettingForAStock(AssessmentKey=9564, 
                       chartKey=0,
                       settingKey=21,
                       settingValue="Martin Pastoors Historical data",
                       copyNextYear=FALSE) 



# test
getSAG(stock=NULL, year = 1989)  
  

